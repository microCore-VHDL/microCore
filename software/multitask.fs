\ ----------------------------------------------------------------------
\ @file : multitask.fs
\ ----------------------------------------------------------------------
\
\ Last change: KS 03.08.2022 18:07:07
\ @project: microForth/microCore
\ @language: gforth_0.6.2
\ @copyright (c): Free Software Foundation
\ @original author: ks - Klaus Schleisiek
\ @contributor:
\
\ @license: This file is part of microForth.
\ microForth is free software for microCore that loads on top of Gforth;
\ you can redistribute it and/or modify it under the terms of the
\ GNU General Public License as published by the Free Software Foundation,
\ either version 3 of the License, or (at your option) any later version.
\ This program is distributed in the hope that it will be useful,
\ but WITHOUT ANY WARRANTY; without even the implied warranty of
\ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
\ GNU General Public License for more details.
\ You should have received a copy of the GNU General Public License
\ along with this program. If not, see http://www.gnu.org/licenses/.
\
\ @brief : A co-operative multitasker for microCore.
\ It has been split up into two parts: multitask.fs and task_lib.fs
\ To use the multitasker, tasks_addr_width must be > 0 in
\ architecture_pkg.vhd
\
\ Version Author   Date       Changes
\   2400    ks   06-Jul-2022  Split up into multitask.fs and task_lib.fs
\ ----------------------------------------------------------------------
Host 

True  Constant PERFORMANCE-MEASUREMENT immediate \ when true, it is done at Priority

Variable TaskVariables  TaskVariables off \ counter for task variable index

Variable 'go-next        0 'go-next      !
Variable 'stopped        0 'stopped      !
Variable 'do-poll-tmax   0 'do-poll-tmax !
Variable 'do-time        0 'do-time      !
Variable 'do-wake        0 'do-wake      !
Variable 'do-poll        0 'do-poll      !
Variable 'do-priority    0 'do-priority  !
Variable 'do-robin       0 'do-robin     !

\ ----------------------------------------------------------------------
\ Application dependent:
\ Make OS performance observable with a scope:
\    gon should set an output signal,
\    goff should reset said output signal.
\
\ The output signal will be '0' as long as the scheduler is active,
\ it is '1' when application code is being executed.
\ ----------------------------------------------------------------------
T definitions H

Macro: gon  ( -- )  ; \ 1     lit, T IO ! H ;
Macro: goff ( -- )  ; \ 1 not lit, T IO ! H ;

\ ----------------------------------------------------------------------
\ The structure of a TaskControlBlock (TCB)
\      0      1     2      3      4      5       6
\ | exec | link | Dsp | sema | time | poll | catch |
\ ----------------------------------------------------------------------

: Taskvariable ( <name> -- )  TaskVariables @ T Constant H   #cell TaskVariables +! ;
: Task         ( <name> -- )  Tcreate   TaskVariables @ T allot H ;
T
Taskvariable #t-exec     \ task state = execution addr
Taskvariable #t-link     \ link to next task
Taskvariable #t-dsp      \ data stack address
Taskvariable #t-sema     \ waiting on semaphore
Taskvariable #t-time     \ waiting on Timer
Taskvariable #t-poll     \ waiting on specific polling routine / message number if halted
Taskvariable #t-catch    \ holds address of previous catch frame or zero

\ ----------------------------------------------------------------------
\ The data structure of Semaphores:
\            0       1           2
\ | owner-task | count | Sema-link |
\ ----------------------------------------------------------------------

Variable Sema-link   0 Sema-link !        \ The anchor of a linked list of all semaphors
H
: Semaphore  ( <name> -- )  Tcreate T here 0 , 0 , Sema-link @ , Sema-link ! H ;
T
0 cells Constant #s-task   \ must remain at the first position
1 cells Constant #s-count
2 cells Constant #s-link

\ ----------------------------------------------------------------------
\ The data structure at Priority:
\             0      1      2      3      4      5    6
\ | do-priority | link | flag | time  | max | task | PC |
\ The fields MUST remain in this order for do-priority
\ ----------------------------------------------------------------------

Create Priority   7 cells allot
   0 cells Constant #p-exec
   1 cells Constant #p-link
   2 cells Constant #p-flag   \ set to true by do-priority, to false by do-robin, modifies PAUSE behaviour
   3 cells Constant #p-time
   4 cells Constant #p-max
   5 cells Constant #p-task
   6 cells Constant #p-pc
Priority #p-flag + Constant Prioflag

Target

H PERFORMANCE-MEASUREMENT T Version PERFORMANCE-MEASUREMENT

Variable Tptr          \ points to the currently active task control block (TCB)

Macro: TCB  ( -- task )       T Tptr @ H ;

: tcb@      ( offset -- n )   Tcb + @ ;
: tcb!      ( n offset -- )   Tcb + ! ;

Task Terminal   Terminal Tptr !

: dsp>task ( dsp -- task-nr )   [ ds_addr_width negate ] Literal shift ;

: task>dsp ( task-nr -- dsp )   ds_addr_width shift ;

: task>rsp ( task-nr -- rsp-1 )  \ rstack grows towards lower addresses
   1+ rs_addr_width shift cell- #rstack +
;
\ ----------------------------------------------------------------------
\ The data structure at RoundRobin and RRLink:
\          0      1
\ | do-robin | link |
\ ----------------------------------------------------------------------

Variable RoundRobin
Variable RRLink

Macro: do-next  ( task -- lfa ) T ld cell+ swap BRANCH H ;

\ ----------------------------------------------------------------------
\ Words that characterize a tasks state.
\ A pointer to one of them is always stored in the first field of a TCB
\ ----------------------------------------------------------------------

: go-next ( lfa -- lfa' )   [ H there 'go-next ! T ]
   @   dup RRLink @ = IF  drop Priority  THEN  do-next
;
: do-wake ( lfa -- )  [ H there 'do-wake ! T ]
   ['] go-next swap cell- st >r  ( R: new-task )  \ mark task as not ready to run
   TCB   dup r@ -                                 \ is the new task different from the running task?
   IF  Rsp @   Dsp @ rot #t-dsp + !               \ save RSP on stack and DSP in TCB
       r@ Tptr !                                  \ switch task
Label wake-up
       r@ #t-dsp + @ >dstack                      \ restore dsp, fill NOS & TOS
       >rstack gon EXIT                           \ restore rsp, fill TOR
   THEN
   rdrop drop gon                                 \ if myself nothing else to do
;
: force-wake ( lfa -- )  \ used by activate
   ['] go-next swap cell- st >r  ( R: new-task )  \ mark task as not ready to run
   TCB   dup r@ - 0= ?GOTO wake-up                \ is the new task not the running task?
   Rsp @   Dsp @ rot #t-dsp + !   r@ Tptr !       \ save RSP on stack and DSP in TCB and switch task
   GOTO wake-up
;
\ ----------------------------------------------------------------------
\             0      1      2      3     4      5
\ | do-priority | link | flag | time | max | task |
\ performance measurement: store Task of maximum run time
\ but exclude the Terminal task, because its time consumption is only
\ relevant during debugging.
\ After debugging, PERFORMANCE-MEASUREMENT can be set to False
\ ----------------------------------------------------------------------

PERFORMANCE-MEASUREMENT [IF]   \ cr .( with performance measurement )

  : do-priority  ( lfa -- lfa' )   [ H there 'do-priority ! T ]
  \  kick-watchdog          \ here the watchdog would be kicked in a real system
     True over cell+ st     \ set Prioflag @ #p-flag      ( -- lfa addr-flag )
     cell+ ld >r            \ fetch previous #p-time      ( -- lfa time )        ( R: addr-time )
     time   dup r@ !  swap -                              ( -- lfa delta-t )     ( R: addr-time )
     r> cell+ ld >r         \ fetch previous #p-max       ( -- lfa delta-t max ) ( R: addr-max )
     over - drop carry? IF  drop rdrop @ do-next  THEN ( -- lfa delta-t )     ( R: addr-max )
     r> st                  \ store #p-max                ( -- lfa addr-max )
     TCB swap cell+ st      \ store #p-task               ( -- lfa addr-task )
     r@ swap cell+ !        \ store #p-pc                 ( -- lfa )
     @ do-next
   ;

   : init-performance ( -- )   Priority #p-max + 3 cells erase ;

   init: init-priority ( -- )  time [ Priority #p-time + ] Literal ! ;

[ELSE]  \ cr .( without performance measurement )

  : do-priority  ( lfa -- lfa' )   [ H there 'do-priority ! T ]
  \  kick-watchdog       \ here the watchdog would be kicked in a real system
     True over cell+ !   \ set Prioflag @ #p-flag      ( -- lfa addr-flag )
     @ do-next
  ;

[THEN]
\ ----------------------------------------------------------------------
\ Words used by an active task
\ ----------------------------------------------------------------------

: halt  ( -- )       goff  Prioflag @ IF  TCB #t-link +  ELSE  Priority  THEN  do-next ;

: pause ( -- )       ['] do-wake TCB ! halt ;

: activate ( task xt -- )
   over   dup >r
   #t-dsp + @ dsp>task              \ determine task number
   dup task>dsp 2 + r@ #t-dsp + !   \ initialize dsp for one element - rsp - on the stack
   task>rsp ['] halt swap st        \ put a "halt" into the bottom position of the return stack
   cell- st   >r                    \ and the word to be executed above
   #t-dsp + @ 1-                    \ compute "empty" dsp of new task
   dstack> >r   >dstack             \ save dsp and switch into stack of new task
   r> r> dup   rot >dstack drop     \ initialize its stack with 2 elements and switch back to own stack
   0 r@ #t-catch + !                \ initialize t-catch field
   ['] force-wake r> !              \ wake new task
;
\ ----------------------------------------------------------------------
\ Words used by an active task to manipulate another tasks
\ ----------------------------------------------------------------------

: wake  ( task -- )  ['] do-wake swap ! ;

: stop  ( task -- )  ['] go-next swap ! ;

\ ----------------------------------------------------------------------
\ do-robin is the code pointer of RoundRobin, which is the anchor
\ of the circular list of time-sharing tasks.
\
\ When a break condition or the break command has been detected,
\ the Terminal task will be stopped.
\ The Terminal task will resume operation when the break condition ends.
\ ----------------------------------------------------------------------

Variable Dsu  \ Dsu = true when umbilical active, false in break condition

init: init-Dsu ( -- )  #f-dsu flag? Dsu ! ;

: do-robin ( lfa -- lfa' )  [ H there 'do-robin ! T ]
   False Prioflag !
   dup @ cell+ @ dup rot !                              \ advance pointer into the circular task list to the next task in the list
   Dsu @   #f-dsu flag?   dup Dsu !
   IF  0= IF  Terminal ['] debug-service activate  THEN \ Break vanished, start the Terminal task
       do-next
   THEN \ #f-dsu not set
   IF  Terminal stop  THEN                              \ New break - Dsu was set. Stop the Terminal task
   do-next
;
\ ----------------------------------------------------------------------
\ Initialization of the minimal multitasking data structure consisting
\ of Priority, RoundRobin und the Terminal task.
\ ----------------------------------------------------------------------

                ' do-priority Priority #p-exec + !
                   RoundRobin Priority #p-link + !
                            0 Priority #p-flag + !
                            0 Priority #p-max + !
                            0 Priority #p-task + !
                            0 Priority #p-pc + !

                   ' do-robin RoundRobin #t-exec + !
                     Terminal RoundRobin #t-link + !

                    ' go-next Terminal #t-exec + !
                     Terminal Terminal #t-link + !
#tasks 1- ds_addr_width shift Terminal #t-dsp + !
                            0 Terminal #t-sema + !
                            0 Terminal #t-time + !
                            0 Terminal #t-poll + !
                            0 Terminal #t-catch + !

\ ----------------------------------------------------------------------
\ During debugging:
\
\ .tasks prints the status of all tasks.
\ .semas prints the status of all semaphors.
\
\ please note that the status of single tasks changes, while
\ they are printed. Only the sequence of tasks is a momentary
\ snapshot, because of task-links.
\ ----------------------------------------------------------------------

: .semas ; noexit  Host: .semas  ( -- )
   [ T Sema-link H ] Literal
   BEGIN  t_@ ?dup
   WHILE  cr   dup Variables .listname   &10 position
          dup t_@ ?dup IF  Variables .listname  ELSE  ." free "  THEN
          dup [ T #s-count H ] Literal + t_@ u. ." counts "
          [ T #s-link H ] Literal +
   REPEAT
;
: next-task?  ( task -- task' f )
   dup #t-link + @ swap
   RoundRobin - IF  dup RRLink @ -  EXIT THEN
   true
;
: task-links  ( -- t1 .. tn n )   Priority  1
   BEGIN  over next-task? WHILE  swap 1+  REPEAT  drop
;
: .task-exec ; noexit  Host: .task-exec  ( task xt -- task )
   'go-next      @ case? IF  ." go-next"                                           EXIT THEN
   'stopped      @ case? IF  ." error: " dup [ T #t-poll H ] Literal + t_@ .error  EXIT THEN
   'do-poll-tmax @ case? IF  ." poll-tmax"                                         EXIT THEN
   'do-time      @ case? IF  ." do-time"                                           EXIT THEN
   'do-wake      @ case? IF  ." do-wake"                                           EXIT THEN
   'do-poll      @ case? IF  ." do-poll"                                           EXIT THEN
   'do-priority  @ case? IF  ." do-priority"                                       EXIT THEN
   'do-robin     @ case? IF  ." do-robin"                                          EXIT THEN
   Colons .listname
;
: .task ; noexit   Host: .task  ( addr -- )
   dup Variables .listname           &15 position                       \ print taskname
\ RoundRobin | do-roundrobin | link |
   [ T RoundRobin H ] Literal case? IF ." ------------------------"  EXIT THEN
\ Priority   | do-priority   | link | flag | time  | max | task |
   dup [ T Priority H ] Literal = IF
      PERFORMANCE-MEASUREMENT [IF]  \ | do-priority | link | flag | time  | max | task |
         dup [ T #t-exec H ] Literal + t_@ [t'] do-priority - IF  drop  EXIT THEN
         dup [ T #p-task H ] Literal + t_@ variables .listname
         dup [ T #p-max  H ] Literal + t_@ . ." Ticks "
             [ T #p-pc   H ] Literal + t_@ ." PC " $.
      [ELSE]
         drop ." no performance measurement"
      [THEN]
   EXIT THEN
\ Tasks
   dup [ T #t-exec H ] Literal + t_@ T .task-exec H &25 position        \ print task exec
\ DSP
   dup [ T Terminal H ] Literal =
   IF  [ T Dsp H ] Literal  ELSE  dup [ T #t-dsp H ] Literal +  THEN
   ." DSP " t_@ $. &35 position                                         \ print task DSP
\ Semaphore ?
   [ T #t-sema H ] Literal + t_@ ?dup 0= ?EXIT
   ." waiting for " variables .listname                                 \ print semaphore
;
Host Command definitions Forth

: .tasks  ( -- )  dis-output
   [t'] task-links t_execute   t> ( #tasks )
   dup >r 0 ?DO  t>  LOOP
       r> 0 ?DO  cr T .task H LOOP
   T .semas H  std-output
;
Target

library task_lib.fs
