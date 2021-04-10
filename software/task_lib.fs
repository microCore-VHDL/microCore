\ ----------------------------------------------------------------------
\ @file : task_lib.fs
\ ----------------------------------------------------------------------
\
\ Last change: KS 10.04.2021 18:08:28
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
\ @brief : A co-operative multitasker library for microCore.
\ To use the multitasker, tasks_addr_width must be > 0 in
\ architecture_pkg.vhd
\
\ Version Author   Date       Changes
\   210     ks   02-May-2020  initial version
\   2200    ks   19-Oct-2020  library version, some simplifications
\ ----------------------------------------------------------------------
Target

True  Version PERFORMANCE-MEASUREMENT  \ when true, it is done at Priority

Host

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
\ Make OS performance observable with a scope.
\
\ gon should set an output signal,
\ goff should reset said output signal.
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

: Taskvariable ( <name> -- )  TaskVariables @ T Constant H   1 TaskVariables +! ;
: Task         ( <name> -- )  TaskVariables @   Tcreate   T allot H ;

T Variable Sema-link   0 Sema-link !        \ The anchor of a linked list of all semaphors

H : Semaphore  ( <name> -- )  Tcreate T here 0 , 0 , Sema-link @ , Sema-link ! H ;

T

Taskvariable #t-exec     \ task state = execution addr
Taskvariable #t-link     \ link to next task
Taskvariable #t-dsp      \ data stack address
Taskvariable #t-sema     \ waiting on semaphore
Taskvariable #t-time     \ waiting on Timer
Taskvariable #t-poll     \ waiting on specific polling routine / message number if halted
Taskvariable #t-catch    \ holds address of previous catch frame or zero

\ ----------------------------------------------------------------------
\ The data structure at Priority:
\             0      1      2      3      4      5    6
\ | do-priority | link | flag | time  | max | task | PC |
\ The fields MUST remain in this order for do-priority
\ ----------------------------------------------------------------------

Create Priority   7 allot
   0 Constant #p-exec
   1 Constant #p-link
   2 Constant #p-flag   \ set to true by do-priority, to false by do-robin, modifies PAUSE behaviour
   3 Constant #p-time
   4 Constant #p-max
   5 Constant #p-task
   6 Constant #p-pc
Priority #p-flag + Constant Prioflag

Target

~ Variable Tptr          \ points to the currently active task control block (TCB)

  Macro: TCB  ( -- task )   T Tptr @ H ;

  : tcb@      ( offset -- n )   Tcb + @ ;
  : tcb!      ( n offset -- )   Tcb + ! ;

  Task Terminal   Terminal Tptr !

  : dsp>task ( dsp -- task-nr )   [ ds_addr_width negate ] Literal shift ;

  : task>dsp ( task-nr -- dsp )   ds_addr_width shift ;

  : task>rsp ( task-nr -- rsp )   rs_addr_width shift #rstack + ;

\ ----------------------------------------------------------------------
\ The data structure at RoundRobin and RRLink:
\          0      1
\ | do-robin | link |
\ ----------------------------------------------------------------------

  Variable RoundRobin
  Variable RRLink

  Macro: do-next  ( task -- lfa ) T ld 1+ swap BRANCH H ;

\ ----------------------------------------------------------------------
\ Words that characterize a tasks state.
\ A pointer to one of them is always stored in the first field of a TCB
\ ----------------------------------------------------------------------

  : go-next ( lfa -- lfa' )   [ H there 'go-next ! T ]
     @   dup RRLink @ = IF  drop Priority  THEN  do-next
  ; noexit

  : do-wake ( lfa -- )  [ H there 'do-wake ! T ]
     ['] go-next swap 1- st >r  ( R: new-task )  \ mark task as not ready to run
     TCB   dup r@ -                              \ is the new task different from the running task?
     IF  Rsp @ Dsp @ rot #t-dsp + !              \ save RSP on stack and DSP in TCB
         r@ Tptr !                               \ switch task
  Label wake-up
         #t-dsp r@ + @ >dstack                   \ restore dsp, fill TOS, NOS
         >rstack gon EXIT                        \ restore rsp, fill TOR
     THEN
     rdrop drop gon                              \ if myself nothing else to do
  ;
  : force-wake ( lfa -- )  \ used by activate
     ['] go-next swap 1- st >r  ( R: new-task )  \ mark task as not ready to run
     TCB   dup r@ -                              \ is the new task not the running task?
     IF  Rsp @ Dsp @ rot #t-dsp + !              \ save RSP on stack and DSP in TCB
         r@ Tptr !                               \ switch task
     THEN
     GOTO wake-up
  ; noexit
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
     \  kick-watchdog       \ here the watchdog would be kicked in a real system
        True over 1+ st     \ set Prioflag @ #p-flag      ( -- lfa addr-flag )
        1+ ld >r            \ fetch previous #p-time      ( -- lfa time )        ( R: addr-time )
        time   dup r@ !  swap -                           ( -- lfa delta-t )     ( R: addr-time )
        r> 1+ ld >r         \ fetch previous #p-max       ( -- lfa delta-t max ) ( R: addr-max )
        over - drop carry? IF  drop rdrop @ do-next  THEN ( -- lfa delta-t )     ( R: addr-max )
        r> st               \ store #p-max                ( -- lfa addr-max )
        TCB swap 1+ st      \ store #p-task               ( -- lfa addr-task )
        r@ swap 1+ !        \ store #p-pc                 ( -- lfa )
        @ do-next
     ; noexit
  
     : init-performance ( -- )   Priority #p-max + 3 erase ;
  
     init: init-priority ( -- )  time [ Priority #p-time + ] Literal ! ;

  [ELSE]  \ cr .( without performance measurement )
  
    : do-priority  ( lfa -- lfa' )   [ H there 'do-priority ! T ]
    \  kick-watchdog       \ here the watchdog would be kicked in a real system
       True over 1+ !      \ set Prioflag @ #p-flag      ( -- lfa addr-flag )
       @ do-next
    ; noexit
  
  [THEN]

\ ----------------------------------------------------------------------
\ Words used by an active task
\ ----------------------------------------------------------------------

  : pause ( -- )       ['] do-wake TCB ! ; noexit  \ fall into halt

  : halt  ( -- )       goff Prioflag @ IF  TCB #t-link +  ELSE  Priority  THEN  do-next ; noexit

  : activate ( task xt -- )
     over   dup >r
     #t-dsp + @ dsp>task              \ determine task number
     dup task>dsp 2 + r@ #t-dsp + !   \ initialize dsp for one element - rsp - on the stack
     task>rsp ['] halt swap st        \ put a "halt" into the bottom position of the return stack
     1- st   >r                       \ and the word to be executed above
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
     dup @ 1+ @ dup rot !                                   \ advance pointer into the circular task list to the next task in the list
     Dsu @   #f-dsu flag?   dup Dsu !
     IF  0= IF  Terminal ['] debug-service activate  THEN    \ Break vanished, start the Terminal task
         do-next
     THEN \ #f-dsu not set
     IF  Terminal stop  THEN                                \ New break, stop the Terminal task
     do-next
  ; noexit

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
\ The data structure of Semaphores:
\            0       1           2
\ | owner-task | count | Sema-link |
\ ----------------------------------------------------------------------

  0 Constant #s-task   \ must remain at the first position
  1 Constant #s-count
  2 Constant #s-link

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
     dup Variables .listname           &15 position                         \ print taskname
  \ RoundRobin | do-roundrobin | link |
     [ T RoundRobin H ] Literal case? IF ." --------------------------"  EXIT THEN
  \ Priority   | do-priority   | link | flag | time  | max | task |
     dup [ T Priority H ] Literal = IF
        T PERFORMANCE-MEASUREMENT H [IF]  \ | do-priority | link | flag | time  | max | task |
           dup [ T #t-exec H ] Literal + t_@ [t'] do-priority - IF  drop  EXIT THEN
           dup [ T #p-task H ] Literal + t_@ variables .listname
           dup [ T #p-max  H ] Literal + t_@ . ." Ticks "
               [ T #p-pc   H ] Literal + t_@ ." PC " $.
      [ELSE]
         drop ." no performance measurement"
      [THEN]
   EXIT THEN
  \ Tasks
     dup [ T #t-exec H ] Literal + t_@ T .task-exec H &25 position          \ print task exec
     dup [ T #t-dsp  H ] Literal + t_@ ." DSP " $. &35 position           \ print task DSP
     [ T #t-sema H ] Literal + t_@ ?dup 0= ?EXIT
     ." waiting on " variables .listname
  ;
  Host Command definitions Forth

  : .semas  ( -- )  dis-output T .semas H std-output ;

  : .tasks  ( -- )  dis-output
     [t'] task-links t_execute   t> ( #tasks )
     dup >r 0 ?DO  t>  LOOP
         r> 0 ?DO  cr T .task H LOOP
     std-output
  ;
  Target
\ ----------------------------------------------------------------------
\ Semaphore methods
\ ----------------------------------------------------------------------

~ : lock ( sema -- )
     BEGIN  ld swap dup IF  TCB -  THEN
     WHILE  dup #t-sema tcb!   halt
     REPEAT
     TCB swap #s-task + st
     [ #s-count #s-task - ] Literal + inc
  ;
~ : stopped ( lfa -- lfa' )   [ H there 'stopped ! T ]   go-next ;

  : message  ( n -- )  \ redefine message
     TCB Terminal = IF  message  EXIT THEN
     #t-poll tcb!   ['] stopped TCB !   halt  \ msg# -> t-poll, stopped -> t-exec
  ;
~ : semaphore-available ( sema -- )
     >r   Priority
     BEGIN  next-task?
     WHILE  dup RoundRobin = IF  #t-link + @  THEN
            dup #t-sema + @ r@ =
            IF  dup wake  0 over #t-sema + !  THEN
     REPEAT  drop
     0 r> #s-task + !
  ;
~ : force-unlock  ( sema -- )
     dup 1+   dup dec @ IF  drop EXIT THEN
     semaphore-available pause
  ;
~ : unlock ( sema -- )   dup @ TCB - IF  #not-my-semaphore message  THEN force-unlock ;

~ : wait ( sema -- )  \ can be used with SIGNAL executed inside interrupt servers
     TCB over st   #s-count +            \ lock semaphor
     BEGIN  ld swap                      \ count > 0?
            IF  dec  0 swap !  EXIT THEN \ decrement count and release semaphor
            halt                         \ wait for signal event
     REPEAT
  ; noexit

  : signal ( sema -- )   dup #s-count + inc   @ ?dup 0= ?EXIT wake ;

\ ----------------------------------------------------------------------
\ Task management
\ ----------------------------------------------------------------------

~ : task-used?  ( task-number -- f )
     >r   Priority #t-link + @
     BEGIN  dup RoundRobin = IF  #t-link + @  THEN
            #t-dsp + ld swap dsp>task r@ = IF  rdrop EXIT THEN
            [ #t-link #t-dsp - ] Literal + @   dup RRLink @ =
     UNTIL  drop rdrop False
  ;
  : get-task-number ( -- task-number )
     #tasks  BEGIN  1-   dup task-used? 0= ?EXIT  dup 0= UNTIL
     #all-tasks-busy message
  ;
  : schedule ( task newtask -- )      \ link newtask into task list after task
     ['] go-next over !   >r
     get-task-number task>dsp r@ #t-dsp + !
     dup #t-link + @   r@ #t-link + !  r@ swap #t-link + !
     0 r> #t-sema + !
  ;
~ : spawn  ( task newtask xt -- )  >r tuck schedule r> activate ;

~ : unlock-semaphores  ( task -- ) \ used by deactive to free semaphores owned by task
     >r   Sema-link @
     BEGIN  ?dup
     WHILE  dup #s-task + @ r@ =
            IF  1 over #s-count + !   dup force-unlock  THEN
            #s-link + @
     REPEAT  rdrop
  ;
  : deactivate ( task -- )
     ['] go-next over !
     dup unlock-semaphores
     0 over #t-sema + !
     TCB - ?EXIT
     TCB #t-link +
     GOTO go-next
  ; noexit

~ : previous-task  ( task -- task-1 )
     >r  Priority
     BEGIN  dup RRLink @ = IF  drop rdrop #task-not-linked message  EXIT THEN
            dup #t-link + @
            over RoundRobin = IF  nip dup #t-link + @  THEN
            r@ = IF  rdrop  EXIT THEN  #t-link + @
     REPEAT
  ;
  : cancel  ( task -- )
     dup previous-task
     over #t-link + @ swap #t-link + !                  \ link task out of task list
     dup RRLink @ = IF  dup #t-link + @ RRLink !  THEN  \ Move RoundRobin Pointer as well
     deactivate
  ;
\ ----------------------------------------------------------------------
\ Polling services without and with time limit.
\
\ Polling services have been added for performance reasons, because
\ a task does not need to be woken up in order to check for an event.
\
\ xt ist the execution token of a word, that checks some condition
\ returning True when the condition is met.
\ ----------------------------------------------------------------------

~ : poll-exec    ( xt t-exec -- )   TCB st   #t-poll + !   halt ;

~ : do-poll       ( lfa -- lfa' )   [ H there 'do-poll ! T ]
     dup [ #t-poll #t-link - ] Literal + @ execute
     0= ?GOTO do-wake   GOTO go-next
  ; noexit

  : poll         ( xt -- )   ['] do-poll poll-exec ;

~ : do-poll-tmax  ( lfa -- lfa' )  [ H there 'do-poll-tmax ! T ]
     dup [ #t-poll #t-link - ] Literal + @ execute
     ?GOTO do-wake
  ; noexit \ fall into do-time

  : do-time  ( lfa -- lfa' )  [ H there 'do-time ! T ]
     dup [ #t-time #t-link - ] Literal + @
     time? 0= ?GOTO go-next
     0 over [ #t-time #t-link - ] Literal + !    \ reset #t-time field
     GOTO do-wake
  ; \ noexit

  : poll-tmax    ( xt ticks -- f )  \ true when condition met, false when timeout
     time + #t-time tcb!   ['] do-poll-tmax poll-exec   #t-time tcb@ 0<>
  ;
\ ----------------------------------------------------------------------
\ Redefining forth_lib.fs' Time-reg based delays for the multitasker
\ ----------------------------------------------------------------------

  : sleep      ( ticks -- )  ahead | ; noexit  \ fall into continue

  : continue   ( time -- )   #t-time tcb!  ['] do-time TCB !  halt ;

\ ----------------------------------------------------------------------
\ Catch and throw for a multitasking environment
\ ----------------------------------------------------------------------

~ : rstack>  ( -- rsp )   Rsp @ ;    \ A call in order to push TOR into the return stack memory

~ : catch ( xt -- error# | 0 )       \ Return address is already on rstack
     Dsp @ >r               ( xt )   \ Save data stack pointer, xt fills NOS slot
     #t-catch tcb@ >r       ( xt )   \ Save previous #t-catch
     rstack> #t-catch tcb!  ( xt )   \ Fill TOR and set #t-catch to RSP
     execute                (  )     \ Execute the word passed on the stack
     r> #t-catch tcb!       (  )     \ Restore previous #t-catch
     rdrop                  (  )     \ Discard saved stack pointer
     0                      ( 0 )    \ Signify normal completion
  ;
  : throw ( error# -- )       \ Returns to context saved by CATCH
     ?dup 0= ?EXIT                   \ Don't throw 0
     #t-catch tcb@ ?dup 0= IF #catch-not-initialized message THEN
     >rstack           ( err# )      \ Return to saved return stack context
     r> #t-catch tcb!  ( err# )      \ Restore previous #t-catch
     r> swap >r        ( saved-dsp ) \ save err# temporarily on rstack
     >dstack r>        ( err# )      \ Change stack pointer
  ; \ EXIT will return to the caller of CATCH, because the return stack has
    \ been restored to the state that existed when CATCH was executed.
