\ ----------------------------------------------------------------------
\ @file : task_lib.fs
\ ----------------------------------------------------------------------
\
\ Last change: KS 16.07.2022 17:46:29
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
\   210     ks   02-May-2020  initial version
\   2200    ks   19-Oct-2020  library version, some simplifications
\   2400    ks   06-Jul-2022  Split up into multitask.fs and task_lib.fs
\ ----------------------------------------------------------------------
\ Semaphore methods
\ ----------------------------------------------------------------------
Target

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
     dup cell+   dup dec   @ IF  drop EXIT THEN
     semaphore-available pause
  ;
~ : unlock ( sema -- )   dup @ TCB - IF  #not-my-semaphore message  THEN force-unlock ;

~ : wait ( sema -- )  \ can be used with SIGNAL executed inside interrupt servers
     TCB over st   #s-count +            \ lock semaphor
     BEGIN  ld swap                      \ count > 0?
            IF  dec  0 swap !  EXIT THEN \ decrement count and release semaphor
            halt                         \ wait for signal event
     REPEAT
  ;
  : signal ( sema -- )   dup #s-count + inc   @ ?dup IF wake THEN ;

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
  ;
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
     0= ?GOTO go-next   GOTO do-wake
  ;
  : poll         ( xt -- )   ['] do-poll poll-exec ;

~ : do-time  ( lfa -- lfa' )  [ H there 'do-time ! T ]
     dup [ #t-time #t-link - ] Literal + @
     time? 0= ?GOTO go-next
     0 over [ #t-time #t-link - ] Literal + !    \ reset #t-time field
     GOTO do-wake
  ;
~ : do-poll-tmax  ( lfa -- lfa' )  [ H there 'do-poll-tmax ! T ]
     dup [ #t-poll #t-link - ] Literal + @ execute
     ?GOTO do-wake   do-time
  ;
  : poll-tmax    ( xt ticks -- f )  \ true when condition met, false when timeout
     time + #t-time tcb!   ['] do-poll-tmax poll-exec   #t-time tcb@ 0<>
  ;
\ ----------------------------------------------------------------------
\ Redefining forth_lib.fs' Time-reg based delays for the multitasker
\ ----------------------------------------------------------------------

  : continue   ( time -- )   #t-time tcb!  ['] do-time TCB !  halt ;

  : sleep      ( ticks -- )  ahead continue ; 

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
  ; \ The final EXIT will return to the caller of CATCH, because the return stack has
    \ been restored to the state that existed when CATCH was executed.
    
