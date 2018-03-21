C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION UIFINP ( TXTSTR, N )
C       -------------------------------------
C
C       PARAMETERS:
C
C       character*(*) txtstr : OUTPUT - command to be executed.
C
C       integer n : OUTPUT - the number of characters in the buffer.
C
C       Reads input from UIF, and returns the command for execution
C       to the application.  Checks first to see if any events are
C       queued, and if not goes off to scan the input queues for data.
C       Any events generated are then processed into commands.
C
C       Function returns FALSE if sucessful, otherwise TRUE.
C
C----------------------------------------------------------------------
C
      logical function uifinp ( txtstr, n )
C
C     =====================================
C
      character*(*) txtstr
      integer n
C
      include 'UIFCOM'
      include 'UIXCOM'
      include 'UIFERR'
      include 'EVENTS'
C
C     LOCAL VARIABLES:
C
C     Return status
C
      logical status
C
C     Devices we scan to see if they have panels
C
      integer device
C
C     Loop counter
C
      integer i
C
C     State of keyboard, pointer and button event queues on entry
C
      integer oldkey, oldpnt, oldbut
C
C     Dummy variable
C
      integer dummy
C
C     'All done' flag
C
      logical done
C
C     'The user broke' flag
C
      logical broke
C
C     CALLED FUNCTIONS
C
C     Enquires the state of an event queue
C
      logical eqnqre
C
C     Sets an event queue into a given state
C
      logical eqsets
C
C------------------------
C
C     Reads from an event queue
C      logical eqread
C------------------------
C
C     Performs real event scanning
C
      logical uixinp
C
C     Checks to see if any events are ready
C
      logical uixevr
C
C     Processes an event done to a command
C
      logical uixipe
C
C     Compresses dynamic memory
C
      logical compm
C
C     Check the system is running
C
      uiferr = WORKED
      if ( uifrun ) then
C
C        Compress dynamic memory
C
         status = compm ( )
C
C        See if there are any events queued ready to be dealt with:
C        if not go have a look
C
         if ( uixevr ( 1 ) ) then
C
C           Process the event to a command
C
   10       continue
            status = uixipe ( txtstr, n, done )
            if ( .not. done .and. uixevr ( 1 ) ) then
               goto 10
            endif
            if ( .not. done ) then
               n = 0
            endif
         else
C
C           Nothing queued, so go see whats happening.  Check the
C           current state of the pointer, button and keyboard queues.
C           Must do all three, but maintain error status
C
            status = eqnqre ( MPOINT, oldpnt, dummy, dummy )
            if ( .not. status ) then
               status = eqnqre ( MBUT, oldbut, dummy, dummy )
               if ( .not. status ) then
                  status = eqnqre ( MKEY, oldkey, dummy, dummy )
               else
                  status = eqnqre ( MKEY, oldkey, dummy, dummy )
                  status = .TRUE.
               endif
            else
               status = eqnqre ( MBUT, oldbut, dummy, dummy )
               status = eqnqre ( MKEY, oldkey, dummy, dummy )
               status = .TRUE.
            endif
            if ( .not. status ) then
C
C              Set the keyboard, pointer and button queues into
C              initialised state.  Break queue should already be
C              initialised.
C
               status = eqsets ( MKEY, QRUN )
               if ( .not. status ) then
                  status = eqsets ( MPOINT, QRUN )
                  if ( .not. status ) then
                     status = eqsets ( MBUT, QRUN )
                     if ( .not. status ) then
C
C                       Scan devices to see if they still have panels
C                       on
C
                        do 30 device = 1, NDEVIC
                           devhap(device) = .FALSE.
                           do 20 i = MINWIN, MAXWIN
                              if ( winnam(i) .ge. 0 ) then
                                 if ( winode(i) .eq. device ) then
                                    devhap(device) = .TRUE.
                                    goto 30
                                 endif
                              endif
   20                      continue
   30                   continue
C
C                       Position and turn the cursors on, and
C                       read the current mouse position
C
                        if ( devhap(HOST) ) then
                           call devcmo ( HOST, curxpo(HOST),
     +                                   curypo(HOST) )
                           call devcon ( HOST )
C
C------------------------
C                           status = eqread ( MPOINT, QSNAP, dummy,
C     +                           curmox(HOST), curmoy(HOST), dummy )
C------------------------
C
                        endif
                        if ( devhap(FSTORE) ) then
                           call devcmo ( FSTORE, curxpo(FSTORE),
     +                                   curypo(FSTORE) )
                           call devcon ( FSTORE )
C
C------------------------
C                           status = eqread ( MPOINT, QSNAP, dummy,
C     +                         curmox(FSTORE), curmoy(FSTORE), dummy )
C------------------------
C
                        endif
C
C                       See if we have any before or after actions
C                       defined and active - so, queue before action
C
                        done = .FALSE.
                        if ( uifaac ) then
C
C                          Any before actions defined?
C
                           if ( befact .gt. 0 ) then
                              call uixevp ( EVACTI, befact )
                           endif
                        endif
C
   40                   continue
C
C                          OK, we can now do the real business.
C                          Scan for input.
C
                           status = uixinp ( broke )
                           if ( .not. status .and. .not. broke ) then
C
C                             See if we now have any events queued.
C
                              if ( uixevr ( 1 ) ) then
C
C                                Process the event to a command
C
   50                            continue
                                 status = uixipe ( txtstr, n, done )
                                 if ( .not. status .and. .not. done
     +                                .and. uixevr ( 1 ) ) goto 50
                              endif
                           endif
                        if ( .not. status .and. .not. done
     +                       .and. .not. broke ) goto 40
C
C                       Now queue an after actions
C
                        if ( uifaac ) then
C
C                          Any after actions defined?
C
                           if ( aftact .gt. 0 ) then
                              call uixevp ( EVACTI, aftact )
                           endif
                        endif
C
C                       Turn the cursors off
C
                        call devcof ( HOST )
                        call devcof ( FSTORE )
                     endif
                  endif
               endif
            endif
C
C           Reset the queues. Note we must always reset all queues, but
C           maintain the error status correctly.
C
            if ( .not. status ) then
               status = eqsets ( MPOINT, oldpnt )
               if ( .not. status ) then
                  status = eqsets ( MBUT, oldbut )
                  if ( .not. status ) then
                     status = eqsets ( MKEY, oldkey )
                  else
                     status = eqsets ( MKEY, oldkey )
                     status = .TRUE.
                  endif
               else
                  status = eqsets ( MBUT, oldbut )
                  status = eqsets ( MKEY, oldkey )
                  status = .TRUE.
               endif
            else
               status = eqsets ( MPOINT, oldpnt )
               status = eqsets ( MBUT, oldbut )
               status = eqsets ( MKEY, oldkey )
               status = .TRUE.
            endif
         endif
      else
C
C        Error - system not running
C
         uiferr = NOTRUN
         status = .TRUE.
      endif
C
C     If UIF error, convert it to Semper error number, if error not set
C     by Semper already
C
      if ( status ) call uifape
C
C     All done
C
      uifinp = status
      return
C
C Copyright (C) 1988-1993:  Synoptics Ltd,  All Rights Reserved
C
      end
