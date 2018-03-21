C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION UIXIPK ( ICH, DONE )
C       -------------------------------------
C
C       PARAMETERS:
C
C       integer ich : INPUT - the character from the keyboard
C                     event queue
C
C       logical done : OUTPUT - 'all done' flag.
C
C       Processes keyboard event queue data into UIF event queue data.
C
C       Function returns FALSE if sucessful, otherwise TRUE.
C
C----------------------------------------------------------------------
C
      logical function uixipk ( ich, done )
C     =====================================
C
      integer ich
      logical done
C
      include 'UIFCOM'
      include 'UIXCOM'
      include 'UIFERR'
      include 'ICSET'
C
C     LOCAL VARIABLES:
C
C     Return status
C
      logical status
C
C     CALLED FUNCTIONS:
C
C     Sets the value of an application variable
C
      logical appack
C
C     Check to see if there is a mandatory panel showing, and if so,
C     whether the currently selected textfield is on it
C
      if ( mpansh ) then
         status = .TRUE.
         if ( seltex .ne. -1 ) then
            if ( texopa(seltex) .eq. mpanid ) then
C
C              Yes, all is well
C
               status = .FALSE.
            endif
         else
            status = .FALSE.
         endif
      else
         status = .FALSE.
      endif
C
C     Split on the key pressed
C
      if ( .not. status ) then
         if ( ich .eq. KBKILL .or. ich .eq. KBDEL ) then
            call uixevp ( EVKBRD, ich )
            done = .TRUE.
         else if ( ich .eq. KBUP ) then
         else if ( ich .eq. KBDOWN ) then
         else if ( ich .eq. KBLEFT .or. ich .eq. KBRITE ) then
C
C           Left or right arrow - if there is a selected textfield,
C           generate a keyboard event, otherwise scan for a move.
C
            if ( seltex .ne. -1 ) then
               call uixevp ( EVKBRD, ich )
               done = .TRUE.
            endif
         else if ( ich .gt. KBFUNC ) then
C
C           Function key
C
         else if ( ich .eq. KBRET ) then
C
C           Return - generate an event for processing
C
            if ( seltex .ne. -1 ) then
               if ( texcle(seltex) .gt. 0 ) then
                  if ( texeac(seltex) .gt. 0 ) then
                     call uixevp ( EVACTI, texeac(seltex) )
                  else
                     call uixevp ( EVACTI, texcon(seltex) )
                  endif
C
C                 Set value of ENO to the id of the textfield
C
                  status = appack ( 8575, real ( seltex ) )
                  done = .TRUE.
               endif
C            else
C               if ( eidl(HOST) .ne. -1 ) then
C                  if ( actdev .eq. BOTH .or. actdev .eq. HOST ) then
C                     call uixevp ( EVSELE, eidl(HOST) )
C                     done = .TRUE.
C                  endif
C               endif
C               if ( eidl(FSTORE) .ne. -1 ) then
C                  if ( actdev .eq. BOTH .or. actdev .eq. FSTORE ) then
C                     call uixevp ( EVSELE, eidl(FSTORE) )
C                     done = .TRUE.
C                  endif
C               endif
C
            endif
         else
C
C           Ordinary character - insert
C
            call uixevp ( EVKBRD, ich )
            done = .TRUE.
         endif
      else
C
C        Error - attempt to interact with textfield not on mandatory
C        panel whist mandatory panel is showing.  Dont return an error,
C        but ring the bell
C
         status = .FALSE.
         call appbel
      endif
C
      uixipk = status
      return
C
C Copyright (C) 1988-1992:  Synoptics Ltd,  All Rights Reserved
C
      end
