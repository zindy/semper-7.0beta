c
c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIXIPB ( CLOSED, DONE )
c       ----------------------------------------
c
c       PARAMETERS:
c
c       integer closed : INPUT - the switch number which closed.
c
c       logical done : OUTPUT - 'all done' flag.
c
c       Processes raw switch queue events.  If the mouse button pressed
c       has an action defined for it, this action is queued as an event
c       for exectuion.  Otherwise, if there is an element currently
c       under the pointer, then a select ation is generated on it,
c       taking account of the mouse button used and any active devices.
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uixipb ( closed, done )
c     ========================================
c
      integer closed
      logical done
c
      include 'UIFCOM'
      include 'UIXCOM'
      include 'UIFERR'
c
c     LOCAL VARIABLES:
c
c     Return status
      logical status
c
c     CALLED FUNCTIONS:
c
c     Checks whether action is OK wrt mandatory panels
      logical uixicm
c
c     First check to see if the mouse button pressed has an action
c     defined for it: if so, this wins!
c
      status = .FALSE.
      if ( closed .eq. lbnumb .and. lmbact .gt. 0 ) then
         call uixevp ( EVACTI, lmbact )
         done = .TRUE.
      else if ( closed .eq. cbnumb .and. cmbact .gt. 0 ) then
         call uixevp ( EVACTI, cmbact )
         done = .TRUE.
      else if ( closed .eq. rbnumb .and. rmbact .gt. 0 ) then
         call uixevp ( EVACTI, rmbact )
         done = .TRUE.
      else
c
c        Check to see if mandatory panel is showing, and if so,
c        whether we are over it (or one of its menu panels)
c
         if ( uixicm () ) then ! 1 ) ) then
c
c           Check to see if we are over any elements - nowt to do if not
c
            if ( eidl(HOST) .ne. -1 ) then
c
c              Was over an element on the host.  Check to see if it was
c              over one on the framestore too
c
               if ( eidl(FSTORE) .eq. -1 ) then
c
c                 No.  Generate a select event on the element provided
c                 that the active device is not set to framestore only
c
                  if ( actdev .ne. FSTORE ) then
                     call uixevp ( EVSELE, eidl(HOST) )
                     done = .TRUE.
                  endif
               else
c
c                 Elements on both devices.  Check to see if we have
c                 been directed to an active device, otherwise use the
c                 buttons to decide which element to select
c
                  if ( actdev .ne. BOTH ) then
                     call uixevp ( EVSELE, eidl(actdev) )
                     done = .TRUE.
                  else
c
c                    Both active, so look at buttons
c
                     if ( closed .eq. lbnumb ) then
                        call uixevp ( EVSELE, eidl(HOST) )
                        done = .TRUE.
                     else if ( closed .eq. rbnumb ) then
                        call uixevp ( EVSELE, eidl(FSTORE) )
                        done = .TRUE.
                     endif
                  endif
               endif
            else if ( eidl(FSTORE) .ne. -1 ) then
c
c              Generate a select event on the element provided that the
c              active device is set to framestore only
c
               if ( actdev .eq. FSTORE ) then
                  call uixevp ( EVSELE, eidl(FSTORE) )
                  done = .TRUE.
               else
c
c                 Use the buttons to decide which element to select
c
                  if ( actdev .eq. BOTH ) then
c
c                    Both active, so look at buttons
c
                     call uixevp ( EVSELE, eidl(FSTORE) )
                     done = .TRUE.
                  endif
               endif
            endif
         else
c
c           Error - attempt to do something on another panel whilst
c           a mandatory panel is showing.  Dont return error, but
c           ring the bell
c
            status = .FALSE.
            call appbel
         endif
      endif
c
      uixipb = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
