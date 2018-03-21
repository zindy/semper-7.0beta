c
c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIFMOB ( BUTTON, ACTION, LENGTH )
c       --------------------------------------------------
c
c       PARAMETERS:
c
c       integer button : INPUT - the button for which to set the action.
c
c       character*(*) action : INPUT - the action to set.
c
c       integer length : INPUT - the length of the action.
c
c       Sets the action to be carried out for the when the given mouse
c       button is pressed.  If the length is zero, action is unset.
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uifmob ( button, action, length )
c     ==================================================
c
      integer button
      character*(*) action
      integer length
c
      include 'UIFCOM'
      include 'UIXCOM'
      include 'UIFERR'
c
      include 'STORECOM'
c
c     LOCAL VARIABLES:
c
c     Return status
      logical status
c
c     Physical index of store
      integer ind
c
c     CALLED FUNCTIONS:
c
c     Frees dynamic memory
      logical freem
c
c     Allocates dynamic memory
      logical allocm
c
c     Gets physical index of store
      logical pindex
c
c     Copies a string
      logical USTNCP
c
c     Check system is initialised
c
      if ( uifisi ) then
c
c        Check the button is valid
c
         if ( button .eq. LEFT .or. button .eq. CENTRE .or.
     +        button .eq. RIGHT ) then
c
c           Check to see if we have to free any space
c
            if ( button .eq. LEFT ) then
               status = .FALSE.
               if ( lmbact .gt. 0 ) then
                  status = freem ( lmbact )
                  lmbact = 0
               endif
               if ( .not. status ) then
                  if ( length .gt. 0 ) then
c
c                    Allocate space for the action
c
                     status = allocm ( length + 1, lmbact )
                     if ( .not. status ) then
c
c                       And copy in the action
c
                        status = pindex ( lmbact, ind )
                        if ( .not. status ) then
                           status = USTNCP ( cstore(ind:), action,
     +                                       length )
                        endif
                     endif
                  endif
               endif
            else if ( button .eq. CENTRE ) then
               status = .FALSE.
               if ( cmbact .gt. 0 ) then
                  status = freem ( cmbact )
                  cmbact = 0
               endif
               if ( .not. status ) then
                  if ( length .gt. 0 ) then
c
c                    Allocate space for the action
c
                     status = allocm ( length + 1, cmbact )
                     if ( .not. status ) then
c
c                       And copy in the action
c
                        status = pindex ( cmbact, ind )
                        if ( .not. status ) then
                           status = USTNCP ( cstore(ind:), action,
     +                                       length )
                        endif
                     endif
                  endif
               endif
            else
               status = .FALSE.
               if ( rmbact .gt. 0 ) then
                  status = freem ( rmbact )
                  rmbact = 0
               endif
               if ( .not. status ) then
                  if ( length .gt. 0 ) then
c
c                    Allocate space for the action
c
                     status = allocm ( length + 1, rmbact )
                     if ( .not. status ) then
c
c                       And copy in the action
c
                        status = pindex ( rmbact, ind )
                        if ( .not. status ) then
                           status = USTNCP ( cstore(ind:), action,
     +                                       length )
                        endif
                     endif
                  endif
               endif
            endif
         else
c
c           Error - bad button
c
            uiferr = BADBUT
            status = .TRUE.
         endif
      else
c
c        Error - system not initialised
c
         uiferr = NOTINI
         status = .TRUE.
      endif
c
      uifmob = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
