c
c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIFIAF ( ACTION, LENGTH )
c       ------------------------------------------
c
c       PARAMETERS:
c
c       character*(*) action : INPUT - the after action to be set.
c
c       integer length : INPUT - the length of the action.
c
c       Sets the after action to that passed in.  The action given is
c       appened to any after action currently defined.
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uifiaf ( action, length )
c     ==========================================
c
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
c     Physical index of store of action
      integer ind
c
c     Length of combined old+new action
      integer len
c
c     Logical index of new action
      integer newaft
c
c     Physical index of old+new action
      integer newind
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
c     Gets length of a string
      integer USTLEN
c
c     Copies a string
      logical USTNCP
c
c     copies a string
      logical USTCPY
c
c     Concatenates a string
      logical USTNCA
c
c     Check system is initialised
c
      if ( uifisi ) then
c
c        Check to see if an after action is already defined
c
         if ( aftact .gt. 0 ) then
            if ( length .gt. 0 ) then
c
c              Yes, so get its length so we can append to it
c
               status = pindex ( aftact, ind )
               if ( .not. status ) then
                  len = USTLEN ( cstore(ind:) )
c
c                 Allocate space of the combination
c
                  len = len + length
                  status = allocm ( len + 1, newaft )
                  if ( .not. status ) then
c
c                    Copy the existing string, and append the new one.
c                    Note we get the old physical index again in case
c                    the allocation of more space has caused a garbage
c                    collect and move of the old contents
c
                     status = pindex ( aftact, ind )
                     if ( .not. status ) then
                        status = pindex ( newaft, newind )
                        if ( .not. status ) then
                           status = USTCPY ( cstore(newind:),
     +                                       cstore(ind:) )
                           if ( .not. status ) then
                              status = USTNCA ( cstore(newind:), action,
     +                                          length )
                              if ( .not. status ) then
c
c                                And free the exising memory
c
                                 status = freem ( aftact )
                                 aftact = newaft
                              endif
                           endif
                        endif
                     endif
                  endif
               endif
            else
               status = .TRUE.
            endif
         else
c
c           Just allocate space for the action
c
            if ( length .gt. 0 ) then
               status = allocm ( length + 1, aftact )
               if ( .not. status ) then
c
c                 And copy in the action
c
                  status = pindex ( aftact, ind )
                  if ( .not. status ) then
                     status = USTNCP ( cstore(ind:), action, length )
                  endif
               endif
            else
               status = .FALSE.
            endif
         endif
      else
c
c        Error - system not initialised
c
         uiferr = NOTINI
         status = .TRUE.
      endif
c
      uifiaf = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
