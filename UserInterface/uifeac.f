c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIFEAC ( EID, TYPE, ACTION, LENGTH )
c       -----------------------------------------------------
c
c       PARAMETERS:
c
c       integer eid : INPUT - the identifier of the element whose action
c                             is to be set
c
c       integer type : INPUT - the type of action to be set.  Vaild
c                              values are BEGACT, CHAACT and ENDACT
c
c       character*(*) action : INPUT - the text of the action to set.
c
c       integer length : INPUT - the length of the action text.
c
c       Sets the action given to be the action to be taken when the
c       given type of action (BEGACT, CHAACT or ENDACT) occurs for the
c       given element.  If the action is a null string, and an action
c       already exists for the type and element, then it is removed.
c       The action string is stored to scratch disc, and the disc and
c       block identifiers and the action length are stored for the
c       element.
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uifeac ( eid, type, action, length )
c     =====================================================
c
      integer eid, type
      character*(*) action
      integer length
c
      include 'UIFCOM'
      include 'UIXCOM'
      include 'UIFERR'
      include 'STORECOM'
c
c     LOCAL VARIABLES:
c
c     Return status
      logical status
c
c     Physical index of storage
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
c     Gets physical index of storage
      logical pindex
c
c     Copies a string
      logical USTNCP
c
c     Check system is initialised
c
      if ( uifisi ) then
c
c        Check element id is valid
c
         if ( eid .ge. MINELE .and. eid .le. MAXELE ) then
            if ( elenam(eid) .ge. 0 ) then
c
c              Save action as appropriate
c
               if ( type .eq. BEGACT ) then
                  if ( elebac(eid) .gt. 0 ) then
c
c                    Free existing action string
c
                     status = freem ( elebac(eid) )
                     elebac(eid) = 0
                  else
                     status = .FALSE.
                  endif
c
c                 And save new action
c
                  if ( .not. status ) then
                     if ( length .gt. 0 ) then
                        status = allocm ( length + 1, elebac(eid) )
                        if ( .not. status ) then
c
c                          And copy in the name
c
                           status = pindex ( elebac(eid), ind )
                           if ( .not. status ) then
                              status = USTNCP ( cstore(ind:),
     +                                          action, length )
                           endif
                        endif
                     endif
                  endif
               else if ( type .eq. CHAACT ) then
                  if ( elecac(eid) .gt. 0 ) then
c
c                    Free existing action string
c
                     status = freem ( elecac(eid) )
                     elecac(eid) = 0
                  else
                     status = .FALSE.
                  endif
c
c                 And save new action
c
                  if ( .not. status ) then
                     if ( length .gt. 0 ) then
                        status = allocm ( length + 1, elecac(eid) )
                        if ( .not. status ) then
c
c                          And copy in the name
c
                           status = pindex ( elecac(eid), ind )
                           if ( .not. status ) then
                              status = USTNCP ( cstore(ind:),
     +                                          action, length )
                           endif
                        endif
                     endif
                  endif
               else if ( type .eq. ENDACT ) then
                  if ( eleeac(eid) .gt. 0 ) then
c
c                    Free existing action string
c
                     status = freem ( eleeac(eid) )
                     eleeac(eid) = 0
                  else
                     status = .FALSE.
                  endif
c
c                 And save new action
c
                  if ( .not. status ) then
                     if ( length .gt. 0 ) then
                        status = allocm ( length + 1, eleeac(eid) )
                        if ( .not. status ) then
c
c                          And copy in the name
c
                           status = pindex ( eleeac(eid), ind )
                           if ( .not. status ) then
                              status = USTNCP ( cstore(ind:),
     +                                          action, length )
                           endif
                        endif
                     endif
                  endif
               else
c
c                 Error - unknown action type
c
                  uiferr = BADACT
                  status = .TRUE.
               endif
            else
c
c              Error - invalid element id
c
               uiferr = BADELE
               status = .TRUE.
            endif
         else
c
c           Error - invalid element id
c
            uiferr = BADELE
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
      uifeac = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
