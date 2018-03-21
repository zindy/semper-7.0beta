c
c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIFTOU ( EID, STRING, LENGTH )
c       -----------------------------------------------
c
c       PARAMETERS:
c
c       integer eid : INPUT - the identifier of the numeric textfield
c                     element whose 'out of range' action is to be
c                     defined.
c
c       character*(*) string : INPUT - the string defining the action.
c
c       integer length : INPUT - the length of the action string.
c
c       Sets the action given to be the action to be taken when the
c       contents of the given numeric textfield element are out of
c       range.  If the action is a null string, and an action already
c       exists for the type and element, then it is removed.  The action
c       string is stored to scratch disc, and the disc and block
c       identifiers and the action length are stored for the element.
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uiftou ( eid, string, length )
c     ===============================================
c
      integer eid
      character*(*) string
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
c     Physical index of memory
      integer ind
c
c     CALLED FUNCTIONS:
c
c     Allocates memory
      logical allocm
c
c     Frees memory
      logical freem
c
c     Gets physical index of store
      logical pindex
c
c     Copies a string
      logical USTCPY
c
c     Check system is initialised
c
      if ( uifisi ) then
c
c        Check textfield id is valid
c
         if ( eid .ge. MINTEX .and. eid .le. MAXTEX ) then
            if ( texnam(eid) .ge. 0 ) then
c
c              Free any existing store for the action
c
               if ( texoor(eid) .gt. 0 ) then
                  status = freem ( texoor(eid) )
                  texoor(eid) = 0
               else
                  status = .FALSE.
               endif
c
c              Now allocates space for the action
c
               if ( .not. status ) then
                  if ( length .gt. 0 ) then
                     status = allocm ( length + 1, texoor(eid) )
c
c                    And copy in the action
c
                     if ( .not. status ) then
                        status = pindex ( texoor(eid), ind )
                        if ( .not. status ) then
                           status = USTCPY ( cstore(ind:), string )
                        endif
                     endif
                  endif
               endif
            else
c
c              Error - bad textfield id
c
               uiferr = BADTEX
               status = .TRUE.
            endif
         else
c
c           Error - bad textfield id
c
            uiferr = BADTEX
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
c     All done
c
      uiftou = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
