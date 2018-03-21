c
c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIFTAS ( EID, VNAME, LENGTH )
c       ----------------------------------------------
c
c       PARAMETERS:
c
c       integer eid : INPUT - the identifier of the textfield element
c                     whose contents are to be assigned to a variable.
c
c       character*(*) vname : INPUT - the name of the variable to which
c                             contents are to be assigned.
c
c       integer length : INPUT - the length of the variable name.
c
c       Assigns the contents of given textfield to the variable with
c       the given name.  The contents are converted to a numeric value,
c       and then assigned to the variable.  An error will be generated
c       if the contents cannot be converted to a numeric value.
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uiftas ( eid, vname, length )
c     ==============================================
c
      integer eid
      character*(*) vname
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
c     Contents value as real number
      real value
c
c     Physical index of store
      integer ind
c
c     CALLED FUNCTIONS:
c
c     Converts string to real number
      logical appval
c
c     Sets variable value
      logical appset
c
c     Gets physical index of store
      logical pindex
c
c     Check system is initialised
c
      if ( uifisi ) then
c
c        Check textfield id is valid
c
         if ( eid .ge. MINTEX .and. eid .le. MAXTEX
     +        .and. texnam(eid) .ge. 0 ) then
c
c           Get the value of the contents of the field
c
            if ( texcon(eid) .gt. 0 ) then
               if ( texcle(eid) .gt. 0 ) then
                  status = pindex ( texcon(eid), ind )
                  if ( .not. status ) then
                     status = appval ( cstore(ind:),
     +                                 texcle(eid), value )
                     if ( .not. status ) then
c
c                       OK, set the variable to the value
c
                        status = appset ( vname, length, value )
                        if ( status ) uiferr = VARSET
                     else
c
c                       Error - can't translate contents to value
c
                        uiferr = NOTNUM
                     endif
                  endif
               else
c
c                 Error - no contents, so cant translate
c
                  uiferr = NOTNUM
                  status = .TRUE.
               endif
            else
c
c              Error - no contents, so cant translate
c
               uiferr = NOTNUM
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
      uiftas = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
