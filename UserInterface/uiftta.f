c
c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIFTTA ( EID, TID )
c       ------------------------------------
c
c       PARAMETERS:
c
c       integer eid : INPUT - the identifier of the textfield element
c                     whose 'tab to' target is to be set
c
c       integer tid : INPUT - the identifer of the target textfield.
c
c       Sets, for textfield eid, the textfield to be selected if tab is
c       pressed while eid is selected.
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uiftta ( eid, tid )
c     ====================================
c
      integer eid, tid
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
c     Check system is initialised
c
      if ( uifisi ) then
c
c        Check both textfield ids are valid
c
         if ( eid .ge. MINTEX .and. eid .le. MAXTEX
     +        .and. texnam(eid) .ge. 0 ) then
            if ( tid .ge. MINTEX .and. tid .le. MAXTEX
     +           .and. texnam(tid) .ge. 0 ) then
c
c              OK, so set the target id.
c
               textaf(eid) = tid
               status = .FALSE.
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
      uiftta = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
