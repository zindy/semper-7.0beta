c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIFENA ( EID, NAME, LENGTH )
c       ---------------------------------------------
c
c       PARAMETERS:
c
c       integer eid : INPUT - the identifier of the element which is
c                             to be named
c
c       character*(*) name : INPUT - the name of the element
c
c       integer length : INPUT - the length of the name
c
c       Sets the name of the element with identifier eid to the name
c       given.  A copy of the name is made into dynamic storage, and
c       the pointer to it stored for the element.  (If a name is already
c       stored, the space is first released).  Any element-type specific
c       actions are carried out (e.g. changing of positions, extents
c       etc.) depending on element type.
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uifena ( eid, name, length )
c     =============================================
c
      integer eid
      character*(*) name
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
c     Physical index of store
      integer ind
c
c     Identifier of any popup/pulldown menu panel
      integer mpid
c
c     CALLED FUNCTIONS:
c
c     Allocates store
      logical allocm
c
c     Frees store
      logical freem
c
c     Gets physical index of store
      logical pindex
c
c     Copies a string
      logical USTNCP
c
c     Recalculates size of a cell
      logical uixcsi
c
c     Recalculates size of a textfield
      logical uixtsi
c
c     Recalculates size of a menu
      logical uixmsi
c
c     Sets the name of a panel
      logical uifpna
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
c              Free any currently allocated space
c
               if ( elenam(eid) .gt. 0 ) then
                  status = freem ( elenam(eid) )
                  elenam(eid) = 0
               else
                  status = .FALSE.
               endif
c
c              Allocate new space, and copy the name in
c
               if ( .not. status ) then
                  if ( length .gt. 0 ) then
                     status = allocm ( length + 1, elenam(eid) )
                     if ( .not. status ) then
                        status = pindex ( elenam(eid), ind )
                        if ( .not. status ) then
                           status = USTNCP (cstore(ind:), name, length)
                        endif
                     endif
                  endif
                  if ( .not. status ) then
c
c                    Re-calculate size of element.
c
                     if ( eid .ge. MINCEL .and. eid .le. MAXCEL ) then
c
c                       Cell element
c
                        status = uixcsi ( eid )
                     else if (eid .ge. MINTEX.and.eid .le. MAXTEX) then
c
c                       Textfield element
c
                        status = uixtsi ( eid )
                     else if (eid .ge. MINMEN.and.eid .le. MAXMEN) then
c
c                       Menu element.
c                       If the menu panel is already created, then
c                       change it name too
c
                        mpid = menpan(eid)
                        if ( mpid .ne. -1 ) then
                            status = uifpna ( mpid, name, length )
                        endif
                        if ( .not. status ) then
                           status = uixmsi ( eid )
                        endif
                     endif
                  endif
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
      uifena = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
