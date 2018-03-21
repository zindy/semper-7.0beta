c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIFESI ( EID, XSIZE, YSIZE )
c       ---------------------------------------------
c
c       PARAMETERS:
c
c       integer eid : INPUT - the identifier of the element whose size
c                             is to be set
c
c       integer xsize, ysize : INPUT - size of the element in device
c                                      coordinates
c
c       Sets the size of the given element to the size requested.  This
c       will be treated as the minimum size of the element contents of
c       the element dictate that it must be larger.  If the panel window
c       for the panel owning the element has been created, it is
c       destroyed.  An error is generated if the panel owning the
c       element is showing on screen.
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uifesi ( eid, xsize, ysize )
c     ============================================
c
      integer eid
      integer xsize, ysize
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
c     Id of panel owning the element
      integer pid
c
c     CALLED FUNCTIONS:
c
c     Destroys a window
      logical windes
c
c     Check system is initialised
c
      if ( uifisi ) then
c
c        Check element id is valid
c
         if ( eid .ge. MINELE .and. eid .le. MAXELE
     +        .and. elenam(eid) .ge. 0 ) then
c
c           If the element is already bigger than the size required,
c           then don't set the size
c
            if ( xsize .gt. elexsi(eid) .and.
     +           ysize .gt. eleysi(eid) ) then
               elexsi(eid) = xsize
               eleysi(eid) = ysize
            endif
c
c           Record the fact that the element is explicitly sized
c
            eleexs(eid) = .TRUE.
c
c           And the minimum size
c
            eleesx(eid) = xsize
            eleesy(eid) = ysize
c
c           And destroy the window owning the element
c
            pid = eleopa(eid)
            if ( panisc(pid) ) then
               status = windes ( panode(pid), panwid(pid) )
               panisc(pid) = .FALSE.
            else
               status = .FALSE.
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
      uifesi = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
