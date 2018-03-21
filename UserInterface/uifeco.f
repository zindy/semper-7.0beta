c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIFECO ( EID, FGC, BGC )
c       -----------------------------------------
c
c       PARAMETERS:
c
c       integer eid : INPUT - the identifier of the element whose colour
c                             is to be set
c
c       integer fgc, bgc : INPUT - the foreground and background
c                                  colours of the element
c
c       Sets the foreground and background colours of the given element
c       to the requested colours.  If the panel owning the element is
c       showing, then the element is (re-)drawn.
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uifeco ( eid, fgc, bgc )
c     =========================================
c
      integer eid, fgc, bgc
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
c     Panel owning element
      integer pid
c
c     Device owning panel
      integer device
c
c     CALLED FUNCTIONS:
c
c     Repaints an element
      logical uifedr
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
c              Check colours are valid
c
               pid = eleopa(eid)
               device = panode(pid)
               if ( fgc .ge. 0 .and. fgc .lt. devnco(device) .and.
     +              bgc .ge. 0 .and. bgc .lt. devnco(device) ) then
c
c                 Save the colours
c
                  elefgc(eid) = fgc
                  elebgc(eid) = bgc
c
c                 If owning panel is showing, repaint the element
c
                  if ( paniss(pid) ) then
                     status = uifedr ( eid )
                  else
                     status = .FALSE.
                  endif
               else
c
c                 Invalid colours for device
c
                  uiferr = COLOBJ
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
      uifeco = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
