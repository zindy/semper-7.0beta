c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIFPSI ( PID, XSIZE, YSIZE )
c       ---------------------------------------------
c
c       PARAMETERS:
c
c       integer pid : INPUT - the identifier of the panel whose size is
c                     to be set
c
c       integer xsize, ysize : INPUT - size of the panel in device
c                                      coordinates
c
c       Sets the size of the given panel to the size requested.  This
c       will be treated as the minimum size of the panel if the panel
c       is auto-sizing.  If the panel is showing, an error is
c       generated, since a showing panel may not have its size changed.
c       If the panel is auto-sizing, and the panel has any elements
c       placed on it, then the size given must be at least as big as the
c       current size of the panel, otherwise the panel size is not
c       changed.  If the panel window has been created, then its size is
c       changed if needed.
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uifpsi ( pid, xsize, ysize )
c     =============================================
c
      integer pid
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
c     Destroys a window
      logical windes
c
c
c     Check system is initialised
c
      if ( uifisi ) then
c
c        Check panel id is valid
c
         if ( pid .ge. MINPAN .and. pid .le. MAXPAN ) then
            if ( pannam(pid) .ge. 0 ) then
c
c              Now check that the panel is not showing
c
               if ( .not. paniss(pid) ) then
c
c                 OK.  Now need to check if the panel is auto-sizing
c                 before we set the size.  Can only do so it it ain't,
c                 or we are setting it bigger than it already is.
c
                  if ( .not. panaut(pid) ) then
c
c                    Not auto-size, so can do as we like
c
                     panexs(pid) = .TRUE.
                     panesx(pid) =  xsize
                     panesy(pid) =  ysize
                     panxsi(pid) = xsize
                     panysi(pid) = ysize
                  else
c
c                    Auto-size, so change only if bigger
c
                     panexs(pid) = .TRUE.
                     panesx(pid) =  xsize
                     panesy(pid) =  ysize
                     if ( xsize .gt. panxsi(pid) ) panxsi(pid) = xsize
                     if ( ysize .gt. panysi(pid) ) panysi(pid) = ysize
                  endif
c
c                 Now we must destroy the panel window (if created) so
c                 it will be created the correct size next time it is
c                 shown
c
                  if ( panisc(pid) ) then
                     status = windes ( panode(pid), panwid(pid) )
c
c                    And mark the window as not created
c
                     panisc(pid) = .FALSE.
                  else
                     status = .FALSE.
                  endif
               else
c
c                 Error - panel is showing
c
                  uiferr = PASHOW
                  status = .TRUE.
               endif
            else
c
c              Error - invalid panel id
c
               uiferr = BADPAN
               status = .TRUE.
            endif
         else
c
c           Error - invalid panel id
c
            uiferr = BADPAN
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
      uifpsi = status
      return
      end
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
