c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIFPCO ( PID, FGC, BGC )
c       -----------------------------------------
c
c       PARAMETERS:
c
c       integer pid : INPUT - the identifier of the panel whose colour
c                             is to be set
c
c       integer fgc, bgc : INPUT - the foreground and background
c                          colours of the panel
c
c       Sets the foreground and background colours of the given panel to
c       the requested colours.  The forground and background colours of
c       the panel window are set.  The colours of the panel window are
c       set, if it has been created.
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uifpco ( pid, fgc, bgc )
c     =========================================
c
      integer pid
      integer fgc, bgc
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
c     Device panel is located on
      integer device
c
c     CALLED FUNCTIONS:
c
c     Sets window colours
      logical wincol
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
c                 Check the colours are valid
c
                  device = panode(pid)
                  if ( fgc .ge. 0 .and. fgc .lt. devnco(device) .and.
     +                 bgc .ge. 0 .and. bgc .lt. devnco(device) ) then
c
c                    Record the colours for the panel
c
                     panfgc(pid) = fgc
                     panbgc(pid) = bgc
c
c                    Set the panel window colours if window is created
c
                     if ( panisc(pid) ) then
                        status = wincol ( panode(pid), panwid(pid),
     +                                    fgc, bgc )
                     else
                        status = .FALSE.
                     endif
                  endif
c
c                 Check for errors
c
                  if ( status ) then
                     uiferr = COLOBJ
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
      uifpco = status
      return
      end
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
