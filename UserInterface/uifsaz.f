c
c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIFSAZ ( SID, XSIZE, YSIZE )
c       ---------------------------------------------
c
c       PARAMETERS:
c
c       integer sid : INPUT - the identifier of the scrolling area whose
c                     size is to be set
c
c       integer xsize, ysize : INPUT - size of the scrolling area in
c                              device coordinates
c
c       Sets the size of the given scrolling area to the size requested.
c       If the scrolling area is showing, an error is generated, since a
c       showing scrolling area may not have its size changed.  If the
c       scrolling area window has been created, then its size is changed
c       if needed.
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uifsaz ( sid, xsize, ysize )
c     =============================================
c
      integer sid
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
c     Check system is initialised
c
      if ( uifisi ) then
c
c        Check scrolling area id is valid
c
         if ( sid .ge. MINSCR .and. sid .le. MAXSCR ) then
            if ( scrnam(sid) .ge. 0 ) then
c
c              Now check that the scrolling area is not showing
c
               if ( .not. scriss(sid) ) then
c
c                 Set the scrolling area size
c
                  scrxsi(sid) = xsize
                  scrysi(sid) = ysize
c
c                 Now we must destroy the scrolling area window (if
c                 created) so it will be created the correct size next
c                 time it is shown
c
                  if ( scrisc(sid) ) then
                     status = windes ( scrode(sid), scrwid(sid) )
c
c                    And mark the window as not created
c
                     scrisc(sid) = .FALSE.
                 endif
              else
c
c                Error - scrolling area is showing
c
                 uiferr = SCSHOW
                 status = .TRUE.
               endif
            else
c
c              Error - invalid scrolling area id
c
               uiferr = BADSCR
               status = .TRUE.
            endif
         else
c
c           Error - invalid scrolling area id
c
            uiferr = BADSCR
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
      uifsaz = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
