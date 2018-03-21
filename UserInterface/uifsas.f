c
c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIFSAS ( SID )
c       -------------------------------
c
c       PARAMETERS:
c
c       integer sid : INPUT - the identifier of the scrolling area to
c                     show.
c
c       Shows a scrolling area on screen.  If the scrolling area window
c       has not been created, creates it.  If the scrolling area window
c       already exists, it is assumed to have the correct contents and
c       is just shown on screen.  Adjusts the scrolling area position to
c       take account of its positioning parameters.  Creates the
c       scrolling area window, shows it, and repaints it.
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uifsas ( sid )
c     ===============================
c
      integer sid
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
c     Device window to be shown sits on
      integer device
c
c     Physical index of store
      integer ind
c
c     Length of scrolling area name string
      integer length
c
c     Identifier of scrolling area window
      integer wid
c
c     Position and size of scrolling area
      integer xpos, ypos, xsize, ysize
c
c     CALLED FUNCTIONS:
c
c     Gets physical index of store
      logical pindex
c
c     Creates a window
      logical wincre
c
c     Sets window colours
      logical wincol
c
c     Clears a window
      logical winclr
c
c     Shows a window
      logical winsho
c
c     Draws the contents of a scrolling area
      logical uixsad
c
c     Finds length of string
      integer USTLEN
c
c     Check system is initialised
c
      status = .FALSE.
      if ( uifisi ) then
c
c        Check its a valid scrolling area id
c
         if ( sid .ge. MINSCR .and. sid .le. MAXSCR ) then
            if ( scrnam(sid) .ge. 0 ) then
c
c              See if scrolling area is showing - do nothing if so
c
               if ( .not. scriss(sid) ) then
c
c                 See if the window is already created.
c
                  device = scrode(sid)
                  if ( .not. scrisc(sid) ) then
c
c                    Create the window
c
                     if ( scrnam(sid) .gt. 0 ) then
                        status = pindex ( scrnam(sid), ind )
                        length = USTLEN ( cstore(ind:) )
                     else
                        ind = 1
                        length = 0
                     endif
                     status = wincre ( scrode(sid), sid, cstore(ind:),
     +                                 length, xpos, ypos, xsize, ysize,
     +                                 wid )
                     if ( .not. status ) then
c
c                       Save the position and size of the scrolling area
c
                        scrxpo(sid) = xpos
                        scrypo(sid) = ypos
                        scrxsi(sid) = xsize
                        scrysi(sid) = ysize
c
c                       Set window background and foreground colours
c
                        scrwid(sid) = wid
                        scrisc(sid) = .TRUE.
                        status = wincol ( device, wid, scrbgc(sid),
     +                                    scrfgc(sid) )
                        if ( .not. status ) then
c
c                          Show the window
c
                           status = winsho ( device, wid )
                           if ( .not. status ) then
c
c                             Clear the window
c
                              status = winclr ( device, wid,
     +                                          scrbgc(sid), 1, 1,
     +                                          xsize - 2, ysize - 2 )
                              if ( .not. status )  then
c
c                                And draw its contents
c
                                 scriss(sid) = .TRUE.
                                 status = uixsad ( sid, 1, 1,
     +                                             xsize-2, ysize-2 )
c
                              endif
                           endif
                        endif
                     endif
                  else
c
c                    Scrolling area window already exists: just show the
c                    window.  Note we have to mark it as showing first
c                    so that dumb windows work properly
c
                     scriss(sid) = .TRUE.
                     status = winsho ( device, scrwid(sid) )
                     if ( status ) scriss(sid) = .FALSE.
                  endif
c
c                 Check for error
c
                  if ( status ) then
                    uiferr = SHOSCR
                  endif
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
      uifsas = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
