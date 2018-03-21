C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION UIFPSH ( PID )
C       -------------------------------
C
C       PARAMETERS:
C
C       integer pid : INPUT - the identifier of the panel to show.
C
C       Shows a panel on screen.  If the panel window has not been
C       created, creates it (if the panel is auto, calculates the size
C       of the panel first).  If the panel window already exists, it is
C       assumed to have the correct contents and is just shown on
C       screen.  Adjusts the position of all elements on the panel to
C       take account of their positioning parameters.  Adjusts the panel
C       position to take account of its positioning parameters.  Creates
C       the panel window, shows it, and paints all the elements on it.
C       If the panel is mandatory, records the fact that there is a
C       mandatory panel showing.  Sets the value of application variable
C       PID to the value of pid supplied.
C
C       Function returns FALSE if sucessful, otherwise TRUE.
C
C----------------------------------------------------------------------
C
      logical function uifpsh ( pid )
C     ===============================
C
      integer pid
C
      include 'UIFCOM'
      include 'UIXCOM'
      include 'UIFERR'
      include 'STORECOM'
C
C     LOCAL VARIABLES:
C
C     Return status
C
      logical status
C
C     Device window to be shown sits on
C
      integer device
C
C     Physical index of store
C
      integer ind
C
C     Length of panel name string
C
      integer length
C
C     Identifier of panel window
C
      integer wid
C
C     Position and size of panel
C
      integer xpos, ypos, xsize, ysize
C
C     'Really showed panel' flag
C
      logical didsho
C
C     CALLED FUNCTIONS:
C
C     Checks if its OK to show a panel wrt any mandatory panels
C
      logical uixmpc
C
C     Gets physical index of store
C
      logical pindex
C
C     Creates a window
C
      logical wincre
C
C     Sets window colours
C
      logical wincol
C
C     Clears a window
C
      logical winclr
C
C     Shows a window
C
      logical winsho
C
C     Draws the contents of a panel
C
      logical uixpdr
C
C     Sets the value of an application variable
C
      logical appack
C
C     Finds length of string
C
      integer USTLEN
C
C     Scans a position to check panel and element there
C
      logical uixisd
C
C     Check system is initialised
C
      status = .FALSE.
      didsho = .FALSE.
      if ( uifisi ) then
C
C        Check its a valid panel id
C
         if ( pid .ge. MINPAN .and. pid .le. MAXPAN ) then
            if ( pannam(pid) .ge. 0 ) then
C
C              Check to see if a mandatory panel is already showing -
C              if so, can't show another
C
               device = panode(pid)
               if ( uixmpc ( pid ) ) then
C
C                 See if panel is showing - do nothing if so
C
                  if ( .not. paniss(pid) ) then
C
C                    See if the window is already created.
C
                     didsho = .TRUE.
                     if ( .not. panisc(pid) ) then
C
C                       Get size and position of the panel
C
                        call uixpsp ( pid, xpos, ypos, xsize, ysize )
C
C                       Save the position and size of the panel
C
                        panxpo(pid) = xpos
                        panypo(pid) = ypos
                        panxsi(pid) = xsize
                        panysi(pid) = ysize
C
C                       Create the window
C
                        if ( pannam(pid) .gt. 0 ) then
                           status = pindex ( pannam(pid), ind )
                           length = USTLEN ( cstore(ind:) )
                        else
                           ind    = 1
                           length = 0
                        endif
                        status = wincre ( panode(pid), pid,
     +                                    cstore(ind:), length,
     +                                    xpos, ypos,
     +                                    xsize, ysize, wid )
                        if ( .not. status ) then
C
C                          Set window background and foreground colours
C
                           panwid(pid) = wid
                           panisc(pid) = .TRUE.
                           status = wincol ( device, wid, panbgc(pid),
     +                                       panfgc(pid) )
                           if ( .not. status ) then
C
C                             Show the window
C
                              status = winsho ( device, wid )
                              if ( .not. status ) then
C
C                                Clear the window
C
                                 status = winclr ( device, wid,
     +                                             panbgc(pid), 1, 1,
     +                                             xsize-2, ysize-2 )
                                 if ( .not. status )  then
C
C                                   And draw its contents
C
                                    paniss(pid) = .TRUE.
                                    status = uixpdr ( pid, 1, 1,
     +                                                xsize-2, ysize-2 )
C
C                                   Record the fact that a mandatory
C                                   panel is showing if needed
C
                                    if ( .not. status ) then
                                       if ( panman(pid) ) then
                                          mpansh = .TRUE.
                                          mpanid = pid
                                       endif
C
C                                      And set the value of application
C                                      variable PNO to the panel we have
C                                      just shown
C
                                       status = appack ( 26175,
     +                                                   real(pid) )
                                    endif
                                 endif
                              endif
                           endif
                        endif
                     else
C
C                       Panel window exists: just show the window
C                       Note we have to mark it as showing first so
C                       that dumb windows work properly
C
                        panxpo(pid) = panxpo(pid) -
     +                                (panxsi(pid)/ 2) * panhor(pid)
                        panypo(pid) = panypo(pid) -
     +                                (panysi(pid)/ 2) * panver(pid)
                        paniss(pid) = .TRUE.
                        status = winsho ( device, panwid(pid) )
                        if ( status ) paniss(pid) = .FALSE.
C
C                       Record the fact that a mandatory panel is
C                       showing if needed
C
                        if ( .not. status ) then
                           if ( panman(pid) ) then
                              mpansh = .TRUE.
                              mpanid = pid
                           endif
C
C                          And set the value of application variable
C                          PNO to the panel we have just shown
C
                           status = appack ( 26175, real(pid) )
                        endif
                     endif
                  endif
C
C                 Check for error
C
                  if ( status ) then
C                     uiferr = SHOPAN
C
                  endif
               else
C
C                  Error - mandatory panel already showing
C
                  uiferr = MANPAN
                  status = .TRUE.
               endif
            else
C
C              Error - invalid panel id
C
               uiferr = BADPAN
               status = .TRUE.
            endif
         else
C
C           Error - invalid panel id
C
            uiferr = BADPAN
            status = .TRUE.
         endif
      else
C
C        Error - system not initialised
C
         uiferr = NOTINI
         status = .TRUE.
      endif
C
C     If it worked, scan the current position for current panel &
C     element
C
      if ( .not. status ) then
         if ( didsho ) then
            status = uixisd ( device, curxpo(device), curypo(device) )
         endif
      endif
C
      uifpsh = status
      return
C
C Copyright (C) 1988-1992:  Synoptics Ltd,  All Rights Reserved
C
      end
C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION UIXMPC ( PID )
C       -------------------------------
C
C       PARAMETERS:
C
C       integer pid : INPUT - the identifier of the panel to show.
C
C       Checks that it OK to show the given panel wrt to any mandatory
C       panels which may already be showing.
C
C       Function returns TRUE if OK to show, otherwise FALSE
C
C----------------------------------------------------------------------
C
      logical function uixmpc ( pid )
C     ===============================
C
      integer pid
C
      include 'UIFCOM'
      include 'UIXCOM'
C
C     LOCAL VARIABLES:
C
C     Loop counter
C
      integer i
C
C     If no mandatory panel is showing, then things must be OK
C
      if ( mpansh ) then
C
C        Ha. Mandatory panel showing.  We can, however, show this panel
C        if it is a menu panel, and the menu is on the mandatory one.
C
         uixmpc = .FALSE.
         do 10 i = MINMEN, MAXMEN
            if ( mennam(i) .ge. 0 ) then
               if ( menpan(i) .eq. pid ) then
                  if ( menopa(i) .eq. mpanid ) then
C
C                    Yup, we can show it
C
                     uixmpc = .TRUE.
                     goto 20
                  endif
               endif
            endif
   10    continue
   20    continue
      else
C
C        Ok, no mandatory panel showing
C
         uixmpc = .TRUE.
      endif
C
      return
C
C Copyright (C) 1988-1992:  Synoptics Ltd,  All Rights Reserved
C
      end
C
      subroutine uixpsp ( pid, xpos, ypos, xsize, ysize )
C     ===================================================
C
      integer pid, xpos, ypos, xsize, ysize
C
      include 'UIFCOM'
      include 'UIXCOM'
      include 'STORECOM'
C
C     LOCAL VARIABLES:
C
C     Return status
C
      logical dummy
C
C     Loop counters
C
      integer i
C
C     Physical index of store
C
      integer ind
C
C     Position of top left of the top-left-most element on the panel,
C     in panel coordinates
C
      integer minexp, mineyp
C
C     Bottom right position of an element when calculating auto-sizing
C
      integer elex, eley
C
C     CALLED FUNCTIONS:
C
C     Gets physical index of store
C
      logical pindex
C
C     Finds length of string
C
      integer USTLEN
C
C     Adjust the element position of all elements on the panel to take
C     account of their positioning parameters
C
      call uixepo ( pid, .TRUE. )
C
C     If panel is auto-sizing, must calculate its size
C
      xpos  = panxpo(pid)
      ypos  = panypo(pid)
      xsize = panxsi(pid)
      ysize = panysi(pid)
Cc
Cc     Adjust position back to take account of how panel has been
Cc     positioned previously
Cc
C      xpos = xpos + (xsize / 2) * panhor(pid)
C      ypos = ypos + (ysize / 2) * panver(pid)
C
      if ( panaut(pid) ) then
C
C        Loop over all elements, finding the top left one so we can
C        adjust element positions relative to it
C
         minexp = 32767
         mineyp = 32767
         do 10 i = MINELE, MAXELE
            if ( elenam(i) .ge. 0 ) then
               if ( eleopa(i) .eq. pid ) then
C
C                 Ignore popup menus...
C
                  if ( i .ge. MINMEN .and. i .le. MAXMEN ) then
                     if ( mentyp(i) .eq. POPUP ) then
                        goto 10
                     endif
                  endif
                  if ( elexpo(i) .lt. minexp ) then
                     minexp = elexpo(i)
                  endif
                  if ( eleypo(i) .lt. mineyp ) then
                     mineyp = eleypo(i)
                  endif
               endif
            endif
   10    continue
C
C        Adjust that so top left element appears at 1,1 in the panel
C
         minexp = minexp - 1
         mineyp = mineyp - 1
C
C        Loop over all elements on the panel, moving them according to
C        the minimum value we have just calculated, and calculate the
C        size of the panel
C
         if ( pannam(pid) .gt. 0 ) then
            dummy = pindex ( pannam(pid), ind )
            xsize = USTLEN ( cstore(ind:) )
         else
            xsize = 0
         endif
         ysize = 0
         do 20 i = MINELE, MAXELE
            if ( elenam(i) .ge. 0 ) then
               if ( eleopa(i) .eq. pid ) then
                  elexpo(i) = elexpo(i) - minexp
                  eleypo(i) = eleypo(i) - mineyp
C
C                 Ignore popup menus
C
                  if ( i .ge. MINMEN .and. i .le. MAXMEN ) then
                     if ( mentyp(i) .eq. POPUP ) then
                        goto 20
                     endif
                  endif
                  elex = elexpo(i) + elexsi(i) - 1
                  eley = eleypo(i) + eleysi(i) - 1
                  if ( elex .gt. xsize ) xsize = elex
                  if ( eley .gt. ysize ) ysize = eley
               endif
            endif
   20    continue
C
C        Only adjust panel size to that calculated if it is bigger
C        than the minmium set by user.
C
         xsize = xsize + 2
         ysize = ysize + 2
         if ( panexs(pid) ) then
            if ( xsize .lt. panesx(pid) ) xsize = panesx(pid)
            if ( ysize .lt. panesy(pid) ) ysize = panesy(pid)
         endif
      endif
C
C     Adjust position as needed to account for how panel is to
C     be positioned
C
      xpos = xpos - (xsize / 2) * panhor(pid)
      ypos = ypos - (ysize / 2) * panver(pid)
C
C     Adjust position of pulldown menu panels
C
      do 30 i = MINELE, MAXELE
         if ( elenam(i) .ge. 0 ) then
            if ( eleopa(i) .eq. pid ) then
               if ( i .ge. MINMEN .and. i .le. MAXMEN ) then
                  if ( mentyp(i) .eq. PULLDN ) then
                     if ( menpan(i) .ne. -1 ) then
                        panxpo(menpan(i)) = elexpo(i) + xpos
                        panypo(menpan(i)) = eleypo(i) + ypos
                     endif
                  endif
               endif
            endif
         endif
   30 continue
C
      return
C
C Copyright (C) 1988-1992:  Synoptics Ltd,  All Rights Reserved
C
      end
