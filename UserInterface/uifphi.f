c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIFPHI ( PID )
c       -------------------------------
c
c       PARAMETERS:
c
c       integer pid : INPUT - the identifier of the panel to hide.
c
c       Hides a panel on screen.  If the panel is not transitory, an
c       error is generated, since a non-transitory panel cannot be
c       hidden. Puts the panel to the bottom of the panel stack for the
c       device, repaints the area it was occupying.  Hides the panel
c       window. If the panel is mandatory, records the fact that there
c       is a no mandatory panel showing for the device which owns the
c       panel.
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uifphi ( pid )
c     ===============================
c
      integer pid
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
c     Loop counter
      integer i
c
c     Device panel is located on
      integer device
c
c     CALLED FUNCTIONS:
c
c     Hides a window
      logical winhid
c
c     Deactivates a menu
      logical uifmda
c
c     Scans at a position to see whats there
      logical uixisd
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
c              Check the panel is showing, and its not a fixed panel
c
               status = .FALSE.
               if ( paniss(pid) .and. .not. panfix(pid) ) then
c
c                 Hide any showing menu panels
c
                  do 1 i = MINMEN, MAXMEN
                     if ( mennam(i) .ge. 0 ) then
                        if ( mentyp(i) .eq. PULLDN ) then
                           if ( menopa(i) .eq. pid ) then
                              status = uifmda( i )
                              if ( status ) goto 2
                           endif
                        endif
                     endif
1                 continue
2                 continue
c
c                 Hide the window
c
                  if ( .not. status ) then
                     device = panode(pid)
                     status = winhid ( device, panwid(pid) )
                     if ( .not. status ) then
c
c                       If this panel was mandatory, then mandatory
c                       panel no longer showing.
c
                        paniss(pid) = .FALSE.
                        if ( panman(pid) ) then
                           mpansh = .FALSE.
                           mpanid = -1
                        endif
c
c                       Adjust the panel position back for its
c                       positioning parameters
c
                        panxpo(pid) = panxpo(pid) +
     +                                (panxsi(pid) / 2) * panhor(pid)
                        panypo(pid) = panypo(pid) +
     +                                (panysi(pid) / 2) * panver(pid)
c
c                       Now re-scan at the current mouse position
c                       to make sure input doesn't get screwed
c
                        status = uixisd ( device, curxpo(device),
     +                                    curypo(device) )
c+++++++++++++++++++++++++++++
cc
cc                       Loop through the event queue, checking to see
cc                       if there are any events generated on elements
cc                       on the panel we have just hidden.  If there
cc                       are, remove them
cc
c                        do 3 i = 0, QSIZE - 1
c                           if ( evtype(i) .eq. EVENTR .or.
c     +                          evtype(i) .eq. EVSELE .or.
c     +                          evtype(i) .eq. EVLEFT ) then
c                              if ( elenam(evdata(i)) .ge. 0 ) then
c                                 if ( eleopa(evdata(i)) .eq. pid ) then
c                                    evtype(i) = EVNULL
c                                 endif
c                              endif
c                           endif
c3                       continue
c+++++++++++++++++++++++++++++
                    endif
                 endif
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
      uifphi = status
      return
      end
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIFPH2 ( PID )
c       -------------------------------
c
c       THIS IS A COPY OF UIFPHI CALLED BY UIFMDA TO PREVENT RECURSION
c
c       PARAMETERS:
c
c       integer pid : INPUT - the identifier of the panel to hide.
c
c       Hides a panel on screen.  If the panel is not transitory, an
c       error is generated, since a non-transitory panel cannot be
c       hidden. Puts the panel to the bottom of the panel stack for the
c       device, repaints the area it was occupying.  Hides the panel
c       window. If the panel is mandatory, records the fact that there
c       is a no mandatory panel showing for the device which owns the
c       panel.
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uifph2 ( pid )
c     ===============================
c
      integer pid
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
c     Loop counter
      integer i
c
c     Device panel is located on
      integer device
c
c     CALLED FUNCTIONS:
c
c     Hides a window
      logical winhid
c
c     Deactivates a menu
      logical uifmd2
c
c     Scans at a position to see whats there
      logical uixisd
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
c              Check the panel is showing, and its not a fixed panel
c
               status = .FALSE.
               if ( paniss(pid) .and. .not. panfix(pid) ) then
c
c                 Hide any showing menu panels
c
                  do 1 i = MINMEN, MAXMEN
                     if ( mennam(i) .ge. 0 ) then
                        if ( mentyp(i) .eq. PULLDN ) then
                           if ( menopa(i) .eq. pid ) then
                              status = uifmd2( i )
                              if ( status ) goto 2
                           endif
                        endif
                     endif
1                 continue
2                 continue
c
c                 Hide the window
c
                  if ( .not. status ) then
                     device = panode(pid)
                     status = winhid ( device, panwid(pid) )
                     if ( .not. status ) then
c
c                       If this panel was mandatory, then mandatory
c                       panel no longer showing.
c
                        paniss(pid) = .FALSE.
                        if ( panman(pid) ) then
                           mpansh = .FALSE.
                           mpanid = -1
                        endif
c
c                       Adjust the panel position back for its
c                       positioning parameters
c
                        panxpo(pid) = panxpo(pid) +
     +                                (panxsi(pid) / 2) * panhor(pid)
                        panypo(pid) = panypo(pid) +
     +                                (panysi(pid) / 2) * panver(pid)
c
c                       Now re-scan at the current mouse position
c                       to make sure input doesn't get screwed
c
                        status = uixisd ( device, curxpo(device),
     +                                    curypo(device) )
c+++++++++++++++++++++++++++++
cc
cc                       Loop through the event queue, checking to see
cc                       if there are any events generated on elements
cc                       on the panel we have just hidden.  If there
cc                       are, remove them
cc
c                        do 3 i = 0, QSIZE - 1
c                           if ( evtype(i) .eq. EVENTR .or.
c     +                          evtype(i) .eq. EVSELE .or.
c     +                          evtype(i) .eq. EVLEFT ) then
c                              if ( elenam(evdata(i)) .ge. 0 ) then
c                                 if ( eleopa(evdata(i)) .eq. pid ) then
c                                    evtype(i) = EVNULL
c                                 endif
c                              endif
c                           endif
c3                       continue
c+++++++++++++++++++++++++++++
                    endif
                 endif
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
      uifph2 = status
      return
      end
c
c Copyright (C) 1988, 1989, 1991:  Synoptics Ltd,  All Rights Reserved
c
