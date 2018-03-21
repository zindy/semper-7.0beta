C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION UIFPDE ( PID )
C       -------------------------------
C
C       PARAMETERS:
C
C       integer pid : INPUT - the identifier of the panel to delete
C
C       Destroys the panel with the given identifier.  Removes panel
C       from the panel stack for the device it was on, and if the panel
C       was showing on screen, removes it and repaints the screen area
C       it occupied.  Destroys the window which the panel occupied.
C       Destroys all the elements on the panel.  If the panel id is the
C       same as the current value of application variable PNO, then
C       PNO's value is unset.  All dynamic store used for the panel is
C       released. If the panel was mandatory, records the fact that
C       there is no longer a mandatory panel showing.
C
C       Function returns FALSE if sucessful, otherwise TRUE.
C
C----------------------------------------------------------------------
C
      logical function uifpde ( pid )
C     ===============================
C
      integer pid
C
      include 'UIFCOM'
      include 'UIXCOM'
      include 'UIFERR'
C
C     LOCAL VARIABLES:
C
C     Return status
C
      logical status
C
C     Loop counter
C
      integer i
C
C     Device panel is located on
C
      integer device
C
C     CALLED FUNCTIONS:
C
C     Destroys a panel
C
      logical uixpde
C
C     Scans for panels and elements at a position
C
      logical uixisd
C
C     Check system is initialised
C
      if ( uifisi ) then
C
C        Check panel id is valid
C
         if ( pid .ge. MINPAN .and. pid .le. MAXPAN ) then
            if ( pannam(pid) .ge. 0 ) then
C
C              First, walk along all elements on the panel, and destroy
C              and menu panels which should go
C
               status = .FALSE.
               do 10 i = MINMEN, MAXMEN
                  if ( mennam(i) .ge. 0 ) then
                     if ( menopa(i) .eq. pid ) then
                        if ( menpan(i) .ne. -1 ) then
                           status = uixpde ( menpan(i) )
                           menpan(i) = -1
                           if ( status ) goto 20
                        endif
                     endif
                  endif
   10          continue
   20          continue
               if ( .not. status ) then
C
C                 Now destroy the panel we were asked to
C
                  status = uixpde ( pid )
C
C                 If it was mandatory and showing, then say no
C                 mandatory panel now showing
C
                  if ( mpansh ) then
                     if ( mpanid .eq. pid ) then
                        mpansh = .FALSE.
                     endif
                  endif
               endif
C
C              Check for error
C
               if ( status ) then
                  uiferr = DESPAN
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
C     Now rescan so current values of panel and element are correct
C
      if ( .not. status ) then
         device = panode(pid)
         status = uixisd ( device, curxpo(device), curypo(device) )
      endif
C
      uifpde = status
      return
C
C Copyright (C) 1988-1992:  Synoptics Ltd,  All Rights Reserved
C
      end
C
      logical function uixpde ( pid )
C     ===============================
C
      integer pid
C
      include 'UIFCOM'
      include 'UIXCOM'
      include 'UIFERR'
C
C     LOCAL VARIABLES:
C
C     Return status
C
      logical status
C
C     Loop counter
C
      integer i
C
C     Device panel is located on
C
      integer device
C
C     CALLED FUNCTIONS:
C
C     Hides a window
C
      logical winhid
C
C     Destroys an element
C
      logical uifede
C
C     Sets the value of an application variable
C
      logical appack
C
C     Frees dynamic memory
C
      logical freem
C
C     Destroys a window
C
      logical windes
C
C     Check system is initialised
C
      if ( uifisi ) then
C
C        Check panel id is valid
C
         if ( pid .ge. MINPAN .and. pid .le. MAXPAN ) then
            if ( pannam(pid) .ge. 0 ) then
C
C              First hide the panel window
C
               device = panode(pid)
               status = .FALSE.
               if ( paniss(pid) ) then
                  status = winhid ( device, panwid(pid) )
                  paniss(pid) = .FALSE.
               endif
               if ( .not. status ) then
C
C                 Now loop destroying all the elements on the panel
C
                  do 10 i = MINELE, MAXELE
                     if ( elenam(i) .ge. 0 ) then
                        if ( eleopa(i) .eq. pid ) then
                           status = uifede ( i )
                           if ( status ) goto 20
                        endif
                     endif
   10             continue
   20             continue
                  if ( .not. status ) then
C
C                    Check to see if the variable PNO was set to this
C                    panel id - if so, must unset it
C
                     if ( curpid .eq. pid ) then
                        status = appack ( 26175, 0.0 )
                        curpid = 0
                     endif
                     if ( .not. status ) then
C
C                       Free space used by name string if allocated
C
                        if ( pannam(pid) .gt. 0 ) then
                           status = freem ( pannam(pid) )
                           pannam(pid) = 0
                        endif
                        if ( .not. status ) then
C
C                          Destroy the panel window
C
                           if ( panisc(pid) ) then
                              status = windes ( device,
     +                                          panwid(pid) )
                              panisc(pid) = .FALSE.
                           endif
                           if ( .not. status ) then
C
C                             And reset the name so that panel can
C                             be reused
C
                              pannam(pid) = -1
                           endif
                        endif
                     endif
                  endif
               endif
C
C              Check for error
C
               if ( status ) then
                  uiferr = DESPAN
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
      uixpde = status
      return
C
C Copyright (C) 1988-1992:  Synoptics Ltd,  All Rights Reserved
C
      end
C
      logical function uixpd2 ( pid )
C     ===============================
C
      integer pid
C
      include 'UIFCOM'
      include 'UIXCOM'
      include 'UIFERR'
C
C     LOCAL VARIABLES:
C
C     Return status
C
      logical status
C
C     Loop counter
C
      integer i
C
C     Device panel is located on
C
      integer device
C
C     CALLED FUNCTIONS:
C
C     Hides a window
C
      logical winhid
C
C     Destroys an element
C
      logical uifed2
C
C     Sets the value of an application variable
C
      logical appack
C
C     Frees dynamic memory
C
      logical freem
C
C     Destroys a window
C
      logical windes
C
C     Check system is initialised
C
      if ( uifisi ) then
C
C        Check panel id is valid
C
         if ( pid .ge. MINPAN .and. pid .le. MAXPAN ) then
            if ( pannam(pid) .ge. 0 ) then
C
C              First hide the panel window
C
               device = panode(pid)
               status = .FALSE.
               if ( paniss(pid) ) then
                  status = winhid ( device, panwid(pid) )
                  paniss(pid) = .FALSE.
               endif
               if ( .not. status ) then
C
C                 Now loop destroying all the elements on the panel
C
                  do 10 i = MINELE, MAXELE
                     if ( elenam(i) .ge. 0 ) then
                        if ( eleopa(i) .eq. pid ) then
                           status = uifed2 ( i )
                           if ( status ) goto 20
                        endif
                     endif
   10             continue
   20             continue
                  if ( .not. status ) then
C
C                    Check to see if the variable PNO was set to this
C                    panel id - if so, must unset it
C
                     if ( curpid .eq. pid ) then
                        status = appack ( 26175, 0.0 )
                        curpid = 0
                     endif
                     if ( .not. status ) then
C
C                       Free space used by name string if allocated
C
                        if ( pannam(pid) .gt. 0 ) then
                           status = freem ( pannam(pid) )
                           pannam(pid) = 0
                        endif
                        if ( .not. status ) then
C
C                          Destroy the panel window
C
                           if ( panisc(pid) ) then
                              status = windes ( device,
     +                                          panwid(pid) )
                              panisc(pid) = .FALSE.
                           endif
                           if ( .not. status ) then
C
C                             And reset the name so that panel can
C                             be reused
C
                              pannam(pid) = -1
                           endif
                        endif
                     endif
                  endif
               endif
C
C              Check for error
C
               if ( status ) then
                  uiferr = DESPAN
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
      uixpd2 = status
      return
C
C Copyright (C) 1988-1992:  Synoptics Ltd,  All Rights Reserved
C
      end
