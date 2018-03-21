C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION UIFEDE ( EID )
C       -------------------------------
C
C       PARAMETERS:
C
C       integer eid : INPUT - the identifier of the element to destroy.
C
C       Destroys the requested element.  If the panel owning the
C       element is showing, then the element is undrawn from the panel.
C       Any dynamic store used by the element is released, and the
C       element is removed from its owning panel, marking the entry it
C       occupied as re-useable.
C
C       Function returns FALSE if sucessful, otherwise TRUE.
C
C----------------------------------------------------------------------
C
      logical function uifede ( eid )
C     ===============================
C
      integer eid
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
C     Owning panel id
C
      integer pid
C
C     Window of owning panel
C
      integer wid
C
C     Identifer of menu owning cell
C
      integer uif
C
C     Position and size of the element
C
      integer xpos, ypos, xsize, ysize
C
C     Device owning panel is located on
C
      integer device
C
C     Panel background colour
C
      integer bgc
C
C     Loop counter
C
      integer i
C
C     CALLED FUNCTIONS:
C
C     Frees dynamic memory
C
      logical freem
C
C     Recalculates size of and redisplays a menu
C
      logical uixcmp
C
C     Destroys a panel
C
      logical uixpd2
C
C     Clears an area of a window
C
      logical winclr
C
C     Sets the value of an application variable
C
      logical appack
C
C     Check system is initialised
C
      if ( uifisi ) then
C
C        Check element id is valid
C
         status = .FALSE.
         if ( eid .ge. MINELE .and. eid .le. MAXELE ) then
            if ( elenam(eid) .ge. 0 ) then
C
C              Free any heap memory used
C
               if ( elenam(eid) .gt. 0 ) then
                  status = freem ( elenam(eid) )
               endif
               elenam(eid) = 0
C
               if ( elebac(eid) .gt. 0 ) then
                  status = freem ( elebac(eid) )
               endif
               elebac(eid) = 0
C
               if ( elecac(eid) .gt. 0 ) then
                  status = freem ( elecac(eid) )
               endif
               elecac(eid) = 0
C
               if ( eleeac(eid) .gt. 0 ) then
                  status = freem ( eleeac(eid) )
               endif
               eleeac(eid) = 0
C
C              Mark the element as usable again
C
               elenam(eid) = -1
C
C              And if the element was current, then set ENO to zero
C
               if ( curid .eq. eid ) then
                  status = appack ( 8575, 0.0 )
                  if ( status ) uiferr = VARSET
                  curid = -1
               endif
C
C              Now element specific stuff
C
               if ( eid .ge. MINCEL .and. eid .le. MAXCEL ) then
C
C                 Free any cell contents
C
                  if ( celcon(eid) .gt. 0 ) then
                     status = freem ( celcon(eid) )
                  endif
                  celcon(eid) = 0
               else if ( eid .ge. MINTEX .and. eid .le. MAXTEX ) then
C
C                 Free any textfield contents
C
                  if ( texcon(eid) .gt. 0 ) then
                     status = freem ( texcon(eid) )
                  endif
                  texcon(eid) = 0
C
C                 Free any textfield range strings
C
                  if ( texmir(eid) .gt. 0 ) then
                     status = freem ( texmir(eid) )
                  endif
                  texmir(eid) = 0
                  if ( texmar(eid) .gt. 0 ) then
                     status = freem ( texmar(eid) )
                  endif
                  texmar(eid) = 0
                  if ( texoor(eid) .gt. 0 ) then
                     status = freem ( texoor(eid) )
                  endif
                  texoor(eid) = 0
C
C                 If this textfield was selected, then set no
C                 selected textfield
C
                  if ( seltex .eq. eid ) seltex = -1
               else if ( eid .ge. MINMEN .and. eid .le. MAXMEN ) then
C
C                 If a popup/pulldown menu, destroy its panel
C
                  if ( menpan(eid) .ne. -1 ) then
                     status = uixpd2 ( menpan(eid) )
                     menpan(eid) = -1
                  endif
               endif
C
C              If owning panel id is showing, clear the element area
C
               pid = eleopa(eid)
               if ( paniss(pid) ) then
C
C                 Calculate the element position
C
                  xpos  = elexpo(eid)
                  ypos  = eleypo(eid)
                  xsize = elexsi(eid)
                  ysize = eleysi(eid)
C
C                 Clear the area
C
                  device = panode(eleopa(eid))
                  wid    = panwid(pid)
                  bgc    = panbgc(pid)
                  status = winclr ( device, wid, bgc, xpos, ypos,
     +                              xsize, ysize )
               endif
C
C              If we destroyed a cell on a menu, fix up the menu
C
               if ( eid .ge. MINCEL .and. eid .le. MAXCEL ) then
                  if ( celome(eid) .ne. 0 ) then
C
C                    Ensure that cell is not on a menu any more
C                    and is not on a panel
C
                     uif = celome(eid)
                     celome(eid) = 0
                     celopa(eid) = -1
C
C                    And fix up the menu
C
                     status = uixcmp ( eid, uif, pid )
                  endif
               endif
C
C              Loop through the event queue, checking to see if there
C              are any events generated on the element.  If there are,
C              remove them
C
               do 10 i = 0, QSIZE - 1
                  if ( evtype(i) .eq. EVENTR .or. evtype(i) .eq. EVSELE
     +                 .or. evtype(i) .eq. EVLEFT ) then
                     if ( evdata(i) .eq. eid ) then
                        evtype(i) = EVNULL
                     endif
                  endif
   10          continue
            else
C
C              Error - invalid element id
C
               uiferr = BADELE
               status = .TRUE.
            endif
         else
C
C           Error - invalid element id
C
            uiferr = BADELE
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
      uifede = status
      return
C
C Copyright (C) 1988-1992:  Synoptics Ltd,  All Rights Reserved
C
      end
C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION UIFED2 ( EID )
C       -------------------------------
C
C       PARAMETERS:
C
C       integer eid : INPUT - the identifier of the element to destroy.
C
C       Destroys the requested element.  If the panel owning the
C       element is showing, then the element is undrawn from the panel.
C       Any dynamic store used by the element is released, and the
C       element is removed from its owning panel, marking the entry it
C       occupied as re-useable.
C
C       Function returns FALSE if sucessful, otherwise TRUE.
C
C----------------------------------------------------------------------
C
      logical function uifed2 ( eid )
C     ===============================
C
      integer eid
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
C     Owning panel id
C
      integer pid
C
C     Window of owning panel
C
      integer wid
C
C     Identifer of menu owning cell
C
      integer uif
C
C     Position and size of the element
C
      integer xpos, ypos, xsize, ysize
C
C     Device owning panel is located on
C
      integer device
C
C     Panel background colour
C
      integer bgc
C
C     Loop counter
C
      integer i
C
C     CALLED FUNCTIONS:
C
C     Frees dynamic memory
C
      logical freem
C
C     Recalculates size of and redisplays a menu
C
      logical uixcmp
Cc
Cc     Destroys a panel
C      logical uixpde
C
C     Clears an area of a window
C
      logical winclr
C
C     Sets the value of an application variable
C
      logical appack
C
C     Check system is initialised
C
      if ( uifisi ) then
C
C        Check element id is valid
C
         status = .FALSE.
         if ( eid .ge. MINELE .and. eid .le. MAXELE ) then
            if ( elenam(eid) .ge. 0 ) then
C
C              Free any heap memory used
C
               if ( elenam(eid) .gt. 0 ) then
                  status = freem ( elenam(eid) )
               endif
               elenam(eid) = 0
C
               if ( elebac(eid) .gt. 0 ) then
                  status = freem ( elebac(eid) )
               endif
               elebac(eid) = 0
C
               if ( elecac(eid) .gt. 0 ) then
                  status = freem ( elecac(eid) )
               endif
               elecac(eid) = 0
C
               if ( eleeac(eid) .gt. 0 ) then
                  status = freem ( eleeac(eid) )
               endif
               eleeac(eid) = 0
C
C              Mark the element as usable again
C
               elenam(eid) = -1
C
C              And if the element was current, then set ENO to zero
C
               if ( curid .eq. eid ) then
                  status = appack ( 8575, 0.0 )
                  if ( status ) uiferr = VARSET
                  curid = -1
               endif
C
C              Now element specific stuff
C
               if ( eid .ge. MINCEL .and. eid .le. MAXCEL ) then
C
C                 Free any cell contents
C
                  if ( celcon(eid) .gt. 0 ) then
                     status = freem ( celcon(eid) )
                  endif
                  celcon(eid) = 0
               else if ( eid .ge. MINTEX .and. eid .le. MAXTEX ) then
C
C                 Free any textfield contents
C
                  if ( texcon(eid) .gt. 0 ) then
                     status = freem ( texcon(eid) )
                  endif
                  texcon(eid) = 0
C
C                 Free any textfield range strings
C
                  if ( texmir(eid) .gt. 0 ) then
                     status = freem ( texmir(eid) )
                  endif
                  texmir(eid) = 0
                  if ( texmar(eid) .gt. 0 ) then
                     status = freem ( texmar(eid) )
                  endif
                  texmar(eid) = 0
                  if ( texoor(eid) .gt. 0 ) then
                     status = freem ( texoor(eid) )
                  endif
                  texoor(eid) = 0
C
C                 If this textfield was selected, then set no
C                 selected textfield
C
                  if ( seltex .eq. eid ) seltex = -1
               else if ( eid .ge. MINMEN .and. eid .le. MAXMEN ) then
C
C                 If a popup/pulldown menu we're in trouble as we
C                 are at the recursion limit
C
                  uiferr = DESPAN
                  status = .TRUE.
                  goto 20
               endif
C
C              If owning panel id is showing, clear the element area
C
               pid = eleopa(eid)
               if ( paniss(pid) ) then
C
C                 Calculate the element position
C
                  xpos  = elexpo(eid)
                  ypos  = eleypo(eid)
                  xsize = elexsi(eid)
                  ysize = eleysi(eid)
C
C                 Clear the area
C
                  device = panode(eleopa(eid))
                  wid    = panwid(pid)
                  bgc    = panbgc(pid)
                  status = winclr ( device, wid, bgc, xpos, ypos,
     +                              xsize, ysize )
               endif
C
C              If we destroyed a cell on a menu, fix up the menu
C
               if ( eid .ge. MINCEL .and. eid .le. MAXCEL ) then
                  if ( celome(eid) .ne. 0 ) then
C
C                    Ensure that cell is not on a menu any more
C                    and is not on a panel
C
                     uif = celome(eid)
                     celome(eid) = 0
                     celopa(eid) = -1
C
C                    And fix up the menu
C
                     status = uixcmp ( eid, uif, pid )
                  endif
               endif
C
C              Loop through the event queue, checking to see if there
C              are any events generated on the element.  If there are,
C              remove them
C
               do 10 i = 0, QSIZE - 1
                  if ( evtype(i) .eq. EVENTR .or. evtype(i) .eq. EVSELE
     +                 .or. evtype(i) .eq. EVLEFT ) then
                     if ( evdata(i) .eq. eid ) then
                        evtype(i) = EVNULL
                     endif
                  endif
   10          continue
            else
C
C              Error - invalid element id
C
               uiferr = BADELE
               status = .TRUE.
            endif
         else
C
C           Error - invalid element id
C
            uiferr = BADELE
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
   20 uifed2 = status
      return
C
C Copyright (C) 1988-1992:  Synoptics Ltd,  All Rights Reserved
C
      end
