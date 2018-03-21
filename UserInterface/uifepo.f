c
c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIFEPO ( EID, XPOS, YPOS )
c       -------------------------------------------
c
c       PARAMETERS:
c
c       integer eid : INPUT - the identifier of the element whose
c                     position is to be set
c
c       integer xpos, ypos : INPUT - position of the element in device
c                            coordinates
c
c       Sets the position of the given element to the position
c       requested.  This position is relative to the top left of the
c       panel that owns it, in device coordinates.  If the panel window
c       for the panel owning the element has been created, it is
c       destroyed.  An error is generated if the panel owning the
c       element is showing on screen.
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uifepo ( eid, xpos, ypos )
c     ===========================================
c
      integer eid, xpos, ypos
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
c     Owning panel id
      integer pid
c
c     'Element is popup menu' flag
      logical ispop
c
c     Id of menu panel for popup menus
      integer mpid
c
c     Device of menu panel for popup menus
      integer device
c
c     CALLED FUNCTIONS:
c
c     Destroys a window
      logical windes
c
c     Recalculates the size of a menu
      logical uixmsi
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
c           See if we are dealing with popup menus, which need special
c           attention
c
            ispop = .FALSE.
            if ( eid .ge. MINMEN .and. eid .le. MAXMEN ) then
               if ( mentyp(eid) .eq. POPUP ) then
                  ispop = .TRUE.
                  mpid = menpan(eid)
                  device = panode(mpid)
               endif
            endif
c
c           Check panel is not showing, unless it is a popup menu,
c           in which case we can move it (provided the menu panel
c           is not showing)
c
            pid = eleopa(eid)
            if ( .not. paniss(pid) ) then
c
c              If owning panel is not autosizing, check that the
c              element is on the panel
c
               if ( .not. panaut(pid) ) then
                  xpos = xpos - (elexsi(eid) / 2) * elehor(pid)
                  ypos = ypos - (eleysi(eid) / 2) * elever(pid)
                  if ( xpos .le. (panxpo(pid) + panxsi(pid) - 1) .and.
     +                 ypos .le. (panypo(pid) + panysi(pid) - 1) ) then
                     status = .FALSE.
                  else
c
c                    Not on panel, but if element is a popup menu, then
c                    it is OK to let it go where it likes
c
                     if ( ispop ) then
                        status = .FALSE.
                     else
                        status = .TRUE.
                     endif
                  endif
                  xpos = xpos + (elexsi(eid) / 2) * elehor(pid)
                  ypos = ypos + (eleysi(eid) / 2) * elever(pid)
               else
                  status = .FALSE.
               endif
               if ( .not. status ) then
c
c                 If window is created, destroy it
c
                  if ( panisc(pid) ) then
                     status = windes ( panode(pid), panwid(pid) )
                     panisc(pid) = .FALSE.
                  else
                     status = .FALSE.
                  endif
c
c                 Now set the new position for the element
c
                  if ( .not. status ) then
                     elexpo(eid) = xpos
                     eleypo(eid) = ypos
c
c                    If a menu, may need to move things around on it
c
                     if ( eid .ge. MINMEN .and. eid .le. MAXMEN ) then
                        status = uixmsi ( eid )
c
c                       And if a popup, move its panel too (if created)
c
                        if ( ispop ) then
                           if ( mpid .ne. -1 ) then
                              if ( .not. paniss(mpid) ) then
                                 panxpo(mpid) = xpos + panxpo(pid) -
     +                                          devxmi(device)
                                 panypo(mpid) = ypos + panypo(pid) -
     +                                          devymi(device)
                              else
c
c                                Menu panel is showing - can't move it
c
                                 uiferr = PASHOW
                                 status = .TRUE.
                              endif
                           endif
                        endif
                     endif
                  endif
               else
c
c                 Error - position not on panel
c
                  uiferr = BADEPO
                  status = .TRUE.
               endif
            else
c
c              Panel is showing - error unless element is popup menu
c
               if ( ispop ) then
                  if ( mpid .ne. -1 ) then
                     if ( .not. paniss(mpid) ) then
c
c                       Yep, all is OK.
c
                        elexpo(eid) = xpos
                        eleypo(eid) = ypos
                        status = uixmsi ( eid )
c
c                       And move the menu panel too
c
                        device = panode(mpid)
                        panxpo(mpid) = xpos + panxpo(pid) -
     +                                 devxmi(device)
                        panypo(mpid) = ypos + panypo(pid) -
     +                                 devymi(device)
                     else
c
c                       Menu panel is showing - can't move it
c
                        uiferr = PASHOW
                        status = .TRUE.
                     endif
                  else
c
c                    Menu panel not created, so all is OK
c
                     elexpo(eid) = xpos
                     eleypo(eid) = ypos
                     status = uixmsi ( eid )
                  endif
               else
c
c                 Error - panel is showing
c
                  uiferr = PASHOW
                  status = .TRUE.
               endif
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
      uifepo = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
