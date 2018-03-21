C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION UIFMDA ( EID )
C       -------------------------------
C
C       PARAMETERS:
C
C       integer eid : INPUT - the identifier of the menu element to be
C                     deactivated.
C
C       Deactivates the given menu element.  The menu which was
C       selected when this menu was activated is reinstated as the
C       currently selected menu (if it is showing).  If the menu is not
C       fixed, then the menu panel is hidden off screen: otherwise, the
C       menu name (if any) of this menu and the selected menu are
C       repainted so that the user can identify the selected menu.
C
C       Function returns FALSE if sucessful, otherwise TRUE.
C
C----------------------------------------------------------------------
C
      logical function uifmda ( eid )
C     ===============================
C
      integer eid
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
C     Physical index of store
C
      integer ind
C
C     Length of string
C
      integer length
C
C     Current value of the variable PNO
C
      real curpno
C
C     CALLED FUNCTIONS:
C
C     Gets physical index of store
C
      logical pindex
C
C     Gets length of a string
C
      integer USTLEN
C
C     Draws a string in a window
C
      logical wintex
C
C     Hides a panel
C      logical uifphi
C
      logical uifph2
C
C     Gets the value of an application variable
C
      logical appget
C
C     Sets the value of an application variable
C
      logical appack
C
C     Check system is initialised
C
      status = .FALSE.
      if ( uifisi ) then
C
C        Check menu id is valid
C
         if ( eid .ge. MINMEN .and. eid .le. MAXMEN
     +        .and. mennam(eid) .ge. 0 ) then
C
C           Check menu is active
C
            if ( menisa(eid) ) then
C
C              Split on menu type
C
               if ( mentyp(eid) .eq. FIXED ) then
C
C                 Check panel is showing
C
                  if ( paniss(menopa(eid)) ) then
C
C                    Repaint the name to show it is no longer active
C
                     if ( mennam(eid) .gt. 0 ) then
                        status = pindex ( mennam(eid), ind )
                        if ( .not. status ) then
                           length = USTLEN ( cstore(ind:) )
                           status = wintex ( panode(menopa(eid)),
     +                                       panwid(menopa(eid)),
     +                                       menfgc(eid), menbgc(eid),
     +                                       NORMAL, menxpo(eid),
     +                                       menypo(eid),
     +                                       cstore(ind:), length )
                        endif
C
C                       Record the fact that the menu is not active
C
                        menisa(eid) = .FALSE.
                     endif
                  else
C
C                    Error - panel is not showing
C
                     uiferr = NOTSHO
                     status = .TRUE.
                  endif
               else if ( mentyp(eid) .eq. POPUP .or.
     +                   mentyp(eid) .eq. PULLDN ) then
C
C                 Hide the menu panel
C
                  if ( menpan(eid) .ne. -1 ) then
C
C                    Get the current value of the variable PNO
C
                     status = appget ( 26175, curpno )
                     if ( .not. status ) then
                        status = uifph2 ( menpan(eid) )
                        if ( .not. status ) then
C
C                          If an ends action is defined for the menu,
C                          then generate it
C
                           if ( meneac(eid) .gt. 0 ) then
                              call uixevp ( EVACTI, meneac(eid) )
C
C                             And ensure that the value of ENO is set
C                             to the menu id
C
                              status = appack( 8575, real(eid) )
                              if ( status ) uiferr = VARSET
                           endif
C
C                          Record the fact that the menu is not active
C
                           menisa(eid) = .FALSE.
C
C                          Restore the value of PNO
C
                           status = appack ( 26175, curpno )
                           if ( status ) uiferr = VARSET
                        endif
                     else
C
C                       Error - cant get value of variable
C
                        uiferr = VARGET
                     endif
                  else
C
C                    Error - panel has not been created
C
                     uiferr = MPNOTC
                     status = .TRUE.
                  endif
               endif
            endif
         else
C
C           Error - bad menu id
C
            uiferr = BADMEN
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
C     All done
C
      uifmda = status
      return
C
C Copyright (C) 1988-1992:  Synoptics Ltd,  All Rights Reserved
C
      end
C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION UIFMD2 ( EID )
C       -------------------------------
C
C       PARAMETERS:
C
C       integer eid : INPUT - the identifier of the menu element to be
C                     deactivated.
C
C       Deactivates the given menu element.  The menu which was
C       selected when this menu was activated is reinstated as the
C       currently selected menu (if it is showing).  If the menu is not
C       fixed, then the menu panel is hidden off screen: otherwise, the
C       menu name (if any) of this menu and the selected menu are
C       repainted so that the user can identify the selected menu.
C
C       Function returns FALSE if sucessful, otherwise TRUE.
C
C----------------------------------------------------------------------
C
      logical function uifmd2 ( eid )
C     ===============================
C
      integer eid
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
C     Physical index of store
C
      integer ind
C
C     Length of string
C
      integer length
C
C     Current value of the variable PNO
C      real curpno
C
C     CALLED FUNCTIONS:
C
C     Gets physical index of store
C
      logical pindex
C
C     Gets length of a string
C
      integer USTLEN
C
C     Draws a string in a window
C
      logical wintex
C
C     Hides a panel
C      logical uifph2
C
C     Check system is initialised
C
      status = .FALSE.
      if ( uifisi ) then
C
C        Check menu id is valid
C
         if ( eid .ge. MINMEN .and. eid .le. MAXMEN
     +        .and. mennam(eid) .ge. 0 ) then
C
C           Check menu is active
C
            if ( menisa(eid) ) then
C
C              Split on menu type
C
               if ( mentyp(eid) .eq. FIXED ) then
C
C                 Check panel is showing
C
                  if ( paniss(menopa(eid)) ) then
C
C                    Repaint the name to show it is no longer active
C
                     if ( mennam(eid) .gt. 0 ) then
                        status = pindex ( mennam(eid), ind )
                        if ( .not. status ) then
                           length = USTLEN ( cstore(ind:) )
                           status = wintex ( panode(menopa(eid)),
     +                                       panwid(menopa(eid)),
     +                                       menfgc(eid), menbgc(eid),
     +                                       NORMAL, menxpo(eid),
     +                                       menypo(eid),
     +                                       cstore(ind:), length )
                        endif
C
C                       Record the fact that the menu is not active
C
                        menisa(eid) = .FALSE.
                     endif
                  else
C
C                    Error - panel is not showing
C
                     uiferr = NOTSHO
                     status = .TRUE.
                  endif
               else if ( mentyp(eid) .eq. POPUP .or.
     +                   mentyp(eid) .eq. PULLDN ) then
C
C                 Hide the menu panel
C
                  if ( menpan(eid) .ne. -1 ) then
C
C                    Whoops! possible apocalypse due to recursion
C
                     uiferr = HIDPAN
                  else
C
C                    Error - panel has not been created
C
                     uiferr = MPNOTC
                  endif
                  status = .TRUE.
               endif
            endif
         else
C
C           Error - bad menu id
C
            uiferr = BADMEN
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
C     All done
C
      uifmd2 = status
      return
C
C Copyright (C) 1988, 1989, 1991:  Synoptics Ltd,  All Rights Reserved
C
      end
