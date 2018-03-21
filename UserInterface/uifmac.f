C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION UIFMAC ( EID )
C       -------------------------------
C
C       PARAMETERS:
C
C       integer eid : INPUT - the identifier of the menu element to be
C                     activated.
C
C       Activates the given menu element.  The currently selected
C       menu id is saved, and the requested menu is made the selected
C       menu.  If the menu is not fixed, then the menu panel is shown on
C       screen: otherwise, the menu name (if any) of this menu and the
C       previously selected menu are repainted so that the user can
C       identify the selected menu.
C
C       Function returns FALSE if sucessful, otherwise TRUE.
C
C----------------------------------------------------------------------
C
      logical function uifmac ( eid )
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
C     Current value of variable PNO
C
      real curpno
C
C     CALLED FUNCTIONS:
C
C     Shows a panel
C
      logical uifpsh
C
C     Gets the value of an application variable
C
      logical appget
C
C     Sets the value of an application variable
C
      logical appack
C      logical uifccy
C      integer oldele
C
C     Check system is initialised
C
      status = .FALSE.
      if ( uifisi ) then
C
C        Check menu id is valid
C
         if ( eid .ge. MINMEN .and. eid .le. MAXMEN ) then
            if ( mennam(eid) .ge. 0 ) then
C
C              If menu is already active, nothing to do
C
               if ( .not. menisa(eid) ) then
C
C                 Split on menu type
C
                  if ( mentyp(eid) .eq. FIXED ) then
Cc
Cc                    Check panel is showing
Cc
C                     if ( paniss(menopa(eid)) ) then
Cc
Cc                       Repaint the name to show it is active
Cc
C                        if ( mennam(eid) .gt. 0 ) then
C                           status = pindex ( mennam(eid), ind )
C                           if ( .not. status ) then
C                              length = USTLEN ( cstore(ind:) )
Cc
Cc                             Save state of cursor, and turn it off
Cc
C                              call devcsa ( panode(menopa(eid)) )
C                              call devcof ( panode(menopa(eid)) )
C                              status = wintex ( panode(menopa(eid)),
C     +                                          panwid(menopa(eid)),
C     +                                          menfgc(eid),
C     +                                          menbgc(eid),
C     +                                          INVERS, menxpo(eid),
C     +                                          menypo(eid),
C     +                                          cstore(ind:), length )
Cc
Cc                             Restore state of cursor
Cc
C                              call uixirc ( panode(menopa(eid)) )
C                              menisa(eid) = .TRUE.
C                           endif
C                        endif
C                     else
Cc
Cc                       Error - panel is not showing
Cc
C                        uiferr = NOTSHO
C                        status = .TRUE.
C                     endif
C
                  else if ( mentyp(eid) .eq. POPUP .or.
     +                      mentyp(eid) .eq. PULLDN ) then
C
C                    Show the menu panel
C
                     if ( menpan(eid) .ne. -1 ) then
C
C                       Save the current value of PNO
C
                        status = appget ( 26175, curpno )
                        if ( .not. status ) then
C
C                          Show the panel
C
                           status = uifpsh ( menpan(eid) )
                           if ( .not. status ) then
C
C                             Set cursor into currently selected cell
C                             for the menu
C
                              call uixisc ( eid )
C
C                             If a begins action is defined for the
C                             menu then generate it
C
                              if ( menbac(eid) .gt. 0 ) then
                                 call uixevp ( EVACTI, menbac(eid) )
C
C                                And set ENO to the value of the menu
C
                                 status = appack ( 8575,
     +                                             real ( eid ) )
                                 if ( status ) then
C
C                                   Error - cant set variable value
C
                                    uiferr = VARSET
                                 endif
                              endif
C
C                             Record the fact that the menu is active
C
                              menisa(eid) = .TRUE.
C
C                             Reset the value of PNO
C
                              status = appack ( 26175, curpno )
                              if ( status ) then
C
C                                Error - cant set variable value
C
                                 uiferr = VARSET
                              endif
                           endif
                        else
C
C                          Error - cant get the variable value
C
                           uiferr = VARGET
                        endif
                     else
C
C                       Error - panel has not been created
C
                        uiferr = MPNOTC
                        status = .TRUE.
                     endif
                  endif
               endif
            else
C
C              Error - bad menu id
C
               uiferr = BADMEN
               status = .TRUE.
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
      uifmac = status
      return
C
C Copyright (C) 1988-1992:  Synoptics Ltd,  All Rights Reserved
C
      end
