C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION UIFCAD ( EID, CID )
C       ------------------------------------
C
C       PARAMETERS:
C
C       integer eid : INPUT - the identifier of the menu element to
C                     which to add the cell
C
C       integer cid : INPUT - the identifier of the cell to be added
C
C       Adds the given cell to the given menu.  If the menu is popup or
C       pulldown, the cell is created on the panel for the menu, and
C       destroyed on the panel to which it currently belongs.
C       If the panel owning the menu is showing, an error is generated,
C       since cells may not be added to a showing menu.
C
C       Function returns FALSE if sucessful, otherwise TRUE.
C
C----------------------------------------------------------------------
C
      logical function uifcad ( eid, cid )
C     ====================================
C
      integer eid
      integer cid
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
C     Identifier of panel created for transient menus
C
      integer pid
C
C     X and Y position of panel for transient menus in device
C     coordinates
C
      integer xpos, ypos
C
C     Physical index of store
C
      integer ind
C
C     Length of menu name
C
      integer length
C
C     Current value of the application variable PID
C
      real pidcur
C
C     CALLED FUNCTIONS:
C
C     Recalculates the size of / redisplays a menu
C
      logical uixcmp
C
C     Gets the value of an application variable
C
      logical appget
C
C     Creates a panel
C
      logical uifpcr
C
C     Sets the value of an application variable
C
      logical appack
C
C     Makes a panel auto-sizing
C
      logical uifpau
C
C     Sets a panel's position
C
      logical uifppo
C
C     Gets physical index of store
C
      logical pindex
C
C     Gets length of string
C
      integer USTLEN
C
C     Sets a panels name
C
      logical uifpna
C
C     Makes a panel transient
C
      logical uifptr
C
C     Check system is initialised
C
      if ( uifisi ) then
C
C        Check menu id is valid
C
         if ( eid .ge. MINMEN .and. eid .le. MAXMEN
     +        .and. mennam(eid) .ge. 0 ) then
C
C           Check panel is not showing
C
            if ( .not. paniss(menopa(eid)) ) then
C
C              Check cell id is valid
C
               if ( cid .ge. MINCEL .and. cid .le. MAXCEL
     +              .and. celnam(cid) .ge. 0 ) then
C
C                 OK, split on menu type
C
                  if ( mentyp(eid) .eq. FIXED ) then
C
C                    Mark the cell as belonging to the menu
C
                     celome(cid) = eid
C
C                    Change the cell colours to those of the menu
C
                     celfgc(cid) = menfgc(eid)
                     celbgc(cid) = menbgc(eid)
C
C                    And recalculate the size of the menu
C
                     status = uixcmp ( cid, eid, celopa(cid) )
                  else if ( mentyp(eid) .eq. PULLDN .or.
     +                      mentyp(eid) .eq. POPUP ) then
C
C                    Check to see if the panel for the menu has been
C                    created yet: if not, create it
C
                     if ( menpan(eid) .eq. -1 ) then
C
C                       Save the current value of variable PNO
C
                        status = appget ( 26175, pidcur )
                        if ( .not. status ) then
C
C                          Create the panel for the menu
C
                           status = uifpcr ( panode(menopa(eid)), pid )
                           if ( .not. status ) then
C
C                             Reset the value of PNO
C
                              status = appack ( 26175, pidcur )
                              if ( .not. status ) then
C
C                                Set its border to single width
C
                                 panbwi(pid) = 1
C
C                                Make it auto-sizing
C
                                 status = uifpau ( pid )
                                 if ( .not. status ) then
C
C                                   Set its position to the menu
C                                   position, and ensure its positioning
C                                   is by top left
C
                                    xpos = menxpo(eid) +
     +                                        panxpo(menopa(eid))
                                    ypos = menypo(eid) +
     +                                        panypo(menopa(eid))
                                    panver(pid) = TOP
                                    panhor(pid) = LEFT
                                    status = uifppo ( pid, xpos, ypos )
                                    if ( .not. status ) then
C
C                                      Ensure positioning is the same as
C                                      the menu
C
C                                       panhor(pid) = menhor(eid)
C                                       panver(pid) = menver(eid)
C
C                                      Copy the menu colours to the
C                                      panel
C
                                       panfgc(pid) = menfgc(eid)
                                       panbgc(pid) = menbgc(eid)
C
C                                      Set its name to the menu name
C
                                       if ( mennam(eid) .gt. 0 ) then
                                          status = pindex (mennam(eid),
     +                                                     ind )
                                          if ( .not. status ) then
                                             length =
     +                                          USTLEN (cstore(ind:))
                                             status = uifpna ( pid,
     +                                           cstore(ind:), length )
                                          endif
                                       endif
                                       if ( .not. status ) then
C
C                                         Make it transient
C
                                          status = uifptr ( pid )
                                          if ( .not. status ) then
C
C                                            Record the menu panel
C
                                             menpan(eid) = pid
                                          endif
                                       endif
                                    endif
                                 endif
                              else
C
C                                Error - couldn't set variable value
C
                                 uiferr = VARSET
                                 status = .TRUE.
                              endif
                           endif
                        else
C
C                           Error - couldn't get variable value
C
                            uiferr = VARGET
                            status = .TRUE.
                        endif
                     else
C
C                       Menu panel already exists
C
                        pid = menpan(eid)
                        status = .FALSE.
                     endif
C
C                    Check that the menu is not showing
C
                     if ( .not. status ) then
                        if ( .not. winiss(pid) ) then
C
C                          Change the cell colours to those of the menu
C
                           celfgc(cid) = menfgc(eid)
                           celbgc(cid) = menbgc(eid)
C
C                          Transfer the cell to the menu panel
C
                           celopa(cid) = pid
C
C                          Make sure it's not on a fixed menu (now)
C
                           celome(cid) = -eid
C
C                          And set it's position to something sensible
C
                           celxpo(cid) = 1
                           celypo(cid) = 1
C
C                          And recalculate the size of the menu
C
                           status = uixcmp ( cid, eid, celopa(cid) )
                        else
C
C                          Error - menu panel is showing
C
                           uiferr = PASHOW
                           status = .TRUE.
                        endif
                     endif
                  endif
               else
C
C                 Error - bad cell id
C
                  uiferr = BADCEL
                  status = .TRUE.
               endif
            else
C
C              Error - panel is showing
C
               uiferr = PASHOW
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
C     OK.  If we had no error, and there is not a currently selected
C     cell, then select this one
C
      if ( mencid(eid) .eq. -1 ) then
         mencid(eid) = cid
      endif
C
C     All done
C
      uifcad = status
      return
C
C Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
C
      end
