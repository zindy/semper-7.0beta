c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIFPPO ( PID, XPOS, YPOS )
c       -------------------------------------------
c
c       PARAMETERS:
c
c       integer pid : INPUT - the identifier of the panel whose position
c                             is to be set.
c
c       integer xpos, ypos : INPUT - position of the top left of the
c                            panel in device coorinates.
c
c       Sets the position of the top left of the given panel to the
c       position requested.  If the panel is showing, an error is
c       generated, since a showing panel cannot be moved.  If the
c       window panel has been created, it is moved to reflect the
c       new position.
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uifppo ( pid, xpos, ypos )
c     ===========================================
c
      integer pid
      integer xpos, ypos
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
c     Device on which panel resides
      integer device
c
c     Panel position change in x and y
      integer deltax, deltay
c
c     Pulldown menu panel identifier
      integer mpid
c
c     Loop counter
      integer i
c
c     CALLED FUNCTIONS:
c
c     Positions a window
      logical winpos
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
c              Now check that the panel is not showing
c
               if ( .not. paniss(pid) ) then
c
c                 Set the position - check its valid first
c
                  device = panode(pid)
                  if ( xpos .ge. 0 .and.
     +                 xpos .le. devxsi(device) - 1 .and.
     +                 ypos .ge. 0 .and.
     +                 ypos .le. devysi(device) - 1 ) then
c
c                    Calculate the x and y delta to new position
c
                     deltax = xpos - panxpo(pid)
                     deltay = ypos - panypo(pid)
c
c                    Set the new panel position
c
                     panxpo(pid) = xpos
                     panypo(pid) = ypos
c
c                    Check to see if there are any pulldown menus
c                    on the panel, and if so, move them too
c
                     do 1 i = MINMEN, MAXMEN
                        if ( mennam(i) .ge. 0 ) then
                           if ( mentyp(i) .eq. PULLDN ) then
                              if ( menopa(i) .eq. pid ) then
                                 if ( menpan(i) .ne. -1 ) then
                                    mpid = menpan(i)
                                    panxpo(mpid) = panxpo(mpid) + deltax
                                    panypo(mpid) = panypo(mpid) + deltay
                                 endif
                              endif
                           endif
                        endif
1                    continue
c
c                    And move the panel window, if it is created
c
                     if ( panisc(pid) ) then
                        status = winpos ( panode(pid), panwid(pid),
     +                                    panxpo(pid), panypo(pid) )
                     else
                        status = .FALSE.
                     endif
                  else
c
c                    Error - bad position for panel
c
                     uiferr = BADPOS
                     status = .TRUE.
                  endif
              else
c
c                 Error - panel is showing
c
                  uiferr = PASHOW
                  status = .TRUE.
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
      uifppo = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
