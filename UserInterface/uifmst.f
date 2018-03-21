c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIFMST ( EID, STYLE )
c       --------------------------------------
c
c       PARAMETERS:
c
c       integer eid : INPUT - the identifier of the menu element which
c                             is to have its style set.
c
c       integer style : INPUT - the style of menu which is to be set.
c                       Valid values : CHOICE, TOGGLE.
c
c       Sets the given menu element to be of the given style.
c       If the panel owning the menu is is showing, an error is
c       generated, since the style of a showing menu cannot be changed.
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uifmst ( eid, style )
c     ======================================
c
      integer eid
      integer style
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
c     Check system is initialised
c
      if ( uifisi ) then
c
c        Check menu id is valid
c
         if ( eid .ge. MINMEN .and. eid .le. MAXMEN
     +        .and. mennam(eid) .ge. 0 ) then
c
c           Check panel is not showing
c
            if ( .not. paniss(menopa(eid)) ) then
c
c              Check style is valid
c
               if ( style .eq. CHOICE .or. style .eq. TOGGLE ) then
c
c                 Ok, change the style of the menu
c
                  mensty(eid) = style
                  status = .FALSE.
               else
c
c                 Error - bad manu style
c
                  uiferr = BADMST
                  status = .TRUE.
               endif
            else
c
c              Error - panel is showing
c
               uiferr = PASHOW
               status = .TRUE.
            endif
         else
c
c           Error - bad menu id
c
            uiferr = BADMEN
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
c     All done
c
      uifmst = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
