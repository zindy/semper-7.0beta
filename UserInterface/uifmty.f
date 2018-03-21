c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIFMTY ( EID, TYPE )
c       -------------------------------------
c
c       PARAMETERS:
c
c       integer eid : INPUT - the identifier of the menu element which
c                             is to have its type set.
c
c       integer type : INPUT - the type of menu which is to be set.
c                      Valid values : FIXED, POPUP, PULLDN
c
c       Sets the given menu element to be of the given type.  If the
c       panel owning the menu is is showing, an error is generated,
c       since the type of a showing menu cannot be changed.
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uifmty ( eid, type )
c     =====================================
c
      integer eid
      integer type
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
c     CALLED FUNCTIONS:
c
c     Destroys a panel
      logical uifpde
c
c     Adds a cell to a menu
      logical uifcad
c
c     Recalculates the size of a menu
      logical uixmsi
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
c              Set the type
c
               if ( type .eq. FIXED ) then
c
c                 If menu wasn't fixed, transfer cells off menu panel
c
                  if ( mentyp(eid) .ne. FIXED ) then
                     do 1 i = MINCEL, MAXCEL
                        if ( celnam(i) .ge. 0 ) then
                           if ( celopa(i) .eq. menpan(eid) ) then
                              celopa(i) = menopa(eid)
                              celome(i) = eid
                           endif
                        endif
1                    continue
c
c                    Destroy any menu panel if already created
c
                     if ( menpan(eid) .ne. -1 ) then
                        status = uifpde ( menpan(eid) )
                        menpan(eid) = -1
                     else
                        status = .FALSE.
                     endif
                     if ( .not. status ) then
c
c                       Mark the menu as FIXED
c
                        mentyp(eid) = FIXED
c
c                       Recalculate the menu size
c
                        status = uixmsi ( eid )
                     endif
                  else
                     status = .FALSE.
                  endif
               else if ( type .eq. PULLDN .or. type .eq. POPUP ) then
c
c                 If menu wasn't same type as requested, transfer cells
c                 onto menu panel
c
                  if ( mentyp(eid) .ne. type ) then
c
c                    Change the menu type
c
                     mentyp(eid) = type
                     status = .FALSE.
                     do 2 i = MINCEL, MAXCEL
                        if ( celnam(i) .ge. 0 ) then
                           if ( celome(i) .eq. eid ) then
                              status = uifcad ( eid, i )
                              if ( status ) goto 3
                           endif
                        endif
2                    continue
3                    continue
                     if ( .not. status ) then
c
c                       Recalculate the menu size
c
                        status = uixmsi ( eid )
                     endif
                  else
                     status = .FALSE.
                  endif
               else
c
c                 Error - bad menu type
c
                  uiferr = BADMTY
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
      uifmty = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
