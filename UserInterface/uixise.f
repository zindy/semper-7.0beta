c
c----------------------------------------------------------------------
c
C       LOGICAL FUNCTION UIXISE ( PID, EID, XPOS, YPOS )
c       ------------------------------------------------
c
c       PARAMETERS:
c
c       integer pid : INPUT - the identifer of the panel owning the
c                     element.
c
c       integer eid : INPUT/OUTPUT - the identifer of the element at the
c                     position given.
c
c       integer xpos, ypos : INPUT - the position at which to scan.
c
c       Checks the element given and the position given to make sure
c       that the position is truly in the element.  For menus, the
c       position is further scanned to see if it over a cell: if so,
c       eid is changed to return the cell id.  For textfields, if the
c       position is not in the 'fillable in' part of the field, then
c       eid is changed to return -1, i.e. no element at the position.
c
c       Function returns FALSE if an element is still to be found at
c       the position, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uixise ( pid, eid, xpos, ypos )
c     ================================================
c
      integer pid, eid, xpos, ypos
c
      include 'UIFCOM'
      include 'UIXCOM'
      include 'UIFERR'
      include 'STORECOM'
c
c     LOCAL VARIABLES:
c
c     Return status
      logical status
c
c     Loop counter
      integer i
c
c     Element position, device coordinates
      integer xepos, yepos
c
c     Device element is located on
      integer device
c
c     Physical index of store
c      integer ind
c
c     Length of name string
c      integer length
c
c     CALLED FUNCTIONS:
c
c     Gets physical index of store
c      logical pindex
c
c     Gets string length
c      integer USTLEN
c
c     Perform any special checking as defined by the type of the element
c
      if ( eid .ge. MINCEL .and. eid .le. MAXCEL ) then
         status = .FALSE.
      else if ( eid .ge. MINMEN .and. eid .le. MAXMEN ) then
c
c        Over a menu.  Scan again to see if over a cell
c
         status = .FALSE.
         do 1 i = MINCEL, MAXCEL
            if ( celnam(i) .ge. 0 ) then
               if ( celome(i) .eq. eid ) then
                  xepos = panxpo(pid) + celxpo(i)
                  if ( xpos .ge. xepos .and. xpos .le.
     +                 xepos + celxsi(i) - 1 ) then
                     yepos = panypo(pid) + celypo(i)
                     if ( ypos .ge. yepos .and. ypos .le.
     +                    yepos + celysi(i) - 1 ) then
c
c                       Hah.  Over a cell.  Reset the element id to
c                       point to it
c
                        eid = i
                        goto 2
                     endif
                  endif
               endif
            endif
1        continue
2        continue
      else if ( eid .ge. MINTEX .and. eid .le. MAXTEX ) then
         status = .FALSE.
c
c        Check that the position is in the field, and not in the name
c
c         length = 0
c         status = .FALSE.
c         if ( texnam(eid) .gt. 0 ) then
c            status = pindex ( texnam(eid), ind )
c            if ( .not. status ) then
c               length = USTLEN ( cstore(ind:) ) + 2
c            endif
c         endif
c         if ( .not. status ) then
c            xepos = panxpo(pid) + texxpo(eid) + length
c            if ( xpos .ge. xepos .and. xpos .le.
c     +           xepos + texlen(eid) - 1 ) then
c               yepos = panypo(pid) + texypo(eid)
c               if ( ypos .ge. yepos .and. ypos .le.
c     +              yepos + texysi(eid) - 1 ) then
cc
cc                 OK!
cc
c                  status = .FALSE.
c               else
cc
cc                 No, outside the field.
cc
c                  status = .TRUE.
c               endif
c            else
cc
cc              No, outside the field.
cc
c               status = .TRUE.
c            endif
c         endif
      endif
c
c     If element is not the same as the last one on its device,
c     generate leaves and enters actions
c
      if ( .not. status ) then
         device = panode(eleopa(eid))
         if ( eid .ne. eidl(device) ) then
            if ( eidl(device) .ne. -1 ) then
               if ( elenam(eidl(device)) .ge. 0 ) then
c
c                 If we have now entered a cell on a fixed menu,
c                 then don't generate a leaves action on the menu
c
                  if ( eid .ge. MINCEL .and. eid .le. MAXCEL ) then
                     if ( celome(eid) .gt. 0 ) then
c
c                       Cell is on a fixed menu.  If the last thing
c                       we were over was its menu, then we dont
c                       generate the leaves action
c
                        if ( eidl(device) .ne. celome(eid) ) then
                           call uixevp ( EVLEFT, eidl(device) )
                        endif
                     else
c
c                       Cell not on a fixed menu
c
                        call uixevp ( EVLEFT, eidl(device) )
                     endif
                  else
c
c                    Element not a cell, so generate a leaves action
c
                     call uixevp ( EVLEFT, eidl(device) )
                  endif
               endif
            endif
            if ( eid .ne. -1 ) then
c
c              If we are now on a fixed menu, and we were previously
c              on one of its cells, then don't generate an enters action
c
               if ( eid .ge. MINMEN .and. eid .le. MAXMEN ) then
                  if ( mentyp(eid) .eq. FIXED .and.
     +                 eidl(device) .ne. -1 ) then
                     if ( eidl(device) .ge. MINCEL .and.
     +                    eidl(device) .le. MAXCEL ) then
c
c                       We were over a cell last time - was it on this
c                       menu?
c
                        if ( celome(eidl(device)) .ne. eid ) then
c
c                          Cell was not on the menu, so generate an
c                          enters action
c
                           call uixevp ( EVENTR, eid )
                        endif
                     else
c
c                       Not over a cell last time - generate an enters
c                       action
c
                        call uixevp ( EVENTR, eid )
                     endif
                  else
c
c                    Not over a fixed menu, or over a fixed menu but
c                    not over any element last time - generate an enters
c                    action
c
                     call uixevp ( EVENTR, eid )
                  endif
               else
c
c                 Not a menu - generate an enters action
c
                  call uixevp ( EVENTR, eid )
               endif
            endif
            eidl(device) = eid
         endif
      endif
c
      uixise = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
