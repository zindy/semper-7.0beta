c
c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIXISD ( DEVICE, XPOS, YPOS )
c       ----------------------------------------------
c
c       PARAMETERS:
c
c       integer device : INPUT - device to be scanned.
c
c       integer xpos, ypos : INPUT - the position at which to scan.
c
c       Scans the given device to find which, if any, panel and element
c       are at the given position.  The panel stack is scanned top down,
c       and if a panel is found at the position, then the panel is
c       scanned for elements at the position.  Correct enters and leaves
c       actions will be generated for any elements found at the position
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uixisd ( device, xpos, ypos )
c     ==============================================
c
      integer device
      integer xpos, ypos
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
c     Local copy of position
      integer lxpos, lypos
c
c     Loop counters
      integer stkpos, i, j
c
c     Panel and element at the position
      integer pid, eid
c
c     Element position, device coordinates
      integer xepos, yepos
c
c     CALLED FUNCTIONS:
c
c     Re-checks an element at a position
      logical uixise
c
c     Walk down the window stack on the requested device, checking to
c     see if the position is on a showing panel
c
      pid = -1
      eid = -1
      lxpos = xpos + devxmi(device)
      lypos = ypos + devymi(device)
      status = .FALSE.
      do 1 stkpos = MAXWIN, MINWIN, -1
c
c        Find the panel at this stack position
c
         do 2 j = MINWIN, MAXWIN
            if ( winnam(j) .ge. 0 ) then
               if ( winode(j) .eq. device ) then
                  if ( winiss(j) ) then
                     if ( winstk(j) .eq. stkpos ) then
                        i = j
                        goto 3
                     endif
                  endif
               endif
            endif
2        continue
c
c        Here if no window at the stack position.  Loop on.
c
         goto 1
3        continue
c
c        Here when window at stack pos. found: id is in i.
c        Position on the panel?
c
         if ( lxpos .ge. winxpo(i) .and.
     +        lxpos .le. winxpo(i) + winxsi(i) - 1 ) then
            if ( lypos .ge. winypo(i) .and.
     +           lypos .le. winypo(i) + winysi(i) - 1 ) then
c
c              OK, position is on this panel.  Need to check if it's
c              over an element
c
               pid = i
               do 4 j = MINELE, MAXELE
                  if ( elenam(j) .ge. 0 ) then
                     if ( eleopa(j) .eq. i ) then
                        xepos = winxpo(i) + elexpo(j)
                        yepos = winypo(i) + eleypo(j)
                        if ( lxpos .ge. xepos .and. lxpos .le.
     +                       xepos + elexsi(j) - 1 ) then
                           if ( lypos .ge. yepos .and. lypos .le.
     +                          yepos + eleysi(j) - 1 ) then
c
c                             OK, Found an element.  Do any special
c                             checking and adjustment
c
                              eid = j
                              status = uixise ( pid, eid, lxpos, lypos )
                              if ( .not. status ) then
c
c                                Found good element.  All did.
c
                                 goto 5
                              else
c
c                                Nothing there.  Keep going
c
                                 eid = -1
                                 status = .FALSE.
                              endif
                           endif
                        endif
                     endif
                  endif
4              continue
c
c              Fallen through the element scanning loop.  Stop panel
c              scanning, since any other panels lower down the stack
c              at this position must be by definition hidden
c
               goto 5
            endif
         endif
1     continue
5     continue
c
c     If we arrive here and we are not over a panel or an element,
c     then we cannot be over the last element, so generate a leaves
c     action if needed (i.e. there was a last element, and it still
c     exists)
c
      if ( pid .eq. -1 .or. eid .eq. -1 ) then
         if ( eidl(device) .ne. -1 ) then
            if ( elenam(eidl(device)) .ge. 0 ) then
               call uixevp ( EVLEFT, eidl(device) )
            endif
         endif
         eidl(device) = eid
      endif
      pidl(device) = pid
c
      uixisd = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
