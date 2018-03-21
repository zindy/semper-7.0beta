c----------------------------------------------------------------------
c
c       SUBROUTINE UIXOBS ( DEVICE )
c       ----------------------------
c
c       PARAMETERS:
c
c       integer device : INPUT - the device whose window stack is to be
c                        updated.
c
c       Updates the obscured flags of all windows on the window stack
c       for the requested device.  Should be called whenever the panel
c       stack positions have been altered.  Walks up the window stack,
c       checking each window against all those above, to check if it is
c       obscured by any of them.
c
c----------------------------------------------------------------------
c
      subroutine uixobs ( device )
c     ============================
c
      integer device
c
      include 'UIFCOM'
      include 'UIXCOM'
      include 'UIFERR'
c
c     LOCAL VARIABLES:
c
c     Loop counters
      integer i, j
c
c     Position and size of the window we are to checking
      integer xpos, ypos, xsize, ysize
c
c     Stack positions used when checking whether windows are obscured
      integer stkpos, stk
c
c     Checks if two rectangles intersect
      logical uixint
c
c     Walk up the stack checking each window against those above it.
c
      do 1 stkpos = MINWIN, MAXWIN
c
c        Find the panel at this stack position.  Must check panel is in
c        use, on the correct device and not hidden as well as stack
c        position.
c
         do 2 i = MINWIN, MAXWIN
            if ( (winnam(i) .ge. 0) .and. (winode(i) .eq. device)
     +           .and. (winstk(i) .eq. stkpos) ) goto 3
2        continue
c
c        No window at this stack position.
c        Try next stack position: this one may not be in use.
c
         goto 1
3        continue
c
c        Now check all windows above it
c
         xpos = winxpo(i)
         ypos = winypo(i)
         xsize = winxsi(i)
         ysize = winysi(i)
         winobs(i) = .FALSE.
c
c        Loop up the stack
c
         do 4 stk = stkpos, MAXWIN
            if ( stk .eq. stkpos ) goto 4
c
c           Find the window at this stack position
c
            do 5 j = MINWIN, MAXWIN
               if ( (winnam(j) .ge. 0) .and.
     +              (winode(j) .eq. device) .and.
     +              (winstk(j) .eq. stk ) ) goto 6
5           continue
c
c           Couldn't find one.  Try next stack position.
c
            goto 4
6           continue
c
c           OK.  Check the extent of this window against the one
c           we are interested in.
c
            if ( uixint ( xpos, ypos, xsize, ysize,
     +                    winxpo(j), winypo(j),
     +                    winxsi(j), winysi(j) ) ) then
c
c              They intersect!
c
               winobs(i) = .TRUE.
c
c              Marked as obscured, so no point checking further.
c              Go up the stack to check the next one
c
               goto 1
            endif
4        continue
1     continue
c
c     All done!
c
      return
      end
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
