c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIXDRE ( DEVICE,MAX,XPOS,YPOS,XSIZE,YSIZE )
c       ------------------------------------------------------------
c
c       PARAMETERS:
c
c       integer device : INPUT - The device to be repainted
c                                (valid values: HOST, FSTORE)
c
c       integer max : INPUT - The window stack position of the highest
c                     window to be repainted.
c
c       integer xpos, ypos : INPUT - The top left of the area of the
c                            device to be repainted.
c
c       integer xsize, ysize : INPUT - The size of the area of the
c                            device to be repainted.
c
c       Repaints all windows on the given device in the given area.
c       Clears the device, then paints all windows from the bottom of
c       the window stack upwards.
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uixdre ( device, max, xpos, ypos, xsize, ysize )
c     =================================================================
c
      integer device, max
      integer xpos, ypos
      integer xsize, ysize
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
c     Window stack position we are showing
      integer stkpos
c
c     CALLED FUNCTIONS:
c
c     Checks whether two rectangles intersect
      logical uixint
c
c     Draws a panel
      logical uixpdr
c
c     Draws a scrolling area
      logical uixsad
c
c     Do all the windows upwards
c
      status = .FALSE.
      devhap(device) = .FALSE.
      do 1 stkpos = MINWIN, max
         do 2 i = MINWIN, MAXWIN
c
c           Window in use?
c
            if ( winnam(i) .ge. 0 ) then
c
c              On correct device?
c
               if ( winode(i) .eq. device ) then
c
c                 Correct stack position?
c
                  devhap(device) = .TRUE.
                  if ( winstk(i) .eq. stkpos ) then
c
c                    Check that the window cuts the area we are
c                    repainting
c
                     if ( uixint ( xpos, ypos, xsize, ysize,
     +                             winxpo(i), winypo(i),
     +                             winxsi(i), winysi(i) ) ) then
c
c                       Draw the window's contents
c
                        if ( i .ge. MINPAN .and. i .le. MAXPAN ) then
c
c                          Its a panel
c
                           status = uixpdr ( i, xpos - winxpo(i),
     +                                       ypos - winypo(i),
     +                                       xsize, ysize )
                        else
                           if ( i .ge. MINSCR .and. i .le. MAXSCR ) then
c
c                             Its a scrolling area
c
                              status = uixsad ( i, xpos - winxpo(i),
     +                                          ypos - winypo(i),
     +                                          xsize, ysize )
                           endif
                        endif
                        if ( status ) goto 3
                     endif
                  endif
               endif
            endif
2        continue
1     continue
3     continue
c
      uixdre = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
