c----------------------------------------------------------------------
c
c	LOGICAL FUNCTION WINCRE ( DEVICE,PID,NAME,LENGTH,XPOS,YPOS,
c                                 XSIZE,YSIZE,WID )
c       -----------------------------------------------------------
c
c	PARAMETERS:
c
c       integer device : INPUT - the device on which the window is to
c                        be created
c
c       integer pid : INPUT - the panel the window is being created for.
c
c       character*(*) name : INPUT - the name to give to the window.
c
c       integer length : INPUT - the length of the name
c
c       integer xpos, ypos : INPUT - the position of the top left of
c                             the window in device coordinates.
c
c       integer xsize, ysize : INPUT - the size of the window in device
c                              coordinates.
c
c       integer wid : OUTPUT - the identifier of the newly created
c                              window
c
c	Creates a window.  Calls the appropriate function to create a
c       window on the given device.
c
c	Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function wincre ( device,pid,name,length,
     +                          xpos,ypos,xsize,ysize,wid )
c     =====================================================
c
      integer device
      integer pid
      character*(*) name
      integer length
      integer xpos, ypos, xsize, ysize
      integer wid
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
c     CALLED FUNCTIONS:
c
c     Creates a window on host device
      logical whocre
c
c     Creates a window on framestore device
      logical wfscre
c
      if ( device .eq. HOST ) then
         status = whocre ( device, pid, name, length,
     +                     xpos, ypos, xsize, ysize, wid )
      else
         status = wfscre ( device, pid, name, length,
     +                     xpos, ypos, xsize, ysize, wid )
      endif
c
      wincre = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
