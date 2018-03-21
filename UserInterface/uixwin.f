c
c----------------------------------------------------------------------
c
c       SUBROUTINE UIXWIN ( ID, DEVICE )
c       --------------------------------
c
c       PARAMETERS:
c
c       integer id : INPUT - the identifier of the window to initialise.
c
c       integer device : INPUT - the device the window is created on
c
c       Sets up default values for object id.
c
c----------------------------------------------------------------------
c
      subroutine uixwin ( id, device )
c     ================================
c
      integer id
      integer device
c
      include 'UIFCOM'
      include 'UIXCOM'
      include 'UIFERR'
c
c     Initialise object data
c
      call uixoin ( id )
c
c     Initialise window data.
c
      winode(id) = device
      winiss(id) = .FALSE.
      winisc(id) = .FALSE.
      winwid(id) = 0
      winobs(id) = .FALSE.
      winstk(id) = 0
      winbwi(id) = 2
      winexs(id) = .FALSE.
      winesx(id) = 0
      winesy(id) = 0
c
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
