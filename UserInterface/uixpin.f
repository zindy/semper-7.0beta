c----------------------------------------------------------------------
c
c       SUBROUTINE UIXPIN ( PID, DEVICE )
c       ---------------------------------
c
c       PARAMETERS:
c
c       integer pid : INPUT - the identifier of the window to initialise
c
c       integer device : INPUT - the device the window is created on
c
c       Sets up default values for panel pid.
c
c----------------------------------------------------------------------
c
      subroutine uixpin ( pid, device )
c     =================================
c
      integer pid
      integer device
c
      include 'UIFCOM'
      include 'UIXCOM'
      include 'UIFERR'
c
c     Initialise object data
c
      call uixoin ( pid )
c
c     Initialise window data.
c
      call uixwin ( pid, device )
c
c     Initialise panel data.
c
      panaut(pid) = .FALSE.
      panfix(pid) = .TRUE.
      panman(pid) = .FALSE.
      paniss(pid) = .FALSE.
      panisc(pid) = .FALSE.
c
      return
      end
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
