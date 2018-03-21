c----------------------------------------------------------------------
c
c       SUBROUTINE UIXEIN ( EID, PID )
c       ------------------------------
c
c       PARAMETERS:
c
c       integer eid : INPUT - the identifier of the element to
c                             initialise.
c
c       integer pid : INPUT - the panel on which it is created.
c
c
c       Sets up default values for element eid.
c
c----------------------------------------------------------------------
c
      subroutine uixein ( eid, pid )
c     ==============================
c
      integer eid
      integer pid
c
      include 'UIFCOM'
      include 'UIXCOM'
      include 'UIFERR'
c
c     Initialise object data
c
      call uixoin ( eid )
c
c     Initialise element data.
c
      eleopa(eid) = pid
      elebac(eid) = 0
      elecac(eid) = 0
      eleeac(eid) = 0
      eleexs(eid) = .FALSE.
c
c     and set element colours to those of its panel
c
      elefgc(eid) = panfgc(pid)
      elebgc(eid) = panbgc(pid)
c
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
