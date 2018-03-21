c----------------------------------------------------------------------
c
c       SUBROUTINE UIXOIN ( ID )
c       ------------------------
c
c       PARAMETERS:
c
c       integer id : INPUT - the identifier of the object to initialise.
c
c       Sets up default values for object id.
c
c----------------------------------------------------------------------
c
      subroutine uixoin ( id )
c     ========================
c
      integer id
c
      include 'UIFCOM'
      include 'UIXCOM'
      include 'UIFERR'
c
c     Initialise object data.  Note object is marked as in use by
c     setting object name identifier to 0.
c
      objnam(id) = 0
      objxpo(id) = 1
      objypo(id) = 1
      objxsi(id) = 0
      objysi(id) = 0
      objfgc(id) = 1
      objbgc(id) = 0
      objhor(id) = horpos
      objver(id) = verpos
c
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
