c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIFSAD ( SID )
c       -------------------------------
c
c       PARAMETERS:
c
c       integer sid : INPUT - the identifier of the scrolling area
c                             to destroy
c
c       Destroys the scrolling area with the given identifier.
c       Deletes the scrolling area panel via UIFPDE, and frees up the
c       line buffers.
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uifsad () ! sid )
c     ===============================
c
!     integer sid
c
      uifsad = .FALSE.
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
