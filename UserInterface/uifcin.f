c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIFCIN ( EID, ICNAME, LENGTH )
c       -----------------------------------------------
c
c       PARAMETERS:
c
c       integer eid : INPUT - the identifier of the cell whose iconic
c                     contents are to be set.
c
c       character*(*) icname : INPUT - the name of the icon identifier.
c
c       integer length : INPUT - the length of the icon identifier.
c
c       Sets the contents of the given cell to the name of the icon
c       given, and the type of the cell to ICONIC.  The name is copied
c       to dynamic storage, and the pointer to it stored for the given
c       element id. If any icon name is already stored, the space used
c       is first released.  An error is generated if the panel owning
c       the cell is showing - it is not possible to change the contents
c       of a cell when it is showing.
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uifcin ( ) !eid, icname, length )
c     ===============================================
c
!     integer eid, icname, length
c
      uifcin = .FALSE.
c
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
