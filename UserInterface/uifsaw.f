c
c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIFSAW ( SID, STRING, LENGTH )
c       -----------------------------------------------
c
c       PARAMETERS:
c
c       integer sid : INPUT - the identifier of the scrolling area to
c                     write to
c
c       character*(*) string : INPUT - the text string to write
c
c       integer length : INPUT - the length of the string
c
c       Writes the given string out into the given scrolling area.
c       If the string is too long for the scrolling area, it will be
c       split as needed.  Scrolling will be performed as necessary.
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uifsaw ( sid ) !, string, length )
c     ===============================================
c
      integer sid
!     character*(*) string
!     integer length
c
      include 'UIFCOM'
      include 'UIXCOM'
      include 'UIFERR'
c
c     LOCAL VARIABLES:
c
c     Function return status
      logical status
c
c     Check system is initialised
c
      if ( uifisi ) then
c
c        Check scrolling area id is valid
c
         if ( sid .ge. MINSCR .and. sid .le. MAXSCR ) then
            if ( scrnam(sid) .ge. 0 ) then
c
c              Save the string into the line buffer
c
c
c              Now check that if the scrolling area is showing
c
               if ( scriss(sid) ) then
c
c                 Check to see if we need to scroll
c
              endif
            else
c
c              Error - invalid scrolling area id
c
               uiferr = BADSCR
               status = .TRUE.
            endif
         else
c
c           Error - invalid scrolling area id
c
            uiferr = BADSCR
            status = .TRUE.
         endif
      else
c
c        Error - system not initialised
c
         uiferr = NOTINI
         status = .TRUE.
      endif
c
      uifsaw = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
