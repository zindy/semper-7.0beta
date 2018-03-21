c
c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIFTVA ( EID, STRING, LENGTH )
c       -----------------------------------------------
c
c       PARAMETERS:
c
c       integer eid : INPUT - the identifier of the textfield element
c                     whose contents are to be set.
c
c       character*(*) string : INPUT - a string containing a numeric
c                              expression the value of which is the
c                              contents of the field.
c
c       integer length : INPUT - the length of the expression string
c
c       Evaluates the given numeric expression and sets the contents of
c       given field to the result via UIFTCO.
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uiftva ( eid, string, length )
c     ===============================================
c
      integer eid
      character*(*) string
      integer length
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
c     String value as real number
      real value
c
c     Length of string for building result
      integer RESLEN
      parameter (RESLEN=80)
c
c     String for building text for of result
      character*(RESLEN) result
c
c     Length of the resulting conversion
      integer len
c
c     CALLED FUNCTIONS:
c
c     Converts string to real number
      logical appval
c
c     Converts real number to string
      logical appstr
c
c     Sets textfield contents
      logical uiftco
c
c     Check system is initialised
c
      if ( uifisi ) then
c
c        Check textfield id is valid
c
         if ( eid .ge. MINTEX .and. eid .le. MAXTEX
     +        .and. texnam(eid) .ge. 0 ) then
c
c           Get the value of the contents of the string
c
            status = appval ( string, length, value )
            if ( .not. status ) then
c
c              Now convert that to a string
c
               status = appstr ( value, result, RESLEN, len )
               if ( .not. status ) then
c
c                 And change the contents
c
                  status = uiftco ( eid, result, len )
               else
c
c                 Error - can't convert to a string
c
                  uiferr = NOTCON
               endif
            else
c
c              Error - can't translate contents to value
c
               uiferr = NOTNUM
            endif
         else
c
c           Error - bad textfield id
c
            uiferr = BADTEX
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
c     All done
c
      uiftva = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
