C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION UIXICA ( LINDEX, TXTSTR, N )
C       ---------------------------------------------
C
C       PARAMETERS:
C
C       integer lindex : INPUT - The logical index of the action string
C                        to be copied.
C
C       character*(*) txtstr : OUTPUT - command to be executed.
C
C       integer n : OUTPUT - the number of characters in the buffer.
C
C       Copies an action string pointed to by LINDEX into the
C       application command buffer.  If the buffer is too short
C       to hold the action string, an error is generated.
C
C       Function returns FALSE if sucessful, otherwise TRUE.
C
C----------------------------------------------------------------------
C
      logical function uixica ( lindex, txtstr, n )
C
C     =============================================
C
      integer lindex
      character*(*) txtstr
      integer n
C
      include 'UIFCOM'
      include 'UIXCOM'
      include 'UIFERR'
      include 'STORECOM'
C
C     LOCAL VARIABLES:
C
C     Return status
C
      logical status
C
C     Physical index of store
C
      integer ind
C
C     Length of action string
C
      integer slen
C
C     CALLED FUCNTIONS:
C
C     Gets physical index of store
C
      logical pindex
C
C     Gets string length
C
      integer USTLEN
C
C     Gets non-blank string length
C
      integer LNBLNK
C
C     Get the length of the string at the logical index
C
      status = pindex ( lindex, ind )
      if ( .not. status ) then
         slen = USTLEN ( cstore(ind:) )
C
C        Check that the string will fit the output buffer
C
         if ( slen .le. len(txtstr) ) then
C
C           Ok, copy the data
C
            txtstr(1:slen) = cstore(ind:ind+slen-1)
C
C           Strip off any trailing spaces
C
            n = LNBLNK ( txtstr(1:slen) )
C
C--------------------------
C      if ( n .gt. 0 ) then
C         write ( 6, 100 ) n, cstore(ind:ind+n-1)
C      else
C         write ( 6, 100 ) n, ' '
C      endif
C100   format ( ' returning action length ', i3, ' contents ', a )
C--------------------------
C
            status = .FALSE.
         else
C
C           Error - action string too long to fit buffer
C
            uiferr = ACLONG
            status = .TRUE.
         endif
      endif
C
      uixica = status
      return
C
C Copyright (C) 1988, 1989, 1993:  Synoptics Ltd,  All Rights Reserved
C
      end
