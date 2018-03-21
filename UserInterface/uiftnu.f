c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIFTNU ( EID, RANMIN, LENMIN, RANMAX, LENMAX )
c       ---------------------------------------------------------------
c
c       PARAMETERS:
c
c       integer eid : INPUT - the identifier of the textfield element
c                     which is to be set numeric.
c
c       character*(*) ranmin : INPUT - a string representing the minimum
c                              of the range of numeric values the field
c                              may contain.
c
c       integer lenmin : INPUT - the length of the range minimum string.
c
c       character*(*) ranmax : INPUT - a string representing the maximum
c                              of the range of numeric values the field
c                              may contain.
c
c       integer lenmax : INPUT - the length of the range maximum string.
c
c       Declares the given textfield to be able to contain only numeric
c       values in a given range.  The range string is parsed to
c       determine the range allowed, and the range is stored for the
c       textfield (an error will be generated if the string is
c       syntactically incorrect).  An error will be generated if the
c       panel owning the textfield is showing since a showing textfield
c       cannot be made numeric.
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uiftnu ( eid, ranmin, lenmin, ranmax, lenmax )
c     ===============================================================
c
      integer eid
      character*(*) ranmin
      integer lenmin
      character*(*) ranmax
      integer lenmax
c
      include 'UIFCOM'
      include 'UIXCOM'
      include 'UIFERR'
      include 'STORECOM'
c
c     LOCAL VARIABLES:
c
c     Return status
      logical status
c
c     Minimum and maximum range strings and operators
      integer minptr, maxptr, minop, maxop
c
c     CALLED FUNCTIONS:
c
c     Parses part of the range string
      logical uixrsr
c
c     Check system is initialised
c
      if ( uifisi ) then
c
c        Check textfield id is valid
c
         if ( eid .ge. MINTEX .and. eid .le. MAXTEX ) then
            if ( texnam(eid) .ge. 0 ) then
c
c              Check panel is not showing
c
               if ( .not. paniss(texopa(eid)) ) then
c
c                 OK. Parse the range string.
c
                  status = uixrsr ( ranmin, lenmin, minop, minptr )
                  if ( .not. status ) then
                     status = uixrsr ( ranmax, lenmax, maxop, maxptr )
                     if ( .not. status ) then
c
c                       Save the values
c
                        texmir(eid) = minptr
                        texmar(eid) = maxptr
                        texmio(eid) = minop
                        texmao(eid) = maxop
c
c                       And mark field as numeric
c
                        texnum(eid) = .TRUE.
                     endif
                  endif
               else
c
c                 Error - panel is showing
c
                  uiferr = PASHOW
                  status = .TRUE.
               endif
            else
c
c              Error - bad textfield id
c
               uiferr = BADTEX
               status = .TRUE.
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
      uiftnu = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
c
c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIXRSR ( RANGE, LENGTH, OP, SUBRAN )
c       -----------------------------------------------------
c
c       PARAMETERS:
c
c       character*(*) range : INPUT - a string representing the operator
c                             and value for one of the subranges of the
c                             range
c
c       integer length : INPUT - the length of the subrange string.
c
c       integer op : OUTPUT - the operator of the subrange
c
c       integer subran : OUTPUT - the value of the subrange
c
c       Reads the operator and value expression of one of the subranges
c       of the valid range of the numeric textfield.
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uixrsr ( range, length, op, subran )
c     =====================================================
c
      character*(*) range
      integer length
      integer op
      integer subran
c
      include 'UIFCOM'
      include 'UIXCOM'
      include 'UIFERR'
      include 'STORECOM'
c
c     LOCAL VARIABLES:
c
c     Return status
      logical status
c
c     Index into string
      integer sindex
c
c     Length of range substring
      integer len
c
c     Start index of substring we are reading from range string
      integer start
c
c     Physical index of store
      integer ind
c
c     CALLED FUNCIONS:
c
c     Reads an operator from the range string
      logical uixrop
c
c     Frees memory
      logical freem
c
c     Allocates memory
      logical allocm
c
c     Gets physical index of string
      logical pindex
c
c     Copies a string
      logical USTNCP
c
c     Initialise things
c
      sindex = 1
      subran = 0
      op = NOCHEK
      status = .FALSE.
      if ( length .gt. 0 ) then
c
c        Read the operator
c
         status = uixrop ( range, length, sindex, op )
         if ( .not. status ) then
c
c           If operator was not NOCHEK, then read remainder as subrange
c           string.  Index points to character AFTER the last one of
c           interest in the subrange string.
c
c           Free existing subrange
c
            start = sindex + 1
            sindex = length + 1
            if ( subran .gt. 0 ) then
               status = freem ( subran )
               subran = 0
            endif
            if ( .not. status ) then
               if ( op .ne. NOCHEK ) then
                  len = sindex - start
                  if ( len .gt. 0 ) then
                     status = allocm ( len + 1, subran )
                     if ( .not. status ) then
                        status = pindex ( subran, ind )
                        if ( .not. status ) then
                           status = USTNCP ( cstore(ind:),
     +                                       range(start:), len )
c+++++++++++++++++
c      write ( 6, 100 ) cstore(ind:ind+len-1)
c100   format ( ' *** Read subrange ->', a, '<- ***' )
c+++++++++++++++++
                        endif
                     endif
                  endif
               else
                  subran = 0
                  sindex = sindex + 1
               endif
            endif
         endif
      endif
c
c     All done
c
      uixrsr = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
c
c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIXROP ( RANGE, LENGTH, SINDEX, OP )
c       -----------------------------------------------------
c
c       PARAMETERS:
c
c       character*(*) range : INPUT - a string representing the operator
c                             and value for one of the subranges of the
c                             range
c
c       integer length : INPUT - the length of the subrange string.
c
c       integer sindex : INPUT/OUTPUT - index into the range string.
c
c       integer op : OUTPUT - the operator read
c
c       Reads the operator of one of the subranges  of the valid range
c       of the numeric textfield.  SINDEX is returned pointing at the
c       last character of the operator.
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uixrop ( range, length, sindex, op )
c     =====================================================
c
      character*(*) range
      integer length, sindex, op
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
c     Loop counter
      integer i
c
c     Strip off leading whitespace
c
      status = .FALSE.
      do 10 i = sindex, length
         if ( range(i:i) .ne. ' ' ) then
            sindex = i
            goto 20
         endif
10    continue
c
c     Here if string is empty - Assume NOCHEK operator
c
      op = NOCHEK
      goto 30
20    continue
c
c     Now see if unchecked was specified
c
      status = .TRUE.
      if ( range(sindex:sindex) .eq. 'a' .or.
     +     range(sindex:sindex) .eq. 'A' ) then
         sindex = sindex + 1
         if ( range(sindex:sindex) .eq. 'n' .or.
     +        range(sindex:sindex) .eq. 'N' ) then
            sindex = sindex + 1
            if ( range(sindex:sindex) .eq. 'y' .or.
     +           range(sindex:sindex) .eq. 'Y' ) then
                op = NOCHEK
                status = .FALSE.
            endif
         endif
      else
c
c       Now the other operators
c
         if ( range(sindex:sindex) .eq. '<' ) then
            op = LT
            status = .FALSE.
            sindex = sindex + 1
            if ( range(sindex:sindex) .eq. '=' ) then
               op = LE
            else
               sindex = sindex - 1
            endif
         else
            if ( range(sindex:sindex) .eq. '>' ) then
               op = GT
               status = .FALSE.
               sindex = sindex + 1
               if ( range(sindex:sindex) .eq. '=' ) then
                  op = GE
               else
                  sindex = sindex - 1
               endif
            else
               if ( range(sindex:sindex) .eq. '=' ) then
                  op = EQ
                  status = .FALSE.
               endif
            endif
         endif
      endif
c
30    continue
c+++++++++++++++++
c      if ( .not. status ) then
c         if ( op .eq. NOCHEK ) write ( 6, 100 ) 'NOCHEK'
c         if ( op .eq. GT ) write ( 6, 100 ) 'GT'
c         if ( op .eq. GE ) write ( 6, 100 ) 'GE'
c         if ( op .eq. EQ ) write ( 6, 100 ) 'EQ'
c         if ( op .eq. LT ) write ( 6, 100 ) 'LT'
c         if ( op .eq. LE ) write ( 6, 100 ) 'LE'
c100      format ( ' *** Read operator ->', a, '<- ***' )
c      endif
c+++++++++++++++++
c
c     Check return status
c
      if ( status ) then
c
c        Error - invalid range string
c
c+++++++++++++++++
c         write ( 6, 110 )
c110      format ( ' *** Invaild operator ***' )
c+++++++++++++++++
         uiferr = BADRAN
      endif
c
      uixrop = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
