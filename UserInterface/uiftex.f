C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION UIFTEX ( EID, IMMED )
C       --------------------------------------
C
C       PARAMETERS:
C
C       integer eid : INPUT - the identifier of the textfield element
C                     whose contents are to be executed.
C
C       logical immed : INPUT - immediate execute if set to .TRUE.
C
C       Executes the contents of the given textfield.  If the textfield
C       is numeric, the appropriate range checking is done: an error is
C       generated if the contents are out if range.  The contents are
C       then copied into the buffer used by UIFINP to return
C       information to the application or a call is made to OBEYCL if
C       the immediate flag is set.  If the numeric contents are
C       out of range, then any out of range action defined for the
C       textfield is copied for execution instead.
C
C       Function returns FALSE if sucessful, otherwise TRUE.
C
C----------------------------------------------------------------------
C
      logical function uiftex ( eid, immed )
C     ======================================
C
      integer eid
      logical immed
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
C     Current value of the field contents
C
      real value
C
C     True if action has been copied for application
C
      logical copied
C
C     Length of string
C
      integer length
C
C     Non-blank length of string
C
      integer nblen
C
C     Logical index of copy of contents string
C
      integer newact
C
C     Physical index of copy of contents string
C
      integer newind
C
C     Loop counter
C
      integer i
C
C     CALLED FUNCTIONS:
C
C     Gets physical index of store
C
      logical pindex
C
C     Gets value of string as a real
C
      logical appval
C
C     Checks value against required range
C
      logical uixcra
C
C     Gets length of string
C
      integer USTLEN
C
C     Copies a string
C
      logical USTNCP
C
C     Allocates memory
C
      logical allocm
C
C     Immediate command execution
C
      logical obeycl
C
C     Check system is initialised
C
      if ( uifisi ) then
C
C        Check textfield id is valid
C
         if ( eid .ge. MINTEX .and. eid .le. MAXTEX ) then
            if ( texnam(eid) .ge. 0 ) then
C
C              If field is numeric, get contents as a real value
C
               status = .FALSE.
               copied = .FALSE.
               if ( texnum(eid) ) then
C******************  THIS IS ALL EXTREMELY SUSPECT ***********
                  if ( texcon(eid) .gt. 0 ) then
                     status = pindex ( texcon(eid), ind )
                     if ( .not. status ) then
                        status = appval ( cstore(ind:), texcle(eid),
     +                                    value )
                        if ( .not. status ) then
C
C                          Check range.
C
                           status = uixcra ( value, texmir(eid),
     +                           texmio(eid), texmar(eid), texmao(eid) )
                           if ( status ) then
C
C                             Failed for some reason.  If it was cos
C                             contents were out of range, copy the out
C                             of range action for field
C
                              if ( uiferr .eq. OOFRAN ) then
                                 if ( texoor(eid) .gt. 0 ) then
                                    status = pindex ( texoor(eid),
     +                                                uifact )
                                    if ( .not. status ) then
                                       uifaln = USTLEN (cstore(uifact:))
                                       uifrdy = .TRUE.
                                       copied = .TRUE.
                                    endif
                                 else
C
C                                   No out of range action defined,
C                                   so silently return as if it was
C                                   in range.
C
                                    status = .FALSE.
                                 endif
                              endif
                           endif
                        else
C
C                          Error - cant read the contents as a number
C
                           uiferr = NOTNUM
                        endif
                     endif
                  else
C
C                    Error - cant read the contents as a number
C
                     uiferr = NOTNUM
                     status = .TRUE.
                  endif
C******************  THIS IS ALL EXTREMELY SUSPECT ***********
               endif
C
C              Copy the contents away for execution
C
               if ( .not. status .and. (.not. copied) ) then
                  status = pindex ( texcon(eid), ind )
                  if ( .not. status ) then
                     length = USTLEN ( cstore(ind:) )
C
C                    Get the non-blank length
C
                     do 10 i = length, 0, -1
                        nblen = i
                        if ( i .eq. 0 ) goto 20
                        if ( cstore(ind+i-1:ind+i-1) .ne. ' ' ) goto 20
   10                continue
   20                continue
C
C                    Allocate space for the copy, if there is anything
C                    to copy
C
                     if ( nblen .gt. 0 ) then
                        if (immed) then
                           status = obeycl ( cstore(ind:ind+nblen) )
                        else
                           status = allocm ( nblen + 1, newact )
                           if ( .not. status ) then
                              status = pindex ( newact, newind )
                              if ( .not. status ) then
C
C                             Get contents physical index again in
C                             case the allocation has caused a garbage
C                             collect
C
                                 status = pindex ( texcon(eid), ind )
                                 if ( .not. status ) then
                                    status = USTNCP ( cstore(newind:),
     +                                             cstore(ind:), nblen )
                                    if ( .not. status ) then
                                       call uixevp ( EVAFRE, newact )
                                    endif
                                 endif
                              endif
                           endif
                        endif
                     endif
                  endif
               endif
            else
C
C              Error - bad textfield id
C
               uiferr = BADTEX
               status = .TRUE.
            endif
         else
C
C           Error - bad textfield id
C
            uiferr = BADTEX
            status = .TRUE.
         endif
      else
C
C        Error - system not initialised
C
         uiferr = NOTINI
         status = .TRUE.
      endif
C
C     All done
C
      uiftex = status
      return
C
C Copyright (C) 1988, 1989, 1990:  Synoptics Ltd,  All Rights Reserved
C
      end
C
      logical function uixcra ( value, minran, minop, maxran, maxop )
C     ===============================================================
C
      real value
      integer minran, minop, maxran, maxop
C
      include 'UIFCOM'
      include 'UIXCOM'
      include 'UIFERR'
      include 'STORECOM'
C
C     LOCAL VARIABLES:
C
C     Return status
      logical status
C
C     Physical index of store
      integer ind
C
C     Length of range checking string
      integer length
C
C     Required value for range
      real reqval
C
C     CALLED FUNCTIONS:
C
C     Gets physical index of store
      logical pindex
C
C     Gets value of string as a real
      logical appval
C
C     Gets length of string
      integer USTLEN
C
C     Check minimum of range
C
      status = .FALSE.
      if ( minop .ne. NOCHEK ) then
C
C        Get the value of the minimum of the range
C
         status = pindex ( minran, ind )
         if ( .not. status ) then
            length = USTLEN ( cstore(ind:) )
            status = appval ( cstore(ind:), length, reqval )
            if ( .not. status ) then
C
C              Now check value was in range
C
               status = .TRUE.
               if ( minop .eq. LT ) then
                  if ( value .lt. reqval ) status = .FALSE.
               else if ( minop .eq. LE ) then
                  if ( value .le. reqval ) status = .FALSE.
               else if ( minop .eq. EQ ) then
                  if ( value .eq. reqval ) status = .FALSE.
               else if ( minop .eq. GE ) then
                  if ( value .ge. reqval ) status = .FALSE.
               else if ( minop .eq. GT ) then
                  if ( value .gt. reqval ) status = .FALSE.
               endif
               if ( status ) then
C
C                 Error - value out of range
C
                  uiferr = OOFRAN
               endif
            else
C
C              Error - can't get range string as a value
C
               uiferr = BADRAN
            endif
         endif
      endif
      if ( .not. status ) then
         if ( maxop .ne. NOCHEK ) then
C
C           Get the value of the maximum of the range
C
            status = pindex ( maxran, ind )
            if ( .not. status ) then
               length = USTLEN ( cstore(ind:) )
               status = appval ( cstore(ind:), length, reqval )
               if ( .not. status ) then
C
C                 Now check value was in range
C
                  status = .TRUE.
                  if ( maxop .eq. LT ) then
                     if ( value .lt. reqval ) status = .FALSE.
                  else if ( maxop .eq. LE ) then
                     if ( value .le. reqval ) status = .FALSE.
                  else if ( maxop .eq. EQ ) then
                     if ( value .eq. reqval ) status = .FALSE.
                  else if ( maxop .eq. GE ) then
                     if ( value .ge. reqval ) status = .FALSE.
                  else if ( maxop .eq. GT ) then
                     if ( value .gt. reqval ) status = .FALSE.
                  endif
                  if ( status ) then
C
C                    Error - value out of range
C
                     uiferr = OOFRAN
                  endif
               else
C
C                 Error - can't get range string as a value
C
                  uiferr = BADRAN
               endif
            endif
         endif
      endif
C
      uixcra = status
      return
C
C Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
C
      end
