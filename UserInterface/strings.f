C
C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION USTCPY ( TO, FROM )
C       ------------------------------------
C
C       PARAMETERS:
C
C       character*(*) to : OUTPUT - The string to be copied into.
C
C       character*(*) from : INPUT - The string to be copied from.
C
C       Copies one sting into another.  Resulting string is null
C       terminated, so length of to string must be long enough for to
C       string contents plus the null.  To string is checked to be long
C       enough to receive string.
C
C       Function returns FALSE if sucessful, otherwise TRUE
C
C----------------------------------------------------------------------
C
      LOGICAL FUNCTION USTCPY ( TO, FROM )
C     ====================================
C
      CHARACTER TO*(*), FROM*(*)
C
      INCLUDE 'UIFCOM'
      INCLUDE 'UIFERR'
C
C     LOCAL VARIABLES:
C
C     From string length
C
      INTEGER LENGTH
C
C     CALLED FUNCTIONS:
C
C     Finds string length
C
      INTEGER USTLEN
C
C     Check to string length is sufficient. Destination must be
C     at least one character longer.
C
      LENGTH = USTLEN ( FROM )
C
      IF ( LEN ( TO ) .GT. LENGTH ) THEN
         TO(1:LENGTH) = FROM(1:LENGTH)
         LENGTH = LENGTH + 1
         TO(LENGTH:LENGTH) = CHAR( 0)
         USTCPY = .FALSE.
      ELSE
C
C        To string too short
C
         UIFERR = SSHORT
         USTCPY = .TRUE.
      ENDIF
C
      RETURN
      END
C
C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION USTNCP ( TO, FROM, LENGTH )
C       --------------------------------------------
C
C       PARAMETERS:
C
C       character*(*) to : OUTPUT - The string to be copied into.
C
C       character*(*) from : INPUT - The string to be copied from.
C
C       integer maxlen : INPUT - The max. length of string to copy.
C
C       Copies one sting into another, up to a maximum length.
C       Resulting string is null terminated, so length of to string
C       must be long enough for to string contents plus the null.  To
C       string is checked to be long enough to receive string.
C
C       Function returns FALSE if sucessful, otherwise TRUE
C
C----------------------------------------------------------------------
C
      LOGICAL FUNCTION USTNCP ( TO, FROM, MAXLEN )
C     ============================================
C
      CHARACTER TO*(*), FROM*(*)
      INTEGER MAXLEN
C
      INCLUDE 'UIFCOM'
      INCLUDE 'UIFERR'
C
C     LOCAL VARIABLES:
C
C     From string length
C
      INTEGER LENGTH
C
C     CALLED FUNCTIONS:
C
C     Finds string length
C
      INTEGER USTLEN
C
C     Check to string length is sufficient. Destination must be
C     at least one character longer.
C
      LENGTH = USTLEN ( FROM )
      IF ( LENGTH .GT. MAXLEN ) LENGTH = MAXLEN
C
      IF ( LEN ( TO ) .GT. LENGTH ) THEN
         TO(1:LENGTH) = FROM(1:LENGTH)
         LENGTH = LENGTH + 1
         TO(LENGTH:LENGTH) = CHAR( 0)
         USTNCP = .FALSE.
      ELSE
C
C        To string too short
C
         UIFERR = SSHORT
         USTNCP = .TRUE.
      ENDIF
C
      RETURN
      END
C
C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION USTNCA ( TO, FROM, MAXLEN )
C       --------------------------------------------
C
C       PARAMETERS:
C
C       character*(*) to : OUTPUT - The string to be concatenated onto.
C
C       character*(*) from : INPUT - The string to concatanate.
C
C       integer maxlen : INPUT - The maxmimum leng of string to
C                        concatenate
C
C       Concatenates one sting to the end of another, up to a maximum
C       number of characters.  Resulting string is null terminated, so
C       length of to string must be long enough for to string contents
C       plus the null. To string is checked to be long enough to receive
C       string.
C
C       Function returns FALSE if sucessful, otherwise TRUE
C
C----------------------------------------------------------------------
C
      LOGICAL FUNCTION USTNCA ( TO, FROM, MAXLEN )
C     ============================================
C
      CHARACTER TO*(*), FROM*(*)
      INTEGER MAXLEN
C
      INCLUDE 'UIFCOM'
      INCLUDE 'UIFERR'
C
C     LOCAL VARIABLES:
C
C     From and to sting lengths
C
      INTEGER FROMLN, TOLN
C
C     Start and end positions of part of to string concatenated string
C        will occupy
C
      INTEGER START, END
C
C     CALLED FUNCTIONS:
C
C     Finds string length
C
      INTEGER USTLEN
C
C     Get lengths of to and from strings
C
      TOLN   = USTLEN ( TO )
      FROMLN = USTLEN ( FROM )
      IF ( FROMLN .GT. MAXLEN ) FROMLN = MAXLEN
C
C     Check to string length
C
      IF ( LEN ( TO ) .GT. FROMLN + TOLN ) THEN
C
C        Concatenate the string
C
         START = TOLN + 1
         END   = START + FROMLN - 1
         TO(START:END) = FROM(1:FROMLN)
         END = END + 1
         TO(END:END) = CHAR( 0)
         USTNCA = .FALSE.
      ELSE
C
C        To string too short
C
         UIFERR = SSHORT
         USTNCA = .TRUE.
      ENDIF
C
      RETURN
      END
C
C----------------------------------------------------------------------
C
C       INTEGER FUNCTION USTLEN ( STRING )
C       ----------------------------------
C
C       PARAMETERS:
C
C       character*(*) string : INPUT - The string whose length is to be
C                              found.
C
C       Finds the length of a string, in characters.  Will cope with
C       either null terminated strings or standard FORTRAN character
C       strings (e.g. 'Hello World!').  If string is null terminated,
C       length does NOT include the null.
C
C       Function returns string length.
C
C----------------------------------------------------------------------
C
      INTEGER FUNCTION USTLEN ( STRING )
C     ==================================
C
      CHARACTER STRING*(*)
C
C     See if string is null terminated
C
      USTLEN = INDEX ( STRING, CHAR( 0) )
      IF ( USTLEN .EQ. 0 ) THEN
C
C        Nope, so length is length of whole string
C
         USTLEN = LEN ( STRING )
      ELSE
C
C        Yep, so ignore null in length
C
         USTLEN = USTLEN - 1
      ENDIF
C
      RETURN
      END
