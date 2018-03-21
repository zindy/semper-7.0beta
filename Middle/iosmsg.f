C Subroutine IOSMSG for SILICON_GRAPHICS
C
      SUBROUTINE IOSMSG(IOS,STRING)
      INTEGER IOS
      CHARACTER*(*) STRING
C
      CHARACTER*50 FORMSG
C
      IF (IOS .LT. 100 .OR. IOS .GT. 165) THEN
         FORMSG = 'Illegal I/O message number'
      ELSE IF (IOS .EQ. 100) THEN
         FORMSG = 'Error in format'
      ELSE IF (IOS .EQ. 101) THEN
         FORMSG = 'I/O unit number illegal'
      ELSE IF (IOS .EQ. 102) THEN
         FORMSG = 'Formatted I/O attempted on unformatted file'
      ELSE IF (IOS .EQ. 103) THEN
         FORMSG = 'Unformatted I/O attempted on formatted file'
      ELSE IF (IOS .EQ. 106) THEN
         FORMSG = 'Can''t backspace file'
      ELSE IF (IOS .EQ. 107) THEN
         FORMSG = 'Null file name'
      ELSE IF (IOS .EQ. 110) THEN
         FORMSG = 'Access past end of record attempted'
      ELSE IF (IOS .EQ. 113) THEN
         FORMSG = 'Out of free space'
      ELSE IF (IOS .EQ. 114) THEN
         FORMSG = 'Access of unconnected unit attempted'
      ELSE IF (IOS .EQ. 115) THEN
         FORMSG = 'Read unexpected character'
      ELSE IF (IOS .EQ. 117) THEN
         FORMSG = 'Bad variable type'
      ELSE IF (IOS .EQ. 120) THEN
         FORMSG = 'No end record'
      ELSE IF (IOS .EQ. 122) THEN
         FORMSG = 'Negative repeat count'
      ELSE IF (IOS .EQ. 126) THEN
         FORMSG = '''new'' file exists'
      ELSE IF (IOS .EQ. 127) THEN
         FORMSG = '''old'' file not found'
      ELSE IF (IOS .EQ. 128) THEN
         FORMSG = 'Unknown system error'
      ELSE IF (IOS .EQ. 129) THEN
         FORMSG = 'Requires seek ability'
      ELSE IF (IOS .EQ. 130) THEN
         FORMSG = 'Illegal argument'
      ELSE IF (IOS .EQ. 145) THEN
         FORMSG = 'Filename too long'
      ELSE IF (IOS .EQ. 147) THEN
         FORMSG = 'Record too long'
      ELSE IF (IOS .EQ. 152) THEN
         FORMSG = 'Append to indexed file not allowed'
      ELSE IF (IOS .EQ. 153) THEN
         FORMSG = 'Must specify record length'
      ELSE IF (IOS .EQ. 160) THEN
         FORMSG = 'Attempt to write to readonly file'
      ELSE IF (IOS .EQ. 162) THEN
         FORMSG = 'Carriagecontrol not allowed on unformatted unit'
      ELSE
         FORMSG = 'message not in dictionary'
      ENDIF
C
      STRING = FORMSG
C
      RETURN
C
C Copyright (C) 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
