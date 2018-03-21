      SUBROUTINE FIOP5
C
C  exercise file access functions
C
C  SYNTAX Fi5  :FIOP5  name='
C
C
      INTEGER IPACK
      LOGICAL FILOPN,FILCIO,FILCLS,FILMAK,SEMCON,SEMKTX
C
      INCLUDE 'COMMON'
      INTEGER HANDLE,NNAM,LENNAM,LENREC,ITEXT(80),L,I,ERDUM
      CHARACTER*80 DUMMY,NAME,PATH,LINE
C
C  get file name from 'name' key as integer array
C
      NNAM=IPACK('NAME')
      LENNAM=80
      IF(SEMKTX(NNAM,'File name ? ',ITEXT,LENNAM,.FALSE.)) RETURN
      IF(LENNAM .EQ. 0) RETURN
C
C  convert to character form
C
      CALL SEMCHS(NAME,ITEXT,LENNAM)
C
C  make full file name, default = 'test.for' in current directory
C
      IF(FILMAK(NAME,'test.for',PATH)) RETURN
C
C  open file
C
      LENREC=80
      IF(FILOPN(PATH,' ',1,1,1,LENREC,DUMMY,HANDLE)) RETURN
      WRITE(RECORD,10) PATH
   10 FORMAT('Examining file ',A)
      IF(SEMCON(RECORD)) GOTO 70
C
C  read lines form source file until error=end of file
C
      I=0
C
C
   20 CONTINUE
C
         I=I+1
C
         IF(FILCIO(HANDLE,1,LINE,L)) GOTO 70
C
C  check for end of file
C
         IF(L .LT. 0) GOTO 50
C
C  now L = length of input
C
C  ignore comment lines
C
         IF(L .GT. 0) THEN
            IF(LINE(1:1) .EQ. 'C' .OR. LINE(1:1) .EQ. 'c') GOTO 20
         ENDIF
C
C  check for continuation line
C
         IF(L .GE. 6) THEN
            IF(LINE(6:6) .NE. ' ') THEN
               WRITE(RECORD,30) I
   30          FORMAT(' Continuation line at line ',I5)
               IF(SEMCON(RECORD)) GOTO 70
               IF(SEMCON(LINE)) GOTO 70
            ENDIF
         ENDIF
C
C  Check for characters beyond column 72
C
         IF(L .GT. 72) THEN
            WRITE(RECORD,40) I
   40       FORMAT(' **** Character(s) beyond column 72 at line ',I5)
            IF(SEMCON(RECORD)) GOTO 70
            IF(SEMCON(LINE)) GOTO 70
         ENDIF
C
C
      GOTO 20
C
C  arrive here when hit end of file
C
   50 CONTINUE
      WRITE(RECORD,60) I
   60 FORMAT(I5,' lines scanned ')
      IF(SEMCON(RECORD)) GOTO 70
C
C  arrive here if error while file was open, or normal completion
C
   70 CONTINUE
C
C  close file
C
      ERDUM=ERROR
      ERROR=0
      IF(FILCLS(HANDLE,.FALSE.)) THEN
C
C  error closing file:
C  report initial error if it occurred, otherwise error in closing file
C
         IF(ERDUM .NE. 0) ERROR=ERDUM
         RETURN
      ENDIF
C
      ERROR=ERDUM
C
C  file closed successfully, ERROR holds any detected error
C
      RETURN
      END
