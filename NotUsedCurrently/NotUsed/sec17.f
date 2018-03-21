      SUBROUTINE FIOP2
C
C  elementary file operation exerciser (2)
C
C  open and close an existing file,
C  print full path to file, if found
C
C
C  SYNTAX:    Fi2   :FIOP2    name='
C
      LOGICAL FILOPN,FILCLS,SEMCON,SEMKTX
      INTEGER IPACK
C
      INCLUDE 'COMMON'
C
      CHARACTER*80 NAME,PATH
      INTEGER ITEXT(80),LENNAM,NNAM,LENREC,HANDLE,DUMMY
C
      IF(SEMCON(' ')) RETURN
      IF(SEMCON(' Exerciser for FILOPN (read only), FILCLS ')) RETURN
      IF(SEMCON('   Default extension is .dsk ')) RETURN
      IF(SEMCON(' ')) RETURN
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
      IF(SEMCON('------ FILOPN ------')) RETURN
      IF(SEMCON('File name as supplied : ')) RETURN
      IF(SEMCON(NAME(1:LENNAM))) RETURN
C
C  open the file
C
      LENREC=80
C... value of LENREC is currenly ignored
      IF(FILOPN(NAME(1:LENNAM),'.dsk',1,1,DUMMY,LENREC,PATH,
     +                                                HANDLE)) THEN
         DUMMY=ERROR
         ERROR=0
         WRITE(RECORD,10) DUMMY
   10    FORMAT('FILOPN failed. ERROR ',I5,' returned')
         IF(SEMCON(RECORD)) RETURN
      ELSE
C
C  show full file name, handle
C
         IF(SEMCON('File opened successfully :')) RETURN
         IF(SEMCON(PATH)) RETURN
         WRITE(RECORD,20) HANDLE
   20    FORMAT('File handle returned as ',I6)
         IF(SEMCON(RECORD)) RETURN
C
C  close file
C
         IF(SEMCON(' ')) RETURN
         IF(SEMCON('------ FILCLS ------')) RETURN
         IF(FILCLS(HANDLE,.FALSE.)) RETURN
         IF(SEMCON('File closed successfully')) RETURN
      ENDIF
C
      RETURN
      END
