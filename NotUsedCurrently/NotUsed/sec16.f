      SUBROUTINE FIOP1
C
C  elementary file operation exerciser (1)
C
C  print current directory
C        current search path
C        components of file 'nam'
C
C
C  SYNTAX:    Fi1   :FIOP1    name='
C
      LOGICAL FILDIR,FILDCD,SEMCON,SEMKTX
      INTEGER IPACK
C
      INCLUDE 'COMMON'
C
      CHARACTER*80 DIR,FULNAM,NAME,PREFIX,EXTEN
      INTEGER ITEXT(80),LENNAM,NNAM
C
      IF(SEMCON(' ')) RETURN
      IF(SEMCON(' Exerciser for FILDIR and FILDCD ')) RETURN
      IF(SEMCON(' ')) RETURN
C
C  get current directory
C
      IF(SEMCON('------ FILDIR ------')) RETURN
      IF(FILDIR(DIR)) RETURN
      IF(SEMCON('Current directory : ')) RETURN
      IF(SEMCON(DIR)) RETURN
      IF(SEMCON(' ')) RETURN
C
C  get file name from 'name' key as integer array
C
      NNAM=IPACK('NAME')
      LENNAM=80
      IF(SEMKTX(NNAM,'File name ? ',ITEXT,LENNAM,.FALSE.)) RETURN
C
C  convert to character form
C
      IF(LENNAM .EQ. 0) RETURN
      CALL SEMCHS(FULNAM,ITEXT,LENNAM)
      IF(SEMCON('------ FILDCD ------')) RETURN
      IF(SEMCON('File name as supplied : ')) RETURN
      IF(SEMCON(FULNAM(1:LENNAM))) RETURN
C
C  split up name into components
C
      IF(FILDCD(FULNAM(1:LENNAM),PREFIX,NAME,EXTEN)) RETURN
      IF(SEMCON('Prefix :')) RETURN
      IF(SEMCON(PREFIX)) RETURN
      IF(SEMCON('Name :')) RETURN
      IF(SEMCON(NAME)) RETURN
      IF(SEMCON('Extension :')) RETURN
      IF(SEMCON(EXTEN)) RETURN
      RETURN
      END
