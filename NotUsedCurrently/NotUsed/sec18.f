      SUBROUTINE FIOP3
C
C  elementary file operation exerciser (3)
C
C  Locates a file of a given name on the search path. The current
C  directory is not searched unless specified in the search path.
C
C  SYNTAX:    Fi3   :FIOP3    name='
C
      LOGICAL SEMCON,SEMKTX,FILPAT,FILSEA
      INTEGER IPACK,NBLANK
C
      INCLUDE 'COMMON'
C
      CHARACTER*80 NAME,PATH
      INTEGER ITEXT(80),LENNAM,NNAM,I,J
      LOGICAL FOUND
C
      IF(SEMCON(' ')) RETURN
      IF(SEMCON(' Exerciser for FILPAT and FILSEA')) RETURN
      IF(SEMCON(' ')) RETURN
C
C  get search path
C
      IF(SEMCON('------ FILPAT ------')) RETURN
C
C  present each element of search path in turn
C
      I=0
   10 CONTINUE
        I=I+1
        IF(FILPAT(I,PATH)) RETURN
        J=NBLANK(PATH)
        IF(J .EQ. 0) GOTO 20
        IF(SEMCON(PATH(1:J))) RETURN
      GOTO 10
C
C  All elements of search path found and output
C
   20 CONTINUE
C
C  get file name from 'name' key as integer array
C
      NNAM=IPACK('NAME')
      LENNAM=80
      IF(SEMKTX(NNAM,'File name ? ',ITEXT,LENNAM,.FALSE.)) RETURN
C
C  stop here if no name supplied
C
      IF(LENNAM .EQ. 0) THEN
         ERROR=10
         IF(SEMCON(' no file specified ')) RETURN
         RETURN
      ENDIF
C
C  convert to character form
C
      CALL SEMCHS(NAME,ITEXT,LENNAM)
C
C  search for file
C
      IF(SEMCON('------ FILELM FILSEA ------')) RETURN
      IF(FILSEA(NAME(1:LENNAM),' ',PATH,FOUND)) RETURN
C
      IF(FOUND) THEN
         J=NBLANK(PATH)
         IF(SEMCON(PATH(1:J))) RETURN
      ELSE
         IF(SEMCON('File not found ')) RETURN
      ENDIF
C
      RETURN
      END
