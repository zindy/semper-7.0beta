C Semper 6 primitive module FILSEA
C
      LOGICAL FUNCTION FILSEA(NAM,DEF,PATHNM,FOUND)
C
C Searches for a named text file to open for reading, and returns the
C full path name in PATH if found. FOUND indicates if the file was
C located in a suitable form.
C   If not found then PATH is returned as a blank string.
C
      CHARACTER*(*) NAM,DEF,PATHNM
      LOGICAL FOUND
C
      INTEGER LNBLNK
      LOGICAL FILDCD,FILDIR,FILELM,FILEXI,GETPAT
C
      INCLUDE 'COMMON'
C
      INTEGER TRYIT,PATHCT,ILEN,PATLEN,PATEND
C
      LOGICAL BUILD,EXISTS
      CHARACTER*255 PATH
      CHARACTER*2048 PATHS
      CHARACTER*130 PRENAM,PREDEF
      CHARACTER*80 NAMNAM,NAMDEF
      CHARACTER*40 EXTNAM,EXTDEF
C
C Set true result and null path in case of error
C
      FILSEA = .TRUE.
      FOUND = .FALSE.
      PATHNM = ' '
C
C Break up supplied names
C
C      write(6,*)'Input def ',def
C      write(6,*)'Input nam ',nam
      IF (FILDCD(NAM,PRENAM,NAMNAM,EXTNAM)) GOTO 20
      IF (FILDCD(DEF,PREDEF,NAMDEF,EXTDEF)) GOTO 20
C      write(6,*)
C      write(6,*)'For nam ',nam
C      write(6,*)'Split of name ',prenam,namnam,extnam !LDM
C      write(6,*)
C      write(6,*)'For def ',def
C      write(6,*)'Split of def  ',predef, namdef,extdef !LDM
C      write(6,*)
C
      IF (LNBLNK(EXTNAM) .EQ. 0) EXTNAM = EXTDEF
      IF (LNBLNK(NAMNAM) .EQ. 0) NAMNAM = NAMDEF
      IF (LNBLNK(PRENAM) .EQ. 0) PRENAM = PREDEF
C
      BUILD = .FALSE.
      IF (LNBLNK(PRENAM) .EQ. 0) THEN
         BUILD = .TRUE.
         TRYIT = 1
C
C Get search path and set prefix to current directory for now
C
C         write(6,*)'Getting paths and so forth '  !LDM
         IF (GETPAT(PATHS,PATHCT)) GOTO 20
C         write(6,*)'Paths ',paths
C         write(6,*)'PathCT ',pathct
         IF (FILDIR(PRENAM)) GOTO 20
      ENDIF
C
C Construct full path
C
   10 PATLEN = LNBLNK(PRENAM)
      IF (PATLEN .EQ. 0) THEN
         PATH = ' '
      ELSE
         PATH(1:PATLEN) = PRENAM(1:PATLEN)
      ENDIF
C
      ILEN = LNBLNK(NAMNAM)
      PATEND = PATLEN+ILEN
      IF (ILEN .NE. 0) PATH(PATLEN+1:PATEND) = NAMNAM(1:ILEN)
      PATLEN = PATEND
C
      ILEN = LNBLNK(EXTNAM)
      PATEND = PATLEN+ILEN
      IF (ILEN .NE. 0) PATH(PATLEN+1:PATEND) = EXTNAM(1:ILEN)
      PATLEN = PATEND
C
C Now see if file exists in a readable form
C
C      write(6,*)'Trying ',path(1:patlen)    !LDM
      IF (FILEXI(PATH(1:PATLEN),EXISTS)) GOTO 20
C      write(6,*)'Exists was ',exists
      IF (EXISTS) THEN
C
C Will return path fit string variable ?
C
         IF (PATLEN .LE. LEN(PATHNM)) THEN
            FILSEA = .FALSE.
            FOUND = .TRUE.
            PATHNM = PATH(1:PATLEN)
         ELSE
            ERROR = 137
         ENDIF
      ELSE
C
C If searching then get next element
C
         IF (BUILD) THEN
            IF (TRYIT .LE. PATHCT) THEN
               IF (FILELM(PATHS,TRYIT,PRENAM)) GOTO 20
               TRYIT = TRYIT + 1
               GOTO 10
            ENDIF
         ENDIF
C
C File not found, but function okay
C
         FILSEA = .FALSE.
      ENDIF
C
   20 RETURN
C200   write(6,*)'Warning: Burp 1'
C      return
C201   write(6,*)'Warning: Burp 2'
C      return
C
C Copyright (C) 1989,1990:  Synoptics Ltd,  All Rights Reserved
C
      END
