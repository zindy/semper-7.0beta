C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION DHOTEX ( XPOS, YPOS, STRING, LENGTH,
C                                 FGC, BGC, STYLE )
C       -------------------------------------------------------------
C
C       PARAMETERS:
C
C       integer xpos, ypos : INPUT - the position (device coordinates)
C                            at which the text is to be drawn.
C
C       character*(*) string : INPUT - the string to be written.
C
C       integer length : INPUT - the length of the string.
C
C       integer fgc, bgc : INPUT - the foreground and background colours
C                          in which to write the text.
C
C       integer style : INPUT - the style (NORMAL or INVERS) in
C                       which to write the text.
C
C       Writes a text string at the given position in the given colours
C       and style on the host display device.
C
C----------------------------------------------------------------------
C
      LOGICAL FUNCTION DHOTEX (XPOS,YPOS,STRING,LENGTH,
     +                         FGC,BGC,STYLE)
C     ========================================================
C
      INTEGER XPOS,YPOS
      CHARACTER*(*) STRING
      INTEGER LENGTH, FGC, BGC, STYLE
C
      INCLUDE 'UIXCOM'
C
      INTEGER XSAVE, YSAVE
      LOGICAL STATUS
C
      LOGICAL MENTOU
C
      CALL SX11CR( XSAVE, YSAVE )
      IF (STYLE .EQ. INVERS) THEN
         CALL SX11MC( BGC, FGC )
      ELSE
         CALL SX11MC( FGC, BGC )
      ENDIF
C
      IF (FGC .NE. BGC) THEN
C
C Move cursor and write string
C
         CALL SX11CU ( XPOS, YPOS )
         STATUS = MENTOU(STRING(1:LENGTH))
      ELSE
         STATUS = .FALSE.
      ENDIF
C
      IF (XSAVE .GE. 0) CALL SX11CU ( XSAVE, YSAVE )
C
      DHOTEX = STATUS
      RETURN
C
C Copyright (C) 1988, 1989, 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION DHOCLR ( COLOUR, XPOS, YPOS, XSIZE, YSIZE )
C       ------------------------------------------------------------
C
C       PARAMETERS:
C
C       integer colour : INPUT - the colour to which the area should
C                        be cleared.
C
C       integer xpos, ypos : INPUT - the position (device coordinates)
C                            of the top left of the area to be cleared.
C
C       integer xsize, ysize : INPUT - the size of the area to clear.
C
C       Clears an area of the host display device to a given
C       colour.
C
C----------------------------------------------------------------------
C
      LOGICAL FUNCTION DHOCLR ( COLOUR, XPOS, YPOS, XSIZE, YSIZE )
C     ============================================================
C
      INTEGER COLOUR, XPOS, YPOS, XSIZE, YSIZE
C
      CHARACTER*150 SPACES
      INTEGER XSAVE, YSAVE, IY, IYPOS
      LOGICAL STATUS
C
      LOGICAL MENTOU
C
      CALL SX11CR( XSAVE, YSAVE )
C
      STATUS = .FALSE.
C
C Until wipe available at lower level just write spaces
C
      IF (XSIZE .GT. 0 .AND. YSIZE .GT. 0) THEN
         CALL SX11MC( COLOUR, COLOUR )
         IYPOS = YPOS
         DO 10 IY = 1, YSIZE
            IF (.NOT. STATUS) THEN
C
C Move cursor and write string
C
               CALL SX11CU ( XPOS, IYPOS )
               SPACES = ' '
               STATUS = MENTOU(SPACES(1:XSIZE))
            ENDIF
            IYPOS = IYPOS + 1
   10    CONTINUE
      ENDIF
C
      IF (XSAVE .GE. 0) CALL SX11CU ( XSAVE, YSAVE )
C
      DHOCLR = STATUS
      RETURN
C
C Copyright (C) 1988, 1989, 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
C----------------------------------------------------------------------
C
C     LOGICAL FUNCTION DHOHOR ( WIDTH, FGC, BGC, XSTART, XEND, YPOS,
C                               DLEFT, DRIGHT, DOWNWD )
C     --------------------------------------------------------------
C
C       PARAMETERS:
C
C       integer width : INPUT - the width of the line.
C
C       integer fgc, bgc : INPUT - the foreground and background
C                          colours in which to draw the line.
C
C       integer xstart, xend : INPUT - the start & end positions of the
C                              line (device coordinates)
C
C       integer ypos : INPUT - the y position of the line
C
C       logical dleft, dright : INPUT - flags indicating if left and
C                               right ends of line need verticals.
C
C       logical downwd : INPUT - flag indicating direction of verticals.
C
C       Draws a horizontal line on the host.  Special action is
C       taken at ends if needed if it is meeting a vertical line.
C
C       Function returns TRUE in case of failure, otherwise FALSE
C
C----------------------------------------------------------------------
C
      LOGICAL FUNCTION DHOHOR ( WIDTH, FGC, BGC, XSTART, XEND, YPOS,
     +                          DLEFT, DRIGHT, DOWNWD )
C     ==============================================================
C
      INTEGER WIDTH, FGC, BGC, XSTART, XEND, YPOS
      LOGICAL DLEFT, DRIGHT, DOWNWD
C
      CHARACTER*150 STRING
      INTEGER XSAVE, YSAVE, XLEN, IX, IC
      LOGICAL STATUS
C
      LOGICAL MENTOU
C
      CALL SX11CR( XSAVE, YSAVE )
C
      STRING = ' '
      STATUS = .FALSE.
C
      IF (XSTART .GT. XEND) THEN
         XLEN = XSTART - XEND + 1
         IX = XEND
      ELSE
         XLEN = XEND - XSTART + 1
         IX = XSTART
      ENDIF
C
      CALL SX11CU( IX, YPOS )
C
      IF (FGC .NE. BGC) THEN
         CALL SX11MC ( FGC, BGC )
         DO 10 IX = 1,XLEN
            IC = 18
            IF (IX .EQ. 1) THEN
               IF (DLEFT) THEN
                  IF (DOWNWD) THEN
                     IC = 13
                  ELSE
                     IC = 14
                  ENDIF
               ENDIF
            ENDIF
            IF (IX .EQ. XLEN) THEN
               IF (DRIGHT) THEN
                  IF (DOWNWD) THEN
                     IC = 12
                  ELSE
                     IC = 11
                  ENDIF
               ENDIF
            ENDIF
            STRING(IX:IX) = CHAR(IC)
   10    CONTINUE
      ENDIF
C
      IF (.NOT.STATUS) STATUS = MENTOU(STRING(1:XLEN))
C
      IF (XSAVE .GE. 0) CALL SX11CU ( XSAVE, YSAVE )
C
      DHOHOR = STATUS
      RETURN
      IJUNK = WIDTH
C
C Copyright (C) 1988, 1989, 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
C----------------------------------------------------------------------
C
C     LOGICAL FUNCTION DHOVER ( WIDTH, FGC, BGC, YSTART, YEND, XPOS,
C                               DTOP, DBOT, RITEWD )
C     --------------------------------------------------------------
C
C       PARAMETERS:
C
C       integer width : INPUT - the width of the line.
C
C       integer fgc, bgc : INPUT - the foreground and background
C                          colours in which to draw the line.
C
C       integer ystart, yend : INPUT - the start & end positions of the
C                              line (device coordinates)
C
C       integer xpos : INPUT - the x position of the line
C
C       logical dtop, dbot : INPUT - flags indicating if top and
C                            bottom ends of line need horizontals.
C
C       logical ritewd : INPUT - flag indicating direction of
C                        horizontals.
C
C       Draws a vertical line on the host device.  Special action
C       is taken at ends if needed if it is meeting a horizontal line.
C
C       Function returns TRUE in case of failure, otherwise FALSE
C
C----------------------------------------------------------------------
C
      LOGICAL FUNCTION DHOVER ( WIDTH, FGC, BGC, YSTART, YEND, XPOS,
     +                          DTOP, DBOT, RITEWD )
C     ==============================================================
C
      INTEGER WIDTH, FGC, BGC, YSTART, YEND, XPOS
      LOGICAL DTOP, DBOT, RITEWD
C
      CHARACTER*150 STRING
      INTEGER XSAVE, YSAVE, YLEN, IP, IY, IC
      LOGICAL STATUS
C
      LOGICAL SX11VO
C
      CALL SX11CR( XSAVE, YSAVE )
      STRING = ' '
      STATUS = .FALSE.
C
      IF (YSTART .GT. YEND) THEN
         YLEN = YSTART - YEND + 1
         IP = YEND
      ELSE
         YLEN = YEND - YSTART + 1
         IP = YSTART
      ENDIF
C
      IF (FGC .NE. BGC) THEN
         CALL SX11MC ( FGC, BGC )
         DO 10 IY = 1,YLEN
            IC = 25
            IF (IY .EQ. 1) THEN
               IF (DTOP) THEN
                  IF (RITEWD) THEN
                     IC = 13
                  ELSE
                     IC = 12
                  ENDIF
               ENDIF
            ENDIF
            IF (IY .EQ. YLEN) THEN
               IF (DBOT) THEN
                  IF (RITEWD) THEN
                     IC = 14
                  ELSE
                     IC = 11
                  ENDIF
               ENDIF
            ENDIF
            STRING(IY:IY) = CHAR(IC)
   10    CONTINUE
      ENDIF
C
      CALL SX11CU( XPOS, IP )
C
      IF (.NOT.STATUS) STATUS = SX11VO(STRING(1:YLEN))
C
      IF (XSAVE .GE. 0) CALL SX11CU ( XSAVE, YSAVE )
C
      DHOVER = STATUS
      RETURN
      IJUNK = WIDTH
C
C Copyright (C) 1988, 1989, 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
