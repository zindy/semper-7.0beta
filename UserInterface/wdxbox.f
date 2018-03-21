C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION WDXBOX ( DEVICE, WID, WIDTH, FGC, BGC,
C              STRING, LENGTH, STYLE, POS, XPOS, YPOS, XSIZE, YSIZE )
C       -------------------------------------------------------------
C
C       PARAMETERS:
C
C       integer device : INPUT - the device on which the window into
C                        which box is to be written is located.
C
C       integer wid : INPUT - the window into which box is to be
C                     written.
C
C       integer width : INPUT - the width of the box edge lines (1 for
C                       single line, 2 for double)
C
C       integer fgc, bgc : INPUT - the foreground and background
C                          colours in which to write the box.
C
C       character*(*) string : INPUT - banner string to write in the
C                              box to edge
C
C       integer length : INPUT - the length of the above string.  If
C                        zero, no banner is written.
C
C       integer style : INPUT - the style (NORMAL, INVERS, UNDERL) in
C                       which to write the text.
C
C       integer pos : INPUT - the banner position in the box top edge
C                     (LEFT, CENTRE or RIGHT)
C
C       integer xpos, ypos : INPUT - the top left position for the box,
C                            window coordinates
C
C       integer xsize, ysize : INPUT - the size of the box, window
C                              coordinates
C
C       Draws a box to the requested window on a dumb device device,
C       in the appropriate colours and style.  If requested, a string
C       will be written into the top edge of the box.
C
C       Function returns FALSE if sucessful, otherwise TRUE.
C
C----------------------------------------------------------------------
C
      LOGICAL FUNCTION WDXBOX ( DEVICE, WID, WIDTH, FGC, BGC,
     +          STRING, LENGTH, STYLE, POS, XPOS, YPOS, XSIZE, YSIZE )
C     ================================================================
C
      INTEGER DEVICE, WID, WIDTH, FGC, BGC
      CHARACTER*(*) STRING
      INTEGER LENGTH, STYLE, POS, XPOS, YPOS, XSIZE, YSIZE
C
      INCLUDE 'UIFCOM'
      INCLUDE 'UIXCOM'
      INCLUDE 'UIFERR'
C
C     LOCAL VARIABLES:
C
C     Return status
C
      LOGICAL STATUS
C
C     Position at which to draw lines, device coords.
C
      INTEGER TOPPOS, LEFPOS, BOTPOS, RIGPOS
C
C     Flags indicating if lines of box need to be drawn
C
      LOGICAL DLEFT, DRIGHT, DTOP, DBOT
C
C     CALLED FUNCTIONS:
C
C     Checks two rectangles for intersection
C
      LOGICAL UIXINT
C
C     Draws a horizontal line on a device
C
      LOGICAL DEVHOR
C
C     Draws a vertical line on a device
C
      LOGICAL DEVVER
C
C     Draws a horizontal line in an obscured window
C
      LOGICAL WDXXHL
C
C     Draws a vertical line in an obscured window
C
      LOGICAL WDXXVL
C
C     Writes a text string in a window
C
      LOGICAL WDXTEX
C
C     See if box intersects the clipping rectangle
C
      STATUS = .FALSE.
      TOPPOS = YPOS + WINYPO(WID)
      BOTPOS = TOPPOS + YSIZE - 1
      LEFPOS = XPOS + WINXPO(WID)
      RIGPOS = LEFPOS + XSIZE - 1
      IF ( .NOT. UIXINT ( CLIPL, CLIPT, CLIPR-CLIPL+1, CLIPB-CLIPT+1,
     +                    LEFPOS, TOPPOS, XSIZE, YSIZE ) ) GOTO 10
C
C     Yes.  Adjust box edges if needed to clip rectangle
C
      DTOP   = .TRUE.
      DLEFT  = .TRUE.
      DBOT   = .TRUE.
      DRIGHT = .TRUE.
      IF ( TOPPOS .LE. CLIPB .AND. BOTPOS .GE. CLIPT ) THEN
         IF ( LEFPOS .LE. CLIPR .AND. RIGPOS .GE. CLIPL ) THEN
            IF ( TOPPOS .LT. CLIPT ) THEN
C
C              Top edge of box clipped off
C
               TOPPOS = CLIPT
               DTOP = .FALSE.
            ENDIF
            IF ( BOTPOS .GT. CLIPB ) THEN
C
C              Bottom edge of box clipped off
C
               BOTPOS = CLIPB
               DBOT = .FALSE.
            ENDIF
            IF ( LEFPOS .LT. CLIPL ) THEN
C
C              Left edge of box clipped off
C
               LEFPOS = CLIPL
               DLEFT = .FALSE.
            ENDIF
            IF ( RIGPOS .GT. CLIPR ) THEN
C
C              Right edge of box clipped off
C
               RIGPOS = CLIPR
               DRIGHT = .FALSE.
            ENDIF
         ELSE
C
C           Box not in clipping rectangle
C
            STATUS = .FALSE.
            GOTO 10
         ENDIF
      ELSE
C
C        Box not in clipping rectangle
C
         STATUS = .FALSE.
         GOTO 10
      ENDIF
C
C     If window not obscured, can do quick draw
C
      IF ( .NOT. WINOBS(WID) ) THEN
         IF ( DTOP ) THEN
            STATUS = DEVHOR ( DEVICE, WIDTH, FGC, BGC, LEFPOS, RIGPOS,
     +                        TOPPOS, DLEFT, DRIGHT, .TRUE. )
            IF ( STATUS ) GOTO 10
         ENDIF
         IF ( DLEFT ) THEN
            STATUS = DEVVER ( DEVICE, WIDTH, FGC, BGC, TOPPOS, BOTPOS,
     +                        LEFPOS, DTOP, DBOT, .TRUE. )
            IF ( STATUS ) GOTO 10
         ENDIF
         IF ( DBOT ) THEN
            STATUS = DEVHOR ( DEVICE, WIDTH, FGC, BGC, LEFPOS, RIGPOS,
     +                        BOTPOS, DLEFT, DRIGHT, .FALSE. )
            IF ( STATUS ) GOTO 10
         ENDIF
         IF ( DRIGHT ) THEN
            STATUS = DEVVER ( DEVICE, WIDTH, FGC, BGC, TOPPOS, BOTPOS,
     +                        RIGPOS, DTOP, DBOT, .FALSE. )
            IF ( STATUS ) GOTO 10
         ENDIF
      ELSE
C
C        Window is obscured.  Draw any unobscured bits of the box
C
         IF ( DTOP ) THEN
            STATUS = WDXXHL ( DEVICE, WID, WIDTH, FGC, BGC, STYLE,
     +                        LEFPOS, RIGPOS, TOPPOS, DLEFT, DRIGHT,
     +                        .TRUE. )
            IF ( STATUS) GOTO 10
         ENDIF
         IF ( DLEFT ) THEN
            STATUS = WDXXVL ( DEVICE, WID, WIDTH, FGC, BGC, STYLE,
     +                        TOPPOS, BOTPOS, LEFPOS, DTOP, DBOT,
     +                        .TRUE. )
            IF ( STATUS) GOTO 10
         ENDIF
         IF ( DBOT ) THEN
            STATUS = WDXXHL ( DEVICE, WID, WIDTH, FGC, BGC, STYLE,
     +                        LEFPOS, RIGPOS, BOTPOS, DLEFT, DRIGHT,
     +                        .FALSE. )
            IF ( STATUS) GOTO 10
         ENDIF
         IF ( DRIGHT ) THEN
            STATUS = WDXXVL ( DEVICE, WID, WIDTH, FGC, BGC, STYLE,
     +                        TOPPOS, BOTPOS, RIGPOS, DTOP, DBOT,
     +                        .FALSE. )
            IF ( STATUS) GOTO 10
         ENDIF
      ENDIF
C
C     Now do the text, if there is some, and the top line of the box is
C     showing
C
      IF ( LENGTH .GT. 0 .AND. DTOP ) THEN
         IF ( POS .EQ. LEFT ) THEN
            LEFPOS = XPOS
         ELSE IF ( POS .EQ. CENTRE ) THEN
            LEFPOS = XPOS + XSIZE / 2 - LENGTH / 2
         ELSE IF ( POS .EQ. RIGHT ) THEN
            LEFPOS = XPOS + XSIZE - LENGTH
         ENDIF
         STATUS = WDXTEX ( DEVICE, WID, FGC, BGC, STYLE, LEFPOS, YPOS,
     +                     STRING, LENGTH )
      ENDIF
C
C     All done
C
   10 CONTINUE
C
      WDXBOX = STATUS
      RETURN
C
C Copyright (C) 1988, 1989, 1990:  Synoptics Ltd,  All Rights Reserved
C
      END
C
      LOGICAL FUNCTION WDXXHL ( DEVICE, WID, WIDTH, FGC, BGC,
     +                STYLE, XPOS, ENDPOS, Y, DLEFT, DRIGHT, DOWNWD )
C     ===============================================================
C
      INTEGER DEVICE, WID, WIDTH, FGC, BGC
      INTEGER STYLE, XPOS, ENDPOS, Y
      LOGICAL DLEFT, DRIGHT, DOWNWD
C
      INCLUDE 'UIFCOM'
      INCLUDE 'UIXCOM'
C
C     LOCAL VARIABLES:
C
C     Return status
C
      LOGICAL STATUS
C
C     X position to draw line segment
C
      INTEGER X
C
C     Length of line segment to draw
C
      INTEGER LEN
C
C     Left and right edge of obscured segment
C
      INTEGER LEDGE, REDGE
C
C     'Draw left and right corner' flags
C
      LOGICAL DXLEFT, DXRITE
C
C     CALLED FUNCTIONS:
C
C     Writes horizontal line on a device
C
      LOGICAL DEVHOR
C
C     Finds unobscured segment of area of window
C
      LOGICAL WDXFST
C
      STATUS = .FALSE.
      X = XPOS
   10 CONTINUE
C
C        Find next unobscured chunk
C
         IF ( .NOT. WDXFST ( DEVICE, WID, X, Y, ENDPOS,
     +                       LEDGE, REDGE ) ) GOTO 20
         LEN = LEDGE - X
         IF ( LEN .GT. 0 ) THEN
            IF ( X .EQ. XPOS .AND. DLEFT ) THEN
               DXLEFT = .TRUE.
            ELSE
               DXLEFT = .FALSE.
            ENDIF
            IF ( X + LEN - 1 .EQ. ENDPOS .AND. DRIGHT )  THEN
               DXRITE = .TRUE.
            ELSE
               DXRITE = .FALSE.
            ENDIF
            STATUS = DEVHOR ( DEVICE, WIDTH, FGC, BGC, X, X + LEN - 1,
     +                        Y, DXLEFT, DXRITE, DOWNWD )
            IF ( STATUS ) GOTO 30
         ENDIF
         X = REDGE + 1
         GOTO 10
   20 CONTINUE
C
C     Draw the last chunk
C
      LEN = LEDGE - X
      IF ( LEN .GT. 0 ) THEN
         IF ( X .EQ. XPOS .AND. DLEFT ) THEN
            DXLEFT = .TRUE.
         ELSE
            DXLEFT = .FALSE.
         ENDIF
         IF ( X + LEN - 1 .EQ. ENDPOS .AND. DRIGHT )  THEN
            DXRITE = .TRUE.
         ELSE
            DXRITE = .FALSE.
         ENDIF
         STATUS = DEVHOR ( DEVICE, WIDTH, FGC, BGC, X, X + LEN - 1, Y,
     +                     DXLEFT, DXRITE, DOWNWD )
         IF ( STATUS ) GOTO 30
      ENDIF
C
C     All done
C
   30 CONTINUE
C
      WDXXHL = STATUS
      RETURN
      IJUNK = STYLE
C
C Copyright (C) 1988 - 1991:  Synoptics Ltd,  All Rights Reserved
C
      END
C
      LOGICAL FUNCTION WDXXVL ( DEVICE, WID, WIDTH, FGC, BGC, STYLE,
     +                          YPOS, ENDPOS, X, DTOP, DBOT, RITEWD )
C     ===============================================================
C
      INTEGER DEVICE, WID, WIDTH, FGC, BGC
      INTEGER STYLE, YPOS, ENDPOS, X
      LOGICAL DTOP, DBOT, RITEWD
C
      INCLUDE 'UIFCOM'
      INCLUDE 'UIXCOM'
C
C     LOCAL VARIABLES:
C
C     Return status
C
      LOGICAL STATUS
C
C     Y position to draw line segment
C
      INTEGER Y
C
C     Length of line segment to draw
C
      INTEGER LEN
C
C     Top and bottom edge of obscured segment
C
      INTEGER TEDGE, BEDGE
C
C     'Draw top and bottom corner' flags
C
      LOGICAL DXTOP, DXBOT
C
C     CALLED FUNCTIONS:
C
C     Writes vertical line on a device
C
      LOGICAL DEVVER
C
C     Finds unobscured vertical segment of area of window
C
      LOGICAL WDXFVS
C
      STATUS = .FALSE.
      Y = YPOS
   10 CONTINUE
C
C        Find next unobscured chunk
C
         IF ( .NOT. WDXFVS ( DEVICE, WID, Y, ENDPOS, X,
     +                       TEDGE, BEDGE ) ) GOTO 20
         LEN = TEDGE - Y
         IF ( LEN .GT. 0 ) THEN
            IF ( Y .EQ. YPOS .AND. DTOP ) THEN
               DXTOP = .TRUE.
            ELSE
               DXTOP = .FALSE.
            ENDIF
            IF ( Y + LEN - 1 .EQ. ENDPOS .AND. DBOT )  THEN
               DXBOT = .TRUE.
            ELSE
               DXBOT = .FALSE.
            ENDIF
            STATUS = DEVVER ( DEVICE, WIDTH, FGC, BGC, Y, Y + LEN - 1,
     +                        X, DXTOP, DXBOT, RITEWD )
            IF ( STATUS ) GOTO 30
         ENDIF
         Y = BEDGE + 1
         GOTO 10
   20 CONTINUE
C
C     Draw the last chunk
C
      LEN = TEDGE - Y
      IF ( LEN .GT. 0 ) THEN
         IF ( Y .EQ. YPOS .AND. DTOP ) THEN
            DXTOP = .TRUE.
         ELSE
            DXTOP = .FALSE.
         ENDIF
         IF ( Y + LEN - 1 .EQ. ENDPOS .AND. DBOT )  THEN
            DXBOT = .TRUE.
         ELSE
            DXBOT = .FALSE.
         ENDIF
         STATUS = DEVVER ( DEVICE, WIDTH, FGC, BGC, Y, Y + LEN - 1, X,
     +                     DXTOP, DXBOT, RITEWD )
         IF ( STATUS ) GOTO 30
      ENDIF
C
C     All done
C
   30 CONTINUE
C
      WDXXVL = STATUS
      RETURN
      IJUNK = STYLE
C
C Copyright (C) 1988 - 1991:  Synoptics Ltd,  All Rights Reserved
C
      END
C
      LOGICAL FUNCTION WDXFVS ( DEVICE, WID, Y, ENDY, X, TEDGE, BEDGE )
C     =================================================================
C
      INTEGER DEVICE, WID, Y, ENDY, X, TEDGE, BEDGE
C
      INCLUDE 'UIFCOM'
      INCLUDE 'UIXCOM'
      INCLUDE 'UIFERR'
C
C     LOCAL VARIABLES:
C
C     Return value indicating segment found
C
      LOGICAL FOUND
C
C     True if bottom edge already found
C
      LOGICAL BFOUND
C
C     Window which generated bottom end of area
C
      INTEGER BOTWIN
C
C     End position of unclipped string
C
      INTEGER ENDPOS
C
C     Left, top, right and bottom edges of window.  Top & bottom edges
C     adjusted to edges of area to clear
C
      INTEGER WINLEF, WINTOP, WINRIG, WINBOT
C
C     Loop counters
C
      INTEGER I, J
C
      FOUND  = .FALSE.
      BFOUND = .FALSE.
      ENDPOS = ENDY
      TEDGE = ENDPOS + 1
      BEDGE = ENDPOS + 1
C
C     Walk up the stack to all windows above one being written into
C     seeing if they cross the area to be written into.  Note we have
C     to scan top to bottom across the area we are looking at
C
      DO 30 I = WINSTK(WID), MAXWIN
         IF ( I .EQ. WINSTK(WID) ) GOTO 30
         DO 10 J = MINWIN, MAXWIN
            IF ( (WINNAM(J) .GE. 0) .AND.
     +           (WINODE(J) .EQ. DEVICE) .AND.
     +           (WINSTK(J) .EQ. I ) .AND.
     +            WINISS(J) ) GOTO 20
   10    CONTINUE
C
C        Empty stack position.  Try again
C
         GOTO 30
   20    CONTINUE
C
C        Window j is now the one at stack position i.  See if it
C        crosses the line position
C
         WINLEF = WINXPO(J)
         WINRIG = WINLEF + WINXSI(J) - 1
         IF ( X .GE. WINLEF .AND. X .LE. WINRIG ) THEN
            WINTOP = WINYPO(J)
            IF ( WINTOP .LT. Y ) WINTOP = Y
            WINBOT = WINYPO(J) + WINYSI(J) - 1
            IF ( WINBOT .GT. ENDPOS ) WINBOT = ENDPOS
            IF ( Y .LE. WINBOT .AND. ENDPOS .GE. WINTOP ) THEN
C
C              Yes, so see if this contributes to either end of
C              the currently calculated obscuring segment.
C
C               if ( wintop .ge. y .and. winbot .le. tedge ) then
               IF ( WINTOP .GE. Y .AND. WINTOP .LE. TEDGE ) THEN
                  TEDGE = WINTOP
C
C                 If bottom edge found, see if we need to swap it if
C                 this window is toper than the one which formed
C                 bottom edge
C
                  IF ( BFOUND ) THEN
                     IF ( WINYPO(J) .LT. WINYPO(BOTWIN) ) THEN
                        IF ( WINYPO(J) + WINYSI(J) - 1 .LT.
     +                       WINYPO(BOTWIN) ) THEN
                           BEDGE = WINBOT
                           BOTWIN = J
                           BFOUND = .TRUE.
                        ENDIF
                     ENDIF
                  ENDIF
                  FOUND = .TRUE.
               ENDIF
               IF ( WINBOT .GE. TEDGE .AND. WINTOP .LE. BEDGE + 1 ) THEN
                  IF ( BFOUND ) THEN
                     IF ( WINBOT .GT. BEDGE ) THEN
                        BOTWIN = J
                        BEDGE = WINBOT
                     ENDIF
                  ELSE
                     BOTWIN = J
                     BEDGE = WINBOT
                  ENDIF
                  FOUND  = .TRUE.
                  BFOUND = .TRUE.
               ENDIF
            ENDIF
         ENDIF
   30 CONTINUE
C
C     All done
C
      WDXFVS = FOUND
      RETURN
C
C Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
C
      END
