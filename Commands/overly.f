C Semper 6 processing module OVERLY
C
      SUBROUTINE OVERLY
C
C Controls all aspects of 8 overlays belonging to the display window.
C The overlays are numbered from 1 to 8.  A overlay number is specified
C by means of the NUMBER key (key name is optional and default is 1).
C Each overlay has associated with it a colour and visibility (whether
C it is turned on or off).  The overlay colour is specified either by
C means of one of the options BLACK, WHITE, RED, GREEN, BLUE, CYAN,
C MAGENTA or YELLOW, or explicitly with the keys RGB, RG2 and RG3.
C The keys HSV, HS2 and HS3 may be used in preference to RGB,RG2,RG3.
C The 8 colour options and the RGB key are mutually exclusive.  If the
C RGB key is set, the RG2 and RG3 keys must also be set.  The option ON
C or OFF specifies whether an overlay is visible or not.  An overlay
C will be used for graphics output, for displaying rubberband lines or
C for displaying the cursor if the option GRAPHICS, RUBBERBAND or
C CURSOR is given.  Note that separate overlays must be used for
C displaying graphics, rubberbanding and the cursor.  The overlays
C are displayed one on top of each other in strict numerical sequence
C with overlay 8 displayed in preference to all the other overlays,
C overlay 7 displayed in preference to overlays 1 to 6, and so on.
C The SHOW option causes the current overlay status to be reported on
C the console output stream.  The ERASE option causes the contents of
C the specified overlay to be cleared.
C
C Syntax:  Overlay :OVERLY $1=1 number=$1 +
C            on off graphics rubberband cursor show +
C            black white red green blue cyan magenta yellow +
C            rgb= rg2= rg3= hsv= hs2= hs3=
C
      LOGICAL OPT,VARSET,CONOPT,SEMCON
      LOGICAL FSOV61,FSER61,FSFL61
      INTEGER IVAL
      REAL    VAL
C
      LOGICAL LRGB,LHSV
      INTEGER I,J,K,N,RED(8),GREEN(8),BLUE(8),VISIB(8)
      INTEGER COLRED(8),COLGRE(8),COLBLU(8),NCOLOR(8)
      INTEGER ICOL(8),IH,GOVER,ROVER,COVER
      REAL    R(8),G(8),B(8),RGB(0:6,3),HUE,SATURN,VALUE,H,DH
      CHARACTER*3 VIS(8)
      CHARACTER*7 COLOUR(0:8)
C
      INCLUDE 'COMMON'
C
C Packed names
C
      INTEGER NNUMBE,NON,NOFF,NGRAPH,NRUBBE,NCURSO,NSHOW,NERASE
      INTEGER NBLACK,NWHITE,NRED,NGREEN,NBLUE,NCYAN,NMAGEN,NYELLO
      INTEGER NRGB,NRG2,NRG3,NHSV,NHS2,NHS3
      PARAMETER (NNUMBE=23253, NON=24560, NOFF=24246, NERASE=8721)
      PARAMETER (NGRAPH=11921, NRUBBE=29642, NCURSO=5658, NSHOW=30735)
      PARAMETER (NBLACK=3681, NWHITE=-5130, NRED=29004, NGREEN=11925)
      PARAMETER (NBLUE=3701, NCYAN=5801, NMAGEN=20847, NYELLO=-8213)
      PARAMETER (NRGB=29082, NRG2=29112, NRG3=29113)
      PARAMETER (NHSV=13582, NHS2=13592, NHS3=13593)
C
      DATA COLRED / 0,255,255,  0,  0,  0,255,255 /
      DATA COLGRE / 0,255,  0,255,  0,255,  0,255 /
      DATA COLBLU / 0,255,  0,  0,255,255,255,  0 /
C
      DATA COLOUR / '       ','black  ','white  ',
     +              'red    ','green  ','blue   ',
     +              'cyan   ','magenta','yellow ' /
C
      DATA NCOLOR / NBLACK,NWHITE,
     +              NRED,NGREEN,NBLUE,
     +              NCYAN,NMAGEN,NYELLO /
C
      DATA RGB    / 0.0,0.0,1.0,1.0,1.0,0.0,0.0,
     +              1.0,0.0,0.0,0.0,1.0,1.0,1.0,
     +              1.0,1.0,1.0,0.0,0.0,0.0,1.0 /
C
C
      IF (MEDN(1).LE.0) THEN
          ERROR=77
          IDMESS='Display is not assigned'
          RETURN
      ENDIF
C
C Fetch current overlay set-up
C
      IF (FSOV61(1,RED,GREEN,BLUE,VISIB,GOVER,ROVER,COVER,ERROR))
     +   RETURN
C
C Convert overlay colour intensities to percentages
C
      DO 10 I=1,8
         R(I)=100.0*REAL(RED(I))/255.0
         G(I)=100.0*REAL(GREEN(I))/255.0
         B(I)=100.0*REAL(BLUE(I))/255.0
   10 CONTINUE
C
C Fetch value of key NUMBER - overlay number
C
      N=IVAL(NNUMBE)
C
C Fault bad value for overlay number
C
      IF (N.LT.1.OR.N.GT.8) THEN
         ERROR=3
         IDERR=NNUMBE
         RETURN
      ENDIF
C
C Fault conflicting options ON and OFF
C
      IF (CONOPT(NON,NOFF)) RETURN
C
C Fault conflicting options GRAPHICS, RUBBERBAND and CURSOR
C
      IF (CONOPT(NGRAPH,NRUBBE)) RETURN
      IF (CONOPT(NRUBBE,NCURSO)) RETURN
      IF (CONOPT(NCURSO,NGRAPH)) RETURN
C
C See if any of options BLACK, WHITE, RED, GREEN, BLUE, CYAN, MAGENTA
C and YELLOW are set, faulting any conflict
C
      K=0
      DO 20 I=1,8
         IF (OPT(NCOLOR(I))) THEN
            IF (K.EQ.0) THEN
               K=I
            ELSE
               ERROR=60
               IDERR=NCOLOR(I)
               IDERR2=NCOLOR(K)
               RETURN
            ENDIF
         ENDIF
   20 CONTINUE
C
C See if keys RGB and HSV are set
C
      LRGB=VARSET(NRGB)
      LHSV=VARSET(NHSV)
C
C Fault conflict between keys RGB and HSV
C
      IF (LRGB.AND.LHSV) THEN
         ERROR=60
         IDERR=NRGB
         IDERR2=NHSV
         RETURN
      ENDIF
C
C Fault conflict between RGB key and colour option
C
      IF (LRGB.AND.K.NE.0) THEN
         ERROR=60
         IDERR=NRGB
         IDERR2=NCOLOR(K)
         RETURN
      ENDIF
C
C Fault conflict between HSV key and colour option
C
      IF (LHSV.AND.K.NE.0) THEN
         ERROR=60
         IDERR=NHSV
         IDERR2=NCOLOR(K)
         RETURN
      ENDIF
C
C If RGB key is set, set up new overlay colour using that key and keys
C RG2 and RG3
C
      IF (LRGB) THEN
C
C Fault key RG2 or RG3 not being set
C
         IF (.NOT.(VARSET(NRG2).AND.VARSET(NRG3))) THEN
            ERROR=3
            IDERR=NRGB
            RETURN
         ENDIF
C
C Set up new overlay colour intensities as specified
C
         R(N)=VAL(NRGB)
         G(N)=VAL(NRG2)
         B(N)=VAL(NRG3)
C
C Fault bad intensity values (percentages must lie in range 0 to 100)
C
         IF (R(N).LT.0.0.OR.R(N).GT.100.0.OR.
     +       G(N).LT.0.0.OR.G(N).GT.100.0.OR.
     +       B(N).LT.0.0.OR.B(N).GT.100.0) THEN
            ERROR=3
            IDERR=NRGB
            RETURN
         ENDIF
      ENDIF
C
C If HSV key is set, set up new overlay colour using that key and keys
C HS2 and HS3
C
      IF (LHSV) THEN
C
C Fault key HS2 or HS3 not being set
C
         IF (.NOT.(VARSET(NHS2).AND.VARSET(NHS3))) THEN
            ERROR=3
            IDERR=NHSV
            RETURN
         ENDIF
C
C Fetch value for key HSV, HS2 and HS3 (hue, saturation and value)
C
         HUE=VAL(NHSV)
         SATURN=VAL(NHS2)
         VALUE=VAL(NHS3)
C
C Fault bad value for key HS2 or HS3
C
         IF (SATURN.LT.0.0.OR.SATURN.GT.100.0.OR.
     +       VALUE.LT.0.0.OR.VALUE.GT.100.0) THEN
            ERROR=3
            IDERR=NHSV
            RETURN
         ENDIF
C
C Convert hue, saturation and value to RGB intensities
C
         H=MOD(HUE/60.0,6.0)
         IF (H.LT.0.0) H=H+6.0
         IH=INT(H)
         DH=H-REAL(IH)
C
         SATURN=SATURN/100.0
C
         R(N)=VALUE*(1.0-SATURN*((1.0-DH)*RGB(IH,1)+DH*RGB(IH+1,1)))
         G(N)=VALUE*(1.0-SATURN*((1.0-DH)*RGB(IH,2)+DH*RGB(IH+1,2)))
         B(N)=VALUE*(1.0-SATURN*((1.0-DH)*RGB(IH,3)+DH*RGB(IH+1,3)))
      ENDIF
C
C If overlay colour specified by means of RGB or HSV key, convert
C intensities to range 0 to 255
C
      IF (LRGB.OR.LHSV) THEN
         RED(N)=NINT(255.0*R(N)/100.0)
         GREEN(N)=NINT(255.0*G(N)/100.0)
         BLUE(N)=NINT(255.0*B(N)/100.0)
      ENDIF
C
C If colour option is set, set up new overlay colour as specified
C
      IF (K.NE.0) THEN
         RED(N)=COLRED(K)
         GREEN(N)=COLGRE(K)
         BLUE(N)=COLBLU(K)
C
C Convert intensities to percentages
C
         R(N)=100.0*RED(N)/255.0
         G(N)=100.0*GREEN(N)/255.0
         B(N)=100.0*BLUE(N)/255.0
      ENDIF
C
C If option ON is set, set overlay visibility on
C
      IF (OPT(NON)) THEN
         VISIB(N)=1
      ENDIF
C
C If option OFF is set, set overlay visibility off
C
      IF (OPT(NOFF)) THEN
         VISIB(N)=0
      ENDIF
C
C If option GRAPHICS set, make specified overlay the graphics overlay
C
      IF (OPT(NGRAPH)) THEN
         IF ( N .EQ. ROVER .OR. N .EQ. COVER ) THEN
            GOTO 70
         ELSE
            GOVER=N
         ENDIF
      ENDIF
C
C If option RUBBERBAND set, make specified overlay the r/band overlay
C
      IF (OPT(NRUBBE)) THEN
         IF ( N .EQ. GOVER .OR. N .EQ. COVER ) THEN
            GOTO 70
         ELSE
            ROVER=N
         ENDIF
      ENDIF
C
C If option CURSOR set, make specified overlay the cursor overlay
C
      IF (OPT(NCURSO)) THEN
         IF ( N .EQ. GOVER .OR. N .EQ. ROVER ) THEN
            GOTO 70
         ELSE
            COVER=N
         ENDIF
      ENDIF
C
C Download new overlay set-up
C
      IF (FSOV61(2,RED,GREEN,BLUE,VISIB,GOVER,ROVER,COVER,ERROR))
     +   RETURN
C
C If option ERASE is set, clear specified overlay
C
      IF (OPT(NERASE)) THEN
C
C Make specified frame the graphics overlay, if not already so
C
         IF (N.NE.GOVER) THEN
            IF (FSOV61(2,RED,GREEN,BLUE,VISIB,N,ROVER,COVER,ERROR))
     +         RETURN
         ENDIF
C
C Erase contents of specified overlay
C
         IF (FSER61(2,FRSIZ(1),FRSI2(1),0,0,1,ERROR)) RETURN
C
C Flush graphics buffer
C
         IF (FSFL61(1,ERROR)) RETURN
C
C Restore graphics overlay, if necessary
C
         IF (N.NE.GOVER) THEN
            IF (FSOV61(2,RED,GREEN,BLUE,VISIB,GOVER,ROVER,COVER,ERROR))
     +         RETURN
         ENDIF
      ENDIF
C
C If option SHOW is set, list overlay set-up
C
      IF (OPT(NSHOW)) THEN
C
C Convert overlay colour to colour string and overlay visibility to
C on/off string
C
         DO 40 I=1,8
            ICOL(I)=0
            DO 30 J=1,8
               IF (COLRED(J).EQ.RED(I).AND.
     +             COLGRE(J).EQ.GREEN(I).AND.
     +             COLBLU(J).EQ.BLUE(I)) ICOL(I)=J
   30       CONTINUE
C
            IF (VISIB(I).EQ.0) THEN
               VIS(I)='off'
            ELSE
               VIS(I)='on '
            ENDIF
   40    CONTINUE
C
C Write overlay set-up to console output stream
C
         IF (SEMCON(' ')) RETURN
         IF (SEMCON('Display overlays:')) RETURN
         DO 50 I=1,8
            WRITE (RECORD,80) I,VIS(I),COLOUR(ICOL(I)),
     +                         R(I),G(I),B(I)
            IF (SEMCON(RECORD(1:62))) RETURN
   50    CONTINUE
         IF (SEMCON(' ')) RETURN
         WRITE (RECORD,90) GOVER,ROVER,COVER
         IF (SEMCON(RECORD(1:68))) RETURN
         IF (SEMCON(' ')) RETURN
      ENDIF
C
   60 RETURN
C
C Fault attempt to use same overlay for graphics/rubberbanding/cursor
C
   70 ERROR = 77
      IDMESS = 'Graphics/rubberband/cursor overlays must be different'
      GOTO 60
C
   80 FORMAT (I3,2X,A3,2X,'Colour:',1X,A7,
     +           2X,'Red,green,blue:',F6.1,',',F6.1,',',F6.1)
   90 FORMAT ('Graphics overlay:',I3,4X,'Rubberband overlay:',I3,4X,
     +        'Cursor overlay:',I3)
C
C Copyright (C) 1988:  Synoptics Ltd,  All Rights Reserved
C
      END
