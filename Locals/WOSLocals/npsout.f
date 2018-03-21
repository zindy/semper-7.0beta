C Semper 6 processing module NPSFL
C
C WOS mod: option MANUAL added
C WOS mods in progress: false colour mode
C - key WITH defines LUT applied to data before output; if this
C   is FALSE, output is false colour using PostScript
C   colorimage operator rather than image; LUT is considered to
C   define mapping for source range MIN:MAX
C - WITH must be PICTURE, not display lut; improve later?
C - full colour to follow later
C - may be OK for screendump mode as well as picture mode
C
C Problems found
C - SEMLAB changes RECORD (undocumented disaster causing a Warning msg
C   to be written to the PostScript file (line 2) for disc pics w/o a
C   Semper title
C - NPSFL1 includes Vax-specific code testing ; in file names which
C   messes up the file handling...
C
      SUBROUTINE NPSFL
C
C Generates a text file containing the source image (or sub-region
C thereof), together with appropriate annotation, in Postscript form.
C The output file name is specified with the NAME key (with default
C file name extension = ".ps").  If the ENCAPSULATED option is set,
C only the image data is output with no header or annotation (in a
C form suitable for including into other documents, with default file
C name extension = ".eps").  Any number of copies can be run off
C by means of the COPIES key.  The default scaling for the image is
C one point (1/72 inch) per pixel.  The TIMES key may be used to
C impose any scaling factor on top of the default scaling.  By default,
C the image is output with the y-axis pointing upwards.  Most printers
C use paper with the long axis set in the vertical direction (portrait
C orientation).  For images that are wider than they are tall, the
C option LANDSCAPE (as opposed to PORTRAIT) may be used to turn the
C image through 90 degrees, so that the y-axis points from right to
C left.  If the option BORDER is set, a border is output around the
C image.  If the option ORIGIN is set, tick marks are added to mark
C the position of the origin.  Any tick marks that would lie off the
C border are omitted.  A caption string may be output above (ABOVE
C option set) or below (default or BELOW option set) the image in
C 10 point Helvetica font, by means of the TEXT key.  Standard picture
C information is output at the top of the page.  This may be suppressed
C by specifying the option NOHEADER.  A complex picture is output in
C the same way as DISPLAY, with the real and imaginary parts appearing
C one alongside the other.  Pixel values are scaled in the same way as
C DISPLAY (picture range or MIN,MAX if PRESET option is set).
C
C A screen
C dumping facility is available with the options FRAME, PARTITION and
C PICTURE.  If one of these options is specified, the corresponding
C region of the display is output, in the same way as a normal picture
C is output, but with any display annotation superimposed on the image.
C The options BLACK and WHITE (default) specify whether the annotation
C appears as black or white when printed.  The header for a display
C region contains a reduced set of information relevant to that region.
C
C Verb descriptor:
C
C  Postscript :NPSFL $1= frame partition picture encapsulated name=' +
C    old new copies=1 times=1 portrait landscape preset black white +
C    size= si2= position= po2= left right top bottom layer= la2= +
C    border origin text=' above below header manual with=
C
C Global declarations
      INCLUDE 'COMMON'
C
      INTEGER IB1(LNBUF/LNINT),LABEL(LNLAB),IB2(LNBUF/LNINT)
      INTEGER IB3(LNBUF/LNINT),IB4(LNBUF/LNINT),LUT(LNBUF/LNINT)
      EQUIVALENCE (RB1,LABEL,IB1),(RB2,IB2),(RB3,IB3),(RB4,IB4)
      EQUIVALENCE (RB5,LUT)
C
C Local declarations
c debug
      logical semcon
c
      INTEGER IVALPN
      INTEGER NBLANK
C     Change: LDM, July 2005
C     Ensure that gfortran does not think of range, unpack as intrinsics
      EXTERNAL RANGE, UNPACKF
      LOGICAL CONOPT,OPT,RANGE,SEMIOE,SEMOPN,SEMROW,VARSET,SEMLAB,ABANDN
      LOGICAL FSRI61,FSOI61,TSTSRG,FSOPTN,FSINIT,FSREGN
      LOGICAL NPSFL0,NPSFL1,NPSFL3,NPSFL4
      LOGICAL LVLUT,LFALSE
      CHARACTER*11 DATSTR
      CHARACTER*8  TIMSTR
      LOGICAL LENCAP,LDISP,LCOMPL,LBORDE,LORIGI,LCAPTN,LHEADE
      INTEGER NCOL,NROW,NLAY,CLASS,FORM,CCOL,CROW
      INTEGER MODE,NDIS,IMOFF,DATIME(7),NLF
      INTEGER I,I1,I2,J,J1,J2,K,K1,K2,M,N,ICOL,IROW,ILAY
      INTEGER ICOL1,ICOL2,IROW1,IROW2,ILAY1,ILAY2
      INTEGER NF,IOS,NPIX,FRAME,X,NPIC,ANNOT
      INTEGER NCPLO,IP,OP,NCLUT,NRLUT,NLLUT,CLLUT,FLUT
      REAL A,B,P,XMIN,XMAX,YMIN,YMAX
C
      INTEGER NPIXEL
      PARAMETER (NPIXEL=72)
C
      CHARACTER*(NPIXEL) PIXELS
      CHARACTER*(FILMAX) FILE
      CHARACTER*4 DFEXT
C
      EQUIVALENCE (ICOL1,SMGI1),(ICOL2,SMGI4)
      EQUIVALENCE (IROW1,SMGI2),(IROW2,SMGI5)
      EQUIVALENCE (ILAY1,SMGI3),(ILAY2,SMGI6)
C
C Output image intensity scale
      REAL BLACK,WHITE
      INTEGER IBLACK,IWHITE
      PARAMETER (BLACK=33.,IBLACK=BLACK, WHITE=126.,IWHITE=WHITE)
C False colour output maps
      INTEGER FCMAP(3,IBLACK:IWHITE)
C
C Packed names
C
      INTEGER NDLLR1,NENCAP,NBLACK,NWHITE,NFRAME
      INTEGER NPARTI,NPICTU,NMANUA,NFALSE,NWITH
      PARAMETER (NDLLR1=-12441, NENCAP=8563, NBLACK=3681, NWHITE=-5130)
      PARAMETER (NFRAME=10321, NPARTI=25658, NPICTU=25963, NMANUA=20854)
      PARAMETER (NWITH=-5181)
C
C Format statements
   10 FORMAT (A,4I8)
   20 FORMAT (A,3(A,:,','))
   30 FORMAT (A,I8,A)
   40 FORMAT (I8,A)
C
C See if dumping display image
      LDISP=OPT(NFRAME).OR.OPT(NPARTI).OR.OPT(NPICTU)
      IF (LDISP) THEN
C
C Determine whether frame, partition or picture coordinates are required
         IF (FSOPTN(MODE,NDIS)) GOTO 150
C
C Initialise framestore graphics accordingly
         IF (FSINIT(MODE,NDIS)) GOTO 150
C
C Set up frame/partition/picture size
         NCOL=1+NINT(FSXSCA*(FSBRIG-FSBLEF))
         NROW=1+NINT(FSYSCA*(FSBBOT-FSBTOP))
C
C Set up frame number for display access
         FRAME=FSFRA
C
C Determine region of interest, depending on standard 2D sub-region
C keys and options specified
         IF (FSREGN(XMIN,XMAX,YMIN,YMAX)) GOTO 150
C
C Set up equivalent sub-region limits in terms of display coordinates
         ICOL1=NINT(FSXSCA*XMIN+FSXOFF)
         ICOL2=NINT(FSXSCA*XMAX+FSXOFF)
         IROW1=NINT(FSYSCA*YMAX+FSYOFF)
         IROW2=NINT(FSYSCA*YMIN+FSYOFF)
         ILAY1=1
         ILAY2=1
C
C Set up X start position for sub-region, taking into account complex
C display picture and use of options RE and IM
         X=ICOL1
         IF (FSI1.EQ.2) X=X+NINT(FSXSCA*FSIOFF)
C
C See if outputting both parts of complex display picture
         LCOMPL=FSI2.NE.FSI1
C
C Set up offset between real and imaginary part of complex display
C picture (if any)
         IMOFF=NINT(FSXSCA*FSIOFF)
C
C Set up position of origin in display coordinates ((0,0) in graphics
C coordinates)
         CCOL=NINT(FSXOFF)
         CROW=NINT(FSYOFF)
C
C Fault conflict between options BLACK and WHITE
         IF (CONOPT(NBLACK,NWHITE)) GOTO 150
C
C Set up output intensity for any display annotation (default is WHITE)
         IF (OPT(NBLACK)) THEN
            ANNOT=BLACK
         ELSE
            ANNOT=WHITE
         ENDIF
C
C Otherwise, set up specified source picture
      ELSE
C
C Determine source picture number (default = SELECT)
         IF (VARSET(NDLLR1)) THEN
            NPIC=IVALPN(NDLLR1)
         ELSE
            NPIC=NINT(SELECT)
         ENDIF
C
C Open source picture
         IF (SEMOPN(1,NPIC,NCOL,NROW,NLAY,CLASS,FORM,LP1)) GOTO 150
C
C Fault picture class that does not entail a 2-D image
         IF (CLASS.EQ.NCLMAC.OR.CLASS.EQ.NCLPLI.OR.
     +       CLASS.EQ.NCLHIS.OR.CLASS.EQ.NCLLUT) THEN
            ERROR=6
            IDERR=NPIC
            GOTO 150
         ENDIF
C
C See if complex source picture
         LCOMPL=FORM.EQ.NFMCOM
C
C Fetch position for source picture origin
         CCOL=CCOLN(LP1)
         CROW=CROWN(LP1)
C
C Establish region of picture to output (including range of layers)
         IF (TSTSRG(1,LP1)) GOTO 150
C
C Establish intensity range for region of picture
         IF (RANGE(1,LP1)) GOTO 150
C
C Fault zero intensity range
         IF (VMIN.EQ.VMAX) THEN
            ERROR=12
            IDERR=NPIC
            GOTO 150
         ENDIF
C
C Set up coefficients to transform intensity range
         A=REAL(WHITE-BLACK)/(VMAX-VMIN)
         B=BLACK-A*VMIN
      ENDIF
C
C Determine size of output image and number of output pages
      ICOL=ICOL2-ICOL1+1
      IF (LCOMPL) ICOL=2*ICOL
      IROW=IROW2-IROW1+1
      ILAY=ILAY2-ILAY1+1
C
C See if option ENCAPSULATED is set
      LENCAP=OPT(NENCAP)
      IF (LENCAP) THEN
         DFEXT = '.eps'
      ELSE
         DFEXT = '.ps'
      ENDIF
C
C Establish whether mapped via LUT
      LFALSE=.FALSE.
      LVLUT=VARSET(NWITH)
C
C If via LUT, open it
      IF (LVLUT) THEN
         IDERR=IVALPN(NWITH)
         IF (SEMOPN(1,IDERR,NCLUT,NRLUT,NLLUT,CLLUT,FLUT,LP2)) GOTO 150
C Check class and size
         IF (CLLUT.NE.NCLLUT) THEN
            ERROR=6
            GOTO 150
         ENDIF
C MONO or FALSE only pro tem..
         IF (NLLUT.NE.1.OR.(NRLUT.NE.1.AND.NRLUT.NE.3)) THEN
            ERROR=5
            GOTO 150
         ENDIF
         LFALSE=NRLUT.EQ.3
C
C Generate local colour maps, stretching input range 0:NCLUT-1 and
C output range O:LUTMAX both to PostScript char range IBLACK:IWHITE
C [Worry: while use of actual LUT length from SEMOPN evades dependence
C on local LUTLEN, there is no way of evading LUTMAX; hence LUTs will
C not be portable across implementations...]
C
C Fetch LUT
         DO 17 N=1,NRLUT
            IF (SEMROW(1,LUT,NFMINT,N,1,LP2)) GOTO 150
            DO 15 I=IBLACK,IWHITE
C Scaled pixel range IBLACK:IWHITE covers i/p range 1:NCLUT of LUT
               IP=NINT((I-BLACK)*(NCLUT-1)/(WHITE-BLACK))+1
C LUT o/p range 0:LUTMAX-1 covers final pixel range IBLACK:IWHITE
               FCMAP(N,I)=NINT(LUT(IP)*(WHITE-BLACK)/(LUTMAX-1)+IBLACK)
   15       CONTINUE
C debug...
            do 18 i1=iblack,iwhite,10
               i2=min(i1+9,iwhite)
               write (record,19) n,i1,i2,(fcmap(n,i),i=i1,i2)
 19            format ('LUT ',i1,' entries ',2i4,': ',10i4)
               if (semcon(record)) goto 150
 18         continue
   17    CONTINUE
      ENDIF
C
C Initialise output file
C ----------------------
C
C Open file
C 
      IF (NPSFL1(FILE,DFEXT)) GOTO 150
C
C Write header comments
C
      IF (LENCAP) THEN
         IF (NPSFL0('%!PS-Adobe-2.0 EPSF-2.0',IOS)) GOTO 130
      ELSE
         IF (NPSFL0('%!PS-Adobe-2.0',IOS)) GOTO 130
      ENDIF
C
      M=1
      N=33
      RECORD(M:N)='%%Title: Semper 6 Postscript file'
      IF (.NOT.LDISP) THEN
         IF (SEMLAB(1,LABEL,LP1)) GOTO 150
         IF (LABEL(LBNCTT).GT.0) THEN
            N=9+LABEL(LBNCTT)
            CALL SEMCHS(RECORD(10:N),LABEL(LBTT1),LABEL(LBNCTT))
         ENDIF
      ENDIF
C BUG FIX: semlab call above appears to corrupt RECORD; fixed w/o
C concealing bug by prefixing %% unconditionally to this line
      IF (NPSFL0('%%'//RECORD(M:MIN(M+71,N)),IOS)) GOTO 130
      M=M+72
   50 IF (M.LE.N) THEN
         RECORD(M-3:M-1)='%%+'
         IF (NPSFL0(RECORD(M-3:MIN(M+68,N)),IOS)) GOTO 130
         M=M+69
         GOTO 50
      ENDIF
C
      IF (NPSFL0('%%Creator: Semper 6 Plus',IOS)) GOTO 130
C
      CALL MCTIME(DATIME)
      RECORD(1:16)='%%CreationDate: '
      RECORD(17:27)=DATSTR(DATIME(3),DATIME(2),DATIME(1))
      RECORD(28:28)=' '
      RECORD(29:36)=TIMSTR(DATIME(4),DATIME(5),DATIME(6))
      IF (NPSFL0(RECORD(1:36),IOS)) GOTO 130
C
      IF (LENCAP) THEN
         WRITE (RDWRTU,10,ERR=130,IOSTAT=IOS)
     +      '%%BoundingBox:',0,0,ICOL,IROW
      ELSE
         WRITE (RDWRTU,10,ERR=130,IOSTAT=IOS)
     +      '%%Pages:',ILAY
         WRITE (RDWRTU,20,ERR=130,IOSTAT=IOS)
     +      '%%DocumentFonts: ','Helvetica'
      ENDIF
C
      IF (OPT(NMANUA)) THEN
         IF (NPSFL0('statusdict begin',IOS)) GOTO 130
         IF (NPSFL0('/manualfeed true def',IOS)) GOTO 130
         IF (NPSFL0('end',IOS)) GOTO 130
      ENDIF
C
      IF (NPSFL0('%%EndComments',IOS)) GOTO 130
C
C Write local dictionary definition
C
      IF (NPSFL0(' ',IOS)) GOTO 130
      IF (LENCAP) THEN
         IF (NPSFL0('5 dict begin',IOS)) GOTO 130
      ELSE
         IF (NPSFL0('60 dict begin',IOS)) GOTO 130
      ENDIF
C
C Write output definitions for image size and input buffer
C
      IF (NPSFL0(' ',IOS)) GOTO 130
      WRITE (RDWRTU,30,ERR=130,IOSTAT=IOS)
     +   '/icol',ICOL,' def',
     +   '/irow',IROW,' def',
     +   '/pixels',NPIXEL,' string def'
C
C If ENCAPSULATED option not set, set up and write information for
C header and annotation
C
      IF (.NOT.LENCAP) THEN
         IF (NPSFL4(IOS,NCOL,NROW,NLAY,CLASS,FORM,CCOL,CROW,
     +              LDISP,LCOMPL,LBORDE,LORIGI,LCAPTN,LHEADE)) GOTO 130
         IF (ERROR .NE. 0) GOTO 150
      ENDIF
C
      IF (NPSFL3(MODE,IOS,LFALSE,
     +   LENCAP,LDISP,LBORDE,LORIGI,LCAPTN,LHEADE)) GOTO 130
C
C If screen dump, offset column start and end by 1, to ensure they are
C both greater than zero (Note: display coordinates start from zero)
      IF (LDISP) THEN
         ICOL1=ICOL1+1
         ICOL2=ICOL2+1
      ENDIF
C
C Set up number of columns across region of interest
      NCOL=ICOL2-ICOL1+1
C
C Set up column indices
      IF (LCOMPL) THEN
         I1=2*ICOL1-1
         I2=2*ICOL2
      ELSE
         I1=ICOL1
         I2=ICOL2
      ENDIF
C
C Set up row indices
      J1=IROW1
      J2=IROW2
C
C Set up layer indices
      K1=ILAY1
      IF (LENCAP) THEN
         K2=ILAY1
      ELSE
         K2=ILAY2
      ENDIF
C
C Loop over layers
      DO 120 K=K1,K2
C
C If not encapsulated, write out page comment
C
         IF (.NOT.LENCAP) THEN
            IF (NPSFL0(' ',IOS)) GOTO 130
            IF (LDISP) THEN
               NLF=FRAME
            ELSE
               NLF=K
            ENDIF
            WRITE (RDWRTU,10,ERR=130,IOSTAT=IOS)
     +         '%%Page:',NLF,1+(K-K1)
         ENDIF
C
C Write out command to process image
         IF (NPSFL0(' ',IOS)) GOTO 130
         IF (NPSFL0('show_image',IOS)) GOTO 130
C
C Loop over rows
         DO 110 J=J1,J2
C
C If dumping from the display, read and scale data directly from the
C framestore and superimpose any display annotation
            IF (LDISP) THEN
C
C Read back image and overlay data
               IF (FSRI61(IB1(I1),NCOL,NFMINT,X,J,FRAME,BLACK,WHITE,0,
     +            ERROR)) GOTO 150
               IF (OVLIND(FSDEV)) THEN
                  IF (FSOI61(IB2(I1),NCOL,X,J,FRAME,0,ERROR)) GOTO 150
               ELSE
                  IF (FSOI61(IB2(I1),NCOL,X,J,0,0,ERROR)) GOTO 150
               ENDIF
C
C If complex display picture, read back imaginary part also
               IF (LCOMPL) THEN
                  IF (FSRI61(IB1(I1+NCOL),NCOL,NFMINT,X+IMOFF,J,FRAME,
     +               BLACK,WHITE,0,ERROR)) GOTO 150
                  IF (OVLIND(FSDEV)) THEN
                     IF (FSOI61(IB2(I1+NCOL),NCOL,X+IMOFF,J,
     +                          FRAME,0,ERROR)) GOTO 150
                  ELSE
                     IF (FSOI61(IB2(I1+NCOL),NCOL,X+IMOFF,J,0,0,ERROR))
     +                  GOTO 150
                  ENDIF
               ENDIF
C
C Superimpose overlay data on image data
               DO 60 I=I1,I2
                  IF (IB2(I).NE.0) IB1(I)=ANNOT
   60          CONTINUE
C
C Check for abandon request
               IF (ABANDN(ERROR)) GOTO 150
C
C Otherwise, read data from source picture and scale it
            ELSE
C
C Read source row from LP1, splitting real and imaginary parts of a
C complex source picture
               IF (LCOMPL) THEN
                  IF (SEMROW(1,RB2,NFMCOM,J,K,LP1)) GOTO 150
                  M=2*ICOL1-1
                  DO 70 I=I1,I1+(NCOL-1)
                     RB1(I)=RB2(M)
                     RB1(I+NCOL)=RB2(M+1)
                     M=M+2
   70             CONTINUE
               ELSE
                  IF (SEMROW(1,RB1,NFMFP,J,K,LP1)) GOTO 150
               ENDIF
C
C Scale the picture data into the range 33 to 126
               DO 80 I=I1,I2
C
C Transform the pixel value
                  P=A*RB1(I)+B
C
C Convert this value to an integer in the range 33 to 126
                  IF (P.LE.BLACK) THEN
                     IB1(I)=33
                  ELSE IF (P.GE.WHITE) THEN
                     IB1(I)=126
                  ELSE
                     IB1(I)=NINT(P)
                  ENDIF
   80          CONTINUE
            ENDIF
C
C If via LUTs, apply map(s)
            IF (LVLUT) THEN
               DO 85 I=I1,I2
                  IF (LFALSE) THEN
                     IB3(I)=FCMAP(2,IB1(I))
                     IB4(I)=FCMAP(3,IB1(I))
                  ENDIF
                  IB1(I)=FCMAP(1,IB1(I))
   85          CONTINUE
            ENDIF
C
C Write out row data
            IF (LFALSE) THEN
               NCPLO=NPIXEL/3
            ELSE
               NCPLO=NPIXEL
            ENDIF
            DO 100 I=I1,I2,NCPLO
C
C Assemble next output record
               OP=1
               DO 90 N=1,MIN(I2-I+1,NCPLO)
C
C Add pixel value(s) to character buffer
                  PIXELS(OP:OP)=CHAR(IB1(I+N-1))
                  IF (LFALSE) THEN
                     PIXELS(OP+1:OP+1)=CHAR(IB3(I+N-1))
                     PIXELS(OP+2:OP+2)=CHAR(IB4(I+N-1))
                     OP=OP+3
                  ELSE
                     OP=OP+1
                  ENDIF
   90          CONTINUE
C
C Write out contents of character buffer
               IF (NPSFL0(PIXELS(1:OP-1),IOS)) GOTO 130
  100       CONTINUE
  110    CONTINUE
C
C Write out remaining Postscript commands for this layer (none if
C ENCAPSULATED option is set)
         IF (.NOT.LENCAP) THEN
            IF (NPSFL0(' ',IOS)) GOTO 130
C
            IF (LBORDE) THEN
               IF (NPSFL0('show_border',IOS)) GOTO 130
            ENDIF
C
            IF (LORIGI) THEN
               IF (NPSFL0('show_origin',IOS)) GOTO 130
            ENDIF
C
            IF (LCAPTN) THEN
               IF (NPSFL0('show_caption',IOS)) GOTO 130
            ENDIF
C
            IF (LHEADE) THEN
               WRITE (RDWRTU,40,ERR=130,IOSTAT=IOS)
     +            NLF,' show_header'
            ENDIF
C
            IF (NPSFL0('showpage',IOS)) GOTO 130
         ENDIF
  120 CONTINUE
C
C Write out trailer
      IF (NPSFL0(' ',IOS)) GOTO 130
      IF (NPSFL0('%%Trailer',IOS)) GOTO 130
      IF (NPSFL0('settransfer',IOS)) GOTO 130
C
C If ENCAPSULATED option not set, write out command to reset
C #copies to 1
      IF (.NOT.LENCAP) THEN
         IF (NPSFL0(' ',IOS)) GOTO 130
         IF (NPSFL0('/#copies 1 def',IOS)) GOTO 130
      ENDIF
C
C Write out command to remove local dictionary from dictionary stack
C
      IF (NPSFL0(' ',IOS)) GOTO 130
      IF (NPSFL0('end',IOS)) GOTO 130
      IF (NPSFL0(' ',IOS)) GOTO 130
C
C ****** CHANGE ******
C
C Close the output file
      CLOSE (RDWRTU,ERR=130,IOSTAT=IOS)
C
C ****** ****** ******
C
      GOTO 150
C
C CLOSE and WRITE errors
  130 NF = NBLANK(FILE)
      IF (SEMIOE(IOS,RDWRTU,FILE(1:NF))) GOTO 140
C
C Close Fortran unit attached to the output file (just in case the file
C is still open)
C
  140 CLOSE (RDWRTU,ERR=150)
C
C General routine exit
C
  150 RETURN
C
C Copyright (C) 1988,1989,1990:  Synoptics Ltd,  All Rights Reserved
C
      END
C Semper 6 subsidiary module NPSFL0
C
      LOGICAL FUNCTION NPSFL0(STRING,IOS)
      CHARACTER*(*) STRING
      INTEGER IOS
C
      INCLUDE 'COMMON'
C
   10 FORMAT(A)
      NPSFL0 = .TRUE.
      WRITE(RDWRTU,10,IOSTAT=IOS,ERR=20) STRING
      NPSFL0 = .FALSE.
   20 RETURN
C
C Copyright (C) 1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C Semper 6 subsidiary module NPSFL1
C
      LOGICAL FUNCTION NPSFL1(PATHNM,DFEXT)
      CHARACTER*(*) PATHNM,DFEXT
C
      INTEGER NBLANK
      LOGICAL CONOPT,FILMAK,FILSEA,OPT,SEMIOE,SEMKTX
C
      INCLUDE 'COMMON'
C
      INTEGER NF,IOS
C
      INTEGER A1FILE(FILMAX)
      LOGICAL EXISTS,LNEW,LOLD
      CHARACTER*(FILMAX) FILE,FILENM
      CHARACTER*3 STAT
C
      EQUIVALENCE (A1FILE,RB1)
C
C Packed names
C
      INTEGER NNAME, NNEW, NOLD
      PARAMETER (NNAME=22453, NNEW=22623, NOLD=24484)
C
      NPSFL1 = .TRUE.
      IF (OPT(NNEW)) THEN
         IF (CONOPT(NOLD,NNEW)) GOTO 10
         LNEW = .TRUE.
      ELSE
         LNEW = .FALSE.
         LOLD = OPT(NOLD)
      ENDIF
C
C Fetch file name from key NAME, prompting if key is absent
C
      NF = FILMAX - 3
      IF (SEMKTX(NNAME,'File name (as textstring): ',A1FILE,NF,.FALSE.))
     +   GOTO 10
      IF (NF .EQ. 0) GOTO 10
C
C Copy file name into CHARACTER variable
C
      CALL SEMCHS(FILE,A1FILE,NF)
C
C See if file exists
C
      IF (FILMAK(FILE,DFEXT,FILENM)) GOTO 10
      FILE = FILENM
      IF (FILSEA(FILE,DFEXT,FILENM,EXISTS)) GOTO 10
C
C.. clearly VAX-specific code here..
C      IF (EXISTS) THEN
C         FILE = FILENM
C         IF (.NOT.LOLD) EXISTS = INDEX(FILE,';') .NE. 0
C      ENDIF
      NF = NBLANK(FILE)
C
C If file already exists, delete it if NEW given
C
      STAT = 'NEW'
      IF (EXISTS) THEN
         IF (LNEW) THEN
            OPEN (RDWRTU,FILE=FILE,STATUS='OLD',ERR=20,IOSTAT=IOS)
            CLOSE (RDWRTU,STATUS='DELETE',ERR=20,IOSTAT=IOS)
         ELSE IF (LOLD) THEN
            STAT = 'OLD'
         ELSE
            IDMESS = FILE
            ERROR = 135
            GOTO 10
         ENDIF
      ENDIF
C
C Try to open the file dynamically
C
      OPEN (RDWRTU,FILE=FILE,STATUS=STAT,FORM='FORMATTED',ERR=20,
     +      RECL=LNBUF/LNREAL,
     +      IOSTAT=IOS)
C
C Rewind the file to make sure it is positioned at the beginning
C ****** CHANGE ******
C
      REWIND (RDWRTU,ERR=20,IOSTAT=IOS)
C
C ****** ****** ******
C
      NPSFL1 = .FALSE.
      PATHNM = FILE
C
   10 RETURN
C
C INQUIRE, OPEN, CLOSE, REWIND and WRITE errors
C
   20 IF (SEMIOE(IOS,RDWRTU,FILE(1:NF))) GOTO 30
C
C Close Fortran unit attached to the output file (just in case the file
C is still open)
C
   30 CLOSE (RDWRTU,ERR=10)
      GOTO 10
C
C Copyright (C) 1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C Semper 6 subsidiary module NPSFL2
C
      SUBROUTINE NPSFL2(INSTR,OUTSTR,N)
C
C Returns the input string in the output string with all ocurrences of
C "(", ")" or "\" replaced by the valid Postscript escape sequence "\(",
C "\)" or "\\".  Any non-printing character codes are replaced by the
C escape sequence "\277" (upside-down question mark).  If this process
C exceeds the length of the output string, the excess characters are
C discarded.  The significant (non-blank) length of the output string
C is returned in N.
C
      CHARACTER*(*) INSTR,OUTSTR
      INTEGER N
C
      INCLUDE 'ICSET'
C
      CHARACTER CHA,ESCAPE
      CHARACTER*4 EXTRA
      CHARACTER*(KTILDE-KSPACE+1) VALID
      INTEGER I,J,LNOUTS
C
C Initialise return character count
      N=0
C
C Initialise string length
      LNOUTS=LEN(OUTSTR)
C
C Set up character variable containing escape character "\"
      ESCAPE=CHAR(KBACKS)
C
C Set up string containing all valid printing characters
      J=1
      DO 10 I=KSPACE,KTILDE
         VALID(J:J)=CHAR(I)
         J=J+1
   10 CONTINUE
C
C Initialise input character pointer
      I=0
C
C Extract next character from string
   20 I=I+1
      CHA=INSTR(I:I)
C
C Determine appropriate conversion for character
      IF (INDEX(VALID,CHA).EQ.0) THEN
         EXTRA(1:1)=ESCAPE
         EXTRA(2:4)='277'
         J=4
      ELSE
         IF (CHA.EQ.'('.OR.CHA.EQ.')'.OR.CHA.EQ.ESCAPE) THEN
            EXTRA(1:1)=ESCAPE
            EXTRA(2:2)=CHA
            J=2
         ELSE
            EXTRA=CHA
            J=1
         ENDIF
      ENDIF
C
C If not enough space in output string, return
      IF (N+J.LE.LNOUTS) THEN
C
C Store output characters
         OUTSTR(N+1:N+J)=EXTRA
         N=N+J
C
C If end of string not yet reached, go back for more
         IF (I.LT.LEN(INSTR)) GOTO 20
      ENDIF
C
      RETURN
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C Sub-processing module NPSFL3
C
      LOGICAL FUNCTION NPSFL3(MODE,IOS,LCOLOR,LENCAP,LDISP,LBORDE,
     +                                 LORIGI,LCAPTN,LHEADE)
      INTEGER MODE,IOS
      LOGICAL LCOLOR,LENCAP,LDISP,LBORDE,LORIGI,LCAPTN,LHEADE
C
      LOGICAL NPSFL5,NPSFL6
C
      INCLUDE 'COMMON'
C
C Write out procedure definition to process image
C
   10 FORMAT(A)
      WRITE (RDWRTU,10,ERR=30,IOSTAT=IOS)
     +   ' ',
     +   '%%BeginProcSet: show_image 1.0 0',
     +   '/show_image {',
     +   ' gsave',
     +   ' image_matrix',
     +   ' icol irow 8 [icol 0 0 irow neg 0 irow]'
      IF (LCOLOR) THEN
         WRITE (RDWRTU,10,ERR=30,IOSTAT=IOS)
     +   ' {currentfile pixels readline pop} false 3 colorimage'
      ELSE
         WRITE (RDWRTU,10,ERR=30,IOSTAT=IOS)
     +   ' {currentfile pixels readline pop} image'
      ENDIF
      WRITE (RDWRTU,10,ERR=30,IOSTAT=IOS)
     +   ' grestore} def',
     +   '%%EndProcSet: show_image 1.0 0'
C
C Write output remaining procedure definitions according to whether
C ENCAPSULATED option is set or not
C
      IF (LENCAP) THEN
         WRITE (RDWRTU,10,ERR=30,IOSTAT=IOS)
     +      ' ',
     +      '%%BeginProcSet: image_matrix 1.0 0',
     +      '/image_matrix {icol irow scale} def',
     +      '%%EndProcSet: image_matrix 1.0 0'
      ELSE
         IF (NPSFL5(IOS,MODE,LDISP,LHEADE)) GOTO 30
         IF (NPSFL6(IOS,LBORDE,LORIGI,LCAPTN)) GOTO 30
      ENDIF
C
C Write out terminating comment for prolog
C
      WRITE (RDWRTU,10,ERR=30,IOSTAT=IOS)
     +   '%%EndProlog'
C
C Write out setting up commands
C
      WRITE (RDWRTU,10,ERR=30,IOSTAT=IOS)
     +   ' ',
     +   '%%BeginSetup',
     +   'currenttransfer',
     +'{dummy exec 0.129412 sub 2.74194 mul} dup 0 currenttransfer put',
     +   'settransfer'
C
      IF (.NOT.LENCAP) THEN
         WRITE (RDWRTU,10,ERR=30,IOSTAT=IOS)
     +      'page_limits'
      ENDIF
C
      WRITE (RDWRTU,10,ERR=30,IOSTAT=IOS)
     +   '%%EndSetup'
C
      NPSFL3 = .FALSE.
   20 RETURN
C
   30 NPSFL3 = .TRUE.
      GOTO 20
C
C Copyright (C) 1988,1989:  Synoptics Ltd., All Rights Reserved
C
      END
C Sub-processing module NPSFL4
C
      LOGICAL FUNCTION NPSFL4(IOS,NCOL,NROW,NLAY,CLASS,FORM,CCOL,CROW,
     +                        LDISP,LCOMPL,LBORDE,LORIGI,LCAPTN,LHEADE)
      INTEGER IOS,NCOL,NROW,NLAY,CLASS,FORM,CCOL,CROW
      LOGICAL LDISP,LCOMPL,LBORDE,LORIGI,LCAPTN,LHEADE
C
      LOGICAL SEMXA1,SEMLAB,SEMKTX,OPTNO,OPT,CONOPT
      INTEGER IVAL
      REAL VAL
      CHARACTER*11 DATSTR
      CHARACTER*8  TIMSTR
      INTEGER LABEL(256),DATIME(7),ITEXT(13)
      INTEGER CLEN(10),FLEN(0:3)
      INTEGER I,N,N1,N2,N3,ICOL1,ICOL2,IROW1,IROW2
      INTEGER IR1LEN,IR2LEN,NCOPY
      REAL TIMES
      CHARACTER*6 CLANDS,CCOMPL,CABOVE
      CHARACTER*11 CSTR(10)
      CHARACTER*7  FSTR(0:3)
      CHARACTER*13 RA1STR,RA2STR
      CHARACTER*11 CRDSTR,CUDSTR
      CHARACTER*8 CRTSTR,CUTSTR
      CHARACTER*156 TITLE
      CHARACTER*128 STRING
C
      INTEGER TEXMAX
      PARAMETER (TEXMAX=150)
C
      INTEGER A1TEXT(TEXMAX)
      CHARACTER*(TEXMAX) TEXT
C
      INCLUDE 'COMMON'
C
      EQUIVALENCE (ICOL1,SMGI1),(ICOL2,SMGI4)
      EQUIVALENCE (IROW1,SMGI2),(IROW2,SMGI5)
      EQUIVALENCE (A1TEXT,LABEL,DATIME,ITEXT,RB1)
C
C Packed names
C
      INTEGER NCOPIE,NTIMES,NPORTR,NLANDS,NBORDE,NORIGI,NTEXT,NABOVE
      INTEGER NBELOW,NHEADE
      PARAMETER (NCOPIE=5416, NTIMES=-374)
      PARAMETER (NPORTR=26218, NLANDS=19254, NBORDE=3818, NORIGI=24729)
      PARAMETER (NTEXT=-225, NABOVE=1695, NBELOW=3412, NHEADE=13001)
C
      DATA CSTR /'Image      ',
     +           'Macro      ',
     +           'Fourier    ',
     +           'Spectrum   ',
     +           'Correlation',
     +           'Undefined  ',
     +           'Walsh      ',
     +           'Plist      ',
     +           'Histogram  ',
     +           'Lut        '/
      DATA CLEN /5,5,7,8,11,9,5,5,9,3/
C
      DATA FSTR /'Byte   ',
     +           'Integer',
     +           'Fp  ',
     +           'Complex'/
      DATA FLEN /4,7,2,7/
C
C Format statements
C
   10 FORMAT (A)
   20 FORMAT (A,A,A)
   30 FORMAT (A,I8,A)
   40 FORMAT (A,F10.5,A)
C
C Determine number of copies to output
C
      NCOPY = IVAL(NCOPIE)
C
C Fault zero or negative number
C
      IF (NCOPY.LE.0) THEN
         ERROR=3
         IDERR=NCOPIE
         GOTO 50
      ENDIF
C
C Fetch value for TIMES key
C
      TIMES = VAL(NTIMES)
C
C Fault zero or negative value
C
      IF (TIMES.LE.0.) THEN
         ERROR=3
         IDERR=NTIMES
         GOTO 50
      ENDIF
C
C Fault conflicting options PROTRAIT and LANDSCAPE
C
      IF (CONOPT(NPORTR,NLANDS)) GOTO 50
C
C See if option LANDSCAPE is set (default option is PORTRAIT)
C
      IF (OPT(NLANDS)) THEN
         CLANDS=' true'
      ELSE
         CLANDS=' false'
      ENDIF
C
C See if option BORDER is set (default option is BORDER)
C
      LBORDE=.NOT.OPTNO(NBORDE)
C
C See if option ORIGIN is set (default option is NOORIGIN)
C
      LORIGI=OPT(NORIGI)
C
C If either option set ...
C
      IF (LBORDE.OR.LORIGI) THEN
C
C ... see if outputting complex picture
C
         IF (LCOMPL) THEN
            CCOMPL=' true'
         ELSE
            CCOMPL=' false'
         ENDIF
      ENDIF
C
C See if TEXT key is set
C
      N=TEXMAX
      IF (SEMKTX(NTEXT,' ',A1TEXT,N,.FALSE.)) GOTO 50
      LCAPTN=N.NE.0
      IF (LCAPTN) THEN
C
C Fetch text string from key TEXT
C
         CALL SEMCHS(TEXT,A1TEXT,N)
         N1=MAX(1,N/3)
         N2=N1
         N3=MAX(1,N-N1-N2)
C
C Fault conflicting options ABOVE and BELOW
C
         IF (CONOPT(NABOVE,NBELOW)) GOTO 50
C
C See if option ABOVE is set (default option is BELOW)
C
         IF (OPT(NABOVE)) THEN
            CABOVE=' true'
         ELSE
            CABOVE=' false'
         ENDIF
      ENDIF
C
C See if option HEADER is set (default option is HEADER) - if display
C output, force NOHEADER
C
      LHEADE=.NOT.OPTNO(NHEADE)
      IF (LHEADE) THEN
C
C See if not dumping from screen
C
         IF (.NOT.LDISP) THEN
C
C Set up range strings
C
            IR1LEN=1
            IF (SEMXA1(4,ITEXT,13,IR1LEN,VMIN,I)) THEN
               IR1LEN=3
               RA1STR='***'
            ELSE
               IR1LEN=IR1LEN-1
               CALL SEMCHS(RA1STR,ITEXT,IR1LEN)
            ENDIF
C
            IR2LEN=1
            IF (SEMXA1(4,ITEXT,13,IR2LEN,VMAX,I)) THEN
               IR2LEN=3
               RA2STR='***'
            ELSE
               IR2LEN=IR2LEN-1
               CALL SEMCHS(RA2STR,ITEXT,IR2LEN)
            ENDIF
C
C Fetch picture label
C
            IF (SEMLAB(1,LABEL,LP1)) GOTO 50
C
C Extract creation date and time string from the picture label
C
            CRDSTR=DATSTR(LABEL(LBDAY),LABEL(LBMON),LABEL(LBYEAR)+1900)
            CRTSTR=TIMSTR(LABEL(LBHOUR),LABEL(LBMIN),LABEL(LBSEC))
C
C Extract title string from the picture label
C
            TITLE=' '
            N=LABEL(LBNCTT)
            IF (N.GT.0) CALL SEMCHS(TITLE,LABEL(LBTT1),N)
         ENDIF
C
C Set up current date and time string
C
         CALL MCTIME(DATIME)
         CUDSTR=DATSTR(DATIME(3),DATIME(2),DATIME(1))
         CUTSTR=TIMSTR(DATIME(4),DATIME(5),DATIME(6))
      ENDIF
C
C Write out this information
C
      WRITE (RDWRTU,10,ERR=70,IOSTAT=IOS) ' '
      WRITE (RDWRTU,30,ERR=70,IOSTAT=IOS)
     +   '/#copies',NCOPY,' def'
      WRITE (RDWRTU,10,ERR=70,IOSTAT=IOS) ' '
      WRITE (RDWRTU,40,ERR=70,IOSTAT=IOS)
     +   '/times',TIMES,' def'
      WRITE (RDWRTU,20,ERR=70,IOSTAT=IOS)
     +   '/landscape ',CLANDS,' def'
C
      IF (LBORDE.OR.LORIGI) THEN
         WRITE (RDWRTU,20,ERR=70,IOSTAT=IOS)
     +      '/complex ',CCOMPL,' def'
      ENDIF
C
      IF (LORIGI.OR.LHEADE) THEN
         WRITE (RDWRTU,30,ERR=70,IOSTAT=IOS)
     +      '/icol1',ICOL1,' def',
     +      '/icol2',ICOL2,' def',
     +      '/irow1',IROW1,' def',
     +      '/irow2',IROW2,' def'
      ENDIF
C
      IF (LORIGI) THEN
         WRITE (RDWRTU,30,ERR=70,IOSTAT=IOS)
     +      '/ccol',CCOL,' def',
     +      '/crow',CROW,' def'
      ENDIF
C
      IF (LCAPTN) THEN
         CALL NPSFL2(TEXT(1:N1),STRING,N)
         WRITE (RDWRTU,20,ERR=70,IOSTAT=IOS)
     +      '/caption1string (',STRING(1:N),') def'
C
         CALL NPSFL2(TEXT(N1+1:N1+N2),STRING,N)
         WRITE (RDWRTU,20,ERR=70,IOSTAT=IOS)
     +      '/caption2string (',STRING(1:N),') def'
C
         CALL NPSFL2(TEXT(N1+N2+1:N1+N2+N3),STRING,N)
         WRITE (RDWRTU,20,ERR=70,IOSTAT=IOS)
     +      '/caption3string (',STRING(1:N),') def'
C
         WRITE (RDWRTU,20,ERR=70,IOSTAT=IOS)
     +      '/above ',CABOVE,' def'
      ENDIF
C
      IF (LHEADE) THEN
         WRITE (RDWRTU,20,ERR=70,IOSTAT=IOS)
     +      '/currentdatestring (',CUDSTR,') def',
     +      '/currenttimestring (',CUTSTR,') def'
         WRITE (RDWRTU,30,ERR=70,IOSTAT=IOS)
     +      '/ncol',NCOL,' def',
     +      '/nrow',NROW,' def'
C
         IF (.NOT.LDISP) THEN
            WRITE (RDWRTU,30,ERR=70,IOSTAT=IOS)
     +         '/nlay',NLAY,' def'
            WRITE (RDWRTU,20,ERR=70,IOSTAT=IOS)
     +         '/classstring (',CSTR(CLASS)(1:CLEN(CLASS)),') def',
     +         '/formstring (',FSTR(FORM)(1:FLEN(FORM)),') def',
     +         '/range1string (',RA1STR(1:IR1LEN),') def',
     +         '/range2string (',RA2STR(1:IR2LEN),') def',
     +         '/createdatestring (',CRDSTR,') def',
     +         '/createtimestring (',CRTSTR,') def'
C
            CALL NPSFL2(TITLE(1:52),STRING,N)
            WRITE (RDWRTU,20,ERR=70,IOSTAT=IOS)
     +         '/title1string (',STRING(1:N),') def'
C
            CALL NPSFL2(TITLE(53:104),STRING,N)
            WRITE (RDWRTU,20,ERR=70,IOSTAT=IOS)
     +         '/title2string (',STRING(1:N),') def'
C
            CALL NPSFL2(TITLE(105:156),STRING,N)
            WRITE (RDWRTU,20,ERR=70,IOSTAT=IOS)
     +         '/title3string (',STRING(1:N),') def'
         ENDIF
      ENDIF
C
   50 NPSFL4 = .FALSE.
   60 RETURN
C
   70 NPSFL4 = .TRUE.
      GOTO 60
C
C Copyright (C) 1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C Sub-processing module NPSFL5
C
      LOGICAL FUNCTION NPSFL5(IOS,MODE,LDISP,LHEADE)
      INTEGER IOS,MODE
      LOGICAL LDISP,LHEADE
C
      INCLUDE 'COMMON'
C
C Write out procedure definition to process image
C
   10 FORMAT(A)
      IF (LHEADE) THEN
         WRITE (RDWRTU,10,ERR=30,IOSTAT=IOS)
     +         ' ',
     +         '%%BeginProcSet: show_header 1.0 0',
     +         '/show_header {',
     +         ' gsave',
     +         ' /Helvetica findfont 7 scalefont setfont',
     +         ' pageleft 5 add pagetop 10 sub xyround moveto'
C
         IF (LDISP) THEN
            IF (MODE.EQ.1) THEN
               WRITE (RDWRTU,10,ERR=30,IOSTAT=IOS)
     +            ' (Frame size:   ) show'
            ELSE IF (MODE.EQ.2) THEN
               WRITE (RDWRTU,10,ERR=30,IOSTAT=IOS)
     +            ' (Partition size:   ) show'
            ELSE IF (MODE.EQ.3) THEN
               WRITE (RDWRTU,10,ERR=30,IOSTAT=IOS)
     +            ' (Picture size:   ) show'
            ENDIF
C
            WRITE (RDWRTU,10,ERR=30,IOSTAT=IOS)
     +            ' ncol 8 string cvs show (,) show',
     +            ' nrow 8 string cvs show',
     +            ' (          Sub-region size:   ) show',
     +            ' icol2 icol1 sub 1 add 8 string cvs show (,) show',
     +            ' irow2 irow1 sub 1 add 8 string cvs show',
     +            ' (          Frame:   ) show 8 string cvs show',
     +            ' (          Scaling factor:   ) show',
     +            ' times 10 string cvs show',
     +            ' (          Current date:   ) show',
     +            ' currentdatestring show',
     +            ' (          Current time:   ) show',
     +            ' currenttimestring show',
     +            ' pageright 1 sub pagetop 14 sub xyround moveto',
     +            ' pageleft  1 add pagetop 14 sub xyround lineto'
         ELSE
            WRITE (RDWRTU,10,ERR=30,IOSTAT=IOS)
     +            ' (Picture size:   ) show',
     +            ' ncol 8 string cvs show (,) show',
     +            ' nrow 8 string cvs show (,) show',
     +            ' nlay 8 string cvs show',
     +            ' (          Class:   ) show classstring show',
     +            ' (          Form:   ) show formstring show',
     +            ' (          Creation date:   ) show',
     +            ' createdatestring show',
     +            ' (          Creation time:   ) show',
     +            ' createtimestring show',
     +            ' pageleft 5 add pagetop 22 sub moveto',
     +            ' (Title:   )  show',
     +            ' title1string show',
     +            ' title2string show',
     +            ' title3string show'
            WRITE (RDWRTU,10,ERR=30,IOSTAT=IOS)
     +            ' pageleft 5 add pagetop 34 sub moveto',
     +            ' (Sub-region size:   ) show',
     +            ' icol2 icol1 sub 1 add 8 string cvs show (,) show',
     +            ' irow2 irow1 sub 1 add 8 string cvs show',
     +            ' (          Layer:   ) show 8 string cvs show',
     +            ' (          Scaling factor:   ) show',
     +            ' times 10 string cvs show',
     +            ' (          Range:   ) show',
     +            ' range1string show (,) show',
     +            ' range2string show',
     +            ' (          Current date:   ) show',
     +            ' currentdatestring show',
     +            ' (          Current time:   ) show',
     +            ' currenttimestring show',
     +            ' pageright 1 sub pagetop 38 sub xyround moveto',
     +            ' pageleft  1 add pagetop 38 sub xyround lineto'
         ENDIF
C
         WRITE (RDWRTU,10,ERR=30,IOSTAT=IOS)
     +         ' pageleft  1 add pagetop  1 sub xyround lineto',
     +         ' pageright 1 sub pagetop  1 sub xyround lineto',
     +         ' closepath',
     +         ' 0.1 setlinewidth 2 setlinecap stroke',
     +         ' grestore} def',
     +         '%%EndProcSet: show_header 1.0 0'
      ENDIF
C
      NPSFL5 = .FALSE.
   20 RETURN
C
   30 NPSFL5 = .TRUE.
      GOTO 20
C
C Copyright (C) 1988,1989:  Synoptics Ltd., All Rights Reserved
C
      END
C Sub-processing module NPSFL6
C
      LOGICAL FUNCTION NPSFL6(IOS,LBORDE,LORIGI,LCAPTN)
      INTEGER IOS
      LOGICAL LBORDE,LORIGI,LCAPTN
C
      INCLUDE 'COMMON'
C
C Write out procedure definition to process image
C
   10 FORMAT(A)
      WRITE (RDWRTU,10,ERR=30,IOSTAT=IOS)
     +      ' ',
     +      '/trnd {transform round exch round exch} def',
     +      '/xyround {trnd itransform} def'
C
      WRITE (RDWRTU,10,ERR=30,IOSTAT=IOS)
     +      ' ',
     +      '%%BeginProcSet: page_limits 1.0 0',
     +      '/page_limits {',
     +      ' gsave',
     +      ' clippath pathbbox',
     +      ' /pagetop    exch def',
     +      ' /pageright  exch def',
     +      ' /pagebottom exch def',
     +      ' /pageleft   exch def',
     +      ' grestore} def',
     +      '%%EndProcSet: page_limits 1.0 0'
C
      WRITE (RDWRTU,10,ERR=30,IOSTAT=IOS)
     +      ' ',
     +      '%%BeginProcSet: image_matrix 1.0 0',
     +      '/image_matrix {',
     +      ' /current_matrix matrix currentmatrix def',
     +      ' pageleft pageright add 2 div',
     +      ' pagebottom pagetop add 2 div',
     +      ' xyround translate',
     +      ' landscape {90 rotate} if',
     +      ' icol times mul irow times mul scale',
     +      ' -0.5 -0.5 xyround translate} def',
     +      '%%EndProcSet: image_matrix 1.0 0'
C
      IF (LBORDE) THEN
         WRITE (RDWRTU,10,ERR=30,IOSTAT=IOS)
     +         ' ',
     +         '%%BeginProcSet: show_border 1.0 0',
     +         '/show_border {',
     +         ' gsave',
     +         ' image_matrix',
     +         ' complex {0.5 0 trnd 0.5 1 trnd} if',
     +         ' 1 0 trnd 1 1 trnd 0 1 trnd 0 0 trnd',
     +         ' current_matrix setmatrix landscape {90 rotate} if',
     +         ' itransform moveto itransform lineto',
     +         ' itransform lineto itransform lineto closepath',
     +         ' complex {itransform moveto itransform lineto} if',
     +         ' 0.1 setlinewidth 2 setlinecap stroke',
     +         ' grestore} def',
     +         '%%EndProcSet: show_border 1.0 0'
      ENDIF
C
      IF (LORIGI) THEN
         WRITE (RDWRTU,10,ERR=30,IOSTAT=IOS)
     +         ' ',
     +         '%%BeginProcSet: show_origin 1.0 0',
     +         '/show_origin {',
     +         ' gsave',
     +         ' image_matrix',
     +         ' /xcen ccol icol1 sub 2 mul 1 add 2 div icol div def',
     +         ' /ycen irow2 crow sub 2 mul 1 add 2 div irow div def',
     +         ' /imcen xcen 0.5 add def',
     +         ' /xcenin xcen 0 lt xcen 1 gt or not def',
     +         ' /ycenin ycen 0 lt ycen 1 gt or not def',
     +         ' /imcenin complex {imcen 0 lt imcen 1 gt or not}',
     +         '                  {false} ifelse def',
     +         ' xcenin  {xcen 0 trnd xcen 1 trnd} if',
     +         ' ycenin  {0 ycen trnd 1 ycen trnd} if',
     +         ' imcenin {imcen 0 trnd imcen 1 trnd} if'
         WRITE (RDWRTU,10,ERR=30,IOSTAT=IOS)
     +         ' current_matrix setmatrix landscape {90 rotate} if',
     +         ' imcenin {itransform moveto  0  4 rlineto',
     +         '          itransform moveto  0 -4 rlineto} if',
     +         ' ycenin  {itransform moveto  4  0 rlineto',
     +         '          itransform moveto -4  0 rlineto} if',
     +         ' xcenin  {itransform moveto  0  4 rlineto',
     +         '          itransform moveto  0 -4 rlineto} if',
     +         ' 0.1 setlinewidth 2 setlinecap stroke',
     +         ' grestore} def',
     +         '%%EndProcSet: show_origin 1.0 0'
      ENDIF
C
      IF (LCAPTN) THEN
         WRITE (RDWRTU,10,ERR=30,IOSTAT=IOS)
     +         ' ',
     +         '%%BeginProcSet: show_caption 1.0 0',
     +         '/show_caption {',
     +         ' gsave',
     +         ' image_matrix',
     +         ' 0.5 above {1} {0} ifelse trnd',
     +         ' current_matrix setmatrix landscape {90 rotate} if',
     +         ' itransform moveto',
     +         ' 0 above {12} {-20} ifelse rmoveto',
     +         ' /Helvetica findfont 10 scalefont setfont',
     +         ' caption1string stringwidth pop',
     +         ' caption2string stringwidth pop add',
     +         ' caption3string stringwidth pop add',
     +         ' 2 div neg 0 rmoveto',
     +         ' caption1string show',
     +         ' caption2string show',
     +         ' caption3string show',
     +         ' grestore} def',
     +         '%%EndProcSet: show_caption 1.0 0'
      ENDIF
C
      NPSFL6 = .FALSE.
   20 RETURN
C
   30 NPSFL6 = .TRUE.
      GOTO 20
C
C Copyright (C) 1988,1989:  Synoptics Ltd., All Rights Reserved
C
      END
