C Semper 6 processing module PSOUT
C
      SUBROUTINE PSOUT
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
C DISPLAY (picture range or MIN,MAX if PRESET option is set).  A screen
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
C  Postscript :PSOUT $1= frame partition picture encapsulated name=' +
C    old new copies=1 times=1 portrait landscape preset black white +
C    size= si2= position= po2= left right top bottom layer= la2= +
C    border origin text=' above below header
C
      INTEGER IVALPN
C
      INTEGER LNBLNK
C
C     Change: LDM, July 2005
C     Ensure that gfortran does not think of range as an intrinsic
      EXTERNAL RANGE
      LOGICAL CONOPT,OPT,RANGE,SEMIOE,SEMOPN,SEMROW,VARSET,SEMLAB,ABANDN
      LOGICAL FSRI61,FSOI61,TSTSRG,FSOPTN,FSINIT,FSREGN
      LOGICAL PSOUTB,PSOUT0,PSOUT1,PSOUT3,PSOUT4,PSDEFI
      CHARACTER*11 DATSTR
      CHARACTER*8  TIMSTR
C
      LOGICAL LENCAP,LDISP,LCOMPL,LBORDE,LORIGI,LCAPTN,LHEADE
      INTEGER NCOL,NROW,NLAY,CLASS,FORM,CCOL,CROW
      INTEGER MODE,NDIS,IMOFF,DATIME(7),NLF
      INTEGER I,I1,I2,J,J1,J2,K,K1,K2,M,N,ICOL,IROW,ILAY
      INTEGER ICOL1,ICOL2,IOSIOE,IROW1,IROW2,ILAY1,ILAY2
      INTEGER NF,NPIX,FRAME,X,NPIC,ANNOT
      REAL A,B,P,XMIN,XMAX,YMIN,YMAX
      INTEGER IOS
C
      INTEGER NPIXEL
      PARAMETER (NPIXEL=72)
C
      CHARACTER*(NPIXEL) PIXELS
      CHARACTER*4 DFEXT
C
      INCLUDE 'COMMON'
C
      CHARACTER*(FILMAX) FILE
      CHARACTER*(RECLEN) MYTEXT
C
      COMMON /PSOUTC/ IOS
C
      INTEGER IB1(LNBUF/LNINT),LABEL(LNLAB),IB2(LNBUF/LNINT)
      EQUIVALENCE (IB1,LABEL,RB1),(IB2,RB2)
C
      EQUIVALENCE (ICOL1,SMGI1),(ICOL2,SMGI4)
      EQUIVALENCE (IROW1,SMGI2),(IROW2,SMGI5)
      EQUIVALENCE (ILAY1,SMGI3),(ILAY2,SMGI6)
C
C Packed names
C
      INTEGER NDLLR1,NENCAP,NBLACK,NWHITE,NFRAME
      INTEGER NPARTI,NPICTU
      PARAMETER (NDLLR1=-12441, NENCAP=8563, NBLACK=3681, NWHITE=-5130)
      PARAMETER (NFRAME=10321, NPARTI=25658, NPICTU=25963)
C
C Output image intensity scale
C
      REAL BLACK,WHITE
      PARAMETER (BLACK=33.0, WHITE=126.0)
C
C Format statements
C
   10 FORMAT (A,2I8)
   20 FORMAT (A,I8,A)
   30 FORMAT (I8,A)
C
C See if dumping display image
C
      LDISP=OPT(NFRAME).OR.OPT(NPARTI).OR.OPT(NPICTU)
      IF (LDISP) THEN
C
C Determine whether frame, partition or picture coordinates are required
C
         IF (FSOPTN(MODE,NDIS)) GOTO 140
C
C Initialise framestore graphics accordingly
C
         IF (FSINIT(MODE,NDIS)) GOTO 140
C
C Set up frame/partition/picture size
C
         NCOL=1+NINT(FSXSCA*(FSBRIG-FSBLEF))
         NROW=1+NINT(FSYSCA*(FSBBOT-FSBTOP))
C
C Set up frame number for display access
C
         FRAME=FSFRA
C
C Determine region of interest, depending on standard 2D sub-region
C keys and options specified
C
         IF (FSREGN(XMIN,XMAX,YMIN,YMAX)) GOTO 140
C
C Set up equivalent sub-region limits in terms of display coordinates
C
         ICOL1=NINT(FSXSCA*XMIN+FSXOFF)
         ICOL2=NINT(FSXSCA*XMAX+FSXOFF)
         IROW1=NINT(FSYSCA*YMAX+FSYOFF)
         IROW2=NINT(FSYSCA*YMIN+FSYOFF)
         ILAY1=1
         ILAY2=1
C
C Set up X start position for sub-region, taking into account complex
C display picture and use of options RE and IM
C
         X=ICOL1
         IF (FSI1.EQ.2) X=X+NINT(FSXSCA*FSIOFF)
C
C See if outputting both parts of complex display picture
C
         LCOMPL=FSI2.NE.FSI1
C
C Set up offset between real and imaginary part of complex display
C picture (if any)
C
         IMOFF=NINT(FSXSCA*FSIOFF)
C
C Set up position of origin in display coordinates ((0,0) in graphics
C coordinates)
C
         CCOL=NINT(FSXOFF)
         CROW=NINT(FSYOFF)
C
C Fault conflict between options BLACK and WHITE
C
         IF (CONOPT(NBLACK,NWHITE)) GOTO 140
C
C Set up output intensity for any display annotation (default is WHITE)
C
         IF (OPT(NBLACK)) THEN
            ANNOT=BLACK
         ELSE
            ANNOT=WHITE
         ENDIF
C
C Otherwise, set up specified source picture
C
      ELSE
C
C Determine source picture number (default = SELECT)
C
         IF (VARSET(NDLLR1)) THEN
            NPIC=IVALPN(NDLLR1)
         ELSE
            NPIC=NINT(SELECT)
         ENDIF
C
C Open source picture
C
         IF (SEMOPN(1,NPIC,NCOL,NROW,NLAY,CLASS,FORM,LP1)) GOTO 140
C
C Fault picture class that does not entail a 2-D image
C
         IF (CLASS.EQ.NCLMAC.OR.CLASS.EQ.NCLPLI.OR.
     +       CLASS.EQ.NCLHIS.OR.CLASS.EQ.NCLLUT) THEN
            ERROR=6
            IDERR=NPIC
            GOTO 140
         ENDIF
C
C See if complex source picture
C
         LCOMPL=FORM.EQ.NFMCOM
C
C Fetch position for source picture origin
C
         CCOL=CCOLN(LP1)
         CROW=CROWN(LP1)
C
C Establish region of picture to output (including range of layers)
C
         IF (TSTSRG(1,LP1)) GOTO 140
C
C Establish intensity range for region of picture
C
         IF (RANGE(1,LP1)) GOTO 140
C
C Fault zero intensity range
C
         IF (VMIN.EQ.VMAX) THEN
            ERROR=12
            IDERR=NPIC
            GOTO 140
         ENDIF
C
C Set up coefficients to transform intensity range
C
         A=REAL(WHITE-BLACK)/(VMAX-VMIN)
         B=BLACK-A*VMIN
      ENDIF
C
C Determine size of output image and number of output pages
C
      ICOL=ICOL2-ICOL1+1
      IF (LCOMPL) ICOL=2*ICOL
      IROW=IROW2-IROW1+1
      ILAY=ILAY2-ILAY1+1
C
C See if option ENCAPSULATED is set
C
      LENCAP=OPT(NENCAP)
      IF (LENCAP) THEN
         DFEXT = '.eps'
      ELSE
         DFEXT = '.ps'
      ENDIF
C
C Attempt to open output file
C
      IF (PSOUT1(FILE,DFEXT)) GOTO 140
C
C Write header comments
C
      IF (LENCAP) THEN
         IF (PSOUT0('%!PS-Adobe-2.0 EPSF-2.0')) GOTO 120
      ELSE
         IF (PSOUT0('%!PS-Adobe-2.0')) GOTO 120
      ENDIF
C
      M=1
      N=33
      MYTEXT(M:N)='%%Title: Semper 6 Postscript file'
      IF (.NOT.LDISP) THEN
         IF (SEMLAB(1,LABEL,LP1)) GOTO 140
         IF (LABEL(LBNCTT).GT.0) THEN
            N=9+LABEL(LBNCTT)
            CALL SEMCHS(MYTEXT(10:N),LABEL(LBTT1),LABEL(LBNCTT))
         ENDIF
      ENDIF
      IF (PSOUT0(MYTEXT(M:MIN(M+71,N)))) GOTO 120
      M=M+72
   40 IF (M.LE.N) THEN
         MYTEXT(M-3:M-1)='%%+'
         IF (PSOUT0(MYTEXT(M-3:MIN(M+68,N)))) GOTO 120
         M=M+69
         GOTO 40
      ENDIF
C
      IF (PSOUT0('%%Creator: Semper 6 Plus')) GOTO 120
C
      CALL MCTIME(DATIME)
      MYTEXT(1:16)='%%CreationDate: '
      MYTEXT(17:27)=DATSTR(DATIME(3),DATIME(2),DATIME(1))
      MYTEXT(28:28)=' '
      MYTEXT(29:36)=TIMSTR(DATIME(4),DATIME(5),DATIME(6))
      IF (PSOUT0(MYTEXT(1:36))) GOTO 120
C
      IF (LENCAP) THEN
         WRITE (RDWRTU,10,ERR=120,IOSTAT=IOS)
     +      '%%BoundingBox:       0        0',ICOL,IROW
      ELSE
         WRITE (RDWRTU,10,ERR=120,IOSTAT=IOS)
     +      '%%Pages:',ILAY
         IF (PSOUT0('%%DocumentFonts: Helvetica')) GOTO 120
      ENDIF
C
      IF (PSOUT0('%%EndComments')) GOTO 120
C
C Write local dictionary definition
C
      IF (PSOUTB()) GOTO 120
      IF (LENCAP) THEN
         IF (PSOUT0('5 dict begin')) GOTO 120
      ELSE
         IF (PSOUT0('60 dict begin')) GOTO 120
      ENDIF
C
C Write output definitions for image size and input buffer
C
      IF (PSOUTB()) GOTO 120
      IF (PSDEFI('/icol',ICOL)) GOTO 120
      IF (PSDEFI('/irow',IROW)) GOTO 120
      WRITE (RDWRTU,20,ERR=120,IOSTAT=IOS)
     +   '/pixels',NPIXEL,' string def'
C
C If ENCAPSULATED option not set, set up and write information for
C header and annotation
C
      IF (.NOT.LENCAP) THEN
         IF (PSOUT4(NCOL,NROW,NLAY,CLASS,FORM,CCOL,CROW,
     +              LDISP,LCOMPL,LBORDE,LORIGI,LCAPTN,LHEADE)) GOTO 120
         IF (ERROR .NE. 0) GOTO 140
      ENDIF
C
      IF (PSOUT3(MODE,LENCAP,LDISP,LBORDE,LORIGI,LCAPTN,LHEADE))
     +           GOTO 120
C
C If screen dump, offset column start and end by 1, to ensure they are
C both greater than zero (Note: display coordinates start from zero)
C
      IF (LDISP) THEN
         ICOL1=ICOL1+1
         ICOL2=ICOL2+1
      ENDIF
C
C Set up number of columns across region of interest
C
      NCOL=ICOL2-ICOL1+1
C
C Set up column indices
C
      IF (LCOMPL) THEN
         I1=2*ICOL1-1
         I2=2*ICOL2
      ELSE
         I1=ICOL1
         I2=ICOL2
      ENDIF
C
C Set up row indices
C
      J1=IROW1
      J2=IROW2
C
C Set up layer indices
C
      K1=ILAY1
      IF (LENCAP) THEN
         K2=ILAY1
      ELSE
         K2=ILAY2
      ENDIF
C
C Loop over layers
C
      DO 110 K=K1,K2
C
C If not encapsulated, write out page comment
C
         IF (.NOT.LENCAP) THEN
            IF (PSOUTB()) GOTO 120
            IF (LDISP) THEN
               NLF=FRAME
            ELSE
               NLF=K
            ENDIF
            WRITE (RDWRTU,10,ERR=120,IOSTAT=IOS)
     +         '%%Page:',NLF,1+(K-K1)
         ENDIF
C
C Write out command to process image
C
         IF (PSOUTB()) GOTO 120
         IF (PSOUT0('show_image')) GOTO 120
C
C Loop over rows
C
         DO 100 J=J1,J2
C
C If dumping from the display, read and scale data directly from the
C framestore and superimpose any display annotation
C
            IF (LDISP) THEN
C
C Read back image and overlay data
C
               IF (FSRI61(IB1(I1),NCOL,NFMINT,X,J,FRAME,BLACK,WHITE,0,
     +            ERROR)) GOTO 140
               IF (OVLIND(FSDEV)) THEN
                  IF (FSOI61(IB2(I1),NCOL,X,J,FRAME,0,ERROR)) GOTO 140
               ELSE
                  IF (FSOI61(IB2(I1),NCOL,X,J,0,0,ERROR)) GOTO 140
               ENDIF
C
C If complex display picture, read back imaginary part also
C
               IF (LCOMPL) THEN
                  IF (FSRI61(IB1(I1+NCOL),NCOL,NFMINT,X+IMOFF,J,FRAME,
     +               BLACK,WHITE,0,ERROR)) GOTO 140
                  IF (OVLIND(FSDEV)) THEN
                     IF (FSOI61(IB2(I1+NCOL),NCOL,X+IMOFF,J,
     +                          FRAME,0,ERROR)) GOTO 140
                  ELSE
                     IF (FSOI61(IB2(I1+NCOL),NCOL,X+IMOFF,J,0,0,ERROR))
     +                  GOTO 140
                  ENDIF
               ENDIF
C
C Superimpose overlay data on image data
C
               DO 50 I=I1,I2
                  IF (IB2(I).NE.0) IB1(I)=ANNOT
   50          CONTINUE
C
C Check for abandon request
C
               IF (ABANDN(ERROR)) GOTO 140
C
C Otherwise, read data from source picture and scale it
C
            ELSE
C
C Read source row from LP1, splitting real and imaginary parts of a
C complex source picture
C
               IF (LCOMPL) THEN
                  IF (SEMROW(1,RB2,NFMCOM,J,K,LP1)) GOTO 140
                  M=2*ICOL1-1
                  DO 60 I=I1,I1+(NCOL-1)
                     RB1(I)=RB2(M)
                     RB1(I+NCOL)=RB2(M+1)
                     M=M+2
   60             CONTINUE
               ELSE
                  IF (SEMROW(1,RB1,NFMFP,J,K,LP1)) GOTO 140
               ENDIF
C
C Scale the picture data into the range 33 to 126
C
               DO 70 I=I1,I2
C
C Transform the pixel value
C
                  P=A*RB1(I)+B
C
C Convert this value to an integer in the range 33 to 126
C
                  IF (P.LE.BLACK) THEN
                     IB1(I)=33
                  ELSE IF (P.GE.WHITE) THEN
                     IB1(I)=126
                  ELSE
                     IB1(I)=NINT(P)
                  ENDIF
   70          CONTINUE
            ENDIF
C
C Write out row data
C
            DO 90 I=I1,I2,NPIXEL
C
C Determine number of pixels in next output record
C
               NPIX=MIN(I2-I+1,NPIXEL)
C
C Assemble next output record
C
               DO 80 N=1,NPIX
C
C Add pixel value to character buffer
C
                  PIXELS(N:N)=CHAR(IB1(I+N-1))
   80          CONTINUE
C
C Write out contents of character buffer
C
               IF (PSOUT0(PIXELS(1:NPIX))) GOTO 120
   90       CONTINUE
  100    CONTINUE
C
C Write out remaining Postscript commands for this layer (none if
C ENCAPSULATED option is set)
C
         IF (.NOT.LENCAP) THEN
            IF (PSOUTB()) GOTO 120
C
            IF (LBORDE) THEN
               IF (PSOUT0('show_border')) GOTO 120
            ENDIF
C
            IF (LORIGI) THEN
               IF (PSOUT0('show_origin')) GOTO 120
            ENDIF
C
            IF (LCAPTN) THEN
               IF (PSOUT0('show_caption')) GOTO 120
            ENDIF
C
            IF (LHEADE) THEN
               WRITE (RDWRTU,30,ERR=120,IOSTAT=IOS)
     +            NLF,' show_header'
            ENDIF
C
            IF (PSOUT0('showpage')) GOTO 120
         ENDIF
  110 CONTINUE
C
C Write out trailer
C
      IF (PSOUTB()) GOTO 120
      IF (PSOUT0('%%Trailer')) GOTO 120
      IF (PSOUT0('settransfer')) GOTO 120
C
C If ENCAPSULATED option not set, write out command to reset
C #copies to 1
C
      IF (.NOT.LENCAP) THEN
         IF (PSOUTB()) GOTO 120
         IF (PSOUT0('/#copies 1 def')) GOTO 120
      ENDIF
C
C Write out command to remove local dictionary from dictionary stack
C
      IF (PSOUTB()) GOTO 120
      IF (PSOUT0('end')) GOTO 120
      IF (PSOUTB()) GOTO 120
C
C ****** CHANGE ******
C
C Close the output file
C
      CLOSE (RDWRTU,ERR=120,IOSTAT=IOS)
C
C ****** ****** ******
C
      GOTO 140
C
C CLOSE and WRITE errors
C
  120 NF = LNBLNK(FILE)
      IOSIOE = IOS
      IF (SEMIOE(IOSIOE,RDWRTU,FILE(1:NF))) GOTO 130
C
C Close Fortran unit attached to the output file (just in case the file
C is still open)
C
  130 CLOSE (RDWRTU,ERR=140)
C
C General routine exit
C
  140 RETURN
C
C Copyright (C) 1988-1996:  Synoptics Ltd,  All Rights Reserved
C
      END
