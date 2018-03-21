C Semper 6 processing module PEXTR
C
      SUBROUTINE PEXTR
C
C Extracts image of selected particle or source background.  The key ID
C specifies the particle id (0 for source background).  Output pixels
C which do not correspond to a particle/source background are set to
C the value for key VALUE.  Key IMAGE specifies the original source
C picture from which output pixel values are determined.  If key IMAGE
C is not set, the output pixel values are set to 1 for particle/source
C background and 0 otherwise.  Keys TO, SEGMENT and PPLIST specify the
C picture numbers for the output and segmented pictures and the
C particle parameter list.
C
      REAL VAL
      INTEGER IVAL,IVALPN,SEMFRM
      LOGICAL VARSET,SEMOPN,SEMROW
C
      INCLUDE 'COMMON'
C
      REAL VALUE
      INTEGER CLASS,FORM,I,ID,INFORM,IOFF,J,JOFF,LP4,NCOL,NLAY,NPIC,NROW
      LOGICAL LIMAGE,LFOUND
C
      INTEGER IB2(LNBUF/LNINT),IB4(LNBUF/LNINT)
C
      EQUIVALENCE (IB2,RB2),(IB4,RB4)
C
      INTEGER NIDENT,NXMIN,NYMAX,NHFERE,NVFERE
      PARAMETER (NIDENT=3, NXMIN=8, NYMAX=11, NHFERE=12, NVFERE=13)
C
C Packed names
C
      INTEGER NSEGME,NPSEGM,NPLIST,NPPLIS,NIMAGE,NVALUE,NID,NPID,NTO
      PARAMETER (NSEGME=30607, NPSEGM=26365, NPLIST=26089, NPPLIS=26252)
      PARAMETER (NIMAGE=14921, NVALUE=-3253)
      PARAMETER (NID=14560, NPID=25964, NTO=-601)
C
C If key SEGMENT is set to zero, picture number for segmented picture
C is obtained from key PSEGMENT
C
      IF (IVAL(NSEGME).EQ.0) THEN
C
C If key PSEGMENT is set, use it to determine picture number
C
         IF (VARSET(NPSEGM)) THEN
            NPIC=IVALPN(NPSEGM)
C
C Otherwise, fault its being unset
C
         ELSE
            ERROR=25
            IDERR=NPSEGM
            GOTO 60
         ENDIF
C
C Otherwise, use key value to determine picture number
C
      ELSE
         NPIC=IVALPN(NSEGME)
      ENDIF
C
C Open segmented picture
C
      IF (SEMOPN(1,NPIC,NCOL,NROW,NLAY,CLASS,FORM,LP2)) GOTO 60
C
C If key PLIST is set to zero, picture number for particle parameter
C list is obtained from key PPLIST
C
      IF (IVAL(NPLIST).EQ.0) THEN
C
C If key PPLIST is set, use it to determine picture number
C
         IF (VARSET(NPPLIS)) THEN
            NPIC=IVALPN(NPPLIS)
C
C Otherwise, fault its being unset
C
         ELSE
            ERROR=25
            IDERR=NPPLIS
            GOTO 60
         ENDIF
C
C Otherwise, use key value to determine picture number
C
      ELSE
         NPIC=IVALPN(NPLIST)
      ENDIF
C
C Open particle parameter list
C
      IF (SEMOPN(1,NPIC,NCOL,NROW,NLAY,CLASS,FORM,LP3)) GOTO 60
C
C Fault non-plist class for particle parameter list
C
      IF (CLASS.NE.NCLPLI) THEN
         ERROR=6
         IDERR=NPIC
         GOTO 60
      ENDIF
C
C Fault bad size for particle parameter list
C
      IF (NROW.NE.1.OR.NLAY.LT.NVFERE) THEN
         ERROR=5
         IDERR=NPIC
         GOTO 60
      ENDIF
C
C See if key IMAGE is set
C
      LIMAGE=VARSET(NIMAGE)
C
C If so, use floating-point data form for generating output results and
C output data form defaults to same form as source picture
C
      IF (LIMAGE) THEN
         INFORM=NFMFP
C
C Open specified source picture
C
         NPIC=IVALPN(NIMAGE)
         IF (SEMOPN(1,NPIC,NCOL,NROW,NLAY,CLASS,FORM,LP1)) GOTO 60
C
C Fault difference between segmented and source picture sizes
C
         IF (NCOL.NE.NCOLS(LP2).OR.NROW.NE.NROWS(LP2)) THEN
            ERROR=5
            IDERR=NPIC
            GOTO 60
         ENDIF
C
C Key VALUE specifies background value for output results
C
         VALUE=VAL(NVALUE)
C
C Otherwise, use integer form
C
      ELSE
         INFORM=NFMINT
C
C Output data form defaults to byte form
C
         FORM=NFMBYT
C
C LP number is not defined
C
         LP1=0
      ENDIF
C
C Fetch value for key ID
C
      ID=IVAL(NID)
C
C If key ID is set to -1, particle id is obtained from key PID
C
      IF (ID.EQ.-1) THEN
C
C If key PID is set, use it to determine particle id
C
         IF (VARSET(NPID)) THEN
            ID=IVAL(NPID)
C
C Otherwise, fault its being unset
C
         ELSE
            ERROR=25
            IDERR=NPID
            GOTO 60
         ENDIF
      ENDIF
C
C Fault negative value for key ID
C
      IF (ID.LT.0) THEN
         ERROR=3
         IDERR=NID
         GOTO 60
      ENDIF
C
C If key ID is set to zero, source background is copied to the output
C picture and therefore output size is same as segmented picture size
C
      IF (ID.EQ.0) THEN
C
C Set output size = segmented picture size
C
         NCOL=NCOLS(LP2)
         NROW=NROWS(LP2)
C
C Zero offset between source and output pictures
C
         IOFF=0
         JOFF=0
C
C Otherwise, determine size of output picture and its position
C relative to source picture from particle size and limits stored in
C the particle parameter list
C
      ELSE
C
C Read particle id's from particle parameter list
C
         IF (SEMROW(1,RB3,NFMFP,1,NIDENT,LP3)) GOTO 60
C
C Look for specified particle id
C
         DO 10 I=1,NCOLS(LP3)
            IF (ID.EQ.NINT(RB3(I))) GOTO 20
   10    CONTINUE
C
C Fault failure to locate specified particle id
C
         ERROR=155
         GOTO 60
C
C Determine output picture size (particle size + 1 pixel all round)
C
   20    IF (SEMROW(1,RB3,NFMFP,1,NHFERE,LP3)) GOTO 60
         NCOL=NINT(RB3(I))+2
C
         IF (SEMROW(1,RB3,NFMFP,1,NVFERE,LP3)) GOTO 60
         NROW=NINT(RB3(I))+2
C
C Determine row/column offset between source and output pictures
C
         IF (SEMROW(1,RB3,NFMFP,1,NXMIN,LP3)) GOTO 60
         IOFF=CCOLN(LP2)+NINT(RB3(I))-2
C
         IF (SEMROW(1,RB3,NFMFP,1,NYMAX,LP3)) GOTO 60
         JOFF=CROWN(LP2)-NINT(RB3(I))-2
      ENDIF
C
C Open output picture
C
      NPIC=IVALPN(NTO)
      LP4=LP1
      IF (SEMOPN(2,NPIC,NCOL,NROW,1,NCLIMA,SEMFRM(FORM),LP4)) GOTO 60
C
C Initialise particle flag
C
      LFOUND=.FALSE.
C
C Generate output picture
C
      DO 50 J=1,NROW
C
C Read row from segmented picture
C
         IF (SEMROW(1,RB2,NFMINT,J+JOFF,1,LP2)) GOTO 60
C
C If key IMAGE is set, output pixels are set to source pixel or value
C of key VALUE
C
         IF (LIMAGE) THEN
C
C Read row from source picture
C
            IF (SEMROW(1,RB1,NFMFP,J+JOFF,1,LP1)) GOTO 60
C
C Scan through source + segmented picture rows to generate output row
C
            DO 30 I=1,NCOL
C
C Fault negative pixel value in segmented picture row
C
               IF (IB2(I+IOFF).LT.0) GOTO 70
C
C Set corresponding output pixel (background = VALUE, particle = source
C pixel value)
C
               IF (IB2(I+IOFF).EQ.ID) THEN
                  RB4(I)=RB1(I+IOFF)
                  LFOUND=.TRUE.
               ELSE
                  RB4(I)=VALUE
               ENDIF
   30       CONTINUE
C
C Otherwise, output pixels are set to 0 or 1
C
         ELSE
C
C Scan through segmented picture row to generate output row
C
            DO 40 I=1,NCOL
C
C Fault negative pixel value
C
               IF (IB2(I+IOFF).LT.0) GOTO 70
C
C Set corresponding output pixel (background = 0, particle = 1)
C
               IF (IB2(I+IOFF).EQ.ID) THEN
                  IB4(I)=1
                  LFOUND=.TRUE.
               ELSE
                  IB4(I)=0
               ENDIF
   40       CONTINUE
         ENDIF
C
C Store results in output picture
C
         IF (SEMROW(2,RB4,INFORM,J,1,LP4)) GOTO 60
   50 CONTINUE
C
C Fault absence of particle in segmented picture
C
      IF (.NOT.LFOUND) ERROR=160
C
   60 RETURN
C
C Fault negative pixel value in segmented picture
C
   70 ERROR=154
      GOTO 60
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
