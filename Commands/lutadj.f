C Semper 6 processing module LUTADJ
C
      SUBROUTINE LUTADJ
C
C Ladjust :LUTADJ scaled= range= ra2= brightness dbri contrast hue +
C   dhue saturation dsat position dpos width upper lower tie +
C   ends= en2= red green blue initially +
C   hsv=0 hs2=1 hs3=1 hs4=hsv hs5=hs2 hs6=hs3
C
C Functions called
C
      INTEGER IVAL
      REAL VAL
      LOGICAL EQNQRE, EQSETD
      LOGICAL FSLU61,SEMLUT,EQSETS,OPT,ABANDN,LUTAD2,LUTADM,LUTADK
      LOGICAL SEMDPD,SEMDPN,VARSET,SEMLU,SEMDIA,SEMSOP
C Local variables
      INTEGER STATEK, STATEP, STATEB
      LOGICAL QKSET, QPSET, QBSET
      INTEGER MODE,ILUT,IDUM,STEP,HSV,PHSV,KEY,I,PARTN,DEVICE
      INTEGER RAN1,RAN2,NRAN1,NRAN2,END1,END2,MOD1,MOD2,RANMIN,RANMAX
      INTEGER VARX,VARY,TYPEX,TYPEY
      REAL PARAM(0:10),V,DV,IVARX,IVARY,VX,VY
      LOGICAL TIE,FINISH,CHANGE,RESTOR,ALL,LSCALE,LRED,LGREEN,LBLUE
      LOGICAL MODIFY,ADJUST(0:10),FIRST
C
      INCLUDE 'COMMON'
      INCLUDE 'EVENTS'
C
      INTEGER RGBLUT(0:3*LUTSIZ-1),OUTLUT(0:3*LUTSIZ-1)
      REAL    HSVLUT(0:3*LUTSIZ-1)
      EQUIVALENCE (RGBLUT,RB1), (HSVLUT,RB2), (OUTLUT,RB3)
C
C Parameters
C N.B. these parameters must be replicated in LUTADK and the numerical
C order must be maintained.
      INTEGER PHUE,PDHUE,PSAT,PDSAT,PBRI,PDBRI
      INTEGER PPOS,PWID,PLOW,PUPP,PNULL,PNORM,PROC
      PARAMETER (PHUE=1,PDHUE=2,PSAT=3,PDSAT=4,PBRI=5,PDBRI=6)
      PARAMETER (PPOS=7,PWID=8,PLOW=9,PUPP=10,PNULL=0,PNORM=0,PROC=1)
C Adjustment rate tweakers
      REAL PMANG,PMSTEP
      PARAMETER (PMANG=1.55, PMSTEP=0.02)
C Lut types
      INTEGER PMONO,PFALSE,PFULL
      PARAMETER (PMONO=1,PFALSE=2,PFULL=3)
C Packed names
      INTEGER NSCA,NRAN,NRA2,NEND,NEN2,NTIE,NBRI,NHUE,NSAT,NDPO,NWID
      INTEGER NPOS,NLOW,NUPP,NDBR,NCON,NDHU,NDSA,NRED,NGRE,NBLU,NR,NR2
      INTEGER NINI,NHSV,NHS2,NHS3,NHS4,NHS5,NHS6
      PARAMETER (NSCA=30521,NRAN=28854,NRA2=28872,NEND=8564,NEN2=8592)
      PARAMETER (NTIE=-366,NBRI=3929,NHUE=13645,NSAT=30460,NDPO=7055)
      PARAMETER (NWID=-5165,NPOS=26219,NLOW=19823,NUPP=-2257,NDBR=6498)
      PARAMETER (NCON=5414,NDHU=6741,NDSA=7161)
      PARAMETER (NRED=29004,NGRE=11925,NBLU=3701,NR=28800,NR2=30080)
      PARAMETER (NINI=14969,NHSV=13582,NHS2=13592,NHS3=13593,NHS4=13594)
      PARAMETER (NHS5=13595,NHS6=13596)
C ---------------------------------------------------------------------
C
C Initialise
      QKSET = .FALSE.
      QPSET = .FALSE.
      QBSET = .FALSE.
C
C Process key SCALED
      LSCALE = VARSET(NSCA)
      IF (LSCALE) THEN
C
C See if key SCALED is set to valid display partition number
         IF (SEMDPN(IVAL(NSCA),DEVICE,PARTN)) GOTO 90
C
C Fetch required DPD from work disc
         IF (SEMDPD(1,PARTN)) GOTO 90
C
C Fault display partition not containing a 2-D picture
         IF (DPTYP.NE.1) THEN
            ERROR=48
            IDERR=1000*DEVICE+PARTN
            GOTO 90
         ENDIF
      ENDIF
C
C Process keys RANGE,RA2 and END, EN2
      IF (LUTAD2(NRAN,NRA2,RAN1,RAN2)) GOTO 90
      IF (LUTAD2(NEND,NEN2,END1,END2)) GOTO 90
C ---------------------------------------------------------------------
      TIE = OPT(NTIE)
C
C If TIE is quoted, check that range is entirely within ends
      IF (TIE .AND. (END1.GE.RAN1 .OR. END2.LE.RAN2)) THEN
         ERROR = 3
         IDERR = NEND
         GOTO 90
      ENDIF
C Decide limits for RANGE parameters (they may be adjusted)
      IF (TIE) THEN
         RANMIN = END1+1
         RANMAX = END2-1
      ELSE
         RANMIN = 0
         RANMAX = LUTLEN-1
      ENDIF
C ---------------------------------------------------------------------
C Get the current Lut 1; Convert to HSV space
C Fetch current look-up table number, then the table itself
      ILUT=INT(CLUT)
      IF (SEMLUT(1,ILUT,MODE,RGBLUT)) GOTO 90
C
C Fault non-existent LUT
      IF (MODE.EQ.0) THEN
         ERROR=69
         IDERR=ILUT
         GOTO 90
      ELSE IF (MODE.EQ.PMONO .OR. MODE.EQ.PFALSE) THEN
C Monochrome or pseudo-colour
         CALL RGBHSV(RGBLUT,HSVLUT,0,LUTLEN-1,MODE)
      ELSE IF (MODE.EQ.PFULL) THEN
C Full colour
         LRED   = OPT(NRED)
         LGREEN = OPT(NGRE)
         LBLUE  = OPT(NBLU)
         IF (.NOT.LRED .AND. .NOT.LGREEN .AND. .NOT.LBLUE) THEN
            LRED   = .TRUE.
            LGREEN = .TRUE.
            LBLUE  = .TRUE.
         ENDIF
C Take the Lut to modify from one of the three guns in priority order:
         IF (LRED) THEN
            CALL RGBHSV(RGBLUT(0)       ,HSVLUT,0,LUTLEN-1,MODE)
         ELSE IF (LGREEN) THEN
            CALL RGBHSV(RGBLUT(LUTLEN)  ,HSVLUT,0,LUTLEN-1,MODE)
         ELSE IF (LBLUE) THEN
            CALL RGBHSV(RGBLUT(LUTLEN*2),HSVLUT,0,LUTLEN-1,MODE)
         ENDIF
      ENDIF
C
C Copy to output Lut as default
      DO 10 I = 0,3*LUTLEN-1
         OUTLUT(I) = RGBLUT(I)
   10 CONTINUE
C ---------------------------------------------------------------------
      IF (OPT(NINI)) THEN
C 'INITIALLY' quoted; must set endpoints of range to required colours
C  given by HSV,HS2,HS3 (at RAN1) to HS4,HS5,HS6 (at RAN2) (later code
C  will 'fill in' linearly between)
         HSVLUT(RAN2)          = VAL(NHS4)/360.0
         HSVLUT(RAN1)          = VAL(NHSV)/360.0
         HSVLUT(RAN2+LUTLEN)   = VAL(NHS5)
         HSVLUT(RAN1+LUTLEN)   = VAL(NHS2)
         HSVLUT(RAN2+LUTLEN*2) = VAL(NHS6)
         HSVLUT(RAN1+LUTLEN*2) = VAL(NHS3)
      ENDIF
C ---------------------------------------------------------------------
C Default to adjusting 'nothing'
      VARX  = PNULL
      TYPEX = PNORM
      VARY  = PNULL
      TYPEY = PNORM
      PARAM(0) = 0.0
C
C Decide which parameters are to be adjusted (highest priority first)
      CALL LUTADO(NBRI,PBRI ,PNORM,VARX,VARY,TYPEX,TYPEY)
      CALL LUTADO(NHUE,PHUE ,PNORM,VARX,VARY,TYPEX,TYPEY)
      CALL LUTADO(NSAT,PSAT ,PNORM,VARX,VARY,TYPEX,TYPEY)
      CALL LUTADO(NDPO,PWID ,PNORM,VARX,VARY,TYPEX,TYPEY)
C 'Width' is synonymous with Dposition
      CALL LUTADO(NWID,PWID ,PNORM,VARX,VARY,TYPEX,TYPEY)
      CALL LUTADO(NPOS,PPOS ,PNORM,VARX,VARY,TYPEX,TYPEY)
      CALL LUTADO(NLOW,PLOW ,PNORM,VARX,VARY,TYPEX,TYPEY)
      CALL LUTADO(NUPP,PUPP ,PNORM,VARX,VARY,TYPEX,TYPEY)
      CALL LUTADO(NDBR,PDBRI,PROC ,VARX,VARY,TYPEX,TYPEY)
C 'contrast' is synonymous with Dbrightness
      CALL LUTADO(NCON,PDBRI,PROC ,VARX,VARY,TYPEX,TYPEY)
      CALL LUTADO(NDHU,PDHUE,PROC ,VARX,VARY,TYPEX,TYPEY)
      CALL LUTADO(NDSA,PDSAT,PROC ,VARX,VARY,TYPEX,TYPEY)
C
C ---------------------------------------------------------------------
C Get current states, and allow KEYBOARD, POINTER and BUTTONS event
C queues to run, and lock the pointer queue.
C
      IF (EQNQRE(MKEY,STATEK,IDUM,IDUM)) GOTO 80
      IF (EQSETS(MKEY,QRUN)) GOTO 80
      QKSET = .TRUE.
      IF (EQNQRE(MPOINT,STATEP,IDUM,IDUM)) GOTO 80
      IF (EQSETS(MPOINT,QRUN)) GOTO 80
      IF (EQSETD(MPOINT,OSETL,1,0,0)) GOTO 80
      QPSET = .TRUE.
      IF (EQNQRE(MBUT,STATEB,IDUM,IDUM)) GOTO 80
      IF (EQSETS(MBUT,QRUN)) GOTO 80
      QBSET = .TRUE.
      IDUM=0
C Initial adjustment step for keyboard
      STEP=4
C ---------------------------------------------------------------------
C
C Set initial values of parameters to current ramps
C First: Hue, Saturation, and Value (brightness)
      DO 20 I=1,3
         HSV = LUTLEN*(I-1)
         PHSV = 2*I-1
C Central value is the average of the two ends
         PARAM(PHSV) = (HSVLUT(RAN1+HSV)+HSVLUT(RAN2+HSV))/2.0
C (Angular) rate-of-change
         IF (RAN1.NE.RAN2) THEN
            DV = HSVLUT(RAN2+HSV)-HSVLUT(RAN1+HSV)
            IF (I.EQ.1 .AND. ABS(DV).GT.0.5) THEN
C Hue treated specially: choose shortest route through colour space
               IF (DV.GT.0.0) THEN
                  DV = DV-1.0
               ELSE
                  DV = DV+1.0
               ENDIF
            ENDIF
            PARAM(PHSV+PROC) = DV
         ELSE
            PARAM(PHSV+PROC) = 0.0
         ENDIF
   20 CONTINUE
C
C Second: Lower, Upper
      PARAM(PLOW) = RAN1/REAL(LUTLEN)
      PARAM(PUPP) = RAN2/REAL(LUTLEN)
C
C Lastly Position and Width
      PARAM(PPOS) = (PARAM(PUPP)+PARAM(PLOW))/2.0
      PARAM(PWID) =  PARAM(PUPP)-PARAM(PLOW)
C
C CHANGE is true since these are 'new' adjustments
      CHANGE = .TRUE.
C FIRST is true so that ajustment is not performed on first pass, yet
C lut is updated
      FIRST = .TRUE.
C ALL is true so that all parameters are updated on first pass
      ALL = .TRUE.
C FINISH is false so we don't exit accidentally on the first pass
      FINISH = .FALSE.
C
C ---------------------------------------------------------------------
   30 IF (CHANGE) THEN
C Save initial values of x and y parameters
         IVARX = PARAM(VARX)
         IVARY = PARAM(VARY)
C Note what is being varied (irrespective of absolute, rate-of-change)
         DO 40 I = 0,10
            ADJUST(I) = .FALSE.
   40    CONTINUE
         ADJUST(VARX-TYPEX) = .TRUE.
         ADJUST(VARY-TYPEY) = .TRUE.
C
C Compute Position and Width in terms of Upper and Lower
         PARAM(PPOS) = (PARAM(PUPP)+PARAM(PLOW))/2.0
         PARAM(PWID) =  PARAM(PUPP)-PARAM(PLOW)
C
C Warn if adjusting colour parameters on monochrome or full colour lut
         IF ((MODE.EQ.PMONO .OR. MODE.EQ.PFULL) .AND.
     +       (ADJUST(PHUE)  .OR. ADJUST(PSAT))) THEN
            IF (SEMDIA(
     +         'Warning: Adjusting Hue or Saturation has no effect on',
     +         NDIWAR)) GOTO 90
            IF (SEMDIA(
     +         '         monochrome or full-colour look-up tables',
     +         NDIWAR)) GOTO 90
C
C Warn if Saturation is zero and not under adjustment, and Hue is under
C adjustment.
         ELSE IF (ABS(PARAM(PSAT)).LT.1E-5 .AND.
     +            ABS(PARAM(PDSAT)).LT.1E-5 .AND.
     +            ADJUST(PHUE) .AND..NOT.ADJUST(PSAT)) THEN
            IF (SEMDIA(
     +         'Warning: Adjusting Hue has no effect on colour',
     +         NDIWAR)) GOTO 90
            IF (SEMDIA(
     +         '         until Saturation is non-zero',
     +         NDIWAR)) GOTO 90
         ENDIF
      ENDIF
C ---------------------------------------------------------------------
      IF (.NOT. FIRST) THEN
C If necessary, convert to angular measure (makes low values of
C gradiant adjust finely, large values adjust coarsely)
         IF (TYPEX.EQ.PNORM) THEN
            VX = PARAM(VARX)
         ELSE
            IF (RAN1.NE.RAN2) THEN
               VX = ATAN(PARAM(VARX)/REAL(RAN2-RAN1)/PMSTEP)/PMANG
            ELSE
               VX = 0.0
            ENDIF
         ENDIF
         IF (TYPEY.EQ.PNORM) THEN
            VY = PARAM(VARY)
         ELSE
            IF (RAN1.NE.RAN2) THEN
               VY = ATAN(PARAM(VARY)/REAL(RAN2-RAN1)/PMSTEP)/PMANG
            ELSE
               VY = 0.0
            ENDIF
         ENDIF
C ---------------------------------------------------------------------
C Permit the user to adjust selected parameters
         IF (SEMSOP()) GOTO 90
         IF (LUTADM(VX,VY,FINISH,STEP,KEY)) GOTO 90
         IF (ABANDN(ERROR)) GOTO 90
C ---------------------------------------------------------------------
C Convert back to 'step' from angular measure if necessary
         IF (TYPEX.EQ.PNORM) THEN
            PARAM(VARX) = VX
         ELSE
            PARAM(VARX) = PMSTEP*REAL(RAN2-RAN1)*TAN(VX*PMANG)
         ENDIF
         IF (TYPEY.EQ.PNORM) THEN
            PARAM(VARY) = VY
         ELSE
            PARAM(VARY) = PMSTEP*REAL(RAN2-RAN1)*TAN(VY*PMANG)
         ENDIF
C
C Interpret any remaining keystrokes
         IF (LUTADK(KEY,STEP,FINISH,VARX,VARY,TYPEX,TYPEY,CHANGE,
     +      RESTOR)) GOTO 90
         IF (ABANDN(ERROR)) GOTO 90
C ---------------------------------------------------------------------
C Restore initial values ?
         IF (RESTOR) THEN
            PARAM(VARX) = IVARX
            PARAM(VARY) = IVARY
         ENDIF
C ---------------------------------------------------------------------
C If adjusting position or width, convert to lower, upper
         IF (ADJUST(PPOS)) THEN
            IF (ADJUST(PUPP)) THEN
               PARAM(PWID) = (PARAM(PUPP)-PARAM(PPOS))*2.0
               PARAM(PLOW) =  PARAM(PUPP)-PARAM(PWID)
            ELSE IF (ADJUST(PLOW)) THEN
               PARAM(PWID) = (PARAM(PPOS)-PARAM(PLOW))*2.0
               PARAM(PUPP) =  PARAM(PLOW)+PARAM(PWID)
            ELSE
               PARAM(PLOW) = PARAM(PPOS)-PARAM(PWID)/2.0
               PARAM(PUPP) = PARAM(PPOS)+PARAM(PWID)/2.0
            ENDIF
         ELSE IF (ADJUST(PWID)) THEN
            IF (ADJUST(PUPP)) THEN
               PARAM(PLOW) =  PARAM(PUPP)-PARAM(PWID)
               PARAM(PPOS) = (PARAM(PLOW)+PARAM(PUPP))/2.0
            ELSE IF (ADJUST(PLOW)) THEN
               PARAM(PUPP) =  PARAM(PLOW)+PARAM(PWID)
               PARAM(PPOS) = (PARAM(PLOW)+PARAM(PUPP))/2.0
            ELSE
               PARAM(PLOW) = PARAM(PPOS)-PARAM(PWID)/2.0
               PARAM(PUPP) = PARAM(PPOS)+PARAM(PWID)/2.0
            ENDIF
         ENDIF
C
C Convert PARAM(PLOW:PUPP) to NRAN1,NRAN2 and clip
         NRAN1 = MAX(RANMIN,MIN(NINT(PARAM(PLOW)*LUTLEN),RANMAX))
         NRAN2 = MAX(NRAN1 ,MIN(NINT(PARAM(PUPP)*LUTLEN),RANMAX))
C
C Has the range changed ?
         IF (NRAN1.NE.RAN1 .OR. NRAN2.NE.RAN2) THEN
C
C Must regenerate ALL parameters
            ALL = .TRUE.
C
            IF (.NOT. TIE .AND. (NRAN1.GT.RAN1)) THEN
C 'Uncovering' on lower part of lut; copy old lut to new
               DO 50 I = RAN1,NRAN1-1
                  OUTLUT(I         ) = RGBLUT(I         )
                  OUTLUT(I+LUTLEN  ) = RGBLUT(I+LUTLEN  )
                  OUTLUT(I+LUTLEN*2) = RGBLUT(I+LUTLEN*2)
   50          CONTINUE
            ENDIF
            IF (.NOT. TIE .AND. (NRAN2.LT.RAN2)) THEN
C 'Uncovering' on upper part of lut; copy old lut to new
               DO 60 I = NRAN2+1,RAN2
                  OUTLUT(I         ) = RGBLUT(I         )
                  OUTLUT(I+LUTLEN  ) = RGBLUT(I+LUTLEN  )
                  OUTLUT(I+LUTLEN*2) = RGBLUT(I+LUTLEN*2)
   60          CONTINUE
            ENDIF
         ELSE
C
C Only regenerate each parameter if it is being adjusted
            ALL = .FALSE.
         ENDIF
C
         RAN1 = NRAN1
         RAN2 = NRAN2
      ENDIF
C ---------------------------------------------------------------------
C Generate a new Lut from the parameters describing it.  It may be
C necessary to set ALL of Hue, Saturation, and intensity (on the first
C pass, or when the RANGE has changed), or only those specified by the
C ADJUST array.
C
C Assume no modifications
      MODIFY = .FALSE.
C
      DO 70 I = 1,3
         HSV = LUTLEN*(I-1)
         PHSV = 2*I-1
C
C Is this parameter (H,S, or V) affected by the adjustments in force?
         IF (ALL.OR. ADJUST(PHSV)) THEN
            MODIFY = .TRUE.
C
C Reconstruct parameters of ramp: step and initial value
            DV = PARAM(PHSV+PROC )
            V  = PARAM(PHSV+PNORM) - DV/2.0
C
C Set parameters to appropriate ramp
            CALL LUTADL(HSVLUT(HSV),RAN1,RAN2,V,DV)
C
C Default range over which Lut is to be modified
            MOD1 = RAN1
            MOD2 = RAN2
C
            IF (TIE) THEN
C Interval [end1..ran1-1] starts at value at end1, joins with above ramp
               IF (RAN1.NE.END1+1) THEN
                  DV = HSVLUT(RAN1+HSV) - HSVLUT(END1+HSV)
                  V  = HSVLUT(END1+HSV)
                  CALL LUTADL(HSVLUT(HSV),END1,RAN1-1,V,DV)
               ENDIF
C Interval [ran2+1..end2] ends at current value, joins with above ramp
               IF (END2.NE.RAN2+1) THEN
                  DV = HSVLUT(END2+HSV) - HSVLUT(RAN2+HSV)
                  V  = HSVLUT(RAN2+HSV)
                  CALL LUTADL(HSVLUT(HSV),RAN2,END2-1,V,DV)
               ENDIF
C
C Lut is modified over the range [end1..end2]
               MOD1 = END1
               MOD2 = END2
            ENDIF
         ENDIF
C
   70 CONTINUE
C
      IF (MODIFY) THEN
         IF (MODE.EQ.PMONO) THEN
C Monochrome
            CALL LUTADV(HSVLUT(LUTLEN*2),OUTLUT,MOD1,MOD2)
         ELSE IF (MODE.EQ.PFALSE) THEN
C Pseudo-colour: Convert entire Lut to RGB space
            CALL HSVRGB(HSVLUT,OUTLUT,MOD1,MOD2)
         ELSE IF (MODE.EQ.PFULL) THEN
C Full colour
            IF (LRED) THEN
               CALL LUTADV(HSVLUT(LUTLEN*2),OUTLUT(0)       ,MOD1,MOD2)
            ENDIF
            IF (LGREEN) THEN
               CALL LUTADV(HSVLUT(LUTLEN*2),OUTLUT(LUTLEN)  ,MOD1,MOD2)
            ENDIF
            IF (LBLUE) THEN
               CALL LUTADV(HSVLUT(LUTLEN*2),OUTLUT(LUTLEN*2),MOD1,MOD2)
            ENDIF
         ENDIF
      ENDIF
C
C Update look-up table in framestore
      IF (FSLU61(2,ILUT,MODE,OUTLUT,ERROR)) GOTO 90
C
C No longer the first pass
      FIRST = .FALSE.
      IF (.NOT.FINISH) GOTO 30
C ---------------------------------------------------------------------
C Update look-up table values on work disc
      IF (SEMLUT(2,ILUT,MODE,OUTLUT)) GOTO 90
C
C Report the final limits in LOWER and UPPER
      IF (LSCALE) THEN
         VX = DPMIN+(DPMAX-DPMIN)*REAL(RAN1)/REAL(LUTLEN)
         VY = DPMIN+(DPMAX-DPMIN)*REAL(RAN2)/REAL(LUTLEN)
      ELSE
         VX = REAL(RAN1)
         VY = REAL(RAN2)
      ENDIF
C
      IF (SEMLU(1,NR ,VX)) GOTO 90
      IF (SEMLU(1,NR2,VY)) GOTO 90
C
C     All done, go and reset the queues
C
      GOTO 90
C
C Here if there was an error setting the queue states: set the error
C number appropriately, then reset those which need doing
C
   80 CONTINUE
      ERROR = 161
C
C Finished - reset the queue states as needed
C
   90 CONTINUE
      IF ( QKSET ) THEN
         QKSET = .FALSE.
         IF (EQSETS(MKEY,STATEK)) GOTO 80
      ENDIF
      IF ( QPSET ) THEN
         QPSET = .FALSE.
         IF (EQSETD(MPOINT,OSETL,0,0,0)) GOTO 80
         IF (EQSETS(MPOINT,STATEP)) GOTO 80
      ENDIF
      IF ( QBSET ) THEN
         QBSET = .FALSE.
         IF (EQSETS(MBUT,STATEB)) GOTO 80
      ENDIF
C
C     ALL DONE
C     --------
C
      RETURN
C
C Copyright (C) 1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C Semper 6 subsidiary module LUTADM
C
      LOGICAL FUNCTION LUTADM(XVAR,YVAR,FINISH,ISTEP,KEY)
C
      INTEGER ISTEP,KEY
      REAL XVAR,YVAR
      LOGICAL FINISH
C
      INCLUDE 'COMMON'
      INCLUDE 'EVENTS'
C
C Functions called
      LOGICAL EQNEXT,EQREAD
C Local variables
      INTEGER QUEUE
      INTEGER IDUM,IENT,ICL,IX,IY,DELTAX,DELTAY
      INTEGER IXMIN,IXMAX,IYMIN,IYMAX
      REAL SCALE
C Parameters
      PARAMETER (IXMIN=-512,IYMIN=-512,IXMAX=512,IYMAX=512)
      PARAMETER (SCALE=1.0/512.0)
C ---------------------------------------------------------------------
      LUTADM = .FALSE.
C
C     Set current POINTER value
C
      IX =  NINT(XVAR/SCALE)
      IY = -NINT(YVAR/SCALE)
C
C     See what events are happening
C
   10 CONTINUE
      KEY=0
      IF ( EQNEXT ( QUEUE ) )  GOTO 10
      IF ( QUEUE .EQ. MBUT )   GOTO 20
      IF ( QUEUE .EQ. MKEY )   GOTO 30
      IF ( QUEUE .EQ. MPOINT ) GOTO 60
      IF ( QUEUE .EQ. MBREAK ) GOTO 90
C
C ---------------------------------------------------------------------
C     Button pressed
C     Check button close 1 (terminate operation)
C
   20 CONTINUE
      IF (EQREAD(MBUT,QTAKE,IENT,ICL,IDUM,IDUM)) GOTO 100
      IF ( ICL.EQ.1 ) THEN
         FINISH = .TRUE.
         GOTO 90
      ENDIF
C
      GOTO 10
C ---------------------------------------------------------------------
C     Key pressed - read and decode all the arrow keys which are ready
C
   30 CONTINUE
   40 CONTINUE
         IF (EQREAD(MKEY,QTAKE,IENT,KEY,IDUM,IDUM)) GOTO 100
         IF ( IENT .EQ. 0 ) GOTO 50
C
C        Try to decode
C
         IF (KEY.EQ.KBRITE) THEN
            IX = IX + ISTEP
         ELSE IF (KEY.EQ.KBLEFT) THEN
            IX = IX - ISTEP
         ELSE IF (KEY.EQ.KBUP) THEN
            IY = IY - ISTEP
         ELSE IF (KEY.EQ.KBDOWN) THEN
            IY = IY + ISTEP
         ELSE
C
C           Key cannot be decoded here; leave it to caller to interpret
C
            FINISH = .FALSE.
            GOTO 90
         ENDIF
         GOTO 40
   50 CONTINUE
C
C     Key was decoded; don't leave it hanging around
C
      KEY = 0
      GOTO 80
C
C ---------------------------------------------------------------------
C     Change values with mouse.
C     There should be something in the pointer queue; read all entries
C     and accumulate the delta values
C
   60 CONTINUE
         IF (EQREAD(MPOINT,QTAKE,IENT,DELTAX,DELTAY,IDUM)) GOTO 100
         IF (IENT.EQ.0) GOTO 70
         IX = IX + DELTAX
         IY = IY + DELTAY
         GOTO 60
   70 CONTINUE
C
C     If value exceeds limits, do something special
C
   80 IF (IX.GT.IXMAX) THEN
         IX = IXMAX
      ENDIF
      IF (IX.LT.IXMIN) THEN
         IX = IXMIN
      ENDIF
C
      IF (IY.GT.IYMAX) THEN
         IY = IYMAX
      ENDIF
      IF (IY.LT.IYMIN) THEN
         IY = IYMIN
      ENDIF
C
      XVAR =  REAL(IX)*SCALE
      YVAR = -REAL(IY)*SCALE
C ---------------------------------------------------------------------
C
C ALL DONE
C --------
C
   90 CONTINUE
      RETURN
C
  100 CONTINUE
C
C Here if there was an event queue error
C
      ERROR = 161
      LUTADM = .TRUE.
      GOTO 90
C
C Copyright (C) 1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C Semper 6 subsidiary module LUTADK
C
      LOGICAL FUNCTION LUTADK(KEY,STEP,FINISH,VARX,VARY,TYPEX,TYPEY,
     +                        CHANGE,RESTOR)
C
C Interprets remaining KEY presses discovered by LUTADM.
C Permits adjustment of STEP size, notifies that adjustment should be
C terminated (FINISH), permits changes to the adjustment parameters
C (given in VARX, VARY, TYPEX, TYPEY and returned therein) (any change
C causes CHANGE to go .true.; finally can indicate that original
C parameter values should be RESTOREd.
C
      INCLUDE 'COMMON'
C
      INTEGER KEY,STEP,VARX,VARY,TYPEX,TYPEY
      LOGICAL FINISH,CHANGE,RESTOR,LUTADA,SEMCON
C Local variables
      CHARACTER*10 STRNAM(0:10)
C
      DATA STRNAM /'nothing','Hue','Hue','Saturation','Saturation',
     +             'Brightness','Brightness',
     +             'Position','Width','Lower edge','Upper edge'/
C
C ---------------------------------------------------------------------
      LUTADK = .TRUE.
C
      RESTOR = .FALSE.
      CHANGE = .FALSE.
C
C Any point in continuing ?
      IF (.NOT.(KEY.EQ.0 .OR. FINISH)) THEN
C
         IF (KEY.EQ.KUCC .OR. KEY.EQ.KLCC) THEN
C
C Coarser stepping
C
            IF (STEP .LT. 64) STEP = STEP + STEP
         ELSE IF (KEY.EQ.KUCF .OR. KEY.EQ.KLCF) THEN
C
C Finer stepping
C
            IF (STEP .NE. 1) STEP = STEP / 2
         ELSE IF (KEY.EQ.KUCR .OR. KEY.EQ.KLCR .OR. KEY.EQ.KDOT) THEN
            RESTOR = .TRUE.
         ELSE IF (KEY.EQ.KBRET .OR. KEY.EQ.KBKILL) THEN
C <RETURN> etc.: finished
            FINISH = .TRUE.
         ELSE IF (KEY.EQ.KLCX .OR. KEY.EQ.KUCX) THEN
            IF (LUTADA(VARX,TYPEX)) GOTO 10
            CHANGE = .TRUE.
         ELSE IF (KEY.EQ.KLCY .OR. KEY.EQ.KUCY) THEN
            IF (LUTADA(VARY,TYPEY)) GOTO 10
            CHANGE = .TRUE.
         ELSE IF (KEY.EQ.KQUEST) THEN
C Status report
            IF (SEMCON(' ')) GOTO 10
            RECORD(1:45)='The x axis is controlling: Rate of change of '
            IF (TYPEX.EQ.0) THEN
               RECORD(28:37) = STRNAM(VARX)
               IF (SEMCON(RECORD(1:37))) GOTO 10
            ELSE
               RECORD(46:55) = STRNAM(VARX)
               IF (SEMCON(RECORD(1:55))) GOTO 10
            ENDIF
            RECORD(1:45)='The y axis is controlling: Rate of change of '
            IF (TYPEY.EQ.0) THEN
               RECORD(28:37) = STRNAM(VARY)
               IF (SEMCON(RECORD(1:37))) GOTO 10
            ELSE
               RECORD(46:55) = STRNAM(VARY)
               IF (SEMCON(RECORD(1:55))) GOTO 10
            ENDIF
         ELSE
C
C Help report
C
            IF (SEMCON(' ')) GOTO 10
            IF (SEMCON(
     +         'Use Mouse or cursor keys to adjust the look-up table'
     +         )) GOTO 10
            IF (SEMCON(
     +         'C and F can be used for coarser or finer stepping'
     +         )) GOTO 10
            IF (SEMCON(
     + 'Left mouse button, <escape> or <return> terminates adjustments'
     +         )) GOTO 10
            IF (SEMCON(
     +         'R or . restores the original look-up table'
     +         )) GOTO 10
            IF (SEMCON(
     +         'X or Y permits a change in the adjustment parameter'
     +         )) GOTO 10
            IF (SEMCON(
     +         '? reports the current adjustment parameters'
     +         )) GOTO 10
         ENDIF
      ENDIF
C
      LUTADK = .FALSE.
C
   10 RETURN
C
C Copyright (C) 1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C Semper 6 subsidiary module LUTADA
C
      LOGICAL FUNCTION LUTADA(VAR,TYPE)
C
      INTEGER VAR,TYPE
C
      INCLUDE 'COMMON'
      INCLUDE 'EVENTS'
C
C Functions called
      LOGICAL EQNEXT,EQREAD,SEMCON
C Local variables
      INTEGER ROC,IDUM,IENT,KEY,QUEUE
      INTEGER PHUE,PDHUE,PSAT,PDSAT,PBRI,PDBRI
      INTEGER PPOS,PWID,PLOW,PUPP,PNULL,PNORM,PROC
C Parameters
      PARAMETER (PHUE=1,PDHUE=2,PSAT=3,PDSAT=4,PBRI=5,PDBRI=6)
      PARAMETER (PPOS=7,PWID=8,PLOW=9,PUPP=10,PNULL=0,PNORM=0,PROC=1)
C
C ---------------------------------------------------------------------
      LUTADA = .FALSE.
C
      ROC = PNORM
C
C Write information message
   10 IF (SEMCON(' ')) GOTO 40
      IF (SEMCON(
     +   'Type B, H or S to adjust Brightness, Hue or Saturation'
     +   )) GOTO 40
      IF (SEMCON('Prefix any of above with D to indicate rate-of change'
     +   )) GOTO 40
      IF (SEMCON('Type P to adjust Position, W to adjust Width'
     +   )) GOTO 40
      IF (SEMCON('Type L to adjust lower limit, U upper limit'
     +   )) GOTO 40
      IF (SEMCON('Type O to turn the adjustment off')) GOTO 40
C
   20 CONTINUE
      IF (EQNEXT(QUEUE)) GOTO 20
      IF (QUEUE .EQ. MBREAK) GOTO 30
      IF (EQREAD(QUEUE,QTAKE,IENT,KEY,IDUM,IDUM)) GOTO 50
C
C Have we got a key press? Ignore it if not
      IF (QUEUE.NE.MKEY) GOTO 20
C
      IF (KEY.EQ.KBRET .OR. KEY.EQ.KBKILL) THEN
C <RETURN> etc.: cancel changes
         GOTO 30
      ELSE IF (KEY.EQ.KLCD .OR. KEY.EQ.KUCD) THEN
C A 'rate-of-change' adjustment required; note it
         ROC = PROC
         GOTO 20
      ELSE IF (KEY.EQ.KLCB .OR. KEY.EQ.KUCB) THEN
         VAR  = PBRI+ROC
         TYPE = ROC
      ELSE IF (KEY.EQ.KLCS .OR. KEY.EQ.KUCS) THEN
         VAR  = PSAT+ROC
         TYPE = ROC
      ELSE IF (KEY.EQ.KLCH .OR. KEY.EQ.KUCH) THEN
         VAR  = PHUE+ROC
         TYPE = ROC
      ELSE IF (KEY.EQ.KLCP .OR. KEY.EQ.KUCP) THEN
         VAR  = PPOS+ROC
         TYPE = PNORM
      ELSE IF (KEY.EQ.KLCL .OR. KEY.EQ.KUCL) THEN
         VAR  = PLOW
         TYPE = PNORM
      ELSE IF (KEY.EQ.KLCU .OR. KEY.EQ.KUCU) THEN
         VAR  = PUPP
         TYPE = PNORM
      ELSE IF (KEY.EQ.KLCW .OR. KEY.EQ.KUCW) THEN
         VAR  = PWID
         TYPE = PNORM
      ELSE IF (KEY.EQ.KLCO .OR. KEY.EQ.KUCO) THEN
         VAR  = PNULL
         TYPE = PNORM
      ELSE
C Repeat information message
         GOTO 10
      ENDIF
C
   30 CONTINUE
      RETURN
C
C Here if SEMCON failed
C
   40 CONTINUE
      LUTADA = .TRUE.
      GOTO 30
C
C Here if there was an event queue error
C
   50 CONTINUE
      ERROR = 161
      LUTADA = .TRUE.
      GOTO 30
C
C Copyright (C) 1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C Semper 6 subsidiary module LUTADO
C
      SUBROUTINE LUTADO(NAME,INDEX,TYPE,VARX,VARY,TYPEX,TYPEY)
C
C Called with (packed) NAME of each parameter which can be nominated to
C be adjusted.  INDEX and TYPE are set by caller to the first and second
C indices into the PARAMS array for the appropriate parameter.  These
C are copied into VARY and TYPEY if the 'y' adjustment is to cause
C PARAMS(VARY,TYPEY) to vary; similarly for VARX and TYPEX.
C VARX and VARY should be set to zero before the first call; they remain
C zero if no parameter is allocated to the particular axis.
C
      INTEGER NAME,INDEX,TYPE,VARX,VARY,TYPEX,TYPEY
C
C Functions called
      LOGICAL OPT
C
C ---------------------------------------------------------------------
      IF (OPT(NAME)) THEN
C Allocate to 'y' axis if free ...
         IF (VARY.EQ.0) THEN
            VARY  = INDEX
            TYPEY = TYPE
C ... or to 'x' axis if free
         ELSE IF (VARX.EQ.0) THEN
            VARX  = INDEX
            TYPEX = TYPE
         ENDIF
      ENDIF
C
      RETURN
C
C Copyright (C) 1989:  Synoptics Ltd,  All Rights Reserved
C
      END
C Semper 6 subsidiary module RGBHSV
C
      SUBROUTINE RGBHSV(INLUT,OUTLUT,START,END,MODE)
C
C Converts the RGB array in INLUT into the corresponding HSV-encoded
C array in OUTLUT over array indices START..END.  Both arrays packed in
C normal Semper format, i.e. all H, all S, then all V.
C RGB array is INTEGER, values 0..LUTMAX
C HSV array is REAL,    values 0..1
C MODE corresponds to the Semper lut mode.  In mode 2 (Pseudo-colour),
C INLUT is treated as RGB. In modes 1 and 3 (mono and full colour),
C INLUT is treated as a mono lut and H and S are all returned zero
C
      INCLUDE 'COMMON'
C
      INTEGER INLUT(0:*),START,END,I,MODE
      REAL OUTLUT(0:*)
C     Local variables
      REAL R,G,B,CR,CG,CB,MAXRGB,MINRGB,RANGE,H,S,V
C
C ---------------------------------------------------------------------
      IF (MODE.EQ.2) THEN
         DO 10 I = START,END
C
         R = REAL(INLUT(I))/REAL(LUTMAX)
         G = REAL(INLUT(I+LUTLEN))/REAL(LUTMAX)
         B = REAL(INLUT(I+LUTLEN*2))/REAL(LUTMAX)
C
         MAXRGB = MAX(R,MAX(G,B))
         MINRGB = MIN(R,MIN(G,B))
C
C 'V' Brightness value is simply the maximum
         V = MAXRGB
C
C ---------------------------------------------------------------------
         IF (MAXRGB.GT.0.0) THEN
            RANGE = MAXRGB-MINRGB
C 'S' Saturation
            S = (RANGE)/MAXRGB
         ELSE
            S = 0.0
         ENDIF
C
         IF (S.NE.0.0) THEN
C
C Non-zero saturation so can determine hue; xC is distance of colour
C from colour x.
            CR = (MAXRGB-R)/RANGE
            CG = (MAXRGB-G)/RANGE
            CB = (MAXRGB-B)/RANGE
C
            IF (R.EQ.MAXRGB) THEN
C Colour is between yellow and magenta
               H = CB-CG
            ELSE IF (G.EQ.MAXRGB) THEN
C Colour is between cyan and yellow
               H = 2+CR-CB
            ELSE
C Colour is between magenta and cyan
               H = 4+CG-CR
            ENDIF
C Convert to range [0..1] and make non-negative
               H = H/6.0
            IF (H.LT.0.0) H = H+1.0
         ELSE
C ---------------------------------------------------------------------
C
C Zero saturation so hue is arbitrary
            H = 0.0
         ENDIF
C
         OUTLUT(I)          = H
         OUTLUT(I+LUTLEN)   = S
         OUTLUT(I+LUTLEN*2) = V
C
   10    CONTINUE
C ---------------------------------------------------------------------
      ELSE IF (MODE.EQ.1 .OR. MODE.EQ.3) THEN
         DO 20 I = START,END
C Zero Hue and Saturation
            OUTLUT(I)          = 0.0
            OUTLUT(I+LUTLEN)   = 0.0
            OUTLUT(I+LUTLEN*2) = REAL(INLUT(I))/REAL(LUTMAX)
   20    CONTINUE
C
      ENDIF
C ---------------------------------------------------------------------
      RETURN
C
C Copyright (C) 1989:  Synoptics Ltd,  All Rights Reserved
C
      END
