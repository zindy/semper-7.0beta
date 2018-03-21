C Semper 6 system module SEMOPN
C
      LOGICAL FUNCTION SEMOPN(IOP,NPIC,NCOL,NROW,NLAY,CLASS,FORM,LPN)
      INTEGER IOP,NPIC,NCOL,NROW,NLAY,CLASS,FORM,LPN
C
C Central data storage management: manages disc directories, display
C partition descriptors and tape picture scans, contructing PCBs for
C use in subsequent data access; also effects label generation and
C transmission between pictures.
C
C iop=1  opens an existing picture, returning dimensions, class, form
C        and label, and the LPN assigned
C iop=2  opens a new picture with the given dimensions, class and form,
C        using the supplied LPN (if non-zero) to furnish a title
C        and centre, and returning the LPN assigned for the new picture
C iop=3  opens a temporary (workspace) picture, on the first writable
C        disc, returning the LPN assigned
C
C Uses RB1-3 as work-space; labels are usually left in RB1 on exit, in
C integer form (LBLINC indicates whether or not this is true: the label
C cannot be recovered in an OLD mode tape re-open in installations
C where tapes cannot be backspaced.)
C
C TRUE returns indicate errors as usual; error 27 is returned for
C an attempt to open a NEW tape picture, but COPY may safely ignore
C the error and make use of the label constructed in RB1.
C
C Logic applicable to implicit disc picture deletion:
C - if an existing picture is found with matching number
C   - if dims, form (but not nec class) match, existing pic re-alloc
C        with fresh label
C   - else WHEN negated, causing retention for current cmd only,
C        and disc directory number negated for the same purpose;
C        fresh space found for new pic
C
C Temporaries are deleted finally by SEMDEL(2) call made by SEMINX on
C re-entry if REQDTS (REQuest Delete Temporaries) is set.
C
      LOGICAL DISC,OPT,SEMLAB,FSINIT,FSERAS,FSVIEW,FSELEC
      LOGICAL SEMLNF,SEMMED,SEMDPD,SEMMON
C
      INCLUDE 'COMMON'
C
      INTEGER*4 SDL(256),PSIZE,SSIZE,FREE,I4N,I41,I42,LNDIR4
      PARAMETER (I41=1,I42=2)
      INTEGER HEADER(6),LABEL(LNLAB),LABEL3(0:LNLAB)
      INTEGER I,J,K,L,LNPXL,L1,MEDIUM,N,N1,N2,NEWLPN,NP1,NP2,NUMBER
      INTEGER WHTEMP(NLPS),RPARS(6),TMPDEV
      INTEGER RNCOL,RNROW,RNLAY,RCCOL,RCROW,RCLAY
      INTEGER SAM,SDPTR,SDPTR2,DEVICE,DIRSZE,DGRAN,PLSTYP
C
      LOGICAL INUSE,REALOC,SPCORG
C
C Packed names
C
      INTEGER NERASE,NVIEW,NSELEC
      PARAMETER (NERASE=8721,NVIEW=-3566,NSELEC=30612)
C
      EQUIVALENCE (RB1,LABEL),(RB2,SDL),(LABEL(LNLAB),LABEL3)
      EQUIVALENCE (RPARS,RNCOL),(RPARS(2),RNROW),(RPARS(3),RNLAY)
      EQUIVALENCE (RPARS(4),RCCOL),(RPARS(5),RCROW),(RPARS(6),RCLAY)
C
C ****** CHANGE ******
C MACVBS contains list of verbs allowed to open macros:
C no change will be needed here at first installation time, but if
C any local verbs written subsequently need access to macro
C texts, this list must be lengthened and the array lengthened
C
      INTEGER NMACVB
      PARAMETER (NMACVB=11)
      INTEGER MACVBS(NMACVB)
C
C Present contents of MACVBS: MACRO,COPY,EXAMINE,TITLE,WP,EDIT,LIST,
C                             SAVE,PCB,READ,WRITE
C
      DATA MACVBS/20843,5416,8961,-381,-5441,8169,19579,
     +   30462,25722,29001,-5530/
C
C ****** ****** ******
C
C Label id flag: Semper
C
      DATA HEADER/KUCS,KLCE,KLCM,KLCP,KLCE,KLCR/
C
      SEMOPN = .TRUE.
C
C ------ MONITOR ------
C
      IF (MONIT) THEN
         IF (IOP.EQ.1) THEN
            WRITE (RECORD,10) IOP,NPIC
         ELSE
            WRITE (RECORD,20) IOP,NPIC,LPN
            IF (SEMMON(RECORD,'SEMOPN',3)) GOTO 440
            WRITE (RECORD,30) NCOL,NROW,NLAY,CLASS,FORM
         ENDIF
         IF (SEMMON(RECORD,'SEMOPN',3)) GOTO 440
      ENDIF
C
   10 FORMAT ('!-SEMOPN: op',I2,' picture',I5)
   20 FORMAT ('!-SEMOPN: op',I2,' picture',I5,' lpn',I3)
   30 FORMAT ('          ncol,nrow,nlay',3I5,'  class,form',2I2)
C
C ------ ------- ------
C
      NEWLPN = -1
      REALOC = .FALSE.
      INUSE  = .FALSE.
      LBLINC = .FALSE.
      IDERR  = NPIC
C
C Check the pic size given if NEW
C
      IF (IOP.NE.1) THEN
         IF (SEMLNF(FORM,LNPXL)) GOTO 440
         IF (NCOL.GT.LNBUF/LNPXL .OR.
     +       NCOL.LE.0 .OR. NROW.LE.0 .OR. NLAY.LE.0) THEN
            ERROR = 5
            GOTO 380
         ENDIF
      ENDIF
      TMPDEV = 1
C
C If TEMP open, find a writeable disc
C
   40 IF (IOP .EQ. 3) THEN
         IDERR = -1
         NUMBER= -1
         LPN = 0
         REQDTS = .TRUE.
         IF (TMPDEV .LE. NDVS) THEN
C
            DO 50 DEVICE = TMPDEV,NDVS
               IF (MEDN(DEVICE).EQ.MEDDC.OR.MEDN(DEVICE).EQ.MEDVM) THEN
                  IF (DVTYP(DEVICE).EQ.FLTPIC) THEN
                     IF (PROTN(DEVICE).EQ.0) GOTO 60
                  ENDIF
               ENDIF
   50       CONTINUE
C
C None assigned or previous error looking at another device
C
         ENDIF
         IF (TMPDEV .EQ. 1) ERROR = 51
         GOTO 380
C
C Note delete request
C
   60    TPSID(DEVICE) = 1
C
C Clear error from previous try
C
         ERROR = 0
      ELSE
C
C Check pic num legal and device assigned
C
         IF (NPIC .LE. 0) GOTO 450
         DEVICE=NPIC/1000
         NUMBER=NPIC-1000*DEVICE
      ENDIF
C
      IF (SEMMED(DEVICE,MEDIUM)) GOTO 380
C
C Could insert zero pic num processing (find free disc pic) here...
C
      IF (NUMBER .EQ. 0) GOTO 450
      IF (MEDIUM .EQ. MEDDS .AND. NUMBER .GT. NDPDS) GOTO 450
C
C None picture disc file access ?
C
      IF (MEDIUM.EQ.MEDDC.OR.MEDIUM.EQ.MEDVM) THEN
         IF (DVTYP(DEVICE).NE.FLTPIC) THEN
            ERROR = 90
            IDERR = DEVICE
            GOTO 380
         ENDIF
      ENDIF
C
C Check device level write-prot violations
C
      IF (IOP.NE.1) THEN
         IF (MEDIUM.NE.MEDDS) THEN
            IF (PROTN(DEVICE).NE.0) THEN
               IDERR = DEVICE
               GOTO 470
            ENDIF
         ENDIF
      ENDIF
C
C Establish whether picture already open (unless TEMP)
C ----------------------------------------------------
C
      IF (IOP.EQ.3) GOTO 110
      DO 70 NEWLPN=1,NLPS
         IF (DEVN(NEWLPN).NE.DEVICE) GOTO 70
         IF (PICN(NEWLPN).NE.NUMBER) GOTO 70
         IF (WHEN(NEWLPN).GT.0) GOTO 80
   70 CONTINUE
C
C No
C
      NEWLPN = -1
      GOTO 110
C
C Yes: consider re-allocating the existing LPN
C
C If OLD, just recover label and quit
C
   80 IF (IOP.EQ.1) GOTO 310
C
C If NEW, we can at least forget about it if not opened this command
C (BASEWH is set by SEMINX to value of CURRWH before any opens,
C being therefore the priority of the first pic opened this command)
C
      INUSE=BASEWH.LE.WHEN(NEWLPN)
      IF (.NOT.INUSE) THEN
         WHEN(NEWLPN)=0
         GOTO 110
      ENDIF
C
C Protected?
C
      IF (WSTAT(NEWLPN).LT.0) GOTO 470
C
C Do dims and form match?
C
      IF (NCOL.NE.NCOLS(NEWLPN)) GOTO 90
      IF (NROW.NE.NROWS(NEWLPN)) GOTO 90
      IF (NLAY.NE.NLAYS(NEWLPN)) GOTO 90
C
C Yes: if display..
C
      N=FORMN(NEWLPN)
      IF (MEDIUM.EQ.MEDDS) THEN
C
C ..fault complex/noncomplex clash
C
         IF (N.EQ.NFMCOM) THEN
            IF (FORM.NE.NFMCOM) GOTO 480
         ELSE
            IF (FORM.EQ.NFMCOM) GOTO 480
         ENDIF
C
C .. does the display scaling match?
C
         IF (GSMIN(NEWLPN).NE.VMIN) GOTO 100
         IF (GSMAX(NEWLPN).NE.VMAX) GOTO 100
      ELSE
C
C Non-display: does form match?
C
         IF (FORM.NE.N) GOTO 90
      ENDIF
C
C Re-allocate LPN for in-situ processing, constructing new label
C
      REALOC=.TRUE.
      GOTO 130
C
C Unsuitable for reallocation?
C
   90 IF (.NOT.(MEDIUM.EQ.MEDDC.OR.MEDIUM.EQ.MEDVM)) GOTO 480
C
C Disguise PCB via negative priority, allowing its survival for current
C command only
C
  100 WHEN(NEWLPN)=-1
      REQDTS=.TRUE.
C
C Find a free PCB
C ---------------
C
  110 J=CURRWH
      DO 120 I=1,NLPS
         L1=WHEN(I)
         IF (L1.GE.0 .AND. L1.LT.J) THEN
            NEWLPN=I
            J=L1
         ENDIF
  120 CONTINUE
C
C Too many opens this command?
C
      IF (BASEWH.LE.J) THEN
         ERROR = 56
         GOTO 380
      ENDIF
C
C Insert basic PCB parms
C
      DEVN(NEWLPN)=DEVICE
      PICN(NEWLPN)=NUMBER
C
C NEW mode: construct label
C -------------------------
C
      IF (IOP.EQ.1) GOTO 190
  130 PLSTYP = 1
      IF (LPN.GT.0 .AND. LPN.LE.NLPS) THEN
         IF (WHEN(LPN).NE.0) THEN
C
C Recover old label
C
            IF (SEMLAB(1,LABEL,LPN)) THEN
               GOTO 380
            ENDIF
            IF (LBLINC) THEN
               IF (LABEL(LBCLAS).EQ.NCLPLI) PLSTYP = LABEL(LBPLTY)
               GOTO 160
            ENDIF
         ENDIF
      ENDIF
C
C Construct a new label from scratch
C
      LBLINC=.TRUE.
      DO 140 I=1,6
         LABEL(I)=HEADER(I)
  140 CONTINUE
      DO 150 I=7,256
         LABEL(I)=0
  150 CONTINUE
C
C Decode reference dims,centres
C
  160 N=LBNC1
      DO 170 I=1,6
         RPARS(I)=256*LABEL(N)+LABEL(N+1)
         N=N+2
  170 CONTINUE
C
C Update dim and centre wherever former altered
C
      SPCORG=CLASS.EQ.NCLHIS.OR.CLASS.EQ.NCLPLI.OR.CLASS.EQ.NCLLUT
      IF (RNCOL.NE.NCOL) THEN
         RNCOL=NCOL
         RCCOL=NCOL/2+1
         IF (SPCORG) RCCOL=1
      ENDIF
      IF (RNROW.NE.NROW) THEN
         RNROW=NROW
         RCROW=NROW/2+1
         IF (SPCORG) RCROW=1
      ENDIF
      IF (RNLAY.NE.NLAY) THEN
         RNLAY=NLAY
         RCLAY=NLAY/2+1
         IF (SPCORG) RCLAY=1
      ENDIF
C
C Encode (possibly revised) dims,centres into label
C
      N=LBNC1
      DO 180 I=1,6
         N1=RPARS(I)/256
         LABEL(N)=N1
         LABEL(N+1)=RPARS(I)-256*N1
         N=N+2
  180 CONTINUE
C
C Update class,form,time,wp
C Position list type defaults to 1 (only if picture is position list)
C
      LABEL(LBCLAS)=CLASS
      LABEL(LBFORM)=FORM
      N=LABEL(LBYEAR+6)
      CALL MCTIME(LABEL(LBYEAR))
      LABEL(LBYEAR)=LABEL(LBYEAR)-1900
      LABEL(LBYEAR+6)=N
      LABEL(LBWP)=0
      IF (CLASS.EQ.NCLPLI) LABEL(LBPLTY) = PLSTYP
C
C If re-allocating, output label and quit
C
      IF (REALOC) GOTO 320
C
C Fill PCB from label
C
      CALL SEMOP3(LABEL,NEWLPN)
C
C Now switch on device type
C
  190 IF (MEDIUM .EQ. MEDDC .OR. MEDIUM .EQ. MEDVM) THEN
         GOTO 210
      ENDIF
C
C Medium-specific code for DISPLAY pics
C -------------------------------------
C (For all devices, the medium-specific code is omitted if an existing
C PCB is re-allocated)
C
C Fetch DPD and check partition created
C
      IF (SEMDPD(1,NUMBER)) GOTO 380
      IF (DPTYP.LT.0) THEN
         ERROR = 53
         GOTO 380
      ENDIF
      DGRAN=GPSIZ(DEVICE)
C
C OLD mode: check type 1, sampling <=1
C
      IF (IOP.EQ.1) THEN
         IF (DPTYP.EQ.0) GOTO 460
         IF (DPTYP.NE.1) THEN
            ERROR = 48
            GOTO 380
         ENDIF
         IF (DPMA.LT.REAL(DGRAN)) THEN
            ERROR = 33
            GOTO 380
         ENDIF
C
C Recover label and fill PCB from it
C
         IF (SEMLAB(1,LABEL,NEWLPN)) GOTO 380
         CALL SEMOP3(LABEL,NEWLPN)
         FORM=FORMN(NEWLPN)
C
C NEW mode: check max frame number
C
      ELSE
         IF (NLAYS(NEWLPN).GT.DPFRA2-DPFRA+1) THEN
            ERROR = 38
            GOTO 380
         ENDIF
C
C Note type and grey-scale
C
         DPTYP=1
         DPIMO=0
         DPMIN=VMIN
         DPMAX=VMAX
C
C Clear source number
C
         DPSRC=0
      ENDIF
C
C Find sampling
C
      N1=DPSIZ/DGRAN
      N2=DPSI2/DGRAN
      SAM=0
  200 SAM=SAM+1
      NP1=(NCOLS(NEWLPN)-1)/SAM+1
      IF (FORM.EQ.NFMCOM) THEN
         DPIMO=NP1*DGRAN
         NP1=2*NP1
      ENDIF
      IF (NP1.GT.N1) GOTO 200
      NP2=(NROWS(NEWLPN)-1)/SAM+1
      IF (NP2.GT.N2) GOTO 200
C
C Record sampling, frame, posn in PCB
C
      PXSAM(NEWLPN)=SAM
      FRAME1(NEWLPN)=DPFRA
      FRAME2(NEWLPN)=DPFRA2
      N1=DPTLX+(N1-NP1+1)*DGRAN/2
      XLEFT(NEWLPN)=N1
      N2=DPTLY+(N2-NP2+1)*DGRAN/2
      YTOP(NEWLPN)=N2
C
C Record grey-scale in PCB
C
      GSMIN(NEWLPN)=DPMIN
      GSMAX(NEWLPN)=DPMAX
C
C If OLD, quit now
C
      IF (IOP.EQ.1) GOTO 330
C
C Set up position map in DPD
C
      DPMA=REAL(DGRAN)/SAM
      DPMA2=-DPMA
      DPMB=N1+DPMA*(CCOLN(NEWLPN)-1)
      DPMB2=N2+DPMA2*(1-CROWN(NEWLPN))
C
C Set up left,right,top,bottom in DPD
C
      DPLEF=1-CCOLN(NEWLPN)
      DPRIG=(NCOLS(NEWLPN)-1)/SAM*SAM
      DPRIG=DPLEF+DPRIG
      DPTOP=CROWN(NEWLPN)-1
      DPBOT=(NROWS(NEWLPN)-1)/SAM*SAM
      DPBOT=DPTOP-DPBOT
C
C Return DPD to disc
C
      IF (SEMDPD(2,NUMBER)) GOTO 380
C
C Initialise display graphics
C
      IF (FSINIT(3,NPIC)) GOTO 380
C
C Erase logic
C
      IF (OPT(NERASE)) THEN
         IF (FSERAS(3,FSXMIN,FSXMAX,FSYMIN,FSYMAX)) GOTO 380
      ENDIF
C
C Output label and quit
C
      GOTO 320
C
C Medium-specific code for disc/memory pics
C -----------------------------------------
C Prepare number of blocks required
C
  210 IF (IOP.NE.1) THEN
         PSIZE=NCOL
         PSIZE=((PSIZE*LNPXL-1)/LNBLK+1)*NROW*NLAY+LABSZE
      ENDIF
C
C Fetch disc directory
C
      DIRSZE=DRSIZ(DEVICE)
      LNDIR4=DIRSZE*LNBLK
      IF (DISC(1,DEVICE,LNDIR4,SDL,I42,NFMBYT,NFMBYT)) GOTO 380
C
C Search directory to find pic location
C
      IF (IOP.EQ.3) GOTO 240
      SDPTR=1
  220 J=SDL(SDPTR)
      IF (J.LT.1000) THEN
         IF (J.EQ.NUMBER) GOTO 230
         SDPTR=SDPTR+2
         GOTO 220
      ENDIF
C
C Pic non-existent: if OLD, error..
C
      IF (IOP.EQ.1) GOTO 460
C
C ..if NEW, simply omit deletion
C
      GOTO 240
C
C Pic found: set location in PCB asap!
C
  230 ADDR(NEWLPN)=SDL(SDPTR+1)
C
C If OLD, recover label, fill PCB and quit
C
      IF (IOP.EQ.1) GOTO 310
C
C Else, implicit delete required.  Is it protected?
C
      IF (SEMLAB(1,LABEL3(1),NEWLPN)) GOTO 380
      IF (LABEL3(LBWP).NE.0) GOTO 470
C
C No: negate number in SDL, preserving for duration of current
C command only
C
      SDL(SDPTR)=-SDL(SDPTR)
      REQDTS=.TRUE.
      TPSID(DEVICE)=1
C
C Find space for new pic (smallest segment large enough)
C
  240 SDPTR2=0
C
C FREE accumulates total free space on disc
C
      FREE=0
      I4N=FLSIZ(DEVICE)+1
      SDPTR=1
  250 I=SDL(SDPTR)
      IF (I.LT.1000) THEN
         IF (I.EQ.0) THEN
            SSIZE=SDL(SDPTR+3)-SDL(SDPTR+1)
            FREE=FREE+SSIZE
            IF (SSIZE .EQ. PSIZE) THEN
C
C Exact fit found: re-use storage
C
               SDPTR2=SDPTR
               GOTO 270
            ELSE IF (PSIZE .LT. SSIZE) THEN
               IF (I4N.GE.SSIZE) THEN
                  I4N=SSIZE
                  SDPTR2=SDPTR
               ENDIF
            ENDIF
         ENDIF
C
         SDPTR=SDPTR+2
         GOTO 250
      ENDIF
C
C Directory scanned without exact fit; SDPTR indicates terminator
C and SDPTR2 indicates SD with sufficient space
C
      IF (SDPTR2.EQ.0) THEN
C
C It can't be done: inadequate total space?
C
         IF (FREE.LT.PSIZE) THEN
            ERROR = 11
         ELSE
            ERROR = 42
         ENDIF
         IDERR2 = DEVICE
C
C If temporary then try again
C
         IF (IOP .EQ. 3) THEN
            TMPDEV = DEVICE + 1
            WHEN(NEWLPN) = 0
            GOTO 40
         ENDIF
C
         GOTO 380
      ENDIF
C
C Split SDPTR to describe pic and trailing free space
C If next SD describes free space, no need to lengthen list
C
      IF (SDL(SDPTR2+2).NE.0) THEN
C
C Move all subsequent SDs down one - if there is SDL space
C
         IF (SDPTR+2.GT.DIRSZE*(LNBLK/LNINT4)/2*2) THEN
            ERROR=37
C
C If temporary then try again
C
            IF (IOP .EQ. 3) THEN
               TMPDEV = DEVICE + 1
               WHEN(NEWLPN) = 0
               GOTO 40
            ENDIF
C
            GOTO 380
         ENDIF
C
         I=SDPTR
  260    SDL(I+2)=SDL(I)
         SDL(I+3)=SDL(I+1)
         I=I-2
         IF (I.GT.SDPTR2) GOTO 260
C
C Set revised free space details in next SD
C
         SDL(SDPTR2+2)=0
      ENDIF
C
      SDL(SDPTR2+3)=SDL(SDPTR2+1)+PSIZE
C
C SDPTR2 now points to suitable SD for new pic, with the SDL
C complete except for the new pic number itself
C
  270 SDL(SDPTR2)=NUMBER
C
C Set addr in PCB
C
      ADDR(NEWLPN)=SDL(SDPTR2+1)
C
C Rewrite the directory to disc
C
      IF (DISC(2,DEVICE,LNDIR4,SDL,I42,NFMBYT,NFMBYT)) GOTO 380
C
C Output label and quit
C
      GOTO 320
C
C End of device-dependent code
C ----------------------------
C
C Recover label, fill PCB and quit
C
  310 IF (SEMLAB(1,LABEL,NEWLPN)) THEN
C
C OLD tape re-allocate goes this way, seeking label..
C
         GOTO 380
      ENDIF
C
C .. but not wanting PCB re-fill (which would alter WSTAT)
C
      CALL SEMOP3(LABEL,NEWLPN)
      GOTO 330
C
C Output label and quit
C
  320 IF (SEMLAB(2,LABEL,NEWLPN)) GOTO 380
C
C Common exit
C -----------
C If macro, check VERB legal
C
  330 IF (CLASSN(NEWLPN).NE.2) GOTO 350
      DO 340 I = 1,NMACVB
         IF (VERB.EQ.MACVBS(I)) GOTO 350
  340 CONTINUE
      ERROR = 6
      GOTO 380
C
C Supply return args
C
  350 LPN=NEWLPN
      IF (IOP.EQ.1) THEN
         NCOL=NCOLS(NEWLPN)
         NROW=NROWS(NEWLPN)
         NLAY=NLAYS(NEWLPN)
         CLASS=CLASSN(NEWLPN)
         FORM=FORMN(NEWLPN)
      ENDIF
C
C ------ MONITOR ------
C
      IF (MONIT) THEN
         IF (IOP.EQ.1) THEN
            WRITE (RECORD,360) NCOL,NROW,NLAY,CLASS,FORM
  360       FORMAT ('!-SEMOPN ncol,nrow,nlay',3I5,'  class,form',2I2)
            IF (SEMMON(RECORD,'SEMOPN',3)) GOTO 440
         ENDIF
         WRITE (RECORD,370) NEWLPN
  370    FORMAT ('!-SEMOPN lpn assigned',I3)
         IF (SEMMON(RECORD,'SEMOPN',3)) GOTO 440
      ENDIF
C
C ------ ------- ------
C
C Return now if temporary
C
      IF (IOP.EQ.3) GOTO 380
C
C If display, set DISPLA ...
C
      IF (MEDIUM.EQ.MEDDS) THEN
         IF (FSELEC()) GOTO 380
C
C ... and set viewing conditions if option VIEW is set
C
         IF (OPT(NVIEW)) THEN
            IF (FSVIEW()) GOTO 380
         ENDIF
      ENDIF
C
C If enabled, select picture and disable further selection
C
      IF (SELOPN) THEN
         CALL SEMSZZ(NPIC)
C
C Selection is unconditional for SELECT command itself
C
         IF (VERB.EQ.NSELEC) SELECT=NPIC
         SELOPN=.FALSE.
      ENDIF
C
C Enable/disable PCB via WHEN now according to whether error
C
  380 IF (ERROR .EQ. 0) THEN
         IF (IOP .EQ. 3) THEN
            WHEN(NEWLPN) = -1
         ELSE
            WHEN(NEWLPN) = CURRWH
         ENDIF
         CURRWH = CURRWH + 1
         IF (CURRWH .GT. 10000) THEN
C
C Need to slide WHEN records to prevent wraparound of integer arithmetic
C
            K = NLPS
            DO 390 I = 1,NLPS
               IF (WHEN(I) .LE. 0) THEN
                  WHTEMP(I) = WHEN(I)
                  K = K - 1
               ENDIF
  390       CONTINUE
C
C Find the nearest entry greater or equal to BASEWH
C
            L = CURRWH
            J = 1
            DO 400 I = 1,NLPS
               N = WHEN(I)
               IF (N .GT. 0 .AND. N .GE. BASEWH .AND. N .LT. L) THEN
                  L = N
                  J = I
               ENDIF
  400       CONTINUE
C
  410       IF (K .GT. 0) THEN
               N = 0
C
C Find largest remaining
C
               DO 420 I = 1,NLPS
                  IF (WHEN(I) .GT. 0 .AND. WHEN(I) .GT. N) THEN
                     N = WHEN(I)
                     L = I
                  ENDIF
  420          CONTINUE
               WHTEMP(L) = K
               WHEN(L) = 0
               K = K - 1
               GOTO 410
            ENDIF
C
C Copy new array back again
C
            DO 430 I = 1,NLPS
               WHEN(I) = WHTEMP(I)
  430       CONTINUE
C
            BASEWH = WHEN(J)
            CURRWH = NLPS + 1
         ENDIF
         SEMOPN = .FALSE.
      ELSE
C
C SEMOPN already set .TRUE. - just tidy up new LPN if used
C
         IF (NEWLPN .NE. -1) WHEN(NEWLPN) = 0
      ENDIF
  440 RETURN
C
C Error returns
C
  450 ERROR=28
      GOTO 380
  460 ERROR=30
      GOTO 380
  470 ERROR=41
      GOTO 380
  480 ERROR=39
      GOTO 380
C
C Copyright (C) 1987-1996: Synoptics Ltd, All Rights Reserved
C
      END
C
C Semper 6 system module SEMOP2
C
      LOGICAL FUNCTION SEMOP2(LABEL)
C
C Returns FALSE iff first 6 items of LABEL are chars S e m p e r
C
      INTEGER LABEL(256)
      INCLUDE 'PARAMS'
C
      SEMOP2 = LABEL(1) .NE. KUCS .OR. LABEL(2) .NE. KLCE .OR.
     +         LABEL(3) .NE. KLCM .OR. LABEL(4) .NE. KLCP .OR.
     +         LABEL(5) .NE. KLCE .OR. LABEL(6) .NE. KLCR
C
      RETURN
C
C Copyright (C) 1987-1992:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper 6 system module SEMOP3
C
      SUBROUTINE SEMOP3(LABEL,LPN)
C
C Part-fills PCB for LPN from supplied label
C
      INTEGER LABEL(256),LPN
C
      INCLUDE 'COMMON'
C
      NCOLS(LPN) = (256*LABEL(LBNC1))+LABEL(LBNC2)
      NROWS(LPN) = (256*LABEL(LBNR1))+LABEL(LBNR2)
      NLAYS(LPN) = (256*LABEL(LBNL1))+LABEL(LBNL2)
      CCOLN(LPN) = (256*LABEL(LBCC1))+LABEL(LBCC2)
      CROWN(LPN) = (256*LABEL(LBCR1))+LABEL(LBCR2)
      CLAYN(LPN) = (256*LABEL(LBCL1))+LABEL(LBCL2)
C
      CLASSN(LPN) = LABEL(LBCLAS)
      FORMN(LPN) = LABEL(LBFORM)
C
      WSTAT(LPN) = 0
      IF (LABEL(LBWP).NE.0 .OR. PROTN(DEVN(LPN)).LT.0) WSTAT(LPN) = -1
      RETURN
C
C Copyright (C) 1987-1991:  Synoptics Ltd, All Rights Reserved
C
      END
