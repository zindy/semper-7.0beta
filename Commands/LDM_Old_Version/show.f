C Semper 6 processing module SHOW
C
      SUBROUTINE SHOW(SYN)
C
C Prints session status information, including error messages, system
C parameters, the variable table, assigned device table, display
C partition list, look-up table list, time of day, program file
C contents, factorisable picture sizes, file search path, usage of
C names, named macros and processing commands
C
      INTEGER SYN(*)
C
C
      REAL VAL
      LOGICAL SEMDPD,SEMFAC,SEMTPS,SEMXA1,ASSNAM
      LOGICAL SHOW1,SHOW2,SHOW6,SHOW7,SHOW8,SHOW9,SHOWSY
C     Change: LDM, July 2005
C     Ensure that gfortran does not think of range, unpack as intrinsics
      EXTERNAL RANGE, UNPACKF
      CHARACTER*3 UNPACKF
      CHARACTER*3  NAMSTR
      CHARACTER*10 MODSTR(3)
C
      INTEGER I,IT,IW,IXCEN,IYCEN,IMACRO,INAME,J,KV,M,NAME,NLOC,NV,PTR
C
      INTEGER IVAL !,IPACK
      LOGICAL ABANDN,OPT,SEMCON,SHOW3,SHOWLF,SHOWNL,SHOWNO,VARSET
C
      REAL X
      INTEGER*4 N4
      INTEGER N,N1,N2,MEDIUM,DEVICE
      LOGICAL LVAR,LDEV,LPAR,LLUT,LTIM,LPROG,LPATH,LUSAGE,LSYSTE
      LOGICAL LERROR,LSIZES,LCOMMA,LMACRO,LECHO,LALL,LYES
      LOGICAL LPAGE
C
      INTEGER KOPTS
      PARAMETER (KOPTS=15)
C
      LOGICAL LOPTS(KOPTS)
      INTEGER NOPTS(KOPTS)
C
      CHARACTER*11 DATSTR
      CHARACTER*8  TIMSTR
C
C Packed names
C
      INTEGER NDEVIC,NVARIA,NPARTI,NSYSTE,NERROR,NSIZES,NCOMMA,NLUT
      INTEGER NMACRO,NTIME,NPROGR,NECHO,NPAGE,NPATH,NUSAGE
      PARAMETER (NDEVIC=6622,NVARIA=-3259,NPARTI=25658,NECHO=8128)
      PARAMETER (NSYSTE=31419,NERROR=8738,NSIZES=30786,NCOMMA=5413)
      PARAMETER (NMACRO=20843,NTIME=-374,NPAGE=25647)
      PARAMETER (NLUT=20060,NPROGR=26335,NPATH=25660,NUSAGE=-2362)
C
      EQUIVALENCE (LOPTS(1),LVAR)
      EQUIVALENCE (LOPTS(2),LDEV)
      EQUIVALENCE (LOPTS(3),LPAR)
      EQUIVALENCE (LOPTS(4),LLUT)
      EQUIVALENCE (LOPTS(5),LTIM)
      EQUIVALENCE (LOPTS(6),LPROG)
      EQUIVALENCE (LOPTS(7),LSYSTE)
      EQUIVALENCE (LOPTS(8),LSIZES)
      EQUIVALENCE (LOPTS(9),LCOMMA)
      EQUIVALENCE (LOPTS(10),LMACRO)
      EQUIVALENCE (LOPTS(11),LECHO)
      EQUIVALENCE (LOPTS(12),LERROR)
      EQUIVALENCE (LOPTS(13),LPATH)
      EQUIVALENCE (LOPTS(14),LUSAGE)
      EQUIVALENCE (LOPTS(15),LPAGE)
C
      INCLUDE 'COMMON'
C
      INTEGER NAMES(LNBUF/LNINT)
      INTEGER DATIME(7),TEXT(FILMAX+1),IB1(256),FACTOR(9)
C
      EQUIVALENCE (RB1,IB1,TEXT),(RB2,NAMES,FACTOR),(RB1LHS,DATIME)
C
      DATA NOPTS / NVARIA,NDEVIC,NPARTI,NLUT,NTIME,NPROGR,NSYSTE,
     +             NSIZES,NCOMMA,NMACRO,NECHO,NERROR,NPATH,
     +             NUSAGE,NPAGE
     +           /
C
C
      DATA MODSTR /'monochrome','false','colour'/
C
C Establish options
C
      NLOC = 0
C
      LYES = .FALSE.
      DO 10 N = 1,KOPTS
         LALL = OPT(NOPTS(N))
         LOPTS(N) = LALL
         IF (LALL) THEN
            LYES = .TRUE.
C
            NLOC = NLOC + 1
C
         ENDIF
   10 CONTINUE
C
      IF (.NOT.LYES) THEN
C
         IF (SEMCON('Options are:')) GOTO 280
         IF (SEMCON(
     +      ' commands, devices, echo, errors, luts, macros,')) GOTO 280
         IF (SEMCON(
     +      ' page, partitions, path, programs, sizes,')) GOTO 280
         IF (SEMCON(' system, time, usage, variables.')) GOTO 280
C
         GOTO 280
      ENDIF
C
C Note first,last dev/part/lut numbers
C
      IF (VARSET(-12441)) THEN
         N1=IVAL(-12441)
         IF (VARSET(-12473)) THEN
            N2=IVAL(-12473)
         ELSE
            N2=N1
         ENDIF
         LALL=.FALSE.
      ELSE
         N1=0
         N2=0
         LALL=.TRUE.
      ENDIF
C
C Code for USAGE option
C ---------------------
C
      IF (LUSAGE) THEN
         IF (SHOW9(SYN)) GOTO 280
      ENDIF
C
C Code for PATH option
C --------------------
C
      IF (SHOW1(LPATH)) GOTO 280
C
C Code for ECHO status option
C ---------------------------
C
      IF (LECHO) THEN
         IF (SHOW7(.TRUE.)) GOTO 280
      ENDIF
C
C Code for terminal PAGE settings
C -------------------------------
C
      IF (LPAGE) THEN
         IF (SHOW7(.FALSE.)) GOTO 280
      ENDIF
C
C Code for VARIABLE table option
C ------------------------------
C
      IF (LVAR) THEN
         IF (SHOWNL('Variables set:')) GOTO 280
C
C Assemble list of variable names
C
         N = 0
C
         DO 20 M = 1,NSEMVE
            NAME = VNAMES(M)
            IF (.NOT.(NAME.EQ.NVARIA.OR.NAME.EQ.-11201)) THEN
               N = N + 1
               NAMES(N) = NAME
            ENDIF
   20    CONTINUE
C
C Sort list of names
C
         CALL SHOW5(NAMES,N)
C
C Print list of names and values
C
         IF (SEMTPS(IW,IT)) GOTO 280
C
C Get at least one item per line ?
C
         IF (IW .LT. 20) IW = 20
C
C Limit to the size of our string
C
         IF (IW .GT. RECLEN) IW = RECLEN
         IT = IW / 18
         IW = IT - 1
C
         DO 50 I = 1,N,IT
            RECORD = ' '
C
            M = 2
            DO 40 J = I,MIN(I+IW,N)
C
C Unpack name and value
C
               INAME = NAMES(J)
               RECORD(M+1:M+3) = UNPACKF(INAME)
C
               PTR = 1
               IF (SEMXA1(4,TEXT,80,PTR,VAL(INAME),INAME)) GOTO 30
   30          PTR = PTR - 1
C
               CALL SEMCHS(RECORD(M+6:M+PTR+5),TEXT,PTR)
C
               M = M + 18
   40       CONTINUE
C
            IF (SEMCON(RECORD(1:M))) GOTO 280
            IF (ABANDN(ERROR)) GOTO 280
   50    CONTINUE
         IF (SHOWLF()) GOTO 280
C
C Summarise variable usage
C
         IF (LOCLEV-1 .GT. NLOC) THEN
            WRITE (RECORD,60) 'Local',LOCLEV-1-NLOC,LOCMAX
   60       FORMAT(A,' variable slots in use:',I4,' of',I4)
            IF (SEMCON(RECORD(1:39))) GOTO 280
            IF (ABANDN(ERROR)) GOTO 280
         ENDIF
         WRITE (RECORD,60) 'Total',NSEMVE-NLOC,NVARS
         IF (SEMCON(RECORD(1:39))) GOTO 280
         IF (ABANDN(ERROR)) GOTO 280
      ENDIF
C
C Code for DEVICE table option
C ----------------------------
C
      IF (LDEV .AND. (LALL .OR. .NOT.LPROG)) THEN
         LYES = .FALSE.
         DO 140 DEVICE=0,NDVS
            IF (LALL) THEN
               IF (DEVICE.EQ.0) GOTO 140
            ELSE
               IF (DEVICE.LT.N1.OR.DEVICE.GT.N2) GOTO 140
            ENDIF
            IF (ABANDN(ERROR)) GOTO 280
            IF (DEVICE.NE.0) THEN
               MEDIUM = MEDN(DEVICE)
               IF (MEDIUM.EQ.0) GOTO 140
            ENDIF
C
C Header
C
            IF (.NOT.LYES) THEN
               IF (SHOWNL('Devices assigned:')) GOTO 280
               LYES = .TRUE.
            ENDIF
C
C Prepare name (unless display or memory)
C
            N=0
            IF (DEVICE .NE. 0) THEN
               IF (.NOT.(MEDIUM.EQ.MEDDS.OR.MEDIUM.EQ.MEDVM)) THEN
                  IF (ASSNAM(1,DEVICE,TEXT)) GOTO 280
                  N=TEXT(1)
               ENDIF
C
C Encode WP status
C
               IF (PROTN(DEVICE).NE.0) THEN
                  N=N+3
                  TEXT(N-1)=KSPACE
                  TEXT(N)=KLCW
                  IF (MEDIUM.EQ.MEDDS) THEN
                     TEXT(N+1)=KLCO
                  ELSE
                     TEXT(N+1)=KLCP
                  ENDIF
               ENDIF
            ENDIF
C
C Print details
C
            IF (DEVICE .EQ. 0) THEN
C
C Workdisc
C
               IF (SEMCON('  0 Work memory')) GOTO 280
            ELSE IF (MEDIUM .EQ. MEDDC .OR. MEDIUM .EQ. MEDVM) THEN
C
C Disc/memory
C
               N4=FLSIZ(DEVICE)
               X=N4
               X=X*REAL(LNBLK)/1024.
               IF (DVTYP(DEVICE) .EQ. FLTPIC) THEN
                  IF (MEDIUM .EQ. MEDVM) THEN
                     IF (SHOW3(DEVICE,'Picture Memory',
     +                         TEXT(2),N)) GOTO 280
                  ELSE
                     IF (DVSCR(DEVICE) .EQ. 0) THEN
                        IF (SHOW3(DEVICE,'Picture Disc',
     +                            TEXT(2),N)) GOTO 280
                     ELSE
                        IF (SHOW3(DEVICE,'Temporary Picture Disc',
     +                            TEXT(2),N)) GOTO 280
                     ENDIF
                  ENDIF
                  N=DRSIZ(DEVICE)*LNBLK/LNINT4/2
                  IF (SEMTPS(IW,IT)) GOTO 280
                  IF (IW .LT. 72) THEN
C ****** WOS ******
C Size printed in MB not KB, allowing files >1GB
                     WRITE (RECORD,70) X/1000.,N4
   70                FORMAT (7X,'Size:',F8.2,'MB =',I9,' blocks',:,
     +                       '   Directory size',I5,' slots')
                     IF (SEMCON(RECORD)) GOTO 280
                     WRITE(RECORD,80) N
   80                FORMAT (7X,'Directory size',I5,' slots')
                  ELSE
                     WRITE (RECORD,70) X/1000.,N4,N
                  ENDIF
                  IF (SEMCON(RECORD)) GOTO 280
               ELSE IF (DVTYP(DEVICE) .EQ. FLTHEL) THEN
C
C Help library
C
                  IF (MEDIUM .EQ. MEDVM) THEN
                     IF (SHOW3(DEVICE,'Help Memory',
     +                         TEXT(2),N)) GOTO 280
                  ELSE
                     IF (DVSCR(DEVICE) .EQ. 0) THEN
                        IF (SHOW3(DEVICE,'Help Library',
     +                            TEXT(2),N)) GOTO 280
                     ELSE
                        IF (SHOW3(DEVICE,'Temporary Help Library',
     +                            TEXT(2),N)) GOTO 280
                     ENDIF
                  ENDIF
                  N=DRSIZ(DEVICE)
                  IF (SEMTPS(IW,IT)) GOTO 280
                  IF (IW .LT. 72) THEN
C ****** WOS change: size in MB not KB
                     WRITE (RECORD,70) X/1000.,N4
                     IF (SEMCON(RECORD)) GOTO 280
                     WRITE(RECORD,80) N
                  ELSE
C ****** WOS change: size in MB not KB
                     WRITE (RECORD,70) X/1000.,N4,N
                  ENDIF
                  IF (SEMCON(RECORD)) GOTO 280
               ELSE IF (DVTYP(DEVICE) .EQ. FLTRUN) THEN
C
C Program/Run library
C
                  IF (MEDIUM .EQ. MEDVM) THEN
                     IF (SHOW3(DEVICE,'Program Memory',
     +                         TEXT(2),N)) GOTO 280
                  ELSE
                     IF (DVSCR(DEVICE) .EQ. 0) THEN
                        IF (SHOW3(DEVICE,'Program Library',
     +                            TEXT(2),N)) GOTO 280
                     ELSE
                        IF (SHOW3(DEVICE,'Temporary Program Library',
     +                            TEXT(2),N)) GOTO 280
                     ENDIF
                  ENDIF
                  N=DRSIZ(DEVICE)/PRCSZE - 3
                  IF (SEMTPS(IW,IT)) GOTO 280
                  IF (IW .LT. 72) THEN
C ****** WOS change: size in MB not KB
                     WRITE (RECORD,70) X/1000.,N4
                     IF (SEMCON(RECORD)) GOTO 280
                     WRITE(RECORD,80) N
                  ELSE
C ****** WOS change: size in MB not KB
                     WRITE (RECORD,70) X/1000.,N4,N
                  ENDIF
                  IF (SEMCON(RECORD)) GOTO 280
               ENDIF
            ELSE IF (MEDIUM .EQ. MEDFL) THEN
C
C Text file
C
               IF (SHOW3(DEVICE,'Text file',TEXT(2),N))
     +            GOTO 280
               WRITE (RECORD,90) DVWID(DEVICE)
   90          FORMAT (7X,'Maximum record length:',I5)
               IF (SEMCON(RECORD)) GOTO 280
            ELSE IF (MEDIUM .EQ. MEDDS) THEN
C
C Display
C
               IF (SHOW3(DEVICE,'Display',TEXT(2),N)) GOTO 280
               IF (SEMTPS(IW,IT)) GOTO 280
               IF (IW .LT. 72) THEN
                  WRITE (RECORD,110) NFRS(DEVICE),
     +                               FRSIZ(DEVICE),FRSI2(DEVICE)
  110             FORMAT (7X,'Number of frames:',I5,'  Frame size:',I5,
     +                    ',',I4,:,'  Monitor size:',I5,',',I4)
                  IF (SEMCON(RECORD)) GOTO 280
                  WRITE (RECORD,120) MONSIZ(DEVICE),MONSI2(DEVICE)
  120             FORMAT (7X,'  Monitor size:',I5,',',I4)
               ELSE
                  WRITE (RECORD,110) NFRS(DEVICE),
     +                               FRSIZ(DEVICE),FRSI2(DEVICE),
     +                               MONSIZ(DEVICE),MONSI2(DEVICE)
               ENDIF
               IF (SEMCON(RECORD)) GOTO 280
               WRITE (RECORD,130) CHSIZ(DEVICE),CHSI2(DEVICE),
     +                            GPSIZ(DEVICE)
  130          FORMAT (7X,'Charactersize:',I3,',',I2,
     +                 '  Greypixelsize:',I2)
               IF (SEMCON(RECORD)) GOTO 280
            ENDIF
C
  140    CONTINUE
C
C None found message
C
         IF (.NOT.LYES) THEN
            IF (SHOWNO('device')) GOTO 280
         ENDIF
      ENDIF
C
C Code for display PARTITION option
C ---------------------------------
C
      IF (LPAR) THEN
         LYES = .FALSE.
C
C Catch all for dis:n for now
C
         N1 = MOD(N1,1000)
         N2 = MOD(N2,1000)
C
         DO 170 N=1,NDPDS
            IF (.NOT.LALL) THEN
               IF (N.LT.N1.OR.N.GT.N2) GOTO 170
            ENDIF
            IF (ABANDN(ERROR)) GOTO 280
            IF (SEMDPD(1,N)) GOTO 280
            IF (DPTYP.GE.0) THEN
               IF (.NOT.LYES) THEN
                  IF (SHOWNL('Display partitions:')) GOTO 280
                  LYES = .TRUE.
               ENDIF
               IXCEN=(DPTLX+DPSIZ/2)-FRSIZ(DPDEV)/2
               IYCEN=FRSI2(DPDEV)/2-(DPTLY+DPSI2/2)
               IF (SEMTPS(IW,IT)) GOTO 280
               IF (IW .LT. 72) THEN
                  WRITE (RECORD,150) N,DPSIZ,DPSI2,IXCEN,IYCEN
  150             FORMAT(I3,2X,'Size:',I5,',',I5,4X,'Centre:',I5,',',I5,
     +                   :,4X,'Frames:',I5,',',I5,4X,'Lut:',I3)
                  IF (SEMCON(RECORD)) GOTO 280
                  WRITE (RECORD,160) DPFRA,DPFRA2,DPLUT
  160             FORMAT(5X,'Frames:',I5,',',I5,4X,'Lut:',I3)
               ELSE
                  WRITE (RECORD,150) N,DPSIZ,DPSI2,IXCEN,IYCEN,
     +                               DPFRA,DPFRA2,DPLUT
               ENDIF
               IF (SEMCON(RECORD)) GOTO 280
            ENDIF
  170    CONTINUE
C
C None found message
C
         IF (.NOT.LYES) THEN
            IF (SHOWNO('display partition')) GOTO 280
         ENDIF
      ENDIF
C
C Code for LUT option
C -------------------
C
      IF (LLUT) THEN
         LYES = .FALSE.
         DO 190 N=1,NLUTS
            IF (.NOT.LALL) THEN
               IF (N.LT.N1.OR.N.GT.N2) GOTO 190
            ENDIF
            IF (ABANDN(ERROR)) GOTO 280
            IF (LUTMOD(N).NE.0) THEN
               IF (.NOT.LYES) THEN
                  IF (SHOWNL('Display look-up tables:')) GOTO 280
                  LYES = .TRUE.
               ENDIF
               WRITE (RECORD,180) N,MODSTR(LUTMOD(N))
  180          FORMAT (I3,2X,'Mode:',1X,A10)
               IF (SEMCON(RECORD)) GOTO 280
            ENDIF
  190    CONTINUE
C
C None found message
C
         IF (.NOT.LYES) THEN
            IF (SHOWNO('display look-up table')) GOTO 280
         ENDIF
      ENDIF
C
C Code for option TIME
C --------------------
C
      IF (LTIM) THEN
         CALL MCTIME(DATIME)
         RECORD = 'Date and time: dd-mmm-yyyy hh:mm:ss'
         RECORD(16:26)=DATSTR(DATIME(3),DATIME(2),DATIME(1))
         RECORD(28:35)=TIMSTR(DATIME(4),DATIME(5),DATIME(6))
         IF (SHOWNL(RECORD(1:35))) GOTO 280
      ENDIF
C
C Code for option PROGRAM
C -----------------------
C
      IF (LPROG) THEN
         LYES = .FALSE.
         IF (PTNUMB .NE. 0) THEN
            DO 200 I = 1,PTNUMB
               DEVICE = PTPRIO(I)
               IDERR = DEVICE
               IF (ABANDN(ERROR)) GOTO 280
C
C Device medium and type should be correct, but check it anyhow
C
               MEDIUM = MEDN(DEVICE)
               IF (MEDIUM.EQ.MEDDC.OR.MEDIUM.EQ.MEDVM) THEN
                  IF (DVTYP(DEVICE) .EQ. FLTRUN) THEN
                     IF (LALL .OR.
     +                   (DEVICE .GE. N1 .AND. DEVICE .LE. N2)) THEN
                         IF (.NOT.LYES) THEN
                            IF (SHOWNL('Programs:')) GOTO 280
                            LYES = .TRUE.
                         ENDIF
                         IF (SHOW2(DEVICE,OPT(10452))) GOTO 280
                     ENDIF
                  ELSE
                     ERROR = 35
                     GOTO 280
                  ENDIF
               ELSE
                  ERROR = 29
                  GOTO 280
               ENDIF
  200       CONTINUE
         ENDIF
         IF (LYES) THEN
            IF (LALL) THEN
               IF (PTNUMB .GT. 1) THEN
                  WRITE (RECORD,210) (PTPRIO(I),I=1,PTNUMB)
  210             FORMAT ('Device search order: ',9(I2,:,','))
                  IF (SHOWNL(RECORD)) GOTO 280
               ENDIF
            ENDIF
         ELSE
            IF (SHOWNO('program')) GOTO 280
         ENDIF
      ENDIF
C
C Code for option SYSTEM
C ----------------------
C
      IF (LSYSTE) THEN
         IF (SHOWSY()) GOTO 280
      ENDIF
C
C Code for option SIZES
C ---------------------
C
      IF (LSIZES) THEN
         PTR = 0
         DO 220 I = 4,LNBUF,4
            IF (.NOT.SEMFAC(I,FACTOR,NV)) THEN
               PTR = PTR + 1
               IB1(PTR) = I
            ENDIF
  220    CONTINUE
         IF (SHOWNL('Factorisable picture sizes:')) GOTO 280
C
         IF (SEMTPS(IW,IT)) GOTO 280
C
C Get at least one item per line ?
C
         IF (IW .LT. 6) IW = 6
C
C Limit to the size of our string
C
         IF (IW .GT. RECLEN) IW = RECLEN
         IT = IW / 6
         IW = IT - 1
C
         DO 240 I = 1,PTR,IT
  230       FORMAT (40I6)
            WRITE (RECORD,230) (IB1(J),J=I,MIN(I+IW,PTR))
            IF (SEMCON(RECORD)) GOTO 280
  240    CONTINUE
C
      ENDIF
C
C Code for option COMMANDS
C ------------------------
C
      IF (LCOMMA) THEN
         KV=5
         N=0
C
C Assemble list of command names
C
  250    INAME=SYN(KV)
C
C Double test to avoid possible overflow with 16 bit integers
C
         IF (INAME.LE.0) THEN
            IF (INAME.LE.-20000) THEN
               KV=INAME+25000
               GOTO 250
            ENDIF
         ENDIF
         KV = KV + 1
C
         NAMSTR=UNPACKF(INAME)
C
C Skip names beginning with $
C
         IF (NAMSTR(1:1).EQ.'$') GOTO 250
C
C END found?
C
         N = N + 1
         NAMES(N) = INAME
         IF (NAMSTR.NE.'end') GOTO 250
C
         WRITE (RECORD,260) N
  260    FORMAT ('Processing commands - total number:',I4)
         IF (SHOWNL(RECORD(1:39))) GOTO 280
C
C Sort list of command names and print
C
         IF (SHOW8(NAMES,N)) GOTO 280
      ENDIF
C
C Code for option MACRO
C ---------------------
C
      IF (LMACRO) THEN
         IF (SHOWNL('Named macro texts:')) GOTO 280
         KV=SYN(4)
         IF (KV.NE.0) THEN
            RECORD='@'
  270       IMACRO=SYN(KV)
            IF (IMACRO.NE.0) THEN
               RECORD(2:4)=UNPACKF(IMACRO)
               N=SYN(KV+1)
               CALL SEMCHS(RECORD(6:),SYN(KV+2),N)
               IF (SEMCON(RECORD(1:N+5))) GOTO 280
               KV=KV+2+N
               GOTO 270
            ENDIF
         ENDIF
      ENDIF
C
C Code for option ERROR
C ---------------------
C
      IF (LERROR) THEN
         IF (SHOW6(LALL,N1,N2)) GOTO 280
      ENDIF
C
C
      IF (SHOWLF()) GOTO 280
C
  280 RETURN
C
C Copyright (C) 1987-1996: Synoptics Ltd,  All Rights Reserved
C
      END
