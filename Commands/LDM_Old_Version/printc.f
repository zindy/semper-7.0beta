C Semper 6 local processing module PRINTC
C
      SUBROUTINE PRINTC
C
C Provides verb PRC
C - PRC prints out contents of SEMCBS/CIP/CFG/ARG/FRL/PRI/PLI/PDB
C                                    /LDB/GDB/IOD
C
      LOGICAL ABANDN,OPT,PRINT2,SEMDPD,SEMCON
      LOGICAL LCBS,LCFG,LARG,LCIP,LFRL,LPLI,LPRI,LPDB,LLDB,LGDB,LIOD
C
C Packed names
C
      INTEGER N,NCBS,NCIP,NCFG,NARG,NFRL,NPLI,NPRI,NPDB,NLDB,NGDB,NIOD
C
      PARAMETER (NCBS=4899,NCIP=5176,NCFG=5047,NARG=2327)
      PARAMETER (NFRL=10332,NPLI=26089,NPRI=26329,NPDB=25762)
      PARAMETER (NLDB=19362,NGDB=11362,NIOD=15004)
C
      INCLUDE 'COMMON'
C
   10 FORMAT ('Dev',I3,'  med',I3,'  protn',I3)
   20 FORMAT ('        flsiz',I8,' drsiz',I6,' tpsid',I2,' dvtyp',I3)
   40 FORMAT ('        frsiz',I5,',',I5,'  nfrs',I3,'  gpsiz',I2)
   50 FORMAT ('        chsiz',I3,',',I3)
   60 FORMAT ('        dvtyp',I3,'  dvhan',I5,'  dvwid',I5)
   70 FORMAT ('        echcon/dia/log/mon/com/inp',6L2)
   80 FORMAT ('LP ',I3,'  when ',I5,'  dev',I3,'  pic ',I4,'  addr ',I8)
   90 FORMAT ('        xleft/labfg ',I5,'  ytop ',I5,'  frame1,2 ',2I4,
     +           '  wstat ',I3)
  100 FORMAT ('        ncols,nrows,nlays ',3I5,'  ccol,row,lay ',3I5)
  110 FORMAT ('        class ',I3,'  form ',I2,'  gsmin,max ',2G12.4,
     +           '  pxsam ',I3)
  120 FORMAT ('error,iderr,iderr2,i4ider ',I3,3I8)
  130 FORMAT ('loclev,vrblev,verb ',2I4,I7)
  140 FORMAT ('reqdcf/dts/fsf/drr',4L2)
  150 FORMAT ('monit',L2,'  selopn',L2,'  lblinc',L2,'  currwh',I6)
  160 FORMAT ('cbsize,cbnum ',2I8,'  ndpds',I4)
  170 FORMAT ('nluts',I4,'  lutlen,lutmax',2I5,'  lutsze',I6)
  180 FORMAT ('smgr1-5 ',5E11.3)
  190 FORMAT ('smgi1-8 ',8I7)
  200 FORMAT ('smgl1-3 ',3L2)
  210 FORMAT ('lindev,linslt ',2I4)
  220 FORMAT ('linnum,linlen ',I4,I6)
  230 FORMAT ('input,inplev ',2I4)
  240 FORMAT ('lev',I2,' dv,ln,slt',3I3,' len,nxt ',2I5,
     +                 ' for,loc',I2,I4,' trp',I5)
  250 FORMAT ('forlev ',2I4)
  260 FORMAT ('Par',I3,'  dptyp',I3)
  270 FORMAT ('        dpnum',I3,'  dpdev',I3,'  dpfra,2',2I4,
     +           '  dplut',I3)
  280 FORMAT ('        dpsiz,2',2I5,'  dptlx,y',2I5)
  290 FORMAT ('        dpmin,dpmax',2G12.4,'  dpimo',I5,'  dpsrc',I6)
  300 FORMAT ('        dpma,dpmb  ',2G12.4)
  310 FORMAT ('        dpma2,dpmb2',2G12.4)
  320 FORMAT ('        dplef,dprig',2G12.4)
  330 FORMAT ('        dpbot,dptop',2G12.4)
  340 FORMAT ('Lut',I3,'  lutmod',I2)
  350 FORMAT ('fsdev',I3,'  fsfra,fsfra2',2I4,'  fslut',I3)
  360 FORMAT ('fspar',I3,'  fsptyp',I2,'  fsi1,2',2I2,'  fsioff',G12.4)
  370 FORMAT ('fsxsca,fsysca,fsxoff,fsyoff',4G12.4)
  380 FORMAT ('fsxmin,fsxmax,fsymin,fsymax',4G12.4)
  390 FORMAT ('fsblef,fsbrig,fsbbot,fsbtop',4G12.4)
  400 FORMAT ('fsxpan,fsypan',2G12.4,'  fsmmod,fsmsiz',2I3)
  410 FORMAT ('tercr,terlf',2L2,'  terxcr,terxlf',2L2)
  420 FORMAT (A,'con/dia/log/mon/com/inp',6L2)
  430 FORMAT ('terwid,terlen',2I4,'  terasp',F8.3,
     +        '  terpro,terwra,terqui',3L2)
  440 FORMAT ('tercnt',I4)
C
      LCBS=OPT(NCBS)
      LCFG=OPT(NCFG)
      LARG=OPT(NARG)
      LCIP=OPT(NCIP)
      LFRL=OPT(NFRL)
      LPLI=OPT(NPLI)
      LPRI=OPT(NPRI)
      LPDB=OPT(NPDB)
      LLDB=OPT(NLDB)
      LGDB=OPT(NGDB)
      LIOD=OPT(NIOD)
      IF (.NOT.(LCBS.OR.LCFG.OR.LARG.OR.LCIP.OR.LFRL.OR.
     +          LPLI.OR.LPRI.OR.LPDB.OR.LLDB.OR.LGDB.OR.LIOD)) THEN
         LCBS=.TRUE.
         LCIP=.TRUE.
         LCFG=.TRUE.
         LARG=.TRUE.
         LFRL=.TRUE.
         LPLI=.TRUE.
         LPRI=.TRUE.
         LPDB=.TRUE.
         LLDB=.TRUE.
         LGDB=.TRUE.
         LIOD=.TRUE.
      ENDIF
C
C SEMCBS
C ------
C
      IF (LCBS) THEN
         IF (PRINT2('CBS')) GOTO 500
         DO 450 N=1,NDVS
            WRITE (RECORD,10) N,MEDN(N),PROTN(N)
            IF (SEMCON(RECORD)) GOTO 500
            IF (MEDN(N).EQ.MEDDC.OR.MEDN(N).EQ.MEDVM) THEN
               WRITE (RECORD,20) FLSIZ(N),DRSIZ(N),TPSID(N),DVTYP(N)
               IF (SEMCON(RECORD)) GOTO 500
            ELSE IF (MEDN(N).EQ.MEDDS) THEN
               WRITE (RECORD,40) FRSIZ(N),FRSI2(N),NFRS(N),GPSIZ(N)
               IF (SEMCON(RECORD)) GOTO 500
               WRITE (RECORD,50) CHSIZ(N),CHSI2(N)
               IF (SEMCON(RECORD)) GOTO 500
            ELSE IF (MEDN(N).EQ.MEDFL) THEN
               WRITE (RECORD,60) DVTYP(N),DVHAN(N),DVWID(N)
               IF (SEMCON(RECORD)) GOTO 500
               WRITE (RECORD,70) ECHCON(N),ECHDIA(N),ECHLOG(N),
     +                           ECHMON(N),ECHCOM(N),ECHINP(N)
               IF (SEMCON(RECORD)) GOTO 500
            ENDIF
  450    CONTINUE
C
         DO 460 N=1,NLPS
            WRITE (RECORD,80) N,WHEN(N),DEVN(N),PICN(N),ADDR(N)
            IF (SEMCON(RECORD)) GOTO 500
            WRITE (RECORD,90) XLEFT(N),YTOP(N),FRAME1(N),FRAME2(N),
     +                       WSTAT(N)
            IF (SEMCON(RECORD)) GOTO 500
            WRITE (RECORD,100) NCOLS(N),NROWS(N),NLAYS(N),
     +                       CCOLN(N),CROWN(N),CLAYN(N)
            IF (SEMCON(RECORD)) GOTO 500
            WRITE (RECORD,110) CLASSN(N),FORMN(N),GSMIN(N),GSMAX(N),
     +                       PXSAM(N)
            IF (SEMCON(RECORD)) GOTO 500
  460    CONTINUE
         IF (ABANDN(ERROR)) GOTO 500
      ENDIF
C
C SEMCIP
C ------
C
      IF (LCIP) THEN
         IF (PRINT2('CIP')) GOTO 500
         WRITE (RECORD,120) ERROR,IDERR,IDERR2,I4IDER
         IF (SEMCON(RECORD)) GOTO 500
         WRITE (RECORD,130) LOCLEV,VRBLEV,VERB
         IF (SEMCON(RECORD)) GOTO 500
         WRITE (RECORD,140) REQDCF,REQDTS,REQFSF,REQDRR
         IF (SEMCON(RECORD)) GOTO 500
         WRITE (RECORD,150) MONIT,SELOPN,LBLINC,CURRWH
         IF (SEMCON(RECORD)) GOTO 500
         IF (ABANDN(ERROR)) GOTO 500
      ENDIF
C
C SEMCFG
C ------
C
      IF (LCFG) THEN
         IF (PRINT2('CFG')) GOTO 500
         WRITE (RECORD,160) CBSIZE,CBNUM,NDPDS
         IF (SEMCON(RECORD)) GOTO 500
         WRITE (RECORD,170) NLUTS,LUTLEN,LUTMAX,LUTSZE
         IF (SEMCON(RECORD)) GOTO 500
         IF (ABANDN(ERROR)) GOTO 500
      ENDIF
C
C SEMARG
C ------
C
      IF (LARG) THEN
         IF (PRINT2('ARG')) GOTO 500
         WRITE (RECORD,180) SMGR1,SMGR2,SMGR3,SMGR4,SMGR5
         IF (SEMCON(RECORD)) GOTO 500
         WRITE (RECORD,190) SMGI1,SMGI2,SMGI3,SMGI4,SMGI5,
     +                     SMGI6,SMGI7,SMGI8
         IF (SEMCON(RECORD)) GOTO 500
         WRITE (RECORD,200) SMGL1,SMGL2,SMGL3
         IF (SEMCON(RECORD)) GOTO 500
         IF (ABANDN(ERROR)) GOTO 500
      ENDIF
C
C SEMPLI
C ------
C
      IF (LPLI) THEN
         IF (PRINT2('PLI')) GOTO 500
         WRITE (RECORD,210) LINDEV,LINSLT
         IF (SEMCON(RECORD)) GOTO 500
         WRITE (RECORD,220) LINNUM,LINLEN
         IF (SEMCON(RECORD)) GOTO 500
         IF (ABANDN(ERROR)) GOTO 500
      ENDIF
C
C SEMPRI
C ------
C
      IF (LPRI) THEN
         IF (PRINT2('PRI')) GOTO 500
         WRITE (RECORD,230) INPUT,INPLEV
         IF (SEMCON(RECORD)) GOTO 500
         N = 0
  470    IF (ABANDN(ERROR)) GOTO 500
         IF (INPLEV .GE. N) THEN
            WRITE(RECORD,240) N,INPDEV(N),INPLIN(N),INPSLT(N),INPLEN(N),
     +                        INPNXT(N),INPFOR(N),INPLOC(N),INTRAP(N)
            IF (SEMCON(RECORD)) GOTO 500
            N = N+1
            GOTO 470
         ENDIF
      ENDIF
C
C SEMFRL
C ------
C
      IF (LFRL) THEN
         IF (PRINT2('FRL')) GOTO 500
         WRITE (RECORD,250) FORLEV
         IF (SEMCON(RECORD)) GOTO 500
         IF (ABANDN(ERROR)) GOTO 500
      ENDIF
C
C SEMPDB
C ------
C
      IF (LPDB) THEN
         IF (PRINT2('PDB')) GOTO 500
         DO 480 N=1,NDPDS
            IF (SEMDPD(1,N)) GOTO 500
            WRITE (RECORD,260) N,DPTYP
            IF (SEMCON(RECORD)) GOTO 500
            IF (DPTYP.GE.0.AND.DPTYP.LE.4) THEN
               WRITE (RECORD,270) DPNUM,DPDEV,DPFRA,DPFRA2,DPLUT
               IF (SEMCON(RECORD)) GOTO 500
               WRITE (RECORD,280) DPSIZ,DPSI2,DPTLX,DPTLY
               IF (SEMCON(RECORD)) GOTO 500
            ENDIF
            IF (DPTYP.GE.1.AND.DPTYP.LE.4) THEN
               WRITE (RECORD,290) DPMIN,DPMAX,DPIMO,DPSRC
               IF (SEMCON(RECORD)) GOTO 500
            ENDIF
            IF (DPTYP.GE.1.AND.DPTYP.LE.3) THEN
               WRITE (RECORD,300) DPMA,DPMB
               IF (SEMCON(RECORD)) GOTO 500
               WRITE (RECORD,310) DPMA2,DPMB2
               IF (SEMCON(RECORD)) GOTO 500
               WRITE (RECORD,320) DPLEF,DPRIG
               IF (SEMCON(RECORD)) GOTO 500
               WRITE (RECORD,330) DPBOT,DPTOP
               IF (SEMCON(RECORD)) GOTO 500
            ENDIF
  480    CONTINUE
         IF (ABANDN(ERROR)) GOTO 500
      ENDIF
C
C SEMLDB
C ------
C
      IF (LLDB) THEN
         IF (PRINT2('LDB')) GOTO 500
         DO 490 N=1,NLUTS
            WRITE (RECORD,340) N,LUTMOD(N)
            IF (SEMCON(RECORD)) GOTO 500
  490    CONTINUE
         IF (ABANDN(ERROR)) GOTO 500
      ENDIF
C
C SEMGDB
C ------
C
      IF (LGDB) THEN
         IF (PRINT2('GDB')) GOTO 500
         WRITE (RECORD,350) FSDEV,FSFRA,FSFRA2,FSLUT
         IF (SEMCON(RECORD)) GOTO 500
         WRITE (RECORD,360) FSPAR,FSPTYP,FSI1,FSI2,FSIOFF
         IF (SEMCON(RECORD)) GOTO 500
         WRITE (RECORD,370) FSXSCA,FSYSCA,FSXOFF,FSYOFF
         IF (SEMCON(RECORD)) GOTO 500
         WRITE (RECORD,380) FSXMIN,FSXMAX,FSYMIN,FSYMAX
         IF (SEMCON(RECORD)) GOTO 500
         WRITE (RECORD,390) FSBLEF,FSBRIG,FSBBOT,FSBTOP
         IF (SEMCON(RECORD)) GOTO 500
         WRITE (RECORD,400) FSXPAN,FSYPAN,FSMMOD,FSMSIZ
         IF (SEMCON(RECORD)) GOTO 500
         IF (ABANDN(ERROR)) GOTO 500
      ENDIF
C
C SEMIOD
C ------
C
      IF (LIOD) THEN
         IF (PRINT2('IOD')) GOTO 500
         WRITE (RECORD,410) TERCR,TERLF,TERXCR,TERXLF
         IF (SEMCON(RECORD)) GOTO 500
         WRITE (RECORD,420) 'sou',SOUCON,SOUDIA,SOULOG,
     +                            SOUMON,SOUCOM,SOUINP
         IF (SEMCON(RECORD)) GOTO 500
         WRITE (RECORD,420) 'ser',SERCON,SERDIA,SERLOG,
     +                            SERMON,SERCOM,SERINP
         IF (SEMCON(RECORD)) GOTO 500
         WRITE (RECORD,430) TERWID,TERLEN,TERASP,TERPRO,TERWRA,TERQUI
         IF (SEMCON(RECORD)) GOTO 500
         WRITE (RECORD,440) TERCNT
         IF (SEMCON(RECORD)) GOTO 500
      ENDIF
C
  500 RETURN
C
C Copyright (C) 1987-1996:  Synoptics Ltd,  All Rights Reserved
C
      END
C
C Semper 6 local sub-processing module PRINT2
C
      LOGICAL FUNCTION PRINT2(S)
C
      CHARACTER*3 S
C
      LOGICAL SEMCON
C
      INCLUDE 'COMMON'
C
      PRINT2 = .TRUE.
      IF (SEMCON(' ')) GOTO 10
      RECORD = 'SEM'//S
      IF (SEMCON(RECORD(1:6))) GOTO 10
      IF (SEMCON('------')) GOTO 10
      PRINT2 = .FALSE.
   10 RETURN
C
C Copyright (C) 1989:  Synoptics Ltd,  All Rights Reserved
C
      END
