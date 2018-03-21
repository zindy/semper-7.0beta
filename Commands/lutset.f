C Semper 6 processing module LUTSET
C
      SUBROUTINE LUTSET
C
C Verb descriptor required:
C Lset :LUTSET $1=clu number=$1 scaled= range= ra2= hue=0 hu2=hue +
C   saturation=0 sa2=sat brightness=0 br2=bri red green blue
C
C Functions called
C
      REAL VAL
      INTEGER IVAL,IPACK
      LOGICAL FSLU61,SEMLUT,VARSET,SEMDPD,SEMDPN,LUTAD2,OPT,SEMDIA
C
C Local variables
C
      REAL H1,H2,DH,S1,S2,DS,V1,V2,DV
      INTEGER MODE
      INTEGER RAN1,RAN2,DEVICE,PARTN,ILUT
      LOGICAL LSCALE,LRED,LGREEN,LBLUE
C
      INCLUDE 'COMMON'
C
      INTEGER RGBLUT(0:3*LUTSIZ-1)
      REAL    HSVLUT(0:3*LUTSIZ-1)
      EQUIVALENCE (RGBLUT,RB1),(HSVLUT,RB2)
C
C Parameters
C Lut types
C
      INTEGER PMONO,PFALSE,PFULL
      PARAMETER (PMONO=1,PFALSE=2,PFULL=3)
C
C Packed names
C
      INTEGER NSCA,NRAN,NRA2,NBRI,NBR2,NHUE,NHU2,NSAT,NSA2
      INTEGER NRED,NGRE,NBLU
      PARAMETER (NSCA=30521,NRAN=28854,NRA2=28872)
      PARAMETER (NBRI=3929,NBR2=3952,NHUE=13645,NHU2=13672)
      PARAMETER (NSAT=30460,NSA2=30472)
      PARAMETER (NRED=29004,NGRE=11925,NBLU=3701)
C
C ---------------------------------------------------------------------
C Process key SCALED
C
      LSCALE = VARSET(NSCA)
      IF (LSCALE) THEN
C
C See if key SCALED is set to valid display partition number
C
         IF (SEMDPN(IVAL(NSCA),DEVICE,PARTN)) GOTO 10
C
C Fetch required DPD from work disc
C
         IF (SEMDPD(1,PARTN)) GOTO 10
C
C Fault display partition not containing a 2-D picture
C
         IF (DPTYP.NE.1) THEN
            ERROR=48
            IDERR=1000*DEVICE+PARTN
            GOTO 10
         ENDIF
      ENDIF
C
C Process keys RANGE,RA2
C
      IF (LUTAD2(NRAN,NRA2,RAN1,RAN2)) GOTO 10
C
      H1 = VAL(NHUE)/360.0
      H2 = VAL(NHU2)/360.0
      S1 = VAL(NSAT)
      S2 = VAL(NSA2)
      V1 = VAL(NBRI)
      V2 = VAL(NBR2)
C
C ---------------------------------------------------------------------
C Fetch look-up table number, then the table itself
C
      ILUT=IVAL(23253)
C
C Get the current Lut
C
      IF (SEMLUT(1,ILUT,MODE,RGBLUT)) GOTO 10
C
C Fault non-existent LUT
C
      IF (MODE.EQ.0) THEN
         ERROR=69
         IDERR=ILUT
         GOTO 10
      ELSE IF (MODE.EQ.PFULL) THEN
C
C Full colour. Assume adjustments applied to all colours if none given
C
         LRED   = OPT(NRED)
         LGREEN = OPT(NGRE)
         LBLUE  = OPT(NBLU)
         IF (.NOT.LRED .AND. .NOT.LGREEN .AND. .NOT.LBLUE) THEN
            LRED   = .TRUE.
            LGREEN = .TRUE.
            LBLUE  = .TRUE.
         ENDIF
      ENDIF
C
C If range is more than one pixel, need to determine rate of change
C
      IF (RAN1.NE.RAN2) THEN
         DH = H2-H1
         DS = S2-S1
         DV = V2-V1
      ENDIF
C
C ---------------------------------------------------------------------
C Warn if setting colour parameters on monochrome or full colour lut
C
      IF ((MODE.EQ.PMONO .OR. MODE.EQ.PFULL) .AND.
     +   (H1.NE.0.0 .OR. H2.NE.0.0 .OR. S1.NE.0.0 .OR. S2.NE.0.0)) THEN
         IF (SEMDIA(
     +      'Warning: Setting Hue or Saturation has no effect on',
     +      NDIWAR)) GOTO 10
         IF (SEMDIA(
     +      '         monochrome or full-colour look-up tables',
     +      NDIWAR)) GOTO 10
C
C Warn if Saturation is zero and Hue is non-zero
C
      ELSE IF ((H1.NE.0.0 .OR. H2.NE.0.0) .AND.
     +        (S1.EQ.0.0 .AND. S2.EQ.0.0)) THEN
         IF (SEMDIA(
     + 'Warning: Hue has no effect on colour whilst Saturation is zero',
     +      NDIWAR)) GOTO 10
      ENDIF
C
C ---------------------------------------------------------------------
C Set each parameter over the specified range
C
      CALL LUTADL(HSVLUT(0)       ,RAN1,RAN2,H1,DH)
      CALL LUTADL(HSVLUT(LUTLEN)  ,RAN1,RAN2,S1,DS)
      CALL LUTADL(HSVLUT(LUTLEN*2),RAN1,RAN2,V1,DV)
C
C Convert to RGB space over the required range
C
      IF (MODE.EQ.PMONO) THEN
C
C Monochrome
C
         CALL LUTADV(HSVLUT(LUTLEN*2),RGBLUT,RAN1,RAN2)
      ELSE IF (MODE.EQ.PFALSE) THEN
C
C Pseudo-colour: Convert entire Lut to RGB space
C
         CALL HSVRGB(HSVLUT,RGBLUT,RAN1,RAN2)
      ELSE IF (MODE.EQ.PFULL) THEN
C
C Full colour
C
         IF (LRED) THEN
            CALL LUTADV(HSVLUT(LUTLEN*2),RGBLUT(0)       ,RAN1,RAN2)
         ENDIF
         IF (LGREEN) THEN
            CALL LUTADV(HSVLUT(LUTLEN*2),RGBLUT(LUTLEN)  ,RAN1,RAN2)
         ENDIF
         IF (LBLUE) THEN
            CALL LUTADV(HSVLUT(LUTLEN*2),RGBLUT(LUTLEN*2),RAN1,RAN2)
         ENDIF
      ENDIF
C
C ---------------------------------------------------------------------
C Update look-up table values on work disc
C
      IF (SEMLUT(2,ILUT,MODE,RGBLUT)) GOTO 10
C
C Update look-up table in framestore
C
      IF (FSLU61(2,ILUT,MODE,RGBLUT,ERROR)) GOTO 10
C
   10 RETURN
C
C Copyright (C) 1989-1993:  Synoptics Ltd,  All Rights Reserved
C
      END
