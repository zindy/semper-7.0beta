C Semper 6.4 user signal routine SEMSIG
C
      LOGICAL FUNCTION SEMSIG(OPCODE,STRING)
      INTEGER OPCODE
      CHARACTER*(*) STRING
C
      INCLUDE 'COMMON'
C
C Opcodes are defined in PARAMS
C
C Current opcodes are:
C    SSIGBE - Semper is beginning (no environment available)
C    SSIGGO - Semper has completed initialisation and is now
C             returning to the main program
C    SSIGEX - Semper is exiting (in SEMEND)
C    SSIGIN - Semper is entering the interpreter (cancels SSIGPR)
C    SSIGPR - Semper is entering a processing routine (cancels SSIGIN)
C
C Other opcodes may be defined later
C
C STRING is used to return an error message to the caller (only valid if
C        SEMSIG returns .TRUE.
C
      IF (OPCODE .NE. SSIGBE .AND. OPCODE .NE. SSIGGO .AND.
     +    OPCODE .NE. SSIGEX .AND. OPCODE .NE. SSIGIN .AND.
     +    OPCODE .NE. SSIGPR) THEN
         STRING = 'Unknown Opcode encountered by SEMSIG'
         SEMSIG = .TRUE.
      ELSE
         SEMSIG = .FALSE.
      ENDIF
C
      RETURN
C
C Copyright (C) 1989-1993:  Synoptics Ltd,  All Rights Reserved
C
      END
