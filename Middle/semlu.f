C Semper 6 system module SEMLU
C
      LOGICAL FUNCTION SEMLU(IOP,NAME,VALUE)
C
C Provides access to variable table in SEMTAB
C
C IOP
C -1   Lookup: returns TRUE iff variable set, with VALUE=value
C  0   Unset: returns TRUE iff fixed/prot, read-only or not set
C  1   Set: sets variable with value VALUE; returns TRUE iff no room
C      or read-only
C  2   Set locally: if NAME not found between VRBLEV and LOCLEV in LOCAL
C      table then remember state, then as opcode 1
C  3   Remember: if NAME not found between VRBLEV and LOCLEV in LOCALs
C      table then remember state; returns TRUE iff read-only or no room
C      in LOCALs table
C
      INTEGER IOP,NAME,I,J
      REAL VALUE
C
      INCLUDE 'COMMON'
C
C Setup the array for accessing the variable table
C
      REAL VARTAB(NVARS)
      EQUIVALENCE (ROVVS,VARTAB)
C
      SEMLU = .FALSE.
C
C Search name list
C
      I = NSEMVE
   10 IF (VNAMES(I) .EQ. NAME) GOTO 40
      I = I - 1
      IF (I .GT. 0) GOTO 10
C
C Not found: switch code on IOP
C ---------
      IF (IOP.EQ.-1) THEN
C
C Lookup mode: return .FALSE. with value zero
C
         VALUE = 0.0
         GOTO 110
      ELSE IF (IOP .EQ. 0) THEN
C
C Unset mode: return .FALSE.
C
         GOTO 110
      ELSE IF (IOP .EQ. 2 .OR. IOP .EQ. 3) THEN
C
C Save modes: scan for existing saved entry
C
         IF (VRBLEV .LT. LOCLEV) THEN
            DO 20 I = VRBLEV+1,LOCLEV
               IF (LOCVAR(I) .EQ. NAME) GOTO 30
   20       CONTINUE
         ENDIF
C
C Not found in save table - remember it as unset
C
         IF (LOCLEV .EQ. LOCMAX) GOTO 80
         LOCLEV = LOCLEV + 1
         LOCUNS(LOCLEV) = .TRUE.
         LOCVAR(LOCLEV) = NAME
      ENDIF
C
C If only saving then return
C
   30 IF (IOP .EQ. 3) GOTO 110
C
C Set mode: any room left in table ?
C
      IF (NSEMVE .EQ. NVARS) GOTO 70
      NSEMVE = NSEMVE + 1
C
C Enter new name and value
C
      VNAMES(NSEMVE) = NAME
      VARTAB(NSEMVE) = VALUE
      GOTO 110
C
C Found: switch code on IOP
C -----
   40 IF (IOP .EQ. -1) THEN
C
C Lookup mode: return .TRUE. with value
C
         VALUE = VARTAB(I)
         GOTO 100
      ELSE IF (IOP .EQ. 0) THEN
C
C Unset mode: fault if fixed
C
         IF (I .LE. NFIXED) THEN
            ERROR = 19
            GOTO 90
         ENDIF
C
         VNAMES(I) = VNAMES(NSEMVE)
         VARTAB(I) = VARTAB(NSEMVE)
         NSEMVE = NSEMVE - 1
         GOTO 110
      ELSE
C
C Set or save mode: Fault if protected
C
         IF (I .LE. NPROT) GOTO 70
         IF (IOP .EQ. 2 .OR. IOP .EQ. 3) THEN
C
C Save modes: scan for existing saved entry
C
            IF (VRBLEV .LT. LOCLEV) THEN
               DO 50 J = VRBLEV+1,LOCLEV
                  IF (LOCVAR(J) .EQ. NAME) GOTO 60
   50          CONTINUE
            ENDIF
C
C Not found in save table - remember it
C
            IF (LOCLEV .EQ. LOCMAX) GOTO 80
            LOCLEV = LOCLEV + 1
            LOCUNS(LOCLEV) = .FALSE.
            LOCVAR(LOCLEV) = NAME
            LOCVAL(LOCLEV) = VARTAB(I)
         ENDIF
      ENDIF
C
C Set new value if not just saving state
C
   60 IF (IOP .NE. 3) VARTAB(I) = VALUE
      GOTO 110
C
C Can't set NNN
C
   70 ERROR = 13
      GOTO 90
C
C Too many saved names
C
   80 ERROR = 104
      GOTO 90
C
C Can't unset NNN
C
   90 IDERR = NAME
C
  100 SEMLU = .TRUE.
C
  110 RETURN
C
C Copyright (C) 1987-1992:  Synoptics Ltd, All Rights Reserved
C
      END
