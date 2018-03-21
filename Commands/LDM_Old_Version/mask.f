C Semper 6 processing module MASK
C
      SUBROUTINE MASK
C
C Masks the source picture with a circular mask or, if key WITH is
C set, with a polygonal mask.  All pixels outside the mask are reset
C unless option INSIDE is set, in which case all pixels inside the
C mask are reset.  A circular mask can also be made to tail off with
C a Gaussian profile if key WIDTH is set.  The reset value is obtained
C from the key VALUE, or from the mask perimeter mean, if VALUE is
C not set.  The masking process is repeated for each layer in the
C source picture.
C
      LOGICAL OPT,VARSET,INSIDE,LVALUE
C
      INCLUDE 'COMMON'
C
C Packed names
C
      INTEGER NINSID,NVALUE,NWITH
      PARAMETER (NINSID=14979,NVALUE=-3253,NWITH=-5181)
C
C See if option INSIDE is set
C
      INSIDE = OPT(NINSID)
C
C See if key VALUE is set
C
      LVALUE = VARSET(NVALUE)
      IF (VARSET(NWITH)) THEN
C
C If key WITH set, use polygonal mask
C
         CALL MASK2(INSIDE,LVALUE)
      ELSE
C
C Otherwise, use polygonal mask
C
         CALL MASK1(INSIDE,LVALUE)
      ENDIF
C
      RETURN
C
C Copyright (C) 1987,1988,1989:  Synoptics Ltd,  All Rights Reserved
C
      END
