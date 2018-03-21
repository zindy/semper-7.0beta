        logical function IsMotorola(junk)
C
C  Copyright (c) 2005 L. D. Marks
C
C  This program is free software; you can redistribute it and/or modify
C  it under the terms of the GNU General Public License as published by
C  the Free Software Foundation; either version 2 of the License, or
C  (at your option) any later version.
C
C  This program is distributed in the hope that it will be useful,
C  but WITHOUT ANY WARRANTY; without even the implied warranty of
C  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C  GNU General Public License for more details.
C
        INCLUDE 'COMMON'
        character*50 ESTRING
        integer sempbuild
        logical semcon   
        I = SEMPBUILD ( 1 , ESTRING)
        IF ( ESTRING(1:I) .EQ. 'WORDBIGE' ) THEN
C               Same as 'MM', i.e. big-endian, motorola
                IsMotorola= .TRUE.
        ELSE IF ( ESTRING(1:I) .EQ. 'WORDLITE' ) THEN
C               Same as 'II', i.e. little-endian, intel
                IsMotorola = .FALSE.
        ELSE
                IsMotorola = .FALSE.
                WRITE(IDMESS,2101) ESTRING(1:I)
 2101           FORMAT('Unrecognised Endian TAG ',A,' assuming Intel')
                IF (SEMCON(IDMESS)) GOTO 360
        ENDIF
 360    RETURN
        END
        logical function IsIntel(junk)
        logical IsMotorola
        IsIntel = .not. IsMotorola(junk)
        return
        end
        logical function IsBigendian(junk)
        logical IsMotorola
        IsBigendian = IsMotorola(junk)
        return
        end
