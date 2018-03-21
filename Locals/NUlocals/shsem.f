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
      SUBROUTINE shsemp
c
C     Invokes a shell from semper
C     Requires the BSD library for the routine system
        integer*4 system,ifail
        ifail=system('sh')
      return
C
C
      end
      SUBROUTINE scomm
c
C     Executes a command from semper
C     Requires the BSD library for the routine system
        integer*4 system,ifail
        character*100 COMMAND
        logical FILSTR
      IF (FILSTR(' ',COMMAND,NF,.TRUE.)) RETURN
      ifail=system(COMMAND(1:NF))
      return
      end

      SUBROUTINE shls
c
C     Invokes a shell from semper to list directory
C     Requires the BSD library for the routine system
        integer*4 system,ifail
        ifail=system('/bin/ls')
      return
C
C
      end
C
      SUBROUTINE shpwd
c
C     Invokes a shell from semper to list directory
C     Requires the BSD library for the routine system
        integer*4 system,ifail
        ifail=system('/bin/pwd')
      return
      end
C
C
      SUBROUTINE cygsta
c
C     Invokes a shell from semper to execute cygstart
C     Requires the BSD library for the routine system
        integer*4 system,ifail
        ifail=system('/usr/bin/cygstart .')
      return
      end

