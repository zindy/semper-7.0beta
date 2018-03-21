c
c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIFIJU ( HOR, VER )
c       ------------------------------------
c
c       PARAMETERS:
c
c       integer hor, ver : INPUT - justification point to be set.
c
c       Set the justification point to be used for subsequent panels and
c       elements.
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uifiju ( hor, ver )
c     ====================================
c
      integer hor, ver
c
      include 'UIFCOM'
      include 'UIXCOM'
      include 'UIFERR'
c
c     LOCAL VARIABLES:
c
c     Return status
      logical status
c
c     Check system is initialised
c
      if ( uifisi ) then
c
c        Check the positioning points given
c
         status = .TRUE.
         if ( hor .eq. LEFT .or. hor .eq. CENTRE
     +        .or. hor .eq. RIGHT ) then
            if ( hor .eq. TOP .or. hor .eq. CENTRE
     +           .or. hor .eq. BOTTOM ) then
c
c              Set the positioning point
c
               horpos = hor
               verpos = ver
               status = .FALSE.
            endif
         endif
c
c        Check it worked
c
         if ( status ) then
c
c           Error - bad positioning point
c
            uiferr = BADPPO
         endif
      else
c
c        Error - system not initialised
c
         uiferr = NOTINI
         status = .TRUE.
      endif
c
      uifiju = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
