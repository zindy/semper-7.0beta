c
c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIFCHI ( EID, STYLE )
c       --------------------------------------
c
c       PARAMETERS:
c
c       integer eid : INPUT - the identifier of the cell whose
c                     highlighting style is to be set.
c
c       integer style : INPUT - the highlighting style to be set. Valid
c                       values: FLASH, TICK, CHECK and INVERT
c
c       Sets the highlighting style for the given cell to the given
c       type.  An error is generated if the panel owning the cell
c       is showing - it is not possible to change the highlighting
c       style of a cell when it is showing.
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uifchi ( eid, style )
c     ======================================
c
      integer eid
      integer style
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
c     CALLED FUNCTIONS:
c
c     Recalculates the size of a cell
      logical uixcsi
c
c     Check system is initialised
c
      if ( uifisi ) then
c
c        Check element id is valid
c
         if ( eid .ge. MINCEL .and. eid .le. MAXCEL
     +        .and. celnam(eid) .ge. 0 ) then
c
c           Check owning panel is not showing
c
            if ( .not. paniss(celopa(eid)) ) then
c
c              Check highlighting style is valid
c
               if ( style .eq. FLASH .or. style .eq. TICK .or.
     +              style .eq. CHECK .or. style .eq. INVERT ) then
c
c                 Set higlighting style
c
                  celsty(eid) = style
c
c                 And the current cycle state, if needed
c
                  if ( style .eq. TICK ) then
                     celcyc(eid) = NONE
                  else if ( style .eq. CHECK ) then
                     celcyc(eid) = NONE
                  else if ( style .eq. INVERT ) then
                     celcyc(eid) = NORMAL
                  endif
c
c                 And recalculate the size
c
                  status = uixcsi ( eid )
               else
c
c                 Error - bad highlighting style
c
                  uiferr = BADHIG
                  status = .TRUE.
               endif
            else
c
c              Error - can't change style of cell while its panel
c              is showing
c
               uiferr = PASHOW
               status = .TRUE.
            endif
         else
c
c           Error - invalid cell id
c
            uiferr = BADCEL
            status = .TRUE.
         endif
      else
c
c        Error - system not initialised
c
         uiferr = NOTINI
         status = .TRUE.
      endif
c
      uifchi = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
