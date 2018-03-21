c
c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIFCCY ( EID, CYCLES )
c       ---------------------------------------
c
c       PARAMETERS:
c
c       integer eid : INPUT - the identifier of the cell whose highlight
c                     state is to be cycled.
c
c       integer cycles : INPUT - the number of cycles to be performed.
c
c       Cycles the given cell through its highlighting states by a given
c       number of cycles.  The visual appearance of the cell is only
c       changed if the panel owning the cell is showing.
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uifccy ( eid, cycles )
c     =======================================
c
      integer eid, cycles
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
c     Cycles a check highlighting cell
      logical uifccc
c
c     Cycles a tick highlighting cell
      logical uifctc
c
c     Cycles a invert highlighting cell
      logical uifcic
c
c     Cycles a flash highlighting cell
      logical uifcfc
c
c     Check system is initialised
c
      if ( uifisi ) then
c
c        Check element id is valid
c
         if ( eid .ge. MINCEL .and. eid .le. MAXCEL ) then
            if ( celnam(eid) .ne. -1 ) then
c
c              Branch on the highlighting type
c
               if ( celsty(eid) .eq. CHECK ) then
                  status = uifccc ( eid, cycles )
               else if ( celsty(eid) .eq. TICK ) then
                  status = uifctc ( eid, cycles )
               else if ( celsty(eid) .eq. INVERT ) then
                  status = uifcic ( eid, cycles )
               else if ( celsty(eid) .eq. FLASH ) then
                  status = uifcfc ( eid, cycles )
               endif
            else
c
c              Error - invalid cell id
c
               uiferr = BADCEL
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
      uifccy = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
