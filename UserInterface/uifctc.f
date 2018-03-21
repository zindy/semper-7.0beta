c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIFCTC ( EID, CYCLES )
c       ---------------------------------------
c
c       PARAMETERS:
c
c       integer eid : INPUT - the identifier of the tick highlighing
c                             cell whose highlight state is to be cycled
c
c       integer cycles : INPUT - the number of cycles to be performed.
c
c       Cycles the given cell, which must have tick highlighting set,
c       through its highlighting states by a given number of cycles.
c       The highlighting states of the cell are NONE, TICKED.
c       The visual appearance of the cell is only changed if the panel
c       owning the cell is showing.
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uifctc ( eid, cycles )
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
c     Loop counter
      integer i
c
c     CALLED FUNCTIONS:
c
c     Draws cell highlighting marker
      logical uixcdh
c
c     Check system is initialised
c
      if ( uifisi ) then
c
c        Check element id is valid
c
         if ( eid .ge. MINCEL .and. eid .le. MAXCEL ) then
            if ( celnam(eid) .ne. -1 ) then
               if ( celsty(eid) .eq. TICK ) then
c
c                 Go through the cycle states
c
                  do 10 i = 1, cycles
                     if ( celcyc(eid) .eq. NONE ) then
                        celcyc(eid) = TICKED
                     else if ( celcyc(eid) .eq. TICKED ) then
                        celcyc(eid) = NONE
                     endif
10                continue
c
c                 Now redraw marker if owning panel is showing
c
                  if ( paniss(celopa(eid)) ) then
                     status = uixcdh ( eid, celcyc(eid) )
                  else
c
c                    Nothing more to do
c
                     status = .FALSE.
                  endif
               else
c
c                 Error - invalid highlighting type
c
                  uiferr = BADHIG
                  status = .TRUE.
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
      uifctc = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
