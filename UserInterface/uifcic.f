c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIFCIC ( EID, CYCLES )
c       ---------------------------------------
c
c       PARAMETERS:
c
c       integer eid : INPUT - the identifier of the invert highlighing
c                             cell whose highlight state is to be cycled
c
c       integer cycles : INPUT - the number of cycles to be performed.
c
c       Cycles the given cell, which must have invert highlighting set,
c       through its highlighting states by a given number of cycles.
c       The highlighting states of the cell are NORMAL, INVERTED.
c       The visual appearance of the cell is only changed if the panel
c       owning the cell is showing.
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uifcic ( eid, cycles )
c     =======================================
c
      integer eid, cycles
c
      include 'UIFCOM'
      include 'UIXCOM'
      include 'UIFERR'
      include 'STORECOM'
c
c     LOCAL VARIABLES:
c
c     Return status
      logical status
c
c     Loop counter
      integer i
c
c     Position to re-write text
      integer xpos, ypos
c
c     Identifier of owning panel
      integer pid
c
c     Identifier of window of owning panel
      integer wid
c
c     Device window is located on
      integer device
c
c     Foreground and background colours of text
      integer fgc,bgc
c
c     Physical index of contents text
      integer ind
c
c     Length of contents text
      integer length
c
c     CALLED FUNCTIONS:
c
c     Gets physical index of string
      logical pindex
c
c     Finds string length
      integer USTLEN
c
c     Writes text in a window
      logical wintex
c
c     Check system is initialised
c
      if ( uifisi ) then
c
c        Check element id is valid
c
         if ( eid .ge. MINCEL .and. eid .le. MAXCEL ) then
            if ( celnam(eid) .ne. -1 ) then
               if ( celsty(eid) .eq. INVERT ) then
c
c                 Go through the cycle states
c
                  do 10 i = 1, cycles
                     if      ( celcyc(eid) .eq. NORMAL ) then
                        celcyc(eid) = INVERS
                     else if ( celcyc(eid) .eq. INVERS ) then
                        celcyc(eid) = NORMAL
                     endif
10                continue
c
c                 Now redraw the contents if owning panel is showing
c
                  if ( paniss(celopa(eid)) ) then
c
c                    But only if it has some contents!...
c
                     if ( celcon(eid) .gt. 0 ) then
                        xpos = celxpo(eid)
                        ypos = celypo(eid)
                        if ( celnam(eid) .gt. 0 .or. celbox(eid) ) then
                           xpos = xpos + 1
                           ypos = ypos + 1
                        endif
                        pid = celopa(eid)
                        wid = panwid(pid)
                        device = winode(wid)
                        fgc = celfgc(eid)
                        bgc = celbgc(eid)
                        status = pindex ( celcon(eid), ind )
                        length = USTLEN ( cstore(ind:) )
                        status = wintex ( device, wid, fgc, bgc,
     +                                    celcyc(eid), xpos, ypos,
     +                                    cstore(ind:), length )
                     else
                        status = .FALSE.
                     endif
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
      uifcic = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
