c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIFCTE ( EID, CONS, LENGTH )
c       ---------------------------------------------
c
c       PARAMETERS:
c
c       integer eid : INPUT - the identifier of the cell whose text
c       contents are to be set.
c
c       character*(*) cons : INPUT - the text contents of the cell.
c
c       integer length : INPUT - the length of the text.
c
c       Sets the contents of the given cell to the text string given,
c       and the type of the cell to TEXT.  The contents are copied to
c       dynamic storage, and the pointer to it stored for the given
c       element id. If any contents are already stored, the space used
c       is first released.  An error is generated if the panel owning
c       the cell is showing - it is not possible to change the contents
c       of a cell when it is showing.
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uifcte ( eid, cons, length )
c     =============================================
c
      integer eid
      character*(*) cons
      integer length
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
c     Physical index of store
      integer ind
c
c     CALLED FUNCTIONS:
c
c     Allocates store
      logical allocm
c
c     Frees store
      logical freem
c
c     Gets physical index of store
      logical pindex
c
c     Copies a string
      logical USTNCP
c
c     Recalculates the size of a cell
      logical uixcsi
c
c     Recalculates the size of / redisplays a menu
      logical uixcmp
c
c     Check system is initialised
c
      if ( uifisi ) then
c
c        Check element id is valid
c
         if ( eid .ge. MINCEL .and. eid .le. MAXCEL ) then
            if ( celnam(eid) .ge. 0 ) then
c
c              Set the cell type
c
               celtyp(eid) = TEXT
c
c              Free any currently allocated space
c
               if ( celcon(eid) .gt. 0 ) then
                  status = freem ( celcon(eid) )
                  celcon(eid) = 0
                  celysi(eid) = celysi(eid) - 1
               else
                  status = .FALSE.
               endif
c
c              Allocate new space, and copy the contents in
c
               if ( .not. status ) then
                  if ( length .gt. 0 ) then
                     status = allocm ( length + 1, celcon(eid) )
                     if ( .not. status ) then
                        status = pindex ( celcon(eid), ind )
                        if ( .not. status ) then
                           status = USTNCP ( cstore(ind:),cons,length )
                        endif
                     endif
                  endif
               endif
c
c              Adjust size of the cell
c
               if ( .not. status ) then
                  status = uixcsi ( eid )
                  if ( .not. status ) then
c
c                    And if it is on a menu, adjust the size
c                    of the menu
c
                     if ( celome(eid) .ne. 0 ) then
                        status = uixcmp (eid, celome(eid), celopa(eid))
                     endif
                  endif
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
      uifcte = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
