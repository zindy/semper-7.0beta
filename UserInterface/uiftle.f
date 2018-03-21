c
c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIFTLE ( EID, LENGTH )
c       ---------------------------------------
c
c       PARAMETERS:
c
c       integer eid : INPUT - the identifier of the textfield element
c                     whose length is to be changed.
c
c       integer length : INPUT - the new length of the textfield, in
c                        characters.
c
c       Sets the length of the given textfield to the given value.
c       The current dynamic store for the contents is freed, a new area
c       of the correct length allocated, and the contents copied into
c       it.  If the new length is shorter than the current length, the
c       contents are truncated (if necessary) to fit the new length.
c       An error will be generated if the panel owning the textfield is
c       showing since the length of a showing textfield cannot be
c       changed.
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uiftle ( eid, length )
c     =======================================
c
      integer eid
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
c     Logical index of new area of store
      integer newsto
c
c     Physical index of old and new store
      integer froind, toind
c
c     Loop counters
      integer i, j
c
c     Position from which to fill string with spaces
      integer start
c
c     Identifier of panel owning textfield
      integer pid
c
c     CALLED FUNCTIONS:
c
c     Gets physical index of store
      logical pindex
c
c     Allocates memory
      logical allocm
c
c     Frees memory
      logical freem
c
c     Copies a string
      logical USTNCP
c
c     Resizes the textfield
      logical uixtsi
c
c     Destroys a window
      logical windes
c
c     Check system is initialised
c
      if ( uifisi ) then
c
c        Check textfield id is valid
c
         if ( eid .ge. MINTEX .and. eid .le. MAXTEX ) then
            if ( texnam(eid) .ge. 0 ) then
c
c              Check panel is not showing
c
               if ( .not. paniss(texopa(eid)) ) then
c
c                 Check length is sensible
c
                  if ( length .gt. 0 ) then
c
c                    Allocate new bit of store
c
                     status = allocm ( length + 1, newsto )
                     if ( .not. status ) then
c
c                       And copy over current contents
c
                        status = pindex ( newsto, toind )
                        if ( .not. status ) then
                           if ( texcon(eid) .gt. 0 ) then
                              status = pindex ( texcon(eid), froind )
                              if ( .not. status ) then
                                 status = USTNCP ( cstore(toind:),
     +                                        cstore(froind:), length )
                                 if ( .not. status ) then
c
c                                   Free up existing store for contents
c
                                    status = freem ( texcon(eid) )
                                    if ( .not. status ) then
                                       texcon(eid) = 0
                                       start = texcle(eid) + 1
                                    endif
                                 endif
                              endif
                           else
                              status = .FALSE.
                              start = 1
                           endif
c
c                          Ok, assign new store to contents
c
                           if ( .not. status ) then
                              texcon(eid) = newsto
                              texlen(eid) = length
c
c                             And pad to end of contents with spaces
c
                              do 10 i = start, length
                                 j = toind + i - 1
                                 cstore(j:j) = ' '
10                            continue
                              j = toind + length
                              cstore(j:j) = CHAR( 0)
c
c                             And recalulate the size
c
                              status = uixtsi ( eid )
                              if ( .not. status ) then
c
c                                And destroy the window owning the
c                                element
c
                                 pid = texopa(eid)
                                 if ( panisc(pid) ) then
                                    status = windes ( panode(pid),
     +                                                panwid(pid) )
                                    panisc(pid) = .FALSE.
                                 endif
                              endif
                           endif
                        else
c
c                          An error - free the newly allocated space
c
                           status = freem ( newsto )
                           status = .TRUE.
                        endif
                     endif
                  else
c
c                    Error - invalid textfield length
c
                     uiferr = BADLEN
                     status = .TRUE.
                  endif
               else
c
c                 Error - panel is showing
c
                  uiferr = PASHOW
                  status = .TRUE.
               endif
            else
c
c              Error - bad textfield id
c
               uiferr = BADTEX
               status = .TRUE.
            endif
         else
c
c           Error - bad textfield id
c
            uiferr = BADTEX
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
c     All done
c
      uiftle = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
