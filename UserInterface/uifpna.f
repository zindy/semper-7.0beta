c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIFPNA ( PID, NAME, LENGTH )
c       ---------------------------------------------
c
c       PARAMETERS:
c
c       integer pid : INPUT - the identifier of the panel which is
c                             to named
c
c       character*(*) name : INPUT - the name of the panel
c
c       integer length : INPUT - the length of the name
c
c       Sets the name of the panel with identifier pid to the name
c       given.  A copy of the name is made into dynamic storage, and the
c       pointer to it stored for the panel.  (If there is already a name
c       stored, the space is first released).  The name of the panel
c       window is changed.  An error will be generated if the panel is
c       showing, as the name of a showing panel cannot be changed.
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uifpna ( pid, name, length )
c     =============================================
c
      integer pid
      character*(*) name
      integer length
c
      include 'UIFCOM'
      include 'UIXCOM'
      include 'UIFERR'
c
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
c     Frees dynamic memory
      logical freem
c
c     Allocates dynamic memory
      logical allocm
c
c     Gets physical index of store
      logical pindex
c
c     Copies a string
      logical USTNCP
c
c     Changes the name of a window
      logical wincna
c
c     Check system is initialised
c
      if ( uifisi ) then
c
c        Check panel id is valid
c
         if ( pid .ge. MINPAN .and. pid .le. MAXPAN ) then
            if ( pannam(pid) .ge. 0 ) then
c
c              Now check that the panel is not showing
c
               if ( .not. paniss(pid) ) then
c
c                 If space already allocated for the name, free it.
c
                  if ( pannam(pid) .gt. 0 ) then
                     status = freem ( pannam(pid) )
                     pannam(pid) = 0
                  else
                     status = .FALSE.
                  endif
c
c                 Now allocate space for the name
c
                  if ( .not. status ) then
                     if ( length .gt. 0 ) then
                        status = allocm ( length + 1, pannam(pid) )
                        if ( .not. status ) then
c
c                          And copy in the name
c
                           status = pindex ( pannam(pid), ind )
                           if ( .not. status ) then
                              status = USTNCP ( cstore(ind:), name,
     +                                         length )

                              if ( .not. status ) then
c
c                                Finally, change the name of the window
c                                if it is created
c
                                 if ( panisc(pid) ) then
                                    status = wincna ( panode(pid),
     +                                                panwid(pid),
     +                                                name, length )
                                 endif
                              endif
                           endif
                        endif
                     endif
                     if ( status ) uiferr = NAMPAN
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
c              Error - invalid panel id
c
               uiferr = BADPAN
               status = .TRUE.
            endif
         else
c
c           Error - invalid panel id
c
            uiferr = BADPAN
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
      uifpna = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
