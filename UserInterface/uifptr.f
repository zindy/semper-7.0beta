c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIFPTR ( PID )
c       -------------------------------
c
c       PARAMETERS:
c
c       integer pid : INPUT - the identifier of the panel which is to
c                     be declared transitory.
c
c       Sets the given panel transitory.  If the panel is showing, an
c       error is generated, since a showing panel cannot be made
c       transitory.
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uifptr ( pid )
c     ===============================
c
      integer pid
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
c        Check panel id is valid
c
         if ( pid .ge. MINPAN .and. pid .le. MAXPAN ) then
            if ( pannam(pid) .ge. 0 ) then
c
c              Now check that the panel is not showing
c
               if ( .not. paniss(pid) ) then
c
c                 OK.  Can make panel transitory.
c
                  panfix(pid) = .FALSE.
                  status = .FALSE.
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
      uifptr = status
      return
      end
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
