c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIFSAC ( DEVICE, SID )
c       ---------------------------------------
c
c       PARAMETERS:
c
c       integer device : INPUT - Defines the device the scrolling area
c                                is to be created on
c                                (valid values: HOSDEV, FSDEV)
c
c       integer sid : OUTPUT - the identifier of the newly created
c                              scrolling area
c
c       Creates a new scrolling area of the default size and position.
c       Creates a panel for the scrolling area via UIFPCR, and
c       initialises the line buffers.  The value of sid is saved before
c       the panel is created so that if can be reinstated.  The
c       identifier of the newly created scrolling area is returned in
c       SID.
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uifsac ( device, sid )
c     =======================================
c
      integer device
      integer sid
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
c     Check system is initialised
c
      if ( uifisi ) then
c
c        Check device is valid
c
         if ( device .eq. HOST .or. device .eq. FSTORE ) then
c
c           Get a free scrolling area
c
            status = .TRUE.
            do 1 i = MINSCR, MAXSCR
               if ( scrnam(i) .eq. -1 ) then
                  sid = i
                  status = .FALSE.
                  goto 2
               endif
1           continue
2           continue
c
            if ( .not. status ) then
c
c              Initialise panel data
c
               call uixsin ( sid, device )
            else
c
c              Error - couldn't create scrolling area
c
               uiferr = NOSCRS
            endif
         else
c
c           Error - invalid device
c
            uiferr = BADDEV
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
      uifsac = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
