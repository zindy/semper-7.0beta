C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION UIFPCR ( DEVICE, PID )
C       ---------------------------------------
C
C       PARAMETERS:
C
C       integer device : INPUT - Defines the device the panel is to be
C                        created on (valid values: HOST, FSTORE)
C
C       integer pid : OUTPUT - the identifier of the newly created panel
C
C       Creates a new panel of the default size and position.  Does NOT
C       create a window for the panel.
C       Initial default settings of panel parameters are carried out,
C       and the panel is placed at the bottom on the panel stack for
C       the device.  The value of variable PID is set to the identifier
C       of the new panel, which is also returned in PID.
C
C       Function returns FALSE if sucessful, otherwise TRUE.
C
C----------------------------------------------------------------------
C
      logical function uifpcr ( device, pid )
C     =======================================
C
      integer device
      integer pid
C
      include 'UIFCOM'
      include 'UIXCOM'
      include 'UIFERR'
C
C     LOCAL VARIABLES:
C
C     Return status
C
      logical status
C
C     Loop counter
C
      integer i
C
C     Sets an application varaible value
C
      logical appack
C
C     Check system is initialised
C
      if ( uifisi ) then
C
C        Check device is valid
C
         if ( device .eq. HOST .or. device .eq. FSTORE ) then
C
C           Get a free panel
C
            status = .TRUE.
            do 10 i = MINPAN, MAXPAN
               if ( pannam(i) .eq. -1 ) then
                  pid = i
                  status = .FALSE.
                  goto 20
               endif
   10       continue
   20       continue
C
            if ( .not. status ) then
C
C              Initialise panel data
C
               call uixpin ( pid, device )
C
C              And set the values of the appropriate application
C              variables
C
               status = appack ( 26175, real ( pid ) )
               if ( .not. status ) then
C
C                 And record the current panel id
C
                  curpid = pid
               endif
            else
C
C              Error - couldn't create panel
C
               uiferr = NOPANS
            endif
         else
C
C           Error - invalid device
C
            uiferr = BADDEV
            status = .TRUE.
         endif
      else
C
C        Error - system not initialised
C
         uiferr = NOTINI
         status = .TRUE.
      endif
C
      uifpcr = status
      return
      end
C
C Copyright (C) 1988-1992:  Synoptics Ltd,  All Rights Reserved
C
