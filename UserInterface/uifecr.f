C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION UIFECR ( PID, TYPE, EID )
C       ------------------------------------------
C
C       PARAMETERS:
C
C       integer pid : INPUT - Identifier of the panel the element is to
C                             be created on
C
C       integer type : INPUT - The type of element to create. Valid
C                      values : CELL, TEXFLD, MENU, GSIGHT, SLIDER, KNOB
C
C       integer eid : OUTPUT - the identifier of the newly created
C                              element
C
C       Creates a new element of the requested type on the requested
C       panel, and returns the id of the newly created element.  Initial
C       default settings of element parameters is carried out, depending
C       on type.  The panel window for the panel on which the element is
C       to be created is destroyed if it already exists.  The value of
C       variable ID is set to the identifier of the new element, which
C       is also returned in EID.
C       An error is generated if the panel on which the element is to
C       be created is showing.
C
C       Function returns FALSE if sucessful, otherwise TRUE.
C
C----------------------------------------------------------------------
C
      logical function uifecr ( pid, type, eid )
C     ==========================================
C
      integer pid, type, eid
C
      include 'UIFCOM'
      include 'UIXCOM'
      include 'UIFERR'
      include 'STORECOM'
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
C     CALLED FUNCTIONS:
C
C     Destroys a window
C
      logical windes
C
C     Sets an application variable value
C
      logical appack
C--------------------
C
C     Sets a textfield length
C      logical uiftle
C--------------------
C
C     Selects a textfield
C
      logical uiftse
C
C     Check system is initialised
C
      if ( uifisi ) then
C
C        Check panel id is valid
C
         if ( pid .ge. MINPAN .and. pid .le. MAXPAN ) then
            if ( pannam(pid) .ge. 0 ) then
C
C              Now check that the owning panel is not showing
C
               if ( .not. paniss(pid) ) then
C
C                 Delete the panel window if it is created
C
                  if ( panisc(pid) ) then
                     status = windes ( panode(pid), panwid(pid) )
                     panisc(pid) = .FALSE.
                  else
                     status = .FALSE.
                  endif
                  if ( .not. status ) then
C
C                    Split on element type
C
                     if ( type .eq. CELL ) then
C
C                       Find a free cell id
C
                        status = .TRUE.
                        do 10 i = MINCEL, MAXCEL
                           if ( celnam(i) .lt. 0 ) then
C
C                             Got one.  Save element id.
C
                              eid = i
                              status = .FALSE.
                              goto 20
                           endif
   10                   continue
   20                   continue
                        if ( .not. status ) then
C
C                          Initialise element data
C
                           call uixein ( eid, pid )
C
C                          And initialise cell data
C
                           celtyp(eid) = TEXT
                           celcon(eid) = 0
                           celsty(eid) = FLASH
                           celcyc(eid) = NORMAL
                           celome(eid) = 0
                           celrow(eid) = -1
                           celcol(eid) = -1
                           celxof(eid) = 0
                           celyof(eid) = 0
                           celdro(eid) = .FALSE.
                           celbox(eid) = .FALSE.
                        else
C
C                          Error - no free cell elements
C
                           uiferr = NOCELS
                        endif
                     else if ( type .eq. TEXFLD ) then
C
C                       Find a free textfield id
C
                        status = .TRUE.
                        do 30 i = MINTEX, MAXTEX
                           if ( texnam(i) .lt. 0 ) then
C
C                             Got one.  Save element id.
C
                              eid = i
                              status = .FALSE.
                              goto 40
                           endif
   30                   continue
   40                   continue
                        if ( .not. status ) then
C
C                          Initialise element data
C
                           call uixein ( eid, pid )
C
C                          And initialise textfield data
C
                           texnum(eid) = .FALSE.
                           texwp(eid)  = .FALSE.
                           texcpo(eid) = 1
                           texmir(eid) = 0
                           texmar(eid) = 0
                           texmio(eid) = NOCHEK
                           texmao(eid) = NOCHEK
                           texoor(eid) = 0
                           textaf(eid) = -1
C
C                          Set things up so that field has default
C                          length and blank contents
C
                           texcle(eid) = 0
      texlen(eid) = 0
C--------------------
C                           status = uiftle ( eid, DEFLEN )
C                           if ( .not. status ) then
C--------------------
C
C                             Set selected textfield to this one
C
                              status = uiftse ( eid )
C--------------------
C                           endif
C--------------------
C
                        else
C
C                          Error - no free textfield elements
C
                           uiferr = NOTEXS
                        endif
                     else if ( type .eq. MENU ) then
C
C                       Find a free menu id
C
                        status = .TRUE.
                        do 50 i = MINMEN, MAXMEN
                           if ( mennam(i) .lt. 0 ) then
C
C                             Got one.  Save element id.
C
                              eid = i
                              status = .FALSE.
                              goto 60
                           endif
   50                   continue
   60                   continue
                        if ( .not. status ) then
C
C                          Initialise element data
C
                           call uixein ( eid, pid )
C
C                          And initialise menu data
C
                           mentyp(eid) = PULLDN
                           mensty(eid) = CHOICE
                           menisa(eid) = .FALSE.
                           mencid(eid) = -1
                           menpan(eid) = -1
                        else
C
C                          Error - no free menu elements
C
                           uiferr = NOMENS
                        endif
                     else
C
C                       Error - unknown element type
C
                        uiferr = BADELE
                        status = .TRUE.
                     endif
C
C                    And set the values of the appropriate application
C                    variables
C
                     if ( .not. status ) then
                        curid = eid
                        status = appack ( 8575, real ( eid ) )
                        if ( status ) uiferr = VARSET
                     endif
                  endif
               else
C
C                 Error - panel is showing
C
                  uiferr = PASHOW
                  status = .TRUE.
               endif
            else
C
C              Error - invalid panel id
C
               uiferr = BADPAN
               status = .TRUE.
            endif
         else
C
C           Error - invalid panel id
C
            uiferr = BADPAN
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
      uifecr = status
      return
C
C Copyright (C) 1988-1992:  Synoptics Ltd,  All Rights Reserved
C
      end
