C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION UIFMOE ( DEVICE, EID )
C       ---------------------------------------
C
C       PARAMETERS:
C
C       integer device : INPUT - the device on which the mouse position
C                        is to be set.
C
C       integer eid : INPUT - the identifer of the panel of element
C                             be which the mouse is to be set.
C
C       Sets the mouse position to be on the panel or element whose
C       is is given. Re-scans at the new position.
C
C       Sets application variables UIX, UIY, PNO and ENO.
C
C       Function returns FALSE if sucessful, otherwise TRUE.
C
C----------------------------------------------------------------------
C
      logical function uifmoe ( device, eid )
C     =======================================
C
      integer device
      integer eid
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
C     Position we are to set the mouse to
C
      integer xpos, ypos
C
C     Owning panel position
C
      integer pxpos, pypos
C
C     CALLED FUNCTIONS:
C
C     Sets application variable value
C
      logical appack
C
C     Scans for elements at a position
C
      logical uixisd
C
C     Check system is initialised
C
      if ( uifisi ) then
C
C        Check the device is valid
C
         if ( device .eq. HOST .or. device .eq. FSTORE ) then
C
C           Check the id is valid
C
            if ( eid .ge. MINPAN .and. eid .le. MAXPAN ) then
               if ( pannam(eid) .ge. 0 ) then
                  status = .FALSE.
               else
C
C                 Error - bad panel id
C
                  status = .TRUE.
                  uiferr = BADPAN
               endif
            else if ( eid .ge. MINELE .and. eid .le. MAXELE ) then
               if ( elenam(eid) .ge. 0 ) then
                  status = .FALSE.
               else
C
C                 Error - bad element id
C
                  status = .TRUE.
                  uiferr = BADEID
               endif
            else
C
C              Error - bad object id
C
               status = .TRUE.
               uiferr = BADOID
            endif
C
            if ( .not. status ) then
C
C              Get the position
C
               xpos  = objxpo(eid)
               ypos  = objypo(eid)
C
C              Convert according to positioning parameters
C
               xpos = xpos + (objxsi(eid)/2) * objhor(eid)
               ypos = ypos + (objysi(eid)/2) * objver(eid)
C
C              If an element, add on the panel position
C
               if ( eid .ge. MINELE .and. eid .le. MAXELE ) then
                  pxpos = panxpo(eleopa(eid))
                  pypos = panypo(eleopa(eid))
                  pxpos = pxpos + (panxsi(eleopa(eid))/2) *
     +                             panhor(eleopa(eid))
                  pypos = pypos + (panysi(eleopa(eid))/2) *
     +                             panver(eleopa(eid))
                  xpos = xpos + pxpos
                  ypos = ypos + pypos
               endif
C
C              Save the new position, set the cursor to it, and
C              rescan
C
               curxpo(device) = xpos
               curypo(device) = ypos
               call devcmo ( device, xpos, ypos )
               status = uixisd ( device, xpos, ypos )
               if ( .not. status ) then
C
C                 OK, set the variables.
C
                  status = appack ( -1985, real(xpos) )
                  if ( .not. status ) then
                     status = appack ( -1986, real(ypos) )
                     if ( .not. status ) then
                        status = appack ( 26175,
     +                                    real(pidl(device)))
                        if ( .not. status ) then
                           status = appack ( 8575,
     +                                       real(eidl(device)))
                        endif
                     endif
                  endif
                  if ( status ) then
C
C                    Error - couldn't set variable value
C
                     uiferr = VARSET
                  endif
               endif
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
      uifmoe = status
      return
C
C Copyright (C) 1988-1992:  Synoptics Ltd,  All Rights Reserved
C
      end
