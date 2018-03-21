c
c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIFTSE ( EID )
c       -------------------------------
c
c       PARAMETERS:
c
c       integer eid : INPUT - the identifier of the textfield element
c                     which is to be selected.
c
c       Makes the given textfield the currently selected textfield.
c       The text cursor is positioned to the current cursor position in
c       the given textfield.
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uiftse ( eid )
c     ===============================
c
      integer eid
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
c     The selected textfield on entry
      integer lastse
c
c     CALLED FUNCTIONS:
c
c     Draws the cursor of a textfield
      logical uixxdc
c
c     Check system is initialised
c
      if ( uifisi ) then
c
c        Check textfield id is valid
c
         if ( eid .ge. MINTEX .and. eid .le. MAXTEX
     +        .and. texnam(eid) .ge. 0 ) then
c
c           Change the selected textfield
c
            if ( eid .ne. seltex ) then
               lastse = seltex
               seltex = eid
c
c              Redraw the cursors in the two fields, if their panels
c              are showing
c
               if ( lastse .ne. -1 ) then
                  if ( paniss(texopa(lastse)) ) then
                     status = uixxdc ( lastse )
                  else
                     status = .FALSE.
                  endif
               else
                  status = .FALSE.
               endif
               if ( .not. status ) then
c
c                 If a begins action is defined for the textfield,
c                 then generate it
c
                  if ( texbac(eid) .gt. 0 ) then
                     call uixevp ( EVACTI, texbac(eid) )
                  endif
c
c                 If panel is showing, redraw contents
c
                  if ( paniss(texopa(eid)) ) then
                     status = uixxdc ( eid )
                  endif
               endif
            else
c
c              Requested textfield is already selected
c
               status = .FALSE.
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
      uiftse = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
