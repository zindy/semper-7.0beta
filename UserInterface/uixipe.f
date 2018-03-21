C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION UIXIPE ( TXTSTR, N, DONE )
C       -------------------------------------------
C
C       PARAMETERS:
C
C       character*(*) txtstr : OUTPUT - command to be executed.
C
C       integer n : OUTPUT - the number of characters in the buffer.
C
C       logical done : OUTPUT - 'all done' flag.
C
C       Processes an input event.  Reads the next event, and switches on
C       the type of it.  Performs any feedback needed, and if necessary
C       extracts any action for the event and returns it to the
C       acpplication.
C
C       Function returns FALSE if sucessful, otherwise TRUE.
C
C----------------------------------------------------------------------
C
      logical function uixipe ( txtstr, n, done )
C
C     ===========================================
C
      character*(*) txtstr
      integer n
      logical done
C
      include 'UIFCOM'
      include 'UIXCOM'
      include 'STORECOM'
C
C     LOCAL VARIABLES:
C
C     Return status
C
      logical status
C
C     The type of event which has occured
C
      integer type
C
C     The data for the event
C
      integer datum
C
C     Element id we are processing
C
      integer eid
C
C     Device element's panel is on
C
      integer device
C
C     CALLED FUNCTIONS:
C
C     Reads an action into the command buffer
C
      logical uixica
C
C     Processes events for cells
C
      logical uixxcp
C
C     Processes events for menus
C
      logical uixxmp
C
C     Processes events for textfields
C
      logical uixxtp
C
C     Frees memory
C
      logical freem
C
C     Get the action
C
      done = .FALSE.
      status = .FALSE.
      call uixevg ( type, datum )
C
      if ( type .eq. EVACTI .or. type .eq. EVAFRE ) then
C
C        If it was an action for execution event, just get it!
C
         status = uixica ( datum, txtstr, n )
C
C        And free if needed
C
         if ( type .eq. EVAFRE ) then
            status = freem ( datum )
         endif
         done = .TRUE.
      else if ( type .eq. EVENTR .or. type .eq. EVLEFT .or.
     +          type .eq. EVSELE ) then
C
C        If was entered, left or selected action, perform any
C        feedback needed, depending on the element type, then get any
C        appropriate action.
C
         eid = datum
         if ( elenam(eid) .ge. 0 ) then
            device = panode(eleopa(eid))
            if ( eid .ge. MINCEL .and. eid .le. MAXCEL ) then
               status = uixxcp ( device, eid, type,
     +                           txtstr, n, done )
            else if ( eid .ge. MINMEN .and. eid .le. MAXMEN ) then
               status = uixxmp ( device, eid, type,
     +                           txtstr, n, done )
            else if ( eid .ge. MINTEX .and. eid .le. MAXTEX ) then
               status = uixxtp ( device, eid, type, datum,
     +                           txtstr, n, done )
            endif
         else
C
C           The element the event was generated on no longer
C           exists, so ignore the event
C
            status = .FALSE.
         endif
      else if ( type .eq. EVKBRD ) then
         status = uixxtp ( device, eid, type, datum, txtstr, n, done )
      endif
C
      uixipe = status
      return
C
C Copyright (C) 1988-1993:  Synoptics Ltd,  All Rights Reserved
C
      end
C
      subroutine uixirc ( device )
C
C     ============================
C
      integer device
C
C     Reset the state of the cursor
C
      call devcre ( device )
C
C     Turn it on
C
      call devcon ( device )
      return
C
C Copyright (C) 1988-1992:  Synoptics Ltd,  All Rights Reserved
C
      end
C
C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION UIXIOM ( EID, MENUID, STYLE )
C       ----------------------------------------------
C
C       PARAMETERS:
C
C       integer eid : INPUT - identifier of a cell
C
C       integer menuid : OUTPUT - idenitifer of menu cell is on.
C
C       integer style : OUTPUT - style of said menu.
C
C       Checks to see if a given cell is on a menu, and returns info. on
C       the menu if the cell is,on one.
C
C       Function returns TRUE is cell is on a menu, otherwise FALSE.
C
C----------------------------------------------------------------------
C
      logical function uixiom ( eid, menuid, style )
C
C     ==============================================
C
      integer eid, menuid, style
C
      include 'UIFCOM'
      include 'UIXCOM'
C
C     LOCAL VARIABLES:
C
C     Loop counter
C
      integer i
C
C     Menu panel if menu is pulldown/popup
C
      integer mpanel
C
C     Find out if this cell is on a menu, and if so, which
C     style.  First check to see if it's on a fixed menu
C
      uixiom = .FALSE.
      if ( celome(eid) .gt. 0 ) then
         uixiom = .TRUE.
         menuid = celome(eid)
         style  = mensty(menuid)
      else
C
C        No, so how about on a pulldown/popup?
C
         mpanel = celopa(eid)
         do 10 i = MINMEN, MAXMEN
            if ( mennam(i) .ge. 0 ) then
               if ( menpan(i) .eq. mpanel ) then
                  uixiom = .TRUE.
                  menuid = i
                  style  = mensty(i)
                  goto 20
               endif
            endif
   10    continue
   20    continue
      endif
C
      return
C
C Copyright (C) 1988-1992:  Synoptics Ltd,  All Rights Reserved
C
      end
C
C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION UIXXCP ( DEVICE, EID, TYPE, TXTSTR, N, DONE )
C       --------------------------------------------------------------
C
C       PARAMETERS:
C
C       integer device : INPUT - the device on which the cell resides.
C
C       integer eid : INPUT - the cell identifier.
C
C       integer type : INPUT - the event type.
C
C       character*(*) txtstr : OUTPUT - command to be executed.
C
C       integer n : OUTPUT - the number of characters in the buffer.
C
C       logical done : OUTPUT - 'all done' flag.
C
C       Processes an event generated on a cell.  Performs any feedback
C       needed, and if necessary extracts any action for the event and
C       returns it to the application.
C
C       Function returns FALSE if sucessful, otherwise TRUE.
C
C----------------------------------------------------------------------
C
      logical function uixxcp (device, eid, type, txtstr, n, done)
C
C     ============================================================
C
      integer device, eid, type
      character*(*) txtstr
      integer n
      logical done
C
      include 'UIFCOM'
      include 'UIXCOM'
C
C     LOCAL VARIABLES:
C
C     Return status
C
      logical status
C
C     Number of cycles
C
      integer ncycle
C
C     'Cell is on a menu' flag
C
      logical onmenu
C
C     Menu cell is on
C
      integer menuid
C
C     Style of the menu
C
      integer style
C
C     CALLED FUNCTIONS:
C
C     Checks to see if cell is on a menu
C
      logical uixiom
C
C     Cycles the state of a cell
C
      logical uifccy
C
C     Scans for actions at a position
C
      logical uixisd
C
C     Deactivates a menu
C
      logical uifmda
C
C     Reads an action into the command buffer
C
      logical uixica
C
C     Sets the value of an application variable
C
      logical appack
C
C     First perform feedback as required.  Need to take different action
C     depending on whether its an INVERT cell or not, whether its on a
C     menu, and whether its a SELECT event or not.  First find if its on
C     a menu.
C
      onmenu = uixiom ( eid, menuid, style )
C
C     Calculate the number of cycles to do based on cell type
C
      if ( celsty(eid) .eq. FLASH ) then
         ncycle = 4
      else
         ncycle = 1
      endif
C
C     Check the event type.
C
      status = .FALSE.
      if ( type .ne. EVSELE ) then
C
C        Entered or left event.  Cycle state if it's an not a flash
C        cell on a choice menu
C
         if ( onmenu ) then
            if ( style .eq. CHOICE .and. celsty(eid) .ne. FLASH ) then
C
C              Save state of cursor, and turn it off
C
               call devcsa ( device )
               call devcof ( device )
C
C              Cycle cell state by one
C
               status = uifccy ( eid, ncycle )
C
C              Restore state of cursor
C
               call uixirc ( device )
            endif
         else
C
C           Cell not on a menu.  Cycle it if it's an invert cell
C
            if ( celsty(eid) .eq. INVERT ) then
               call devcsa ( device )
               call devcof ( device )
               status = uifccy ( eid, ncycle )
               call uixirc ( device )
            endif
         endif
      else
C
C        Select.  Cycle state, unless its an invert cell on a
C        toggle menu or not on menu.  First save state of cursor,
C        and turn it off
C
         call devcsa ( device )
         call devcof ( device )
         if ( celsty(eid) .eq. INVERT ) then
            if ( onmenu ) then
               if ( style .eq. TOGGLE ) then
                  status = uifccy ( eid, ncycle )
               else
                  status = .FALSE.
               endif
               mencid(menuid) = eid
            endif
         else
            if ( onmenu ) then
               if ( style .eq. TOGGLE ) then
                  status = uifccy ( eid, ncycle )
               else
                  if ( celsty(eid) .eq. FLASH ) then
                     status = uifccy ( eid, ncycle )
                  else
                     status = .FALSE.
                  endif
               endif
               mencid(menuid) = eid
            else
               status = uifccy ( eid, ncycle )
            endif
         endif
C
C        If the cell was a drop cell, drop its menu
C
         if ( celdro(eid) ) then
            if ( onmenu ) then
               if ( mentyp(menuid) .eq. POPUP .or.
     +              mentyp(menuid) .eq. PULLDN ) then
                  status = uifmda ( menuid )
                  if ( .not. status ) then
C
C                    And re-scan at this position now the menu is gone
C
                     status = uixisd ( device, curxpo(device),
     +                                         curypo(device) )
                  endif
               endif
            endif
         endif
C
C        Restore state of the cursor
C
         call uixirc ( device )
      endif
C
C     OK, all feedback done.  Fetch out any actions.
C
      if ( .not. status ) then
         if ( type .eq. EVENTR ) then
            if ( celbac(eid) .gt. 0 ) then
               status = uixica ( celbac(eid), txtstr, n )
               if ( .not. status ) then
C
C                 And set ENO to the id of the cell
C
                  status = appack ( 8575, real ( eid ) )
                  done = .TRUE.
               endif
            endif
         else if ( type .eq. EVSELE ) then
            if ( celcac(eid) .gt. 0 ) then
               status = uixica ( celcac(eid), txtstr, n )
               if ( .not. status ) then
C
C                 And set ENO to the id of the cell
C
                  status = appack ( 8575, real ( eid ) )
                  done = .TRUE.
               endif
            endif
         else if ( type .eq. EVLEFT ) then
            if ( celeac(eid) .gt. 0 ) then
               status = uixica ( celeac(eid), txtstr, n )
               if ( .not. status ) then
C
C                 And set ENO to the id of the cell
C
                  status = appack ( 8575, real ( eid ) )
                  done = .TRUE.
               endif
            endif
         endif
      endif
C
      uixxcp = status
      return
C
C Copyright (C) 1988-1993:  Synoptics Ltd,  All Rights Reserved
C
      end
C
C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION UIXXMP ( DEVICE, EID, TYPE, TXTSTR, N, DONE )
C       --------------------------------------------------------------
C
C       PARAMETERS:
C
C       integer device : INPUT - the device on which the menu resides.
C
C       integer eid : INPUT - the menu identifier.
C
C       integer type : INPUT - the event type.
C
C       character*(*) txtstr : OUTPUT - command to be executed.
C
C       integer n : OUTPUT - the number of characters in the buffer.
C
C       logical done : OUTPUT - 'all done' flag.
C
C       Processes an event generated on a menu.  Performs any feedback
C       needed, and if necessary extracts any action for the event and
C       returns it to the application.
C
C       Function returns FALSE if sucessful, otherwise TRUE.
C
C----------------------------------------------------------------------
C
      logical function uixxmp (device, eid, type, txtstr, n, done)
C
C     ============================================================
C
      integer device, eid, type
      character*(*) txtstr
      integer n
      logical done
C
      include 'UIFCOM'
      include 'UIXCOM'
      include 'STORECOM'
C
C     LOCAL VARIABLES:
C
C     Return status
C
      logical status
C
C     Sets the value of an application variable
C
      logical appack
C
C     Style to draw menu name in
C
      integer style
C
C     Id of panel owning menu
C
      integer pid
C
C     Id of panel window
C
      integer wid
C
C     Menu foreground colour
C
      integer fgc
C
C     Menu background colour
C
      integer bgc
C
C     Menu position
C
      integer xpos, ypos
C
C     Physical index of menu name
C
      integer ind
C
C     Length of menu name
C
      integer leng
C
C     CALLED FUNCTIONS:
C
C     Activates a menu
C
      logical uifmac
C
C     Reads an action into the command buffer
C
      logical uixica
C
C     Gets physical index of store
C
      logical pindex
C
C     Gets a string length
C
      integer USTLEN
C
C     Draws text in a window
C
      logical wintex
C
C     If event IS a select, and the menu is a pulldown, then activate
C     the menu
C
      status = .FALSE.
      if ( type .eq. EVSELE ) then
         if ( mentyp(eid) .eq. PULLDN ) then
C
C           Turn the cursor off
C
            call devcof ( device )
            status = uifmac ( eid )
C
C           And turn it back on again
C
            call devcon ( device )
         else
            status = .FALSE.
         endif
      else
C
C        Either enters or leaves event.
C
         if ( mentyp(eid) .eq. FIXED ) then
C
Cc
Cc           Turn the cursor off
Cc
C            call devcof ( device )
Cc
Cc           Activate or deactivate the menu
Cc
C            if ( type .eq. EVENTR ) then
C                status = uifmac ( eid )
C            else
C                status = uifmda ( eid )
C            endif
Cc
Cc           And turn it back on again
Cc
C            call devcon ( device )
C
         else if ( mentyp(eid) .eq. PULLDN ) then
C
C           Pulldown menu. If not active, highlight name if entering
C           and lowlight if leaving
C
            status = .FALSE.
            if ( .not. menisa(eid) ) then
               if ( mennam(eid) .gt. 0 ) then
                  if ( type .eq. EVENTR ) then
C
C                    Entered: draw menu name inverse.
C
                     style = INVERS
                  else
C
C                    Left: draw menu name normal.
C
                     style = NORMAL
                  endif
C
C                 Draw the menu name
C
                  pid = menopa(eid)
                  wid = winwid(pid)
                  fgc = menfgc(eid)
                  bgc = menbgc(eid)
                  xpos = menxpo(eid)
                  ypos = menypo(eid)
                  status = pindex ( mennam(eid), ind )
                  if ( .not. status ) then
                     leng = USTLEN ( cstore(ind:) )
                     if ( .not. status ) then
C
C                       Save state of cursor, and turn it off
C
                        call devcsa ( device )
                        call devcof ( device )
C
C                       Draw the text
C
                        status = wintex ( device, wid, fgc, bgc,
     +                                    style, xpos, ypos,
     +                                    cstore(ind:), leng )
C
C                       Restore state of the cursor
C
                        call uixirc ( device )
                     endif
                  endif
               endif
            endif
         else
            status = .FALSE.
         endif
      endif
C
C     OK, all feedback done.  Generate actions for fixed menus
C     (will have already been done by activating other menus)
C
      if ( .not. status ) then
         if ( mentyp(eid) .eq. FIXED ) then
C
C           Fixed menu...
C
            if ( type .eq. EVENTR ) then
               if ( menbac(eid) .gt. 0 ) then
                  status = uixica ( menbac(eid), txtstr, n )
                  if ( .not. status ) then
C
C                    And set ENO to the id of the menu
C
                     status = appack ( 8575, real ( eid ) )
                     done = .TRUE.
                  endif
               endif
            else if ( type .eq. EVLEFT ) then
               if ( meneac(eid) .gt. 0 ) then
                  status = uixica ( meneac(eid), txtstr, n )
                  if ( .not. status ) then
C
C                    And set ENO to the id of the menu
C
                     status = appack ( 8575, real ( eid ) )
                     done = .TRUE.
                  endif
               endif
            endif
         endif
      endif
C
      uixxmp = status
      return
C
C Copyright (C) 1988-1993:  Synoptics Ltd,  All Rights Reserved
C
      end
C
C----------------------------------------------------------------------
C
C       LOGICAL FUNCTION UIXXTP ( DEVICE, EID, TYPE, DATUM, TXTSTR, N,
C                                 DONE )
C       --------------------------------------------------------------
C
C       PARAMETERS:
C
C       integer device : INPUT - the device on which the textfield
C                                resides.
C
C       integer eid : INPUT - the textfield identifier.
C
C       integer type : INPUT - the event type.
C
C       integer datum : INPUT - the event datum.
C
C       character*(*) text : OUTPUT - command to be executed.
C
C       integer n : OUTPUT - the number of characters in the buffer.
C
C       logical done : OUTPUT - 'all done' flag.
C
C       Processes an event generated on a textfield.  Performs any
C       feedback needed, and if necessary extracts any action for the
C       event and returns it to the application.
C
C       Function returns FALSE if sucessful, otherwise TRUE.
C
C----------------------------------------------------------------------
C
      logical function uixxtp (device, eid, type, datum, txtstr, n, done
     + )
C
C     ==================================================================
C ==
C
      integer device, eid, type, datum
      character*(*) txtstr
      integer n
      logical done
C
      include 'UIFCOM'
      include 'UIXCOM'
      include 'STORECOM'
      include 'ICSET'
C
C     LOCAL VARIABLES:
C
C     Return status
C
      logical status
C
C     Textfield name length
C
      integer len
C
C     Physical index of store
C
      integer ind
C
C     Textfield content start and end position, device coords
C
      integer xfpos, xfend
C
C     Character typed in
C
      integer ich
C
C     Owning panel id of element
C
      integer pid
C
C     'I've changed the field contents' flag
C
      logical chang
C
C     CALLED FUNCTIONS:
C
C     Selects a textfield
C
      logical uiftse
C
C     Changes textfield contents
C
      logical uiftco
C
C     Deletes characters from textfield
C
      logical uiftde
C
C     Move the cursor in a textfield
C
      logical uiftcp
C
C     Inserts a character in a textfield
C
      logical uiftin
C
C     Gets the physical index of store
C
      logical pindex
C
C     Gets string length
C
      integer USTLEN
C
C     Reads an action into the command buffer
C
      logical uixica
C
C     Sets the value of an application variable
C
      logical appack
C
C     If event IS a select, select the textfield.
C
      status = .FALSE.
      chang = .FALSE.
      if ( type .eq. EVSELE ) then
C
C        Save state of cursor, and turn it off
C
         call devcsa ( device )
         call devcof ( device )
C
C        If user hit on the name, just leave the field cursor
C        where it is.  Otherwise, position the field cursor to
C        where hit was made
C
C        Select the field
C
         status = uiftse ( eid )
         if ( .not. status ) then
C
C           Hit in the contents?
C
            pid = texopa(eid)
            device = panode(pid)
            len = 0
            status = .FALSE.
            if ( texnam(eid) .gt. 0 ) then
               status = pindex ( texnam(eid), ind )
               if ( .not. status ) then
                  len = USTLEN ( cstore(ind:) ) + 2
               endif
            endif
            if ( .not. status ) then
               xfpos = texxpo(eid) + panxpo(pid) + len
               xfend = xfpos + texlen(eid) - 1
               if ( curxpo(device) .ge. xfpos .and.
     +              curxpo(device) .le. xfend ) then
C
C                 Hit was in the field.  Move the cursor
C
                  status = uiftcp ( eid, ABSOL,
     +                              curxpo(device) - xfpos + 1 )
               endif
            endif
         endif
C
C        Restore state of cursor
C
         call uixirc ( device )
      else if ( type .eq. EVKBRD ) then
C
C        Keyboard event.  Act on the character if there is a selected
C        textfield
C
         if ( seltex .ne. -1 ) then
            if ( .not. texwp(seltex) ) then
               ich = datum
               device = panode(eleopa(seltex))
               if ( ich .eq. KBKILL ) then
C
C                 Kill - clear the selected textfield
C                 Save state of cursor, and turn it off
C
                  call devcsa ( device )
                  call devcof ( device )
                  status = uiftco ( seltex, ' ', 0 )
                  chang = .TRUE.
C
C                 Restore state of cursor
C
                  call uixirc ( device )
               else if ( ich .eq. KBDEL ) then
C
C                 Delete - delete a character
C                 Save state of cursor, and turn it off
C
                  call devcsa ( device )
                  call devcof ( device )
                  status = uiftde ( seltex, 1 )
                  chang = .TRUE.
C
C                 Restore state of cursor
C
                  call uixirc ( device )
               else if ( ich .eq. KBLEFT ) then
C
C                 Cursor left - move left a character
C                 Save state of cursor, and turn it off
C
                  call devcsa ( device )
                  call devcof ( device )
                  status = uiftcp ( seltex, LEFT, 1 )
C
C                 Restore state of cursor
C
                  call uixirc ( device )
               else if ( ich .eq. KBRITE ) then
C
C                 Cursor right - move right a character
C                 Save state of cursor, and turn it off
C
                  call devcsa ( device )
                  call devcof ( device )
                  status = uiftcp ( seltex, RIGHT, 1 )
C
C                 Restore state of cursor
C
                  call uixirc ( device )
               else if ( ich .eq. KBTAB ) then
C
C                 Tab - go on to next field if such defined
C
                  if ( textaf(seltex) .ne. -1 ) then
C
C                    Save state of cursor, and turn it off
C
                     call devcsa ( device )
                     call devcof ( device )
                     status = uiftse ( textaf(seltex) )
C
C                    Restore state of cursor
C
                     call uixirc ( device )
                  endif
               else
C
C                 Ordinary character - insert if not a conrol character.
C
                  if ( ich .ge. 32 .and. ich .le. 126 ) then
C
C                    Save state of cursor, and turn it off
C
                     call devcsa ( device )
                     call devcof ( device )
                     status = uiftin ( seltex, CHAR(ich), 1 )
                     chang = .TRUE.
C
C                    Restore state of cursor
C
                     call uixirc ( device )
                  else
                     status = .FALSE.
                  endif
               endif
            else
C
C              Textfield is write protected.  Beep at the swine.
C
               call appbel
               status = .FALSE.
            endif
         endif
      endif
C
C     Now generate any actions required
C
      if ( .not. status ) then
         if ( type .eq. EVSELE ) then
C
C           Textfield selected, so do begins action if defined
C
            if ( texbac(eid) .gt. 0 ) then
               status = uixica ( texbac(eid), txtstr, n )
               if ( .not. status ) then
C
C                 And set ENO to the id of the textfield
C
                  status = appack ( 8575, real ( eid ) )
                  done = .TRUE.
               endif
            endif
         else if ( chang ) then
C
C           Contents changed, so do changes action if defined
C
            if ( seltex .ne. -1 ) then
               if ( texcac(seltex) .gt. 0 ) then
                  status = uixica ( texcac(seltex), txtstr, n )
                  if ( .not. status ) then
C
C                    And set ENO to the id of the textfield
C
                     status = appack ( 8575, real ( eid ) )
                     done = .TRUE.
                  endif
               endif
            endif
         endif
      endif
C
      uixxtp = status
      return
C
C Copyright (C) 1988-1993:  Synoptics Ltd,  All Rights Reserved
C
      end
C
C----------------------------------------------------------------------
C
C       SUBROUTINE UIXISC ( EID )
C       -------------------------
C
C       PARAMETERS:
C
C       integer eid : INPUT - the menu identifier.
C
C       Positions cursor to the currently selected cell on the given
C       menu.
C
C----------------------------------------------------------------------
C
      subroutine uixisc ( eid )
C
C     ========================
C
      integer eid
C
      include 'UIFCOM'
      include 'UIXCOM'
C
C     LOCAL VARIABLES:
C
C     Panel owning the currently selected cell
C
      integer pid
C
C     Device on which the panel resides
C
      integer device
C
C     Currently selected cell id for the menu
C
      integer cid
C
C     Position to which to move the mouse to put it on currently
C     selected cell
C
      integer x, y
C
C     Function return status
C
      logical dummy
C
C     CALLED FUNCTIONS:
C
C     Scans a device at a position
C
      logical uixisd
C
C     Move the mouse onto the selected cell of the menu, if there is one
C
      cid = mencid(eid)
      if ( cid .ne. -1 ) then
         pid = celopa(cid)
         device = panode(pid)
         x = celxpo(cid) + panxpo(pid)
         y = celypo(cid) + panypo(pid)
         call devcmo ( device, x, y )
C
C        Remember where the mouse is
C
         curxpo(device) = x
         curypo(device) = y
C
C        Rescan at the position
C
         dummy = uixisd ( device, x, y )
      endif
C
      return
C
C Copyright (C) 1988-1992:  Synoptics Ltd,  All Rights Reserved
C
      end
