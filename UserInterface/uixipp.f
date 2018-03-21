c
c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIXIPP ( DELX, DELY, DONE )
c       --------------------------------------------
c
c       PARAMETERS:
c
c       integer delx, dely : INPUT - the pointer position change to
c                            process
c
c       logical done : OUTPUT - 'all done' flag.
c
c       Processes pointer event queue data into UIF event queue data.
c       Echos the cursor position, then scans all panels and elements
c       on all devices to generate events on the appropriate elements.
c
c       Function returns FALSE if sucessful, otherwise TRUE.
c
c----------------------------------------------------------------------
c
      logical function uixipp ( delx, dely, done )
c     ============================================
c
      integer delx, dely
      logical done
c
      include 'UIFCOM'
      include 'UIXCOM'
c
c     LOCAL VARIABLES:
c
c     Return status
      logical status
c
c     Loop counter (devices beng scanned)
      integer device
c
c     CALLED FUNCTIONS:
c
c     Scans a device to see what is at a position
      logical uixisd
c
c     If we need to, echo the new position
c
      call uixiec ( delx, dely )
c
c     Check what is at the postion on both devices
c
      status = .FALSE.
      do 1 device = 1, NDEVIC
         if ( devhap (device) ) then
            if ( .not. uixisd ( device, curxpo(device),
     +                          curypo(device) ) ) then
               done = .TRUE.
            else
c
c              Error - failed in scan
c
               status = .TRUE.
               goto 2
            endif
         endif
1     continue
2     continue
c
      uixipp = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
c
c----------------------------------------------------------------------
c
c       SUBROUTINE UIXIEC ( DELX, DELY )
c       --------------------------------
c
c       PARAMETERS:
c
c       integer delx, dely : INPUT - the pointer delta to echo
c
c       Echos the given change in pointer position.  On workstation
c       systems, (assumed to be anything which echos the pointer on all
c       devices i.e. ECHALL), it is assumed the the event queue drivers
c       maintain an absolute mouse position in devmox,devmoy for the
c       device, and also the interface window absolute position in
c       devxof,devyof.  These positions are assumed to be in pixels
c       relative to a top-left screen origin.  These absolute positions
c       are used to calculate the 'cursor' position in the interface
c       window - the delta values ARE NOT used at all.  On non
c       workstation systems, the deltas are used to calculate a new
c       cursor position, which is echoed if needed.
c
c----------------------------------------------------------------------
c
      subroutine uixiec ( delx, dely )
c     ================================
c
      integer delx, dely
c
      include 'UIFCOM'
      include 'UIXCOM'
      include 'EVENTS'
c
c     LOCAL VARIABLES:
c
c     Function return status
      logical dummy
c
c     Accumulated deltas
      integer deltax(NDEVIC), deltay(NDEVIC)
      save deltax, deltay
c
c     Dummy variable
      integer idummy
c
c     Hardware cursor echoing state
      integer hardec
c
c     'Do I need to software echo' flags for devices
      logical echoho, echofs
c
c     Loop counter (device being echoed)
      integer device
c
c     New cursor position
      integer newx, newy
c
c     Maximum cursor position on the device
      integer devxma, devyma
c
c     'Movement needed' flag
      logical moveit
c
c     CALLED FUNCTIONS:
c
c     Gets queue data
      logical eqgetd
c
c     DATA INITIALISATION:
c
      data deltax, deltay / 0, 0, 0, 0 /
c
c     See how much echoing is done by hardware
c
      dummy = eqgetd ( MPOINT, hardec, idummy, idummy )
      if ( hardec .ne. ECHALL ) then
c
c        Some software echoing needed.  See how much, and do it
c
         echoho = .TRUE.
         echofs = .TRUE.
         if ( hardec .eq. ECHHST .or. .not. devhap(HOST) )
     +      echoho = .FALSE.
         if ( hardec .eq. ECHDSP .or. .not. devhap(FSTORE) )
     +      echofs = .FALSE.
c
c        Loop over the devices doing the business
c
         do 10 device = 1, NDEVIC
c
c           Calculate the new cursor position
c
            deltax(device) = deltax(device) + delx
            deltay(device) = deltay(device) + dely
            newx = curxpo(device) + deltax(device) / devmxs(device)
            newy = curypo(device) + deltay(device) / devmys(device)
c
c           Decide if we need to move the cursor
c
            if ( newx .ne. curxpo(device) .or.
     +           newy .ne. curypo(device) ) then
                  moveit = .TRUE.
            else
               moveit = .FALSE.
            endif
c
c           Any cursor movement needed?
c
            if ( moveit ) then
c
c              Yes, so echo the new cursor position if needed
c
               if ( (device .eq. HOST   .and. echoho) .or.
     +              (device .eq. FSTORE .and. echofs) ) then
c
c                 Check new position is on the device
c
                  if ( newx .lt. 0 ) newx = 0
                  if ( newy .lt. 0 ) newy = 0
c
                  devxma = devxsi(device) - 1
                  if ( newx .gt. devxma ) newx = devxma
                  devyma = devysi(device) - 1
                  if ( newy .gt. devyma ) newy = devyma
c
c                 And move the cursor
c
                  call devcmo ( device, newx, newy )
               else
                  newx = 0
                  newy = 0
               endif
               deltax(device) = 0
               deltay(device) = 0
c
c              Save the new cursor position
c
               curxpo(device) = newx
               curypo(device) = newy
            endif
10       continue
      else
c
c        Calculate the new cursor position
c
         do 20 device = 1, NDEVIC
            newx = (curmox(device) - devxof(device)) / devmxs(device)
            newy = (curmoy(device) - devyof(device)) / devmys(device)
c
c           Decide if we need to move the 'cursor', i.e. we have
c           a new position in the interface window.
c
            if ( newx .ne. curxpo(device) .or.
     +           newy .ne. curypo(device) ) then
               curxpo(device) = newx
               curypo(device) = newy
            endif
20       continue
      endif
c
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
c
c----------------------------------------------------------------------
c
c       SUBROUTINE UIXXRM ( DEVICE, XPOS, YPOS )
c       ----------------------------------------
c
c       PARAMETERS:
c
c       integer device : INPUT - the device for which to set the
c                        mouse position.
c
c       integer xpos, ypos : INPUT - the new mouse position.
c
c       Sets the mouse position for the given device to the given
c       values.
c
c----------------------------------------------------------------------
c
      subroutine uixxrm ( device, xpos, ypos )
c     ========================================
c
      integer device, xpos, ypos
c
      include 'UIFCOM'
      include 'UIXCOM'
c
c     Set the values
c
      curmox(device) = xpos
      curmoy(device) = ypos
c
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
