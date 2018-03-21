      subroutine uixevp ( type, datum )
c     =================================
c
      integer type, datum
c
      include 'UIFCOM'
      include 'UIXCOM'
c
c     LOCAL VARIABLES:
c
c     index into queue for data
      integer ind
c
c     Insert into the queue, provided its not full
c
      ind = IAND ( qinto + 1, QMASK )
      if ( ind .ne. qread ) then
         qinto = ind
         evtype(qinto) = type
         evdata(qinto) = datum
c-------------
c         call uixxep ( type, datum )
c-------------
      endif
c
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
c
      subroutine uixevg ( type, datum )
c     =================================
c
      integer type, datum
c
      include 'UIFCOM'
      include 'UIXCOM'
c
c     Read from the queue, provided its not empty
c
      if ( qread .ne. qinto ) then
         qread = IAND ( qread + 1, QMASK )
         type  = evtype(qread)
         datum = evdata(qread)
      endif
c
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
c
      logical function uixevr ( dummy )
c     =================================
c
      integer dummy
c
      include 'UIFCOM'
      include 'UIXCOM'
c
c     Return TRUE if something is ready in the queue
c
      if ( qread .ne. qinto ) then
         uixevr = .TRUE.
      else
         uixevr = .FALSE.
      endif
c
      return
      IJUNK = DUMMY
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
c-------------
c      subroutine uixxep ( type, datum )
cc     =================================
cc
c      integer type, datum
cc
c      include 'UIFCOM'
c      include 'UIXCOM'
cc
c      logical status
c      character*6 types(6)
c      integer oldx, oldy
c      logical uifssu
c      data types /'ENTERS','SELECT', 'LEAVES', 'KEYBRD', 'ACTION',
c     +            'A+FREE'/
cc
c      status = uifssu ( )
c      write ( 6, 1010 ) types(type)
c1010  format ( ' Queuing ', a, ' event' )
c      if ( type .eq. EVENTR .or. type .eq. EVSELE .or.
c     +     type .eq. EVLEFT ) then
c         status = uifssu ( )
c         if ( datum .ge. MINCEL .and. datum .le. MAXCEL ) then
c             write ( 6, 1020 ) 'CELL', datum
c         else if ( datum .ge. MINMEN .and. datum .le. MAXMEN ) then
c             write ( 6, 1020 ) 'MENU', datum
c         else if ( datum .ge. MINTEX .and. datum .le. MAXTEX ) then
c             write ( 6, 1020 ) 'TEXTFIELD', datum
c         endif
c      endif
c1020  format ( ' On ', a, ' with id ', i3 )
cc
c      return
cc
cc Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
cc
c      end
c-------------
