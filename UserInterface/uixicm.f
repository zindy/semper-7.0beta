c----------------------------------------------------------------------
c
c       LOGICAL FUNCTION UIXICM ( DUMMY )
c       ---------------------------------
c
c       PARAMETERS:
c
c       integer dummy : INPUT - dummy variable
c
c       Checks whether an interactive action is valid wrt any mandatory
c       panels which may be showing.
c
c       Function returns TRUE if action would be OK, otherwise FALSE.
c
c----------------------------------------------------------------------
c
      logical function uixicm ( ) !dummy )
c     =================================
c
!     integer dummy
c
      include 'UIFCOM'
      include 'UIXCOM'
c
c     LOCAL VARIABLES:
c
c     Return status
      logical status
c
c     Device on which mandatory panel is located
      integer device
c
c     Loop counter
      integer i
c
c     If a mandatory panel is not showing, then everything must be OK.
c
      status = .TRUE.
      if ( mpansh ) then
c
c        Panel was showing: was the interaction with it?
c
         device = panode(mpanid)
c      write ( 6, 100 ) pidl(device), mpanid
c100   format ( ////' pidl(device) = ', i3,' mpanid = ', i3 )
c      call lib$wait ( 1 )
         if ( pidl(device) .ne. mpanid ) then
c
c           No, so was it with a menu panel belonging to the mandatory
c           panel?
c
            do 1 i = MINMEN, MAXMEN
               if ( mennam(i) .ge. 0 ) then
                  if ( menpan(i) .eq. pidl(device) ) then
c
c                    Menu panel id is now in i.  Check to see
c                    if that menu is on the mandatory panel
c
                     if ( menopa(i) .eq. mpanid ) then
c
c                       All is OK.
c
                        goto 3
                     else
c
c                       Nope, on another panel.  Can't interact
c
                        goto 2
                     endif
                  endif
               endif
1           continue
2           continue
c
c           Here is interaction is no good.
c
            status = .FALSE.
c
3           continue
         endif
      endif
c
c     All done
c
      uixicm = status
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
