c
c----------------------------------------------------------------------
c
c       SUBROUTINE UIXEPO ( PID, FORWRD )
c       ---------------------------------
c
c       PARAMETERS:
c
c       integer pid : INPUT - the identifier of panel whose elements are
c                     to have their positions adjusted.
c
c       logical forwrd : INPUT - if true, convert positions.  If false,
c                         converts converted positions back.
c
c       Calculates the position of all elements on the given panel
c       taking into account the positioning information of each element
c
c----------------------------------------------------------------------
c
      subroutine uixepo ( pid, forwrd )
c     =================================
c
      integer pid
      logical forwrd
c
      include 'UIFCOM'
      include 'UIXCOM'
      include 'UIFERR'
c
c     LOCAL VARIABLES:
c
c     Loop counter
      integer i
c
c     See whether we are adjusting to take account to the positioning
c     parameters, or doing the reverse (i.e. adjusting adjusted
c     positions back again!)
c
c     Loop over all elements, adjusting
c
      do 10 i = MINELE, MAXELE
c
c        Element in use?
c
         if ( elenam(i) .ge. 0 ) then
c
c           On the correct panel?
c
            if ( eleopa(i) .eq. pid ) then
               if ( forwrd ) then
c
c                 Adjust the position, unless its a cell on a menu,
c                 in which case leave well alone
c
                  if ( i .ge. MINCEL .and. i .le. MAXCEL ) then
                     if ( celome(i) .le. 0 ) then
                        elexpo(i) = elexpo(i)-(elexsi(i)/2) * elehor(i)
                        eleypo(i) = eleypo(i)-(eleysi(i)/2) * elever(i)
c      write ( 6, 100 ) elexpo(i), eleypo(i)
c100   format ( ' Moved cell to ', i3, i3 )
                     endif
                  else
                     elexpo(i) = elexpo(i) - (elexsi(i)/2) * elehor(i)
                     eleypo(i) = eleypo(i) - (eleysi(i)/2) * elever(i)
                  endif
               else
c
c                 Adjust back
c
                  elexpo(i) = elexpo(i) + (elexsi(i)/2) * elehor(i)
                  eleypo(i) = eleypo(i) + (eleysi(i)/2) * elever(i)
               endif
            endif
         endif
10    continue
c
      return
c
c Copyright (C) 1988, 1989:  Synoptics Ltd,  All Rights Reserved
c
      end
