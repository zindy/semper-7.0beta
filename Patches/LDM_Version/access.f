C	A little patch for fort77's lack of an access command
	integer function access(path,pmode)
	character*(*) path
	character*1   pmode
	logical rvalue
C	Should add other options sometime...
	rvalue=.false.
	if(pmode.eq.'r')then
		inquire(file=path,exist=rvalue)
	endif
	write(6,*)'Access of ',path,' gives ',rvalue
	if(rvalue)then
                access=0
        else
                access=1
        endif
	return
	end
		
