C       See if we have 2-byte integers defined
        integer I1
        integer*2 I2
        integer*4 t1,t2
        i1=256*256
        I2=i1
        i1=i1+100
        i2=i2+100
        t1=i1
        t2=i2
        if(t1.eq.t2)then
                write(6,*)'OK'
        else
                write(6,*)'FAIL'
        endif
        end

