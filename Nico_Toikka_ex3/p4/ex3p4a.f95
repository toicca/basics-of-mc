program ex3p4a
    implicit none
    integer, parameter:: ik=selected_int_kind(11), rk=selected_real_kind(11,100)
    integer(kind=ik)::j
    real(kind=rk) :: res

    print'("Seq. 501...510, base 7")'
    do j=1, 10
        res=korput(int(500+j,ik), int(7,ik))
        print '(i3,": ",f6.4)',500+j, res
    end do
    print*

    print'("Seq. 501...510, base 13")'
    do j=1, 10
        res=korput(int(500+j,ik), int(13,ik))
        print '(i3,": ",f6.4)',500+j, res
    end do
    

contains
real(kind=rk) function korput(index, base)
    implicit none
    integer(kind=ik), intent(in) :: index, base
    integer(kind=ik) :: i
    real(kind=rk) :: f
    i=index
    korput=0
    f=1
    do 
        if(i<=0) exit
        f = f/real(base, rk)
        korput = korput + f*(mod(i,base))
        i=floor(i/real(base, rk))
    end do
end function korput
end program ex3p4a