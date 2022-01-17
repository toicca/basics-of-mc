program ex3p4b
    implicit none
    integer, parameter:: ik=selected_int_kind(11), rk=selected_real_kind(11,100)
    real(kind=rk), parameter :: pi = 4.0*atan(1.0), I_e=0.946997089979748, a=0, b=10
    real(kind=rk) :: V, r=1, err=0, u, I_mc, start, finish
    real(kind=rk), allocatable :: m(:)
    integer(kind=ik) :: i, j, k, h, N, bins=100, samples=100

    !Code is as in p3, but with genrand's changed to Van der Corput sequence and without error calculations

    print '("DS:")' 
    do i=2, 6
        N=10**i
        allocate(m(N))
        call cpu_time(start)
        do j=1, N
            u = korput(int(j, ik),int(11, ik))*b
            m(j) = f(u)
        end do
        I_mc=(b-a)/real(N,rk)*sum(m)
        call cpu_time(finish)
        print'("N = 10^",i1, ", deltaI = ", f17.15,", time = ", f6.3)', i, abs(I_e-I_mc), finish-start
        open(1,action="write",file="data_vdc.txt",status="replace", position="append")
        write(1,*) abs(I_e-I_mc), i, "DS"
        close(1)
        deallocate(m)
    end do

    print*
    print '("SS:")'
    do i=2, 6
        N=10**i
        allocate(m(N))
        call cpu_time(start)
        do j=1, N
            u = (j+korput(int(j, ik),int(11, ik)))*b/real(N,rk)
            m(j) = f(u)
        end do
        I_mc=(b-a)/real(N,rk)*sum(m)
        call cpu_time(finish)
        print'("N = 10^",i1, ", deltaI = ", f17.15,", time = ", f6.3)', i, abs(I_e-I_mc), finish-start
        open(1,action="write",file="data_vdc.txt",status="old", position="append")
        write(1,*) abs(I_e-I_mc), i, "SS"
        close(1)
        deallocate(m)
    end do

    print*
    print '("HM:")'
    do i=2, 6
        N=10**i
        allocate(m(N))
        call cpu_time(start)
        do j=1, N
            m(j) = hm()
        end do
        I_mc=b*0.4/real(N,rk)*sum(m)
        call cpu_time(finish)
        print'("N = 10^",i1, ", deltaI = ", f17.15,", time = ", f6.3)', i, abs(I_e-I_mc), finish-start
        open(1,action="write",file="data_vdc.txt",status="old", position="append")
        write(1,*) abs(I_e-I_mc), i, "HM"
        close(1)
        deallocate(m)
    end do

    print*
    print '("PSS:")'
    do i=2, 6
        N=10**i
        allocate(m(N))
        call cpu_time(start)
        do j=1, N/bins
            do k=1, bins
                u = (k+korput(int((j-1)+k, ik),int(11, ik)))*b/real(bins,rk)
                m(bins*(j-1)+k) = f(u)
            end do
        end do
        I_mc=(b-a)/real(N,rk)*sum(m)
        call cpu_time(finish)
        print'("N = 10^",i1, ", deltaI = ", f17.15,", time = ", f6.3)', i, abs(I_e-I_mc), finish-start
        open(1,action="write",file="data_vdc.txt",status="old", position="append")
        write(1,*) abs(I_e-I_mc), i, "PSS"
        close(1)
        deallocate(m)
    end do

    print*
    print '("IS:")'
    do i=2, 6
        N=10**i
        allocate(m(N))
        call cpu_time(start)
        do j=1, N
            m(j) = is(j)
        end do
        !1.17762 for normalization
        I_mc=1.17762/real(N,rk)*sum(m)
        call cpu_time(finish)
        print'("N = 10^",i1, ", deltaI = ", f17.15,", time = ", f6.3)', i, abs(I_e-I_mc), finish-start
        open(1,action="write",file="data_vdc.txt",status="old", position="append")
        write(1,*) abs(I_e-I_mc), i, "IS"
        close(1)
        deallocate(m)
    end do
    
contains
real(kind=rk) function f(x)
    implicit none
    real(kind=rk), intent(in) :: x
    f=exp(x*log(real(2,rk))-2-log(gamma(real(x+1))))
end function

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

real(kind=rk) function hm()
    implicit none
    real(kind=rk) :: x, y, f0
    !Generate a x-coordinate within [a, b]
    x = korput(int(j, ik),int(11, ik))*b

    !Generate a y-coordinate within [0, 0.4], upper limit decided by graph of f(x)
    y = korput(int(j, ik),int(19, ik))*0.4

    !Function's value with x
    f0 = f(x)

    !Check for hit
    if(y<f0) then
        hm=1     !hit
    else
        hm=0     !no hit
    end if
end function hm

real(kind=rk) function is(ind)
    implicit none
    integer(kind=ik),intent(in) :: ind
    integer(kind=ik)::v
    real(kind=rk) :: x, y, f0, x0, y0
    !We choose our distribution to be gaussian, since it fits the given function. 
    !With a gaussian function we can use Box-Muller to distribute the numbers
    !Generate a x and y for Box-Muller
    x = korput(int(ind, ik),int(11, ik))
    y = korput(int(ind, ik),int(19, ik))
    !Gaussian distributed number x0 with A=1.5, mu=1.5 and sigma=1.9 from Box-Muller
    v=1
    do
        x0 = 1.9 * sqrt(-2*log(x)) * sin(2*pi*y) + 1.5
        if(x0<0) then
            x = korput(int(ind+v, ik),int(11, ik))
            y = korput(int(ind+v, ik),int(19, ik))
        else 
            exit
        end if
        v=v+1
    end do
    !Value of g with x0
    y0 = 1.5/(1.9*sqrt(2*pi))*exp(-1/real(2, rk)*((x0-1.5)/1.9)**2)
    
    !Value of function to be integrated with x0
    f0 = f(x0)

    !Return the ratio of the functions
    is=f0/y0

end function is
end program ex3p4b