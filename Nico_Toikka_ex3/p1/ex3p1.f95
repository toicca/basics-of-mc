program ex3p1
    use mt_mod
    implicit none
    integer, parameter:: ik=selected_int_kind(11), rk=selected_real_kind(11,100)
    real(kind=rk), parameter :: pi = 4.0*atan(1.0)
    integer(kind=ik), parameter :: N=100000
    integer(kind=ik) :: i
    real(kind=rk), dimension(N) :: x, f
    real(kind=rk) :: u1, u2, y, z, sig=6.4, mu=0, A=3

    call init_genrand64(int(12365, ik))

    !Analytical approach
    do i=1, N
        !RN with interval [0, 1]
        u1 = genrand64_real1()
        !Set to proper distribution (solved in inversion.pdf)
        f(i) = -2*tan(atan(real(5,rk))-2*u1*atan(real(5, rk)))
    end do

    !Analytical+rejection method
    i=1
    do
        if(i==N) exit
        !Box-Muller algorithm
        u1 = genrand64_real2()
        u2 = genrand64_real2()
        x(i) = (sig * sqrt(-2.0 * log(1-u1)) * cos(2*pi*u2) + mu)

        !Values for comparison
        z = 1/(atan(real(5,rk))*(x(i)**2+4))
        y = genrand64_real2()*A*1/((sig*sqrt(2*pi)))*exp(-1/2*(x(i)/sig)**2)

        !Comparison, if it's a hit we calculate next value and x is stored in array x
        if(z>y) then
            i=i+1
        end if

    end do

    !Save the data
    open(1,action="write",file="data.txt",status="replace")
    do i=1, N
        write(1,*) f(i), "Analytical"
        write(1,*) x(i), "Analytical+Rejection"
    end do
    close(1)


end program ex3p1
