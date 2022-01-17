program ex3p3
    use mt_mod
    implicit none
    integer, parameter:: ik=selected_int_kind(11), rk=selected_real_kind(11,100)
    real(kind=rk), parameter :: pi = 4.0*atan(1.0), I_e=0.946997089979748, a=0, b=10
    real(kind=rk) :: fsum=0, fmean, f2mean, f2=0, V, r=1, err=0, u, I_mc, start, finish
    real(kind=rk), allocatable :: m(:)
    integer(kind=ik) :: i, j, k, h, N, bins=100, samples=100

    call init_genrand64(int(12354, ik))

    !Each method has it's own block of code with similar structure to other methods. Not pretty, but it gets the job done

    print '("DS:")' 
    !i is the power of N. We loop through the powers while calculating the required information
    do i=2, 6
        N=10**i
        allocate(m(N))
        !if is for error calculation at N=10^3
        if(i==3) then
            !Do the calculation 100 times for error calculation
            do k=1, samples
                !Integral itself with timing. Array m-contains MC-values and f is the function to be integrated.
                call cpu_time(start)
                do j=1, N
                    u = genrand64_real1()*b
                    m(j) = f(u)
                end do
                I_mc=(b-a)/real(N,rk)*sum(m)
                call cpu_time(finish)
                !These are for error
                fsum = fsum + I_mc
                f2 = f2 + I_mc*I_mc
            end do
            !Rest of error calculation
            fmean=fsum/samples
            f2mean=f2/samples
            err=sqrt(abs(f2mean-fmean)/real(samples,rk))
        else
            !Cleaner look at the integration
            call cpu_time(start)
            do j=1, N
                u = genrand64_real1()*b
                m(j) = f(u)
            end do
            I_mc=(b-a)/real(N,rk)*sum(m)
            call cpu_time(finish)
        end if
        !Print and save results
        print'("N = 10^",i1, ", deltaI = ", f17.15,", time = ", f6.3)', i, abs(I_e-I_mc), finish-start

        open(1,action="write",file="data_mt.txt",status="replace", position="append")
        write(1,*) abs(I_e-I_mc), i, "DS"
        close(1)
        deallocate(m)
        
    end do
    print'("Error: ", f8.6)', err
    fsum=0
    f2mean=0

    !As written before, rest of the methods are done similarly to direct sampling,
    !but with required tweaks to the loop of MC-values and to the integral.
    !Hit & miss and importance samling methods use functions that are defined at the end of code

    print*
    print '("SS:")'
    do i=2, 6
        N=10**i
        allocate(m(N))
        if(i==3) then
            do k=1, samples
                call cpu_time(start)
                do j=1, N
                    u = (j+genrand64_real1())*b/real(N,rk)
                    m(j) = f(u)
                end do
                I_mc=(b-a)/real(N,rk)*sum(m)
                call cpu_time(finish)
                fsum = fsum + I_mc
                f2 = f2 + I_mc*I_mc
            end do
            fmean=fsum/samples
            f2mean=f2/samples
            err=sqrt(abs(f2mean-fmean)/real(samples,rk))
        else
            call cpu_time(start)
            do j=1, N
                u = genrand64_real1()*b
                m(j) = f(u)
            end do
            I_mc=(b-a)/real(N,rk)*sum(m)
            call cpu_time(finish)
        end if
        print'("N = 10^",i1, ", deltaI = ", f17.15,", time = ", f6.3)', i, abs(I_e-I_mc), finish-start
        open(1,action="write",file="data_mt.txt",status="old", position="append")
        write(1,*) abs(I_e-I_mc), i, "SS"
        close(1)
        deallocate(m)
    end do
    print'("Error: ", f8.6)', err
    fsum=0
    f2mean=0

    print*
    print '("HM:")'
    do i=2, 6
        N=10**i
        allocate(m(N))
        if(i==3) then
            do k=1, samples
                call cpu_time(start)
                do j=1, N
                    m(j) = hm()
                end do
                I_mc=b*0.4/real(N,rk)*sum(m)
                call cpu_time(finish)
                fsum = fsum + I_mc
                f2 = f2 + I_mc*I_mc
            end do
            fmean=fsum/samples
            f2mean=f2/samples
            err=sqrt(abs(f2mean-fmean)/real(samples,rk))
        else
            call cpu_time(start)
                do j=1, N
                    m(j) = hm()
                end do
                I_mc=b*0.4/real(N,rk)*sum(m)
                call cpu_time(finish)
        end if
        print'("N = 10^",i1, ", deltaI = ", f17.15,", time = ", f6.3)', i, abs(I_e-I_mc), finish-start
        open(1,action="write",file="data_mt.txt",status="old", position="append")
        write(1,*) abs(I_e-I_mc), i, "HM"
        close(1)
        deallocate(m)
    end do
    print'("Error: ", f8.6)', err
    fsum=0
    f2mean=0


    print*
    print '("PSS:")'
    do i=2, 6
        N=10**i
        allocate(m(N))
        if (i==3) then
            do h=1, samples
                call cpu_time(start)
                do j=1, N/bins
                    do k=1, bins
                        u = (k+genrand64_real1())*b/real(bins,rk)
                        m(bins*(j-1)+k) = f(u)
                    end do
                end do
                I_mc=(b-a)/real(N,rk)*sum(m)
                call cpu_time(finish)
                fsum = fsum + I_mc
                f2 = f2 + I_mc*I_mc
            end do
            fmean=fsum/samples
            f2mean=f2/samples
            err=sqrt(abs(f2mean-fmean)/real(samples,rk))
        else
            call cpu_time(start)
            do j=1, N/bins
                do k=1, bins
                    u = (k+genrand64_real1())*b/real(bins,rk)
                    m(bins*(j-1)+k) = f(u)
                end do
            end do
            I_mc=(b-a)/real(N,rk)*sum(m)
            call cpu_time(finish)
        end if
        print'("N = 10^",i1, ", deltaI = ", f17.15,", time = ", f6.3)', i, abs(I_e-I_mc), finish-start
        open(1,action="write",file="data_mt.txt",status="old", position="append")
        write(1,*) abs(I_e-I_mc), i, "PSS"
        close(1)
        deallocate(m)
    end do
    print'("Error: ", f8.6)', err
    fsum=0
    f2mean=0

    print*
    print '("IS:")'
    do i=2, 6
        N=10**i
        allocate(m(N))
        if (i==3) then
            do k=1, samples
                call cpu_time(start)
                do j=1, N
                    m(j) = is()
                end do
                I_mc=1.17762/real(N,rk)*sum(m)
                call cpu_time(finish)
                fsum = fsum + I_mc
                f2 = f2 + I_mc*I_mc
            end do
            fmean=fsum/samples
            f2mean=f2/samples
            err=sqrt(abs(f2mean-fmean)/real(samples,rk))
        else
            call cpu_time(start)
            do j=1, N
                m(j) = is()
            end do
            I_mc=1.17762/real(N,rk)*sum(m)
            call cpu_time(finish)
        end if
        print'("N = 10^",i1, ", deltaI = ", f17.15,", time = ", f6.3)', i, abs(I_e-I_mc), finish-start
        open(1,action="write",file="data_mt.txt",status="old", position="append")
        write(1,*) abs(I_e-I_mc), i, "IS"
        close(1)
        deallocate(m)
    end do
    print'("Error: ", f8.6)', err
    
contains
real(kind=rk) function f(x)
    implicit none
    real(kind=rk), intent(in) :: x
    f=exp(x*log(real(2,rk))-2-log(gamma(real(x+1))))
end function

!Hit & miss function
real(kind=rk) function hm()
    implicit none
    real(kind=rk) :: x, y, f0
    !Generate a x-coordinate within [a, b]
    x = genrand64_real1()*b

    !Generate a y-coordinate within [0, 0.4], upper limit decided by graph of f(x)
    y = genrand64_real1()*0.4

    !Function's value with x
    f0 = f(x)

    !Check for hit
    if(y<f0) then
        hm=1     !hit
    else
        hm=0     !no hit
    end if
end function hm

!Importance sampling
real(kind=rk) function is()
    implicit none
    integer(kind=ik)::v
    real(kind=rk) :: x, y, f0, x0, y0
    !We choose our distribution to be gaussian, since it fits the given function. 
    !With a gaussian function we can use Box-Muller to distribute the numbers
    !Generate a x and y for Box-Muller
    x = genrand64_real1()
    y = genrand64_real1()
    !Gaussian distributed number x0 with A=1.5, mu=1.5 and sigma=1.9 from Box-Muller
    v=1
    do
        x0 = 1.9 * sqrt(-2*log(x)) * sin(2*pi*y) + 1.5
        if(x0<0) then
            x = genrand64_real1()
            y = genrand64_real1()
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

end program ex3p3
