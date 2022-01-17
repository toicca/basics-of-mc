program ex4p1
    use mt_mod
    implicit none
    integer, parameter:: ik=selected_int_kind(11), rk=selected_real_kind(11,100)
    integer(kind=ik) :: i, j, h, l, Np, runs=500
    real(kind=rk), parameter :: pi = 4.0*atan(1.0), k=8.6173324E-5, T=600
    real(kind=rk) :: x, x0, x1, trial, state, u, u1, a=1/real(2,rk)*k*T, mean, E_theor=3/2*k*T, sumrun=0, x_max, b
    real(kind=rk), allocatable :: E_MC(:), means(:), rms(:)

    b=sqrt(2/pi)*1/(a**3)
    
    call init_genrand64(int(123465, ik))

    open(1, action="write", file="data_max.txt", status="replace", position="append")
    open(2, action="write", file="data_points.txt", status="replace", position="append")

    allocate(means(runs))
    allocate(rms(1000))

    !Data for (E_max, RMS) plot with N=10^3 
    Np = 10**3
    allocate(E_MC(Np))
    !Vary E_max from 0.001 to 0.1
    do l=1, 100
        !Markov Chain
        x_max=0.001*l       !0b
        do h=1, runs
            !Inital config.
            x = 3/real(2,rk)*k*T    !0a
            do i=1, Np
                state = rho(x)      !0c
                x0 = x              !1
                u = genrand64_real1()*2-1   !2
                x1 = x + u*x_max    !3
                trial = rho(x1)     !4

                if (trial/state >= 1) then
                    x = x1
                    state = trial   !5
                else if (trial/state >= genrand64_real1()) then
                    x = x1
                    state = trial   !5
                else
                    x = x0
                end if
                E_MC(i) = abs(x)    !6
            end do

            means(h)=sum(E_MC)/real(Np, rk) !Mean energy of distribution
        end do

        !Sum inside RMS   
        do i=1, runs
            sumrun=sumrun+(means(i)-E_theor)**2
        end do

        !RMS for x_max=0.001*l
        rms(l)=sqrt(1/real(runs,rk)*sumrun)
        sumrun=0

        print *, l*0.001,"done."
    end do

    !Save data
    do l=1, 1000
        write(1, *) rms(h), 0.001*l
    end do
    deallocate(E_MC)
    deallocate(rms)

    print*, "E_max done"

    allocate(rms(6))
    
    !Set x_max according to previous results
    x_max=0.05

    !Markov Chain as before, but now with looping over N_points
    do j=2, 7
        Np = 10**j
        allocate(E_MC(Np))
        do h=1, runs
            x = 3/real(2,rk)*k*T
            do i=1, Np
                x0 = x
                E_MC(i) = abs(x)
                state = rho(x)
                u = genrand64_real1()*2-1
                x1 = x + u*x_max
                trial = rho(x1)
                if (trial/state >= 1) then
                    x = x1
                    state = trial
                else if (trial/state >= genrand64_real1()) then
                    x = x1
                    state = trial
                else
                    x = x0
                end if
            end do
            means(h)=sum(E_MC)/real(Np, rk)
        end do
        do i=1, runs
            sumrun=sumrun+(means(i)-E_theor)**2
        end do
        rms(j-1)=sqrt(1/real(runs,rk)*sumrun)
        sumrun=0
        write(2, *) rms(j-1), 10**j
        deallocate(E_MC)
        print*, j, "-power done"
    end do
     
    close(1)
    close(2)


contains

real(kind=rk) function rho(E)
    implicit none
    real(kind=rk), intent(in) :: E
    rho=b*E**2*exp(-1/real(2,rk)*(E/a)**2)
end function rho
end program ex4p1