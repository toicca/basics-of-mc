program MC_ex01_p04_Buffon
    use mc_randoms
    implicit none
    real,parameter :: pi=4.0*atan(1.0)
    integer::seed=13,iarg,j,k,n,hits
    real(8)::result,l=2,d=10,error=0
    character(len=80)::arg

    !RNG seed from arguments, default seed 13
    iarg=command_argument_count()
    if (iarg==1) then
        call get_command_argument(1,arg)
        read(arg,*) seed
    end if

    call seed_lcg(int(seed,ik))

    !Simulation part
    n=10000
    !Do 100 simulations with the new throw count
    do j=1,100
        !Simulate a single set of throws
        hits=0
        do k=1,n
            hits=hits+throw()
        end do
        if(hits==0) then
            result=0
        else
            result=n*2*l/(d*hits)
        end if
        error=error+abs(result-pi)
    end do
    error=error/100

    print*,"Error: ",error


contains
    integer function throw()
        implicit none
        real(rk)::y,alpha
        !Randomize the position and the angle of the needle.
        !It's only necessary to look at the other half of the throwing space, since the situation can always be mirrored
        y=rand_lcg()*d/2
        alpha=rand_lcg()*pi/2
        !Check if the end of the needle crosses a line
        if (l/2*sin(alpha)>=y) then
            throw=1
        else
            throw=0
        end if
    end function

end program MC_ex01_p04_Buffon