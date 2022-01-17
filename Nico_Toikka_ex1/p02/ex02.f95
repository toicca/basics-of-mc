program MC_ex01_p01
    implicit none
    real,parameter :: pi=4.0*atan(1.0)
    integer::seed=13,iarg,i,j,k,n,hits
    real(8)::results(7,100),l=2,d=10,errors(7)=0
    character(len=80)::arg

    !RNG seed from arguments
    iarg=command_argument_count()
    if (iarg==1) then
        call get_command_argument(1,arg)
        read(arg,*) seed
    end if

    call srand(seed)

    !Simulation part
    n=1
    do i=1,7
        !Update the amount of throws for next simulations
        n=n*10
        !Do 100 simulations with the new throw count
        do j=1,100
            !Simulate a single set of throws
            hits=0
            do k=1,n
                hits=hits+throw()
            end do
            if(hits==0) then
                results(i,j)=0
            else
                results(i,j)=n*2*l/(d*hits)
            end if
            errors(i)=errors(i)+abs(results(i,j)-pi)
        end do
        errors(i)=errors(i)/100
    end do

    !Saving and plotting of the error data
    open(1,action="write",file="data.txt",status="replace")
    do i=1,7
        write(1,*) i, errors(i)
    end do
    close(1)
    call execute_command_line("python plot.py")

contains
    integer function throw()
        implicit none
        real(8)::y,alpha
        !Randomize the position and the angle of the needle.
        !It's only necessary to look at the other half of the throwing space, since the situation can always be mirrored
        y=rand()*d/2
        alpha=rand()*pi/2
        !Check if the end of the needle crosses a line
        if (l/2*sin(alpha)>=y) then
            throw=1
        else
            throw=0
        end if
    end function

end program MC_ex01_p01