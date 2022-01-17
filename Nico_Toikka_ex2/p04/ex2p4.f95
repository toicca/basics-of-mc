program ex2p4
    use MC_randoms
    implicit none
    integer(kind=ik), parameter :: N=100000, NTAB=32
    integer(kind=ik) :: i, j, k
    real(kind=rk), dimension(N) :: C=0, x
    real(kind=rk), dimension(NTAB) :: RN_TAB
    real(kind=rk) :: sqrmean=0, meanpow=0, numerator=0, RN, iy

    !-----------------------------------------------------------------------------------
    !LCG without the shuffle
    !-----------------------------------------------------------------------------------
    
    !Initialization of RN's and variables that don't change with k
    call seed_lcg(int(876123, ik))
    x = [(rand_lcg(), i=1, N, 1)]

    !<x_i>^2
    sqrmean = sum(x)/N
    sqrmean = sqrmean*sqrmean

    !<x_i^2>
    do i=1, N
        meanpow=meanpow+x(i)*x(i)
    end do
    meanpow=meanpow/N

    !Function itself
    do k=1, N-1

        !<x_(i+k)*x_i> term and numerator
        do i=1, N-k
            numerator=numerator+x(i+k)*x(i)
        end do
        numerator=numerator/(N-k)-sqrmean

        !Combine the values and reset the numerator
        C(k)=numerator/(meanpow-sqrmean)
        numerator=0
    end do

    !Save the results
    open(1,action="write",file="data_lcg.txt",status="replace")
    do i=1, N-1
        write(1,*) i, c(i) 
    end do
    close(1)

    !-----------------------------------------------------------------------------------
    !Park-Miller similarly to LCG
    !-----------------------------------------------------------------------------------

    call seed_pm(int(987123, ik))
    
    !Variables
    x = [(rand_pm(), i=1, N, 1)]
    sqrmean = sum(x)/N
    sqrmean = sqrmean*sqrmean
    do i=1, N
        meanpow=meanpow+x(i)*x(i)
    end do
    meanpow=meanpow/N

    !Function
    do k=1, N-1
        do i=1, N-k
            numerator=numerator+x(i+k)*x(i)
        end do
        numerator=numerator/(N-k)-sqrmean
        C(k)=numerator/(meanpow-sqrmean)
        numerator=0
    end do

    !Saving
    open(1,action="write",file="data_pm.txt",status="replace")
    do i=1, N-1
        write(1,*) i, c(i) 
    end do
    close(1)

    !-----------------------------------------------------------------------------------
    !LCG with the shuffle
    !-----------------------------------------------------------------------------------
    
    call seed_lcg(int(879961, ik))

    !Shuffle:
    !Generate a table and pick the first number
    RN_TAB=[(rand_lcg(), j=1, NTAB, 1)]
    iy=RN_TAB(1)

    !Use the table to generate an array for the correlation test
    do i=1, N

        !New random number to replace a value from the table
        RN=rand_lcg()

        !Random number from the table with iy
        !iy*(NTAB-1) sets the interval of possible numbers to [0,31] so we add one to it
        iy=RN_TAB(int(iy*(NTAB-1))+1)

        !Change the "taken" value to the new RN and store the taken value for the test
        RN_TAB(int(iy*(NTAB-1)+1))=RN
        x(i)=iy
    end do

    !Correlation test as before:
    sqrmean = sum(x)/N
    sqrmean = sqrmean*sqrmean
    do i=1, N
        meanpow=meanpow+x(i)*x(i)
    end do
    meanpow=meanpow/N

    !Function
    do k=1, N-1
        do i=1, N-k
            numerator=numerator+x(i+k)*x(i)
        end do
        numerator=numerator/(N-k)-sqrmean
        C(k)=numerator/(meanpow-sqrmean)
        numerator=0
    end do

    !Saving
    open(1,action="write", file="data_lcg_shuffle.txt", status="replace")
    do i=1, N-1
        write(1,*) i, c(i) 
    end do
    close(1)
    
end program ex2p4
