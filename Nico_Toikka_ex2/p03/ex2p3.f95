program ex2p3
    use MC_randoms
    implicit none
    !Number of bins, shots, tests and expected value
    integer(kind=ik), parameter:: M=100, N=1000000, T=100, E=N/M
    integer(kind=ik), dimension(M):: y=0
    integer(kind=ik):: i, j
    !Variables for results
    real(kind=rk):: chi=0, seed0, rn
    real(kind=rk), dimension(T):: chi_arr

    !We'll use Fortran's RNG to change seed between runs
    call srand(1237654)
    seed0=irand()

    !-----------------------------------------------------------------------------------
    ! LCG
    !-----------------------------------------------------------------------------------
    call seed_lcg(int(seed0,ik))
    do j=1, T
        !Generate a number and place it in an appropriate bin
        !Since the generator gives a number between 0 and 1, we can multiply it by the number of bins
        !and round it to integer to find its correct bin as change to integer rounds down.
        do i=1, N
            rn=rand_lcg()
            y(int(rn*M+1, ik)) = y(int(rn*M+1, ik))+1
        end do
        
        !Chi-squared for this run
        do i=1, M
            chi=chi+((y(i)-E)**2)/E
        end do

        !Save the result and get a new sequence
        chi_arr(j)=chi
        chi=0
        y=0
        seed0=irand()
        call seed_lcg(int(seed0, ik))
    end do

    !Save the data for analysis in Python
    open(1,action="write",file="data_lcg.txt",status="replace")
    do i=1,100
        write(1,*) chi_arr(i)
    end do
    close(1)

    print*, "LCG döne"

    !-----------------------------------------------------------------------------------
    !Park-Miller
    !-----------------------------------------------------------------------------------
    call seed_pm(int(seed0,ik))
    do j=1, T
        do i=1, N
            rn=rand_pm()
            y(int(rn*M+1, ik)) = y(int(rn*M+1, ik))+1
        end do
        
        do i=1, M
            chi=chi+((y(i)-E)**2)/E
        end do

        chi_arr(j)=chi
        chi=0
        y=0
        seed0=irand()
        call seed_pm(int(seed0, ik))
    end do

    open(1,action="write",file="data_pm.txt",status="replace")
    do i=1,100
        write(1,*) chi_arr(i)
    end do
    close(1)

    print*, "PM döne"

    !-----------------------------------------------------------------------------------
    !Mersenne Twister
    !-----------------------------------------------------------------------------------
    call init_genrand64(int(seed0,ik))
    do j=1, T
        do i=1, N
            rn=genrand64_real2()
            y(int(rn*M+1, ik)) = y(int(rn*M+1, ik))+1
        end do
        
        do i=1, M
            chi=chi+((y(i)-E)**2)/E
        end do

        chi_arr(j)=chi
        chi=0
        y=0
        seed0=irand()
        call init_genrand64(int(seed0,ik))
    end do

    open(1,action="write",file="data_mt.txt",status="replace")
    do i=1,100
        write(1,*) chi_arr(i)
    end do
    close(1)

    print*, "MT döne"

    !-----------------------------------------------------------------------------------
    !QCG
    !-----------------------------------------------------------------------------------
    call seed_qcg(int(seed0,ik))
    do j=1, T
        do i=1, N
            rn=rand_qcg()
            !There were strange things happening with some seeds, so I added a condition just to be safe
            !QCG only gives the same 11 values but somehow it managed to overflow
            if(rn<0) then
                rn=rand_qcg()
            end if
            y(int(rn*M+1, ik)) = y(int(rn*M+1, ik))+1
        end do
        
        do i=1, M
            chi=chi+((y(i)-E)**2)/E
        end do

        chi_arr(j)=chi
        chi=0
        y=0
        seed0=irand()
        call seed_qcg(int(seed0, ik))
    end do

    open(1,action="write",file="data_qcg.txt",status="replace")
    do i=1,100
        write(1,*) chi_arr(i)
    end do
    close(1)

    print*, "QCG döne"

    !-----------------------------------------------------------------------------------

end program ex2p3
