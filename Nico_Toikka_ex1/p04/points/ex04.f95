program MC_ex01_p04
    use mc_randoms
    implicit none
    real(rk)::a
    real(rk),allocatable::p_lcg(:,:),p_pm(:,:),p_mt(:,:)
    integer::i
    !Initialize
    call seed_lcg(int(15,ik))
    call seed_pm(int(7895,ik))
    call init_genrand64(int(431,ik))
    allocate(p_lcg(100,2),p_pm(100,2),p_mt(100,2))
    !100 points
    do i=1, 100
        p_lcg(i,1)=rand_lcg()
        p_lcg(i,2)=rand_lcg()
        p_pm(i,1)=rand_pm()
        p_pm(i,2)=rand_pm()
        p_mt(i,1)=genrand64_real3()
        p_mt(i,2)=genrand64_real3()
    end do
    !Adjust the interval
    p_lcg=p_lcg*2-1
    p_pm=p_pm*2-1
    p_mt=p_mt*2-1

    !Save the data
    !LCG
    open(1,action="write",file="data_lcg100.txt",status="replace")
    do i=1,100
        write(1,*) p_lcg(i,1), p_lcg(i,2)
    end do
    close(1)
    !PM
    open(1,action="write",file="data_pm100.txt",status="replace")
    do i=1,100
        write(1,*) p_pm(i,1), p_pm(i,2)
    end do
    close(1)
    !MT
    open(1,action="write",file="data_mt100.txt",status="replace")
    do i=1,100
        write(1,*) p_mt(i,1), p_mt(i,2)
    end do
    close(1)
    deallocate(p_lcg,p_pm,p_mt)

    !Data for 10000 points
    allocate(p_lcg(10000,2),p_pm(10000,2),p_mt(10000,2))
    !10000points
    do i=1, 10000
        p_lcg(i,1)=rand_lcg()
        p_lcg(i,2)=rand_lcg()
        p_pm(i,1)=rand_pm()
        p_pm(i,2)=rand_pm()
        p_mt(i,1)=genrand64_real3()
        p_mt(i,2)=genrand64_real3()
    end do
    !Adjust the interval
    p_lcg=p_lcg*2-1
    p_pm=p_pm*2-1
    p_mt=p_mt*2-1

    !Save the data
    !LCG
    open(1,action="write",file="data_lcg10000.txt",status="replace")
    do i=1,10000
        write(1,*) p_lcg(i,1), p_lcg(i,2)
    end do
    close(1)
    !PM
    open(1,action="write",file="data_pm10000.txt",status="replace")
    do i=1,10000
        write(1,*) p_pm(i,1), p_pm(i,2)
    end do
    close(1)
    !MT
    open(1,action="write",file="data_mt10000.txt",status="replace")
    do i=1,10000
        write(1,*) p_mt(i,1), p_mt(i,2)
    end do
    close(1)

    !Region [-0.01,0.01] Seed 1
    !LCG
    i=1
    do
        if(i>1000) exit
        a=rand_lcg()
        if(a<=0.01.and.a>=-0.01) then
            p_lcg(i,1)=a
            do
                a=rand_lcg()
                if(a<=0.01.and.a>=-0.01) then
                    p_lcg(i,2)=a
                    exit
                end if
            end do
            i=i+1
        end if
    end do

    !PM
    i=1
    do
        if(i>1000) exit
        a=rand_pm()
        if(a<=0.01.and.a>=-0.01) then
            p_pm(i,1)=a
            do
                a=rand_pm()
                if(a<=0.01.and.a>=-0.01) then
                    p_pm(i,2)=a
                    exit
                end if
            end do
            i=i+1
        end if
    end do

    !MT
    i=1
    do
        if(i>1000) exit
        a=genrand64_real3()
        if(a<=0.01.and.a>=-0.01) then
            p_mt(i,1)=a
            do
                a=genrand64_real3()
                if(a<=0.01.and.a>=-0.01) then
                    p_mt(i,2)=a
                    exit
                end if
            end do
            i=i+1
        end if
    end do

    !Save the data
    !LCG
    open(1,action="write",file="data_lcg_region1.txt",status="replace")
    do i=1,1000
        write(1,*) p_lcg(i,1), p_lcg(i,2)
    end do
    close(1)
    !PM
    open(1,action="write",file="data_pm_region1.txt",status="replace")
    do i=1,1000
        write(1,*) p_pm(i,1), p_pm(i,2)
    end do
    close(1)
    !MT
    open(1,action="write",file="data_mt_region1.txt",status="replace")
    do i=1,1000
        write(1,*) p_mt(i,1), p_mt(i,2)
    end do
    close(1)

    !Region [-0.01,0.01] Seed 2
    call seed_lcg(int(8239,ik))
    call seed_pm(int(46,ik))
    call init_genrand64(int(20010,ik))
    !LCG
    i=1
    do
        if(i>1000) exit
        a=rand_lcg()
        if(a<=0.01.and.a>=-0.01) then
            p_lcg(i,1)=a
            do
                a=rand_lcg()
                if(a<=0.01.and.a>=-0.01) then
                    p_lcg(i,2)=a
                    exit
                end if
            end do
            i=i+1
        end if
    end do

    !PM
    i=1
    do
        if(i>1000) exit
        a=rand_pm()
        if(a<=0.01.and.a>=-0.01) then
            p_pm(i,1)=a
            do
                a=rand_pm()
                if(a<=0.01.and.a>=-0.01) then
                    p_pm(i,2)=a
                    exit
                end if
            end do
            i=i+1
        end if
    end do

    !MT
    i=1
    do
        if(i>1000) exit
        a=genrand64_real3()
        if(a<=0.01.and.a>=-0.01) then
            p_mt(i,1)=a
            do
                a=genrand64_real3()
                if(a<=0.01.and.a>=-0.01) then
                    p_mt(i,2)=a
                    exit
                end if
            end do
            i=i+1
        end if
    end do

    !Save the data
    !LCG
    open(1,action="write",file="data_lcg_region2.txt",status="replace")
    do i=1,1000
        write(1,*) p_lcg(i,1), p_lcg(i,2)
    end do
    close(1)
    !PM
    open(1,action="write",file="data_pm_region2.txt",status="replace")
    do i=1,1000
        write(1,*) p_pm(i,1), p_pm(i,2)
    end do
    close(1)
    !MT
    open(1,action="write",file="data_mt_region2.txt",status="replace")
    do i=1,1000
        write(1,*) p_mt(i,1), p_mt(i,2)
    end do
    close(1)
    
end program MC_ex01_p04