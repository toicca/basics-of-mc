module MC_randoms
    use mt_mod
    implicit none
    integer,parameter:: ik=selected_int_kind(11)
    integer,parameter:: rk=selected_real_kind(11,11)
    !LCG parameters.
    integer(kind=ik),parameter::lcg_m=113829760, lcg_a=10671541, lcg_c=3
    integer(kind=ik)::lcg_seed
    !Park-Miller parameters
    integer(kind=ik),parameter::pm_m=2147483647, pm_a=16807,pm_q=127773,pm_r=2836
    integer(kind=ik)::pm_seed

contains
    !LCG routines
    subroutine seed_lcg(seed)
        implicit none
        integer(kind=ik),intent(in)::seed
        lcg_seed=seed
    end subroutine seed_lcg

    real(rk) function rand_lcg()
        implicit none
        integer(kind=ik)::lcg_r
        lcg_r=mod(int(lcg_a*lcg_seed+lcg_c,ik),int(lcg_m,ik))
        !Return value between [0,1)
        rand_lcg=real(lcg_r,rk)/lcg_m
        !Change the value for I
        lcg_seed=lcg_r
    end function rand_lcg

    !Park-Miller routines
    subroutine seed_pm(seed)
        implicit none
        integer(kind=ik),intent(in)::seed
        pm_seed=seed
    end subroutine seed_pm

    real(rk) function rand_pm()
        implicit none
        integer(kind=ik)::k
        !Schrage approximations with the xor stuff from course material
        pm_seed=xor(pm_seed,123459876)
        k=pm_seed/pm_q
        pm_seed=pm_a*(pm_seed-k*pm_q)-pm_r*k
        !Adjust to correct interval
        if (rand_pm<0) then
           rand_pm=rand_pm + pm_m
        end if
        !Set the seed and result interval
        rand_pm=real(pm_seed,rk)/pm_m
        pm_seed=xor(pm_seed,123459876)
    end function rand_pm
end module MC_randoms

program MC_ex01_p03
    use MC_randoms
    implicit none
    integer::i
    real(rk)::a,b,c,a0,b0,c0
    real(rk),allocatable::rng_arr(:),temp_arr(:)
    !Problem 3 part a:
    call seed_lcg(int(15,ik))
    a=rand_lcg()
    call seed_pm(int(7895,ik))
    b=rand_pm()
    call init_genrand64(int(431,ik))
    c=genrand64_real3()
    print *,"LCG value: ",a
    print *,"Park-Miller value: ",b
    print *,"Mersenne twister: ",c

    !Problem 3 part b:
    !LCG repeat interval:
    i=1
    do  
        a0=rand_lcg()
        if (a0==a) exit
        i=i+1
    end do
    print*, "LCG repeat interval:",i
    !Park-Miller repeat interval:
    i=1
    do  
        b0=rand_pm()
        if (b0==b) exit
        i=i+1
    end do
    print*, "Park-Miller repeat interval:",i
    !Mersenne twister repeat interval:
    i=1
    do  
        c0=genrand64_real3()
        if (c0==c) exit
        i=i+1
    end do
    print*, "Mersenne twister repeat interval:",i

end program MC_ex01_p03
