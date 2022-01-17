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
        if (pm_seed<0) then
           pm_seed=pm_seed + pm_m
        end if
        !Set the seed and result interval
        rand_pm=real(pm_seed,rk)/pm_m
        pm_seed=xor(pm_seed,123459876)
    end function rand_pm
end module MC_randoms
