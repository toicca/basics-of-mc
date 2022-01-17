module mod_QCG
    implicit none
    integer,parameter:: ik=selected_int_kind(11)
    integer,parameter:: rk=selected_real_kind(11, 11)
    !QCG parameters
    integer(kind=ik),parameter:: qcg_m=11, qcg_a=11, qcg_b=1, qcg_c=5
    integer(kind=ik):: qcg_seed

contains
    subroutine seed_qcg(seed)
        implicit none
        integer(kind=ik), intent(in):: seed
        qcg_seed=seed
    end subroutine seed_qcg

    real(rk) function rand_qcg()
        implicit none
        qcg_seed=mod(qcg_a*qcg_seed**2+qcg_b*qcg_seed+qcg_c, qcg_m)
        rand_qcg=real(qcg_seed, rk)/real(qcg_m, rk)
    end function rand_qcg     
end module

program ex2p1
    use mod_QCG
    implicit none
    integer(kind=ik)::i
    real(kind=rk)::x0, x
    !Set seed
    call seed_qcg(int(345343, ik))

    !First value
    x0=rand_qcg()
    print*, x0
    i=1
    do  
        x=rand_qcg()
        print*,x
        if (x==x0) exit
        i=i+1
    end do
    print*, "QCG repeat interval:",i
end program ex2p1