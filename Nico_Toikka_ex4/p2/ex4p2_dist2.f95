program ex4p2_dist2
    use mt_mod
    implicit none
    integer, parameter:: ik=selected_int_kind(11), rk=selected_real_kind(11,100), counts=100, distrs=10000
    integer :: i, j, k
    reaL(kind=rk), dimension(100) :: x, y
    real(kind=rk), dimension(100) :: h=0
    real(kind=ik), dimension(distrs, counts) :: D_k=0
    real(kind=rk) :: mean_t=0, mean_w=0, dx_bar=0.1, u, x_low, x_up, x_bar

    call init_genrand64(int(1234765, ik))

    !Poisson dist.
    do i=1, size(x)
        x(i)=i*dx_bar
        y(i)=poisson((real(i*dx_bar, rk)))
    end do  

    !True mean
    do j=1, size(y)
        mean_t=mean_t+y(j)*x(j)
    end do
    mean_t=mean_t/real(sum(y), rk)

    !Generate synthetic data to D_k and store their means to h
    do i=1, distrs
        D_k(i,:) = 0
        do j=1, counts
            u=real(sum(y), rk)*genrand64_real3()
            k=1
            do
                if(F_k(int(k-1, ik))<u.and.u<=F_k(int(k, ik))) exit
                k=k+1
            end do
            D_k(i,k) = D_k(i,k) + 1
        end do

        do j=1, counts
            mean_w=mean_w+D_k(i,j)*j
        end do
        mean_w=mean_w/sum(D_k(i,:))*0.1

        j=int(mean_w/dx_bar)

        h(j) = h(j) + 1
        mean_w=0
    end do

    !Lower and upper uncertainties
    do k=1, size(h)
        if(H_k(k)<0.165.and.H_k(k)>=0.155) then
            x_low=k*0.1
            exit
        end if
    end do

    do k=1, size(h)
        if(H_k(k)<0.845.and.H_k(k)>=0.835) then
            x_up=k*0.1
            exit
        end if
    end do


    !Results
    print*, "Lower and upper uncertainty respectively:"
    print*, mean_t-x_low, 3.15-mean_t
    print*, "Error assuming Gaussian shape:"
    do i=1, size(x)
        x(i)=x(i)*x(i)
    end do
    x_bar=sum(x)/real(size(x), rk)
    print*,1/sqrt(real(size(x),rk))*sqrt(x_bar-mean_t**2)

    !Save data, this part was changed as needed
    open(1, action="write", file="data_distr2.txt", status="replace", position="append")
    do i=1, size(h)
        write(1, *) H_k(i), i*0.1
    end do
    close(1)


contains

real(kind=rk) function poisson(x_in)
    implicit none
    real(kind=rk), intent(in) :: x_in
    poisson=exp(x_in*log(real(3,rk))-3-log(gamma(real(x_in+1))))
end function

real(kind=rk) function F_k(k)
    implicit none
    integer(kind=ik), intent(in) :: k
    F_k=sum(y(:k))
end function

real(kind=rk) function H_k(k)
    implicit none
    integer, intent(in) :: k
    H_k=sum(h(:k))/real(sum(h), rk)
end function
    
end program ex4p2_dist2