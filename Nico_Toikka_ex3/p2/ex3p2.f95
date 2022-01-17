program ex3p2
    use mt_mod
    implicit none
    integer, parameter:: ik=selected_int_kind(11), rk=selected_real_kind(11,100)
    real(kind=rk), parameter :: pi = 4.0*atan(1.0)
    real(kind=rk) :: fsum=0, fmean, V, r=1, err
    integer(kind=ik), parameter :: N=100000
    integer(kind=ik) :: d, i

    call init_genrand64(int(123465, ik))

    do d=1, 15
        !Count the number of hits
        do i=1, N 
            fsum=fsum+f(d)
        end do

        !Get the statistics, formula for error from lecture notes
        fmean=fsum/N
        err=(2*r)**d*sqrt(fsum-fsum**2/N)/N

        !Volume, with V_e being the volume of a n-cube (or d-cube with these variables)
        V=(2*r)**d*fmean

        !Print results and reset sum for next dimension
        print('(A, I2, 4X, A, f6.4, 4X, A, f6.4)'),"Dimension: ",d,"Volume: ",V,"Error: +/- ",err
        fsum=0
    end do

contains
real(kind=rk) function f(dim)
    integer(kind=ik), intent(in) :: dim
    integer(kind=ik) :: k
    real(kind=rk), dimension(dim) :: arr
    real(kind=rk) :: sq

    !Generate a list of random points on interval [-r, r] based on given dimension
    arr=[((r*(2*genrand64_real1()-1))**2, k=1, dim, 1)]

    !Check if the random coordinates hit inside the sphere
    sq=sum(arr)
    if(sq<(r*r)) then
        f=1     !hit
    else
        f=0     !no hit
    end if
end function f
end program ex3p2
