program sand_pile
  use mt19937_64
  implicit none
  integer,parameter :: rk=selected_real_kind(15,307)
  integer,parameter :: pile_unit=10,stat_unit=11
  real(kind=rk) :: u
  integer(kind=8) :: seed
  integer :: i,L,grain,Ngrains,ntopple,pile_write_interval
  integer,allocatable :: h(:)
  logical,allocatable :: topple(:)
  character(80) :: filename
  
  ! Read command line arguments
  call read_arguments(L,Ngrains,seed)
  write(6,'(a,i0,a,i0,a,i0)') 'Starting simulation with L=',L,', Ngrains=',Ngrains,', seed=',seed
  
  call init_genrand64(seed)

  ! Allocate the pile array; allocate two extra spaces below 1 and above L,
  ! so that toppling over the limits doesn't have to be checked as exceptional cases
  allocate(h(-1:L+2))
  allocate(topple(L))
  
  ! Set pile writing so that only 100 frames will be written
  ! If Ngrains is not divisible by 100, add the modulus to the first frame
  pile_write_interval=(Ngrains-mod(Ngrains,100))/100
  
  filename='sand_pile.dat'
  call open_file(pile_unit,filename)
  filename='statistics.dat'
  call open_file(stat_unit,filename)
  
  ! Initialize height everywhere to zero, and uncheck the toppling of all sites
  h=0
  topple=.false.

  call write_stat_header(stat_unit)
  do grain=1,Ngrains
    u=genrand64_real2()
    i=int(u*L)+1
    h(i)=h(i)+1
    
    ntopple=0
    do
      ! Checks each site for the toppling criterion
      call check_toppling(topple,h,L)
      
      ! If no site is marked for toppling, exit the loop
      if (all(topple .eqv. .false.)) then
        exit
      endif
      
      ! Executes the toppling event for each marked site
      do i=1,L
        if (topple(i)) then
          call toppling(i,h,L)
          ntopple=ntopple+1
          topple(i)=.false.
        endif
      end do
    end do
    
    ! Write out one out of 100 frames of pile height profile.
    if (grain>mod(Ngrains,100) .and. mod(grain-mod(Ngrains,100),pile_write_interval)==0) then
      call write_pile(pile_unit,h,L)
    endif
    ! Write out statistics every step:
    ! the current grain, the size of the previous toppling event,
    ! the number of grains in the system, and finally the pile height
    call write_statistics(stat_unit,grain,ntopple,sum(h(1:L)),maxval(h))
  end do
  
  close(pile_unit)
  close(stat_unit)
  
contains

  subroutine check_toppling(topple,h,L)
    ! Checks all sites for the toppling condition;
    ! if height at site i is more than 2 grains higher than the height at either
    ! side, mark the site for toppling
    implicit none
    integer,intent(in) :: L
    integer,intent(in) :: h(-1:L+1)
    logical,intent(out) :: topple(L)
    integer :: i
    
    do i=1,L
      if (h(i)>h(i-1)+2 .or. h(i)>h(i+1)+2) then
        topple(i)=.true.
      endif
    end do
  end subroutine check_toppling
  
  subroutine toppling(i,h,L)
    ! Execute toppling for site i; if the height at both sides is more than
    ! 2 grains lower than at the toppling site, topple to both directions. Othewise, topple
    ! two grains to the side that is more than grains lower than the toppling site.
    implicit none
    integer,intent(in) :: i,L
    integer,intent(inout) :: h(-1:L+2)

    if (h(i)>h(i-1)+2 .and. h(i)>h(i+1)+2) then
      h(i)=h(i)-2
      h(i-1)=h(i-1)+1
      h(i+1)=h(i+1)+1
    elseif(h(i)>h(i-1)+2) then
      h(i)=h(i)-2
      h(i-1)=h(i-1)+1
      h(i-2)=h(i-2)+1
    else
      h(i)=h(i)-2
      h(i+1)=h(i+1)+1
      h(i+2)=h(i+2)+1
    endif
    ! Clear the toppled grains at the overflow sites of h; these grains are removed
    h(-1)=0
    h(0)=0
    h(L+1)=0
    h(L+2)=0
  end subroutine toppling

  subroutine read_arguments(L,Ngrains,seed)
    ! Read command line arguments. Stops the program at an error
    implicit none
    integer,intent(out) :: L,Ngrains
    integer(kind=8),intent(out) :: seed
    integer :: iarg,ios
    character(80) :: arg
    
    iarg=command_argument_count()
    if (iarg/=3) then
      call get_command_argument(0,arg)
      write(0,'(a)') 'Usage: '//trim(arg)//' L Ngrains seed'
      stop
    endif
    
    call get_command_argument(1,arg)
    read(arg,*,iostat=ios) L
    if (ios/=0) then
      write(0,'(a)') '*** Error: invalid system size L='//arg
      stop
    endif
  
    call get_command_argument(2,arg)
    read(arg,*,iostat=ios) Ngrains
    if (ios/=0 .or. Ngrains<100) then
      write(0,'(a)') '*** Error: invalid number of grains Ngrains='//arg
      stop
    endif
  
    call get_command_argument(3,arg)
    read(arg,*,iostat=ios) seed
    if (ios/=0) then
      write(0,'(a)') '*** Error: invalid random number seed='//arg
      stop
    endif

  end subroutine read_arguments
  
  subroutine open_file(unit,filename)
    ! Opens file filename at unit unit
    integer,intent(in) :: unit
    character(80) :: filename
    integer :: ios

    open(unit=unit,file=filename,iostat=ios)
    if (ios/=0) then
      write(0,'(a)') '*** Error opening file '//trim(filename)
      stop
    endif
  end subroutine open_file
  
  subroutine write_pile(unit,h,L)
    ! Writes the pile information; figure out how to
    ! plot or animate this, if you're interested.
    implicit none
    integer,intent(in) :: L,unit
    integer,intent(in) :: h(-1:L+1)
    integer :: i,ios
    
    do i=1,L
      write(unit,'(i10,i10)',iostat=ios) i,h(i)
      if (ios/=0) then
        write(0,'(a)') '*** Error writing to file sand_pile.dat'
        stop
      endif
    end do
  end subroutine write_pile

  subroutine write_stat_header(unit)
    implicit none
    integer,intent(in) :: unit
    integer :: ios
    
    write(unit,'(a10,a10,a10,a10)',iostat=ios) 'grain','ntopple','hsum','hmax'
    if (ios/=0) then
      write(0,'(a)') '*** Error writing to file statistics.dat'
      stop
    endif

  end subroutine write_stat_header

  subroutine write_statistics(unit,grain,ntopple,hsum,hmax)
    implicit none
    integer,intent(in) :: unit,grain,ntopple,hsum,hmax
    integer :: ios
    
    write(unit,'(i10,i10,i10,i10)',iostat=ios) grain,ntopple,hsum,hmax
    if (ios/=0) then
      write(0,'(a)') '*** Error writing to file statistics.dat'
      stop
    endif

  end subroutine write_statistics

end program sand_pile
