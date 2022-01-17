!Program follows algorithm for TGFSR generator given in lecture notes
program ex2p2
    implicit none
    integer, parameter:: ik=selected_int_kind(10)
    !Parameters and initial array
    integer(kind=ik)::i, k=0    !i is used just as a place holder variable
    integer(kind=ik), parameter:: w=16, p=25, q=11, a=43125,  n=25
    integer(kind=ik), dimension(n):: m=[(i, i=10, n+9, 1)]
    
    !a0 right shift by one bit
    i=RSHIFT(m(k+1), 1)

    !XOR operations do not require a specific order, so we start with the XOR
    !where the other element in the operation is chosen based on the LSB

    !Check the LSB and choose the correct XOR based on it
    if(BTEST(i, 0)) then
        i=XOR(i, a)
    else
        i=XOR(i, 0)
    end if

    !XOR with modulo and check the result
    m(k+1)=XOR(m(MOD(k+q, p)+1), i)
    print*, m(k+1)  !Gives 43109

end program ex2p2
