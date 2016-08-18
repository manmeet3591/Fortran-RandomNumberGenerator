program testRG
use mod_Precision
use mod_RandomNumberGenerator
implicit none

    class(RandomNumberGenerator), pointer :: myRandomNumberGen

    integer, parameter :: randCount = 20
    integer :: i, A(randCount)
    real(PRECISION) :: B(randCount)

    allocate( myRandomNumberGen )

    do i = 1, 20
        call myRandomNumberGen % getRandomInt( 1, 30, A(i) )
    end do  

    write(*, "(20I4)" ) A
    write(*,*) 

    do i = 1, 20
        call myRandomNumberGen % getRandomReal( 1.0, 30.0, B(i) )
    end do  

    write(*, "(20F8.4)") B
    write(*,*) 

end program
