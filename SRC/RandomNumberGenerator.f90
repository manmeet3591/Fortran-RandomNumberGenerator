module mod_RandomNumberGenerator
use mod_Precision
use mod_OutputLog
!-----------------------------------------------------------------------
! 工作类
!-----------------------------------------------------------------------
type, public :: RandomNumberGenerator

    logical, private :: isInit = .false.

contains

    procedure, private :: init => m_init

    procedure, public :: getInitStatus => c_isInit
    procedure, public :: getRandomInt  => c_randInt
    procedure, public :: getRandomReal => c_randReal

end type RandomNumberGenerator
!-----------------------------------------------------------------------

    private :: m_init
    private :: c_isInit
    private :: m_lcg
    private :: c_randInt
    private :: c_randReal

contains
    
    !* 初始化
    subroutine m_init( this )
    implicit none
        class(RandomNumberGenerator), intent(inout) :: this

        integer :: ised, i, pid
        integer(PRECISION) :: t
        integer, allocatable :: sed(:)

        if( .not. this%isInit ) then
        
            !* 获得种子大小
            call random_seed( size = ised )

            !* 分配种子
            allocate( sed( ised ) )
            
            call system_clock( t )

            !* gfortran
            pid = getpid()

            !* ifort 
            !call getpid(pid)

            t = IEOR( t, int( pid, kind(t) ) ) 

            do i = 1, ised
                call m_lcg( t, sed(i) ) 
            end do

            !* 给定种子
            call random_seed( put = sed )

            deallocate( sed )

            !* 初始化完毕
            this%isInit = .true.

        end if

        return
    end subroutine m_init
    !====


    !* 是否初始化
    subroutine c_isInit( this, init )
    implicit none
        class(RandomNumberGenerator), intent(in)  :: this
        logical,                      intent(out) :: init

        init = this%isInit

        return
    end subroutine c_isInit
    !====

    !* 线性同余算法
    subroutine m_lcg( s, lcg ) 
    implicit none
        integer(PRECISION), intent(inout) :: s
        integer,            intent(out)   :: lcg

        if (s == 0) then
            s = 104729_8
        else
            s = mod(s, 4294967296_8)
        end if

        s = mod( s * 279470273_8, 4294967291_8 )

        lcg = int(mod(s, int(huge(0), 8)), kind(0))

        return
    end subroutine m_lcg
    !====

    !* 生成[M, N]之间的随机整数
    subroutine c_randInt( this, M, N, ans )
    implicit none
        class(RandomNumberGenerator), intent(inout) :: this
        integer,                      intent(in)    :: M, N
        integer,                      intent(out)   :: ans

        real(PRECISION) :: tmp

        if( M > N ) call LogErr("c_randInt: M > N")

        call this%init()
        call random_number( tmp )

        ans = floor( tmp * ( M - N + 1 ) + N )

        return
    end subroutine c_randInt
    !====

    !* 生成[M, N]之间的随机实数
    subroutine c_randReal( this, M, N, ans )
    implicit none
        class(RandomNumberGenerator), intent(inout) :: this
        real(PRECISION),              intent(in)    :: M, N
        real(PRECISION),              intent(out)   :: ans

        real(PRECISION) :: tmp

        if( M > N ) call LogErr("c_randReal: M > N")

        call this%init()
        call random_number( tmp )

        ans = tmp * ( M - N + 1 ) + N 

        return
    end subroutine c_randReal
    !====


end module mod_RandomNumberGenerator
