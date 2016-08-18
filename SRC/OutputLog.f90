module mod_OutputLog
implicit none

    character(len=180), private :: errFileName  = './ERROR.txt'
    character(len=180), private :: infoFileName = './INFO.txt'
    
    private :: m_outputStr


contains

    !* 输出错误信息
    subroutine LogErr( err )
    implicit none
        character(len=*), intent(in) :: err

        write(*,*) "LogErr: Error! See ERROR.txt for details."

        call m_outputStr( errFileName, err )

        return
    end subroutine LogErr
    !====

    !* 输出信息
    subroutine LogInfo( info )
    implicit none
        character(len=*), intent(in) :: info

        call m_outputStr( infoFileName, info )

        return
    end subroutine LogInfo
    !====


    !* 将字符串输出到指定文件
    subroutine m_outputStr( fileName, str )
    implicit none
        character(len=*), intent(in) :: fileName, str

        logical :: alive

        INQUIRE(file=trim(fileName), exist=alive)
        if( .not. alive ) then
            open(unit=33, file=fileName, form='formatted', status='new')
        else
            open(unit=33, file=fileName, form='formatted', status='old')
        end if

        write(33, *)
        write(33, *) trim(str)
        write(33, *)

        return
    end subroutine m_outputStr
    !====


end module mod_OutputLog
