Module program_mod

implicit none

CONTAINS

   subroutine program_arguments(strarg)
   USE strutil
   character(len=:), allocatable, intent(out) :: strarg
   character(len=100)                         :: str
   integer                                    :: narg, iargc
!
   narg = iargc()
   select case (narg)
      case (0)
        do
           write(unit=*,fmt='(a)')'Insert Space Group Symbol or Space Group Number: '
           read(unit=*,fmt='(a)') str
           if (len_trim(str) > 0) exit
        enddo
        strarg = str

      !case (1)
      !  call getarg(1,str)
      !  strarg = str

      case (1:)
        call get_command(str)
        call cutst(str)
        strarg = str

   end select
!
   end subroutine program_arguments

End Module program_mod
