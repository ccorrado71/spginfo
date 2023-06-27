module cmdpath

implicit none

contains

   subroutine get_cmdpath(path,fullpath,ier)
   use ISO_C_BINDING
#if defined(__INTEL_COMPILER)
   use IFPORT
#endif
   interface
       function readlink(path, buf, bufsize) bind(C, NAME = 'readlink')
           import
           integer(C_SIZE_T) :: readlink
           character(KIND = C_CHAR), intent(IN) :: path(*)
           character(KIND = C_CHAR) :: buf(*)
           integer(C_SIZE_T), value :: bufsize
       end function
   end interface

   character(256), intent(out)           :: path
   character(256), intent(out), optional :: fullpath
   integer, intent(out)                  :: ier
   integer                               :: pid, i, idx
   integer(C_SIZE_T)                     :: szret
   character(KIND = C_CHAR)              :: cbuf(256)

   ier = 0
   pid = GETPID()

   write (path, '(i0)') pid
   path = '/proc/'//TRIM(path)//'/exe'

   szret = readlink(TRIM(path)//C_NULL_CHAR, cbuf, SIZE(cbuf, KIND = C_SIZE_T))
   if (szret == -1) then
       !stop 'Error reading link'
       ier = 1
   endif

   path = ''
   do i = 1, SIZE(cbuf)
       if (cbuf(i) == C_NULL_CHAR) exit
       path(i:i) = cbuf(i)
   enddo

   if (present(fullpath)) then
       fullpath = path
   endif
   idx = INDEX(path, '/', BACK = .TRUE.)
   path(idx+1:) = ' '

   end subroutine get_cmdpath

end module cmdpath
