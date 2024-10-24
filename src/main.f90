module test_space

implicit none

contains

   subroutine test_spg()
   use spginfom
   use unit_cell
   type(spaceg_type) :: spg,std_spg
   real, dimension(6) :: cellpar = [10.696,18.813,6.816,90.,111.30,90.]
   type(cell_type) :: cell,cellnew
   real, dimension(3,3) :: pmat
   real, dimension(3)      :: pvet
   integer                 :: i
!
   spg = init_spaceg_type('P 21/n')
   if (.not.spg%standard) then
       write(6,'(a)')trim(spg%symbol_xhm)//' is not standard'
       std_spg = standard_spg(spg)
       write(6,'(a)')'Standard is '//trim(std_spg%symbol_xhm)
       !call string_to_matrix(spg%pmat1,pmat,pvet)
       call spg%get_pmat1(pmat,pvet)
       do i=1,3
          write(6,'(3f10.3,f10.2)')pmat(i,:),pvet(i)
       enddo
   endif
   cell = set_cell_type(cellpar)
   write(6,'(a)')'Input cell in '//trim(spg%symbol_xhm)
   call cell%write(6)
   call cell_transform(cell,pmat,cellnew)
   write(6,'(a)')'Output cell in '//trim(std_spg%symbol_xhm)
   call cellnew%write(6)

   !P21/n -> P21/a 
   !call string_to_matrix('c,b,-a-c',pmat1,pvet1)
   !do i=1,3
   !   write(6,'(3i5,f10.2)')pmat1(i,:),pvet1(i)
   !enddo
   !pmat2 = matmul(pmat,pmat1)
   !do i=1,3
   !   write(6,'(3i5,f10.2)')pmat2(i,:)
   !enddo
!
   end subroutine test_spg

end module test_space

   program spginfo
   USE program_mod
   USE strutil
   USE spginfom
   USE fileutil
   USE nr
   use errormod
   use cmdpath
       use test_space
   USE iso_fortran_env, only: ERROR_UNIT
   implicit none
   character(len=:), allocatable     :: string
   integer                           :: val
   integer                           :: ier
   integer                           :: len
   type(spaceg_type)                 :: spaceg
   logical                           :: sfound
   integer                           :: j,i
   integer                           :: icode
   integer                           :: sunit
   integer                           :: nsymop
   type(symop_type)                  :: symoptemp
   character(len=80)                 :: line
   type(symop_type), dimension(192)  :: symop
   character(len=20), dimension(530) :: spgsym
   integer, dimension(530)           :: freq
   integer :: nspgsym,kpos
   integer, dimension(:), allocatable :: iord
   type(error_type) :: err
   character(len=256) :: path
!
   call get_cmdpath(path,ier=ier)
   if (ier > 0) then
       write(ERROR_UNIT,'(a)')'Error in get_cmdpath'
       go to 10
   endif

   call spg_set_symmetry_file(trim(path)//'syminfo.lib',err)
   if (err%signal) then
       call err%print()
       go to 10
   endif

   call load_spg_database(err)
   if (err%signal) then
       call err%print()
       go to 10
   endif
       !call test_spg()
       !go to 10
!  
   call program_arguments(string)
!
   call s_to_i(string,val,ier,len)
   if (ier == 0) then
!
!      Look up space group from number
       sfound = .true.
       icode = 1
       do while (sfound)
          call spg_load(spaceg,spgnum=val,sfound=sfound,code=icode)
          if (sfound) then
              !if (icode > 2) write(6,'(1x,100("="))')
              call spaceg%prn()
              write(6,'(1x,100("="))')
              icode = icode + 1
          else
              exit
          endif
       enddo
       if (.not.sfound .and. icode == 1) write(0,'(a,i0)') 'Unknown Space Group Number: ',val

   else

       select case (string) 
         case ('-test')
           call spg_database_print()
           !do i=1,230
           !   do j=1,spg_index(i)%nat
           !      kpos = spg_index(i)%pos(j)
           !      call spg_data(kpos)%prn()
           !      write(6,'(1x,100("="))')
           !   enddo
           !enddo
  
         !case ('-list')
         !  call print_extg(6)
           
         case ('--version')
           write(0,*)'spginfo library 29.9.2020'

         case ('--tetragonal')
           nspgsym = 0
           do i=1,size(sg_info)
              if (csys_code_from_num(sg_info(i)%num) == CS_Tetragonal) then 
                  if (i > 1 .and. s_eqidb(sg_info(i)%hm2,sg_info(i-1)%hm2)) cycle
                  nspgsym = nspgsym + 1
                  spgsym(nspgsym) = sg_info(i)%hm2
                  icode = 1
                  call spg_load(spaceg,spgnum=sg_info(i)%num,sfound=sfound,code=icode)
                  freq(nspgsym) = spaceg%freq_no
              endif
           enddo
           allocate(iord(nspgsym))
           call indexx(freq(:nspgsym),iord)
           iord = iord(nspgsym:1:-1)
           spgsym(:nspgsym) = spgsym(iord)
           freq(:nspgsym) = freq(iord)
           do i=1,nspgsym
              write(6,'(a,i10)')spgsym(i)
           enddo

         case default
!
           if (file_exist(string)) then   ! is a file?
!
!              Space group from symmetry operators
               sfound = .false.
               call get_unit(sunit)
               open(sunit,file=string,status='OLD',action='read',iostat=ier)
               if (ier == 0) then
                   nsymop = 0
                   do
                      read(sunit,'(a)',iostat=ier) line
                      if (ier /= 0) exit
                      call string_to_symop(line,symoptemp,ier)
                      if (ier == 0) then
                          nsymop = nsymop + 1
                          if (nsymop > 192) exit
                          symop(nsymop) = symoptemp
                      endif
                   enddo
                   if (nsymop > 0) then
                       call spg_load(spaceg,symop=symop(:nsymop),sfound=sfound)
                       if (sfound) call spaceg%prn()
                   endif
               endif
               if (.not.sfound) write(0,'(a,a)') 'Cannot find space group from file: '//trim(string)
           else
!
!              Space group from string
               call spg_load(spaceg,spgstr=string,sfound=sfound)
               if (sfound) then
                   call spaceg%prn()
               else
                   write(0,'(a,a)') 'Unknown Space Group Symbol: ',trim(string)
               endif
           endif

       end select
   endif
!
10 continue
!
   end program spginfo


