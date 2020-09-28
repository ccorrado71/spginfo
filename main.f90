   program spginfo
   USE program_mod
   USE strutil
   USE spginfom
   USE fileutil
   USE nr
   use errormod
   implicit none
   character(len=:), allocatable :: string
   integer                       :: val
   integer                       :: ier
   integer                       :: len
   type(spaceg_type)             :: spaceg
   logical                       :: sfound
   integer                       :: j,i
   integer                       :: icode
   integer                       :: sunit
   integer                       :: nsymop
   type(symop_type)              :: symoptemp
   character(len=80)             :: line
   type(symop_type), dimension(192) :: symop
!     integer, dimension(230) :: vetz = 0
!   integer :: inis,ends
   character(len=20), dimension(530) :: spgsym
   integer, dimension(530)           :: freq
   integer :: nspgsym,kpos
   integer, dimension(:), allocatable :: iord
   type(error_type) :: err
!
   call load_spg_database(err)
!  leggi argomenti da linea di comando
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
           !do j=1,230
           !   sfound = .true.
           !   icode = 1
           !   do while (sfound)
           !      call spg_load(spaceg,spgnum=j,sfound=sfound,code=icode)
           !      if (sfound) then
           !          write(6,'(1x,100("="))')
           !          call spaceg%prn()
           !      endif
           !   enddo
           !enddo
           do i=1,230
              do j=1,spg_index(i)%nat
                 kpos = spg_index(i)%pos(j)
                 call spg_data(kpos)%prn()
                 write(6,'(1x,100("="))')
              enddo
           enddo
  
         case ('--version')
           write(0,*)'spginfo library 13.2.2013'

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
                         !call read_egrfile()
                         !  stop
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
