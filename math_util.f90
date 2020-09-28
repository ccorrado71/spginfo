 MODULE math_util

 real, parameter, private :: EPSS = 1.0E-05

 private :: equal_vector_r, equal_vector_i
 interface equal_vector
     module procedure equal_vector_r, equal_vector_i
 end interface

 private :: equal_matrix_r, equal_matrix_i
 interface equal_matrix
     module procedure equal_matrix_r, equal_matrix_i
 end interface

 contains

   logical function is_nan(x)
#ifdef __INTEL_COMPILER
   USE ieee_arithmetic
   real, intent(in) :: x
   is_nan = ieee_is_nan(x) 
#else
   USE strutil
   real, intent(in) :: x
   is_nan = (trim(r_to_s(x)) == 'NaN')
#endif
   end function is_nan

!---------------------------------------------------------------------------------------------------------
  
   function derivative(a)  result(der)
!
!  First derivative of vector using 2-point central difference
!
   real, dimension(:), intent(in) :: a
   real, dimension(size(a))       :: der
   integer                        :: n
!
   n = size(a)
   der(1) = a(2) - a(1)
   der(n) = a(n) - a(n-1)
!
   der(2:n-1) = a(3:n) - a(1:n-2) / 2
!
   end function derivative

!---------------------------------------------------------------------------------------------------------

   function straight_line(x1,y1,x2,y2) result(line)
!
!  Calculate straigt line between 2 points: y=line(1)*x + line(2)
!
   real, intent(in)   :: x1,y1,x2,y2
   real, dimension(2) :: line
!
   line(1) = (y2 -y1) / (x2 -x1)
   line(2) = -line(1)*x1 + y1
!
   end function straight_line

!---------------------------------------------------------------------------------------------------------

   function line_line_intersection(l1,l2,ier)  result(xp)
!
!  Intersection of two lines l1(1)x+l1(2), l2(1)x+l2(2)
!
   real, dimension(2), intent(in) :: l1,l2
   integer, intent(out)           :: ier
   real, dimension(2)             :: xp
!
   if (abs(l1(1) - l2(1)) <= epsilon(1.0)) then  ! parallel lines
       ier = 1
       xp(:) = 0
   else
       ier = 0
       xp(1) = (l2(2) - l1(2)) / (l1(1) - l2(1))
       xp(2) = (l1(1)*l2(2) - l2(1)*l1(2)) / (l1(1) - l2(1))
   endif
!
   end function line_line_intersection

!---------------------------------------------------------------------------------------------------------

   real function integrate(x,y,n1,n2)  result(area)
!
!  Integrate array in range n1-n2 with n2 >= n1
!
   real, dimension(:), intent(in) :: x,y
   integer, intent(in)            :: n1,n2
!
   if (n1 == n2) then
       area = y(n1)
   else
       area = 0.5 * sum((x(n1+1:n2) - x(n1:n2-1)) * (y(n1+1:n2) + y(n1:n2-1)))
   endif
!
   end function integrate

!---------------------------------------------------------------------------

   logical function equal_matrix_r(mat1,mat2)
!
!  mat1 is equal to mat2 ?
!
   real, dimension(:,:), intent(in) :: mat1,mat2
   equal_matrix_r = all (abs(mat1 - mat2) < EPSS)
   end function equal_matrix_r

!---------------------------------------------------------------------------

   logical function equal_matrix_i(mat1,mat2)
!
!  mat1 is equal to mat2 ?
!
   integer, dimension(:,:), intent(in) :: mat1,mat2
   equal_matrix_i = all (abs(mat1 - mat2) == 0)
   end function equal_matrix_i

!---------------------------------------------------------------------------

   logical function equal_vector_r(vet1,vet2)
!
!  vet1 is equal to vet2 ?
!
   real, dimension(:), intent(in) :: vet1,vet2
   equal_vector_r = all (abs(vet1 - vet2) < EPSS)
   end function equal_vector_r

!---------------------------------------------------------------------------

   logical function equal_vector_i(vet1,vet2)
   integer, dimension(:), intent(in) :: vet1,vet2
   equal_vector_i = all (abs(vet1 - vet2) == 0)
   end function equal_vector_i

 END MODULE math_util
