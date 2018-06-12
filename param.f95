program param

implicit none
integer, parameter :: dp = selected_real_kind(14,200)
integer :: i, j, nlines
real(dp) :: z_min, z_max
real, DIMENSION(4,4) :: A

!read (*,*) z_min, z_max, nlines

OPEN (10, file='Text File.txt',FORM='FORMATTED',STATUS='OLD',ACTION='READ')
READ(10,*) ((A(i,j),i=1,4),j=1,4)
PRINT*, A(4,3)
!OPEN (2, file='jla_lcparams_limited.txt',FORM='FORMATTED',STATUS='NEW',ACTION='WRITE')

!DO i=2,nlines
 !  READ (1,*) A(i,2)
  !   IF (A(i,2)>=z_min .AND. A(i,2)<=z_max) THEN
   !         WRITE(2,*) A(i,2)
     
  ! READ (1,*) A(i,4)
  ! WRITE(2,*) A(i,4)
   !  END IF
!END DO



end program param
