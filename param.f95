program param

implicit none
integer, parameter :: dp = selected_real_kind(14,200)
integer :: i, j, nlines, stat, arrays_dimension,p,arrays_dimension_outliers
real(dp) :: z_min, z_max, q_0, j_0
REAL(dp), ALLOCATABLE :: zcmb(:),zhel(:),dz(:),mb(:),dmb(:),xone(:),dxone(:),color(:),dcolor(:),threerdvar(:),dthreerdvar(:),tmax(:)
Real(dp), ALLOCATABLE :: dtmax(:),cov_m_s(:),cov_m_c(:),cov_s_c(:),set(:),ra(:),dec(:),biascor(:),logz(:), mb_02(:)
! character(len=:), ALLOCATABLE :: name(:)
character(len=7) :: name
!read (*,*) z_min, z_max, nlines

read(*,*) z_min
read(*,*) z_max
read(*,*) q_0
read(*,*) j_0

! write(*,*) z_min
! write(*,*) z_max

OPEN (10, file='jla_lcparams.txt',FORM='FORMATTED',STATUS='OLD',ACTION='READ')
arrays_dimension=0
READ(10,*)

Do

    READ(10,*,iostat=stat) 
    
        If (stat .ne. 0) then
            exit
        Else
            arrays_dimension=arrays_dimension+1
        End If
    
End Do

Close(10)

 write(*,*) 'El número de datos es', arrays_dimension


allocate (zcmb(arrays_dimension),zhel(arrays_dimension),dz(arrays_dimension),mb(arrays_dimension),dmb(arrays_dimension))

allocate(xone(arrays_dimension),dxone(arrays_dimension),color(arrays_dimension),dcolor(arrays_dimension))

allocate(threerdvar(arrays_dimension),dthreerdvar(arrays_dimension),tmax(arrays_dimension),dtmax(arrays_dimension))

allocate(cov_m_s(arrays_dimension),cov_m_c(arrays_dimension),cov_s_c(arrays_dimension),set(arrays_dimension),ra(arrays_dimension), &
logz(arrays_dimension),mb_02(arrays_dimension))

allocate(dec(arrays_dimension),biascor(arrays_dimension))
OPEN (10, file='jla_lcparams.txt',FORM='FORMATTED',STATUS='OLD',ACTION='READ')
read(10,*)
OPEN (7, file='Values_z', status='unknown', form='formatted')
OPEN (8, file='Values_mb', status='unknown', form='formatted')
OPEN (9, file='z_mb', status='unknown', form='formatted')
OPEN (11, file='z_mb_outliers', status='unknown', form='formatted')
OPEN (12, file='final', status='unknown', form='formatted')
Do p=1, arrays_dimension
    read(10,*) name, zcmb(p),zhel(p),dz(p),mb(p),&
    dmb(p),xone(p),dxone(p), &
    color(p),dcolor(p),threerdvar(p), &
    dthreerdvar(p),tmax(p),dtmax(p), &
    cov_m_s(p),cov_m_c(p),cov_s_c(p), &
    set(p),ra(p),dec(p),biascor(p)
    
    write(7,*) zcmb(p),dz(p)
    write(8,*) mb(p),dmb(p)
    write(9,*) zcmb(p),mb(p)
End Do


Close(7)
Close(8)
Close(9)
Close(10)

OPEN (9, file='z_mb', status='unknown', form='formatted')
arrays_dimension_outliers=0
Do i=1, arrays_dimension   
    read(9,*) zcmb(i),mb(i)    
    If (zcmb(i) > z_min .and. zcmb(i) < z_max) then
        write(11,*) zcmb(i),mb(i)
         logz(i)=LOG10(299792458*zcmb(i)*(1+0.5*(1-q_0)*zcmb(i)-(1-q_0-3*(q_0**2)+j_0)*(zcmb(i))**2/6))
        
         mb_02(i)=0.2*mb(i)
         
          write(12,*) logz(i),mb_02(i)
         
        arrays_dimension_outliers=arrays_dimension_outliers+1
    End If
End Do

 write(*,*) 'El número de datos entre z_min y z_max es', arrays_dimension_outliers


end program param
