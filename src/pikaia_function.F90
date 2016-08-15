module pikaia_function

use types
use pikaia
use beale
use beale_data
use strata
use output

implicit none

contains

! This function is called by PIKAIA.  PIKAIA provides
! a vector of real numbers [0,1] that are converted in this function
! to integer-valued stratum boundaries.
!
! This function forms the strata, calls the Beale calculation function,
! then calculates the total RMSE over all strata and reports this back to PIKAIA.

function evaluate_fitness_function(iNumBounds,x)  result(rValue)

  implicit none

  integer (kind=T_INT), intent(in) :: iNumBounds
  real (kind=T_REAL), dimension(:), intent(in) :: x
  real (kind=T_REAL) :: rValue

  integer (kind=T_INT) :: i,j

  integer (kind=T_INT) :: iBeginDate, iDeltaDate, iStatus

  integer (kind=T_INT) :: iB_Year, iB_Month, iB_Day
  integer (kind=T_INT) :: iE_Year, iE_Month, iE_Day
  integer (kind=T_INT) :: iNumInvalidStrata

  character (len=256) :: sBuf, sStartDate, sEndDate
  character (len=1) :: sTab = CHAR(9)

  real (kind=T_REAL) :: r_edf

  logical (kind=T_LOGICAL) :: lValid

  iNumInvalidStrata = 0

  ! make sure all accumulators are zeroed out prior to calculations
  call reset_strata_stats(pStrata)

  iBeginDate = MINVAL(pFlow%iJulianDay)
  iDeltaDate = MAXVAL(pFlow%iJulianDay) - MINVAL(pFlow%iJulianDay)

  ! keep track of how many times this routine has been called
  pConfig%iFuncCallNum = pConfig%iFuncCallNum + 1

!  print *, "BEGIN:",MINVAL(pFlow%iJulianDay)
!  print *, "END:",MAXVAL(pFlow%iJulianDay)

!  print *, x(1:n)

  call ssort(x,iNumBounds)

!  print *, x(1:n)

  pStrata(1)%iStartDate = pConfig%iStartDate
  pStrata( iNumBounds )%iEndDate = pConfig%iEndDate

  ! assign strata bounds
  do i=1,iNumBounds
    pConfig%iStrataBound(i) = iBeginDate + iDeltaDate * x(i)
    ! assign strata boundaries 
    if ( i < iNumBounds ) then
      pStrata(i)%iEndDate = pConfig%iStrataBound(i)
      pStrata(i+1)%iStartDate = pConfig%iStrataBound(i) + 1
    endif
    pConfig%rPikaiaXValues(i) = x(i)
  end do

  call calculate_and_combine_stratum_loads( pConfig, pStrata, pFlow, pConc, iNumBounds + 1 )

  ! Pikaia *MAXIMIZES* the fitness function (objective function); need to return a values
  ! that gets LARGER as the MSE decreases.
  if ( pConfig%rCombinedMSE >= 0. ) then
    rValue = 1. / pConfig%rCombinedMSE * 1.e+6
  else
    rValue = -99999.
  end if

end function evaluate_fitness_function

end module pikaia_function
