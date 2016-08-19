module pikaia_function

use types
use pikaia_module
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

function evaluate_fitness_function( iNumBounds, x )  result(rValue)

  implicit none

  integer (kind=T_INT), intent(in) :: iNumBounds
  real (kind=T_REAL), dimension(:), intent(in) :: x
  real (kind=T_REAL) :: rValue

  ! [ LOCALS ]
  integer (kind=T_INT) :: i,j
  real (kind=T_REAL)   :: genome_values( iNumBounds )

  integer (kind=T_INT) :: iBeginDate, iDeltaDate, iStatus

  integer (kind=T_INT) :: iB_Year, iB_Month, iB_Day
  integer (kind=T_INT) :: iE_Year, iE_Month, iE_Day
  integer (kind=T_INT) :: iNumInvalidStrata
  integer (kind=T_INT) :: iStrataBoundary
  integer (kind=T_INT) :: iCurrentNumberOfBounds
  integer (kind=T_INT) :: iCurrentNumberOfStrata

  character (len=256) :: sBuf, sStartDate, sEndDate
  character (len=1) :: sTab = CHAR(9)

  real (kind=T_REAL) :: r_edf

  logical (kind=T_LOGICAL) :: lValid

  iNumInvalidStrata = 0

  ! keep track of how many times this routine has been called
  pConfig%iFuncCallNum = pConfig%iFuncCallNum + 1

!  print *, "BEGIN:",MINVAL(pFlow%iJulianDay)
!  print *, "END:",MAXVAL(pFlow%iJulianDay)

!  print *, x(1:n)

  call ssort(x, iNumBounds)

  genome_values = x(1:iNumBounds)

  call create_new_strata_from_genome( pConfig, genome_values, pStrata )

  call calculate_and_combine_stratum_loads( pConfig, pStrata, pStats, pFlow, pConc )

  ! Pikaia *MAXIMIZES* the fitness function (objective function); need to return a values
  ! that gets LARGER as the MSE decreases.

  if ( pConfig%iMinimizationStatistic == MINIMIZE_MEAN_SQUARED_ERROR ) then

    if ( pStats%rCombinedMSE >= 0. ) then
      rValue = 1. / pStats%rCombinedMSE * 1.e+6
    else
      rValue = -99999.
    end if

  elseif ( pConfig%iMinimizationStatistic == MINIMIZE_CONFIDENCE_INTERVAL ) then

    if ( pStats%rCombinedLoadCI >= 0. ) then
      rValue = 1. / pStats%rCombinedLoadCI * 1.e+8
    else
      rValue = -99999.
    end if

  endif

end function evaluate_fitness_function

end module pikaia_function
