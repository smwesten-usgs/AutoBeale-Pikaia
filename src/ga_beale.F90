module beale_pikaia_func

use types
use genetic_algorithm
use beale
use beale_data

implicit none

contains

! This function is called by PIKAIA.  PIKAIA provides
! a vector of real numbers [0,1] that are converted in this function
! to integer-valued stratum boundaries.
!
! This function forms the strata, then calculates the total RMSE
! over all strata and reports this back to PIKAIA.

function ga_beale(iNumBounds,x)  result(rValue)

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
  call reset_bealestats(pBealeStats)

  iBeginDate = MINVAL(pFlow%iJulianDay)
  iDeltaDate = MAXVAL(pFlow%iJulianDay) - MINVAL(pFlow%iJulianDay)

  pConfig%iFuncCallNum = pConfig%iFuncCallNum + 1

!  print *, "BEGIN:",MINVAL(pFlow%iJulianDay)
!  print *, "END:",MAXVAL(pFlow%iJulianDay)

!  print *, x(1:n)

  call ssort(x,iNumBounds)

!  print *, x(1:n)

  ! assign strata bounds
  do i=1,iNumBounds
    pConfig%iStrataBound(i) = iBeginDate + iDeltaDate * x(i)
    pConfig%rPikaiaXValues(i) = x(i)
  end do

  pConfig%rCombinedLoad = rZERO
  pConfig%rCombinedMSE = rZERO
  pConfig%rCombinedRMSE = rZERO

  ! loop over all strata members
  do i=1,iNumBounds+1
    pB=>pBealeStats(i)
    call assemble_strata(pConfig,pFlow,pConc,pB,i,lValid)

    call gregorian_date(pB%iStartDate, iB_Year, iB_Month, iB_Day)
    call gregorian_date(pB%iEndDate, iE_Year, iE_Month, iE_Day)

    write(sBuf,FMT="(i2.2,'/',i2.2,'/',i4.4)") &
      iB_Month,iB_Day,iB_Year
    pB%sStartDate = trim(sBuf)
    write(sBuf,FMT="(i2.2,'/',i2.2,'/',i4.4)") &
      iE_Month,iE_Day,iE_Year
    pB%sEndDate = trim(sBuf)

    if(lValid) then

      ! date values are legal and at least 2 samples exist for the
      ! current stratum...  O.K. to call Beale_Estimator

      call Beale_Estimator(pFlow,pConc,pB, pConfig)

      pConfig%rCombinedLoad = pConfig%rCombinedLoad + &
          pBealeStats(i)%rStratumCorrectedLoad

      ! Equation M, Baum (1982)
      ! MSE = MSE_d * N^2 = sum(N_h^2 * MSE_hd)
      pConfig%rCombinedMSE = pConfig%rCombinedMSE + &
          pBealeStats(i)%rStratumMeanSquareError

    else

      ! date value is illegal (end date comes before start date)
      ! or less than 2 samples fall within the current date range for the
      ! stratum...

      ! need to provide Pikaia with disincentive (i.e. rValue)
      ! to settle on a stratum scheme that includes
      ! illegal start and end dates

      pB%rDailyMeanSquareError = 0.
      pB%rDailyCorrectedLoadEstimate = -99999.
      pB%rStratumCorrectedLoad = -99999.
      pB%rStratumMeanSquareError = 0.
      pB%rDailyBiasedLoadEstimate = -99999.
      pB%rDailyLoadBiasCorrection = -99999.
      pB%rS_qq = rZERO
      pB%rS_lq = rZERO
      pB%rS_ll = rZERO
      pB%rS_q2l = rZERO
      pB%rS_ql2 = rZERO
      pB%rS_q3 = rZERO


!      pB%iNumDays = 0
!      pB%iNumSamples = 0
!      pB%rMeanFlow = 0.
!      pB%rMeanSampleFlow = 0.
!      pB%rMeanSampleConc = 0.
!      pB%rMeanSampleLoad = 0.

      iNumInvalidStrata = iNumInvalidStrata + 1

    end if

  end do    ! loop over all strata members

  ! now that we have considered all the strata, are there any invalid
  ! strata members?  If so, return negative value; else return value
  ! based on

  if(iNumInvalidStrata > 0) then

    pConfig%rCombinedLoad = HUGE(rValue)
    pConfig%rCombinedMSE = HUGE(rValue)
    rValue = -99999.
    pConfig%rCombinedRMSE = HUGE(rValue)
    pConfig%rCombinedLoadCI = HUGE(rValue)
    pConfig%rCombinedLoadAnnualized = HUGE(rValue)
    pConfig%rCombinedLoadAnnualizedCI = HUGE(rValue)

  else

    pConfig%rCombinedRMSE = sqrt(pConfig%rCombinedMSE)
    r_edf = rf_effective_degrees_freedom(pConfig,pBealeStats)

    pConfig%rCombinedLoadCI = rf_compute_CI(r_edf, pConfig%rCombinedMSE)

    pConfig%rCombinedLoadAnnualized = pConfig%rCombinedLoad * &
      365. / REAL(pConfig%iTotNumDays,kind=T_REAL)

    pConfig%rCombinedLoadAnnualizedCI = rf_compute_CI(r_edf, &
       pConfig%rCombinedMSE * 365.25**2 / REAL(pConfig%iTotNumDays**2, &
         kind=T_REAL))

!    rValue = 1./(SUM(pBealeStats(1:iNumBounds+1)%rStratumMeanSquareError))*1.e+6

     rValue = 1. / pConfig%rCombinedRMSE * 1.e+6

!    rValue = 1. / pConfig%rCombinedLoadCI * 1.e+6

  end if

!  if(COUNT(pBealeStats(1:n+1)%iNumSamples<2)/=0) then
!  if(lFALSE) then

!if(iNumInvalidStrata>0) then
!
!    write(LU_ECHO_OUT,FMT="('Load',',',500(f16.4,','))") &
!      (pBealeStats(i)%rDailyCorrectedLoadEstimate,i=1,iNumBounds+1)
!    write(LU_ECHO_OUT,FMT="('NumSamples',',',500(i10,','))") &
!      (pBealeStats(i)%iNumSamples,i=1,iNumBounds+1)
!    write(LU_ECHO_OUT,FMT="('RVal',','500(f24.4,','))") &
!      (pBealeStats(i)%rStratumMeanSquareError,i=1,iNumBounds+1)
!    write(LU_ECHO_OUT,FMT="('Dates',','500(a10,','))") &
!      (pBealeStats(i)%sStartDate,pBealeStats(i)%sEndDate,i=1,iNumBounds+1)
!    write(LU_ECHO_OUT,FMT="('Fitness:',',',f18.3)") rValue
!    write(LU_ECHO_OUT,FMT=*) ' '
!
!    flush(LU_ECHO_OUT)
!
!end if

  return

end function ga_beale

end module beale_pikaia_func
