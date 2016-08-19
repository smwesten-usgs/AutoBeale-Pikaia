!> @file
!! Contains a single module, @ref beale, which contains functions and
!! subroutines needed to apply the Beale Ratio Estimator to a given stratum
!! (subset) of the complete dataset.

module beale

  use types
  use units
  implicit none

  contains

!> Calculate the Beale Ratio Estimator for a specific stratum or subset of the data.
!!
!!

subroutine calculate_Beale_load(pFlow, pConc, pStratum, pConfig)

  type (FLOW_T), dimension(:), pointer :: pFlow
  type (CONC_T), dimension(:), pointer :: pConc
  type (STRATUM_STATS_T), pointer      :: pStratum
  type (CONFIG_T), pointer             :: pConfig

  real (kind=T_REAL) :: rSum = rZERO
  real (kind=T_REAL) :: rQ_bar = rZERO
  real (kind=T_REAL) :: rQ_bar_sq = rZERO
  real (kind=T_REAL) :: rL_bar = rZERO
  real (kind=T_REAL) :: rL_bar_sq = rZERO
  real (kind=T_REAL) :: rMSE_1 = rZERO
  real (kind=T_REAL) :: rMSE_2 = rZERO
  real (kind=T_REAL) :: rMSE_3 = rZERO
  real (kind=T_REAL) :: rTheta = rZERO
  real (kind=T_REAL) :: rBiasCorr_numerator, rBiasCorr_denominator

! Data Types in type pStratum
!integer (kind=T_INT) :: nDays
!integer (kind=T_INT) :: nNumSamples
!real (kind=T_REAL) :: rMeanFlow

!real (kind=T_REAL) :: rMeanSampleFlow
!real (kind=T_REAL) :: rMeanSampleConc

!real (kind=T_REAL) :: rDailyLoadBiasCorrection
!real (kind=T_REAL) :: rDailyCorrectedLoadEstimate

  ! count of SAMPLES taken within current date range
  pStratum%iNumSamples = COUNT(pConc%iJulianDay>=pStratum%iStartDate &
                        .and. pConc%iJulianDay <=pStratum%iEndDate &
                        .and. pConc%lInclude)

  ! count of ALL flow values within current date range
  pStratum%iNumDays = COUNT(pFlow%iJulianDay>= pStratum%iStartDate .and. &
                            pFlow%iJulianDay <= pStratum%iEndDate)

  ! "theta" is defined in Tin (1965) as (1/n - 1/N)
!  rTheta = (1_T_REAL/REAL(pStratum%iNumSamples,kind=T_REAL)) &
!               - (1_T_REAL/REAL(pStratum%iNumDays, kind=T_REAL))

  ! theta is defined in Baun (1982) as the following:
  rTheta = (1_T_REAL/REAL(pStratum%iNumSamples,kind=T_REAL))

  ! sum ALL flows within current date range (i.e. within current STRATUM)
  rSum = SUM(pFlow%rFlow,(pFlow%iJulianDay>= pStratum%iStartDate .and. &
                            pFlow%iJulianDay <= pStratum%iEndDate))

  ! calculate mean STRATUM flow
  if(pStratum%iNumDays>0) then
    pStratum%rMeanFlow = rSum / pStratum%iNumDays
  else
    pStratum%rMeanFlow = -9999.0
  end if

  ! sum SAMPLE flows within current date range (i.e. within current STRATUM)
  rSum = SUM(pConc%rFlow,(pConc%iJulianDay>= pStratum%iStartDate &
                      .and. pConc%iJulianDay <= pStratum%iEndDate &
                      .and. pConc%lInclude))

  ! calculate mean SAMPLE flow
  if(pStratum%iNumSamples>0) then
    pStratum%rMeanSampleFlow = rSum / pStratum%iNumSamples
    rQ_bar = pStratum%rMeanSampleFlow
    rQ_bar_sq = rQ_bar**2.
  else
    pStratum%rMeanSampleFlow = -9999.0
    rQ_bar = 0.
    rQ_bar_sq = 0.
  end if


  ! sum SAMPLE *CONC* within current date range (i.e. within current STRATUM)
  rSum = SUM(pConc%rConc,(pConc%iJulianDay>= pStratum%iStartDate &
                       .and. pConc%iJulianDay <= pStratum%iEndDate &
                       .and. pConc%lInclude))

  ! calculate mean STRATUM *CONC*
  if(pStratum%iNumSamples>0) then
    pStratum%rMeanSampleConc = rSum / pStratum%iNumSamples
  else
    pStratum%rMeanSampleConc = -9999.0
  end if

  ! sum SAMPLE *LOAD* within current date range (i.e. within current STRATUM)
  rSum = SUM(pConc%rDailyLoad,(pConc%iJulianDay>= pStratum%iStartDate &
                       .and. pConc%iJulianDay <= pStratum%iEndDate &
                       .and. pConc%lInclude))

  ! calculate mean STRATUM *LOAD*
  if(pStratum%iNumSamples>0) then
    pStratum%rMeanSampleLoad = rSum / pStratum%iNumSamples
    rL_bar = pStratum%rMeanSampleLoad
    rL_bar_sq = rL_bar**2.
  else
    pStratum%rMeanSampleLoad = -9999.0
    rL_bar = 0.
    rL_bar_sq = 0.
  end if

  ! after call to calculate_variance, we will have our std deviation
  ! terms populated within pStratum
  call calculate_variance(pConc, pStratum)

  rBiasCorr_numerator = 1_T_REAL + (1_T_REAL / REAL(pStratum%iNumSamples,kind=T_REAL) &
    * pStratum%rS_lq / rL_bar / rQ_bar)

  rBiasCorr_denominator = 1_T_REAL + (1_T_REAL / REAL(pStratum%iNumSamples,kind=T_REAL) &
    * pStratum%rS_qq / (rQ_bar**2))

  pStratum%rDailyLoadBiasCorrection = rBiasCorr_numerator &
                   / rBiasCorr_denominator

  pStratum%rDailyBiasedLoadEstimate = pStratum%rMeanFlow * &
    pStratum%rMeanSampleLoad / pStratum%rMeanSampleFlow

  pStratum%rDailyCorrectedLoadEstimate = pStratum%rDailyBiasedLoadEstimate * &
     pStratum%rDailyLoadBiasCorrection

  rMSE_1 = rTheta * ( (pStratum%rS_qq / rQ_bar_sq) + &
          (pStratum%rS_ll / rL_bar_sq) - &
          (2_T_REAL * pStratum%rS_lq / (rL_bar * rQ_bar)) )

  rMSE_2 = rTheta**2 *(  2_T_REAL*(pStratum%rS_qq**2/rQ_bar_sq**2) - &
            (4_T_REAL*pStratum%rS_qq*pStratum%rS_lq/(rQ_bar_sq * rL_bar * rQ_bar)) + &
            (pStratum%rS_lq**2/((rL_bar*rQ_bar)**2)) + &
            ((pStratum%rS_qq * pStratum%rS_ll)/(rQ_bar_sq * rL_bar_sq))  )


  ! this term is from Tin (1965), equation V(t2), top of pp 299.
  rMSE_3 = ( 2_T_REAL * rTheta /   REAL(pStratum%iNumDays,kind=T_REAL) ) * &
    ( (pStratum%rS_q3 / rQ_bar**3) - &
    (2_T_REAL*pStratum%rS_q2l / (rL_bar*rQ_bar**2)) + &
    (pStratum%rS_ql2 / (rL_bar**2 *rQ_bar)) )

!    write(*,FMT="(3(GS14.6,1x))") rMSE_1,rMSE_2,rMSE_3

  ! Equation C, Baum (1982), with third term from Tin (1965),
  ! for V(t2), top of page 299
  pStratum%rDailyMeanSquareError = (rL_bar * pStratum%rMeanFlow / pStratum%rMeanSampleFlow)**2 &
      * ( rMSE_1 + rMSE_2 + rMSE_3) !* REAL(pStratum%iNumDays,kind=T_REAL)**2

  ! Equation E, Baum (1982)
  pStratum%rStratumCorrectedLoad = pStratum%rDailyCorrectedLoadEstimate * &
      REAL(pStratum%iNumDays, kind=T_REAL)

  ! Equation F, Baum (1982)
  pStratum%rStratumMeanSquareError = pStratum%rDailyMeanSquareError * &
    REAL(pStratum%iNumDays**2,kind=T_REAL)

  pStratum%rDailyLoadCI = calculate_confidence_interval(REAL(pStratum%iNumSamples - 1, kind=T_REAL), &
      pStratum%rDailyMeanSquareError)


  pStratum%rStratumLoadCI = calculate_confidence_interval(REAL(pStratum%iNumSamples - 1, kind=T_REAL), &
      pStratum%rStratumMeanSquareError)

end subroutine calculate_Beale_load

!--------------------------------------------------------------------------------------------------

!> Calculate the various variance terms needed for calculating the Beale Ratio Estimator.
!!
!! @param pConc pointer to array of CONC_T type containing entire concentration dataset.
!! @param pStratum pointer of STRATUM_STATS_T type holding stratum boundaries and statistics.

subroutine calculate_variance(pConc,pStratum)

  type (CONC_T), dimension(:), pointer :: pConc
  type (STRATUM_STATS_T), pointer :: pStratum

  integer (kind=T_INT) :: n, i
  real (kind=T_REAL) :: rSum
  real (kind=T_REAL) :: rSum_lq
  real (kind=T_REAL) :: rSum_qq
  real (kind=T_REAL) :: rSum_ll
  real (kind=T_REAL) :: rSum_q2l
  real (kind=T_REAL) :: rSum_q3
  real (kind=T_REAL) :: rSum_ql2
  real (kind=T_REAL) :: rTheta

  rSum = rZERO
  rSum_lq = rZERO
  rSum_qq = rZERO
  rSum_ll = rZERO
  rSum_q2l = rZERO
  rSum_q3 = rZERO
  rSum_ql2 = rZERO
  rTheta = rZERO

  ! the value in pStratum must be current or we get garbage!
  n = pStratum%iNumSamples

  call Assert(LOGICAL(n==COUNT(pConc%iJulianDay >= pStratum%iStartDate &
     .and. pConc%iJulianDay <= pStratum%iEndDate &
     .and. pConc%lInclude),kind=T_LOGICAL), &
     "Mismatch between sample number calculation methods; routine calculate_variance")

  if(n>1) then

    rTheta = 1_T_REAL / (REAL(n,kind=T_REAL)-1_T_REAL)

    do i=1,size(pConc%rConc)

      if(pConc(i)%iJulianDay >= pStratum%iStartDate &
         .and. pConc(i)%iJulianDay <= pStratum%iEndDate &
         .and. pConc(i)%lInclude) then

        ! update accumulators

        rSum_lq = rSum_lq + pConc(i)%rLoadTimesFlow &
                   - (pStratum%rMeanSampleLoad * pStratum%rMeanSampleFlow)

        rSum_qq = rSum_qq + (pConc(i)%rFlow - pStratum%rMeanSampleFlow)**2

        rSum_ll = rSum_ll + (pConc(i)%rDailyLoad - pStratum%rMeanSampleLoad)**2

        rSum_q2l = rSum_q2l + (pConc(i)%rFlow - pStratum%rMeanSampleFlow)**2 &
                    * (pConc(i)%rDailyLoad - pStratum%rMeanSampleLoad)

        rSum_q3 = rSum_q3 + (pConc(i)%rFlow - pStratum%rMeanSampleFlow)**3

        rSum_ql2 = rSum_ql2 + (pConc(i)%rFlow - pStratum%rMeanSampleFlow) &
                    * (pConc(i)%rDailyLoad-pStratum%rMeanSampleLoad)**2

      end if

    end do

    pStratum%rS_lq = rTheta * rSum_lq     ! this is S_xy in Baun's paper, eqn B
    pStratum%rS_qq = rTheta * rSum_qq     ! this is S_x**2 in Baun's paper, eqn B
    pStratum%rS_ll = rTheta * rSum_ll
    pStratum%rS_q2l = rTheta * rSum_q2l
    pStratum%rS_q3 = rTheta * rSum_q3
    pStratum%rS_ql2 = rTheta * rSum_ql2

!     sumx3=sumx3 + (pConc(i)%rFlow-avflow)**3
!   sumx2y=sumx2y + (pConc(i)%rFlow-avflow)**2 * (pConc(i)%rDailyLoad-avload)
!   sumxy2=sumxy2 + (pConc(i)%rFlow-avflow) * (pConc(i)%rDailyLoad-avload)**2

  else

    pStratum%rS_lq = rZERO
    pStratum%rS_qq = rZERO
    pStratum%rS_ll = rZERO
    pStratum%rS_q2l = rZERO
    pStratum%rS_q3 = rZERO
    pStratum%rS_ql2 = rZERO

  end if

  return

end subroutine calculate_variance

!----------------------------------------------------------------------

subroutine calculate_daily_loads(pFlow,pConc,pConfig, pStats)

  type (FLOW_T), dimension(:), pointer :: pFlow
  type (CONC_T), dimension(:), pointer :: pConc
  type (CONFIG_T), pointer             :: pConfig
  type (COMBINED_STATS_T), pointer     :: pStats

  integer (kind=T_INT) :: i,j
  integer (kind=T_INT) :: iDayOfYear,iNumDaysInYear
  real (kind=T_REAL) :: rFlow
  real (kind=T_REAL) :: rYearFrac
  logical (kind=T_LOGICAL) :: lSetSampleDayToTrue = lTRUE

  write(LU_STD_OUT,FMT="(t23,'Flow',t40,'Conc',t58,'Load')")
  write(LU_STD_OUT,FMT="(t23,'----',t40,'----',t58,'----')")

  pConfig%iCountUniqueSamples = 0

  ! calculate an "annualized" total flow
  pStats%rTotalFlow=SUM(pFlow%rFlow) * 86400_T_REAL    ! cubic meters of water
  pStats%rTotalFlowAnnualized =  pStats%rTotalFlow * 365_T_REAL &
      / REAL(pConfig%iTotNumDays,kind=T_REAL)

  do i=1,SIZE(pConc%rConc)

    ! when the get_flow routine finds a matching date it 1) returns
    ! the appropriate matching flow value, and 2) sets the lSampleDay
    ! flag to true to ease calculations later on

    rFlow = get_flow(pConc(i)%iJulianDay,pFlow)

    pConc(i)%rFlow = rFlow
    pConc(i)%rFlowSquared = rFlow**2

    ! calculate daily load
    pConc(i)%rDailyLoad = &
       pConc(i)%rConc * rFlow * rBASE_CONVERSION_FACTOR

    pConc(i)%rLoadTimesFlow = pConc(i)%rDailyLoad * pConc(i)%rFlow

    if(COUNT(pConc%iJulianDay==pConc(i)%iJulianDay)==1) then
      pConfig%iCountUniqueSamples = pConfig%iCountUniqueSamples + 1
    end if

    iDayOfYear = day_of_year(pConc(i)%iMonth,pConc(i)%iDay,pConc(i)%iYear)
    iNumDaysInYear = num_days_in_year(pConc(i)%iYear)

    rYearFrac = REAL(iDayOfYear,kind=T_SGL) / &
        REAL(iNumDaysInYear,kind=T_SGL)

    write(LU_STD_OUT,FMT="(i2.2,'/',i2.2,'/',i4,': ',3(a,1x))") &
      pConc(i)%iMonth,pConc(i)%iDay,pConc(i)%iYear, &
      trim(sf_Q_units(pConfig,rFlow)), &
      trim(sf_C_units(pConfig,pConc(i)%rConc)), &
      trim(sf_L_units(pConfig,pConc(i)%rDailyLoad))

    write(LU_LOADS_OUT,FMT="(a,',',i2.2,'/',i2.2,'/',i4,',',F12.4,','," &
         //"2(G14.4,','),G14.4)") &
      trim(pConc(i)%sTribName), &
      pConc(i)%iMonth,pConc(i)%iDay,pConc(i)%iYear, &
      rYearFrac, &
      rFlow, &
      pConc(i)%rConc, &
      pConc(i)%rDailyLoad

  end do


end subroutine calculate_daily_loads

!-----------------------------------------------------------------------


!-----------------------------------------------------------------------

subroutine calculate_and_report_monthly_stats(iLU,pFlow,pConc,pConfig)

  integer (kind=T_INT), intent(in) :: iLU         ! logical unit for output
  type (FLOW_T), dimension(:), pointer :: pFlow
  type (CONC_T), dimension(:), pointer :: pConc
  type (CONFIG_T), pointer :: pConfig ! pointer to data structure that contains
                                        ! program options, flags, and other settings

  integer (kind=T_INT) :: iBeginJulDay
  integer (kind=T_INT) :: iEndJulDay
  integer (kind=T_INT) :: iMonth

  integer (kind=T_INT) :: iDayOfYear
  character (len=3) :: sMonthName
  logical (kind=T_LOGICAL) :: lMonthEnd

  real (kind=T_REAL) :: rFlowSum,rFlowMean,rFlowMin,rFlowMax
  real (kind=T_REAL) :: rConcSum,rConcMean,rConcMin,rConcMax
  real (kind=T_REAL) :: rLoadSum,rLoadMean,rLoadMin,rLoadMax

  integer (kind=T_INT) :: iFlowCount, iConcCount

  do iMonth = 1,12

    call LookupMonth(iMonth, 1_T_INT, pFlow(1)%iYear,iDayOfYear, &
              sMonthName,lMonthEnd)

    iFlowCount = COUNT(pFlow%iMonth==iMonth)
    iConcCount = COUNT(pConc%iMonth==iMonth)

    rFlowSum = SUM(pFlow%rFlow,pFlow%iMonth==iMonth)
    rConcSum = SUM(pConc%rConc,pConc%iMonth==iMonth)
    rLoadSum = SUM(pConc%rDailyLoad,pConc%iMonth==iMonth)

    rFlowMin = MINVAL(pFlow%rFlow,pFlow%iMonth==iMonth)
    rConcMin = MINVAL(pConc%rConc,pConc%iMonth==iMonth)
    rLoadMin = MINVAL(pConc%rDailyLoad,pConc%iMonth==iMonth)

    rFlowMax = MAXVAL(pFlow%rFlow,pFlow%iMonth==iMonth)
    rConcMax = MAXVAL(pConc%rConc,pConc%iMonth==iMonth)
    rLoadMax = MAXVAL(pConc%rDailyLoad,pConc%iMonth==iMonth)

    if(iFlowCount > 0) then
      rFlowMean = rFlowSum / iFlowCount
    else
      rFlowMean = -9999.0
    endif

    if(iConcCount > 0) then
      rConcMean = rConcSum / iConcCount
      rLoadMean = rLoadSum / iConcCount
    else
      rConcMean = -9999.0
      rLoadMean = -9999.0
    endif

    write(iLU,FMT="(a)") repeat("-",72)
    write(iLU,FMT="(a,a)") "Statistics for ",sMonthName
    write(iLU,FMT="('Statistic',t10,3(a20,1x))") 'Flow','Conc','Load'
    write(iLU,FMT="(a)") ""
    write(iLU,FMT="('count:',t10,3(i20,1x))") iFlowCount,&
      iConcCount, iConcCount
    write(iLU,FMT="(a)") ""

    if(rConcMean>0) then
      write(iLU,FMT="('mean:',t10,3(a20,1x))") &
         trim(sf_Q_units(pConfig,rFlowMean)),&
         trim(sf_C_units(pConfig,rConcMean)), &
         trim(sf_L_units(pConfig,rLoadMean))
      write(iLU,FMT="('minimum:',t10,3(a20,1x))") &
         trim(sf_Q_units(pConfig,rFlowMin)),&
         trim(sf_C_units(pConfig,rConcMin)), &
         trim(sf_L_units(pConfig,rLoadMin))
      write(iLU,FMT="('maximum:',t10,3(a20,1x))") &
         trim(sf_Q_units(pConfig,rFlowMax)),&
         trim(sf_C_units(pConfig,rConcMax)), &
         trim(sf_L_units(pConfig,rLoadMax))
    else
      write(iLU,FMT="('mean:',t10,a20)") &
        trim(sf_Q_units(pConfig,rFlowMean))
      write(iLU,FMT="('minimum:',t10,a20)") &
        trim(sf_Q_units(pConfig,rFlowMin))
      write(iLU,FMT="('maximum:',t10,a20)") &
        trim(sf_Q_units(pConfig,rFlowMax))
    end if
    write(iLU,FMT="(a)") ""

  end do

  write(iLU,FMT="(a)") repeat("-",72)

end subroutine calculate_and_report_monthly_stats

!-----------------------------------------------------------------------

function get_flow(iJulianDay,pFlow)  result(rFlow)

  ! [ ARGUMENTS ]
  integer (kind=T_INT),intent(in) :: iJulianDay
  type (FLOW_T), dimension(:), pointer :: pFlow
  real (kind=T_REAL) :: rFlow

  ! [ LOCALS ]
  integer (kind=T_INT) :: i,iCount
  integer (kind=T_INT) :: iDay,iMonth,iYear
  character(len=256) :: sBuf

  call Assert(LOGICAL(iJulianDay>0,kind=T_LOGICAL), &
    "function get_flow called with an illegal Julian Day value")

  iCount = 0

  do i=1,SIZE(pFlow%rFlow)

    if(pFlow(i)%iJulianDay==iJulianDay) then
      rFlow = pFlow(i)%rFlow
      iCount=iCount+1
    end if

  end do

  if(iCount==0) then
    call gregorian_date(iJulianDay, iYear, iMonth, iDay)
    write(LU_STD_OUT, &
      FMT="('Could not find matching flow for ',i2.2,'/',i2.2,'/',i4)") &
      iMonth,iDay,iYear
    write(sBuf,FMT="('Julian Day: ',i10)") iJulianDay
    call Assert(LOGICAL(lFALSE,kind=T_LOGICAL),sBuf)
  elseif(iCount>1) then
    call gregorian_date(iJulianDay, iYear, iMonth, iDay)
    write(LU_STD_OUT, &
      FMT="('More than one flow for ',i2.2,'/',i2.2,'/',i4)") &
      iMonth,iDay,iYear
    write(sBuf,FMT="('Julian Day: ',i10)") iJulianDay
    call Assert(LOGICAL(lFALSE,kind=T_LOGICAL),sBuf)
  end if

  return

end function get_flow

!-----------------------------------------------------------------------

subroutine bealecalc_orig(pConfig, pFlow, pConc, pStrata)

      type (CONFIG_T), pointer :: pConfig ! pointer to data structure that contains
                                          ! program options, flags, and other settings

      type (FLOW_T), dimension(:), pointer :: pFlow
      type (CONC_T), dimension(:), pointer :: pConc
      type (STRATA_T), pointer             :: pStrata

      ! this routine lifted almost verbatim from Dr. Peter Richard's
      ! AutoBeale code

      ! *** ATTENTION!! The original code expected all concentration and
      ! load data to be in mg/L and grams, respectively.  This implementation
      ! takes whatever conversions have already been performed on the data
      ! AND PASSES THEM to the routine....(i.e. mg/L and Kg)

      real(kind=T_REAL), dimension(30) ::  t05 = (/ &
          12.71,4.3,3.18,2.78,2.57,  2.45,2.37,2.31,2.26,2.23, &
          2.2,2.18,2.16,2.14,2.13,   2.12,2.11,2.10,2.09,2.09, &
          2.08,2.07,2.07,2.06,2.06,  2.06,2.05,2.05,2.05,2.04/)

      real(kind=T_REAL) :: cumfl, cumse, df, suml, suml2, sumf, sumf2, sumfl, &
        sumx3, sumx2y, sumxy2, flowmu, fpc, avload, avflow, templ, &
        sxy, sx2, sxy2, sx3, f1, f2, f3, fl1, fl2, tmp1, tmp2, tmp3, tmp4, &
        sy, sy2, sx2y, s1, s2, s3, tval1, tval2, frac, tval, ci, rf, rn, &
        mse1, mse2, ce1, ce2

      real(kind=T_REAL),dimension(pStrata%iCurrentNumberOfStrata) :: &
         flowes, rmse, r, nf

      integer (kind=T_INT) :: l,i, j, ndays

      type (STRATUM_STATS_T), pointer  :: pStratum

      ! define ndays = TOTAL number of flow records
      ndays = SIZE(pFlow%rFlow)

      do j=1, pStrata%iCurrentNumberOfStrata

        pStratum => pStrata%pStratum(j)

        cumfl=0.0d0
        cumse=0.0d0
        df=0.0d0

        r(j)=0.0d0
        suml=0.0d0
        suml2=0.0d0
        sumf=0.0d0
        sumf2=0.0d0
        sumfl=0.0d0
        sumx3=0.0d0
        sumx2y=0.0d0
        sumxy2=0.0d0
        nf(j)=0
        flowmu=0.0d0

        do l=1,ndays
          if (pFlow(l)%iJulianDay <= pStratum%iEndDate .and. &
              pFlow(l)%iJulianDay >= pStratum%iStartDate) then
            nf(j)=nf(j)+1 ! # daily flows in strata
            flowmu=flowmu+pFlow(l)%rFlow
            ! search for matching sample record
            do i=1,SIZE(pConc%rFlow)
              if(pFlow(l)%iJulianDay == pConc(i)%iJulianDay) then
                templ=pConc(i)%rDailyLoad
                r(j)=r(j)+1.0d0
                suml=suml+templ
                suml2=suml2+templ*templ
                sumf=sumf+pConc(i)%rFlow
                sumf2=sumf2+pConc(i)%rFlow*pConc(i)%rFlow
                sumfl=sumfl+pConc(i)%rFlow*templ
              end if
            end do
          else
            cycle
          end if
        end do

        fpc=1.0d0
        flowmu=flowmu/REAL(nf(j),kind=T_REAL)  ! average flow over the entire STRATUM
        avflow=sumf/r(j)                       ! avg flow for SAMPLED DAYS
        avload=suml/r(j)                       ! AVG LOAD on SAMPLED DAYS

!      print *, "ndays: ",ndays,"  nf(j): ",nf(j),"  r: ",r
!      print *, "Flowmu: ",flowmu,"  avflow: ",avflow,"  avload: ",avload

        write(LU_STD_OUT,FMT=*) ' ===> OLD AUTOBEALE OUTPUT <==='

        write(LU_STD_OUT,FMT="(t2,'strata:',i3,2x,'ndays:',i4,2x,'nf(j):',f6.0,2x,'r:',f6.0)") &
          j, ndays, nf(j), r(j)
        write(LU_STD_OUT,FMT="(t2,'flowmu:',f12.3,2x,'avflow:',f12.3,2x,'avload:',f12.3)") &
          flowmu, avflow, avload

        do l=1,ndays        !now calculate the third order terms
          if (pFlow(l)%iJulianDay<=pStratum%iEndDate .and. &
              pFlow(l)%iJulianDay>=pStratum%iStartDate)then
            do i=1,SIZE(pConc%rFlow)
              if(pFlow(l)%iJulianDay == pConc(i)%iJulianDay) then
                sumx3=sumx3 + (pConc(i)%rFlow-avflow)**3
                sumx2y=sumx2y + (pConc(i)%rFlow-avflow)**2 * (pConc(i)%rDailyLoad-avload)
                sumxy2=sumxy2 + (pConc(i)%rFlow-avflow) * (pConc(i)%rDailyLoad-avload)**2
              end if
            end do
          else
            cycle
          end if
        end do

        if(r(j) .gt. 1.0d0) then
          sxy=(sumfl-(r(j)*avflow*avload))/(r(j)-1.0d0)
          sx2=(sumf2-(r(j)*avflow*avflow))/(r(j)-1.0d0)
          sy2=(suml2-(r(j)*avload*avload))/(r(j)-1.0d0)
          if (avload.gt.0.0d0) then
            sx2y=(sumx2y/(r(j)-1.0d0))
            sxy2=(sumxy2/(r(j)-1.0d0))
            sx3= (sumx3/(r(j)-1.0d0))
          else
            sx3=0.0d0
            sx2y=0.0d0
            sxy2=0.0d0
          end if
        else
          sxy=0.0d0
          sx2=0.0d0
          sy2=0.0d0
          sx2y=0.0d0
          sxy2=0.0d0
          sx3=0.0d0
        end if

        if (avload.gt.0.0d0) then
          sx2y=sx2y/(avflow**2*avload)
          sxy2=sxy2/(avflow*avload**2)
          sx3= sx3/(avflow**3)
        end if

        if (avload > rNEAR_ZERO) then            !whew! some conc data
          f1=1.0d0+((fpc/r(j))*(sxy/(avload*avflow)))
          f2=1.0d0+((fpc/r(j))*(sx2/(avflow*avflow)))
          flowes(j)=avload*(flowmu/avflow)*(f1/f2)
          s1=sx2/(avflow*avflow)
          s2=sy2/(avload*avload)
          s3=sxy/(avflow*avload)
          f1=(fpc/r(j))*(s1+s2-s3-s3)
          f2=(fpc**2/r(j)**2)*(2.0d0*s1*s1-4.0d0*s1*s3+s3*s3+s1*s2)
          f3=((2.0d0*fpc/r(j))/nf(j)) * (sx3-2.0d0*sx2y+sxy2)

          rmse(j)=(avload*flowmu/avflow)**2*(f1+f2+f3)

          write(*,FMT="('RMSE (orig):',f16.3)") rmse(j)
          print *, '   (this is the daily MSE for a stratum)'
          print *, ' '

          tmp1=flowmu/avflow
          tmp2=avload*tmp1
          tmp3=flowes(j)-tmp2
          tmp4=rmse(j)*r(j)
        else                        !gads! no conc data in this stratum
          flowes(j)=0.0d0
          rmse(j)=0.0d0
          tmp1=flowmu/avflow
          tmp2=0.0d0
          tmp3=0.0d0
          tmp4=0.0d0
        end if

      end do  ! end of loop over each strata member

      do i=1,pStrata%iCurrentNumberOfStrata

        pStratum => pStrata%pStratum(i)

        tmp1=r(i)-1.0_T_REAL

        rf = REAL(pStratum%iNumDays,kind=T_REAL)
        rn = REAL(pConfig%iTotNumDays,kind=T_REAL)

        cumfl=cumfl+flowes(i)*(rf/rn)        ! eqn. H in Baum (1982)
        cumse=cumse+(rmse(i)*rf*rf/(rn*rn))   ! eqn. I in Baum (1982)
        df=df+(rf**4*rmse(i)**2/tmp1)
      enddo

      if( pStrata%iCurrentNumberOfStrata .gt. 1) then
        if (df.eq.0) df=-1
         df=(cumse*rn*rn)*(cumse*rn*rn)/df
         ce1=cumfl*365.0_T_REAL
         ce2=cumfl*0.365_T_REAL
         mse1=cumse*133225.0_T_REAL  ! 133225 = 365**2
         mse2=cumse*0.133225_T_REAL  ! this is a rough translation of
      end if                         ! eqn. M in Baum (1982)

! calculate the 95% confidence interval half-width (i.e. the �number)
      if (df.ge.30) then
        tval=1.96_T_REAL+2.4_T_REAL/df
      else
        tval1=t05(INT(dmax1(1.0_T_REAL,df)))
        tval2=t05(INT(dmax1(1.0_T_REAL,df)+1))
        frac=df-INT(df)
        tval=tval1+frac*(tval2-tval1)
      end if
      if (pStrata%iCurrentNumberOfStrata.gt.1) then
        ci=tval*sqrt(mse2)
      else
        ci=tval*sqrt(rmse(1)*0.133225_T_REAL)
      end if

      write(LU_STD_OUT,fmt="(a)") 'OLD AUTOBEALE:     load         mse                df         ci'

      if (pStrata%iCurrentNumberOfStrata.gt.1) then
        write (LU_STD_OUT,11) 'OLD AUTOBEALE:',ce2*1000.,mse2*1.e+06, &
           df,ci*1000.
      else
        write (LU_STD_OUT,11) 'OLD AUTOBEALE:',flowes(1)*0.365_T_REAL*1000., &
           rmse(1)*0.133225_T_REAL*1.e+06,r(1)-1.,ci*1000.
      end if

11      format (a,2f16.2,f10.3,f16.2)


300   return

  end subroutine bealecalc_orig

!-----------------------------------------------------------------------

function calculate_effective_degrees_of_freedom(pConfig,pStrata, pStats)  result(r_edf)

  type (CONFIG_T), pointer :: pConfig ! pointer to data structure that contains
                                      ! program options, flags, and other settings

  type (STRATA_T), pointer                      :: pStrata
  type (COMBINED_STATS_T), pointer              :: pStats

  ! [ LOCALS ]
  integer (kind=T_INT)              :: indx
  type (STRATUM_STATS_T), pointer   :: pStratum

  real (kind=T_REAL) :: r_edf,rNsamp_minus_one,rN_h,rN,rL_d,rMSE_d

  r_edf = rZERO
  rN_h = rZERO
  rN = rZERO
  rL_d = rZERO
  rMSE_d = rZERO


  call Assert(LOGICAL( pStrata%iCurrentNumberOfStrata > 0,kind=T_LOGICAL), &
    'Function calculate_effective_degrees_of_freedom called with df < 1')

  if( pStrata%iCurrentNumberOfStrata == 1) then

    r_edf = REAL(pStrata%pStratum(1)%iNumSamples,kind=T_REAL)-1.0_T_REAL  !r
    rMSE_d = pStrata%pStratum(1)%rDailyMeanSquareError
    pStats%rCombinedDailyMSE = rMSE_d

  else

    do indx=1, pStrata%iCurrentNumberOfStrata

      pStratum => pStrata%pStratum(indx)

      rNsamp_minus_one = REAL(pStratum%iNumSamples, kind=T_REAL) - 1.0_T_REAL

      rN_h = REAL(pStratum%iNumDays,kind=T_REAL)    !rf
      rN = REAL(pConfig%iTotNumDays,kind=T_REAL)  !rn

!    cumfl=cumfl+flowes(i)*(rf/rn)        ! eqn. H in Baum (1982)

      ! eqn. H in Baum (1982)
      rL_d = rL_d + pStratum%rDailyCorrectedLoadEstimate*(rN_h/rN)

!    cumse=cumse+(rmse(i)*rf*rf/(rn*rn))

      ! eqn. I in Baun (1982)
      ! This is the estimated total DAILY mean square error of the
      ! combined strata
      rMSE_d = rMSE_d + pStratum%rDailyMeanSquareError * rN_h**2 / rN**2

      pStats%rCombinedDailyMSE = rMSE_d
      pStats%rCombinedDailyRMSE = sqrt(rMSE_d)

      !df=df+(rf**4*rmse(i)**2/tmp1)
      r_edf=r_edf+(rN_h**4_T_REAL*pStratum%rDailyMeanSquareError**2 / rNsamp_minus_one)

    enddo

    if (r_edf.eq.0) r_edf=-1
    r_edf = (rMSE_d * rN**2 ) **2 /r_edf

  end if

    pStats%rCombinedEffectiveDegreesFreedom = r_edf

  return

end function calculate_effective_degrees_of_freedom

!----------------------------------------------------------------------

function calculate_confidence_interval(rDegreesFreedom, rMSE)  result(r_CI)

  real (kind=T_REAL), intent(in) :: rDegreesFreedom
  real (kind=T_REAL), intent(in) :: rMSE

  integer (kind=T_INT) :: i
  real (kind=T_REAL) :: r_CI, rTval1, rTval2, rTval, rFrac

  real(kind=T_REAL), dimension(30) ::  t05 = (/ &
    12.71,4.3,3.18,2.78,2.57,2.45,2.37,2.31,2.26,2.23, &
    2.2,2.18,2.16,2.14,2.13,2.12,2.11,2.10,2.09,2.09, &
    2.08,2.07,2.07,2.06,2.06,2.06,2.05,2.05,2.05,2.04/)

! calculate the 95% confidence interval half-width (i.e. the �number)
! basically, we're using eq. 12.19, Efron and Tibshirani, p. 159
! to estimate a confidence interval based on the appropriate Student's t-value

  if (rDegreesFreedom >= 30.) then
    rTval=1.96_T_REAL + 2.4_T_REAL / rDegreesFreedom
  else
    rTval1=t05(INT(dmax1(1.0_T_REAL,rDegreesFreedom)))
    rTval2=t05(INT(dmax1(1.0_T_REAL,rDegreesFreedom)+1))
    rFrac=rDegreesFreedom - INT(rDegreesFreedom)
    rTval=rTval1+rFrac*(rTval2-rTval1)
  end if

!    ci=tval*sqrt(mse2)

  r_CI = rTval * sqrt(rMSE)

  ! ! calculate the 95% confidence interval half-width (i.e. the �number)
  !       if (df.ge.30) then
  !       tval=1.96d0+2.4d0/df
  !       else
  !       tval1=t05(idint(dmax1(1.0d0,df)))
  !       tval2=t05(idint(dmax1(1.0d0,df)+1))
  !       frac=df-idint(df)
  !       tval=tval1+frac*(tval2-tval1)
  !       end if
  !       if (nstrata.gt.1) then
  !       ci=tval*sqrt(mse2)
  !       else
  !       ci=tval*sqrt(rmse(1)*0.133225d0)
  !       end if
  !       if (bigio) write (2,15) ci,loadunits(1:2)
  !
  !       if (nstrata.gt.1) then
  !       write (3,11) oldtribname,ce2,mse2,df,ci
  !       else
  !       write (3,11) oldtribname,flowes(1)*0.365d0,rmse(1)*0.133225d0,r(1)-1.,ci

end function calculate_confidence_interval

!----------------------------------------------------------------------


subroutine save_best_result(pBestConfig, pConfig, pBestStrata, pStrata, pBestStats, pStats )

  type (CONFIG_T), pointer                       :: pBestConfig
  type (CONFIG_T), pointer                       :: pConfig
  type (STRATA_T), pointer                       :: pBestStrata
  type (STRATA_T), pointer                       :: pStrata
  type (COMBINED_STATS_T), pointer               :: pBestStats
  type (COMBINED_STATS_T), pointer               :: pStats

  ! [ LOCALS ]
  logical (kind=T_LOGICAL) :: lIsBestConfig

  if ( pConfig%iMinimizationStatistic == MINIMIZE_MEAN_SQUARED_ERROR ) then

    lIsBestConfig = pStats%rCombinedMSE < pBestStats%rCombinedMSE

  elseif ( pConfig%iMinimizationStatistic == MINIMIZE_CONFIDENCE_INTERVAL ) then

    lIsBestConfig = pStats%rCombinedLoadCI < pBestStats%rCombinedLoadCI

  else

    call assert( lFALSE, "Internal programming error: unknown minimization statistic.")

  endif

  if( lIsBestConfig ) then

    pBestConfig = pConfig
    pBestStrata%pStratum = pStrata%pStratum
    pBestStrata%iCurrentNumberOfStrata = pStrata%iCurrentNumberOfStrata
    pBestStats = pStats

    ! pBestConfig%sFlowFileName = pConfig%sFlowFileName
    ! pBestConfig%iFlowFileFormat = pConfig%iFlowFileFormat
    ! pBestConfig%sConcFileName = pConfig%sConcFileName
    ! pBestConfig%sStartDate = pConfig%sStartDate
    ! pBestConfig%sEndDate = pConfig%sEndDate
    ! pBestConfig%iStartDate = pConfig%iStartDate
    ! pBestConfig%iEndDate = pConfig%iEndDate
    ! pBestConfig%iConcFileFormat = pConfig%iConcFileFormat
    ! pBestConfig%sShortOutputFileName = pConfig%sShortOutputFileName
    ! pBestConfig%sExtendedOutputFileName = pConfig%sExtendedOutputFileName
    ! pBestConfig%iFlowUnitsCode = pConfig%iFlowUnitsCode
    ! pBestConfig%iConcUnitsCode = pConfig%iConcUnitsCode
    ! pBestConfig%iLoadUnitsCode = pConfig%iLoadUnitsCode
    ! pBestConfig%sFlowUnits = pConfig%sFlowUnits
    ! pBestConfig%sConcUnits = pConfig%sConcUnits
    ! pBestConfig%sLoadUnits = pConfig%sLoadUnits
    ! pBestConfig%iNumFlowPoints = pConfig%iNumFlowPoints
    ! pBestConfig%iNumConcPoints = pConfig%iNumConcPoints
    ! pBestStrata%iCurrentNumberOfStrata = pStrata%iCurrentNumberOfStrata
    ! pBestConfig%iTotNumDays = pConfig%iTotNumDays
    !
    ! pBestConfig%rStratumCorrectedLoad = pConfig%rStratumCorrectedLoad
    ! pBestConfig%rStratumMeanSquareError = pConfig%rStratumMeanSquareError
    ! pBestConfig%rStratumLoadCI = pConfig%rStratumLoadCI
    !
    ! pBestConfig%iStrataBound = pConfig%iStrataBound
    !
    ! pBestConfig%rCombinedLoad = pStats%rCombinedLoad
    ! pBestConfig%rCombinedLoadAnnualized = pStats%rCombinedLoadAnnualized
    ! pBestConfig%rCombinedLoadAnnualizedCI = pStats%rCombinedLoadAnnualizedCI
    !
    ! pBestConfig%rJackCombinedLoadAnnualized = pStats%rJackCombinedLoadAnnualized
    ! pBestConfig%rJackCombinedLoadAnnualizedCI = pStats%rJackCombinedLoadAnnualizedCI
    !
    ! pBestConfig%rCombinedMSE = pStats%rCombinedMSE
    ! pBestConfig%rCombinedRMSE = pStats%rCombinedRMSE
    !
    ! pBestConfig%rCombinedDailyLoad = pStats%rCombinedDailyLoad
    ! pBestConfig%rCombinedDailyMSE = pStats%rCombinedDailyMSE
    ! pBestConfig%rCombinedDailyRMSE = pStats%rCombinedDailyRMSE
    !
    ! pBestConfig%rTotalFlow = pStats%rTotalFlow
    ! pBestConfig%rTotalFlowAnnualized = pStats%rTotalFlowAnnualized
    !
    ! pBestConfig%rCombinedEffectiveDegreesFreedom = &
    !    pStats%rCombinedEffectiveDegreesFreedom
    ! pBestConfig%rCombinedLoadCI = pStats%rCombinedLoadCI
    !
    ! pBestConfig%iFuncCallNum =   pConfig%iFuncCallNum

  end if

end subroutine save_best_result

!----------------------------------------------------------------------

subroutine reset_combined_stats(pStats)

  type (COMBINED_STATS_T), pointer :: pStats

    pStats%rCombinedLoad = rZERO
    pStats%rCombinedLoadAnnualized = rZERO
    pStats%rCombinedLoadAnnualizedCI = rZERO
    pStats%rCombinedMSE = rZERO
    pStats%rCombinedRMSE = rZERO

    pStats%rCombinedDailyLoad = rZERO
    pStats%rCombinedDailyMSE = rZERO
    pStats%rCombinedDailyRMSE = rZERO

    pStats%rCombinedEffectiveDegreesFreedom = rZERO
    pStats%rCombinedLoadCI = 1.E+27

end subroutine reset_combined_stats

!----------------------------------------------------------------------

end module beale
