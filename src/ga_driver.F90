module ga_driver

!    Opens the window, calls beale subroutine(s)

  use types
  use beale
  use genetic_algorithm
  use beale_data
  use beale_pikaia_func

  implicit none

  contains

subroutine pikaia_driver(pConfig, pBestConfig)


  integer*4 ios,wh, i, j, iNumStrata, iNumBounds, k, ii
  integer*4 iStat

  character*260 outfilename
  character*260 concfilename
  character*260 path
  character*260 name

  character (len=256) :: sRecord, sItem, sBuf, sStartDate, sEndDate
  character (len=1) :: sTab = CHAR(9)


  type (T_CONFIG), pointer :: pConfig,pBestConfig
                                   ! pointer to data structure that contains
                                   ! program options, flags, and other settings

!  type (T_FLOW), dimension(:), pointer :: pFlow
!  type (T_CONC), dimension(:), pointer :: pConc
!  type (T_BEALE_STATS), dimension(:), pointer :: pBealeStats
!  type (T_BEALE_STATS), pointer :: pB

  integer (kind=T_INT) :: iMonth,iDay,iYear
  integer (kind=T_INT) :: iStartDate, iEndDate

  INTEGER             :: seed, STATUS, iNumAttempts
  REAL (kind=T_REAL), dimension(12) :: ctrl(12)
  real (kind=T_REAL), dimension(:),allocatable :: x

  real (kind=T_REAL) :: fb_min,fb_max,fb, rTempval, r_edf, r_CI

!_________________________ End of declarations ________________________


  ALLOCATE (pBealeStats(iMAX_STRATA), STAT=iStat)
  call Assert( LOGICAL( iStat == 0,kind=T_LOGICAL), &
     "Could not allocate memory for BEALE STATS data array")

  ALLOCATE(x(iPikaiaMaxParameters), STAT=iStat)
  call Assert( LOGICAL( iStat == 0,kind=T_LOGICAL), &
     "Could not allocate memory for PIKAIA genotype data structure")


!----------------------------------------------------------------------
! calc_daily_load simply calculates the load for all days for which
!                 we have concentration data:
!                 L = Q x C
!----------------------------------------------------------------------

!  call calc_daily_load(pFlow,pConc,pConfig)

!  call monthly_stats(LU_STD_OUT,pFlow,pConc,pConfig)


!           ctrl( 1) - number of individuals in a population (default
!                      is 100)
!           ctrl( 2) - number of generations over which solution is
!                      to evolve (default is 500)
!           ctrl( 3) - number of significant digits (i.e., number of
!                      genes) retained in chromosomal encoding (default
!                      is 6)  (Note: This number is limited by the
!                      machine floating point precision.  Most 32-bit
!                      floating point representations have only 6 full
!                      digits of precision.  To achieve greater preci-
!                      sion this routine could be converted to double
!                      precision, but note that this would also require
!                      a double precision random number generator, which
!                      likely would not have more than 9 digits of
!                      precision if it used 4-byte integers internally.)
!           ctrl( 4) - crossover probability; must be  <= 1.0 (default
!                      is 0.85). If crossover takes place, either one
!                      or two splicing points are used, with equal
!                      probabilities
!           ctrl( 5) - mutation mode; 1/2/3/4/5 (default is 2)
!                      1=one-point mutation, fixed rate
!                      2=one-point, adjustable rate based on fitness
!                      3=one-point, adjustable rate based on distance
!                      4=one-point+creep, fixed rate
!                      5=one-point+creep, adjustable rate based on fitness
!                      6=one-point+creep, adjustable rate based on distance
!           ctrl( 6) - initial mutation rate; should be small (default
!                      is 0.005) (Note: the mutation rate is the proba-
!                      bility that any one gene locus will mutate in
!                      any one generation.)
!           ctrl( 7) - minimum mutation rate; must be >= 0.0 (default
!                      is 0.0005)
!           ctrl( 8) - maximum mutation rate; must be <= 1.0 (default
!                      is 0.25)
!           ctrl( 9) - relative fitness differential; range from 0
!                      (none) to 1 (maximum).  (default is 1.)
!           ctrl(10) - reproduction plan; 1/2/3=Full generational
!                      replacement/Steady-state-replace-random/Steady-
!                      state-replace-worst (default is 3)
!           ctrl(11) - elitism flag; 0/1=off/on (default is 0)
!                      (Applies only to reproduction plans 1 and 2)
!           ctrl(12) - printed output 0/1/2=None/Minimal/Verbose
!                      (default is 0)


  ctrl(:) = -1
  ctrl(1) = iPikaiaPopulationSize
  ctrl(2) = iPikaiaGenerationLength
  ctrl(3) = iPikaiaNumSigFigs       ! number of significant figures in chromosome
  ctrl(4) = 0.85                    ! crossover probability
  ctrl(5) = 2     ! 2=one-point, adjustable rate based on fitness
  ctrl(7) = 0.005 ! initial mutation rate
  ctrl(8) = 0.25  ! maximum mutation rate
  ctrl(9) = 1.0
  ctrl(10) = 3    ! 3 = steady-state, delete-worst reproduction plan
  ctrl(11) = 0    ! elitism flag
  ctrl(12) = iPikaiaOutputOption

  seed = 3141562
  CALL rninit(seed)

  ! first do the UNSTRATIFIED case...
  iNumStrata = 1
  pConfig%iMaxNumStrata = 1
  pConfig%iStrataBound(0) = MINVAL(pFlow%iJulianDay)
  pConfig%iStrataBound(pConfig%iMaxNumStrata) = MAXVAL(pFlow%iJulianDay)

  pB=>pBealeStats(1)
  pB%iStartDate = pConfig%iStrataBound(0)
  pB%iEndDate =pConfig%iStrataBound(pConfig%iMaxNumStrata)
  call gregorian_date(pConfig%iStrataBound(0), iYear, iMonth, iDay)
  sStartDate = trim(int2char(iMonth))//"/"//trim(int2char(iDay))//"/"//trim(int2char(iYear))
  call gregorian_date(pConfig%iStrataBound(pConfig%iMaxNumStrata), &
     iYear, iMonth, iDay)
  sEndDate = trim(int2char(iMonth))//"/"//trim(int2char(iDay))//"/"//trim(int2char(iYear))

  call Beale_Estimator(pFlow,pConc,pB, pConfig)

  pConfig%rCombinedLoad = pBealeStats(1)%rStratumCorrectedLoad
  pConfig%rCombinedMSE = pBealeStats(1)%rStratumMeanSquareError

  pConfig%rCombinedRMSE = sqrt(pConfig%rCombinedMSE)

  r_edf = rf_effective_degrees_freedom(pConfig,pBealeStats)
  pConfig%rCombinedLoadCI = rf_compute_CI(r_edf, pConfig%rCombinedMSE)

  pConfig%rCombinedLoadAnnualized = pConfig%rCombinedLoad * &
     365.25_T_REAL / REAL(pConfig%iTotNumDays,kind=T_REAL)

    pConfig%rCombinedLoadAnnualizedCI = rf_compute_CI(r_edf, &
       pConfig%rCombinedMSE * 365.25**2 / REAL(pConfig%iTotNumDays**2, &
         kind=T_REAL))

  sBuf = trim(pConc(1)%sTribName)//"_"//trim(pConc(1)%sConstituentName)

  write(LU_STATS_OUT,FMT="(a,a,i4,5a,4(f16.2,a),500a)") &
    trim(sBuf),sTab, &
    iNumStrata, sTab,&
    trim(sStartDate),sTab,trim(sEndDate),sTab, &
    pConfig%rCombinedLoad, sTab, pConfig%rCombinedRMSE, sTab, &
    pConfig%rCombinedEffectiveDegreesFreedom, sTab, &
    pConfig%rCombinedLoadCI, sTab, &
    (pBealeStats(k)%sStartDate,sTab,pBealeStats(k)%sEndDate,sTab, &
      k=1,iNumStrata)

  call save_best(pBestConfig,pConfig)

  if(.not. pConfig%lJackknife) then

    call print_strata_stats(pConfig, pB, pFlow, pConc, 1, LU_STD_OUT)
    call print_strata_summary(pConfig, LU_STD_OUT)

    call print_strata_stats(pConfig,pB, pFlow, pConc, 1, LU_LONG_RPT)
    call print_strata_summary(pConfig, LU_LONG_RPT)

  end if

!  call bealecalc_orig(pConfig, pFlow, pConc, pBealeStats)
  !
  ! Now begin calculations *with* stratification
  !
  do iNumBounds=1,pConfig%iMaxEvalStrata

    pConfig%iFuncCallNum = 0

    ctrl(1) = iPikaiaPopulationSize    ! # individuals
    ctrl(2) = iPikaiaGenerationLength  ! # generations

    iNumStrata = iNumBounds + 1
    pConfig%iMaxNumStrata = iNumStrata

    ! iNumBounds represents the number of BOUNDARIES between strata
    ! therefore, the number of strata is iNumBounds+1
    !

    ! record absolute start and end date values
    pConfig%iStrataBound(0) = MINVAL(pFlow%iJulianDay) - 1
    pConfig%iStrataBound(pConfig%iMaxNumStrata) = MAXVAL(pFlow%iJulianDay)

    if(iNumStrata>(pConfig%iCountUniqueSamples-1)/2) then
      write(6,FMT="('Not enough data to support ',i3,' strata...')") iNumStrata
      cycle
    end if

    iNumAttempts = 1

    do
      if(iNumAttempts>iPikaiaMaxNumAttempts) exit

      call reset_config(pConfig)

      call pikaia(ga_beale,iNumBounds,ctrl,x,fb,status)

      if(fb<0.) then

        iNumAttempts = iNumAttempts + 1
        ctrl(1) = ctrl(1) + 12       ! try again, with larger population
        ctrl(2) = ctrl(2) + 175      ! try again, with more generations

        print *, '#### FAILED TO FIND AN ACCEPTABLE SOLUTION; RERUNNING...'
        print *, '  setting ctrl(1) [population size] =       ',ctrl(1)
        print *, '  setting ctrl(2) [number of generations] = ',ctrl(2)

      else

        exit

      end if

    end do

    ! run Beale one last time with final solution from Pikaia
!    rTempVal = ga_beale(iNumBounds,x)

!    r_edf = rf_effective_degrees_freedom(pConfig,pBealeStats)

!    pConfig%rCombinedLoadCI = rf_compute_CI(r_edf, pConfig%rCombinedMSE)

!    pConfig%rCombinedLoadAnnualized = pConfig%rCombinedLoad * &
!      365. / REAL(pConfig%iTotNumDays,kind=T_REAL)

!    pConfig%rCombinedLoadAnnualizedCI = rf_compute_CI(r_edf, &
!       pConfig%rCombinedMSE * 365.25**2 / REAL(pConfig%iTotNumDays**2, &
!         kind=T_REAL))

    sBuf = trim(pConc(1)%sTribName)//"_"//trim(pConc(1)%sConstituentName)

    write(LU_STATS_OUT,FMT="(a,a,i4,5a,4(f14.2,a),f14.2,500a)") &
      trim(sBuf),sTab,iNumStrata, sTab,&
      trim(sStartDate),sTab,trim(sEndDate),sTab, &
       pConfig%rCombinedLoad, sTab, pConfig%rCombinedLoadCI, sTab, &
       pConfig%rCombinedLoadAnnualized, sTab, &
       pConfig%rCombinedEffectiveDegreesFreedom, sTab, &
         pConfig%rCombinedRMSE, sTab, &
      (pBealeStats(k)%sStartDate,sTab,pBealeStats(k)%sEndDate,sTab, &
        k=1,iNumStrata)

    flush(LU_STATS_OUT)

    ! copy the specifics of the stratification to the Pconfig data structure
    do j=1,iNumStrata
      pB=>pBealeStats(j)
      pConfig%rStratumCorrectedLoad(j) = pB%rStratumCorrectedLoad
      pConfig%rStratumMeanSquareError(j) = pB%rStratumMeanSquareError
      pConfig%rStratumLoadCI(j) = pB%rStratumLoadCI
    end do

    if(.not. pConfig%lJackknife) then

      write(LU_STD_OUT,FMT=*) repeat("_",80)
      write(LU_STD_OUT,FMT=*) " "

      do j=1,iNumStrata
        pB=>pBealeStats(j)
!       call print_strata_stats(pConfig, pB, pFlow, pConc, j, LU_STD_OUT)
        call print_strata_stats(pConfig,pB, pFlow, pConc, j, LU_LONG_RPT)
      end do

      call print_strata_summary(pConfig, LU_STD_OUT)
      call print_strata_summary(pConfig, LU_LONG_RPT)

      write(LU_STD_OUT,FMT=*) " "

    end if

!    call bealecalc_orig(pConfig, pFlow, pConc, pBealeStats)

    call save_best(pBestConfig,pConfig)

  end do

  if(.not. pConfig%lJackknife) then

    write(LU_STD_OUT,*) ''
    write(LU_LONG_RPT,*) ''

    write(LU_STD_OUT,*) repeat("=",80)
    write(LU_LONG_RPT,*) repeat("=",80)

    write(LU_STD_OUT,*) 'SUMMARY of Optimum Strata:'
    write(LU_LONG_RPT,*) 'SUMMARY of Optimum Strata:'

    write(LU_STD_OUT,*) repeat("=",80)
    write(LU_LONG_RPT,*) repeat("=",80)

    call print_strata_summary(pBestConfig, LU_STD_OUT)
    call print_strata_summary(pBestConfig, LU_LONG_RPT)

  end if

!----------------------------------------------------------------------
! Deallocate pointers and clean up
!----------------------------------------------------------------------

  DEALLOCATE (pBealeStats, STAT=iStat)
  call Assert( LOGICAL( iStat == 0,kind=T_LOGICAL), &
     "Could not deallocate memory for BEALE STATS data array")

  DEALLOCATE(x, STAT=iStat)
  call Assert( LOGICAL( iStat == 0,kind=T_LOGICAL), &
     "Could not deallocate memory for PIKAIA genotype data structure")

end subroutine pikaia_driver

end module ga_driver
