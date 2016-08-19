module strata

  use beale, only       : calculate_effective_degrees_of_freedom,     &
                          calculate_confidence_interval,              &
                          calculate_Beale_load
  use beale_data, only  : pFlow
  use types
  use units
  implicit none

contains

subroutine create_new_strata_from_genome( pConfig, x, pStrata )

  type (CONFIG_T), pointer                        :: pConfig
  real (kind=T_REAL), intent(inout)               :: x(:)
  type (STRATA_T), pointer                        :: pStrata

  ! [ LOCALS ]
  type (STRATUM_STATS_T), pointer       :: pStratum
  integer (kind=T_INT)                  :: indx
  integer (kind=T_INT)                  :: iBeginDate
  integer (kind=T_INT)                  :: iEndDate
  integer (kind=T_INT)                  :: iDeltaDate
  integer (kind=T_INT)                  :: iStrataBoundary

  iBeginDate = MINVAL( pFlow%iJulianDay )
  iDeltaDate = MAXVAL( pFlow%iJulianDay ) - MINVAL( pFlow%iJulianDay )
  iEndDate   = MAXVAL( pFlow%iJulianDay )

  ! target the first of the active strata; assign startdate
  pStratum => pStrata%pStratum(1)
  pStratum%iStartDate = pConfig%iStartDate

  ! target the last of the active strata; assign enddate
  pStratum => pStrata%pStratum( pStrata%iCurrentNumberOfStrata )
  pStratum%iEndDate = pConfig%iEndDate

  ! assign strata bounds
  do indx=1, ubound( x, 1 )
    iStrataBoundary = pConfig%iStartDate + iDeltaDate * x(indx)
    ! assign strata boundaries
    pStratum => pStrata%pStratum(indx)
    pStratum%iEndDate = iStrataBoundary

    pStratum => pStrata%pStratum(indx+1)
    pStratum%iStartDate = iStrataBoundary + 1
  end do

  ! do indx=1, pStrata%iCurrentNumberOfStrata-1
  !   pStratum => pStrata%pStratum(indx)
  !   write(*,fmt="(i5,t12,f14.11,t30,a,t45,a)") indx, x(indx), pretty_date(pStratum%iStartDate), pretty_date(pStratum%iEndDate)
  ! enddo
  !
  ! pStratum => pStrata%pStratum( pStrata%iCurrentNumberOfStrata )
  ! write(*,fmt="(i5,t12,a,t30,a,t45,a)") indx, '--', pretty_date(pStratum%iStartDate), pretty_date(pStratum%iEndDate)

end subroutine create_new_strata_from_genome

!--------------------------------------------------------------------------------------------------

subroutine create_genome_from_initial_strata(pConfig,pConc, n, x)

  type (CONC_T), dimension(:), pointer :: pConc
  type (CONFIG_T), pointer :: pConfig ! pointer to data structure that contains
                                      ! program options, flags, and other settings

  integer (kind=T_INT), intent(in) :: n   ! number of BOUNDARIES between STRATA
  real (kind=T_REAL), dimension(:), intent(inout) :: x

  ! [ LOCALS ]
  integer (kind=T_INT) :: i,j, iCount, iCount_old, iMinSamplesPerStratum
  integer (kind=T_INT) :: iStartBound, iEndBound, iMidBound
  integer (kind=T_INT) :: iTotDays

  iMinSamplesPerStratum = MAX(pConfig%iCountUniqueSamples / (n+1),2)

!  print *, 'iMinSamplesPerStratum:', iMinSamplesPerStratum

  iStartBound = pConfig%iStartDate
  iEndBound = pConfig%iStartDate

  iTotDays = pConfig%iEndDate - pConfig%iStartDate + 1

  n_strata: do i=1,n

    iMidBound = -99999

    do

      iEndBound = iEndBound + 1

      ! find the current number of samples present in this stratum
      iCount = COUNT(pConc%iJulianDay>=iStartBound &
               .and. pConc%ijulianDay <= iEndBound &
               .and. pConc%lInclude)

!      print *, iStartBound,'-',iEndBound,':',iCount

      if(iCount < iMinSamplesPerStratum) cycle

      if(iCount>=iMinSamplesPerStratum .and. iMidBound < 0) then
        iMidBound = iEndBound
        iCount_old = iCount
      endif

      if(iCount > iCount_old) then
        ! select the midpoint between sample clusters and reset counters
        x(i) = REAL(REAL(iEndBound+iMidBound)/2. - REAL(pConfig%iStartDate))&
                 / REAL(iTotDays)

        iCount = COUNT(pConc%iJulianDay>=iStartBound &
               .and. pConc%ijulianDay <= (iEndBound+iMidBound)/2. &
               .and. pConc%lInclude)
        !
        ! print *, 'i:',i
        ! print *, 'iStartBound:',iStartBound
        ! print *, 'iEndBound:', iEndBound
        ! print *, 'iMidBound:',iMidbound
        ! print *, 'iTotDays:',iTotDays!
        ! print *, 'iStartDate:',pConfig%iStartDate
        ! print *, 'iCount:', iCount
        ! print *, 'x(i):',x(i)
        ! print *, '---'

        iStartBound = iEndBound
        iMidBound = -99999
        iCount_old = -99999
        exit
      end if

       call Assert(LOGICAL(iEndBound <= pConfig%iEndDate,kind=T_LOGICAL), &
           "Logic error in routine create_genome_from_initial_strata")

     end do

  end do n_strata

end subroutine create_genome_from_initial_strata

!----------------------------------------------------------------------

subroutine check_stratum_validity(pConfig, pFlow, pConc, pStrata, iStrataNum, lValid)

  ! input the parameters required to create a stratum boundary.

  type (CONFIG_T), pointer :: pConfig ! pointer to data structure that contains
                                        ! program options, flags, and other settings
  type (FLOW_T), dimension(:), pointer    :: pFlow
  type (CONC_T), dimension(:), pointer    :: pConc
  type (STRATA_T), pointer                :: pStrata
  integer (kind=T_INT),intent(in)         :: iStrataNum
  logical (kind=T_LOGICAL), intent(inout) :: lValid

  ! [ LOCALS ]
  type (STRATUM_STATS_T), pointer  :: pStratum

  lValid = lTRUE

  if ( iStrataNum > pStrata%iCurrentNumberOfStrata ) then
    write(*,fmt="(a,i5,a,i5)") 'iStrataNum =',iStrataNum, '  pStrata%iCurrentNumberOfStrata', &
      pStrata%iCurrentNumberOfStrata
    call Assert(.False._T_LOGICAL, "Too many strata specified in subroutine check_stratum_validity", __FILE__, __LINE__)
  endif

  pStratum => pStrata%pStratum( iStrataNum )

  ! count of SAMPLES taken within current date range
  pStratum%iNumSamples = COUNT(pConc%iJulianDay >= pStratum%iStartDate       &
                         .and. pConc%iJulianDay <= pStratum%iEndDate   &
                         .and. pConc%lInclude )

  if(pStratum%iEndDate <= pStratum%iStartDate) then
    lValid = lFALSE
  elseif(pStratum%iNumSamples <= 2) then
    lValid = lFALSE
  end if

end subroutine check_stratum_validity

!--------------------------------------------------------------------------------------------------

subroutine calculate_and_combine_stratum_loads( pConfig, pStrata, pStats, pFlow, pConc )

  type (CONFIG_T), pointer                                :: pConfig
  type (STRATA_T), pointer                                :: pStrata
  type (COMBINED_STATS_T), pointer                        :: pStats
  type (FLOW_T), dimension(:), pointer                    :: pFlow
  type (CONC_T), dimension(:), pointer                    :: pConc

  ! [ LOCALS ]
  type (STRATUM_STATS_T), pointer     :: pStratum
  logical (kind=T_LOGICAL)            :: lValid
  integer (kind=T_INT)                :: indx
  real (kind=T_REAL)                  :: r_edf

  pStats%rCombinedLoad = rZERO
  pStats%rCombinedMSE  = rZERO
  pStats%rCombinedRMSE = rZERO

  ! loop over all strata members
  do indx=1, pStrata%iCurrentNumberOfStrata
    pStratum => pStrata%pStratum(indx)

    call check_stratum_validity(pConfig, pFlow, pConc, pStrata, indx, lValid)

    pStratum%sStartDate = pretty_date(pStratum%iStartDate)
    pStratum%sEndDate = pretty_date(pStratum%iEndDate)

    if(lValid) then

      ! date values are legal and at least 2 samples exist for the
      ! current stratum...  O.K. to call calculate_Beale_load

      call calculate_Beale_load(pFlow,pConc,pStratum, pConfig)

      pStats%rCombinedLoad = pStats%rCombinedLoad + pStratum%rStratumCorrectedLoad

      ! Equation M, Baum (1982)
      ! MSE = MSE_d * N^2 = sum(N_h^2 * MSE_hd)
      pStats%rCombinedMSE = pStats%rCombinedMSE + pStratum%rStratumMeanSquareError

!    print *, pretty_date( pStratum%iStartDate), "  ", pretty_date(pStratum%iEndDate), &
!      pStratum%iNumSamples, pStratum%rStratumCorrectedLoad, pStratum%rStratumMeanSquareError

    else

      ! date value is illegal (end date comes before start date)
      ! or less than 2 samples fall within the current date range for the
      ! stratum...

      ! set statistics to default error values
      pStrata%pStratum%rDailyMeanSquareError = -99999.
      pStrata%pStratum%rDailyCorrectedLoadEstimate = -99999.
      pStrata%pStratum%rStratumCorrectedLoad = -99999.
      pStrata%pStratum%rStratumMeanSquareError = -99999.
      pStrata%pStratum%rDailyBiasedLoadEstimate = -99999.
      pStrata%pStratum%rDailyLoadBiasCorrection = -99999.
      pStrata%pStratum%rS_qq = rZERO
      pStrata%pStratum%rS_lq = rZERO
      pStrata%pStratum%rS_ll = rZERO
      pStrata%pStratum%rS_q2l = rZERO
      pStrata%pStratum%rS_ql2 = rZERO
      pStrata%pStratum%rS_q3 = rZERO
      exit

    end if

  end do    ! loop over all strata members

  ! if any of the currently active strata members possesses negative load values,
  ! make the combined stats reflect this
  if( any( pStrata%pStratum(1:pStrata%iCurrentNumberOfStrata )%rStratumCorrectedLoad < 0. ) ) then

    pStats%rCombinedLoad = -HUGE( rZERO )
    pStats%rCombinedMSE = -HUGE( rZERO )
    pStats%rCombinedRMSE = -HUGE( rZERO )
    pStats%rCombinedLoadCI = -HUGE( rZERO )
    pStats%rCombinedLoadAnnualized = -HUGE( rZERO )
    pStats%rCombinedLoadAnnualizedCI = -HUGE( rZERO )

  else

    pStats%rCombinedRMSE = sqrt(pStats%rCombinedMSE)
    r_edf = calculate_effective_degrees_of_freedom(pConfig, pStrata, pStats)

    pStats%rCombinedLoadCI = calculate_confidence_interval(r_edf, pStats%rCombinedMSE)

    pStats%rCombinedLoadAnnualized = pStats%rCombinedLoad * 365. / REAL(pConfig%iTotNumDays,kind=T_REAL)

    pStats%rCombinedLoadAnnualizedCI = calculate_confidence_interval(r_edf, &
       pStats%rCombinedMSE * 365.25**2 / REAL(pConfig%iTotNumDays**2, &
         kind=T_REAL))

  end if

end subroutine calculate_and_combine_stratum_loads

!--------------------------------------------------------------------------------------------------

subroutine reset_strata_stats(pStrata)

  type (STRATA_T), pointer :: pStrata

  pStrata%pStratum%iStartDate = 0
  pStrata%pStratum%iEndDate = 0

  pStrata%pStratum%sStartDate = "NONE"
  pStrata%pStratum%sEndDate = "NONE"

  pStrata%pStratum%iNumDays = 0
  pStrata%pStratum%iNumSamples = 0
  pStrata%pStratum%rMeanFlow = 0

  pStrata%pStratum%rMeanSampleFlow = rZERO
  pStrata%pStratum%rMeanSampleConc = rZERO
  pStrata%pStratum%rMeanSampleLoad = rZERO

  pStrata%pStratum%rDailyBiasedLoadEstimate = rZERO
  pStrata%pStratum%rDailyLoadBiasCorrection = rZERO
  pStrata%pStratum%rDailyCorrectedLoadEstimate = rZERO
  pStrata%pStratum%rDailyMeanSquareError = rZERO
  pStrata%pStratum%rDailySumOfSquareError = rZERO
  pStrata%pStratum%rDailyLoadCI = rZERO

  pStrata%pStratum%rStratumCorrectedLoad = rZERO
  pStrata%pStratum%rStratumMeanSquareError = rZERO
  pStrata%pStratum%rStratumLoadCI = rZERO

  pStrata%pStratum%rS_qq = rZERO
  pStrata%pStratum%rS_lq = rZERO
  pStrata%pStratum%rS_ll = rZERO
  pStrata%pStratum%rS_q2l = rZERO
  pStrata%pStratum%rS_ql2 = rZERO
  pStrata%pStratum%rS_q3 = rZERO

end subroutine reset_strata_stats

!--------------------------------------------------------------------------------------------------

subroutine print_stratum_stats(pConfig, pStrata, pFlow, pConc, iStrataNumber, iLU)

  type (CONFIG_T), pointer             :: pConfig
  type (STRATA_T), pointer             :: pStrata
  type (FLOW_T), dimension(:), pointer :: pFlow
  type (CONC_T), dimension(:), pointer :: pConc

  ![ LOCALS ]
  type (STRATUM_STATS_T), pointer  :: pStratum
  integer (kind=T_INT), intent(in) :: iStrataNumber
  integer (kind=T_INT), intent(in) :: iLU

  integer (kind=T_INT) :: iSMonth, iSDay, iSYear
  integer (kind=T_INT) :: iEMonth, iEDay, iEYear
  integer (kind=T_INT) :: iMonth, iDay, iYear
  integer (kind=T_INT) :: i

!  write(iLU,FMT=*) repeat("-",60)

  if(iStrataNumber == 1) then
    write(iLU,FMT=*) repeat("~",80)
    write(iLU,FMT="(' ===> BEGINNING OF SUMMARY for calculation with ',i3,' strata')") &
      pStrata%iCurrentNumberOfStrata
    write(iLU,FMT="(1x,'FLOW DATA: ',a)") "'"//trim(pConfig%sFlowFileName)//"'"
    write(iLU,FMT="(1x,'CONENTRATION DATA: ',a)") "'"//trim(pConfig%sConcFileName)//"'"
    write(iLU,FMT=*) repeat("~",80)
  end if

  pStratum => pStrata%pStratum( iStrataNumber )

  write(iLU,FMT="(t5,'Stratum number: ',i5)") iStrataNumber

  call gregorian_date(pStratum%iStartDate, iSYear, iSMonth, iSDay)
  call gregorian_date(pStratum%iEndDate, iEYear, iEMonth, iEDay)

  write(iLU,&
   FMT="(t7,'begins on:',3x,i2.2,'/',i2.2,'/',i4,3x'ends on:',3x,i2.2,'/',i2.2,'/',i4)") &
     iSMonth, iSDay, iSYear,iEMonth, iEDay, iEYear

  write(iLU,FMT="(t7,'number of days in stratum: ',t35,i5)") &
     pStratum%iNumDays

  write(iLU, FMT=*) " "

  write(iLU,FMT="(t7,'mean stratum FLOW: ',t48,a)") &
     trim(sf_Q_units(pConfig,pStratum%rMeanFlow))

  write(iLU,FMT="(t7,'RATIO stratum FLOW to sample FLOW: ',t48,f12.2)") &
     pStratum%rMeanFlow / pStratum%rMeanSampleFlow

  write(iLU, FMT=*) " "

  write(iLU,FMT="(t7,'number of samples: ',t30,i5)") &
     pStratum%iNumSamples

  write(iLU, FMT=*) " "

  if(iLU /= LU_STD_OUT) then

    write(iLU,FMT= &
      "('Date           Flow              Concentration       Load')")
    write(iLU,FMT= &
      "('---------    -----------------  -----------------  -----------------')")

    do i=1,size(pConc%rConc)

      if(pStratum%iStartDate<=pConc(i)%iJulianDay .and. &
         pStratum%iEndDate>=pConc(i)%iJulianDay) then

        call gregorian_date(pConc(i)%iJulianDay, iYear, iMonth, iDay)

        write(iLU,FMT="(t4,i2.2,'/',i2.2,'/',i4.4,t16,a,t38,a,t60,a)") &
          iMonth,iDay,iYear, &
               trim(sf_Q_units(pConfig,pConc(i)%rFlow)), &
               trim(sf_C_units(pConfig,pConc(i)%rConc)), &
               trim(sf_L_units(pConfig,pConc(i)%rDailyLoad))

      end if

    end do

    write(iLU,FMT="(t14,4(a20,2x))") &
      '-------------','-------------','-------------'

    write(iLU,FMT="(t4,'    MEAN: ',4(a20,2x))") &
      trim(sf_Q_units(pConfig,pStratum%rMeanSampleFlow)), &
      trim(sf_C_units(pConfig,pStratum%rMeanSampleConc)), &
      trim(sf_L_units(pConfig,pStratum%rMeanSampleLoad))

    write(iLU, FMT=*) " "

    write(iLU, &
      FMT="(t4,'Bias correction factor for STRATUM: ',t60,f14.3)") &
         pStratum%rDailyLoadBiasCorrection

    write(iLU, &
      FMT="(t4,'Biased DAILY load estimate for STRATUM: ',t60,a)") &
         trim(sf_L_units(pConfig,pStratum%rDailyBiasedLoadEstimate))

  end if

  write(iLU, &
     FMT="(t4,'Corrected DAILY load estimate for STRATUM: ',t60,a)") &
        trim(sf_L_units(pConfig,pStratum%rDailyCorrectedLoadEstimate))

  write(iLU, &
     FMT="(t4,'CI for corrected DAILY load estimate for STRATUM: ',t60,a)") &
        trim(sf_L_units(pConfig,pStratum%rDailyLoadCI))

  write(iLU, FMT=*) " "

  write(iLU, &
     FMT="(t4,'MSE estimate for DAILY load estimate for STRATUM: ',t60,a)") &
        trim(sf_L2_units(pConfig,pStratum%rDailyMeanSquareError))

  write(iLU, &
     FMT="(t4,'RMSE estimate for DAILY load estimate for STRATUM: ',t60,a)") &
        trim(sf_L_units(pConfig,sqrt(pStratum%rDailyMeanSquareError)))

  write(iLU, FMT=*) " "

  write(iLU, &
     FMT="(t4,'Corrected load estimate for STRATUM: ',t60,a)") &
        trim(sf_L_units(pConfig,pStratum%rStratumCorrectedLoad))

  write(iLU, &
     FMT="(t4,'CI for load estimate for STRATUM: ',t60,a)") &
        trim(sf_L_units(pConfig,pStratum%rStratumLoadCI))

  write(iLU, FMT=*) " "

  write(iLU, &
     FMT="(t4,'MSE estimate for load estimate for STRATUM: ',t60,a)") &
        trim(sf_L2_units(pConfig,pStratum%rStratumMeanSquareError))

  write(iLU, &
     FMT="(t4,'RMSE estimate for load estimate for STRATUM: ',t60,a)") &
        trim(sf_L_units(pConfig,sqrt(pStratum%rStratumMeanSquareError)))

  write(iLU, FMT=*) " "

  write(iLU,FMT=*) repeat("_",80)

end subroutine print_stratum_stats

end module strata
