module strata

  use types
  use units
  implicit none

contains

subroutine find_initial_strata(pConfig,pConc, n, x)

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


!        print *, 'i:',i
!        print *, 'iStartBound:',iStartBound
!        print *, 'iEndBound:', iEndBound
!        print *, 'iMidBound:',iMidbound
!        print *, 'iTotDays:',iTotDays
!        print *, 'iStartDate:',pConfig%iStartDate
!        print *, 'iCount:', iCount
!        print *, 'x(i):',x(i)
!        print *, '---'

        iStartBound = iEndBound
        iMidBound = -99999
        iCount_old = -99999
        exit
      end if

       call Assert(LOGICAL(iEndBound <= pConfig%iEndDate,kind=T_LOGICAL), &
           "Logic error in routine find_initial_strata")

     end do

  end do n_strata

  return

end subroutine find_initial_strata

!----------------------------------------------------------------------

subroutine assemble_strata(pConfig,pFlow,pConc,pStratum,iStrataNum,lValid)

  ! input the parameters required to create a stratum boundary.

  type (CONFIG_T), pointer :: pConfig ! pointer to data structure that contains
                                        ! program options, flags, and other settings
  type (FLOW_T), dimension(:), pointer :: pFlow
  type (CONC_T), dimension(:), pointer :: pConc
  type (STRATUM_STATS_T), pointer :: pStratum

  integer (kind=T_INT),intent(in) :: iStrataNum
  logical (kind=T_LOGICAL), intent(out) :: lValid

  lValid = lTRUE

  call Assert(LOGICAL(iStrataNum<=pConfig%iMaxNumStrata,kind=T_LOGICAL), &
    "Too many strata specified in subroutine assemble_strata")

  pStratum%iStartDate = pConfig%iStrataBound(iStrataNum - 1) + 1
  pStratum%iEndDate = pConfig%iStrataBound(iStrataNum)

  ! count of SAMPLES taken within current date range
  pStratum%iNumSamples = COUNT(pConc%iJulianDay >= pStratum%iStartDate       &
                         .and. pConc%iJulianDay <= pStratum%iEndDate   &
                         .and. pConc%lInclude )

  if(pStratum%iEndDate <= pStratum%iStartDate) then
    lValid = lFALSE
  elseif(pStratum%iNumSamples <= 2) then
    lValid = lFALSE
  end if

end subroutine assemble_strata

!--------------------------------------------------------------------------------------------------

subroutine reset_stratum_stats(pStratum)

  type (STRATUM_STATS_T), dimension(:), pointer :: pStratum

  pStratum%iStartDate = 0
  pStratum%iEndDate = 0

  pStratum%sStartDate = "NONE"
  pStratum%sEndDate = "NONE"

  pStratum%iNumDays = 0
  pStratum%iNumSamples = 0
  pStratum%rMeanFlow = 0

  pStratum%rMeanSampleFlow = rZERO
  pStratum%rMeanSampleConc = rZERO
  pStratum%rMeanSampleLoad = rZERO

  pStratum%rDailyBiasedLoadEstimate = rZERO
  pStratum%rDailyLoadBiasCorrection = rZERO
  pStratum%rDailyCorrectedLoadEstimate = rZERO
  pStratum%rDailyMeanSquareError = rZERO
  pStratum%rDailySumOfSquareError = rZERO
  pStratum%rDailyLoadCI = rZERO

  pStratum%rStratumCorrectedLoad = rZERO
  pStratum%rStratumMeanSquareError = rZERO
  pStratum%rStratumLoadCI = rZERO

  pStratum%rS_qq = rZERO
  pStratum%rS_lq = rZERO
  pStratum%rS_ll = rZERO
  pStratum%rS_q2l = rZERO
  pStratum%rS_ql2 = rZERO
  pStratum%rS_q3 = rZERO

  return

end subroutine reset_stratum_stats

!--------------------------------------------------------------------------------------------------

subroutine print_stratum_stats(pConfig, pStratum, pFlow, pConc, iStrataNumber, iLU)

  type (CONFIG_T), pointer :: pConfig ! pointer to data structure that contains
                                      ! program options, flags, and other settings
  type (STRATUM_STATS_T), pointer :: pStratum
  type (FLOW_T), dimension(:), pointer :: pFlow
  type (CONC_T), dimension(:), pointer :: pConc

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
      pConfig%iMaxNumStrata
    write(iLU,FMT="(1x,'FLOW DATA: ',a)") "'"//trim(pConfig%sFlowFileName)//"'"
    write(iLU,FMT="(1x,'CONENTRATION DATA: ',a)") "'"//trim(pConfig%sConcFileName)//"'"
    write(iLU,FMT=*) repeat("~",80)
  end if


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

!  write(iLU,FMT="(t4,'S_qq:',t20,F14.3)") pStratum%rS_qq
!  write(iLU,FMT="(t4,'S_ll:',t20,F14.3)") pStratum%rS_ll
!  write(iLU,FMT="(t4,'S_lq:',t20,F14.3)") pStratum%rS_lq
!  write(iLU,FMT="(t4,'S_q^2l:',t20,F14.3)") pStratum%rS_q2l
!  write(iLU,FMT="(t4,'S_q^3:',t20,F14.3)") pStratum%rS_q3
!  write(iLU,FMT="(t4,'S_ql^2:',t20,F14.3)") pStratum%rS_ql2

  return

end subroutine print_stratum_stats

end module strata
