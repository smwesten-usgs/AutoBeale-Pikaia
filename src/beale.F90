module beale

  use types

  implicit none

  contains

subroutine read_data(pConfig, pFlow, pConc)

  ! code has been modified to read tab-delimited file as obtained
  ! from USGS NWIS web interface

  !! [ ARGUMENTS ]
  type (T_CONFIG), pointer :: pConfig ! pointer to data structure that contains
                                        ! program options, flags, and other settings

  type (T_FLOW), dimension(:), pointer :: pFlow
  type (T_CONC), dimension(:), pointer :: pConc

  ! [ LOCALS ]
  integer (kind=T_INT) :: i
  integer (kind=T_INT) :: iStat

  real (kind=T_REAL) :: rValue

  character (len=256) :: sRecord, sItem
  character (len=256) :: sDateStr
  character (len=256) :: sTribName
  character (len=256) :: sBuf

  integer (kind=T_INT) :: iB_Year, iB_Month, iB_Day
  integer (kind=T_INT) :: iE_Year, iE_Month, iE_Day

! read the flow data for a given run, which determines the span of the run (first time only)

  i=0

  write(LU_STD_OUT,*) ">> Reading FLOW data file"
  do

    read ( unit=LU_FLOWDAT, fmt="(a256)", iostat=iStat ) sRecord

    if ( iStat < 0 ) exit     ! EOF mark
    call Assert( LOGICAL( iStat == 0,kind=T_LOGICAL), &
       "Terminating due to error reading FLOW file" )
    if ( sRecord(1:1) == "#" ) cycle      ! Ignore comment lines
    if(  sRecord(1:9) == "agency_cd" ) then
      ! read another line and throw it away
      read ( unit=LU_FLOWDAT, fmt="(a256)", iostat=iStat ) sRecord
      call Assert( LOGICAL( iStat == 0,kind=T_LOGICAL), &
           "Terminating due to error reading FLOW file" )
	  cycle      ! Ignore header information
    end if

    ! if no read errors, increment loop counter
    i=i+1

    if(pConfig%iFlowFileFormat == iFLOW_FILE_FORMAT_ORIGINAL) then

      call CleanUpCsv( sRecord)

      pFlow(i)%sAgencyCode = "99999"
      pFlow(i)%sStationID = "99999"
      call Chomp( sRecord, pFlow(i)%sDate )

      read(pFlow(i)%sDate(1:4),FMT=*,iostat=iStat) pFlow(i)%iYear
      call Assert( LOGICAL( iStat == 0,kind=T_LOGICAL), &
       "Internal read error: pFlow(i)%iYear")

	   read(pFlow(i)%sDate(5:6),FMT=*,iostat=iStat) pFlow(i)%iMonth
      call Assert( LOGICAL( iStat == 0,kind=T_LOGICAL), &
       "Internal read error: pFlow(i)%iMonth")

      read(pFlow(i)%sDate(7:8),FMT=*,iostat=iStat) pFlow(i)%iDay
      call Assert( LOGICAL( iStat == 0,kind=T_LOGICAL), &
       "Internal read error: pFlow(i)%iDay")

   	pFlow(i)%iJulianDay = julian_day ( pFlow(i)%iYear, &
	     pFlow(i)%iMonth, pFlow(i)%iDay )

    	sDateStr = pFlow(i)%sDate(1:4)//pFlow(i)%sDate(5:6)//pFlow(i)%sDate(7:8)

      call Chomp( sRecord, sItem )

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !!!  *** ALERT ***  WE ARE CONVERTING INCOMING FLOWS TO CMS !!!!!
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      read (sItem,*,iostat=iStat) rValue
      call Assert( LOGICAL( iStat == 0,kind=T_LOGICAL), &
       "Internal read error: pFlow(i)%rFlow; sItem="//trim(sItem))
      pFlow(i)%rFlow = rValue * &
          FLOW_UNITS(pConfig%iFlowUnitsCode)%rConversionFactor

    elseif(pConfig%iFlowFileFormat == iFLOW_FILE_FORMAT_USGS) then

      call Chomp_tab( sRecord, pFlow(i)%sAgencyCode )
      call Chomp_tab( sRecord, pFlow(i)%sStationID )
      call Chomp_tab( sRecord, pFlow(i)%sDate )

      call Assert(LOGICAL(len_trim(pFlow(i)%sDate)>0,kind=T_LOGICAL), &
        "Error reading date string in flow file;" &
        //" have you specified the appropriate flow file input format?")

      read(pFlow(i)%sDate(1:4),FMT=*) pFlow(i)%iYear
	   read(pFlow(i)%sDate(6:7),FMT=*) pFlow(i)%iMonth
      read(pFlow(i)%sDate(9:10),FMT=*) pFlow(i)%iDay

   	pFlow(i)%iJulianDay = julian_day ( pFlow(i)%iYear, &
	     pFlow(i)%iMonth, pFlow(i)%iDay )

    	sDateStr = pFlow(i)%sDate(1:4)//pFlow(i)%sDate(6:7)//pFlow(i)%sDate(9:10)

      call Chomp_tab( sRecord, sItem )

      read (sItem,*) pFlow(i)%rFlow

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !!!  *** ALERT ***  WE ARE CONVERTING INCOMING FLOWS TO CMS !!!!!
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      read (sItem,*,iostat=iStat) rValue
      call Assert( LOGICAL( iStat == 0,kind=T_LOGICAL), &
       "Internal read error: pFlow(i)%rFlow; sItem="//trim(sItem))
      pFlow(i)%rFlow = rValue * &
          FLOW_UNITS(pConfig%iFlowUnitsCode)%rConversionFactor

    else

      call Assert(lFALSE,"Unknown or undefined input flow file format")

    end if

    call Assert(LOGICAL(pFlow(i)%iMonth > 0 .and. pFlow(i)%iMonth <= 12, &
      kind=T_LOGICAL),"Month value out of range reading flow file")

    call Assert(LOGICAL(pFlow(i)%iDay > 0 .and. pFlow(i)%iDay <= 31, &
      kind=T_LOGICAL),"Day value out of range reading flow file")

!   #     e  Value has been estimated.
!   #
!   agency_cd	site_no	datetime	04_00060_00003	04_00060_00003_cd
!   5s	15s	16s	14s	14s
!   USGS	040851385	2005-08-01	1970	Ae
!   USGS	040851385	2005-08-02	2050	Ae

	write(LU_STD_OUT,FMT="(i3,': 'a10,1x,i9,1x,f12.4)") i,trim(sDateStr), &
	   pFlow(i)%iJulianDay, rf_Q_disp(pConfig,pFlow(i)%rFlow)

    flush(LU_STD_OUT)

    if(pFlow(i)%rFlow < 0.0) then
      write (*,*) "Missing value code for flow on ",trim(sDateStr), &
        ". There MUST be a flow for every day of the year!"// &
        " Aborting this run!"
      stop
    end if
!
  end do  ! end of loop over days

  pConfig%iTotNumDays = pFlow(i)%iJulianDay - pFlow(1)%iJulianDay + 1

  pConfig%iStartDate = pFlow(1)%iJulianDay
  pConfig%iEndDate = pFlow(i)%iJulianDay

  ! populate the start and end date text strings
  call gregorian_date(MINVAL(pFlow%iJulianDay), iB_Year, iB_Month, iB_Day)
  call gregorian_date(MAXVAL(pFlow%iJulianDay), iE_Year, iE_Month, iE_Day)

  ! populate the starting and ending date strings
  write(sBuf,FMT="(i2.2,'/',i2.2,'/',i4.4)") &
    iB_Month,iB_Day,iB_Year
  pConfig%sStartDate = trim(sBuf)
  write(sBuf,FMT="(i2.2,'/',i2.2,'/',i4.4)") &
    iE_Month,iE_Day,iE_Year
  pConfig%sEndDate = trim(sBuf)

  pConfig%rTotalFlow=SUM(pFlow%rFlow) * 86400_T_REAL    ! cubic meters of water
  pConfig%rTotalFlowAnnualized =  pConfig%rTotalFlow * 365_T_REAL &
      / REAL(pConfig%iTotNumDays,kind=T_REAL)


! read the concentrations for a given load calculation, converting the units as you go
! determine the maximum concentration for scaling purposes in the GUI

  write(*,*)
  write(*,*) ">> Reading CONCENTRATION data file"

!  FORMAT EXPECTED IS SHOWN BELOW:
!  #Trib Name	Constituent	Month	Day	Year	Hour	Minute	Concentration	Units
!  Rock96	    NO23    	01  	06	1997	12	    00	    4.7	            mg/L
!  Rock96	    NO23	    01	    13 	1997	12  	00	    3.6         	mg/L

  i=0

  conc_loop: do

    read ( unit=LU_CONCDAT, fmt="(a256)", iostat=iStat ) sRecord

    if ( iStat < 0 ) then
      exit conc_loop    ! EOF mark
	 end if
    call Assert( LOGICAL( iStat == 0,kind=T_LOGICAL), &
       "Terminating due to error reading CONCENTRATION file" )

    if ( sRecord(1:1) == "#" ) cycle      ! Ignore comment lines

    ! we have a non-comment record; increment loop counter
    i=i+1

    if(pConfig%iConcFileFormat == iCONC_FILE_FORMAT_ORIGINAL) then

      ! file should look like the example below

      ! Rock96NO23	9706161200	2.5

      call CleanUpCsv( sRecord)

      call Chomp( sRecord, pConc(i)%sTribName )

      pConc(i)%sConstituentName = ""

      call Chomp( sRecord, pConc(i)%sDate )

      ! attempt to deal with 2-digit year values
      if ( len_trim( pConc(i)%sDate ) == 10 ) then

        read(pConc(i)%sDate(1:2),FMT=*) pConc(i)%iYear
        read(pConc(i)%sDate(3:4),FMT=*) pConc(i)%iMonth
        read(pConc(i)%sDate(5:6),FMT=*) pConc(i)%iDay

        if(len_trim(pConc(i)%sDate(7:8))>0) &
          read(pConc(i)%sDate(7:8),FMT=*) pConc(i)%iHour

        if(len_trim(pConc(i)%sDate(9:10))>0) &
          read(pConc(i)%sDate(9:10),FMT=*) pConc(i)%iMinute

        ! assumption is that this is a pre-Y2K dataset
        pConc(i)%iYear = pConc(i)%iYear + 1900

      else

        read(pConc(i)%sDate(1:4),FMT=*) pConc(i)%iYear
        read(pConc(i)%sDate(5:6),FMT=*) pConc(i)%iMonth
        read(pConc(i)%sDate(7:8),FMT=*) pConc(i)%iDay

        if(len_trim(pConc(i)%sDate(9:10))>0) &
          read(pConc(i)%sDate(9:10),FMT=*) pConc(i)%iHour

        if(len_trim(pConc(i)%sDate(11:12))>0) &
          read(pConc(i)%sDate(11:12),FMT=*) pConc(i)%iMinute

      endif

      write(pConc(i)%sDate,FMT="(i4,i2.2,i2.2)") pConc(i)%iYear, pConc(i)%iMonth,pConc(i)%iDay

    	pConc(i)%iJulianDay = julian_day ( pConc(i)%iYear, pConc(i)%iMonth, pConc(i)%iDay )

    	sDateStr = pConc(i)%sDate

      call Chomp(sRecord,sItem)

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !!!  *** ALERT ***  WE ARE CONVERTING INCOMING CONC TO mg/L !!!!!
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      read (sItem,*,iostat=iStat) rValue
      call Assert( LOGICAL( iStat == 0,kind=T_LOGICAL), &
       "Internal read error: pConc(i)%rConc; sItem="//trim(sItem))
      pConc(i)%rConc = rValue * &
          CONC_UNITS(pConfig%iConcUnitsCode)%rConversionFactor


   elseif(pConfig%iConcFileFormat == iCONC_FILE_FORMAT_USGS) then

! input should look like this (tab-delimited):
!
!#Trib Name	Constituent	Month	Day	Year	Hour	Minute	Concentration	Units
!Rock96	NO23	01	06	1997	12	00	4.7	mg/L
!Rock96	NO23	01	13	1997	12	00	3.6	mg/L

!      call CleanUpCsv( sRecord)

      call Chomp_tab( sRecord, pConc(i)%sTribName )

      call Chomp_tab(sRecord,pConc(i)%sConstituentName)
!      read (sItem,FMT=*,iostat=iStat) pConc(i)%sConstituentName
!      call Assert(LOGICAL(iStat==0,kind=T_LOGICAL), &
!        "Internal read error: consitutent name;" &
!         //" read in "//trim(sItem))

      call Chomp_tab(sRecord,sItem)
      read (sItem,FMT=*,iostat=iStat) pConc(i)%iMonth
      call Assert(LOGICAL(iStat==0,kind=T_LOGICAL), &
        "Internal read error: month value;" &
         //" read in "//trim(sItem))

      call Chomp_tab(sRecord,sItem)
      read (sItem,FMT=*,iostat=iStat) pConc(i)%iDay
      call Assert(LOGICAL(iStat==0,kind=T_LOGICAL), &
        "Internal read error: day value;" &
         //" read in "//trim(sItem))

      call Chomp_tab(sRecord,sItem)
      read (sItem,FMT=*,iostat=iStat) pConc(i)%iYear
      call Assert(LOGICAL(iStat==0,kind=T_LOGICAL), &
        "Internal read error: year value;" &
         //" read in "//trim(sItem))

      call Chomp_tab(sRecord,sItem)
      read (sItem,FMT=*,iostat=iStat) pConc(i)%iHour
      call Assert(LOGICAL(iStat==0,kind=T_LOGICAL), &
        "Internal read error: hour value;" &
         //" read in "//trim(sItem))

      call Chomp_tab(sRecord,sItem)
      read (sItem,FMT=*,iostat=iStat) pConc(i)%iMinute
      call Assert(LOGICAL(iStat==0,kind=T_LOGICAL), &
        "Internal read error: minute value;" &
         //" read in "//trim(sItem))

      write(pConc(i)%sDate,FMT="(i4,i2.2,i2.2)") pConc(i)%iYear, &
        pConc(i)%iMonth,pConc(i)%iDay

   	pConc(i)%iJulianDay = julian_day ( pConc(i)%iYear, &
   	pConc(i)%iMonth, pConc(i)%iDay )

    	sDateStr = pConc(i)%sDate

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !!!  *** ALERT ***  WE ARE CONVERTING INCOMING CONC TO mg/L !!!!!
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      call Chomp_tab(sRecord,sItem)
      read (sItem,*,iostat=iStat) rValue
      call Assert( LOGICAL( iStat == 0,kind=T_LOGICAL), &
       "Internal read error: pConc(i)%rConc; sItem="//trim(sItem))

      call Chomp_tab(sRecord,sItem)
      pConc(i)%sConcUnits = sItem

      if(i==1) then
        if(trim(pConc(i)%sConcUnits)=="mg/L" &
            .or. trim(pConc(i)%sConcUnits)=="mg/l") then
          pConfig%iConcUnitsCode = iMILLIGRAMS_PER_LITER
        elseif(trim(pConc(i)%sConcUnits)=="g/L" &
            .or. trim(pConc(i)%sConcUnits)=="gg/l") then
          pConfig%iConcUnitsCode = iGRAMS_PER_LITER
        elseif(trim(pConc(i)%sConcUnits)=="ug/L" &
            .or. trim(pConc(i)%sConcUnits)=="ug/l") then
          pConfig%iConcUnitsCode = iMICROGRAMS_PER_LITER
        elseif(trim(pConc(i)%sConcUnits)=="ng/L" &
            .or. trim(pConc(i)%sConcUnits)=="ng/l") then
          pConfig%iConcUnitsCode = iNANOGRAMS_PER_LITER
        else
        call Assert(lFALSE, &
          "No known units assigned in the concentration file;" &
          //" read in '"//trim(pConc(i)%sConcUnits)//"'")
        end if
      end if

      ! now apply the appropriate conversion factor
      pConc(i)%rConc = rValue * &
          CONC_UNITS(pConfig%iConcUnitsCode)%rConversionFactor

   else

     call Assert(lFALSE,"Unknown or undefined input concentration file format")

   endif

   call Assert(LOGICAL(pConc(i)%iMonth > 0 .and. pConc(i)%iMonth <= 12, &
     kind=T_LOGICAL),"Month value out of range reading concentration file")

   call Assert(LOGICAL(pConc(i)%iDay > 0 .and. pConc(i)%iDay <= 31, &
     kind=T_LOGICAL),"Day value out of range reading concentration file")

!    if(pConc(i)%rConc < 0.0) exit

  end do conc_loop

  return

end subroutine read_data

!----------------------------------------------------------------------

subroutine find_initial_strata(pConfig,pConc, n, x)

  type (T_CONC), dimension(:), pointer :: pConc
  type (T_CONFIG), pointer :: pConfig ! pointer to data structure that contains
                                      ! program options, flags, and other settings

  integer (kind=T_INT), intent(in) :: n   ! number of BOUNDARIES

  real (kind=T_REAL), dimension(:), intent(out) :: x

  integer (kind=T_INT) :: i,j, iCount, iCount_old, iMinSamplesPerStrata

  integer (kind=T_INT) :: iStartBound, iEndBound, iMidBound

  integer (kind=T_INT) :: iTotDays

  iMinSamplesPerStrata = MAX(pConfig%iCountUniqueSamples / (n+1),2)

!  print *, 'iMinSamplesPerStrata:', iMinSamplesPerStrata

  iStartBound = pConfig%iStartDate
  iEndBound = pConfig%iStartDate

  iTotDays = pConfig%iEndDate - pConfig%iStartDate + 1

  n_strata: do i=1,n

    iMidBound = -99999

    do

      iEndBound = iEndBound + 1

      iCount = COUNT(pConc%iJulianDay>=iStartBound &
               .and. pConc%ijulianDay <= iEndBound &
               .and. pConc%lInclude)

!      print *, iStartBound,'-',iEndBound,':',iCount

      if(iCount < iMinSamplesPerStrata) cycle

      if(iCount>=iMinSamplesPerStrata .and. iMidBound < 0) then
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

subroutine Beale_Estimator(pFlow,pConc,pB, pConfig)

  type (T_FLOW), dimension(:), pointer :: pFlow
  type (T_CONC), dimension(:), pointer :: pConc
  type (T_BEALE_STATS), pointer :: pB
  type (T_CONFIG), pointer :: pConfig ! pointer to data structure that contains
                                      ! program options, flags, and other settings


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

! Data Types in type pB
!integer (kind=T_INT) :: nDays
!integer (kind=T_INT) :: nNumSamples
!real (kind=T_REAL) :: rMeanFlow

!real (kind=T_REAL) :: rMeanSampleFlow
!real (kind=T_REAL) :: rMeanSampleConc

!real (kind=T_REAL) :: rDailyLoadBiasCorrection
!real (kind=T_REAL) :: rDailyCorrectedLoadEstimate

  ! count of SAMPLES taken within current date range
  pB%iNumSamples = COUNT(pConc%iJulianDay>=pB%iStartDate &
                        .and. pConc%iJulianDay <=pB%iEndDate &
                        .and. pConc%lInclude)

  ! count of ALL flow values within current date range
  pB%iNumDays = COUNT(pFlow%iJulianDay>= pB%iStartDate .and. &
                            pFlow%iJulianDay <= pB%iEndDate)

  ! "theta" is defined in Tin (1965) as (1/n - 1/N)
!  rTheta = (1_T_REAL/REAL(pB%iNumSamples,kind=T_REAL)) &
!               - (1_T_REAL/REAL(pB%iNumDays, kind=T_REAL))

  ! theta is defined in Baun (1982) as the following:
  rTheta = (1_T_REAL/REAL(pB%iNumSamples,kind=T_REAL))

  ! sum ALL flows within current date range (i.e. within current STRATUM)
  rSum = SUM(pFlow%rFlow,(pFlow%iJulianDay>= pB%iStartDate .and. &
                            pFlow%iJulianDay <= pB%iEndDate))

  ! calculate mean STRATUM flow
  if(pB%iNumDays>0) then
    pB%rMeanFlow = rSum / pB%iNumDays
  else
    pB%rMeanFlow = -9999.0
  end if

  ! sum SAMPLE flows within current date range (i.e. within current STRATUM)
  rSum = SUM(pConc%rFlow,(pConc%iJulianDay>= pB%iStartDate &
                      .and. pConc%iJulianDay <= pB%iEndDate &
                      .and. pConc%lInclude))

  ! calculate mean SAMPLE flow
  if(pB%iNumSamples>0) then
    pB%rMeanSampleFlow = rSum / pB%iNumSamples
    rQ_bar = pB%rMeanSampleFlow
    rQ_bar_sq = rQ_bar**2.
  else
    pB%rMeanSampleFlow = -9999.0
    rQ_bar = 0.
    rQ_bar_sq = 0.
  end if


  ! sum SAMPLE *CONC* within current date range (i.e. within current STRATUM)
  rSum = SUM(pConc%rConc,(pConc%iJulianDay>= pB%iStartDate &
                       .and. pConc%iJulianDay <= pB%iEndDate &
                       .and. pConc%lInclude))

  ! calculate mean STRATUM *CONC*
  if(pB%iNumSamples>0) then
    pB%rMeanSampleConc = rSum / pB%iNumSamples
  else
    pB%rMeanSampleConc = -9999.0
  end if

  ! sum SAMPLE *LOAD* within current date range (i.e. within current STRATUM)
  rSum = SUM(pConc%rDailyLoad,(pConc%iJulianDay>= pB%iStartDate &
                       .and. pConc%iJulianDay <= pB%iEndDate &
                       .and. pConc%lInclude))

  ! calculate mean STRATUM *LOAD*
  if(pB%iNumSamples>0) then
    pB%rMeanSampleLoad = rSum / pB%iNumSamples
    rL_bar = pB%rMeanSampleLoad
    rL_bar_sq = rL_bar**2.
  else
    pB%rMeanSampleLoad = -9999.0
    rL_bar = 0.
    rL_bar_sq = 0.
  end if

  ! after call to calc_variance, we will have our std deviation
  ! terms populated within pB
  call calc_variance(pConc, pB)

  rBiasCorr_numerator = 1_T_REAL + (1_T_REAL / REAL(pB%iNumSamples,kind=T_REAL) &
    * pB%rS_lq / rL_bar / rQ_bar)

  rBiasCorr_denominator = 1_T_REAL + (1_T_REAL / REAL(pB%iNumSamples,kind=T_REAL) &
    * pB%rS_qq / (rQ_bar**2))

  pB%rDailyLoadBiasCorrection = rBiasCorr_numerator &
                   / rBiasCorr_denominator

  pB%rDailyBiasedLoadEstimate = pB%rMeanFlow * &
    pB%rMeanSampleLoad / pB%rMeanSampleFlow

  pB%rDailyCorrectedLoadEstimate = pB%rDailyBiasedLoadEstimate * &
     pB%rDailyLoadBiasCorrection

  rMSE_1 = rTheta * ( (pB%rS_qq / rQ_bar_sq) + &
          (pB%rS_ll / rL_bar_sq) - &
          (2_T_REAL * pB%rS_lq / (rL_bar * rQ_bar)) )

  rMSE_2 = rTheta**2 *(  2_T_REAL*(pB%rS_qq**2/rQ_bar_sq**2) - &
            (4_T_REAL*pB%rS_qq*pB%rS_lq/(rQ_bar_sq * rL_bar * rQ_bar)) + &
            (pB%rS_lq**2/((rL_bar*rQ_bar)**2)) + &
            ((pB%rS_qq * pB%rS_ll)/(rQ_bar_sq * rL_bar_sq))  )


  ! this term is from Tin (1965), equation V(t2), top of pp 299.
  rMSE_3 = ( 2_T_REAL * rTheta /   REAL(pB%iNumDays,kind=T_REAL) ) * &
    ( (pB%rS_q3 / rQ_bar**3) - &
    (2_T_REAL*pB%rS_q2l / (rL_bar*rQ_bar**2)) + &
    (pB%rS_ql2 / (rL_bar**2 *rQ_bar)) )

!    write(*,FMT="(3(GS14.6,1x))") rMSE_1,rMSE_2,rMSE_3

  ! Equation C, Baum (1982), with third term from Tin (1965),
  ! for V(t2), top of page 299
  pB%rDailyMeanSquareError = (rL_bar * pB%rMeanFlow / pB%rMeanSampleFlow)**2 &
      * ( rMSE_1 + rMSE_2 + rMSE_3) !* REAL(pB%iNumDays,kind=T_REAL)**2

  ! Equation E, Baum (1982)
  pB%rStratumCorrectedLoad = pB%rDailyCorrectedLoadEstimate * &
      REAL(pB%iNumDays, kind=T_REAL)

  ! Equation F, Baum (1982)
  pB%rStratumMeanSquareError = pB%rDailyMeanSquareError * &
    REAL(pB%iNumDays**2,kind=T_REAL)

  pB%rDailyLoadCI = rf_compute_CI(REAL(pB%iNumSamples - 1, kind=T_REAL), &
      pB%rDailyMeanSquareError)


  pB%rStratumLoadCI = rf_compute_CI(REAL(pB%iNumSamples - 1, kind=T_REAL), &
      pB%rStratumMeanSquareError)


  return

end subroutine Beale_Estimator

!-----------------------------------------------------------------------

subroutine calc_variance(pConc,pB)

  type (T_CONC), dimension(:), pointer :: pConc
  type (T_BEALE_STATS), pointer :: pB

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

  ! the value in pB must be current or we get garbage!
  n = pB%iNumSamples

  call Assert(LOGICAL(n==COUNT(pConc%iJulianDay >= pB%iStartDate &
     .and. pConc%iJulianDay <= pB%iEndDate &
     .and. pConc%lInclude),kind=T_LOGICAL), &
     "Mismatch between sample number calculation methods; routine calc_variance")

  if(n>1) then

    rTheta = 1_T_REAL / (REAL(n,kind=T_REAL)-1_T_REAL)

    do i=1,size(pConc%rConc)

      if(pConc(i)%iJulianDay >= pB%iStartDate &
         .and. pConc(i)%iJulianDay <= pB%iEndDate &
         .and. pConc(i)%lInclude) then

        ! update accumulators

        rSum_lq = rSum_lq + pConc(i)%rLoadTimesFlow &
                   - (pB%rMeanSampleLoad * pB%rMeanSampleFlow)

        rSum_qq = rSum_qq + (pConc(i)%rFlow - pB%rMeanSampleFlow)**2

        rSum_ll = rSum_ll + (pConc(i)%rDailyLoad - pB%rMeanSampleLoad)**2

        rSum_q2l = rSum_q2l + (pConc(i)%rFlow - pB%rMeanSampleFlow)**2 &
                    * (pConc(i)%rDailyLoad - pB%rMeanSampleLoad)

        rSum_q3 = rSum_q3 + (pConc(i)%rFlow - pB%rMeanSampleFlow)**3

        rSum_ql2 = rSum_ql2 + (pConc(i)%rFlow - pB%rMeanSampleFlow) &
                    * (pConc(i)%rDailyLoad-pB%rMeanSampleLoad)**2

      end if

    end do

    pB%rS_lq = rTheta * rSum_lq     ! this is S_xy in Baun's paper, eqn B
    pB%rS_qq = rTheta * rSum_qq     ! this is S_x**2 in Baun's paper, eqn B
    pB%rS_ll = rTheta * rSum_ll
    pB%rS_q2l = rTheta * rSum_q2l
    pB%rS_q3 = rTheta * rSum_q3
    pB%rS_ql2 = rTheta * rSum_ql2

!     sumx3=sumx3 + (pConc(i)%rFlow-avflow)**3
!   sumx2y=sumx2y + (pConc(i)%rFlow-avflow)**2 * (pConc(i)%rDailyLoad-avload)
!   sumxy2=sumxy2 + (pConc(i)%rFlow-avflow) * (pConc(i)%rDailyLoad-avload)**2

  else

    pB%rS_lq = rZERO
    pB%rS_qq = rZERO
    pB%rS_ll = rZERO
    pB%rS_q2l = rZERO
    pB%rS_q3 = rZERO
    pB%rS_ql2 = rZERO

  end if

  return

end subroutine calc_variance

!----------------------------------------------------------------------

subroutine clean_flow_data(pFlow)

  type (T_FLOW), dimension(:), pointer :: pFlow

  integer (kind=T_INT) :: i, iCount, iStat, iJulianDay, iNextJulianDay
  integer (kind=T_INT) :: iMM, iDD, iYYYY
  character (len=256) :: sBuf

  do i=1,size(pFlow)-1
    iJulianDay = pFlow(i)%iJulianDay
    iNextJulianDay = pFlow(i+1)%iJulianDay
    call gregorian_date(iJulianDay,iYYYY, iMM, iDD)
    write(sBuf,FMT="(i2.2,'/',i2.2,'/',i4.4)") iMM, iDD, iYYYY
    call Assert(LOGICAL(iJulianDay + 1 == iNextJulianDay, kind=T_LOGICAL), &
      'Flow missing or out of order: '//trim(sBuf))
    call Assert(LOGICAL(pFlow(i)%rFlow>rZERO, kind=T_LOGICAL), &
      'Missing flow value (negative value detected): '//trim(sBuf))

  end do

  return

end subroutine clean_flow_data

!----------------------------------------------------------------------

subroutine clean_concentration_data(pFlow,pConc)

  type (T_FLOW), dimension(:), pointer :: pFlow
  type (T_CONC), dimension(:), pointer :: pConc

  type (T_CONC), dimension(:), pointer :: pCopy

  integer (kind=T_INT) :: i, iCount, iStat, iNumValues, j, k

  iCount = 0

  ! here we are counting the number of days within the range of interest
  ! for which we have one or more concentration data available
  do i=MINVAL(pFlow%iJulianDay),MAXVAL(pFlow%iJulianDay)
    if(COUNT(pConc%iJulianDay==i)>0) then
      iCount=iCount+1
    end if
  end do

  ALLOCATE(pCopy(iCount),STAT=iStat)
  call Assert(iStat==0, &
    "Problem allocating memory in function clean_conc_data")

  iCount = 0
  ! now iterate over the date range again, copying and averaging where
  ! necessary
  do i=MINVAL(pFlow%iJulianDay),MAXVAL(pFlow%iJulianDay)
    iNumValues = COUNT(pConc%iJulianDay==i)
    if(iNumValues>0) then
      iCount = iCount + 1
      do j=1,SIZE(pConc)
        if(pConc(j)%iJulianDay == i) then
          pCopy(iCount) = pConc(j)
          if(iNumValues>1) then
            pCopy(iCount)%iHour = 99
            pCopy(iCount)%iMinute = 99
            pCopy(iCount)%rConc = SUM(pConc%rConc,pConc%iJulianDay==i) &
                             / iNumValues
          end if
        end if
      end do
    end if
  end do

  deallocate(pConc,STAT=iStat)
  call Assert(iStat==0, &
    "Problem deallocating memory in function clean_conc_data")

  ! now point 'pConc' at our modified copy
  pConc => pCopy

  return

end subroutine clean_concentration_data

!----------------------------------------------------------------------

subroutine calc_daily_load(pFlow,pConc,pConfig)

  type (T_FLOW), dimension(:), pointer :: pFlow
  type (T_CONC), dimension(:), pointer :: pConc
  type (T_CONFIG), pointer :: pConfig ! pointer to data structure that contains
                                        ! program options, flags, and other settings

  integer (kind=T_INT) :: i,j
  integer (kind=T_INT) :: iDayOfYear,iNumDaysInYear
  real (kind=T_REAL) :: rFlow
  real (kind=T_REAL) :: rYearFrac
  logical (kind=T_LOGICAL) :: lSetSampleDayToTrue = lTRUE

  write(LU_STD_OUT,FMT="(t23,'Flow',t40,'Conc',t58,'Load')")
  write(LU_STD_OUT,FMT="(t23,'----',t40,'----',t58,'----')")

  pConfig%iCountUniqueSamples = 0

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


end subroutine calc_daily_load

!-----------------------------------------------------------------------

subroutine assemble_strata(pConfig,pFlow,pConc,pB,iStrataNum,lValid)


  ! input the parameters required to create a stratum boundary.

  type (T_CONFIG), pointer :: pConfig ! pointer to data structure that contains
                                        ! program options, flags, and other settings
  type (T_FLOW), dimension(:), pointer :: pFlow
  type (T_CONC), dimension(:), pointer :: pConc
  type (T_BEALE_STATS), pointer :: pB

  integer (kind=T_INT),intent(in) :: iStrataNum
  logical (kind=T_LOGICAL), intent(out) :: lValid

  lValid = lTRUE

  call Assert(LOGICAL(iStrataNum<=pConfig%iMaxNumStrata,kind=T_LOGICAL), &
    "Too many strata specified in subroutine assemble_strata")

  pB%iStartDate = pConfig%iStrataBound(iStrataNum - 1) + 1
  pB%iEndDate = pConfig%iStrataBound(iStrataNum)

  ! count of SAMPLES taken within current date range
  pB%iNumSamples = COUNT(pConc%iJulianDay>=pB%iStartDate &
                       .and. pConc%iJulianDay <=pB%iEndDate &
                       .and. pConc%lInclude )

  if(pB%iEndDate <= pB%iStartDate) then
    lValid = lFALSE
  elseif(pB%iNumSamples <= 2) then
    lValid = lFALSE
  end if

  return

end subroutine assemble_strata

!-----------------------------------------------------------------------

subroutine monthly_stats(iLU,pFlow,pConc,pConfig)

  integer (kind=T_INT), intent(in) :: iLU         ! logical unit for output
  type (T_FLOW), dimension(:), pointer :: pFlow
  type (T_CONC), dimension(:), pointer :: pConc
  type (T_CONFIG), pointer :: pConfig ! pointer to data structure that contains
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

end subroutine monthly_stats

!-----------------------------------------------------------------------

function get_flow(iJulianDay,pFlow)  result(rFlow)

  ! [ ARGUMENTS ]
  integer (kind=T_INT),intent(in) :: iJulianDay
  type (T_FLOW), dimension(:), pointer :: pFlow
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

subroutine bealecalc_orig(pConfig, pFlow, pConc, pB)

      type (T_CONFIG), pointer :: pConfig ! pointer to data structure that contains
                                          ! program options, flags, and other settings

      type (T_FLOW), dimension(:), pointer :: pFlow
      type (T_CONC), dimension(:), pointer :: pConc
      type (T_BEALE_STATS),dimension(:), pointer :: pB

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

      real(kind=T_REAL),dimension(pConfig%iMaxNumStrata) :: &
         flowes, rmse, r, nf

      integer (kind=T_INT) :: l,i, j, ndays

      ! define ndays = TOTAL number of flow records
      ndays = SIZE(pFlow%rFlow)

      do j=1,pConfig%iMaxNumStrata

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
        if (pFlow(l)%iJulianDay<=pB(j)%iEndDate .and. &
            pFlow(l)%iJulianDay>=pB(j)%iStartDate) then
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
        if (pFlow(l)%iJulianDay<=pB(j)%iEndDate .and. &
            pFlow(l)%iJulianDay>=pB(j)%iStartDate)then
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

!      write(LU_STD_OUT,FMT="(t4,'sx2 (orig):',t20,F14.3)") sx2
!      write(LU_STD_OUT,FMT="(t4,'sy2 (orig):',t20,F14.3)") sy2
!      write(LU_STD_OUT,FMT="(t4,'sxy (orig):',t20,F14.3)") sxy
!      write(LU_STD_OUT,FMT="(t4,'sx2y (orig):',t20,F14.3)") sx2y
!      write(LU_STD_OUT,FMT="(t4,'sx3 (orig):',t20,F14.3)") sx3
!      write(LU_STD_OUT,FMT="(t4,'sxy2 (orig):',t20,F14.3)") sxy2

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

    end do

      do i=1,pConfig%iMaxNumStrata
        tmp1=r(i)-1.0_T_REAL

        rf = REAL(pB(i)%iNumDays,kind=T_REAL)
        rn = REAL(pConfig%iTotNumDays,kind=T_REAL)

        cumfl=cumfl+flowes(i)*(rf/rn)        ! eqn. H in Baum (1982)
!        print *,'cumse,rmse(i),rf,rn'
!        print *,cumse,rmse(i),rf,rn
!        print *, ' '
        cumse=cumse+(rmse(i)*rf*rf/(rn*rn))   ! eqn. I in Baum (1982)
        df=df+(rf**4*rmse(i)**2/tmp1)
      enddo

      if(pConfig%iMaxNumStrata .gt. 1) then
        if (df.eq.0) df=-1
         df=(cumse*rn*rn)*(cumse*rn*rn)/df
         ce1=cumfl*365.0_T_REAL
         ce2=cumfl*0.365_T_REAL
         mse1=cumse*133225.0_T_REAL  ! 133225 = 365**2
         mse2=cumse*0.133225_T_REAL  ! this is a rough translation of
      end if                         ! eqn. M in Baum (1982)


!      print *,'pConfig%iMaxNumStrata,tmp1,cumfl,cumse,df, rn'
!
!      write(*,FMT="(i3,1x,20(f14.3,1x))") pConfig%iMaxNumStrata,tmp1,&
!         cumfl,cumse,df, rn


! calculate the 95% confidence interval half-width (i.e. the �number)
      if (df.ge.30) then
        tval=1.96_T_REAL+2.4_T_REAL/df
      else
        tval1=t05(INT(dmax1(1.0_T_REAL,df)))
        tval2=t05(INT(dmax1(1.0_T_REAL,df)+1))
        frac=df-INT(df)
        tval=tval1+frac*(tval2-tval1)
      end if
      if (pConfig%iMaxNumStrata.gt.1) then
        ci=tval*sqrt(mse2)
      else
        ci=tval*sqrt(rmse(1)*0.133225_T_REAL)
      end if

      write(LU_STD_OUT,fmt="(a)") 'OLD AUTOBEALE:     load         mse                df         ci'

      if (pConfig%iMaxNumStrata.gt.1) then
        write (LU_STD_OUT,11) 'OLD AUTOBEALE:',ce2*1000.,mse2*1.e+06, &
           df,ci*1000.
      else
        write (LU_STD_OUT,11) 'OLD AUTOBEALE:',flowes(1)*0.365_T_REAL*1000., &
           rmse(1)*0.133225_T_REAL*1.e+06,r(1)-1.,ci*1000.
      end if

11      format (a,2f16.4,f10.3,f16.4)


300   return

  end subroutine bealecalc_orig

!-----------------------------------------------------------------------

function rf_effective_degrees_freedom(pConfig,pB)  result(r_edf)

  type (T_CONFIG), pointer :: pConfig ! pointer to data structure that contains
                                      ! program options, flags, and other settings

  type (T_BEALE_STATS), dimension(:), pointer :: pB

  integer (kind=T_INT) :: i

  real (kind=T_REAL) :: r_edf,rNsamp_minus_one,rN_h,rN,rL_d,rMSE_d

  r_edf = rZERO
  rN_h = rZERO
  rN = rZERO
  rL_d = rZERO
  rMSE_d = rZERO


  call Assert(LOGICAL(pConfig%iMaxNumStrata>0,kind=T_LOGICAL), &
    'Function rf_effective_degrees_freedom called with df < 1')

  if(pConfig%iMaxNumStrata==1) then

    r_edf = REAL(pB(1)%iNumSamples,kind=T_REAL)-1.0_T_REAL  !r
    rMSE_d = pB(1)%rDailyMeanSquareError
    pConfig%rCombinedDailyMSE = rMSE_d

  else

    do i=1,pConfig%iMaxNumStrata
!    tmp1=r(i)-1.0_T_REAL
      rNsamp_minus_one = REAL(pB(i)%iNumSamples,kind=T_REAL)-1.0_T_REAL  !r

      rN_h = REAL(pB(i)%iNumDays,kind=T_REAL)    !rf
      rN = REAL(pConfig%iTotNumDays,kind=T_REAL)  !rn

!    cumfl=cumfl+flowes(i)*(rf/rn)        ! eqn. H in Baum (1982)

      ! eqn. H in Baum (1982)
      rL_d = rL_d + pB(i)%rDailyCorrectedLoadEstimate*(rN_h/rN)

!    cumse=cumse+(rmse(i)*rf*rf/(rn*rn))

      ! eqn. I in Baun (1982)
      ! This is the estimated total DAILY mean square error of the
      ! combined strata
      rMSE_d = rMSE_d + pB(i)%rDailyMeanSquareError * rN_h**2 / rN**2

      pConfig%rCombinedDailyMSE = rMSE_d
      pConfig%rCombinedDailyRMSE = sqrt(rMSE_d)

      !df=df+(rf**4*rmse(i)**2/tmp1)
      r_edf=r_edf+(rN_h**4_T_REAL*pB(i)%rDailyMeanSquareError**2 &
          / rNsamp_minus_one)

    enddo

    if (r_edf.eq.0) r_edf=-1
    r_edf=(rMSE_d*rN**2)**2/r_edf

  end if

    pConfig%rCombinedEffectiveDegreesFreedom = r_edf

  return

end function rf_effective_degrees_freedom

!----------------------------------------------------------------------

function rf_compute_CI(rDegreesFreedom, rMSE)  result(r_CI)

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

  return

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

end function rf_compute_CI

!----------------------------------------------------------------------

subroutine print_strata_stats(pConfig, pB, pFlow, pConc, iStrataNumber, iLU)

  type (T_CONFIG), pointer :: pConfig ! pointer to data structure that contains
                                      ! program options, flags, and other settings
  type (T_BEALE_STATS), pointer :: pB
  type (T_FLOW), dimension(:), pointer :: pFlow
  type (T_CONC), dimension(:), pointer :: pConc

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

  call gregorian_date(pB%iStartDate, iSYear, iSMonth, iSDay)
  call gregorian_date(pB%iEndDate, iEYear, iEMonth, iEDay)

  write(iLU,&
   FMT="(t7,'begins on:',3x,i2.2,'/',i2.2,'/',i4,3x'ends on:',3x,i2.2,'/',i2.2,'/',i4)") &
     iSMonth, iSDay, iSYear,iEMonth, iEDay, iEYear

  write(iLU,FMT="(t7,'number of days in stratum: ',t35,i5)") &
     pB%iNumDays

  write(iLU, FMT=*) " "

  write(iLU,FMT="(t7,'mean stratum FLOW: ',t48,a)") &
     trim(sf_Q_units(pConfig,pB%rMeanFlow))

  write(iLU,FMT="(t7,'RATIO stratum FLOW to sample FLOW: ',t48,f12.2)") &
     pB%rMeanFlow / pB%rMeanSampleFlow

  write(iLU, FMT=*) " "

  write(iLU,FMT="(t7,'number of samples: ',t30,i5)") &
     pB%iNumSamples

  write(iLU, FMT=*) " "

  if(iLU /= LU_STD_OUT) then

    write(iLU,FMT= &
      "('Date           Flow              Concentration       Load')")
    write(iLU,FMT= &
      "('---------    -----------------  -----------------  -----------------')")

    do i=1,size(pConc%rConc)

      if(pB%iStartDate<=pConc(i)%iJulianDay .and. &
         pB%iEndDate>=pConc(i)%iJulianDay) then

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
      trim(sf_Q_units(pConfig,pB%rMeanSampleFlow)), &
      trim(sf_C_units(pConfig,pB%rMeanSampleConc)), &
      trim(sf_L_units(pConfig,pB%rMeanSampleLoad))

    write(iLU, FMT=*) " "

    write(iLU, &
      FMT="(t4,'Bias correction factor for STRATUM: ',t60,f14.3)") &
         pB%rDailyLoadBiasCorrection

    write(iLU, &
      FMT="(t4,'Biased DAILY load estimate for STRATUM: ',t60,a)") &
         trim(sf_L_units(pConfig,pB%rDailyBiasedLoadEstimate))

  end if

  write(iLU, &
     FMT="(t4,'Corrected DAILY load estimate for STRATUM: ',t60,a)") &
        trim(sf_L_units(pConfig,pB%rDailyCorrectedLoadEstimate))

  write(iLU, &
     FMT="(t4,'CI for corrected DAILY load estimate for STRATUM: ',t60,a)") &
        trim(sf_L_units(pConfig,pB%rDailyLoadCI))

  write(iLU, FMT=*) " "

  write(iLU, &
     FMT="(t4,'MSE estimate for DAILY load estimate for STRATUM: ',t60,a)") &
        trim(sf_L2_units(pConfig,pB%rDailyMeanSquareError))

  write(iLU, &
     FMT="(t4,'RMSE estimate for DAILY load estimate for STRATUM: ',t60,a)") &
        trim(sf_L_units(pConfig,sqrt(pB%rDailyMeanSquareError)))

  write(iLU, FMT=*) " "

  write(iLU, &
     FMT="(t4,'Corrected load estimate for STRATUM: ',t60,a)") &
        trim(sf_L_units(pConfig,pB%rStratumCorrectedLoad))

  write(iLU, &
     FMT="(t4,'CI for load estimate for STRATUM: ',t60,a)") &
        trim(sf_L_units(pConfig,pB%rStratumLoadCI))

  write(iLU, FMT=*) " "

  write(iLU, &
     FMT="(t4,'MSE estimate for load estimate for STRATUM: ',t60,a)") &
        trim(sf_L2_units(pConfig,pB%rStratumMeanSquareError))

  write(iLU, &
     FMT="(t4,'RMSE estimate for load estimate for STRATUM: ',t60,a)") &
        trim(sf_L_units(pConfig,sqrt(pB%rStratumMeanSquareError)))

  write(iLU, FMT=*) " "

  write(iLU,FMT=*) repeat("_",80)

!  write(iLU,FMT="(t4,'S_qq:',t20,F14.3)") pB%rS_qq
!  write(iLU,FMT="(t4,'S_ll:',t20,F14.3)") pB%rS_ll
!  write(iLU,FMT="(t4,'S_lq:',t20,F14.3)") pB%rS_lq
!  write(iLU,FMT="(t4,'S_q^2l:',t20,F14.3)") pB%rS_q2l
!  write(iLU,FMT="(t4,'S_q^3:',t20,F14.3)") pB%rS_q3
!  write(iLU,FMT="(t4,'S_ql^2:',t20,F14.3)") pB%rS_ql2

  return

end subroutine print_strata_stats


subroutine save_best(pBestConfig, pConfig)

  type (T_CONFIG), pointer :: pBestConfig ! pointer to data structure that contains
                                          ! program options, flags, and other settings

  type (T_CONFIG), pointer :: pConfig ! pointer to data structure that contains
                                      ! program options, flags, and other settings

  if(pConfig%rCombinedLoadCI < pBestConfig%rCombinedLoadCI) then

    pBestConfig%sFlowFileName = pConfig%sFlowFileName
    pBestConfig%iFlowFileFormat = pConfig%iFlowFileFormat
    pBestConfig%sConcFileName = pConfig%sConcFileName
    pBestConfig%sStartDate = pConfig%sStartDate
    pBestConfig%sEndDate = pConfig%sEndDate
    pBestConfig%iStartDate = pConfig%iStartDate
    pBestConfig%iEndDate = pConfig%iEndDate
    pBestConfig%iConcFileFormat = pConfig%iConcFileFormat
    pBestConfig%sShortOutputFileName = pConfig%sShortOutputFileName
    pBestConfig%sExtendedOutputFileName = pConfig%sExtendedOutputFileName
    pBestConfig%iFlowUnitsCode = pConfig%iFlowUnitsCode
    pBestConfig%iConcUnitsCode = pConfig%iConcUnitsCode
    pBestConfig%iLoadUnitsCode = pConfig%iLoadUnitsCode
    pBestConfig%sFlowUnits = pConfig%sFlowUnits
    pBestConfig%sConcUnits = pConfig%sConcUnits
    pBestConfig%sLoadUnits = pConfig%sLoadUnits
    pBestConfig%iNumFlowPoints = pConfig%iNumFlowPoints
    pBestConfig%iNumConcPoints = pConfig%iNumConcPoints
    pBestConfig%iMaxNumStrata = pConfig%iMaxNumStrata
    pBestConfig%iTotNumDays = pConfig%iTotNumDays

    pBestConfig%rStratumCorrectedLoad = pConfig%rStratumCorrectedLoad
    pBestConfig%rStratumMeanSquareError = pConfig%rStratumMeanSquareError
    pBestConfig%rStratumLoadCI = pConfig%rStratumLoadCI

    pBestConfig%iStrataBound = pConfig%iStrataBound

    pBestConfig%rCombinedLoad = pConfig%rCombinedLoad
    pBestConfig%rCombinedLoadAnnualized = pConfig%rCombinedLoadAnnualized
    pBestConfig%rCombinedLoadAnnualizedCI = pConfig%rCombinedLoadAnnualizedCI

    pBestConfig%rJackCombinedLoadAnnualized = pConfig%rJackCombinedLoadAnnualized
    pBestConfig%rJackCombinedLoadAnnualizedCI = pConfig%rJackCombinedLoadAnnualizedCI

    pBestConfig%rCombinedMSE = pConfig%rCombinedMSE
    pBestConfig%rCombinedRMSE = pConfig%rCombinedRMSE

    pBestConfig%rCombinedDailyLoad = pConfig%rCombinedDailyLoad
    pBestConfig%rCombinedDailyMSE = pConfig%rCombinedDailyMSE
    pBestConfig%rCombinedDailyRMSE = pConfig%rCombinedDailyRMSE

    pBestConfig%rTotalFlow = pConfig%rTotalFlow
    pBestConfig%rTotalFlowAnnualized = pConfig%rTotalFlowAnnualized

    pBestConfig%rCombinedEffectiveDegreesFreedom = &
       pConfig%rCombinedEffectiveDegreesFreedom
    pBestConfig%rCombinedLoadCI = pConfig%rCombinedLoadCI

    pBestConfig%iFuncCallNum =   pConfig%iFuncCallNum

  end if

  return

end subroutine save_best

!----------------------------------------------------------------------
subroutine reset_bealestats(pB)

  type (T_BEALE_STATS), dimension(:), pointer :: pB

  pB%iStartDate = 0
  pB%iEndDate = 0

  pB%sStartDate = "NONE"
  pB%sEndDate = "NONE"

  pB%iNumDays = 0
  pB%iNumSamples = 0
  pB%rMeanFlow = 0

  pB%rMeanSampleFlow = rZERO
  pB%rMeanSampleConc = rZERO
  pB%rMeanSampleLoad = rZERO

  pB%rDailyBiasedLoadEstimate = rZERO
  pB%rDailyLoadBiasCorrection = rZERO
  pB%rDailyCorrectedLoadEstimate = rZERO
  pB%rDailyMeanSquareError = rZERO
  pB%rDailySumOfSquareError = rZERO
  pB%rDailyLoadCI = rZERO

  pB%rStratumCorrectedLoad = rZERO
  pB%rStratumMeanSquareError = rZERO
  pB%rStratumLoadCI = rZERO

  pB%rS_qq = rZERO
  pB%rS_lq = rZERO
  pB%rS_ll = rZERO
  pB%rS_q2l = rZERO
  pB%rS_ql2 = rZERO
  pB%rS_q3 = rZERO

  return

end subroutine reset_bealestats

!----------------------------------------------------------------------

subroutine reset_config(pConfig)

  type (T_CONFIG), pointer :: pConfig ! pointer to data structure that contains
                                      ! program options, flags, and other settings

    pConfig%rCombinedLoad = rZERO
    pConfig%rCombinedLoadAnnualized = rZERO
    pConfig%rCombinedLoadAnnualizedCI = rZERO
    pConfig%rCombinedMSE = rZERO
    pConfig%rCombinedRMSE = rZERO

    pConfig%rCombinedDailyLoad = rZERO
    pConfig%rCombinedDailyMSE = rZERO
    pConfig%rCombinedDailyRMSE = rZERO

    pConfig%rCombinedEffectiveDegreesFreedom = &
       rZERO
    pConfig%rCombinedLoadCI = 1.E+27

    pConfig%iFuncCallNum = rZERO

  return

end subroutine reset_config

!----------------------------------------------------------------------

subroutine print_short_report(pConfig, pConc)

  type (T_CONFIG), pointer :: pConfig ! pointer to data structure that contains
                                        ! program options, flags, and other settings
  type (T_CONC), dimension(:), pointer :: pConc

  character (len=1)   :: sTab = CHAR(9)
  character (len=32)  :: sTimeStamp

!  if(pConfig%lJackknife) then

    sTimeStamp = make_timestamp()

    write(LU_SHORT_RPT, &
      FMT="(12a,i5,a,f18.2,a,a,a,5(f18.2,a),ES16.4,a,3(f18.2,a),i10)")  &
         trim(sTimeStamp), sTab,                                              &
        trim(pConfig%sFlowFileName),sTab,trim(pConfig%sConcFileName),sTab,    &
        trim(pConc(1)%sConstituentName), sTab,                                &
        trim(pConfig%sStartDate), sTab, trim(pConfig%sEndDate), sTab,         &
        pConfig%iMaxNumStrata,sTab,pConfig%rCombinedLoad                      &
          / LOAD_UNITS(pConfig%iLoadUnitsCode)%rConversionFactor,sTab,        &
        trim(LOAD_UNITS(pConfig%iLoadUnitsCode)%sUnits),sTab,                 &
        pConfig%rCombinedLoadCI                                               &
          / LOAD_UNITS(pConfig%iLoadUnitsCode)%rConversionFactor, sTab,       &
        pConfig%rCombinedLoadAnnualized                                       &
          / LOAD_UNITS(pConfig%iLoadUnitsCode)%rConversionFactor, sTab,       &
        pConfig%rCombinedLoadAnnualizedCI                                     &
          / LOAD_UNITS(pConfig%iLoadUnitsCode)%rConversionFactor, sTab,       &
        pConfig%rJackCombinedLoadAnnualized                                   &
          / LOAD_UNITS(pConfig%iLoadUnitsCode)%rConversionFactor, sTab,       &
        pConfig%rJackCombinedLoadAnnualizedCI                                 &
          / LOAD_UNITS(pConfig%iLoadUnitsCode)%rConversionFactor, sTab,       &
        pConfig%rTotalFlowAnnualized, sTab,                                   &
        pConfig%rCombinedMSE, sTab,pConfig%rCombinedRMSE,                     &
        sTab,pConfig%rCombinedEffectiveDegreesFreedom, sTab,                  &
        pConfig%iFuncCallNum

!  else

!    write(LU_SHORT_RPT, &
!      FMT="(10a,i4,a,f18.2,a,a,a,3(f18.2,a),ES16.4,a,3(f18.2,a),i10)") &
!        trim(pConfig%sFlowFileName),sTab,trim(pConfig%sConcFileName),sTab,&
!        trim(pConc(1)%sConstituentName), sTab, &
!        trim(pConfig%sStartDate), sTab, trim(pConfig%sEndDate), sTab, &
!        pConfig%iMaxNumStrata,sTab,pConfig%rCombinedLoad,sTab, &
!        trim(LOAD_UNITS(pConfig%iLoadUnitsCode)%sUnits),sTab, &
!        pConfig%rCombinedLoadCI, sTab, &
!        pConfig%rCombinedLoadAnnualized, sTab, &
!        pConfig%rCombinedLoadAnnualizedCI, sTab, &
!        pConfig%rTotalFlowAnnualized, sTab, &
!        pConfig%rCombinedMSE, sTab,pConfig%rCombinedRMSE,&
!        sTab,pConfig%rCombinedEffectiveDegreesFreedom, sTab, &
!        pConfig%iFuncCallNum
!
!  end if

  flush(LU_SHORT_RPT)

  return

end subroutine print_short_report

!----------------------------------------------------------------------

subroutine print_strata_summary(pConfig, iLU)

  !! [ ARGUMENTS ]
  type (T_CONFIG), pointer :: pConfig ! pointer to data structure that contains
                                        ! program options, flags, and other settings

  integer (kind=T_INT), intent(in) :: iLU
  integer (kind=T_INT) :: i
  integer (kind=T_INT) :: iSYear, iSMonth, iSDay
  integer (kind=T_INT) :: iEYear, iEMonth, iEDay


!  write(iLU,FMT=*) repeat("-",80)

  write(iLU,FMT=*) ' SUMMARY OVER ALL STRATA:'

  write(iLU,FMT="('    NUMBER OF STRATA:',i4)") pConfig%iMaxNumStrata

  do i=1,pConfig%iMaxNumStrata

    call gregorian_date(pConfig%iStrataBound(i - 1) + 1, iSYear, iSMonth, iSDay)
    call gregorian_date(pConfig%iStrataBound(i), iEYear, iEMonth, iEDay)

    write(iLU,&
     FMT="(t7,i3,t15,' -- begins on:',3x,i2.2,'/',i2.2,'/'," &
       //"i4,3x'ends on:',3x,i2.2,'/',i2.2,'/',i4)") i,&
       iSMonth, iSDay, iSYear,iEMonth, iEDay, iEYear

  end do

  write(iLU, FMT="(/)")

  write(iLU,&
    FMT="('    COMBINED LOAD:',t31,a25)") &
      trim(sf_L_units(pConfig,pConfig%rCombinedLoad))
  write(iLU,&
    FMT="('    CONFIDENCE INTERVAL:',t31,a25)") &
      trim(sf_L_units(pConfig,pConfig%rCombinedLoadCI))

  write(iLU,FMT=*) " "

  write(iLU,&
    FMT="('    COMBINED LOAD (PER YEAR):',t31,a25,a3)") &
      trim(sf_L_units(pConfig,pConfig%rCombinedLoadAnnualized)),'/yr'

  write(iLU,&
    FMT="('    COMBINED LOAD CI:',t31,a25,a3)") &
      trim(sf_L_units(pConfig,pConfig%rCombinedLoadAnnualizedCI)),'/yr'

  write(iLU,&
    FMT="('    TOTAL ANNNUALIZED FLOW:',t31,ES16.4,a)") &
      pConfig%rTotalFlowAnnualized,' cubic meters'

  write(iLU,FMT=*) " "

  write(iLU,&
    FMT="('    COMBINED MSE:',t31,a28)") &
      trim(sf_L2_units(pConfig,pConfig%rCombinedMSE))
  write(iLU,&
    FMT="('    COMBINED RMSE:',t31,a25)") &
      trim(sf_L_units(pConfig,pConfig%rCombinedRMSE))

  write(iLU,&
    FMT="('    COMBINED DEGREES FREEDOM:',t31,13x,f12.2)") &
      pConfig%rCombinedEffectiveDegreesFreedom
  write(iLU,&
    FMT="('    NUMBER of strata boundary sets evaluated:',i10)") &
      pConfig%iFuncCallNum

  write(iLU,FMT=*) repeat("~",80)
  write(iLU,FMT="(' ===> END OF SUMMARY for calculation with ',i3,' strata')") &
    pConfig%iMaxNumStrata
  write(iLU,FMT=*) repeat("~",80)

  write(iLU,FMT=*) " "

  flush(iLU)

  return

end subroutine print_strata_summary

!----------------------------------------------------------------------

function rf_Q_disp(pConfig,rValue)   result(rConvertedValue)

  type (T_CONFIG), pointer :: pConfig ! pointer to data structure that contains
                                        ! program options, flags, and other settings

  real (kind=T_REAL) :: rValue, rConvertedValue

  rConvertedValue = rValue / &
       FLOW_UNITS(pConfig%iFlowUnitsCode)%rConversionFactor

  return

end function rf_Q_disp

!----------------------------------------------------------------------

function sf_Q_units(pConfig,rValue)  result(sQ_w_units)

  ! returns a formated text string containing the flow value converted
  ! to display units, along with the description of those units

  type (T_CONFIG), pointer :: pConfig ! pointer to data structure that contains
                                        ! program options, flags, and other settings

  real (kind=T_REAL) :: rValue, rConvertedValue
  character(len=256) :: sQ_w_units, sUnits

  rConvertedValue = rValue / &
     FLOW_UNITS(pConfig%iFlowUnitsCode)%rConversionFactor

  sUnits = FLOW_UNITS(pConfig%iFlowUnitsCode)%sUnits

  if(rConvertedValue > 1.E+10 .or. rConvertedValue < 1.0E-1) then

    write(sQ_w_units,FMT="(g14.3,1x,a)") rConvertedValue, trim(sUnits)

  else

    write(sQ_w_units,FMT="(f14.1,1x,a)") rConvertedValue, trim(sUnits)

  end if


  return

end function sf_Q_units

!----------------------------------------------------------------------

function sf_C_units(pConfig,rValue)  result(sC_w_units)

  ! returns a formated text string containing the concentration value converted
  ! to display units, along with the description of those units

  type (T_CONFIG), pointer :: pConfig ! pointer to data structure that contains
                                        ! program options, flags, and other settings

  real (kind=T_REAL) :: rValue, rConvertedValue
  character(len=256) :: sC_w_units, sUnits

  rConvertedValue = rValue / &
     CONC_UNITS(pConfig%iConcUnitsCode)%rConversionFactor

  sUnits = CONC_UNITS(pConfig%iConcUnitsCode)%sUnits

  if(rConvertedValue > 1.E+8 .or. rConvertedValue < 1.0E-3) then

    write(sC_w_units,FMT="(g16.3,1x,a)") rConvertedValue, trim(sUnits)

  else

    write(sC_w_units,FMT="(f16.3,1x,a)") rConvertedValue, trim(sUnits)

  end if

end function sf_C_units

!----------------------------------------------------------------------

function sf_L_units(pConfig,rValue)  result(sL_w_units)

  ! returns a formated text string containing the load value converted
  ! to display units, along with the description of those units

  type (T_CONFIG), pointer :: pConfig ! pointer to data structure that contains
                                        ! program options, flags, and other settings

  real (kind=T_REAL) :: rValue, rConvertedValue
  character(len=256) :: sL_w_units, sUnits

  rConvertedValue = rValue / &
     LOAD_UNITS(pConfig%iLoadUnitsCode)%rConversionFactor

  sUnits = LOAD_UNITS(pConfig%iLoadUnitsCode)%sUnits

  if(rConvertedValue > 1.E+3 .and. rConvertedValue <= 1.0E+11) then

    write(sL_w_units,FMT="(f16.0)") rConvertedValue

  elseif(rConvertedValue > 10. .and. rConvertedValue <= 1.0E+3) then

    write(sL_w_units,FMT="(f16.1)") rConvertedValue

  else  ! either really small or really big - allow scientific notation

    write(sL_w_units,FMT="(g16.5)") rConvertedValue

  end if

  sL_w_units = trim(sL_w_units)//" "//trim(sUnits)

  return

end function sf_L_units

!----------------------------------------------------------------------

function sf_L2_units(pConfig,rValue)  result(sL2_w_units)

  ! returns a formated text string containing the load value converted
  ! to display units, along with the description of those units

  type (T_CONFIG), pointer :: pConfig ! pointer to data structure that contains
                                        ! program options, flags, and other settings

  real (kind=T_REAL) :: rValue, rConvertedValue
  character(len=256) :: sL2_w_units, sUnits

  rConvertedValue = rValue / &
     (LOAD_UNITS(pConfig%iLoadUnitsCode)%rConversionFactor**2)

  sUnits = trim(LOAD_UNITS(pConfig%iLoadUnitsCode)%sUnits)//"**2"

  if(rConvertedValue > 1.E+3 .and. rConvertedValue <= 1.0E+11) then

    write(sL2_w_units,FMT="(f16.0,1x,a)") rConvertedValue, trim(sUnits)

  elseif(rConvertedValue > 10. .and. rConvertedValue <= 1.0E+3) then

    write(sL2_w_units,FMT="(f16.1,1x,a)") rConvertedValue, trim(sUnits)

  else  ! either really small or really big - allow scientific notation

    write(sL2_w_units,FMT="(g16.5,1x,a)") rConvertedValue, trim(sUnits)

  end if

  return

end function sf_L2_units

!----------------------------------------------------------------------

end module beale
