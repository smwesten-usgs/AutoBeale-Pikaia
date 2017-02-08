module input

  use types
  use units, only : rf_Q_disp
  implicit none

contains

subroutine read_data(pConfig, pFlow, pConc)

  ! code has been modified to read tab-delimited file as obtained
  ! from USGS NWIS web interface

  !! [ ARGUMENTS ]
  type (CONFIG_T), pointer :: pConfig ! pointer to data structure that contains
                                        ! program options, flags, and other settings

  type (FLOW_T), dimension(:), pointer :: pFlow
  type (CONC_T), dimension(:), pointer :: pConc

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
    if(  sRecord(1:9) == "agency_cd" ) cycle
    !@TODO: make this user configurable!!
    if ( sRecord(1:4) /= "USGS" ) cycle
    !   ! read another line and throw it away
    !   read ( unit=LU_FLOWDAT, fmt="(a256)", iostat=iStat ) sRecord
    !   call Assert( LOGICAL( iStat == 0,kind=T_LOGICAL), &
    !        "Terminating due to error reading FLOW file" )
	  ! cycle      ! Ignore header information
    ! end if

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

      iStat = verify( sItem, "0123456789.")

      if ( iStat > 0 ) then

        pConc(i)%rConc = rValue * CONC_UNITS(pConfig%iConcUnitsCode)%rConversionFactor

      else

        pConc(i)%rConc = -99999.

      endif

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

      ! test to see whether argument is numeric
      iStat = verify( sItem, "0123456789.")

      if ( iStat > 0. ) then

        pConc(i)%rConc = rValue

      else

        pConc(i)%rConc = -99999.

      endif

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
      if ( pConc(i)%rConc > 0. )  &
        pConc(i)%rConc = rValue * CONC_UNITS(pConfig%iConcUnitsCode)%rConversionFactor

   else

     call Assert(lFALSE,"Unknown or undefined input concentration file format")

   endif

   call Assert(LOGICAL(pConc(i)%iMonth > 0 .and. pConc(i)%iMonth <= 12, &
     kind=T_LOGICAL),"Month value out of range reading concentration file")

   call Assert(LOGICAL(pConc(i)%iDay > 0 .and. pConc(i)%iDay <= 31, &
     kind=T_LOGICAL),"Day value out of range reading concentration file")

!    if(pConc(i)%rConc < 0.0) exit

  end do conc_loop

end subroutine read_data

!--------------------------------------------------------------------------------------------------

subroutine clean_flow_data(pFlow)

  type (FLOW_T), dimension(:), pointer :: pFlow

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

end subroutine clean_flow_data

!--------------------------------------------------------------------------------------------------

subroutine clean_concentration_data(pFlow,pConc)

  type (FLOW_T), dimension(:), pointer :: pFlow
  type (CONC_T), dimension(:), pointer :: pConc

  type (CONC_T), dimension(:), pointer :: pCopy

  integer (kind=T_INT) :: i, iCount, iStat, iNumValues, j, k

  logical    :: found_match

  iCount = 0

  ! here we are counting the number of days within the range of interest
  ! for which we have one or more concentration data available
  do i=MINVAL(pFlow%iJulianDay),MAXVAL(pFlow%iJulianDay)
    if(COUNT(pConc%iJulianDay==i)>0) then
      iCount=iCount+1
    end if
  end do

  ALLOCATE( pCopy( count( pConc%rConc >= 0. ) ),STAT=iStat)
  call Assert(iStat==0, &
    "Problem allocating memory in function clean_conc_data")

  iCount = 0
  ! now iterate over the date range again, copying and averaging where
  ! necessary
  do i=MINVAL(pFlow%iJulianDay),MAXVAL(pFlow%iJulianDay)
    iNumValues = COUNT( pConc%iJulianDay==i  )
    if( iNumValues > 0. ) then
      iCount = iCount + 1
      do j=1,SIZE(pConc)
        if ( pConc(j)%rConc < 0. ) cycle
        if(pConc(j)%iJulianDay == i) then
          pCopy(iCount) = pConc(j)
          if(iNumValues > 1) then
            pCopy(iCount)%iHour = 99
            pCopy(iCount)%iMinute = 99
            pCopy(iCount)%rConc = SUM(pConc%rConc,pConc%iJulianDay==i) / iNumValues
            exit
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

end subroutine clean_concentration_data

end module input
