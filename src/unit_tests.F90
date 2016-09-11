program unit_tests

  use types
  use beale
  use beale_data
  use output
  use units
  use strata
  use iso_fortran_env, only : OUTPUT_UNIT
  implicit none

  type (STRATA_T), pointer                       :: pTestStrata
  type (CONFIG_T), pointer                       :: pTestConfig
  type (CONC_T), dimension(:), pointer           :: pTestConc
  type (FLOW_T), dimension(:), pointer           :: pTestFlow
  type (COMBINED_STATS_T), pointer               :: pTestStats
  type (STRATUM_STATS_T), pointer                :: pStratum

  integer :: test_flow_dates(365)
  integer :: test_conc_months(52)
  integer :: test_conc_days(52)
  integer :: test_conc_dates(52)
  integer :: iStat

  character (len=4) :: buf_str


  ! discharge data are from the ROCKFLOW.DAT file that was distributed with the latest available
  ! version of AutoBeale as a test case
  real :: test_flow_data(365) = [                                                             &
                    11.0,10.0,11.0,12.0,52.0,48.0,13.0,7.0,6.0,5.4,5.0,4.5,4.3,4.0,4.5,5.0,   &
                    5.6,5.0,4.2,3.8,4.1,100.0,140.0,30.0,35.0,56.0,13.0,120.0,80.0,20.0,9.0,  &
                    7.6,7.0,17.0,250.0,518.0,65.0,26.0,16.0,12.0,10.0,9.4,8.8,9.0,7.5,7.2,    &
                    6.8,6.3,24.0,119.0,71.0,125.0,123.0,41.0,20.0,14.0,14.0,1440.0,370.0,     &
                    75.0,372.0,97.0,42.0,32.0,85.0,68.0,44.0,35.0,94.0,43.0,22.0,16.0,261.0,  &
                    121.0,32.0,21.0,18.0,15.0,14.0,13.0,12.0,11.0,10.0,11.0,65.0,35.0,18.0,   &
                    240.0,100.0,88.0,38.0,21.0,16.0,16.0,24.0,35.0,17.0,13.0,11.0,10.0,9.9,   &
                    123.0,154.0,44.0,24.0,26.0,200.0,105.0,38.0,21.0,16.0,14.0,12.0,11.0,     &
                    10.0,9.5,9.4,10.0,9.9,9.5,9.6,9.6,21.0,58.0,26.0,40.0,26.0,16.0,18.0,     &
                    15.0,13.0,11.0,10.0,9.4,9.3,8.9,8.4,8.9,120.0,83.0,22.0,13.0,10.0,9.2,    &
                    914.0,563.0,72.0,30.0,22.0,31.0,508.0,1410.0,666.0,107.0,60.0,34.0,24.0,  &
                    19.0,15.0,14.0,12.0,12.0,12.0,13.0,11.0,10.0,9.3,8.4,9.3,8.7,8.3,9.5,     &
                    13.0,11.0,7.6,10.0,32.0,19.0,9.5,24.0,124.0,34.0,13.0,8.6,7.0,6.0,5.4,    &
                    5.2,6.2,336.0,87.0,20.0,9.9,7.4,6.2,5.9,5.1,4.7,4.4,4.1,3.8,3.6,4.2,4.3,  &
                    3.9,3.8,3.6,3.4,3.3,3.1,2.9,2.9,2.8,3.6,2.8,8.8,7.5,3.9,3.2,2.8,2.6,2.6,  &
                    2.4,2.7,3.7,4.3,3.4,7.6,95.0,67.0,22.0,8.8,5.7,4.4,3.4,4.3,6.1,5.7,4.4,   &
                    3.5,2.9,2.8,2.9,2.8,2.6,2.6,2.4,2.4,2.4,2.3,2.2,2.3,3.6,2.5,3.0,3.0,2.4,  &
                    2.1,2.0,2.0,1.9,2.2,16.0,43.0,9.7,4.9,3.3,2.6,2.2,2.0,2.1,1.7,1.5,1.6,    &
                    1.7,1.7,1.7,1.5,1.6,1.5,2.6,3.2,3.0,3.3,3.9,3.9,4.7,3.8,3.9,3.4,3.1,3.4,  &
                    3.6,3.9,3.4,3.5,3.7,4.4,5.2,5.1,4.1,3.7,3.5,3.4,3.4,3.3,3.6,3.5,3.3,3.3,  &
                    3.4,3.3,3.4,3.5,3.2,3.1,3.2,4.6,4.5,4.1,3.8,3.6,3.8,4.0,4.6,6.4,7.5,6.0,  &
                    4.9,4.8,4.5,9.0,50.0,39.0,18.0,12.0,9.0,26.0,25.0,14.0,9.3,7.7,7.5,90.0,  &
                    161.0,48.0,22.0,14.0,10.0,8.9,8.4,9.2,8.8,8.1,7.7,8.1,50.0,39.0,220.0,    &
                    113.0,45.0,22.0,15.0,12.0,10.0 ]

  ! concentration data are from the file ROCKNO23.DAT which was distributed as part of a test
  ! case with the latest available version of AutoBeale
  integer :: test_conc_mmdd(52) = [ 0106,0113,0120,0127,0203,0210,0217,0224,0303,0310,0317,0324,  &
                                   0331,0407,0414,0421,0428,0505,0512,0519,0526,0602,0609,0616,  &
                                   0623,0630,0707,0714,0721,0728,0804,0811,0818,0825,0901,0908,  &
                                   0915,0922,0929,1006,1013,1020,1027,1103,1110,1117,1124,1201,  &
                                   1208,1215,1222,1229 ]

  real :: test_conc_data(52) = [                                                               &
                          4.7,3.6,2.8,2.8,1.9,3.0,2.5,6.0,3.4,4.4,3.9,3.1,4.4,2.6,5.5,3.8,     &
                          1.7,3.8,2.3,2.3,7.6,4.3,3.9,2.5,2.2,5.7,3.2,3.7,1.8,1.2,1.0,0.8,     &
                          1.8,1.3,0.7,0.6,0.7,2.1,0.8,0.2,0.0,0.0,0.2,0.0,0.1,0.1,0.5,6.8,     &
                          4.1,4.7,3.0,5.3 ]

  integer :: day
  integer :: n
  allocate( pTestConfig, stat=iStat )
  call assert( iStat==0, "Problem allocating memory.", __FILE__, __LINE__ )

  allocate( pTestStrata, stat=iStat )
  call assert( iStat==0, "Problem allocating memory.", __FILE__, __LINE__ )

  allocate( pTestStrata%pStratum(3), stat=iStat )
  call assert( iStat==0, "Problem allocating memory.", __FILE__, __LINE__ )

  allocate( pTestConc(52), stat=iStat )
  call assert( iStat==0, "Problem allocating memory.", __FILE__, __LINE__ )
  allocate( pTestFlow(365) , stat=iStat )
  call assert( iStat==0, "Problem allocating memory.", __FILE__, __LINE__ )
  allocate( pTestStats , stat=iStat )
  call assert( iStat==0, "Problem allocating memory.", __FILE__, __LINE__ )

  ! assemble CONFIGURATION dataset
  pTestStrata%iCurrentNumberOfStrata = 3
  pTestConfig%iMaxNumStrata = 3
  pTestConfig%iTotNumDays   = 365
  pTestConfig%iConcUnitsCode = iMILLIGRAMS_PER_LITER
  pTestConfig%iFlowUnitsCode = iCUBIC_FEET_PER_SEC

  ! assemble STRATA dataset
  pStratum => pTestStrata%pStratum(1)
  pStratum%iStartDate = julian_day( iYear=1997, iMonth=1, iDay=1 )
  pStratum%iEndDate   = julian_day( iYear=1997, iMonth=4, iDay=20 )
  pStratum%iNumDays   = pStratum%iEndDate - pStratum%iStartDate + 1

  pStratum => pTestStrata%pStratum(2)
  pStratum%iStartDate = julian_day( iYear=1997, iMonth=4, iDay=21 )
  pStratum%iEndDate   = julian_day( iYear=1997, iMonth=5, iDay=26 )
  pStratum%iNumDays   = pStratum%iEndDate - pStratum%iStartDate + 1

  pStratum => pTestStrata%pStratum(3)
  pStratum%iStartDate = julian_day( iYear=1997, iMonth=5, iDay=27 )
  pStratum%iEndDate   = julian_day( iYear=1997, iMonth=12, iDay=31 )
  pStratum%iNumDays   = pStratum%iEndDate - pStratum%iStartDate + 1

  ! assemble FLOW dataset; first figure out the Julian Date for the first day of the run
  test_flow_dates(1) = julian_day( iYear=1997, iMonth=1, iDay=1 )

  ! populate the remaining day values
  do day=2,365
    test_flow_dates( day ) = test_flow_dates(1) + day - 1
  end do

  pTestFlow%iJulianDay = test_flow_dates
  pTestFlow%rFlow      = test_flow_data * FLOW_UNITS(pTestConfig%iFlowUnitsCode)%rConversionFactor

  ! assemble CONCENTRATION dataset
  do n=1,52
    write( buf_str, fmt="(i4)") test_conc_mmdd(n)
    read( buf_str, fmt="(i2,i2)") test_conc_months(n), test_conc_days(n)
    test_conc_dates(n) = julian_day( iYear=1997, iMonth=test_conc_months(n), iDay=test_conc_days(n) )
  end do

  pTestConc%iJulianDay = test_conc_dates
  pTestConc%rConc      = test_conc_data
  pTestConc%iMonth     = test_conc_months
  pTestConc%iDay       = test_conc_days
  pTestConc%iYear      = 1997

  call calculate_daily_loads(pFlow=pTestFlow, pConc=pTestConc, pConfig=pTestConfig, pStats=pTestStats )
  call bealecalc_orig(pConfig=pTestConfig, pFlow=pTestFlow, pConc=pTestConc, pStrata=pTestStrata)

  deallocate( pTestStrata%pStratum )
  allocate( pTestStrata%pStratum(5))

  ! assemble new STRATA dataset
  pStratum => pTestStrata%pStratum(1)
  pStratum%iStartDate = julian_day( iYear=1997, iMonth=1, iDay=1 )
  pStratum%iEndDate   = julian_day( iYear=1997, iMonth=4, iDay=20 )
  pStratum%iNumDays   = pStratum%iEndDate - pStratum%iStartDate + 1

  pStratum => pTestStrata%pStratum(2)
  pStratum%iStartDate = julian_day( iYear=1997, iMonth=4, iDay=21 )
  pStratum%iEndDate   = julian_day( iYear=1997, iMonth=5, iDay=26 )
  pStratum%iNumDays   = pStratum%iEndDate - pStratum%iStartDate + 1

  pStratum => pTestStrata%pStratum(3)
  pStratum%iStartDate = julian_day( iYear=1997, iMonth=5, iDay=27 )
  pStratum%iEndDate   = julian_day( iYear=1997, iMonth=8, iDay=17 )
  pStratum%iNumDays   = pStratum%iEndDate - pStratum%iStartDate + 1

  pStratum => pTestStrata%pStratum(4)
  pStratum%iStartDate = julian_day( iYear=1997, iMonth=8, iDay=18 )
  pStratum%iEndDate   = julian_day( iYear=1997, iMonth=12, iDay=1 )
  pStratum%iNumDays   = pStratum%iEndDate - pStratum%iStartDate + 1

  pStratum => pTestStrata%pStratum(5)
  pStratum%iStartDate = julian_day( iYear=1997, iMonth=12, iDay=2 )
  pStratum%iEndDate   = julian_day( iYear=1997, iMonth=12, iDay=31 )
  pStratum%iNumDays   = pStratum%iEndDate - pStratum%iStartDate + 1

  pTestStrata%iCurrentNumberOfStrata = 5
  pTestConfig%iMaxNumStrata = 5

  call calculate_daily_loads(pFlow=pTestFlow, pConc=pTestConc, pConfig=pTestConfig, pStats=pTestStats )
  call bealecalc_orig(pConfig=pTestConfig, pFlow=pTestFlow, pConc=pTestConc, pStrata=pTestStrata)

  call calculate_and_combine_stratum_loads( pTestConfig, pTestStrata, pTestStats, pTestFlow, pTestConc )
  call print_strata_summary(pTestConfig, pTestStrata, pTestStats, OUTPUT_UNIT )

end program unit_tests
