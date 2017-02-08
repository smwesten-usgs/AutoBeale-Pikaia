module types

  !	Common declarations for the BEALE program.

  use iso_fortran_env, only : OUTPUT_UNIT
  implicit none

  ! Define the sizes of base types used in the model
  integer*2, public, parameter :: T_LOGICAL = 4
  integer*2, public, parameter :: T_INT = 4
!  integer*2, public, parameter :: T_REAL = SELECTED_REAL_KIND(p=13,r=200)
  integer*2, public, parameter :: T_REAL = 8
  integer*2, public, parameter :: T_SGL = 4

  ! define some useful constants
  logical (kind=T_LOGICAL), public, parameter :: lTRUE = .true._T_LOGICAL
  logical (kind=T_LOGICAL), public, parameter :: lFALSE = .false._T_LOGICAL

  real (kind=T_REAL), public, parameter :: rZERO = 0_T_REAL
  real (kind=T_REAL), public, parameter :: rNEAR_ZERO = 1E-7_T_REAL

  ! define maximum number of strata to allow
  integer (kind=T_INT), public, parameter :: iMAX_STRATA = 64

  ! define Fortran logical unit numbers
  integer (kind=T_INT), public, parameter :: LU_STD_OUT = 6
  integer (kind=T_INT), public, parameter :: LU_FLOWDAT = 100
  integer (kind=T_INT), public, parameter :: LU_CONCDAT = 101
  integer (kind=T_INT), public, parameter :: LU_LONG_RPT = 2
  integer (kind=T_INT), public, parameter :: LU_SHORT_RPT = 3
  integer (kind=T_INT), public, parameter :: LU_STATS_OUT = 20
  integer (kind=T_INT), public, parameter :: LU_ECHO_OUT = 21
  integer (kind=T_INT), public, parameter :: LU_PIKAIA_OUT = 22
  integer (kind=T_INT), public, parameter :: LU_FILELIST = 23
  integer (kind=T_INT), public, parameter :: LU_JACKKNIFE_OUT = 24
  integer (kind=T_INT), public, parameter :: LU_R_SCRIPT = 25
  integer (kind=T_INT), public, parameter :: LU_LOADS_OUT = 26

  integer (kind=T_INT), parameter :: iGRAMS_PER_LITER = 1
  integer (kind=T_INT), parameter :: iMILLIGRAMS_PER_LITER = 2
  integer (kind=T_INT), parameter :: iMICROGRAMS_PER_LITER = 3
  integer (kind=T_INT), parameter :: iNANOGRAMS_PER_LITER = 4

  integer (kind=T_INT), parameter :: iCUBIC_FEET_PER_SEC = 1
  integer (kind=T_INT), parameter :: iCUBIC_METERS_PER_SEC = 2

  integer (kind=T_INT), parameter :: iMETRIC_TONS_PER_YEAR = 1
  integer (kind=T_INT), parameter :: iKILOGRAMS_PER_YEAR = 2

  integer (kind=T_INT), parameter :: iFLOW_FILE_FORMAT_ORIGINAL = 0
  integer (kind=T_INT), parameter :: iFLOW_FILE_FORMAT_USGS = 1

  integer (kind=T_INT), parameter :: iCONC_FILE_FORMAT_ORIGINAL = 0
  integer (kind=T_INT), parameter :: iCONC_FILE_FORMAT_USGS = 1

  integer (kind=T_INT), parameter :: MINIMIZE_MEAN_SQUARED_ERROR = 0
  integer (kind=T_INT), parameter :: MINIMIZE_CONFIDENCE_INTERVAL = 1

  type, public :: UNIT_CONVERSION_T
    real (kind=T_REAL) :: rConversionFactor
    character (len=256) :: sUnits
    character (len=256) :: sUnitsSpelledOut
  end type UNIT_CONVERSION_T

  type, public :: FLOW_T
    character (len=256) :: sAgencyCode
    character (len=256) :: sStationID
    integer (kind=T_INT) :: iYear = 0
    integer (kind=T_INT) :: iMonth = 0
    integer (kind=T_INT) :: iDay = 0
    integer (kind=T_INT) :: iJulianDay = 0
    character (len=256) :: sDate = "NA"
    real (kind=T_REAL) :: rFlow = - 9999.0
    logical (kind=T_LOGICAL) :: lSampleDay = lFALSE
  end type FLOW_T

  type, public :: CONC_T
    character (len=256) :: sTribName
    character (len=256) :: sConstituentName
    integer (kind=T_INT) :: iYear = 0
    integer (kind=T_INT) :: iMonth = 0
    integer (kind=T_INT) :: iDay = 0
    integer (kind=T_INT) :: iJulianDay = 0
    integer (kind=T_INT) :: iHour = 0
    integer (kind=T_INT) :: iMinute = 0
    character (len=256) :: sDate = "NA"
    real (kind=T_REAL) :: rConc = -9999.0
    character (len=32) :: sConcUnits = "NONE"
    real (kind=T_REAL) :: rDailyLoad = -9999.0
    real (kind=T_REAL) :: rLoadTimesFlow = -9999.0
    real (kind=T_REAL) :: rFlow = -9999.0
    real (kind=T_REAL) :: rFlowSquared = -9999.0
    real (kind=T_REAL) :: rTemp = 0.
    logical (kind=T_LOGICAL) :: lInclude = lTRUE
  end type CONC_T

  type, public :: CONFIG_T
    logical (kind=T_LOGICAL) :: lJackknife = lFALSE
    integer (kind=T_INT)     :: iMinimizationStatistic = MINIMIZE_MEAN_SQUARED_ERROR
    integer (kind=T_INT)     :: iStartDate
    integer (kind=T_INT)     :: iEndDate
    character (len=20)       :: sStartDate
    character (len=20)       :: sEndDate

    character (len=256) :: sBaseDirName = ""
    character (len=256) :: sFlowDirName = ""
    character (len=256) :: sConcDirName = ""
    character (len=256) :: sResultsDirName = ""
    character (len=256) :: sFlowFileName = ""
    character (len=256) :: sOptionalLabel = ""
    integer (kind=T_INT) :: iFlowFileFormat = iFLOW_FILE_FORMAT_USGS
    character (len=256) :: sConcFileName = ""
    integer (kind=T_INT) :: iConcFileFormat = iCONC_FILE_FORMAT_USGS
    character (len=256) :: sShortOutputFileName = ""
    character (len=256) :: sExtendedOutputFileName = ""
    integer (kind=T_INT) :: iFlowUnitsCode = iCUBIC_FEET_PER_SEC
    integer (kind=T_INT) :: iConcUnitsCode = iMILLIGRAMS_PER_LITER
    integer (kind=T_INT) :: iLoadUnitsCode = iKILOGRAMS_PER_YEAR
    character (len=256) :: sFlowUnits = "NA"
    character (len=256) :: sConcUnits = "NA"
    character (len=256) :: sLoadUnits = "NA"
    integer (kind=T_INT) :: iNumFlowPoints = 0
    integer (kind=T_INT) :: iNumConcPoints = 0
    integer (kind=T_INT) :: iMaxNumStrata = 1
    integer (kind=T_INT) :: iMaxEvalStrata = 11
    integer (kind=T_INT) :: iTotNumDays = 0
    integer (kind=T_INT) :: iMinSamplesPerStratum = 3
    integer (kind=T_INT) :: iCountUniqueSamples  = 0
    integer (kind=T_INT) :: iFuncCallNum         = 0
    real (kind=T_REAL),dimension(iMAX_STRATA) :: rPikaiaXValues
  end type CONFIG_T

  type, public :: JACKKNIFE_T
    real (kind=T_REAL) :: rEstimate
    real (kind=T_REAL) :: rEstimate_SQ
  end type JACKKNIFE_T

  type, public :: STRATUM_STATS_T
    integer (kind=T_INT) :: iStartDate
    integer (kind=T_INT) :: iEndDate

    character (len=20) :: sStartDate
    character (len=20) :: sEndDate

    integer (kind=T_INT) :: iNumDays
    integer (kind=T_INT) :: iNumSamples
    real (kind=T_REAL) :: rMeanFlow

    real (kind=T_REAL) :: rMeanSampleFlow
    real (kind=T_REAL) :: rMeanSampleConc
    real (kind=T_REAL) :: rMeanSampleLoad

    real (kind=T_REAL) :: rDailyBiasedLoadEstimate
    real (kind=T_REAL) :: rDailyLoadBiasCorrection
    real (kind=T_REAL) :: rDailyCorrectedLoadEstimate
    real (kind=T_REAL) :: rDailyMeanSquareError
    real (kind=T_REAL) :: rDailySumOfSquareError
    real (kind=T_REAL) :: rDailyLoadCI

    real (kind=T_REAL) :: rStratumCorrectedLoad
    real (kind=T_REAL) :: rStratumMeanSquareError
    real (kind=T_REAL) :: rStratumLoadCI

    real (kind=T_REAL) :: rS_qq
    real (kind=T_REAL) :: rS_lq
    real (kind=T_REAL) :: rS_ll
    real (kind=T_REAL) :: rS_q2l
    real (kind=T_REAL) :: rS_ql2
    real (kind=T_REAL) :: rS_q3
  end type STRATUM_STATS_T

  type, public :: STRATA_T
    integer (kind=T_INT)                            :: iCurrentNumberOfStrata
    type ( STRATUM_STATS_T ), dimension(:), pointer :: pStratum
  end type STRATA_T

  type, public :: COMBINED_STATS_T
    real (kind=T_REAL) :: rTotalFlow                 = 0.
    real (kind=T_REAL) :: rCombinedLoad              = 0.
    real (kind=T_REAL) :: rCombinedLoadAnnualized    = 0.
    real (kind=T_REAL) :: rTotalFlowAnnualized       = 0.
    real (kind=T_REAL) :: rCombinedMSE               = HUGE( 0. )
    real (kind=T_REAL) :: rCombinedRMSE              = HUGE( 0. )
    real (kind=T_REAL) :: rCombinedLoadCI            = HUGE( 0. )
    real (kind=T_REAL) :: rCombinedLoadAnnualizedCI  = HUGE( 0. )

    real (kind=T_REAL) :: rCombinedDailyLoad         = HUGE( 0. )
    real (kind=T_REAL) :: rCombinedDailyMSE          = HUGE( 0. )
    real (kind=T_REAL) :: rCombinedDailyRMSE         = HUGE( 0. )

    real (kind=T_REAL) :: rJackCombinedLoadAnnualized   = HUGE( 0. )
    real (kind=T_REAL) :: rJackCombinedLoadAnnualizedCI = HUGE( 0. )

    real (kind=T_REAL) :: rCombinedEffectiveDegreesFreedom  = HUGE( 0. )
  end type COMBINED_STATS_T


  type,public :: MONTH_T
    !! Container for calendar lookup information
    character (len=3) :: sName          ! Abbreviated name
	  character (len=9) :: sFullName      ! Full month name
    integer (kind=T_INT) :: iStart      ! Starting (Julian) date
    integer (kind=T_INT) :: iEnd        ! Ending (Julian) date
	  integer (kind=T_INT) :: iMonth      ! Month number (1-12)
  end type MONTH_T

  !! Month information
  type ( MONTH_T ),dimension(12),target :: YEAR_INFO = (/ &
      MONTH_T( 'JAN','JANUARY  ',   1,  31, 1 ), &
      MONTH_T( 'FEB','FEBRUARY ',  32,  59, 2 ), &
      MONTH_T( 'MAR','MARCH    ',  60,  90, 3 ), &
      MONTH_T( 'APR','APRIL    ',  91, 120, 4 ), &
      MONTH_T( 'MAY','MAY      ', 121, 151, 5 ), &
      MONTH_T( 'JUN','JUNE     ', 152, 181, 6 ), &
      MONTH_T( 'JUL','JULY     ', 182, 212, 7 ), &
      MONTH_T( 'AUG','AUGUST   ', 213, 243, 8 ), &
      MONTH_T( 'SEP','SEPTEMBER', 244, 273, 9 ), &
      MONTH_T( 'OCT','OCTOBER  ', 274, 304, 10 ), &
      MONTH_T( 'NOV','NOVEMBER ', 305, 334, 11 ), &
      MONTH_T( 'DEC','DECEMBER ', 335, 365, 12 ) /)

  ! concentration conversion factors represent the values that one
  ! needs to multiply by in order to yield units of mg/L
  type(UNIT_CONVERSION_T), dimension(4),target :: CONC_UNITS = (/ &
      UNIT_CONVERSION_T(1.0E+3_T_REAL,'g/L','GRAMS PER LITER'), &
      UNIT_CONVERSION_T(1.0_T_REAL,'mg/L','MILLIGRAMS PER LITER'), &
      UNIT_CONVERSION_T(1.0E-3_T_REAL,'ug/L','MICROGRAMS PER LITER'), &
      UNIT_CONVERSION_T(1.0E-6_T_REAL,'ng/L','NANOGRAMS PER LITER') /)

  ! flow conversion factors respresent the values that one needs to
  ! multiply by in order to yield cubic meters per second
  type(UNIT_CONVERSION_T), dimension(2),target :: FLOW_UNITS = (/ &
      UNIT_CONVERSION_T(0.028316849_T_REAL, 'cfs','CUBIC FEET PER SECOND'), &
      UNIT_CONVERSION_T(1.0_T_REAL,'cms','CUBIC METERS PER SECOND') /)

  ! load conversion factors represent the values that one needs to
  ! multiply load (in grams) by in order to yield kilograms
  type(UNIT_CONVERSION_T), dimension(2),target :: LOAD_UNITS = (/ &
      UNIT_CONVERSION_T(1.0E+3_T_REAL, 'MT','METRIC TONS'), &
      UNIT_CONVERSION_T(1.0_T_REAL,'Kg','KILOGRAMS') /)

  ! lingua Franca for load calculations is Kg/day
  ! mg/L * cms * 86.4 yields load in kg/day
  ! mg/L * cms * 86400 yields load in g/day

  real (kind=T_REAL), parameter :: rBASE_CONVERSION_FACTOR = 86.4_T_REAL

  contains

function make_timestamp()   result( sTimestamp )

  character(len=32)    :: sTimestamp

  ! [ LOCALS ]
  character (len=16) :: sDateText
  character (len=16) :: sTimeText

  call DATE_AND_TIME(sDateText, sTimeText)

  sTimestamp = trim(sDateText)//" "//trim(sTimeText)

end function

!--------------------------------------------------------------------------
!!****s* types/Chomp_tab
! NAME
!   Chomp_tab - Chomps all text up to the first tab character from a text string.
!
! SYNOPSIS
!   Chomps all text up to the first tab character from the text string in
!   sRecord and returns it in sItem; leaves the remaining text in sRecord.
!
! INPUTS
!   sRecord - Character string to operate on.
!
! OUTPUTS
!   sRecord - Character string to operate on.
!   sItem - Character string to operate on.
!
! EXAMPLE
!   input:   sRecord = "THIS IS<tab> THE TIME"    sItem = ""
!   output:  sRecord = " THE TIME"                sItem = "THIS IS"
!
! SOURCE

  subroutine Chomp_tab(sRecord, sItem)

  ! ARGUMENTS
  character (len=*), intent(inout) :: sRecord
  character (len=256), intent(out) :: sItem
  ! LOCALS
  integer (kind=T_INT) :: iR                      ! Index in sRecord
  character (len=1), parameter :: cTab = ACHAR(9) ! ASCII tab character

  iR = SCAN(sRecord,cTab)

  if(iR==0) then
    sItem = trim(sRecord)      ! no tab found; return entirety of sRecord
	sRecord = ""			   ! as sItem
  else
    sItem = trim(sRecord(1:iR-1))
	sRecord = trim(sRecord(iR+1:))
    do iR=1,len_trim(sRecord)
	  if (sRecord(iR:iR) == " " ) then
        cycle
	  else
	    exit
	  end if
	end do
    sRecord = sRecord(iR:)
  end if

  return

  end subroutine Chomp_tab

!!***

!--------------------------------------------------------------------------

  function compare_reals(rValue1, rValue2)

	logical (kind=T_LOGICAL) ::  compare_reals
	real (kind=T_REAL) :: rTOLERANCE
    real (kind=T_REAL) :: rValue1, rValue2
	real (kind=T_REAL) :: rUpperBound, rLowerBound

	rTOLERANCE = 1.0E-7
	rUpperBound = rValue2 + rTOLERANCE
	rLowerBound = rValue2 - rTOLERANCE

	if(rValue1 < rUpperBound .AND.rValue1 > rLowerBound) then
	  compare_reals = .TRUE.
	else
	  compare_reals = .FALSE.
	endif

	return

  end function compare_reals

!--------------------------------------------------------------------------
!!****s* types/Assert
! NAME
!   Assert - General-purpose error-checking routine.
!
! SYNOPSIS
!   General-purpose error-checking routine. If lCondition is .false.,
!   prints the error message and stops!
!
! INPUTS
!   lCondition - statement that evaluates to a logical .true. or .false value.
!   sErrorMessage - Accompanying error message to print if lCondition is .false.
!
! OUTPUTS
!   NONE
!
! SOURCE

subroutine assert(lCondition, sMessage, sModule, iLine, sCalledBy, iCalledByLine )

   logical (kind=T_LOGICAL), intent(in)                :: lCondition
   character (len=*), intent(in)                       :: sMessage
   character (len=*), intent(in), optional             :: sCalledBy
   integer (kind=T_INT), intent(in), optional          :: iCalledByLine
   character (len=*), intent(in), optional             :: sModule
   integer (kind=T_INT), intent(in), optional          :: iLine

   if (.not. lCondition) then

     if (present( sCalledBy ) .and. present( iCalledByLine ) &
       .and. present(sModule) .and. present(iLine) ) then
       call die( sMessage=sMessage, sCalledBy=sCalledBy, iCalledByLine=iCalledByLine, &
         sModule=sModule, iLine=iLine )
     elseif ( present(sModule) .and. present(iLine) ) then
       call die( sMessage=sMessage, sModule=sModule, iLine=iLine )
     elseif ( present(sModule) ) then
       call die( sMessage=sMessage, sModule=sModule )
     else
       call die( sMessage=sMessage )
     endif

   endif

 end subroutine assert


 subroutine die(sMessage, sModule, iLine, sHints, sCalledBy, iCalledByLine )

  character (len=*), intent(in)               :: sMessage
  character (len=*), intent(in), optional     :: sModule
  integer (kind=T_INT), intent(in), optional  :: iLine
  character (len=*), intent(in), optional     :: sHints
  character (len=*), intent(in), optional     :: sCalledBy
  integer (kind=T_INT), intent(in), optional  :: iCalledByLine

  ! [ LOCALS ]
  character (len=6) :: sLineNum

  write(*,fmt="(/,a,/)") "** ERROR -- PROGRAM EXECUTION HALTED **"
  write(*,fmt="(t12,a)") "error condition:  "//trim(sMessage)

  if ( present( sCalledBy ) )  &
    write(*,fmt="(t18,a)") "called by:  "//trim(sCalledBy)

  if (present(iCalledByLine)) then
    write(sLineNum, fmt="(i0)") iCalledByLine
    write(*,fmt="(t16,a)") "line number:  "//trim(sLineNum)
  endif

  if (present(sModule))  &
    write(*,fmt="(t21,a)")  "module:  "//trim(sModule)

  if (present(iLine)) then
    write(sLineNum, fmt="(i0)") iLine
    write(*,fmt="(t16,a)") "line number:  "//trim(sLineNum)
  endif

  if (present(sHints)) &
    write(*,fmt="(t12,a)") "==> "//trim(sHints)

  write(*,fmt="(/)")

  stop

end subroutine die

!!***

!--------------------------------------------------------------------------
!!****f* types/julian_day
! NAME
!   julian_day - Convert from a Gregorian calendar date to a Julian day number.
!
! SYNOPSIS
!   Conversion from a Gregorian calendar date to a Julian day number.
!   Valid for any Gregorian calendar date producing a Julian day
!   greater than zero.
!
! INPUTS
!   iYear   4-digit year
!   iMonth  2-digit month (1-12)
!   iDay    2-digit day (1-31)
!
! OUTPUTS
!   iJD     integer number of days that have elapsed since noon
!           Greenwich Mean Time (UT or TT) Monday, January 1, 4713 BC
!
! SOURCE

function julian_day ( iYear, iMonth, iDay ) result(iJD)

  ! [ ARGUMENTS ]
  integer (kind=T_INT), intent(in) :: iYear, iMonth, iDay
  ! [ LOCALS ]
  integer (kind=T_INT) i,j,k
  ! [ RETURN VALUE ]
  integer (kind=T_INT) :: iJD

  call Assert(LOGICAL(iYear>0,kind=T_LOGICAL), &
    "Julian Day calculation requested for an implausible year (<1 CE)")

  i= iYear
  j= iMonth
  k= iDay

  iJD= k-32075_T_INT + 1461_T_INT * (i + 4800_T_INT + (j - 14_T_INT) / 12_T_INT) &
        /4_T_INT + 367_T_INT * (j - 2_T_INT - (j - 14_T_INT)/ 12_T_INT * 12_T_INT) &
        /12_T_INT - 3_T_INT *((i + 4900_T_INT + (j - 14_T_INT) &
        /12_T_INT)/100_T_INT)/4_T_INT

  return

end function julian_day

!!***

!--------------------------------------------------------------------------
!!****f* types/num_days_in_year
! NAME
!   num_days_in_year - Return the number of days in the given year.
!
! SYNOPSIS
!   This function simply returns the number of days given the current year.
!
! INPUTS
!   iYear   4-digit year
!
! OUTPUTS
!   iNumDaysInYear - integer number of days that have elapsed between
!                    January 1 and December 31 of the current year.
!
! SOURCE

function num_days_in_year(iYear) result(iNumDaysInYear)

  integer (kind=T_INT), intent(in) :: iYear

  ! [ LOCALS ]
  integer (kind=T_INT) :: iFirstDay, iLastDay, iNumDaysInYear

  iFirstDay = julian_day ( iYear, 1, 1 )
  iLastDay = julian_day ( iYear, 12, 31 )
  iNumDaysInYear = iLastDay - iFirstDay + 1

  return

end function num_days_in_year

!!***

!--------------------------------------------------------------------------
!!****f* types/days_of_year
! NAME
!   day_of_year - Return the current day number in the given year.
!
! SYNOPSIS
!   This function simply returns the number of days given the current year.
!
! INPUTS
!   iMonth  2-digit month of year (1-12)
!   iDay    2-digit day of month (1-~31)
!   iYear   4-digit year
!
! OUTPUTS
!   iDayOfYear - integer number of days that have elapsed since
!                January 1 and current day of year.
!
! SOURCE

function day_of_year(iMonth, iDay, iYear) result(iDayOfYear)

  integer (kind=T_INT), intent(in) :: iMonth
  integer (kind=T_INT), intent(in) :: iDay
  integer (kind=T_INT), intent(in) :: iYear

  ! [ LOCALS ]
  integer (kind=T_INT) :: iFirstDay, iLastDay, iDayOfYear

  iFirstDay = julian_day ( iYear, 1, 1 )
  iLastDay = julian_day ( iYear, iMonth, iDay )
  iDayOfYear = iLastDay - iFirstDay + 1

  return

end function day_of_year

!!***

!--------------------------------------------------------------------------
!!****f* types/gregorian_date
! NAME
!   gregorian_date - Convert from a Julian day number to a Gregorian date.
!
! SYNOPSIS
!   Conversion to a Gregorian calendar date from a Julian date.
!   Valid for any Gregorian calendar date producing a Julian day number
!   greater than zero.
!
! INPUTS
!   iJD     integer number of days that have elapsed since noon
!           Greenwich Mean Time (UT or TT) Monday, January 1, 4713
! OUTPUTS
!   iYear   4-digit year
!   iMonth  2-digit month (1-12)
!   iDay    2-digit day (1-31)
!
! NOTES
!   Reference: Fliegel, H. F. and van Flandern, T. C. (1968).
!   Communications of the ACM, Vol. 11, No. 10 (October, 1968).
!   Modified from code found at:
!       http://aa.usno.navy.mil/faq/docs/JD_Formula.html
!
! SOURCE

subroutine gregorian_date(iJD, iYear, iMonth, iDay)

!! COMPUTES THE GREGORIAN CALENDAR DATE (YEAR,MONTH,DAY)
!! GIVEN THE JULIAN DATE (JD).

  ! [ ARGUMENTS ]
  integer (kind=T_INT) :: iJD
  integer (kind=T_INT), intent(inout) :: iYear, iMonth, iDay
  ! [ LOCALS ]
  integer (kind=T_INT) iI,iJ,iK,iL,iN

  iL= iJD + 68569_T_INT
  iN= 4*iL / 146097_T_INT
  iL= iL - (146097_T_INT * iN + 3_T_INT)/4_T_INT
  iI= 4000_T_INT * (iL + 1_T_INT) / 1461001_T_INT
  iL= iL - 1461_T_INT * iI / 4_T_INT + 31_T_INT
  iJ= 80_T_INT * iL / 2447_T_INT
  iK= iL - 2447_T_INT * iJ / 80_T_INT
  iL= iJ / 11_T_INT
  iJ= iJ + 2_T_INT - 12_T_INT * iL
  iI= 100_T_INT * (iN - 49_T_INT) + iI + iL

  iYear = iI
  iMonth = iJ
  iDay = iK

  return

end subroutine gregorian_date

!--------------------------------------------------------------------------------------------------

function pretty_date( iJD )   result( sDateTxt )

  integer (kind=T_INT)            :: iJD
  character (len=10)              :: sDateTxt

  ![ LOCALS ]
  integer (kind=T_INT) :: iMonth, iDay, iYear

  call gregorian_date(iJD,iYear,iMonth,iDay)

  write( sDateTxt, fmt="(i2.2,'/',i2.2,'/',i4.4)") iMonth, iDay, iYear

end function pretty_date

!--------------------------------------------------------------------------

function days_in_month(iMonth, iYear)   result(iNumDaysInMonth)

  ! [ ARGUMENTS ]
  integer (kind=T_INT),intent(in) :: iMonth
  integer (kind=T_INT),intent(in) :: iyear

  ! [ LOCALS ]
  integer (kind=T_INT) :: iBeginJulDay  ! Julian Day of beginning of month
  integer (kind=T_INT) :: iEndJulDay    ! Julian Day of end of month
  integer (kind=T_INT) :: i
  integer (kind=T_INT) :: iNumDaysInMonth
  integer (kind=T_INT) :: iDayOfYear
  character (len=3) :: sMonthName
  logical (kind=T_LOGICAL) :: lMonthEnd

  iBeginJulDay = julian_day ( iYear, iMonth, 1_T_INT )

  do i=28,31

    call LookupMonth(iMonth, i, iYear,iDayOfYear, &
                       sMonthName,lMonthEnd)
    if(lMonthEnd) exit

  end do

  iEndJulDay = julian_day ( iYear, iMonth, i)

  iNumDaysInMonth = iEndJulDay - iBeginJulDay + 1

  return

end function days_in_month

!--------------------------------------------------------------------------
!!****s* types/LookUpMonth
! NAME
!   LookUpMonth - Returns the month name and integer index given day of year.
!
! SYNOPSIS
!   This subroutine returns the name and integer index of the month (1-12) that
!   corresponds to the current integer day of the year (1-365). Also indicates
!   whether the current day is the last day in the current month. Does *not*
!   account for leap years.
!
! INPUTS
!   iMonth - Month index (1-12) corresponding to the current day of the year.
!   iDay - Day index (1-31) corresponding to the current day within the
!          present month.
!   iYear - Current Gregorian year (4-digit).
!
! OUTPUTS
!   iDayOfYear - Integer value corresponding to the number of days since
!                January 1 of the current year (e.g. January 1 = day 1).
!   sMonthName - character string representing the name of the current month.
!   lMonthEnd -  .true. if the current day of the month is the last day of
!                the month, .false. otherwise.
!
! NOTES
!   see http://daac.gsfc.nasa.gov/julian_calendar.shtml for tables relating
!   the day of year to calendar month
!
! SOURCE

subroutine LookupMonth(iMonth, iDay, iYear,iDayOfYear, &
                       sMonthName,lMonthEnd)

  ! [ ARGUMENTS ]
  integer (kind=T_INT),intent(in) :: iMonth
  integer (kind=T_INT),intent(in) :: iDay
  integer (kind=T_INT),intent(in) :: iYear
  integer (kind=T_INT),intent(out) :: iDayOfYear
  character (len=3),intent(out) :: sMonthName
  logical (kind=T_LOGICAL),intent(out) :: lMonthEnd
  ! [ LOCALS ]
  type ( MONTH_T ),pointer :: mo
  integer (kind=T_INT) :: iEpochMonth = 1
  integer (kind=T_INT) :: iEpochDay = 1
  integer (kind=T_INT) :: iEpochJulianDay
  integer (kind=T_INT) :: iDummy1, iDummy2
  integer (kind=T_INT) :: iJulianDay
  integer (kind=T_INT) :: iTomorrowsMonth

  ! get the Julian Day corresponding to the first day of the
  ! current calendar year
  iEpochJulianDay = julian_day( iYear, iEpochMonth, iEpochDay )

  ! get the Julian Day corresponding to the current day of the
  ! current calendar year
  iJulianDay = julian_day (iYear, iMonth, iDay)

  ! calculate day of year as the difference between today's Julian
  ! Day Number and the JDN for Jan 1 of the current year.
  iDayOfYear = iJulianDay - iEpochJulianDay + 1

  call gregorian_date(iJulianDay + 1, iDummy1, iTomorrowsMonth, iDummy2)

  lMonthEnd = .not. (iTomorrowsMonth == iMonth)

  mo => YEAR_INFO(iMonth)

  sMonthName = mo%sName

!  do i=1,12
!    mo => YEAR_INFO(i)
!    if ( iDayOfYear >= mo%iStart .and. iDayOfYear <= mo%iEnd ) then
!      iMonth = i
!      iDayOfMonth = iDayOfYear - mo%iStart + 1
!      sMonthName = mo%sName
!      lMonthEnd = iDayOfYear == mo%iEnd
!      exit
!    end if
!  end do

  return
end subroutine LookupMonth

!!***

SUBROUTINE SSORT (x,n)

  IMPLICIT NONE
!
!    Example of an Insertion Sort, Modified to Fortran90
!    Function in shifting contents of X
!
!***BEGIN PROLOGUE  SSORT
!***PURPOSE  Sort an array and make the same interchanges in
!            an auxiliary array.  The array is sorted in
!            decreasing order.
!***TYPE      SINGLE PRECISION
!***KEYWORDS  SORT, SORTING
!
!   Description of Parameters
!      X - array of values to be sorted   (usually abscissas)
!      IY - array to be carried with X (all swaps of X elements are
!          matched in IY .  After the sort IY(J) contains the original
!          postition of the value X(J) in the unsorted X array.
!      N - number of values in array X to be sorted
!      KFLAG - Not used in this implementation
!
!***REVISION HISTORY  (YYMMDD)
!   950310  DATE WRITTEN
!   John Mahaffy
!***END PROLOGUE  SSORT
!     .. Scalar Arguments ..
      INTEGER n
!     .. Array Arguments ..
      real (kind=T_REAL) :: x(:)
      real (kind=T_REAL),allocatable :: x_rev(:)
!      integer (kind=T_INT) :: iy(:)
!     .. Local Scalars ..
      real (kind=T_REAL) :: TEMP
      integer (kind=T_INT) :: I, J, K, ITEMP, iStat
!
!***FIRST EXECUTABLE STATEMENT  SSORT


  ALLOCATE(x_rev(n),STAT=iStat)
  call Assert(LOGICAL(iStat==0,kind=T_LOGICAL), &
    "Problem allocating memory in SSORT")
!
  do I=2,N
!
!    If the Ith element is out of order with the preceeding
!    element search for its position in the portion of the
!    array that is already ordered.
!
     IF ( X(I).GT.X(I-1) ) THEN
       do J=I-2,1,-1
         IF(X(I).LT.X(J)) go to 70
       end do


       J=0
!
!     Use Fortran 90 Intrinsic Function to
!     Shift   array elements and insert
!     cshift is a circular shift.  With -1 as the second
!     argument, it shifts all listed array elements up one
!     element in the array, except the last listed element, which
!     circles around to the first listed array element.
!

  70   x(j+1:i) = cshift(x(j+1:i),-1)

       ! iy goes along for the ride
!       iy(j+1:i) = cshift(iy(j+1:i),-1)

         ENDIF
  end do

  do i=1,n
    x_rev(i) = x(n+1-i)
  end do

  x(1:n) = x_rev(1:n)

  return

end subroutine ssort

!--------------------------------------------------------------------------
!!****s* types/CleanUpCsv
! NAME
!   CleanUpCsv - Strips punctuation from a string, making it Fortran-friendly.
!
! SYNOPSIS
!   Strips commas, tabs, and double-quotes from a text string.
!
! INPUTS
!   s - Character string to operate on.
!
! OUTPUTS
!   s - Character string to operate on.
! SOURCE

subroutine CleanUpCsv ( s )

  ! ARGUMENTS
  character (len=*), intent(inout) :: s
  ! LOCALS
  integer (kind=T_INT) :: i,j
  ! LOCAL PARAMETERS
  character (len=1),dimension(3),parameter :: &
                REPLACE_CHARS = (/ ',', achar(9), '"' /)

  do i=1,size(REPLACE_CHARS)
  do j=1,len(s)
    if ( s(j:j) == REPLACE_CHARS(i) ) s(j:j) = ' '
  end do
  end do

  return
end subroutine CleanUpCsv

!!***
!--------------------------------------------------------------------------
!!****s* types/CleanUp
! NAME
!   CleanUp - Strips spaces and punctuation from a string, making it
!             Fortran-friendly.
!
! SYNOPSIS
!   Strips SPACES, commas, tabs, and double-quotes from a text string.
!
! INPUTS
!   s - Character string to operate on.
!
! OUTPUTS
!   s - Character string to operate on.
! SOURCE

subroutine CleanUp ( s )

  ! ARGUMENTS
  character (len=*), intent(inout) :: s
  ! LOCALS
  integer (kind=T_INT) :: i,j, iCount
  character(len=256) :: sTemp = ""
  ! LOCAL PARAMETERS
  character (len=1),dimension(3),parameter :: &
                REPLACE_CHARS = (/ ',', achar(9), '"' /)

  sTemp=""
  iCount = 0

  do i=1,len(s)

    do j=1,size(REPLACE_CHARS)
      if ( s(i:i) == REPLACE_CHARS(j) ) s(i:i) = ' '
    end do

    if(s(i:i)==' ') then
      cycle
    else
      iCount = iCount + 1
      sTemp(iCount:iCount) = s(i:i)
    end if

  end do

  s=""
  s = trim(sTemp)

  return
end subroutine CleanUp

!!***

!--------------------------------------------------------------------------
!!****s* types/Chomp
! NAME
!   Chomp - Chomps the first space-delimited word from the
!           beginning of a text string.
!
! SYNOPSIS
!   Chomps the first space-delimited word from the the text string in
!   sRecord and returns it in sItem; leaves the remaining text in sRecord.
!
! INPUTS
!   sRecord - Character string to operate on.
!
! OUTPUTS
!   sRecord - Character string to operate on.
!   sItem - Character string to operate on.
!
! EXAMPLE
!   input:   sRecord = "THIS IS THE TIME"    sItem = ""
!   output:  sRecord = "IS THE TIME"         sItem = "THIS"
!
! SOURCE

subroutine Chomp(sRecord, sItem)

  ! ARGUMENTS
  character (len=*), intent(inout) :: sRecord
  character (len=256), intent(out) :: sItem
  ! LOCALS
  integer (kind=T_INT) :: iR                      ! Index in sRecord
  integer (kind=T_INT) :: iS                      ! Index in sItem
  logical (kind=T_LOGICAL) :: lSkip               ! TRUE while skipping spaces

  ! Set my pointers and remove leading and trailing spaces
  iR = 1
  iS = 1
  sItem = ""
  lSkip = lTRUE
  do iR=1,len_trim(sRecord)
      if ( lSkip .and. sRecord(iR:iR) == " " ) then
          cycle
      else if ( .not. lSkip .and. sRecord(iR:iR) == " " ) then
          exit
      else
          lSkip = lFALSE
          sItem(iS:iS) = sRecord(iR:iR)
          iS = iS+1
      end if
  end do
  sRecord = sRecord(iR:)

  return
end subroutine Chomp

!----------------------------------------------------------------------

subroutine Uppercase(s)

  character(len=*), intent(inout) :: s
  integer(kind=T_INT) :: i, iASCII

  do i=1,len(s)

    iASCII = ICHAR(s(i:i))

    if(iASCII>=97 .and. iASCII<=122) then
      s(i:i) = CHAR(iASCII - 32)
    end if

  end do

  return

end subroutine Uppercase

!!***
subroutine find_dot(sRecord, sItem)

  ! returns all non-blank elements of a string up to the "."

  ! ARGUMENTS
  character (len=*), intent(inout) :: sRecord
  character (len=256), intent(out) :: sItem
  ! LOCALS
  integer (kind=T_INT) :: iR                      ! Index in sRecord
  integer (kind=T_INT) :: iS                      ! Index in sItem
  logical (kind=T_LOGICAL) :: lSkip               ! TRUE while skipping spaces

  ! Set my pointers and remove leading and trailing spaces
  iR = 1
  iS = 1
  sItem = ""
  lSkip = lTRUE
  do iR=1,len_trim(sRecord)
      if ( lSkip .and. sRecord(iR:iR) == " " ) then
          cycle
      else if ( .not. lSkip .and. sRecord(iR:iR) == "." ) then
          exit
      else
          lSkip = lFALSE
          sItem(iS:iS) = sRecord(iR:iR)
          iS = iS+1
      end if
  end do
  sRecord = sRecord(iR:)

  return
end subroutine find_dot

!!***

!----------------------------------------------------------------------

function int2char(iValue)  result(sValue)

  integer (kind=T_INT) :: iValue
  character (len=256) :: sValue

  write(sValue,FMT=*) iValue

  call CleanUp(sValue)

  return

end function int2char

!----------------------------------------------------------------------

function real2char(rValue)  result(sValue)

  real (kind=T_REAL) :: rValue
  character (len=256) :: sValue

  write(sValue,FMT="(f16.3)") rValue

  sValue = ADJUSTL(sValue)

  return

end function real2char

end module
