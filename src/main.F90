program main

  use types
  use beale_data
  use ga_driver
  use beale
  use output
  use version_control, only      : GIT_COMMIT_HASH_STRING, GIT_BRANCH_STRING,   &
                                   BEALE_VERSION, COMPILE_DATE, COMPILE_TIME

  implicit none

  integer (kind=T_INT)           :: iStat, i,j
  integer (kind=T_INT)           :: iCommandCount, iValue, iNumFiles, iNumIterations
  character (len=256)            :: sRecord, sItem, sBuf, sOutputFilePrefix
  character (len=256)            :: sSite, sConstituent
  character (len=256)            :: sConcFile, sFlowFile, sResultsDir
  character (len=256)            :: sFileList = ""
  character (len=:), allocatable :: buf_str
  integer (kind=T_INT)           :: str_len
  character (len=1), parameter   :: sTab = CHAR(9)

  integer (kind=T_INT) :: iBMonth,iBDay,iBYear
  integer (kind=T_INT) :: iEMonth,iEDay,iEYear

  real (kind=T_REAL) :: rThetaHat, rSum1,rAvg1,rSum2,rAvg2, rSE_jack, rSE_CI

  type(T_JACKKNIFE), dimension(:),pointer :: pJackknife

  ALLOCATE (pConfig, STAT=iStat)
  call Assert( LOGICAL( iStat == 0,kind=T_LOGICAL), &
     "Could not allocate memory for program control data structure")

  ALLOCATE (pBestConfig, STAT=iStat)
  call Assert( LOGICAL( iStat == 0,kind=T_LOGICAL), &
     "Could not allocate memory for copy of program control data structure")

  iCommandCount = COMMAND_ARGUMENT_COUNT()

  if(iCommandCount < 4) then
    buf_str = "AutoBeale-Pikaia version "//trim(BEALE_VERSION)
    str_len = len_trim( buf_str )
    write(*, "(/,a)") repeat("-", str_len + 4)
    write(*, "('| ',a,' |')") buf_str
    write(*, "(a,/)") repeat("-", str_len + 4)
    write(*,"(a,t35,a)") "Git branch and commit hash:",trim( adjustl(GIT_BRANCH_STRING) )            &
                  //", "//trim( adjustl(GIT_COMMIT_HASH_STRING) )
    write(*,"(a,t35,a)") "Compile date, time: ", trim(COMPILE_DATE)//" "//trim(COMPILE_TIME)
#ifdef __GFORTRAN__
      write(*,"(a,t35,a)") "Compiler name:", "gfortran version "//trim(__VERSION__)
#endif
    write(*,"(/,a,/)") "Usage: autobeale_pikaia -flow[_old] 'flow_filename' -conc[_old] 'concentration filename'"
    stop
  end if

  do i=1,iCommandCount,2
    call GET_COMMAND_ARGUMENT(i,sBuf)
    if(trim(sBuf)=="-filelist") then
      call GET_COMMAND_ARGUMENT(i+1,sBuf)
      sFileList = trim(sBuf)

    elseif(trim(sBuf)=="-flow") then
      call GET_COMMAND_ARGUMENT(i+1,sBuf)
      pConfig%sFlowFileName = trim(sBuf)
      pConfig%iFlowFileFormat = iFLOW_FILE_FORMAT_USGS

    elseif(trim(sBuf)=="-flow_old") then
      call GET_COMMAND_ARGUMENT(i+1,sBuf)
      pConfig%sFlowFileName = trim(sBuf)
      pConfig%iFlowFileFormat = iFLOW_FILE_FORMAT_ORIGINAL

    elseif(trim(sBuf)=="-conc") then
      call GET_COMMAND_ARGUMENT(i+1,sBuf)
      pConfig%sConcFileName = trim(sBuf)
      pConfig%iConcFileFormat = iCONC_FILE_FORMAT_USGS

    elseif(trim(sBuf)=="-conc_old") then
      call GET_COMMAND_ARGUMENT(i+1,sBuf)
      pConfig%sConcFileName = trim(sBuf)
      pConfig%iConcFileFormat = iCONC_FILE_FORMAT_ORIGINAL

    elseif(trim(sBuf)=="-max_strata") then
      call GET_COMMAND_ARGUMENT(i+1,sBuf)
      read(sBuf,FMT=*) iValue
      pConfig%iMaxEvalStrata = iValue - 1

    elseif(trim(sBuf)=="-basedir") then
      call GET_COMMAND_ARGUMENT(i+1,sBuf)
      read(sBuf,FMT=*) pConfig%sBaseDirName

    elseif(trim(sBuf)=="-flowdir") then
      call GET_COMMAND_ARGUMENT(i+1,sBuf)
      read(sBuf,FMT=*) pConfig%sFlowDirName

    elseif(trim(sBuf)=="-concdir") then
      call GET_COMMAND_ARGUMENT(i+1,sBuf)
      read(sBuf,FMT=*) pConfig%sConcDirName

    elseif(trim(sBuf)=="-resultsdir") then
      call GET_COMMAND_ARGUMENT(i+1,sBuf)
      read(sBuf,FMT=*) pConfig%sResultsDirName

    elseif(trim(sBuf)=="-jackknife") then
      call GET_COMMAND_ARGUMENT(i+1,sBuf)
      if(trim(sBuf)=="ON") then
        pConfig%lJackknife=lTRUE
      endif

    elseif(trim(sBuf)=="-load_units") then
      call GET_COMMAND_ARGUMENT(i+1,sBuf)
      if(trim(sBuf)=="Kg" .or. trim(sBuf)=="kg") then
        pConfig%iLoadUnitsCode = iKILOGRAMS_PER_YEAR
      elseif(trim(sBuf)=="MT" .or. trim(sBuf)=="mt") then
        pConfig%iLoadUnitsCode = iMETRIC_TONS_PER_YEAR
      else
        call Assert(lFALSE, &
          "Unknown loading units: "//trim(sBuf))
      end if

    elseif(trim(sBuf)=="-conc_units") then
      call GET_COMMAND_ARGUMENT(i+1,sBuf)
      if(trim(sBuf)=="mg/L" .or. trim(sBuf)=="mg/l") then
        pConfig%iConcUnitsCode = iMILLIGRAMS_PER_LITER
      elseif(trim(sBuf)=="ug/L" .or. trim(sBuf)=="ug/l") then
        pConfig%iConcUnitsCode = iMICROGRAMS_PER_LITER
      elseif(trim(sBuf)=="ng/L" .or. trim(sBuf)=="ng/l") then
        pConfig%iConcUnitsCode = iNANOGRAMS_PER_LITER
      elseif(trim(sBuf)=="g/L" .or. trim(sBuf)=="g/l") then
        pConfig%iConcUnitsCode = iGRAMS_PER_LITER
      else
        call Assert(lFALSE, &
          "Unknown concentration units: "//trim(sBuf))
      end if

    else
      call Assert(lFALSE, &
       "Unknown program option: "//trim(sBuf))
    end if

  end do

  write(sBuf,FMT="(i3)") iPikaiaMaxParameters

  call Assert(LOGICAL(pConfig%iMaxEvalStrata>0 .and. &
    pConfig%iMaxEvalStrata < iPikaiaMaxParameters+1, &
    kind=T_LOGICAL), &
    "Number of possible strata is limited to the range 0 to " &
    //trim(sBuf))

  iNumFiles = 0
  iNumIterations = 1

  if(trim(sFileList)/="") then
    i=0
    open (UNIT=LU_FILELIST,iostat=iStat, &
      file=trim(sFileList),status='OLD')
    call Assert(LOGICAL(iStat==0,kind=T_LOGICAL), &
      "Could not open file list: "//trim(sFileList))
    do
      read(LU_FILELIST,*, iostat=iStat)
      if (iStat/=0) exit
      i=i+1
    end do
    rewind(LU_FILELIST)
    iNumFiles = i
  end if

  main_loop: do

  if(iNumFiles>0) then
    print *, '>> Attempting to read filenames from a list file'
    read(LU_FILELIST,*,iostat=iStat) pConfig%sFlowFileName, &
       pConfig%sConcFileName
    call Assert(LOGICAL(iStat==0,kind=T_LOGICAL), &
      "Problem reading flow and concentration filenames from filelist.")
    print *, 'Flow file: ',pConfig%sFlowFileName
    print *, 'Concentration file: ',pConfig%sConcFileName
  end if

  sRecord = pConfig%sFlowFileName

  call find_dot(sRecord, sOutputFilePrefix)

!----------------------------------------------------------------------
! Set configuration options
!----------------------------------------------------------------------

!  pConfig%sFlowFileName = "Q_Kalamazoo_CAL05.txt"
!  pConfig%sConcFileName = "TotPhos_Kalamazoo_CAL05.txt"
!  pConfig%sFlowFileName = "flowfile.txt"
!  pConfig%sConcFileName = "concfile.txt"

!  pConfig%sFlowFileName = "ROCKFLOW.DAT"
!  pConfig%sConcFileName = "ROCKNO23_4yr.DAT"


  pConfig%sShortOutputFileName = "beale_summary.txt"

!----------------------------------------------------------------------
! Open data files
!----------------------------------------------------------------------


  sConcFile = trim(pConfig%sBaseDirName)//trim(pConfig%sConcDirName) &
     //trim(pConfig%sConcFileName)

  sFlowFile = trim(pConfig%sBaseDirName)//trim(pConfig%sFlowDirName) &
     //trim(pConfig%sFlowFileName)

  sResultsDir = trim(pConfig%sBaseDirName)//trim(pConfig%sResultsDirName)

  open (UNIT=LU_CONCDAT,iostat=iStat, &
    file=trim(sConcFile),status='OLD')

  if(iStat /= 0) then
    print *, "Could not open concentration file:", &
      trim(sConcFile)
    stop
  end if

  open (UNIT=LU_FLOWDAT,iostat=iStat, &
    file=trim(sFlowFile),status='OLD')

  if(iStat /= 0) then
    print *, "Could not open flow file:", &
      trim(sFlowFile)
    stop
  end if

  open (UNIT=LU_STATS_OUT,iostat=iStat, &
    file=trim(sResultsDir)//"\beale_stats.txt",status='REPLACE')

  call Assert(LOGICAL(iStat==0,kind=T_LOGICAL), &
    "Could not open output stats file")
!
!----------------------------------------------------------------------
! Open files for output
!----------------------------------------------------------------------

  open (LU_SHORT_RPT,file=trim(sResultsDir)//"\"//trim(pConfig%sShortOutputFileName), &
    ACCESS='APPEND',FORM='FORMATTED',iostat=iStat)

  if(iStat /= 0) then
    print *, "Could not open summary output file: ", &
      trim(pConfig%sShortOutputFileName)
    stop
  end if

  open (UNIT=LU_LOADS_OUT,iostat=iStat, &
    file=trim(sResultsDir)//"\flow_conc_load_daily.txt",access='APPEND')
!
  call Assert(LOGICAL(iStat==0,kind=T_LOGICAL), &
    "Could not open flow, conc, and load result file")

  open (LU_JACKKNIFE_OUT,file=trim(sResultsDir)//"\"//"jackknife_results.txt", &
    ACCESS='APPEND',FORM='FORMATTED',iostat=iStat)

  call Assert(LOGICAL(iStat==0,kind=T_LOGICAL), &
    "Could not open jackknife output file")


  if(FTELL(LU_LOADS_OUT)==0) then

      write(LU_LOADS_OUT,FMT="(500a)") &
        'TribName',',', &
        'Date',',', 'Fraction.of.year',',',&
        'Flow',',','Conc',',','Load'

  end if

  if(FTELL(LU_SHORT_RPT)==0) then

      write(LU_SHORT_RPT,FMT="(500a)") &
        'FlowFileName',sTab, 'ConcFileName',sTab,&
        'Constituent', sTab, &
        'StartDate', sTab, 'EndDate', sTab, &
        'MaxNumStrata',sTab,'CombinedLoad',sTab, &
        'Units',sTab,'CombinedLoadCI', sTab,&
        'AnnualizedLoad', sTab, &
        'AnnualizedLoadCI', sTab, &
        'AnnualizedLoad-Jackknife', sTab, &
        'AnnualizedLoadCI-Jackknife', sTab, &
        'AnnualizedFlow',sTab, &
        'CombinedMSE', sTab,'CombinedRMSE',&
        sTab,'CombinedEffectiveDegreesFreedom', sTab, &
        'FuncCallNum'

  end if

  if(FTELL(LU_JACKKNIFE_OUT)==0) then

      write(LU_JACKKNIFE_OUT,FMT="(500a)") &
        'TribName',sTab,'Constituent',sTab, &
        'StartDate',sTab,'EndDate',sTab,'Load', &
        sTab,'Units'

  end if

!  open (LU_PIKAIA_OUT,file='Pikaia_output.txt', &
!    STATUS='REPLACE',FORM='FORMATTED',iostat=iStat)
!
!  if(iStat /= 0) then
!    print *, "Could not open summary output file:", &
!      trim(pConfig%sShortOutputFileName)
!    stop
!  end if

!----------------------------------------------------------------------
! Read through flow file to determine how many records are present
!----------------------------------------------------------------------

  i=0
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

    i=i+1
  end do

  pConfig%iNumFlowPoints = i

!----------------------------------------------------------------------
! Read through concentration file to see how many records are present
!----------------------------------------------------------------------

  i=0
  do
    read ( unit=LU_CONCDAT, fmt="(a256)", iostat=iStat ) sRecord

    if ( iStat < 0 ) exit     ! EOF mark
    call Assert( LOGICAL( iStat == 0,kind=T_LOGICAL), &
       "Terminating due to error reading CONCENTRATION file" )
    if ( sRecord(1:1) == "#" ) cycle      ! Ignore comment lines

    i=i+1
  end do

  pConfig%iNumConcPoints = i

  rewind(LU_CONCDAT)
  rewind(LU_FLOWDAT)

!----------------------------------------------------------------------
! Now allocate required memory for concentration and flow data
!----------------------------------------------------------------------

  ALLOCATE (pFlow(pConfig%iNumFlowPoints), STAT=iStat)
  call Assert( LOGICAL( iStat == 0,kind=T_LOGICAL), &
     "Could not allocate memory for FLOW data structure")
  write(LU_STD_OUT,FMT="('Allocated memory for ',i4,' flow records')") &
     pConfig%iNumFlowPoints

  ALLOCATE (pConc(pConfig%iNumConcPoints), STAT=iStat)
  call Assert( LOGICAL( iStat == 0,kind=T_LOGICAL), &
     "Could not allocate memory for CONCENTRATION data structure")
  write(LU_STD_OUT,FMT="('Allocated memory for ',i4,' concentration records')") &
     pConfig%iNumConcPoints

  ALLOCATE (pJackknife(pConfig%iNumConcPoints), STAT=iStat)
  call Assert( LOGICAL( iStat == 0,kind=T_LOGICAL), &
     "Could not allocate memory for jackknife results")
  write(LU_STD_OUT,FMT="('Allocated memory to hold ',i4,' jackknife results')") &
     pConfig%iNumConcPoints

  call read_data (pConfig, pFlow, pConc)

  call clean_concentration_data(pFlow,pConc)

  ! update this value once the input concentration file has been culled of
  ! duplicates
  pConfig%iNumConcPoints = size(pConc)

  call clean_flow_data(pFlow)

  ! *******************************************************************
  ! Now, finally open extended output file
  ! *******************************************************************

  iBMonth = pFlow(1)%iMonth
  iBDay = pFlow(1)%iDay
  iBYear = pFlow(1)%iYear

  iEMonth = pFlow(size(pFlow%rFlow))%iMonth
  iEDay = pFlow(size(pFlow%rFlow))%iDay
  iEYear = pFlow(size(pFlow%rFlow))%iYear

  sSite = trim(pConc(1)%sTribName)
  sConstituent = trim(pConc(1)%sConstituentName)

  call CleanUp (sSite)
  call CleanUp(sConstituent)

  write(pConfig%sExtendedOutputFileName, &
  FMT="('LOAD_',a,'_',a,'_',i4.4,i2.2,i2.2,'-'," &
      //"i4.4,i2.2,i2.2,'.txt')") &
    trim(sSite),trim(sConstituent),iBYear,iBMonth,iBDay, &
    iEYear,iEMonth,iEDay

  open (UNIT=LU_LONG_RPT,&
    FILE=trim(sResultsDir)//"\"//trim(pConfig%sExtendedOutputFileName), &
    STATUS='REPLACE', FORM='FORMATTED',iostat=iStat)

  if(iStat /= 0) then
    print *, "Could not open extended output file:",trim(pConfig%sExtendedOutputFileName)
    stop
  end if

  call calc_daily_load(pFlow,pConc,pConfig)

  call monthly_stats(LU_LONG_RPT,pFlow,pConc,pConfig)

  if(pConfig%lJackknife) then

    write(LU_STD_OUT,"(/,'Performing jackknife, flow file = ',a)") &
      trim(pConfig%sFlowFileName)

    iNumIterations = pConfig%iNumConcPoints

    pJackknife%rEstimate = rZERO
    pJackknife%rEstimate_SQ = rZERO

    do j=1,iNumIterations

      pConc%lInclude = lTRUE

      ! one by one, eliminate data points and rerun the whole shootin match
      pConc(j)%lInclude = lFALSE

      pConfig%iNumConcPoints = COUNT(pConc%lInclude)

      ! subroutine "pikaia_driver" takes care of iterating over multiple strata
      ! to determine the optimum stratification scheme
      call pikaia_driver(pConfig, pBestConfig)

      pJackknife(j)%rEstimate = pBestConfig%rCombinedLoadAnnualized
      pJackknife(j)%rEstimate_SQ = pBestConfig%rCombinedLoadAnnualized**2

      write(LU_STD_OUT, &
        "('  jackknife iteration ',i3,' of ',i3,' : annual load = ',a)") &
         j,iNumIterations,trim(sf_L_units(pConfig,pJackknife(j)%rEstimate))

      write(LU_JACKKNIFE_OUT,FMT="(4a,2(i4.4,'-',i2.2,'-',i2.2,a),a)") &
          trim(sSite), sTab,trim(sConstituent),sTab, &
          iBYear,iBMonth,iBDay,sTab,iEYear,iEMonth,iEDay,sTab, &
          trim(sf_L_units(pConfig,pJackknife(j)%rEstimate))

      call reset_config(pConfig)
      call reset_config(pBestConfig)

    end do

    ! eq. 11.4, Efron and Tibshirani, p. 141
    rThetaHat = SUM(pJackknife%rEstimate) / REAL(iNumIterations)

    rSum1 = rZERO

    do j=1,iNumIterations
      rSum1 = rSum1 + (pJackknife(j)%rEstimate - rThetaHat)**2
    end do

    ! eq. 11.5, Efron and Tibshirani, p. 141
    ! includes an "inflation factor": (n-1) / n, because the jackknife
    ! deviations tend to be smaller than the bootstrap variations.
    rSE_jack = sqrt( (REAL(iNumIterations) - 1_T_REAL ) *rSum1 &
                / REAL(iNumIterations) )

    rSE_CI = rf_compute_CI(REAL(iNumIterations,kind=T_REAL)-1., &
      (REAL(iNumIterations) - 1_T_REAL ) * rSum1 / REAL(iNumIterations))

    ! now run one more time in non-Jackknife mode
    call reset_config(pConfig)
    call reset_config(pBestConfig)
    pConc%lInclude = lTRUE
    pConfig%lJackknife=lFALSE
    pConfig%iNumConcPoints = size(pConc)

    pConfig%rJackCombinedLoadAnnualizedCI = rSE_CI
    pConfig%rJackCombinedLoadAnnualized = rThetaHat

    pBestConfig%rJackCombinedLoadAnnualizedCI = rSE_CI
    pBestConfig%rJackCombinedLoadAnnualized = rThetaHat

    call pikaia_driver(pConfig, pBestConfig)

    write(LU_LONG_RPT,*) ''

    write(LU_LONG_RPT,*) 'JACKKNIFE ESTIMATES:'
    write(LU_LONG_RPT,FMT="(3(a,1x))") pConfig%sFlowFileName, &
        pConfig%sStartDate, pConfig%sEndDate
    write(LU_LONG_RPT,FMT="(i4,2x,a)") &
      (j,sf_L_units(pConfig,pJackknife(j)%rEstimate),j=1,iNumIterations)
    write(LU_LONG_RPT,FMT="('Theta Hat:',f18.4)") rThetaHat
    write(LU_LONG_RPT,FMT="('Std Err:',f18.4)") rSE_jack
    write(LU_LONG_RPT,FMT="('CI:',f18.4)") rSE_CI

    pConfig%lJackknife=lTRUE
    pBestConfig%lJackknife=lTRUE

  else

    pConc%lInclude = lTRUE

    pConfig%iNumConcPoints = size(pConc)

    ! subroutine "pikaia_driver" takes care of iterating over multiple strata
    ! to determine the optimum stratification scheme
    call pikaia_driver(pConfig, pBestConfig)

  end if

  call print_short_report(pBestConfig,pConc)
  call write_R_script(pConfig,pBestConfig,pFlow,pConc)



  call reset_config(pConfig)
  call reset_config(pBestConfig)

  iNumFiles = iNumFiles - 1

  close(LU_FLOWDAT)
  close(LU_CONCDAT)
  close(LU_LONG_RPT)
  close(LU_SHORT_RPT)
  close(LU_STATS_OUT)
  close(LU_JACKKNIFE_OUT)
  close(LU_LOADS_OUT)
!  close(LU_ECHO_OUT)

  DEALLOCATE (pJackknife, STAT=iStat)
  call Assert( LOGICAL( iStat == 0,kind=T_LOGICAL), &
     "Could not deallocate memory for jackknife results")

  DEALLOCATE (pFlow, STAT=iStat)
  call Assert( LOGICAL( iStat == 0,kind=T_LOGICAL), &
     "Could not deallocate memory for FLOW data structure")

  DEALLOCATE (pConc, STAT=iStat)
  call Assert( LOGICAL( iStat == 0,kind=T_LOGICAL), &
     "Could not deallocate memory for CONCENTRATION data structure")

  if(iNumFiles<=0) exit

  end do main_loop

  DEALLOCATE (pConfig, STAT=iStat)
  call Assert( LOGICAL( iStat == 0,kind=T_LOGICAL), &
     "Could not deallocate memory for program control data structure")

  DEALLOCATE (pBestConfig, STAT=iStat)
  call Assert( LOGICAL( iStat == 0,kind=T_LOGICAL), &
     "Could not deallocate memory for optimum program control data structure")

  close(LU_FILELIST)

end program main