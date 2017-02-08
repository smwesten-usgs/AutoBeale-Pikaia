module beale_data

use types
implicit none

  type (CONFIG_T), pointer :: pConfig
  type (CONFIG_T), pointer :: pBestConfig

  integer, parameter :: PIKAIA_MAX_NUM_PARAMETERS                      = 32
  integer, parameter :: PIKAIA_MAX_POPULATION_SIZE                     = 200
  integer, parameter :: PIKAIA_MAX_GENERATION_LENGTH                   = 1000
  integer, parameter :: PIKAIA_MAX_NUM_OPTIMIZATION_ATTEMPTS           = 3
  integer, parameter :: PIKAIA_MAX_NUM_ITERATIONS_WITHOUT_IMPROVEMENT  = 12
  integer, parameter :: PIKAIA_OUTPUT_OPTION                           = 0
  integer, parameter :: PIKAIA_NUM_GENES_IN_CHROMOSOME                 = 7

  type (FLOW_T), dimension(:), pointer           :: pFlow
  type (CONC_T), dimension(:), pointer           :: pConc
  type (STRATA_T), pointer                       :: pStrata
  type (STRATA_T), pointer                       :: pBestStrata
  type (COMBINED_STATS_T), pointer               :: pStats
  type (COMBINED_STATS_T), pointer               :: pBestStats
  type(JACKKNIFE_T), dimension(:),pointer        :: pJackknife

contains

  subroutine initialize_configuration_variables()

    integer (kind=T_INT) :: iStat

    ALLOCATE (pConfig, STAT=iStat)
    call Assert( LOGICAL( iStat == 0,kind=T_LOGICAL), &
       "Could not allocate memory for program control data structure")

    ALLOCATE (pBestConfig, STAT=iStat)
    call Assert( LOGICAL( iStat == 0,kind=T_LOGICAL), &
       "Could not allocate memory for copy of program control data structure")

  end subroutine initialize_configuration_variables

!--------------------------------------------------------------------------------------------------

  subroutine initialize_beale_variables()

  !----------------------------------------------------------------------
  ! Now allocate required memory for concentration and flow data
  !----------------------------------------------------------------------
    integer (kind=T_INT) :: iStat

    allocate ( pStats, stat=iStat)
    call Assert( LOGICAL( iStat == 0,kind=T_LOGICAL), &
       "Could not allocate memory for copy of combined stats data structure")

    allocate ( pBestStats, stat=iStat)
      call Assert( LOGICAL( iStat == 0,kind=T_LOGICAL), &
        "Could not allocate memory for copy of combined stats data structure")

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

    ALLOCATE (pStrata, STAT=iStat)
    call Assert( LOGICAL( iStat == 0,kind=T_LOGICAL), &
      "Could not allocate memory for BEALE STATS data array")

    ALLOCATE (pStrata%pStratum( iMAX_STRATA ), STAT=iStat)
    call Assert( LOGICAL( iStat == 0,kind=T_LOGICAL), &
      "Could not allocate memory for BEALE STATS data array")

    ALLOCATE (pBestStrata, STAT=iStat)
    call Assert( LOGICAL( iStat == 0,kind=T_LOGICAL), &
      "Could not allocate memory for BEALE STATS data array")

    ALLOCATE (pBestStrata%pStratum( iMAX_STRATA ), STAT=iStat)
    call Assert( LOGICAL( iStat == 0,kind=T_LOGICAL), &
      "Could not allocate memory for BEALE STATS data array")

  end subroutine initialize_beale_variables

end module beale_data
