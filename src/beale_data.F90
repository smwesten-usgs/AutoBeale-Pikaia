module beale_data

use types
implicit none

  type (CONFIG_T), pointer :: pConfig
  type (CONFIG_T), pointer :: pBestConfig
  type (CONFIG_T), pointer :: pTestConfig

  integer, parameter :: PIKAIA_MAX_NUM_PARAMETERS                      = 32
  integer, parameter :: PIKAIA_MAX_POPULATION_SIZE                     = 150
  integer, parameter :: PIKAIA_MAX_GENERATION_LENGTH                   = 500
  integer, parameter :: PIKAIA_MAX_NUM_OPTIMIZATION_ATTEMPTS           = 3
  integer, parameter :: PIKAIA_MAX_NUM_ITERATIONS_WITHOUT_IMPROVEMENT  = 12
  integer, parameter :: PIKAIA_OUTPUT_OPTION                           = 0
  integer, parameter :: PIKAIA_NUM_GENES_IN_CHROMOSOME                 = 7

  type (FLOW_T), dimension(:), pointer           :: pFlow
  type (CONC_T), dimension(:), pointer           :: pConc
  type (STRATUM_STATS_T), dimension(:), pointer  :: pStrata
  type (STRATUM_STATS_T), pointer                :: pStratum
  type (STRATUM_STATS_T), dimension(:), pointer  :: pTestStrata

end module beale_data
