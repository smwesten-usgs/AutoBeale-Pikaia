module beale_data

use types
implicit none

  type (T_CONFIG), pointer :: pConfig
  type (T_CONFIG), pointer :: pBestConfig
  type (T_CONFIG), pointer :: pTestConfig

  integer, parameter :: iPikaiaMaxParameters = 32

  integer, parameter :: iPikaiaPopulationSize = 150
  integer, parameter :: iPikaiaGenerationLength = 500
  integer, parameter :: iPikaiaMaxNumAttempts = 3
  integer, parameter :: iPikaiaMaxIterations_w_NoChange = 12
  integer, parameter :: iPikaiaOutputOption = 0
  integer, parameter :: iPikaiaNumSigFigs = 7

  type (T_FLOW), dimension(:), pointer, save :: pFlow
  type (T_CONC), dimension(:), pointer, save :: pConc
  type (T_BEALE_STATS), dimension(:), pointer, save :: pBealeStats
  type (T_BEALE_STATS), pointer, save :: pB

end module beale_data
