module beale_data

use types
implicit none

  type (T_CONFIG), pointer :: pConfig ! pointer to data structure that contains
                                        ! program options, flags, and other settings

  type (T_CONFIG), pointer :: pBestConfig ! pointer to data structure that contains
                                          ! program options, flags, and other settings

  integer, parameter :: iPikaiaMaxParameters = 32

  integer, parameter :: iPikaiaPopulationSize = 150
  integer, parameter :: iPikaiaGenerationLength = 700
  integer, parameter :: iPikaiaMaxNumAttempts = 3
  integer, parameter :: iPikaiaMaxIterations_w_NoChange = 7
  integer, parameter :: iPikaiaOutputOption = 0
  integer, parameter :: iPikaiaNumSigFigs = 6

  type (T_FLOW), dimension(:), pointer, save :: pFlow
  type (T_CONC), dimension(:), pointer, save :: pConc
  type (T_BEALE_STATS), dimension(:), pointer, save :: pBealeStats
  type (T_BEALE_STATS), pointer, save :: pB

end module beale_data
