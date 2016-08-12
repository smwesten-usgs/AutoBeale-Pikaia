module units

  use types
  implicit none

contains

function rf_Q_disp(pConfig,rValue)   result(rConvertedValue)

  type (CONFIG_T), pointer :: pConfig ! pointer to data structure that contains
                                        ! program options, flags, and other settings

  real (kind=T_REAL) :: rValue, rConvertedValue

  rConvertedValue = rValue / &
       FLOW_UNITS(pConfig%iFlowUnitsCode)%rConversionFactor

end function rf_Q_disp

!----------------------------------------------------------------------

function sf_Q_units(pConfig,rValue)  result(sQ_w_units)

  ! returns a formated text string containing the flow value converted
  ! to display units, along with the description of those units

  type (CONFIG_T), pointer :: pConfig ! pointer to data structure that contains
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

end function sf_Q_units

!----------------------------------------------------------------------

function sf_C_units(pConfig,rValue)  result(sC_w_units)

  ! returns a formated text string containing the concentration value converted
  ! to display units, along with the description of those units

  type (CONFIG_T), pointer :: pConfig ! pointer to data structure that contains
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

  type (CONFIG_T), pointer :: pConfig ! pointer to data structure that contains
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

end function sf_L_units

!----------------------------------------------------------------------

function sf_L2_units(pConfig,rValue)  result(sL2_w_units)

  ! returns a formated text string containing the load value converted
  ! to display units, along with the description of those units

  type (CONFIG_T), pointer :: pConfig ! pointer to data structure that contains
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

end module units
