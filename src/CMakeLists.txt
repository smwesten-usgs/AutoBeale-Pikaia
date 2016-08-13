add_executable( autobeale_pikaia
      beale_data.F90
      beale.F90
      run_pikaia.F90
      input.F90
      pikaia.F90
      pikaia_function.F90
      main.F90
      output.F90
      strata.F90
      types.F90
      units.F90
      generated/version_control.F90 )

add_executable( autobeale_tests
      beale.F90
      beale_data.F90
      output.F90
      types.F90
      units.F90
      unit_tests.F90 )
