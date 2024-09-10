set(PROG_SRC arrayutil.f90
             CMakeLists.txt
             cmdpath.F90
             constants.f90
             error.F90
             fileutil.F90
             fract.f90
             lsqutil.F90
             main.f90
             math_util.F90
             program.f90
             spginfo.f90
             strutil.f90
             symm_table.f90
             unit_cell.f90)

add_executable(${PROJECT_NAME} ${PROG_SRC})
