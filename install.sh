cmake -DCMAKE_Fortran_COMPILER=gfortran  -B ./build
cmake --build ./build --parallel 4
cp ./build/src/spginfo . 
cp spginfo ~/bin
