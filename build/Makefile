vpath %.f95 ../src
#FFLAGS	= -O3 -Wl,--heap,0x177000,--stack,0x08000 -march=pentium4 -funroll-loops -fomit-frame-pointer -malign-double -msse2
FFLAGS	= -O2 -march=prescott -pipe -mfpmath=sse -ffast-math -funroll-loops -fomit-frame-pointer -malign-double -msse2
#FFLAGS	= -O2 -march=pentium4 -funroll-loops -fomit-frame-pointer -malign-double -msse2
#FFLAGS	= -ftrace=full -fbounds-check
#FFLAGS_F95	= -ftrace=full -fbounds-check
#FFLAGS	=
F95		= g95
#F95		= gfortran
GCC     = gcc

LIBPATH = -LD:\MinGW\lib\gcc-lib\i686-pc-mingw32\4.0.4
#LIBPATH	= -L/cygdrive/c/usr/local/lib/gcc-lib/i686-pc-mingw/4.0.3
LIB		= -lf95

#LIBPATH	= -L/cygdrive/c/usr/local/lib/gcc-lib/i686-pc-cygwin/4.0.3
#
#

LIB		= -lf95

autobeale:	beale.o types.o pikaia.o ga_beale.o beale_data.o ga_driver.o \
		run_beale.o
		$(F95) -o beale.exe run_beale.o \
		ga_driver.o beale.o types.o pikaia.o ga_beale.o beale_data.o\
		$(LIBPATH) $(LIB) $(FFLAGS_F95)

run_beale.o:    run_beale.F90 types.o beale.o pikaia.o beale_data.o ga_driver.o
		$(F95) $(LIBPATH) $(FFLAGS_F95) -c run_beale.F90 -o run_beale.o

ga_driver.o:    ga_driver.F90 types.o beale.o pikaia.o beale_data.o
		$(F95) $(LIBPATH) $(FFLAGS_F95) -c ga_driver.F90 -o ga_driver.o

types.o:	types.F90
		$(F95) $(LIBPATH) $(FFLAGS_F95) -c types.F90 -o types.o

pikaia.o:	pikaia.F90 beale_data.o
		$(F95) $(LIBPATH) $(FFLAGS_F95) -c pikaia.F90 -o pikaia.o

beale.o:	beale.F90 types.o
		$(F95) $(LIBPATH) $(FFLAGS) -c beale.F90 -o beale.o

beale_data.o:	beale_data.F90 types.o
		$(F95) $(LIBPATH) $(FFLAGS) -c beale_data.F90 -o beale_data.o

ga_beale.o:	ga_beale.F90 beale.o pikaia.o types.o beale_data.o
		$(F95) $(LIBPATH) $(FFLAGS) -c ga_beale.F90 -o ga_beale.o

clean :
		rm *.o
		rm *.exe
		rm *.mod
