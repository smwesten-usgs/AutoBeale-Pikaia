module genetic_algorithm

! Code converted using TO_F90 by Alan Miller
! Date: 2008-02-20  Time: 12:19:19

use types
IMPLICIT NONE

INTEGER, SAVE  :: iseed

type, public :: T_ORGANISM

  real(kind=T_REAL), dimension(:),pointer :: rPhenotype
  integer (kind=T_INT), dimension(:),pointer :: iGenotype

  real (kind=T_REAL) :: rFitnessValue
  integer (kind=T_INT) :: iFitnessRank
  logical (kind=T_LOGICAL) :: lLegalParameterSpace

end type T_ORGANISM

type, public :: T_POPULATION

  integer :: iPopulationSize
  integer :: iNumGenotypeDigits

  type(T_ORGANISM), dimension(:),pointer :: pOrganism

end type T_POPULATION


contains

SUBROUTINE pikaia(ff,n,ctrl,x,f,STATUS)
!=======================================================================
!     Optimization (maximization) of user-supplied "fitness" function
!     ff  over n-dimensional parameter space  x  using a basic genetic
!     algorithm method.

!     Paul Charbonneau & Barry Knapp
!     High Altitude Observatory
!     National Center for Atmospheric Research
!     Boulder CO 80307-3000
!     <paulchar@hao.ucar.edu>
!     <knapp@hao.ucar.edu>

!     Version 1.2   [ 2002 April 3 ]

!     Genetic algorithms are heuristic search techniques that
!     incorporate in a computational setting, the biological notion
!     of evolution by means of natural selection.  This subroutine
!     implements the three basic operations of selection, crossover,
!     and mutation, operating on "genotypes" encoded as strings.

!     Version 1.2 differs from version 1.0 (December 1995) in that
!     it includes (1) two-point crossover, (2) creep mutation, and
!     (3) dynamical adjustment of the mutation rate based on metric
!     distance in parameter space.

!     References:

!        Charbonneau, Paul. "An introduction to gemetic algorithms for
!           numerical optimization", NCAR Technical Note TN-450+IA
!           (April 2002)

!        Charbonneau, Paul. "Release Notes for PIKAIA 1.2",
!           NCAR Technical Note TN-451+STR (April 2002)

!        Charbonneau, Paul, and Knapp, Barry. "A User's Guide
!           to PIKAIA 1.0" NCAR Technical Note TN-418+IA
!           (December 1995)

!        Goldberg, David E.  Genetic Algorithms in Search, Optimization,
!           & Machine Learning.  Addison-Wesley, 1989.

!        Davis, Lawrence, ed.  Handbook of Genetic Algorithms.
!           Van Nostrand Reinhold, 1991.

!=======================================================================
!     USES: ff, urand, setctl, report, rnkpop, select, encode, decode,
!           cross, mutate, genrep, stdrep, newpop, adjmut

use types
use beale_data
use beale
IMPLICIT NONE


INTEGER, INTENT(IN)                      :: n
REAL (kind=T_REAL), INTENT(IN OUT)       :: ctrl(12)
REAL (kind=T_REAL), INTENT(OUT)          :: x(n)
REAL (kind=T_REAL), INTENT(OUT)          :: f
INTEGER, INTENT(OUT)                     :: STATUS

integer (kind=T_INT) :: iBeginTime, iCurrTime

integer (kind=T_INT) :: iStartDate, iEndDate

INTERFACE
  FUNCTION ff(n, x) RESULT(fn_val)
    use types
    IMPLICIT NONE
    INTEGER, INTENT(IN)  :: n
    REAL (kind=T_REAL), INTENT(IN)     :: x(:)
    REAL (kind=T_REAL)                 :: fn_val
  END FUNCTION ff
END INTERFACE

!     Input:


!      o Integer  n  is the parameter space dimension, i.e., the number
!        of adjustable parameters.

!      o Function  ff  is a user-supplied scalar function of n vari-
!        ables, which must have the calling sequence f = ff(n,x), where
!        x is a REAL (kind=T_REAL) parameter array of length n.  This function must
!        be written so as to bound all parameters to the interval [0,1];
!        that is, the user must determine a priori bounds for the para-
!        meter space, and ff must use these bounds to perform the appro-
!        priate scalings to recover true parameter values in the
!        a priori ranges.

!        By convention, ff should return higher values for more optimal
!        parameter values (i.e., individuals which are more "fit").
!        For example, in fitting a function through data points, ff
!        could return the inverse of chi**2.

!        In most cases initialization code will have to be written
!        (either in a driver or in a separate subroutine) which loads
!        in data values and communicates with ff via one or more labeled
!        common blocks.  An example exercise driver and fitness function
!        are provided in the accompanying file, xpkaia.f.


!      Input/Output:


!      o Array  ctrl  is an array of control flags and parameters, to
!        control the genetic behavior of the algorithm, and also printed
!        output.  A default value will be used for any control variable
!        which is supplied with a value less than zero.  On exit, ctrl
!        contains the actual values used as control variables.  The
!        elements of ctrl and their defaults are:

!           ctrl( 1) - number of individuals in a population (default
!                      is 100)
!           ctrl( 2) - number of generations over which solution is
!                      to evolve (default is 500)
!           ctrl( 3) - number of significant digits (i.e., number of
!                      genes) retained in chromosomal encoding (default
!                      is 6)  (Note: This number is limited by the
!                      machine floating point precision.  Most 32-bit
!                      floating point representations have only 6 full
!                      digits of precision.  To achieve greater preci-
!                      sion this routine could be converted to double
!                      precision, but note that this would also require
!                      a double precision random number generator, which
!                      likely would not have more than 9 digits of
!                      precision if it used 4-byte integers internally.)
!           ctrl( 4) - crossover probability; must be  <= 1.0 (default
!                      is 0.85). If crossover takes place, either one
!                      or two splicing points are used, with equal
!                      probabilities
!           ctrl( 5) - mutation mode; 1/2/3/4/5 (default is 2)
!                      1=one-point mutation, fixed rate
!                      2=one-point, adjustable rate based on fitness
!                      3=one-point, adjustable rate based on distance
!                      4=one-point+creep, fixed rate
!                      5=one-point+creep, adjustable rate based on fitness
!                      6=one-point+creep, adjustable rate based on distance
!           ctrl( 6) - initial mutation rate; should be small (default
!                      is 0.005) (Note: the mutation rate is the proba-
!                      bility that any one gene locus will mutate in
!                      any one generation.)
!           ctrl( 7) - minimum mutation rate; must be >= 0.0 (default
!                      is 0.0005)
!           ctrl( 8) - maximum mutation rate; must be <= 1.0 (default
!                      is 0.25)
!           ctrl( 9) - relative fitness differential; range from 0
!                      (none) to 1 (maximum).  (default is 1.)
!           ctrl(10) - reproduction plan; 1/2/3=Full generational
!                      replacement/Steady-state-replace-random/Steady-
!                      state-replace-worst (default is 3)
!           ctrl(11) - elitism flag; 0/1=off/on (default is 0)
!                      (Applies only to reproduction plans 1 and 2)
!           ctrl(12) - printed output 0/1/2=None/Minimal/Verbose
!                      (default is 0)


!     Output:



!      o Array  x(1:n)  is the "fittest" (optimal) solution found,
!         i.e., the solution which maximizes fitness function ff

!      o Scalar  f  is the value of the fitness function at x

!      o Integer  status  is an indicator of the success or failure
!         of the call to pikaia (0=success; non-zero=failure)


!     Constants

INTEGER, PARAMETER :: nmax = 32
INTEGER, PARAMETER :: pmax = 300
INTEGER, PARAMETER :: dmax = 8
integer (kind=T_INT), parameter :: iPikaiaMaxSeconds = 75

!      o NMAX is the maximum number of adjustable parameters
!        (n <= NMAX)

!      o PMAX is the maximum population (ctrl(1) <= PMAX)

!      o DMAX is the maximum number of Genes (digits) per Chromosome
!        segement (parameter) (ctrl(3) <= DMAX)


!     Local variables
INTEGER :: np, nd, ngen, imut, irep, ielite, ivrb, k, ip, ig,  &
    ip1, ip2, NEW, newtot
REAL (kind=T_REAL) :: pcross, pmut, pmutmn, pmutmx, fdif

REAL (kind=T_REAL) :: ph(nmax,2), oldph(nmax,pmax), newph(nmax,pmax)

INTEGER :: gn1(nmax*dmax), gn2(nmax*dmax)
INTEGER :: ifit(pmax), jfit(pmax)
REAL (kind=T_REAL) :: fitns(pmax)

real (kind=T_REAL), dimension(n) :: rX_Val

real (kind=T_REAL) :: rFit1, rFit2, rBestFit

integer (kind=T_INT) :: iIterationCount, iFitnessCount

!     Function urand should not take any arguments.  If the user wishes
!     to be able to initialize urand, so that the same sequence of
!     random numbers can be repeated, this capability could be imple-
!     mented with a separate subroutine, and called from the user's
!     driver program.  An example urand function (and initialization
!     subroutine) which uses the function ran0 (the "minimal standard"
!     random number generator of Park and Miller [Comm. ACM 31, 1192-
!     1201, Oct 1988; Comm. ACM 36 No. 7, 105-110, July 1993]) is
!     provided.


!     Set control variables from input and defaults
CALL setctl (ctrl,n,np,ngen,nd,pcross,pmutmn,pmutmx,pmut,imut,  &
    fdif,irep,ielite,ivrb,STATUS)
IF (STATUS /= 0) THEN
  WRITE(*,*) ' Control vector (ctrl) argument(s) invalid'
  RETURN
END IF

!     Make sure locally-dimensioned arrays are big enough
IF (n > nmax .OR. np > pmax .OR. nd > dmax) THEN
  WRITE(*,*) ' Number of parameters, population, or genes too large'
  STATUS = -1
  RETURN
END IF

iBeginTime = SECNDS(0.)

rBestFit = rZERO

call find_initial_strata(pConfig,pConc, n, rX_Val)

!     Compute initial (random but bounded) phenotypes
DO  ip=1,30   ! loop over population
  DO  k=1,n      ! loop over parameters
!    oldph(k,ip)=urand()
     oldph(k,ip) = rX_Val(k)  + (urand()-0.5_T_REAL)/10_T_REAL
     oldph(k,ip) = MIN(oldph(k,ip),1.)
     oldph(k,ip) = MAX(oldph(k,ip),0.)

  END DO

! seed one set of equally-spaced parameters
!  DO  k=1,n      ! loop over parameters
!    oldph(k,np)= k /(n+1.)
!  END DO


!  fitns(ip) = ff(n,oldph(1,ip))
  fitns(ip) = ff(n,oldph(:,ip))
END DO

DO  ip=31,np   ! loop over population
  DO  k=1,n      ! loop over parameters
    oldph(k,ip)=urand()

  END DO

! seed one set of equally-spaced parameters
!  DO  k=1,n      ! loop over parameters
!    oldph(k,np)= k /(n+1.)
!  END DO


!  fitns(ip) = ff(n,oldph(1,ip))
  fitns(ip) = ff(n,oldph(:,ip))
END DO

!     Rank initial population by fitness order
CALL rnkpop(np,fitns,ifit,jfit)

iFitnessCount = 0

!     Main Generation Loop
DO  ig=1,ngen

  iFitnessCount = iFitnessCount + 1

!        Main Population Loop
  newtot=0
  DO  ip=1,np/2

    iIterationCount = 0

    do
!           1. pick two parents

      iIterationCount = iIterationCount + 1

      do
        CALL select(np,jfit,fdif,ip1)
        CALL select(np,jfit,fdif,ip2)
        IF (ip1 /= ip2) exit
      end do

!           2. encode parent phenotypes
      CALL encode(n,nd,oldph(:,ip1),gn1)
      CALL encode(n,nd,oldph(:,ip2),gn2)

!           3. breed
      CALL cross(n,nd,pcross,gn1,gn2)
      CALL mutate(n,nd,pmut,gn1,imut)
      CALL mutate(n,nd,pmut,gn2,imut)

!           4. decode offspring genotypes
      CALL decode(n,nd,gn1,ph(:,1))
      CALL decode(n,nd,gn2,ph(:,2))

      rFit1=ff(n,ph(:,1))
      rFit2=ff(n,ph(:,2))

!      if( (.not.    (lDup1 .or. lDup2)) &
       if( rFit1>=rZERO &
            .and. rFit2>=rZERO ) exit       ! otherwise we've stumbled
                                            ! onto an illegal combination
                                            ! of dates
      if(iIterationCount > 15) then     ! give up...switch to known legal
                                        ! parameter values
        DO  k=1,n      ! loop over parameters
          ph(k,1) = rX_Val(k) + (urand()-0.5_T_REAL)/10.
          ph(k,1) = MIN(ph(k,1),1.)
          ph(k,1) = MAX(ph(k,1),0.)
          ph(k,2) = rX_Val(k)
        END DO

        exit

      end if

    end do

!           5. insert into population
    IF (irep == 1) THEN
      CALL genrep(nmax,n,np,ip,ph,newph)
    ELSE
      CALL stdrep(ff,nmax,n,np,irep,ielite, ph,oldph,fitns,ifit,jfit,NEW)
      newtot = newtot+NEW
    END IF

!        End of Main Population Loop
  END DO

!        if running full generational replacement: swap populations
  IF (irep == 1) CALL newpop(ff,ielite,nmax,n,np,oldph,newph,  &
      ifit,jfit,fitns,newtot)

!        adjust mutation rate?
  IF (imut == 2 .OR. imut == 3 .OR. imut == 5 .OR. imut == 6)  &
      CALL adjmut(nmax,n,np,oldph,fitns,ifit,pmutmn,pmutmx, pmut,imut)

  IF (ivrb > 0) CALL report  &
      (ivrb,nmax,n,np,nd,oldph,fitns,ifit,pmut,ig,newtot)

  iCurrTime = SECNDS(REAL(iBeginTime))
  if(iCurrTime>iPikaiaMaxSeconds) exit

  if(ABS(rBestFit - fitns(ifit(np)))>rNEAR_ZERO) then
    rBestFit = fitns(ifit(np))
    iFitnessCount = 0
  endif

  if(iFitnessCount > iPikaiaMaxIterations_w_NoChange) then
!    print *, 'No significant change...Bailing'
!    print *, rBestFit
!    print *, fitns(ifit(np))
    exit
  end if

!     End of Main Generation Loop
END DO

!     Return best phenotype and its fitness
DO  k=1,n
  x(k) = oldph(k,ifit(np))
END DO
f = fitns(ifit(np))

END SUBROUTINE pikaia
!********************************************************************

FUNCTION urand()  result(fn_val)
!=====================================================================
!     Return the next pseudo-random deviate from a sequence which is
!     uniformly distributed in the interval [0,1]

!     Uses the function ran0, the "minimal standard" random number
!     generator of Park and Miller (Comm. ACM 31, 1192-1201, Oct 1988;
!     Comm. ACM 36 No. 7, 105-110, July 1993).
!=====================================================================
use types
IMPLICIT NONE

real (kind=T_REAL) :: fn_val

fn_val = ran0( iseed )

RETURN
END FUNCTION urand
!*********************************************************************

SUBROUTINE rninit( seed )
!=====================================================================
!     Initialize random number generator urand with given seed
!=====================================================================
use types
IMPLICIT NONE

INTEGER, INTENT(IN)                      :: seed
!     Input


!     Output - none

!     Set the seed value
iseed = seed
IF(iseed <= 0) iseed=123456
RETURN
END SUBROUTINE rninit
!*********************************************************************

FUNCTION ran0( seed )
!=====================================================================
!     "Minimal standard" pseudo-random number generator of Park and
!     Miller.  Returns a uniform random deviate r s.t. 0 < r < 1.0.
!     Set seed to any non-zero integer value to initialize a sequence,
!     then do not change seed between calls for successive deviates
!     in the sequence.

!     References:
!        Park, S. and Miller, K., "Random Number Generators: Good Ones
!           are Hard to Find", Comm. ACM 31, 1192-1201 (Oct. 1988)
!        Park, S. and Miller, K., in "Remarks on Choosing and Imple-
!           menting Random Number Generators", Comm. ACM 36 No. 7,
!           105-110 (July 1993)
!=====================================================================
! *** Declaration section ***

use types
IMPLICIT NONE

INTEGER, INTENT(IN OUT)                  :: seed
!     Input/Output:


!     Output:
REAL (kind=T_REAL) :: ran0

!     Constants:

INTEGER, PARAMETER :: a=48271
INTEGER, PARAMETER :: m=2147483647
INTEGER, PARAMETER :: q=44488
INTEGER, PARAMETER :: r=3399

REAL (kind=T_REAL), PARAMETER :: scale=1./m
REAL (kind=T_REAL), PARAMETER :: eps=1.2E-7
REAL (kind=T_REAL), PARAMETER :: rnmx=1.-eps

!     Local:
INTEGER :: j

! *** Executable section ***

j = seed/q
seed = a*(seed-j*q)-r*j
IF (seed < 0) seed = seed+m
ran0 = MIN(seed*scale,rnmx)
RETURN
END FUNCTION ran0
!**********************************************************************

SUBROUTINE rqsort(n,a,p)
!======================================================================
!     Return integer array p which indexes array a in increasing order.
!     Array a is not disturbed.  The Quicksort algorithm is used.

!     B. G. Knapp, 86/12/23

!     Reference: N. Wirth, Algorithms and Data Structures,
!     Prentice-Hall, 1986
!======================================================================
use types
IMPLICIT NONE

INTEGER, INTENT(IN)                      :: n
REAL (kind=T_REAL), INTENT(IN)                         :: a(n)
INTEGER, INTENT(OUT)                     :: p(n)
!     Input:



!     Output:


!     Constants

INTEGER, PARAMETER :: lgn=32
INTEGER, PARAMETER :: q=11
!        (LGN = log base 2 of maximum n;
!         Q = smallest subfile to use quicksort on)

!     Local:
REAL (kind=T_REAL) :: x
INTEGER :: stackl(lgn),stackr(lgn),s,t,l,m,r,i,j

!     Initialize the stack
stackl(1)=1
stackr(1)=n
s=1

!     Initialize the pointer array
DO  i=1,n
  p(i)=i
END DO

2 IF (s > 0) THEN
  l=stackl(s)
  r=stackr(s)
  s=s-1

  3    IF ((r-l) < q) THEN

!           Use straight insertion
    DO  i=l+1,r
      t = p(i)
      x = a(t)
      DO  j=i-1,l,-1
        IF (a(p(j)) <= x) GO TO 5
        p(j+1) = p(j)
      END DO
      j=l-1
      5          p(j+1) = t
    END DO
  ELSE

!           Use quicksort, with pivot as median of a(l), a(m), a(r)
    m=(l+r)/2
    t=p(m)
    IF (a(t) < a(p(l))) THEN
      p(m)=p(l)
      p(l)=t
      t=p(m)
    END IF
    IF (a(t) > a(p(r))) THEN
      p(m)=p(r)
      p(r)=t
      t=p(m)
      IF (a(t) < a(p(l))) THEN
        p(m)=p(l)
        p(l)=t
        t=p(m)
      END IF
    END IF

!           Partition
    x=a(t)
    i=l+1
    j=r-1
    7       IF (i <= j) THEN
      8          IF (a(p(i)) < x) THEN
        i=i+1
        GO TO 8
      END IF
      9          IF (x < a(p(j))) THEN
        j=j-1
        GO TO 9
      END IF
      IF (i <= j) THEN
        t=p(i)
        p(i)=p(j)
        p(j)=t
        i=i+1
        j=j-1
      END IF
      GO TO 7
    END IF

!           Stack the larger subfile
    s=s+1
    IF ((j-l) > (r-i)) THEN
      stackl(s)=l
      stackr(s)=j
      l=i
    ELSE
      stackl(s)=i
      stackr(s)=r
      r=j
    END IF
    GO TO 3
  END IF
  GO TO 2
END IF
RETURN
END SUBROUTINE rqsort
!***********************************************************************

SUBROUTINE setctl (ctrl,n,np,ngen,nd,pcross,pmutmn,pmutmx,pmut,imut,  &
    fdif,irep,ielite,ivrb,STATUS)
!===================================================================
!     Set control variables and flags from input and defaults
!===================================================================
use types
IMPLICIT NONE

!     Input
INTEGER :: n

!     Input/Output
REAL (kind=T_REAL) :: ctrl(12)

!     Output
INTEGER :: np, ngen, nd, imut, irep, ielite, ivrb, STATUS
REAL (kind=T_REAL) :: pcross, pmutmn, pmutmx, pmut, fdif

!     Local
INTEGER :: i
REAL (kind=T_REAL) :: dfault(12)
SAVE     dfault
DATA     dfault /100,500,5,.85,2,.005,.0005,.25,1,1,1,0/

DO  i=1,12
  IF (ctrl(i) < 0.) ctrl(i)=dfault(i)
END DO

np = ctrl(1)
ngen = ctrl(2)
nd = ctrl(3)
pcross = ctrl(4)
imut = ctrl(5)
pmut = ctrl(6)
pmutmn = ctrl(7)
pmutmx = ctrl(8)
fdif = ctrl(9)
irep = ctrl(10)
ielite = ctrl(11)
ivrb = ctrl(12)
STATUS = 0

!     Print a header
IF (ivrb > 0) THEN

  WRITE(*,2) ngen,np,n,nd,pcross,pmut,pmutmn,pmutmx,fdif
  2    FORMAT(/1X,60('*'),/,  &
      ' *',13X,'PIKAIA Genetic Algorithm Report ',13X,'*',/, 1X,60('*'),//,  &
      '   Number of Generations evolving: ',i4,/,  &
      '       Individuals per generation: ',i4,/,  &
      '    Number of Chromosome segments: ',i4,/,  &
      '    Length of Chromosome segments: ',i4,/,  &
      '            Crossover probability: ',f9.4,/,  &
      '            Initial mutation rate: ',f9.4,/,  &
      '            Minimum mutation rate: ',f9.4,/,  &
      '            Maximum mutation rate: ',f9.4,/,  &
      '    Relative fitness differential: ',f9.4)
  IF (imut == 1) WRITE(*,3) 'Uniform, Constant Rate'
  IF (imut == 2) WRITE(*,3) 'Uniform, Variable Rate (F)'
  IF (imut == 3) WRITE(*,3) 'Uniform, Variable Rate (D)'
  IF (imut == 4) WRITE(*,3) 'Uniform+Creep, Constant Rate'
  IF (imut == 5) WRITE(*,3) 'Uniform+Creep, Variable Rate (F)'
  IF (imut == 6) WRITE(*,3) 'Uniform+Creep, Variable Rate (D)'
  3    FORMAT( '                    Mutation Mode: ',a)
  IF (irep == 1) WRITE(*,4) 'Full generational replacement'
  IF (irep == 2) WRITE(*,4) 'Steady-state-replace-random'
  IF (irep == 3) WRITE(*,4) 'Steady-state-replace-worst'
  4    FORMAT( '                Reproduction Plan: ',a)
END IF

!     Check some control values
IF (imut /= 1 .AND. imut /= 2 .AND. imut /= 3 .AND. imut /= 4  &
      .AND. imut /= 5 .AND. imut /= 6) THEN
  WRITE(*,10)
  STATUS = 5
END IF
10 FORMAT(' ERROR: illegal value for imut (ctrl(5))')

IF (fdif > 1.) THEN
  WRITE(*,11)
  STATUS = 9
END IF
11 FORMAT(' ERROR: illegal value for fdif (ctrl(9))')

IF (irep /= 1 .AND. irep /= 2 .AND. irep /= 3) THEN
  WRITE(*,12)
  STATUS = 10
END IF
12 FORMAT(' ERROR: illegal value for irep (ctrl(10))')

IF (pcross > 1.0 .OR. pcross < 0.) THEN
  WRITE(*,13)
  STATUS = 4
END IF
13 FORMAT(' ERROR: illegal value for pcross (ctrl(4))')

IF (ielite /= 0 .AND. ielite /= 1) THEN
  WRITE(*,14)
  STATUS = 11
END IF
14 FORMAT(' ERROR: illegal value for ielite (ctrl(11))')

IF (irep == 1 .AND. imut == 1 .AND. pmut > 0.5 .AND. ielite == 0) THEN
  WRITE(*,15)
END IF
15 FORMAT(' WARNING: dangerously high value for pmut (ctrl(6));',  &
    /' (Should enforce elitism with ctrl(11)=1.)')

IF (irep == 1 .AND. imut == 2 .AND. pmutmx > 0.5 .AND. ielite == 0) THEN
  WRITE(*,16)
END IF
16 FORMAT(' WARNING: dangerously high value for pmutmx (ctrl(8));',  &
    /' (Should enforce elitism with ctrl(11)=1.)')

IF (fdif < 0.33 .AND. irep /= 3) THEN
  WRITE(*,17)
END IF
17 FORMAT(' WARNING: dangerously low value of fdif (ctrl(9))')

IF (MOD(np,2) > 0) THEN
  np=np-1
  WRITE(*,18) np
END IF
18 FORMAT(' WARNING: decreasing population size (ctrl(1)) to np=',i4)

RETURN
END SUBROUTINE setctl
!********************************************************************
SUBROUTINE report (ivrb,ndim,n,np,nd,oldph,fitns,ifit,pmut,ig,nnew)

!     Write generation report to standard output
use types
IMPLICIT NONE

!     Input:
INTEGER :: np,ifit(np),ivrb,ndim,n,nd,ig,nnew
REAL (kind=T_REAL) :: oldph(ndim,np),fitns(np),pmut

!     Output: none

!     Local
REAL (kind=T_REAL) :: bestft,pmutpv
SAVE bestft,pmutpv
INTEGER :: ndpwr,k, i
LOGICAL :: rpt
DATA bestft,pmutpv /0,0/

rpt=.false.

IF (pmut /= pmutpv) THEN
  pmutpv=pmut
  rpt=.true.
END IF

IF (fitns(ifit(np)) /= bestft) THEN
  bestft=fitns(ifit(np))
  rpt=.true.
END IF

IF (rpt .OR. ivrb >= 2) THEN

!        Power of 10 to make integer genotypes for display
  ndpwr = nint(10.**nd)

!  if(urand()>0.85) then

    DO  i=np,1,-1
        WRITE(LU_PIKAIA_OUT,FMT="(f16.3,1x,20(i10,1x))") &
          fitns(ifit(i)), (nint(ndpwr*oldph(k,ifit(i))),k=1,n)
    END DO

!  end if

  write(LU_PIKAIA_OUT,FMT=*) ' '

  if(nnew>0) then

    write(*,'(/a6,a6,a10,4(a16,1x))') 'Gen','New','PMUT','Best fitness',&
     '2nd best fitness','median fitness','worst fitness'

    WRITE(*,'(i6,i6,f10.6,4(f16.3,1x))') ig,nnew,pmut,  &
      fitns(ifit(np)), fitns(ifit(np-1)), fitns(ifit(np/2)),fitns(ifit(1))
    DO  k=1,n
      WRITE(*,'(i4,":",17x,4(i16,1x))') k,nint(ndpwr*oldph(k,ifit(np  ))),  &
        nint(ndpwr*oldph(k,ifit(np-1))), nint(ndpwr*oldph(k,ifit(np/2))), &
        nint(ndpwr*oldph(k,ifit(1)))
    END DO

  end if

END IF
END SUBROUTINE report

!**********************************************************************
!                         GENETICS MODULE
!**********************************************************************

!     ENCODE:    encodes phenotype into genotype
!                called by: PIKAIA

!     DECODE:    decodes genotype into phenotype
!                called by: PIKAIA

!     CROSS:     Breeds two offspring from two parents
!                called by: PIKAIA

!     MUTATE:    Introduces random mutation in a genotype
!                called by: PIKAIA

!     ADJMUT:    Implements variable mutation rate
!                called by: PIKAIA

!**********************************************************************
SUBROUTINE encode(n,nd,ph,gn)
!======================================================================
!     encode phenotype parameters into integer genotype
!     ph(k) are x,y coordinates [ 0 < x,y < 1 ]
!======================================================================
use types
IMPLICIT NONE

!     Inputs:
INTEGER :: n, nd
REAL (kind=T_REAL) :: ph(n)

!     Output:
INTEGER :: gn(n*nd)
logical (kind=T_LOGICAL) :: lDup

!     Local:
INTEGER :: ip, i, j, ii, iTempVal, iMaxCount, iCount
REAL (kind=T_REAL) :: z

integer, dimension(n) :: iPhenotype

lDup = lFALSE

z=10.**nd
ii=0
DO  i=1,n
  ip=INT(ph(i)*z)
  iPhenotype(i) = ip
  DO  j=nd,1,-1
    gn(ii+j)=MOD(ip,10)
    ip=ip/10
  END DO
  ii=ii+nd
END DO

iMaxCount = 0

do i=1,n

  iTempVal = iPhenotype(i)
  iCount = COUNT(iPhenotype==iTempVal)
  if(iCount > iMaxCount) then
    iMaxCount = iCount
  end if

end do

if(iMaxCount>1) then
  lDup = lTRUE
end if

RETURN
END SUBROUTINE encode

!**********************************************************************
SUBROUTINE decode(n,nd,gn,ph)
!======================================================================
!     decode genotype into phenotype parameters
!     ph(k) are x,y coordinates [ 0 < x,y < 1 ]
!======================================================================
use types
IMPLICIT NONE

!     Inputs:
INTEGER :: n, nd, gn(n*nd)

!     Output:
REAL (kind=T_REAL) :: ph(n)

!     Local:
INTEGER :: ip, i, j, ii, iMaxCount, iCount, iTempVal
REAL (kind=T_REAL) :: z

z=10.**(-nd)
ii=0
DO  i=1,n
  ip=0
  DO  j=1,nd
    ip=10*ip+gn(ii+j)
  END DO
  ph(i)=ip*z

  ii=ii+nd
END DO

RETURN
END SUBROUTINE decode

!**********************************************************************
SUBROUTINE cross(n,nd,pcross,gn1,gn2)
!======================================================================
!     breeds two parent chromosomes into two offspring chromosomes
!     breeding occurs through crossover. If the crossover probability
!     test yields true (crossover taking place), either one-point or
!     two-point crossover is used, with equal probabilities.

!     Compatibility with version 1.0: To enforce 100% use of one-point
!     crossover, un-comment appropriate line in source code below
!======================================================================
use types
IMPLICIT NONE

!     Inputs:
INTEGER :: n, nd
REAL (kind=T_REAL) :: pcross

!     Input/Output:
INTEGER :: gn1(n*nd), gn2(n*nd)

!     Local:
INTEGER :: i, ispl, ispl2, itmp, t

!     Use crossover probability to decide whether a crossover occurs
IF (urand() < pcross) THEN

!        Compute first crossover point
  ispl=INT(urand()*n*nd)+1

!        Now choose between one-point and two-point crossover
  IF (urand() < 0.5) THEN
    ispl2=n*nd
  ELSE
    ispl2=INT(urand()*n*nd)+1
!           Un-comment following line to enforce one-point crossover
!           ispl2=n*nd
    IF (ispl2 < ispl) THEN
      itmp=ispl2
      ispl2=ispl
      ispl=itmp
    END IF
  END IF

!        Swap genes from ispl to ispl2
  DO  i=ispl,ispl2
    t=gn2(i)
    gn2(i)=gn1(i)
    gn1(i)=t
  END DO
END IF

RETURN
END SUBROUTINE cross

!**********************************************************************
SUBROUTINE mutate(n,nd,pmut,gn,imut)
!======================================================================
!     Mutations occur at rate pmut at all gene loci
!        imut=1    Uniform mutation, constant rate
!        imut=2    Uniform mutation, variable rate based on fitness
!        imut=3    Uniform mutation, variable rate based on distance
!        imut=4    Uniform or creep mutation, constant rate
!        imut=5    Uniform or creep mutation, variable rate based on
!                  fitness
!        imut=6    Uniform or creep mutation, variable rate based on
!                  distance
!======================================================================
use types
IMPLICIT NONE

!     Input:
INTEGER :: n, nd, imut
REAL (kind=T_REAL) :: pmut

!     Input/Output:
INTEGER :: gn(n*nd)

!     Local:
INTEGER :: i,j,k,l,ist,inc,loc,kk

!     Decide which type of mutation is to occur
IF(imut >= 4.AND.urand() <= 0.5)THEN

!     CREEP MUTATION OPERATOR
!     Subject each locus to random +/- 1 increment at the rate pmut
  DO  i=1,n
    DO  j=1,nd
      IF (urand() < pmut) THEN
!     Construct integer
        loc=(i-1)*nd+j
        inc=nint ( urand() )*2-1
        ist=(i-1)*nd+1
        gn(loc)=gn(loc)+inc
!                 write(*,*) ist,loc,inc
!     This is where we carry over the one (up to two digits)
!     first take care of decrement below 0 case
        IF(inc < 0 .AND. gn(loc) < 0)THEN
          IF(j == 1)THEN
            gn(loc)=0
          ELSE
            DO  k=loc,ist+1,-1
              gn(k)=9
              gn(k-1)=gn(k-1)-1
              IF( gn(k-1) >= 0 )GO TO 4
            END DO
!    we popped under 0.00000 lower bound; fix it up
            IF( gn(ist) < 0.)THEN
              DO  l=ist,loc
                gn(l)=0
              END DO
            END IF
            4                   CONTINUE
          END IF
        END IF
        IF(inc > 0 .AND. gn(loc) > 9)THEN
          IF(j == 1)THEN
            gn(loc)=9
          ELSE
            DO  k=loc,ist+1,-1
              gn(k)=0
              gn(k-1)=gn(k-1)+1
              IF( gn(k-1) <= 9 )GO TO 7
            END DO
!                       we popped over 9.99999 upper bound; fix it up
            IF( gn(ist) > 9 )THEN
              DO  l=ist,loc
                gn(l)=9
              END DO
            END IF
            7                   CONTINUE
          END IF
        END IF
      END IF
    END DO
  END DO

ELSE

!     UNIFORM MUTATION OPERATOR
!     Subject each locus to random mutation at the rate pmut
  DO  i=1,n*nd
    IF (urand() < pmut) THEN
      gn(i)=INT(urand()*10.)
    END IF
  END DO
END IF

RETURN
END SUBROUTINE mutate

!**********************************************************************
SUBROUTINE adjmut(ndim,n,np,oldph,fitns,ifit,pmutmn,pmutmx, pmut,imut)
!======================================================================
!     dynamical adjustment of mutation rate;
!        imut=2 or imut=5 : adjustment based on fitness differential
!                           between best and median individuals
!        imut=3 or imut=6 : adjustment based on metric distance
!                           between best and median individuals
!======================================================================
use types
IMPLICIT NONE

!     Input:
INTEGER :: n, ndim, np, ifit(np), imut
REAL (kind=T_REAL) :: oldph(ndim,np), fitns(np), pmutmn, pmutmx

!     Input/Output:
REAL (kind=T_REAL) :: pmut

!     Local:
INTEGER :: i
REAL (kind=T_REAL) :: rdif, rdiflo, rdifhi, delta
PARAMETER      (rdiflo=0.05, rdifhi=0.25, delta=1.5)

IF(imut == 2.OR.imut == 5)THEN
!     Adjustment based on fitness differential
  rdif=ABS(fitns(ifit(np))-fitns(ifit(np/2)))/  &
      (fitns(ifit(np))+fitns(ifit(np/2)))
ELSE IF(imut == 3.OR.imut == 6)THEN
!     Adjustment based on normalized metric distance
  rdif=0.
  DO  i=1,n
    rdif=rdif+( oldph(i,ifit(np))-oldph(i,ifit(np/2)) )**2
  END DO
  rdif=SQRT( rdif ) / FLOAT(n)
END IF

IF(rdif <= rdiflo)THEN
  pmut=MIN(pmutmx,pmut*delta)
ELSE IF(rdif >= rdifhi)THEN
  pmut=MAX(pmutmn,pmut/delta)
END IF

RETURN
END SUBROUTINE adjmut


!**********************************************************************
!                       REPRODUCTION MODULE
!**********************************************************************

!     SELECT:   Parent selection by roulette wheel algorithm
!               called by: PIKAIA

!     RNKPOP:   Ranks initial population
!               called by: PIKAIA, NEWPOP

!     GENREP:   Inserts offspring into population, for full
!               generational replacement
!               called by: PIKAIA

!     STDREP:   Inserts offspring into population, for steady-state
!               reproduction
!               called by: PIKAIA
!               calls:     FF

!     NEWPOP:   Replaces old generation with new generation
!               called by: PIKAIA
!               calls:     FF, RNKPOP

!**********************************************************************
SUBROUTINE select(np,jfit,fdif,idad)
!======================================================================
!     Selects a parent from the population, using roulette wheel
!     algorithm with the relative fitnesses of the phenotypes as
!     the "hit" probabilities [see Davis 1991, chap. 1].
!======================================================================
!     USES: urand
use types
IMPLICIT NONE

!     Input:
INTEGER :: np, jfit(np)
REAL (kind=T_REAL) :: fdif

!     Output:
INTEGER :: idad

!     Local:
INTEGER :: np1, i
REAL (kind=T_REAL) :: dice, rtfit

np1 = np+1
dice = urand()*np*np1
rtfit = 0.
DO  i=1,np
  rtfit = rtfit+np1+fdif*(np1-2*jfit(i))
  IF (rtfit >= dice) THEN
    idad=i
    GO TO 2
  END IF
END DO

!     Assert: loop will never exit by falling through

stop "Yo! your logic failed!"

2 RETURN
END SUBROUTINE select

!**********************************************************************
SUBROUTINE rnkpop(n,arrin,indx,rank)
!======================================================================
!     Calls external sort routine to produce key index and rank order
!     of input array arrin (which is not altered).
!======================================================================
!     USES: rqsort
use types
IMPLICIT NONE

!     Input
INTEGER :: n
REAL (kind=T_REAL) :: arrin(n)

!     Output
INTEGER :: indx(n),rank(n)

!     Local
INTEGER :: i

!     Compute the key index
CALL rqsort(n,arrin,indx)

!     ...and the rank order
DO  i=1,n
  rank(indx(i)) = n-i+1
END DO
RETURN
END SUBROUTINE rnkpop

!***********************************************************************
SUBROUTINE genrep(ndim,n,np,ip,ph,newph)
!=======================================================================
!     full generational replacement: accumulate offspring into new
!     population array
!=======================================================================
use types
IMPLICIT NONE

!     Input:
INTEGER :: ndim, n, np, ip
REAL (kind=T_REAL) :: ph(ndim,2)

!     Output:
REAL (kind=T_REAL) :: newph(ndim,np)

!     Local:
INTEGER :: i1, i2, k


!     Insert one offspring pair into new population
i1=2*ip-1
i2=i1+1
DO  k=1,n
  newph(k,i1)=ph(k,1)
  newph(k,i2)=ph(k,2)
END DO

RETURN
END SUBROUTINE genrep

!**********************************************************************
SUBROUTINE stdrep (ff,ndim,n,np,irep,ielite,ph,oldph,fitns,ifit,jfit,nnew)
!======================================================================
!     steady-state reproduction: insert offspring pair into population
!     only if they are fit enough (replace-random if irep=2 or
!     replace-worst if irep=3).
!======================================================================
!     USES: ff, urand
use types
IMPLICIT NONE

!     Input:
INTEGER :: ndim, n, np, irep, ielite
REAL (kind=T_REAL) :: ph(ndim,2)

INTERFACE
  FUNCTION ff(n, x) RESULT(fn_val)
    use types
    IMPLICIT NONE
    INTEGER, INTENT(IN)  :: n    ! number of parameters
    real (kind=T_REAL), intent(in) :: x(:)
    real (kind=T_REAL) :: fn_val
  END FUNCTION ff
END INTERFACE

!     Input/Output:
REAL (kind=T_REAL) :: oldph(ndim,np), fitns(np)
INTEGER :: ifit(np), jfit(np)

!     Output:
INTEGER :: nnew

!     Local:
INTEGER :: i, j, k, i1, if1
REAL (kind=T_REAL) :: fit, fit2

integer (kind=T_INT) :: iCount

nnew = 0
loop1:  DO  j=1,2

!        1. compute offspring fitness (with caller's fitness function)

!  fit=ff(n,ph(1,j))

  fit=ff(n,ph(1:n,j))

!        2. if fit enough, insert in population
  DO  i=np,1,-1
    IF (fit > fitns(ifit(i))) THEN

!              make sure the phenotype is not already in the population

      IF (i < np) THEN
        DO  k=1,n
          IF (oldph(k,ifit(i+1)) /= ph(k,j)) GO TO 6
!          IF (ABS(oldph(k,ifit(i+1)) - ph(k,j))>rNEAR_ZERO) GO TO 6
        END DO
        CYCLE loop1
        6             CONTINUE
      END IF

!              offspring is fit enough for insertion, and is unique

!              (i) insert phenotype at appropriate place in population

      IF (irep == 3) THEN
        ! replace the individual with lowest fitness
        i1=1
      ELSE IF (ielite == 0 .OR. i == np) THEN
        i1=INT(urand()*np)+1
      ELSE
        i1=INT(urand()*(np-1))+1
      END IF
      if1 = ifit(i1)
      fitns(if1)=fit
      DO  k=1,n
        oldph(k,if1)=ph(k,j)
      END DO

!              (ii) shift and update ranking arrays
      IF (i < i1) THEN

!                 shift up
        jfit(if1)=np-i
        DO  k=i1-1,i+1,-1
          jfit(ifit(k))=jfit(ifit(k))-1
          ifit(k+1)=ifit(k)
        END DO
        ifit(i+1)=if1
      ELSE

!                 shift down
        jfit(if1)=np-i+1
        DO  k=i1+1,i
          jfit(ifit(k))=jfit(ifit(k))+1
          ifit(k-1)=ifit(k)
        END DO
        ifit(i)=if1
      END IF
      nnew = nnew+1
      CYCLE loop1
    END IF
  END DO

END DO loop1

RETURN
END SUBROUTINE stdrep

!**********************************************************************
SUBROUTINE newpop (ff,ielite,ndim,n,np,oldph,newph,ifit,jfit,fitns,nnew)
!======================================================================
!     replaces old population by new; recomputes fitnesses & ranks
!======================================================================
!     USES: ff, rnkpop
use types
IMPLICIT NONE

!     Input:
INTEGER :: ndim, np, n, ielite

INTERFACE
  FUNCTION ff(n, x) RESULT(fn_val)
    use types
    IMPLICIT NONE
    INTEGER, INTENT(IN)  :: n
    real (kind=T_REAL), intent(in) :: x(:)
    real (kind=T_REAL) :: fn_val
  END FUNCTION ff
END INTERFACE

!     Input/Output:
REAL (kind=T_REAL) :: oldph(ndim,np), newph(ndim,np)

!     Output:
INTEGER :: ifit(np), jfit(np), nnew
REAL (kind=T_REAL) :: fitns(np)

!     Local:
INTEGER :: i, k


nnew = np

!     if using elitism, introduce in new population fittest of old
!     population (if greater than fitness of the individual it is
!     to replace)
IF (ielite == 1 .AND. ff(n,newph(:,1)) < fitns(ifit(np))) THEN
  DO  k=1,n
    newph(k,1)=oldph(k,ifit(np))
  END DO
  nnew = nnew-1
END IF

!     replace population
DO  i=1,np
  DO  k=1,n
    oldph(k,i)=newph(k,i)
  END DO

!        get fitness using caller's fitness function
  fitns(i)=ff(n,oldph(:,i))
END DO

!     compute new population fitness rank order
CALL rnkpop(np,fitns,ifit,jfit)

RETURN
END SUBROUTINE newpop

end module genetic_algorithm
