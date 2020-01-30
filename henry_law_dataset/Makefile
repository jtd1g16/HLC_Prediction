##############################################################################

compiler = g95
#compiler = gfortran
#compiler = lahey

##############################################################################

ifeq ($(compiler),g95)
  ### gnu compiler www.g95.org
  #F90        = g95
  F90  = /soft/g95/v0.92/bin/i686-unknown-linux-gnu-g95
  ### Choose compiler options:
  ### -Wall           = Enable most warning messages
  ### -cpp            = run the C preprocessor
  ### -O<n>           = optimization level <n>=0...3, zero = no optimization
  ### -O              = same as -O1
  ### -g              = debug option
  ### -fbounds-check  = check array and substring bounds at runtime
  ### -fimplicit-none = IMPLICIT NONE, unless overridden by IMPLICIT statements
  ### -ftrace=full    = show line number when arithmetic exceptions occur
  ### -fno-backslash  = do not interpret backslashes in strings as escape codes
  ### -r8             = double precision
  #F90FLAGS  = -cpp -fno-backslash
  #F90FLAGS = -cpp -O0 -pg -fbounds-check -fimplicit-none -Wall -ftrace=full -fno-backslash
  F90FLAGS = -cpp -r8 -O0 -g -fbounds-check -fimplicit-none -ftrace=full -fno-backslash
endif

# ----------------------------------------------------------------------------

ifeq ($(compiler),gfortran)
  ### gnu compiler: http://gcc.gnu.org/fortran/index.html
  ### see compiler options: gfortran --help -v
  ### gfortran settings from Hella Riede (2013)
  ### don't use '-x f95-cpp-input' in F90FLAGS -> linker error:
  ###   not a bug, see http://gcc.gnu.org/bugzilla/show_bug.cgi?id=29497
  F90      = gfortran
  ### for production: -O3: optimize, -cpp: enable pre-processing,
  ###   -ffree-line-length-none: no line width limit
  #F90FLAGS = -O3 -cpp -ffree-line-length-none
  ### for debug: -O0: no optimization, -Wall: all error messages,
  ###   -fbounds-check: check bounds before indexing arrays
  F90FLAGS = -O0 -cpp -fbounds-check -Wall
  ### for debug: do not compile, check syntax
  #F90FLAGS = -cpp -fsyntax-only -fdump-parse-tree 
  ### other flags to try while troubleshooting:
  ###   -std=legacy, -fimplicit-none, -finit-local-zero, -fall-intrinsics,
  ###   -ffpe-trap=invalid,zero,overflow,underflow,denormal
endif

# ----------------------------------------------------------------------------

ifeq ($(compiler),lahey)
  ### lf95 = Lahey compiler
  F90        = lf95
  #F90FLAGS  = -Cpp --pca
  F90FLAGS   = -Cpp --dbl --chk a,e,s,u --pca --ap -O0 -g --trap
endif

##############################################################################

### name of the executable that will be produced
PROG = henry.exe

# complete list of all f90 source files
SRCS = $(wildcard *.f90)

# the object files are the same as the source files but with suffix ".o"
OBJS := $(SRCS:.f90=.o)

MAKEFILE_INC = depend.mk

# If you don't have the perl script sfmakedepend, get it from:
# http://www.arsc.edu/~kate/Perl
F_makedepend = ./sfmakedepend --file=$(MAKEFILE_INC)

all: $(PROG)

# the dependencies depend on the link
# the executable depends on depend and also on all objects
# the executable is created by linking all objects
$(PROG): depend $(OBJS)
	$(F90) $(F90FLAGS) $(OBJS) -o $@

# update file dependencies
depend $(MAKEFILE_INC): $(SRCS)
	$(F_makedepend) $(SRCS)

# this command is executed by "gmake clean"
clean:
	rm -f $(OBJS) *.mod *.log gmon.out TAGS *~

# all object files *.o depend on their source files *.f90
# the object files are created with the "-c" compiler option
%.o: %.f90
	$(F90) $(F90FLAGS) -c $<

# list of dependencies (via USE statements)
include depend.mk
