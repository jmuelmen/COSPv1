.SUFFIXES: .f90 .o .mod

RM=rm -f

include makefile.inc


NCFLAGS= $(INCLUDES)  $(LIBS)


MODULES	     =  mo_util.o

OBJS         =  pp.o


pp:	$(MODULES) $(OBJS)
		$(F90) -o $@  $(F90FLAGS) $(OBJS) $(MODULES) $(NCFLAGS)

clean:		
		$(RM)  *.o *.mod  pp *.lst


%.o: %.f90
	$(F90) $(F90FLAGS) $(NCFLAGS) -c $<


# DEPENDENCIES : only dependencies after this line
pp.o: mo_util.o
