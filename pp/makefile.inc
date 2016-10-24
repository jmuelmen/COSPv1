NETCDFROOT       = $(HOME)/netcdf-4.4.3-intel14
NETCDF_LIB       = -L$(NETCDFROOT)/lib -lnetcdf -lnetcdff
NETCDF_INCLUDE   = -I$(NETCDFROOT)/include

LIB      = -L../lib  -lmass -lmassvp6
LIBS     = $(NETCDF_LIB) 


INCLUDE  = ../include
INCLUDES =   $(NETCDF_INCLUDE)

F90      = ifort

