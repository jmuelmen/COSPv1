NETCDFROOT       = /opt/netcdf/current
NETCDF_LIB       = -L$(NETCDFROOT)/lib -lnetcdf -lnetcdff
NETCDF_INCLUDE   = -I$(NETCDFROOT)/include

HDF5ROOT         = /opt/hdf5/current
HDF5_LIB         = -L$(HDF5ROOT)/lib -lhdf5_hl -lhdf5
HDF5_INCLUDE     = -I$(HDF5ROOT)/include

SZIPROOT         = /opt/szip/current
SZIP_LIB         = -L$(SZIPROOT)/lib -lsz
SZIP_INCLUDE     = -I$(SZIPROOT)/include

ZLIBROOT         = /opt/zlib/current
ZLIB_LIB         = -L$(ZLIBROOT)/lib -lz
ZLIB_INCLUDE     = -I$(ZLIBROOT)/include

LIB      = -L../lib  -lmass -lmassvp6
LIBS     = $(NETCDF_LIB) $(HDF5_LIB) $(ZLIB_LIB) $(SZIP_LIB)


INCLUDE  = ../include
INCLUDES =   $(NETCDF_INCLUDE) $(HDF5_INCLUDE) $(ZLIB_INCLUDE) 

F90      = gfortran

