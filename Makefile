.SUFFIXES : .F .f .c .o .a .f90 .f95 .F90
########################################################################
#              Adapt these variables to your environment
########################################################################
F90      = ifort
#F90FLAGS = -C -check -fpp
#F90FLAGS = -check bounds
#F90FLAGS = -pg
F90FLAGS = -O0 -g
NCDF_INC = $(HOME)/netcdf-4.4.3-intel14/include
NCDF_LIB = $(HOME)/netcdf-4.4.3-intel14/lib

CMOR_INC = $(HOME)/cmor/include
CMOR_LIB = $(HOME)/cmor/lib

INC = /data/cr2/hadac/software/include
LIB = /data/cr2/hadac/software/lib

UDUNITS_LIB = $(HOME)/udunits/lib
UDUNITS_INC = $(HOME)/udunits/include

UUID_LIB = $(HOME)/uuid/lib
UUID_INC = $(HOME)/uuid/include


# Non-optional simulators. You should not need to change this
RS_PATH = quickbeam
CS_PATH = actsim
LLNL_PATH = llnl
ISCCP_PATH = icarus-scops-4.1-bsd
MISR_PATH = MISR_simulator
MODIS_PATH = MODIS_simulator
# RTTOV variables. You may need to change this
RTTOV_PATH     = /data/cr2/hadac/software/rttov
RTTOV_LIB_PATH = $(RTTOV_PATH)/rttov92.$(F90)/lib 
RTTOV_INC_PATH = $(RTTOV_PATH)/rttov92.$(F90)/include 
RTTOV_MOD_PATH = $(RTTOV_PATH)/rttov92.$(F90)/mod 
########################################################################
#              End of modifications
########################################################################

PROG =  cosp_test
OBJS =  cosp_radar.o cosp_types.o cosp_constants.o cosp_simulator.o \
        cosp_utils.o scops.o prec_scops.o cosp.o cosp_stats.o \
        pf_to_mr.o \
        cosp_lidar.o radar_simulator_types.o zeff.o \
        array_lib.o atmos_lib.o dsd.o format_input.o \
        gases.o load_hydrometeor_classes.o \
        math_lib.o mrgrnk.o optics_lib.o radar_simulator.o \
        lidar_simulator.o cosp_io.o llnl_stats.o lmd_ipsl_stats.o \
        cosp_isccp_simulator.o icarus.o \
        cosp_misr_simulator.o MISR_simulator.o \
        cosp_modis_simulator.o modis_simulator.o \
        cosp_rttov_simulator.o

all: $(PROG)


$(PROG): $(OBJS)
	$(F90) $(F90FLAGS) $(PROG).F90 $(OBJS) \
	-L${CMOR_LIB} -L. -lcmor -I$(CMOR_INC) \
	-I$(NCDF_INC) -L${NCDF_LIB} -lnetcdff \
	-L${UDUNITS_LIB} -Wl,-rpath=${UDUNITS_LIB} -ludunits2 -lexpat -I${UDUNITS_INC} \
	-L${UUID_LIB} -Wl,-rpath=${UUID_LIB} -luuid -I$(UUID_INC) \
	-o $(PROG)

cmor1: $(OBJS)
	$(F90) $(F90FLAGS) $(PROG).F90 $(OBJS) -I$(CMOR_INC) -L${CMOR_LIB} -lcmor \
	-I$(NCDF_INC) -L${NCDF_LIB} -lnetcdf -o $(PROG)
 
rttov: $(OBJS) cosp_rttov.o
	$(F90) $(F90FLAGS) $(PROG).F90 $(OBJS) cosp_rttov.o \
	-I$(NCDF_INC) -L${NCDF_LIB} -lnetcdff -lnetcdf  \
	-I$(INC) -L${LIB} -lsz \
	-I$(CMOR_INC) -L${CMOR_LIB} -lcmor \
	-I$(UUID_INC) -L${UUID_LIB} -luuid \
	-L${UDUNITS_LIB} -ludunits2 -lexpat \
	-L${RTTOV_LIB_PATH} -lrttov9.1 \
	-o $(PROG)
        
%.o: %.f90
	@echo $(F90) $(F90FLAGS) -c -I$(NCDF_INC) -I$(CMOR_INC) $<
	$(F90) $(F90FLAGS) -c -I$(NCDF_INC) -I$(CMOR_INC) $<
	@echo "-----------------------------"

%.o: %.F90
	@echo $(F90) $(F90FLAGS) -c -I$(NCDF_INC) -I$(CMOR_INC) $<
	$(F90) $(F90FLAGS) -c -I$(NCDF_INC) -I$(CMOR_INC) $<
	@echo "-----------------------------"

$(PROG).o     : cosp_constants.o cosp_types.o cosp.o cosp_io.o
cosp_io.o       : cosp_constants.o cosp_types.o cosp_modis_simulator.o
cosp.o          : cosp_simulator.o cosp_types.o cosp_modis_simulator.o
cosp_lidar.o    : cosp_constants.o cosp_types.o
cosp_radar.o    : cosp_constants.o cosp_types.o radar_simulator_types.o \
	              array_lib.o atmos_lib.o format_input.o math_lib.o optics_lib.o
cosp_simulator.o: cosp_types.o cosp_radar.o cosp_lidar.o \
                  cosp_isccp_simulator.o cosp_misr_simulator.o \
                  cosp_modis_simulator.o cosp_rttov_simulator.o cosp_stats.o
cosp_stats.o    : cosp_constants.o cosp_types.o llnl_stats.o lmd_ipsl_stats.o
cosp_types.o    : cosp_constants.o cosp_utils.o radar_simulator_types.o
cosp_utils.o    : cosp_constants.o
lmd_ipsl_stats.o : llnl_stats.o
array_lib.o    : mrgrnk.o
dsd.o          : array_lib.o math_lib.o
format_input.o : array_lib.o
load_hydrometeor_classes.o: radar_simulator_types.o
math_lib.o                : array_lib.o mrgrnk.o
radar_simulator.o         : array_lib.o math_lib.o mrgrnk.o optics_lib.o \
	                        radar_simulator_types.o
zeff.o                    : math_lib.o optics_lib.o
cosp_isccp_simulator.o    : cosp_constants.o cosp_types.o
cosp_misr_simulator.o     : cosp_constants.o cosp_types.o
cosp_modis_simulator.o    : cosp_constants.o cosp_types.o modis_simulator.o
cosp_rttov_simulator.o    : cosp_constants.o cosp_types.o

clean_objs:
	rm -f $(OBJS) *.mod *.o

clean:
	rm -f $(PROG) $(OBJS) *.mod *.o fort.*

scops.o : $(ISCCP_PATH)/scops.f
	$(F90) $(F90FLAGS) -c -I$(ISCCP_PATH) $<

icarus.o : $(ISCCP_PATH)/icarus.f
	$(F90) $(F90FLAGS) -c $<

prec_scops.o : $(LLNL_PATH)/prec_scops.f
	$(F90) $(F90FLAGS) -c $<

pf_to_mr.o : $(LLNL_PATH)/pf_to_mr.f
	$(F90) $(F90FLAGS) -c $<

radar_simulator_types.o : $(RS_PATH)/radar_simulator_types.f90
	$(F90) $(F90FLAGS) -c $<

atmos_lib.o : $(RS_PATH)/atmos_lib.f90
	$(F90) $(F90FLAGS) -c $<

zeff.o : $(RS_PATH)/zeff.f90
	$(F90) $(F90FLAGS) -c $<

array_lib.o : $(RS_PATH)/array_lib.f90
	$(F90) $(F90FLAGS) -c $<

dsd.o : $(RS_PATH)/dsd.f90
	$(F90) $(F90FLAGS) -c $<

format_input.o : $(RS_PATH)/format_input.f90
	$(F90) $(F90FLAGS) -c $<

gases.o : $(RS_PATH)/gases.f90
	$(F90) $(F90FLAGS) -c $<

load_hydrometeor_classes.o : $(RS_PATH)/load_hydrometeor_classes.f90
	$(F90) $(F90FLAGS) -c $<

math_lib.o : $(RS_PATH)/math_lib.f90
	$(F90) $(F90FLAGS) -c $<

mrgrnk.o : $(RS_PATH)/mrgrnk.f90
	$(F90) $(F90FLAGS) -c $<

optics_lib.o : $(RS_PATH)/optics_lib.f90
	$(F90) $(F90FLAGS) -c $<

radar_simulator.o : $(RS_PATH)/radar_simulator.f90
	$(F90) $(F90FLAGS) -c $<

lidar_simulator.o : $(CS_PATH)/lidar_simulator.F90
	$(F90) $(F90FLAGS) -c $<

lmd_ipsl_stats.o : $(CS_PATH)/lmd_ipsl_stats.F90
	$(F90) $(F90FLAGS) -c $<

llnl_stats.o : $(LLNL_PATH)/llnl_stats.F90
	$(F90) $(F90FLAGS) -c $<

cosp_radar.o : $(LLNL_PATH)/cosp_radar.F90
	$(F90) $(F90FLAGS) -c $<

MISR_simulator.o : $(MISR_PATH)/MISR_simulator.f
	$(F90) $(F90FLAGS) -c $<

modis_simulator.o : $(MODIS_PATH)/modis_simulator.F90 
	$(F90) $(F90FLAGS) -c $<

cosp_rttov.o : cosp_rttov.F90 
	$(F90) $(F90FLAGS) -c -I $(RTTOV_INC_PATH) -I $(RTTOV_MOD_PATH) $<
