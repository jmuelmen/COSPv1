#! /bin/ksh

#set -e
set -e #-x

cosppath=$PWD
cosp=$cosppath/cosp_test
prep_offl=$cosppath/prep_offl.ksh


expid="amip"

mon=01

#exp=simT63_12008_r2443offl/
#ipath=/work/bm0671/m300111/OUT/echam-dev-mns/$exp

ipath=$cosppath
outpath=$cosppath/out

if [[ ! -e $outpath ]]; then
mkdir -p $outpath
fi


# prepare ########################
set -A ifiles $ipath/echam6-ham_run.barat_T31_cospoff_1_test_200901.0*nc
oppath=$ipath/cospOffl_input
echo $oppath

echo ${ifiles[*]}

$prep_offl $oppath $cosppath "${ifiles[*]}" 


# run simulators, CMORize ##########################

wmode="replace"
ntim=1


if [[ ! -d $outpath ]]; then
 mkdir $outpath
fi
if [[ ! -d $outpath/run ]]; then
 mkdir $outpath/run
fi
if [[ ! -d $outpath/run/cmor ]]; then
 mkdir $outpath/run/cmor
fi
table=CMIP5_cf3hr
#table=COSP_table_1D
if [[ ! -e $outpath/run/cmor/$table ]]; then
cp $cosppath/cmor/$table $outpath/run/cmor
fi

cd $outpath/run

set -A ifiles $oppath/cf3hr_2009-${mon}*


#tfile=temp01.nc

################# FILE LOOP ##################
for fi in ${ifiles[*]}; do
#if [[ -e $tfile ]]; then
#rm -f $tfile
#fi
#ln -s $fi $tfile

filename=${fi##*/}
echo $filename
day=${filename:14:2}
hour=${filename:17:2}
echo $mon $day $hour

n=`ncks -m $fi | grep -in "Reffi dimension 0" | awk '{print $7}'| sed "s/,//"`



cat -> cosp_input_nl.txt << EOF
! original copyright: see cosp_input_nl.txt.org 
! Namelist that sets up the main COSP options
&COSP_INPUT
  CMOR_NL='./cmor/cosp_cmor_nl.txt', ! CMOR namelist
  NPOINTS=$n,! Number of gridpoints 153,27840,7081
  NPOINTS_IT=$n,! Max number of gridpoints to be processed in one iteration
  NCOLUMNS=20,  ! Number of subcolumns
  NLEVELS=19,   ! Number of model levels
  USE_VGRID=.true., ! Use fixed vertical grid for outputs? (if .true. then you need to define number of levels with Nlr)
  NLR=40,       ! Number of levels in statistical outputs (only used if USE_VGRID=.true.)
  CSAT_VGRID=.true., ! CloudSat vertical grid? (if .true. then the CloudSat standard grid is used for the outputs.
                     !  USE_VGRID needs also be .true.)
  DINPUT="$oppath/",  ! Directory where the input files are located. Useful when processing multiple files.
                ! Leave blank ('') if you are using the full path in FINPUT.
   FINPUT="$filename"
  ! FINPUT='test_mpi_in.nc'
  !FINPUT='cosp_input_um.nc', ! List input NetCDF files
  !FINPUT='cosp_input_um_2d','cosp_input_um_2d.nc','cosp_input_um_2d.nc','cosp_input_um_2d.nc', ! cosp_input_um_2d.nc NetCDF file with 2D inputs
  !----------------------------------------------------------------------------------
  !--------------- Inputs related to radar simulations
  !----------------------------------------------------------------------------------
  RADAR_FREQ=94.0, ! CloudSat radar frequency (GHz)
  SURFACE_RADAR=0, ! surface=1, spaceborne=0
  use_mie_tables=0,! use a precomputed lookup table? yes=1,no=0
  use_gas_abs=1,   ! include gaseous absorption? yes=1,no=0
  do_ray=0,        ! calculate/output Rayleigh refl=1, not=0
  melt_lay=0,      ! melting layer model off=0, on=1
  k2=-1,           ! |K|^2, -1=use frequency dependent default
  use_reff=.true., ! True if you want effective radius to be used by radar simulator (always used by lidar)
  use_precipitation_fluxes=.true.,  ! True if precipitation fluxes are input to the algorithm 
  !----------------------------------------------------------------------------------
  !---------------- Inputs related to lidar simulations
  !----------------------------------------------------------------------------------
  Nprmts_max_hydro=12, ! Max number of parameters for hydrometeor size distributions
  Naero=1,             ! Number of aerosol species (Not used)
  Nprmts_max_aero=1,   ! Max number of parameters for aerosol size distributions (Not used)
  lidar_ice_type=0,    ! Ice particle shape in lidar calculations (0=ice-spheres ; 1=ice-non-spherical)
  OVERLAP=3,   !  overlap type: 1=max, 2=rand, 3=max/rand
  !----------------------------------------------------------------------------------
  !---------------- Inputs related to ISCCP simulator
  !----------------------------------------------------------------------------------
  ISCCP_TOPHEIGHT=1,  !  1 = adjust top height using both a computed
                       !  infrared brightness temperature and the visible
                       !  optical depth to adjust cloud top pressure. Note
                       !  that this calculation is most appropriate to compare
                       !  to ISCCP data during sunlit hours.
                      !  2 = do not adjust top height, that is cloud top
                       !  pressure is the actual cloud top pressure
                       !  in the model
                      !  3 = adjust top height using only the computed
                       !  infrared brightness temperature. Note that this
                       !  calculation is most appropriate to compare to ISCCP
                       !  IR only algortihm (i.e. you can compare to nighttime
                       !  ISCCP data with this option)
  ISCCP_TOPHEIGHT_DIRECTION=2,   ! direction for finding atmosphere pressure level1
                                 ! with interpolated temperature equal to the radiance
                                 ! determined cloud-top temperature
                                 ! 1 = find the *lowest* altitude (highest pressure) level
                                 ! with interpolated temperature equal to the radiance
                                 ! determined cloud-top temperature
                                 ! 2 = find the *highest* altitude (lowest pressure) level
                                 ! with interpolated temperature equal to the radiance 
                                 ! determined cloud-top temperature. This is the 
                                 ! default value since V4.0 of the ISCCP simulator.
                                 ! ONLY APPLICABLE IF top_height EQUALS 1 or 3
  !----------------------------------------------------------------------------------
  !-------------- RTTOV inputs
  !----------------------------------------------------------------------------------
  Platform=1,    ! satellite platform
  Satellite=15,  ! satellite
  Instrument=0,  ! instrument
  Nchannels=8,   ! Number of channels to be computed
  Channels=1,3,5,6,8,10,11,13,        ! Channel numbers (please be sure that you supply Nchannels)
  Surfem=0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  ! Surface emissivity (please be sure that you supply Nchannels)
  ZenAng=50.0, ! Satellite Zenith Angle
  CO2=5.241e-04, ! Mixing ratios of trace gases
  CH4=9.139e-07,
  N2O=4.665e-07,
  CO=2.098e-07,
  wmode="$wmode",
  iday=$day,
  ihour=$hour,
  imon=$mon
/
EOF

cat -> cosp_output_nl.txt << EOF
! original copyright: see cosp_output_nl.txt.org 
! Namelist that sets up output-related variables. It controls 
! the instrument simulators to run and the list of variables 
! to be written to file
&COSP_OUTPUT
  ! Simulator flags
  Lradar_sim=.true.,
  Llidar_sim=.true.,
  Lisccp_sim=.false.,
  Lmisr_sim=.false.,
  Lmodis_sim=.false.,
  Lrttov_sim=.false.,
  ! Output variables
  !- Variables unique in 1D (curtain) mode
  Ltoffset=.true.,
  !- CloudSat
  Lcfaddbze94=.true.,
  Ldbze94=.false.,
  !- CALIPSO
  Latb532=.false.,
  LcfadLidarsr532=.true.,
  Lclcalipso=.true.,
  Lclhcalipso=.true.,
  Lcllcalipso=.true.,
  Lclmcalipso=.true.,
  Lcltcalipso=.true.,
  LparasolRefl=.true.,
  !- ISCCP
  Lalbisccp=.true.,
  Lboxptopisccp=.true.,
  Lboxtauisccp=.true.,
  Lpctisccp=.true.,
  Lclisccp=.true.,
  Ltauisccp=.true.,
  Lcltisccp=.true.,
  Lmeantbisccp=.true.,
  Lmeantbclrisccp=.true.,
  !- MISR
  LclMISR=.true.,
  !- Use lidar and radar
  Lclcalipso2=.true.,
  Lcltlidarradar=.false.,
  !- These are provided for debugging or special purposes
  Lfracout=.false.,
  LlidarBetaMol532=.false.,  
  !- MODIS
  Lcltmodis=.false.,
  Lclwmodis=.false.,
  Lclimodis=.false.,
  Lclhmodis=.false.,
  Lclmmodis=.false.,
  Lcllmodis=.false.,
  Ltautmodis=.false.,
  Ltauwmodis=.false.,
  Ltauimodis=.false.,
  Ltautlogmodis=.false.,
  Ltauwlogmodis=.false.,
  Ltauilogmodis=.false.,
  Lreffclwmodis=.false.,
  Lreffclimodis=.false.,
  Lpctmodis=.false.,
  Llwpmodis=.false.,
  Liwpmodis=.false.,
  Lclmodis=.false.,
  !- RTTOV
  Ltbrttov=.false.,
/
EOF

cat -> cmor/cosp_cmor_nl.txt << EOF
&CMOR
  INPATH = './cmor',
  OUTPATH = '$outpath',
  START_DATE = '2008-${mon}-01',
  MODEL_ID = 'MPI-ESM-LR',
  EXPERIMENT_ID = '$expid',
  BRANCH_TIME=0.,
  PARENT_EXPERIMENT_ID='N/A',
  PARENT_EXPERIMENT_RIP='N/A',
  FORCING='GHG Oz SD Sl Vl LU',
  INSTITUTION = 'Max Planck Institute for Meteorology',
  INSTITUTE_ID='MPI-M',
  SOURCE = 'MPI-ESM-LR 2011; URL: http://svn.zmaw.de/svn/cosmos/branches/releases/mpi-esm-cmip5/src/mod; atmosphere: ECHAM6 (REV: 4873), T63L47; land: JSBACH (REV: 4873);',
  CALENDAR = 'proleptic_gregorian',
  REALIZATION = 1,
  INITIALIZATION_METHOD = 1,
  PHYSICS_VERSION = 1,
  CONTACT = 'cmip5-mpi-esm@dkrz.de"',
  HISTORY = 'COSP satellite simulator package run offline for ECHAM6-HAM2',
  COMMENT = 'EUCLIPSE/CFMIP',
  REFERENCES = 'COSP: Bodas-Salcedo et al., 2011. COSP: satellite simulation software for model assessment. Bull. Am. Met. Soc., doi: 10.1175/2011BAMS2856.1. ECHAM6: n/a; JSBACH: Raddatz et al., 2007. Will the tropical land biosphere dominate the climate-carbon cycle feedback during the twenty first century? Climate Dynamics, 29, 565-574, doi 10.1007/s00382-007-0247-8',
  TABLE = "$table" !COSP_table_1D COSP_table_1D,CMIP5_cf3hr,COSP_table_2D,CMIP5_cf3hr.cmor1,COSP_table_2D.cmor1
  MAXTSTEPS = 1000
  ntimes_passed=1
  opath="$outpath"
/
EOF

# run it
env LD_LIBRARY_PATH=${HOME}/netcdf-4.4.3-intel14/lib $cosp



wmode="append"
(( ntim = ntim + 1 ))
done
################# FILE LOOP ##################
exit
