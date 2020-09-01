Readme for mb_sensitivity/model_runs

Enclosed folders:

1) CLM: this contains all the CLM input files. Our CLM input data was identical
for each run, so these should be used for each run. This consists of five years
of hourly data repeated over two cycles. Edit the drv_clmin.dat file to specify
file names for each run.

2) ecoslim: this folder contains an EcoSLIM input file for one of the runs. This
has a similar structure for each run. This particular file is written for the
anisotropic scenario at the beginning of the particle tracking. EcoSLIM runs
were simulated over cycles of 200 years on an hourly timestep. Therefore, this
requires one year of hourly data outputs from EcoSLIM (not included here due to
space constraints). The particle restart file included here can be used for all
runs, as long as the 20-layer indicator file (D_indicator_ES.pfb) is used.

3) PFCLM: this folder contains the tcl input files for the ParFlow-CLM runs,
including the referenced pressure files from the initialized water tables. These
scripts are what we used to generate the hourly outputs for reading into
EcoSLIM.


Scenarios respond to each letter designation:
A - Homogeneous
B - Two-layered
C - Three-layered
D - Low-k Exponential Decay
F - Variable Soil
G - High-k Exponential Decay
H - Anisotropic
