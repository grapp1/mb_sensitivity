#-----------------------------------------------------------------------------
#Calculate Flow using Mannings equation on USGS stations
## units = m3/s
# 20190826 GR update to calculate the percentage change per time step
#-----------------------------------------------------------------------------

# Import the ParFlow TCL package
lappend   auto_path $env(PARFLOW_DIR)/bin
package   require parflow
namespace import Parflow::*

pfset     FileVersion    4

pfset Process.Topology.P 1
pfset Process.Topology.Q 1
pfset Process.Topology.R 1

#-----------------------------------------------------------------------------
# Computational Grid
#-----------------------------------------------------------------------------
pfset ComputationalGrid.Lower.X           0.0
pfset ComputationalGrid.Lower.Y           0.0
pfset ComputationalGrid.Lower.Z           0.0

pfset ComputationalGrid.NX                91
pfset ComputationalGrid.NY                70
pfset ComputationalGrid.NZ                20

pfset ComputationalGrid.DX                90.0
pfset ComputationalGrid.DY                90.0
pfset ComputationalGrid.DZ                100.0

set dx                                    90.0
set dy                                    90.0
set dz                                    100.0

#-----------------------------------------------------------------------------
# Runname Directory and timing
#-----------------------------------------------------------------------------
set timesteps      8760
set runname        "A_v6q"
cd "./A_v6_q3_out/"

# Timestep is the multiplier use for flow calculations... since mannings units are in m and hr just set to 1 for flow m^3
set TimeStep 1

# Set up file for writing water table outputs
set summary_name  "../wb_A_v6q3.txt"
set file_summary [open $summary_name w 0600]
puts $file_summary "Hour\t Total_subsurface_storage\t Total_surface_storage\t Total_surface_runoff\t ET\t"

# Read in all of the general files
set mask                [pfload $runname.out.mask.pfb]
set specific_storage    [pfload $runname.out.specific_storage.pfb]
set porosity            [pfload $runname.out.porosity.pfb]
set top                 [pfcomputetop $mask]
set mannings            [pfload $runname.out.mannings.silo]
set sx                  [pfload ../garrett.slopex.pfb]
set sy                  [pfload ../garrett.slopey.pfb]

for {set i 1} {$i <=$timesteps } {incr i} {
    # Read in inputs
    set fin [format $runname.out.press.%05d.pfb $i]
    set pressure [pfload $fin]
    set fin [format $runname.out.satur.%05d.pfb $i]
    set saturation [pfload $fin]


    # Calculate water balance
    set water_table_depth [pfwatertabledepth $top $saturation]
    set subsurface_storage [pfsubsurfacestorage $mask $porosity $pressure $saturation $specific_storage]
    set total_subsurface_storage [pfsum $subsurface_storage]
    set surface_storage [pfsurfacestorage $top $pressure]
    set total_surface_storage [pfsum $surface_storage]
    set surface_runoff [pfsurfacerunoff $top $sx $sy $mannings $pressure]
    set total_surface_runoff [expr [pfsum $surface_runoff] * $TimeStep]

    #set fin [format $runname.out.qflx_evap_tot.%05d.silo $i]
    #set input [pfload $fin]
    set ET_sum 0

    puts $file_summary "$i\t $total_subsurface_storage\t $total_surface_storage\t $total_surface_runoff\t $ET_sum\t"

    #Calculate the water table depth
    #set water_table_depth [pfwatertabledepth $top $saturation]
    #set fout_silo [format WTD.%03d.silo $i]
    #pfsave $water_table_depth  -silo $fout_silo

    # Clear Variables
    pfdelete $pressure
    pfdelete $saturation
    #pfdelete $water_table_depth

}
