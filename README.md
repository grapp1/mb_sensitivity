mb_sensitivity - repo to support research on sensitivity of simulated mountain-
  block hydrology to subsurface conceptualization

Please provide proper attribution if you use any of the data contained in this repo.

Subdirectories:

1. *model_runs*: ParFlow-CLM input files and EcoSLIM input files for the six
   scenarios. Our research used ParFlow version 3.4.0. More information about
   the model runs is included in the folder.

2. *outputs*: selected figures and files from outputs from the six scenarios.
   This includes the water table figures and the streamflow data for each run.

3. *scripts*: R scripts and supporting files that we developed for processing
   and visualizing the model data

  - Figure 3 (water tables): water_table_calc.R
  - Figure 4 (discharge): streamflow_analysis.R
  - Figures 5, 6, 7, and 8 (flowpaths):
        particle_analysis_1.R (Figures 5 and 8)
        particle_analysis_2.R (Figure 6)
        particle_analysis_3.R (Figure 7)
  - Figure 10 (sensitivity plot): sens_scatter.R


Scenarios respond to each letter designation:

  - A - Homogeneous
  - B - Two-layered
  - C - Three-layered
  - D - Low-k Exponential Decay
  - F - Variable Soil
  - G - High-k Exponential Decay
  - H - Anisotropic
