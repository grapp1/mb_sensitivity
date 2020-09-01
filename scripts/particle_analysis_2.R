# -------------------------------------------------------------------------------------------------
# particle_analysis_2.R
# EcoSLIM analysis script
# to generate scatter plots of cell-averaged particle ages/flowpath lengths
# -------------------------------------------------------------------------------------------------

library(ggplot2)
library(ggnewscale)
library(tidyr)
library(readr)
library(dplyr)
library(openxlsx)
library(cowplot)
library(zoo)
library(plotrix)
library(plyr)
library(spatstat)
library(gridExtra)
library(grDevices)
source("~/mb_sensitivity/scripts/prob_dens_fxn.R")
source("~/mb_sensitivity/scripts/EcoSLIM_read_fxn_update.R")
source("~/mb_sensitivity/scripts/cell_agg_fxn.R")

# -------------------------------------------------------------------------------------------------
# calculating cell-averaged values
# -------------------------------------------------------------------------------------------------

soil_len_rat_A <- cell_agg_fxn(exited_particles_A, agg_colname = "soil_len_ratio")
soil_len_avg_A <- cell_agg_fxn(exited_particles_A, agg_colname = "soil_len")
age_avg_A <- cell_agg_fxn(exited_particles_A, agg_colname = "age")
len_avg_A <- cell_agg_fxn(exited_particles_A, agg_colname = "path_len")
sat_age_avg_A <- cell_agg_fxn(exited_particles_A, agg_colname = "sat_age")
slen_avg_A <- cell_agg_fxn(exited_particles_A, agg_colname = "spath_len")
cell_avg_A <- soil_len_avg_A
cell_avg_A$soil_len_ratio <- soil_len_rat_A$soil_len_ratio
cell_avg_A$path_len <- len_avg_A$path_len
cell_avg_A$age <- age_avg_A$age
cell_avg_A$spath_len <- slen_avg_A$spath_len
cell_avg_A$sat_age <- sat_age_avg_A$sat_age
cell_avg_A$upath_len <- cell_avg_A$path_len - cell_avg_A$spath_len
cell_avg_A$usat_age <- cell_avg_A$age - cell_avg_A$sat_age
cell_avg_A <- cell_avg_A[which(cell_avg_A$age > 0), ]
load(file="~/mb_sensitivity/outputs/water_tables/wt_A_v5_991.df.Rda")
cell_avg_A <- left_join(x = cell_avg_A, y = wt_A_v5_991.df[ , c("x", "y","dtw","elev","wt_elev")], by = c("X_cell" = "x","Y_cell" = "y"))
rm(lmres_A)
lmres_A <- lm(spath_len ~ sat_age, data=cell_avg_A)
ggplot() + geom_point(data = cell_avg_A, aes(x = sat_age,y = spath_len,color=(spath_len/path_len)),alpha = 1) + 
  geom_abline(slope = lmres_A$coefficients[2], intercept = lmres_A$coefficients[1], col="purple",linetype = "twodash")

soil_len_rat_B <- cell_agg_fxn(exited_particles_B, agg_colname = "soil_len_ratio")
soil_len_avg_B <- cell_agg_fxn(exited_particles_B, agg_colname = "soil_len")
age_avg_B <- cell_agg_fxn(exited_particles_B, agg_colname = "age")
len_avg_B <- cell_agg_fxn(exited_particles_B, agg_colname = "path_len")
sat_age_avg_B <- cell_agg_fxn(exited_particles_B, agg_colname = "sat_age")
slen_avg_B <- cell_agg_fxn(exited_particles_B, agg_colname = "spath_len")
cell_avg_B <- soil_len_avg_B
cell_avg_B$soil_len_ratio <- soil_len_rat_B$soil_len_ratio
cell_avg_B$path_len <- len_avg_B$path_len
cell_avg_B$age <- age_avg_B$age
cell_avg_B$spath_len <- slen_avg_B$spath_len
cell_avg_B$sat_age <- sat_age_avg_B$sat_age
cell_avg_B$upath_len <- cell_avg_B$path_len - cell_avg_B$spath_len
cell_avg_B$usat_age <- cell_avg_B$age - cell_avg_B$sat_age
cell_avg_B <- cell_avg_B[which(cell_avg_B$age > 0), ]
load(file="~/mb_sensitivity/outputs/water_tables/wt_B_v4_1037.df.Rda")
cell_avg_B <- left_join(x = cell_avg_B, y = wt_B_v4_1037.df[ , c("x", "y","dtw","elev","wt_elev")], by = c("X_cell" = "x","Y_cell" = "y"))
rm(lmres_B)
lmres_B <- lm(spath_len ~ sat_age, data=cell_avg_B)
ggplot() + geom_point(data = cell_avg_B, aes(x = sat_age,y = spath_len,color=(spath_len/path_len)),alpha = 1) + 
  geom_abline(slope = lmres_B$coefficients[2], intercept = lmres_B$coefficients[1], col="purple",linetype = "twodash")

soil_len_rat_C <- cell_agg_fxn(exited_particles_C, agg_colname = "soil_len_ratio")
soil_len_avg_C <- cell_agg_fxn(exited_particles_C, agg_colname = "soil_len")
age_avg_C <- cell_agg_fxn(exited_particles_C, agg_colname = "age")
len_avg_C <- cell_agg_fxn(exited_particles_C, agg_colname = "path_len")
sat_age_avg_C <- cell_agg_fxn(exited_particles_C, agg_colname = "sat_age")
slen_avg_C <- cell_agg_fxn(exited_particles_C, agg_colname = "spath_len")
cell_avg_C <- soil_len_avg_C
cell_avg_C$soil_len_ratio <- soil_len_rat_C$soil_len_ratio
cell_avg_C$path_len <- len_avg_C$path_len
cell_avg_C$age <- age_avg_C$age
cell_avg_C$spath_len <- slen_avg_C$spath_len
cell_avg_C$sat_age <- sat_age_avg_C$sat_age
cell_avg_C$upath_len <- cell_avg_C$path_len - cell_avg_C$spath_len
cell_avg_C$usat_age <- cell_avg_C$age - cell_avg_C$sat_age
cell_avg_C <- cell_avg_C[which(cell_avg_C$age > 0), ]
load(file="~/mb_sensitivity/outputs/water_tables/wt_C_v4_1036.df.Rda")
cell_avg_C <- left_join(x = cell_avg_C, y = wt_C_v4_1036.df[ , c("x", "y","dtw","elev","wt_elev")], by = c("X_cell" = "x","Y_cell" = "y"))
rm(lmres_C)
lmres_C <- lm(spath_len ~ sat_age, data=cell_avg_C)
ggplot() + geom_point(data = cell_avg_C, aes(x = sat_age,y = spath_len,color=(spath_len/path_len)),alpha = 1) + 
  geom_abline(slope = lmres_C$coefficients[2], intercept = lmres_C$coefficients[1], col="purple",linetype = "twodash")

soil_len_rat_D <- cell_agg_fxn(exited_particles_D, agg_colname = "soil_len_ratio")
soil_len_avg_D <- cell_agg_fxn(exited_particles_D, agg_colname = "soil_len")
age_avg_D <- cell_agg_fxn(exited_particles_D, agg_colname = "age")
len_avg_D <- cell_agg_fxn(exited_particles_D, agg_colname = "path_len")
sat_age_avg_D <- cell_agg_fxn(exited_particles_D, agg_colname = "sat_age")
slen_avg_D <- cell_agg_fxn(exited_particles_D, agg_colname = "spath_len")
cell_avg_D <- soil_len_avg_D
cell_avg_D$soil_len_ratio <- soil_len_rat_D$soil_len_ratio
cell_avg_D$path_len <- len_avg_D$path_len
cell_avg_D$age <- age_avg_D$age
cell_avg_D$spath_len <- slen_avg_D$spath_len
cell_avg_D$sat_age <- sat_age_avg_D$sat_age
cell_avg_D$upath_len <- cell_avg_D$path_len - cell_avg_D$spath_len
cell_avg_D$usat_age <- cell_avg_D$age - cell_avg_D$sat_age
cell_avg_D <- cell_avg_D[which(cell_avg_D$age > 0), ]
load(file="~/mb_sensitivity/outputs/water_tables/wt_D_v4_993.df.Rda")
cell_avg_D <- left_join(x = cell_avg_D, y = wt_D_v4_993.df[ , c("x", "y","dtw","elev","wt_elev")], by = c("X_cell" = "x","Y_cell" = "y"))
rm(lmres_D)
lmres_D <- lm(spath_len ~ sat_age, data=cell_avg_D)
ggplot() + geom_point(data = cell_avg_D, aes(x = sat_age,y = spath_len,color=(spath_len/path_len)),alpha = 1) + 
  geom_abline(slope = lmres_D$coefficients[2], intercept = lmres_D$coefficients[1], col="purple",linetype = "twodash")

soil_len_rat_F <- cell_agg_fxn(exited_particles_F, agg_colname = "soil_len_ratio")
soil_len_avg_F <- cell_agg_fxn(exited_particles_F, agg_colname = "soil_len")
age_avg_F <- cell_agg_fxn(exited_particles_F, agg_colname = "age")
len_avg_F <- cell_agg_fxn(exited_particles_F, agg_colname = "path_len")
sat_age_avg_F <- cell_agg_fxn(exited_particles_F, agg_colname = "sat_age")
slen_avg_F <- cell_agg_fxn(exited_particles_F, agg_colname = "spath_len")
cell_avg_F <- soil_len_avg_F
cell_avg_F$soil_len_ratio <- soil_len_rat_F$soil_len_ratio
cell_avg_F$path_len <- len_avg_F$path_len
cell_avg_F$age <- age_avg_F$age
cell_avg_F$spath_len <- slen_avg_F$spath_len
cell_avg_F$sat_age <- sat_age_avg_F$sat_age
cell_avg_F$upath_len <- cell_avg_F$path_len - cell_avg_F$spath_len
cell_avg_F$usat_age <- cell_avg_F$age - cell_avg_F$sat_age
cell_avg_F <- cell_avg_F[which(cell_avg_F$age > 0), ]
load(file="~/mb_sensitivity/outputs/water_tables/wt_F_v1_997.df.Rda")
cell_avg_F <- left_join(x = cell_avg_F, y = wt_F_v1_997.df[ , c("x", "y","dtw","elev","wt_elev")], by = c("X_cell" = "x","Y_cell" = "y"))
rm(lmres_F)
lmres_F <- lm(spath_len ~ sat_age, data=cell_avg_F)
ggplot() + geom_point(data = cell_avg_F, aes(x = sat_age,y = spath_len,color=(spath_len/path_len)),alpha = 1) + 
  geom_abline(slope = lmres_F$coefficients[2], intercept = lmres_F$coefficients[1], col="purple",linetype = "twodash")

soil_len_rat_G <- cell_agg_fxn(exited_particles_G, agg_colname = "soil_len_ratio")
soil_len_avg_G <- cell_agg_fxn(exited_particles_G, agg_colname = "soil_len")
age_avg_G <- cell_agg_fxn(exited_particles_G, agg_colname = "age")
len_avg_G <- cell_agg_fxn(exited_particles_G, agg_colname = "path_len")
sat_age_avg_G <- cell_agg_fxn(exited_particles_G, agg_colname = "sat_age")
slen_avg_G <- cell_agg_fxn(exited_particles_G, agg_colname = "spath_len")
cell_avg_G <- soil_len_avg_G
cell_avg_G$soil_len_ratio <- soil_len_rat_G$soil_len_ratio
cell_avg_G$path_len <- len_avg_G$path_len
cell_avg_G$age <- age_avg_G$age
cell_avg_G$spath_len <- slen_avg_G$spath_len
cell_avg_G$sat_age <- sat_age_avg_G$sat_age
cell_avg_G$upath_len <- cell_avg_G$path_len - cell_avg_G$spath_len
cell_avg_G$usat_age <- cell_avg_G$age - cell_avg_G$sat_age
cell_avg_G <- cell_avg_G[which(cell_avg_G$age > 0), ]
load(file="~/mb_sensitivity/outputs/water_tables/wt_G_v1_0993.df.Rda")
cell_avg_G <- left_join(x = cell_avg_G, y = wt_G_v1_0993.df[ , c("x", "y","dtw","elev","wt_elev")], by = c("X_cell" = "x","Y_cell" = "y"))
rm(lmres_G)
lmres_G <- lm(spath_len ~ sat_age, data=cell_avg_G)
ggplot() + geom_point(data = cell_avg_G, aes(x = sat_age,y = spath_len,color=dtw),alpha = 1) + 
  geom_abline(slope = lmres_G$coefficients[2], intercept = lmres_G$coefficients[1], col="purple",linetype = "twodash")
lmres_G$coefficients[2]/1000
summary(lmres_G)$r.squared


soil_len_rat_H <- cell_agg_fxn(exited_particles_H, agg_colname = "soil_len_ratio")
soil_len_avg_H <- cell_agg_fxn(exited_particles_H, agg_colname = "soil_len")
age_avg_H <- cell_agg_fxn(exited_particles_H, agg_colname = "age")
len_avg_H <- cell_agg_fxn(exited_particles_H, agg_colname = "path_len")
sat_age_avg_H <- cell_agg_fxn(exited_particles_H, agg_colname = "sat_age")
slen_avg_H <- cell_agg_fxn(exited_particles_H, agg_colname = "spath_len")
cell_avg_H <- soil_len_avg_H
cell_avg_H$soil_len_ratio <- soil_len_rat_H$soil_len_ratio
cell_avg_H$path_len <- len_avg_H$path_len
cell_avg_H$age <- age_avg_H$age
cell_avg_H$spath_len <- slen_avg_H$spath_len
cell_avg_H$sat_age <- sat_age_avg_H$sat_age
cell_avg_H$upath_len <- cell_avg_H$path_len - cell_avg_H$spath_len
cell_avg_H$usat_age <- cell_avg_H$age - cell_avg_H$sat_age
cell_avg_H <- cell_avg_H[which(cell_avg_H$age > 0), ]
load(file="~/mb_sensitivity/outputs/water_tables/wt_H_v1_1018.df.Rda")
cell_avg_H <- left_join(x = cell_avg_H, y = wt_H_v1_1018.df[ , c("x", "y","dtw","elev","wt_elev")], by = c("X_cell" = "x","Y_cell" = "y"))
rm(lmres_H)
lmres_H <- lm(spath_len ~ sat_age, data=cell_avg_H)
ggplot() + geom_point(data = cell_avg_H, aes(x = sat_age,y = spath_len,color=dtw),alpha = 1) + 
  geom_abline(slope = lmres_H$coefficients[2], intercept = lmres_H$coefficients[1], col="purple",linetype = "twodash")
lmres_H$coefficients[2]/1000
summary(lmres_H)$r.squared

sap_len_rat_A <- cell_agg_fxn(exited_particles_A, agg_colname = "sap_len_ratio")
sap_len_rat_G <- cell_agg_fxn(exited_particles_G, agg_colname = "sap_len_ratio")
sap_len_rat_H <- cell_agg_fxn(exited_particles_H, agg_colname = "sap_len_ratio")
cell_avg_A <- left_join(x = cell_avg_A, y = sap_len_rat_A[ , c("X_cell", "Y_cell","sap_len_ratio")], by = c("X_cell","Y_cell"))
cell_avg_G <- left_join(x = cell_avg_G, y = sap_len_rat_G[ , c("X_cell", "Y_cell","sap_len_ratio")], by = c("X_cell","Y_cell"))
cell_avg_H <- left_join(x = cell_avg_H, y = sap_len_rat_H[ , c("X_cell", "Y_cell","sap_len_ratio")], by = c("X_cell","Y_cell"))

cell_avg_A$scen <- "Homogeneous"
cell_avg_G$scen <- "High-K Exponential Decay"
cell_avg_H$scen <- "Anisotropic"

# -------------------------------------------------------------------------------------------------
# scatter plots
# -------------------------------------------------------------------------------------------------

x_limit <- 800
y_limit <- 80000
transparency <- 0.5

cell_avg_scatterA <- ggplot() + geom_point(data = cell_avg_A, aes(x = sat_age,y = spath_len),alpha = transparency) + 
  scale_x_continuous(name="Particle saturated age (yr)",limits = c(0,x_limit), expand=c(0,0), breaks = seq(0,x_limit,100),labels = scales::comma) +
  ggtitle("Homogeneous") + 
  scale_y_continuous(name="Particle saturated path length (m)", expand=c(0,0), breaks = seq(0,y_limit,10000), 
                     limits = c(0,y_limit),labels = scales::comma) + 
  expand_limits(x = 0, y = 0) + theme_bw() +
  theme(panel.border = element_rect(colour = "black", size=1, fill=NA), panel.grid.major = element_line(colour="grey", size=0.1), legend.position="none",plot.margin = margin(5,15,5,5)) + 
  geom_abline(slope = lmres_A$coefficients[2], intercept = lmres_A$coefficients[1], col="black", linetype = "dashed")
#cell_avg_scatterA

cell_avg_scatterB <- ggplot() + geom_point(data = cell_avg_B, aes(x = sat_age,y = spath_len),alpha = transparency) + 
  scale_x_continuous(name="Particle saturated age (yr)",limits = c(0,x_limit), expand=c(0,0), breaks = seq(0,x_limit,100),labels = scales::comma) +
  ggtitle("Two-layered") + 
  scale_y_continuous(name="Particle saturated path length (m)", expand=c(0,0), breaks = seq(0,y_limit,10000), 
                     limits = c(0,y_limit),labels = scales::comma) + 
  expand_limits(x = 0, y = 0) + theme_bw() +
  theme(panel.border = element_rect(colour = "black", size=1, fill=NA), panel.grid.major = element_line(colour="grey", size=0.1), legend.position="none",plot.margin = margin(5,15,5,5)) + 
  geom_abline(slope = lmres_B$coefficients[2], intercept = lmres_B$coefficients[1], col="black", linetype = "dashed")
#cell_avg_scatterB

cell_avg_scatterC <- ggplot() + geom_point(data = cell_avg_C, aes(x = sat_age,y = spath_len),alpha = transparency) + 
  scale_x_continuous(name="Particle saturated age (yr)",limits = c(0,x_limit), expand=c(0,0), breaks = seq(0,x_limit,100),labels = scales::comma) +
  ggtitle("Three-layered") + 
  scale_y_continuous(name="Particle saturated path length (m)", expand=c(0,0), breaks = seq(0,y_limit,10000), 
                     limits = c(0,y_limit),labels = scales::comma) + 
  expand_limits(x = 0, y = 0) + theme_bw() +
  theme(panel.border = element_rect(colour = "black", size=1, fill=NA), panel.grid.major = element_line(colour="grey", size=0.1), legend.position="none",plot.margin = margin(5,15,5,5)) + 
  geom_abline(slope = lmres_C$coefficients[2], intercept = lmres_C$coefficients[1], col="black", linetype = "dashed")
#cell_avg_scatterC

cell_avg_scatterD <- ggplot() + geom_point(data = cell_avg_D, aes(x = sat_age,y = spath_len),alpha = transparency) + 
  scale_x_continuous(name="Particle saturated age (yr)",limits = c(0,x_limit), expand=c(0,0), breaks = seq(0,x_limit,100),labels = scales::comma) +
  ggtitle("Low-K Exp. Decay") + 
  scale_y_continuous(name="Particle saturated path length (m)", expand=c(0,0), breaks = seq(0,y_limit,10000), 
                     limits = c(0,y_limit),labels = scales::comma) + 
  expand_limits(x = 0, y = 0) + theme_bw() +
  theme(panel.border = element_rect(colour = "black", size=1, fill=NA), panel.grid.major = element_line(colour="grey", size=0.1), legend.position="none",plot.margin = margin(5,15,5,5)) + 
  geom_abline(slope = lmres_D$coefficients[2], intercept = lmres_D$coefficients[1], col="black", linetype = "dashed")
#cell_avg_scatterD

cell_avg_scatterF <- ggplot() + geom_point(data = cell_avg_F, aes(x = sat_age,y = spath_len),alpha = transparency) + 
  scale_x_continuous(name="Particle saturated age (yr)",limits = c(0,x_limit), expand=c(0,0), breaks = seq(0,x_limit,100),labels = scales::comma) +
  ggtitle("Variable Soil") + 
  scale_y_continuous(name="Particle saturated path length (m)", expand=c(0,0), breaks = seq(0,y_limit,10000), 
                     limits = c(0,y_limit),labels = scales::comma) + 
  expand_limits(x = 0, y = 0) + theme_bw() +
  theme(panel.border = element_rect(colour = "black", size=1, fill=NA), panel.grid.major = element_line(colour="grey", size=0.1), legend.position="none",plot.margin = margin(5,15,5,5)) + 
  geom_abline(slope = lmres_F$coefficients[2], intercept = lmres_F$coefficients[1], col="black", linetype = "dashed")
#cell_avg_scatterF

cell_avg_scatterG <- ggplot() + geom_point(data = cell_avg_G, aes(x = sat_age,y = spath_len),alpha = transparency) + 
  scale_x_continuous(name="Particle saturated age (yr)",limits = c(0,x_limit), expand=c(0,0), breaks = seq(0,x_limit,100),labels = scales::comma) +
  ggtitle("High-K Exp. Decay") + 
  scale_y_continuous(name="Particle saturated path length (m)", expand=c(0,0), breaks = seq(0,y_limit,10000), 
                     limits = c(0,y_limit),labels = scales::comma) + 
  expand_limits(x = 0, y = 0) + theme_bw() +
  theme(panel.border = element_rect(colour = "black", size=1, fill=NA), panel.grid.major = element_line(colour="grey", size=0.1), legend.position="none",plot.margin = margin(5,15,5,5)) + 
  geom_abline(slope = lmres_G$coefficients[2], intercept = lmres_G$coefficients[1], col="black", linetype = "dashed")
cell_avg_scatterG

cell_avg_scatterH <- ggplot() + geom_point(data = cell_avg_H, aes(x = sat_age,y = spath_len),alpha = transparency) + 
  scale_x_continuous(name="Particle saturated age (yr)",limits = c(0,x_limit), expand=c(0,0), breaks = seq(0,x_limit,100),labels = scales::comma) +
  ggtitle("Anisotropic") + 
  scale_y_continuous(name="Particle saturated path length (m)", expand=c(0,0), breaks = seq(0,y_limit,10000), 
                     limits = c(0,y_limit),labels = scales::comma) +  
  expand_limits(x = 0, y = 0) + theme_bw() +
  theme(panel.border = element_rect(colour = "black", size=1, fill=NA), panel.grid.major = element_line(colour="grey", size=0.1), legend.position="none",plot.margin = margin(5,15,5,5)) + 
  geom_abline(slope = lmres_H$coefficients[2], intercept = lmres_H$coefficients[1], col="black", linetype = "dashed")
#cell_avg_scatterH


grid.arrange(cell_avg_scatterA, cell_avg_scatterB,cell_avg_scatterC,cell_avg_scatterF, cell_avg_scatterD,cell_avg_scatterH, nrow = 2)

