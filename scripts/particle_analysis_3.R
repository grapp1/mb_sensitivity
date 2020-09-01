# -------------------------------------------------------------------------------------------------
# particle_analysis_3.R
# EcoSLIM analysis script
# to generate scatter plots of cell-averaged greatest depth of flowpaths for three-layered and
# anisotropic scenarios
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
library(RColorBrewer)
source("~/mb_sensitivity/scripts/prob_dens_fxn.R")
source("~/mb_sensitivity/scripts/EcoSLIM_read_fxn_update.R")
source("~/mb_sensitivity/scripts/cell_agg_fxn.R")

load(file="~/mb_sensitivity/outputs/exit_particles/exited_particles_A.Rda")
load(file="~/mb_sensitivity/outputs/exit_particles/exited_particles_C.Rda")
load(file="~/mb_sensitivity/outputs/exit_particles/exited_particles_H.Rda")

# -------------------------------------------------------------------------------------------------
# calculating greatest flowpath depth from each cell
# -------------------------------------------------------------------------------------------------

layer_colnames <- c(names(exited_particles_A[17:37]))

exited_particles_A$deeplyr <- 0
for(i in 1:nrow(exited_particles_A)){
  for(j in 1:20){
    if(exited_particles_A[i,layer_colnames[j]] > 0){
      exited_particles_A$deeplyr[i] <- j
      break
    }
  }
}
summary(exited_particles_A$deeplyr)

exited_particles_C$deeplyr <- 0
for(i in 1:nrow(exited_particles_C)){
  for(j in 1:20){
    if(exited_particles_C[i,layer_colnames[j]] > 0){
      exited_particles_C$deeplyr[i] <- j
      break
    }
  }
}
summary(exited_particles_C$deeplyr)

exited_particles_H$deeplyr <- 0
for(i in 1:nrow(exited_particles_H)){
  for(j in 1:20){
    if(exited_particles_H[i,layer_colnames[j]] > 0){
      exited_particles_H$deeplyr[i] <- j
      break
    }
  }
}
summary(exited_particles_H$deeplyr)

deeplyr_A <- cell_agg_fxn(exited_particles_A, agg_colname = "deeplyr", funct = max)
deeplyr_C <- cell_agg_fxn(exited_particles_C, agg_colname = "deeplyr", funct = max)
deeplyr_H <- cell_agg_fxn(exited_particles_H, agg_colname = "deeplyr", funct = max)
cell_avg_A <- left_join(x = cell_avg_A, y = deeplyr_A[ , c("X_cell", "Y_cell","deeplyr")], by = c("X_cell","Y_cell"))
cell_avg_C <- left_join(x = cell_avg_C, y = deeplyr_C[ , c("X_cell", "Y_cell","deeplyr")], by = c("X_cell","Y_cell"))
cell_avg_H <- left_join(x = cell_avg_H, y = deeplyr_H[ , c("X_cell", "Y_cell","deeplyr")], by = c("X_cell","Y_cell"))
cell_avg_A <- left_join(x = cell_avg_A, y = layers, by = c("deeplyr"="layer"))
cell_avg_C <- left_join(x = cell_avg_C, y = layers, by = c("deeplyr"="layer"))
cell_avg_H <- left_join(x = cell_avg_H, y = layers, by = c("deeplyr"="layer"))

# -------------------------------------------------------------------------------------------------
# plotting
# -------------------------------------------------------------------------------------------------

deeplyr_bins <- matrix(c(-2.5,-1.5,0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,9.5,11.5,12.5,16.5,20))
col_num <- which(colnames(deeplyr_A)=="deeplyr")
deeplyr_A$deeplyr_bins <- cut(deeplyr_A[,col_num], c(deeplyr_bins), include.lowest = TRUE)
summary(deeplyr_H$deeplyr_bins)
deeplyr_plotA <- ggplot() + geom_tile(data = deeplyr_A, aes(x = X,y = Y, fill = factor(deeplyr_bins)), color="gray",size = 0.05) + 
  #scale_fill_manual(values=c("gray50","white","purple4","purple","darkred","firebrick1","orangered3","orange","yellow", "chartreuse","cyan","cyan4","navy"),
  scale_fill_manual(values=c("gray50","white",brewer.pal(11, "Spectral")[1],brewer.pal(11, "Spectral")), guide = guide_legend(reverse = TRUE, ncol = 4),
                      labels=c("NA","Outside of Domain","> 600","400 - 600","300 - 400","200 - 300","150 - 200","100 - 150","60 - 100","20 - 60","10 - 20","2 - 10","0 - 2")) +
  scale_x_continuous(name="",expand=c(0,0),breaks=c(seq(0,8200,1000)),labels = scales::comma) + 
  scale_y_continuous(name="",expand=c(0,0),breaks=c(seq(0,6000,1000)),labels = scales::comma) +
  labs(fill = "Maximum flowpath depth (m)") + theme_bw() +
  theme(panel.border = element_rect(colour = "black", size=0.5, fill=NA), panel.grid.major = element_line(colour="grey", size=0.1), legend.position = "none",
        legend.background = element_rect(linetype="solid", colour ="white"),plot.margin = margin(5,15,5,5),
        title =element_text(size=10),legend.text = element_text(color="black",size=10),
        axis.ticks.x=element_blank(),axis.ticks.y=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank()) + 
  ggtitle("")
deeplyr_plotA

deeplyr_C$deeplyr_bins <- cut(deeplyr_C[,col_num], c(deeplyr_bins), include.lowest = TRUE)
deeplyr_plotC <- ggplot() + geom_tile(data = deeplyr_C, aes(x = X,y = Y, fill = factor(deeplyr_bins)), color="gray",size = 0.05) + 
  scale_fill_manual(values=c("gray50","white",brewer.pal(11, "Spectral")[1],brewer.pal(11, "Spectral")), guide = guide_legend(reverse = TRUE, ncol = 4),
                    labels=c("NA","Outside of Domain","> 600","400 - 600","300 - 400","200 - 300","150 - 200","100 - 150","60 - 100","20 - 60","10 - 20","2 - 10","0 - 2")) +
  scale_x_continuous(name="",expand=c(0,0),breaks=c(seq(0,8200,1000)),labels = scales::comma) + 
  scale_y_continuous(name="",expand=c(0,0),breaks=c(seq(0,6000,1000)),labels = scales::comma) +
  labs(fill = "Maximum flowpath depth (m)") + theme_bw() +
  theme(panel.border = element_rect(colour = "black", size=0.5, fill=NA), panel.grid.major = element_line(colour="grey", size=0.1), legend.position = "none",
        legend.background = element_rect(linetype="solid", colour ="white"),plot.margin = margin(5,15,5,5),
        title =element_text(size=10),legend.text = element_text(color="black",size=10),
        axis.ticks.x=element_blank(),axis.ticks.y=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank()) + 
  ggtitle("")
deeplyr_plotC

deeplyr_H$deeplyr_bins <- cut(deeplyr_H[,col_num], c(deeplyr_bins), include.lowest = TRUE)
deeplyr_plotH <- ggplot() + geom_tile(data = deeplyr_H, aes(x = X,y = Y, fill = factor(deeplyr_bins)), color="gray",size = 0.05) + 
  scale_fill_manual(values=c("gray50","white",brewer.pal(11, "Spectral")[1],brewer.pal(11, "Spectral")), guide = guide_legend(reverse = TRUE, ncol = 4),
                    labels=c("NA","Outside of Domain","> 600","400 - 600","300 - 400","200 - 300","150 - 200","100 - 150","60 - 100","20 - 60","10 - 20","2 - 10","0 - 2")) +
  scale_x_continuous(name="",expand=c(0,0),breaks=c(seq(0,8200,1000)),labels = scales::comma) + 
  scale_y_continuous(name="",expand=c(0,0),breaks=c(seq(0,6000,1000)),labels = scales::comma) +
  labs(fill = "Maximum flowpath depth (m)") + theme_bw() +
  theme(panel.border = element_rect(colour = "black", size=0.5, fill=NA), panel.grid.major = element_line(colour="grey", size=0.1), legend.position = "none",
        legend.background = element_rect(linetype="solid", colour ="white"),plot.margin = margin(5,15,5,5),
        title =element_text(size=10),legend.text = element_text(color="black",size=10),
        axis.ticks.x=element_blank(),axis.ticks.y=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank()) + 
  ggtitle("")
deeplyr_plotH


grid.arrange(deeplyr_plotA,deeplyr_plotC,deeplyr_plotH, nrow = 1)
