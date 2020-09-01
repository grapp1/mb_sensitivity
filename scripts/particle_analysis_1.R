# -------------------------------------------------------------------------------------------------
# particle_analysis_1.R
# EcoSLIM analysis script
# to generate saturated age PDFs and variance plots for six scenarios
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
source("~/mb_sensitivity/scripts/prob_dens_fxn.R")
source("~/mb_sensitivityh/scripts/EcoSLIM_read_fxn_update.R")
source("~/mb_sensitivity/scripts/cell_agg_fxn.R")
source("~/mb_sensitivity/scripts/var_bin_fxn.R")
source("~/mb_sensitivity/scripts/particle_flowpath_fxn.R")

# -------------------------------------------------------------------------------------------------
# loading particle output files
# -------------------------------------------------------------------------------------------------

load(file="~/mb_sensitivity/outputs/exit_particles/exited_particles_A.Rda")
load(file="~/mb_sensitivity/outputs/exit_particles/exited_particles_B.Rda")
load(file="~/mb_sensitivity/outputs/exit_particles/exited_particles_C.Rda")
load(file="~/mb_sensitivity/outputs/exit_particles/exited_particles_D.Rda")
load(file="~/mb_sensitivity/outputs/exit_particles/exited_particles_F.Rda")
load(file="~/mb_sensitivity/outputs/exit_particles/exited_particles_G.Rda")
load(file="~/mb_sensitivity/outputs/exit_particles/exited_particles_H.Rda")

# -------------------------------------------------------------------------------------------------
# generating PDF for scenarios
# -------------------------------------------------------------------------------------------------
bin_size_age <- 3
pdf_exit_A_fw1 <- pdfxn(exited_particles_A, max(exited_particles_A$age), bin_size_age,column = "sat_age")
pdf_exit_B_fw1 <- pdfxn(exited_particles_B, max(exited_particles_B$age), bin_size_age,column = "sat_age")
pdf_exit_C_fw1 <- pdfxn(exited_particles_C, max(exited_particles_C$age), bin_size_age,column = "sat_age")
pdf_exit_D_fw1 <- pdfxn(exited_particles_D, max(exited_particles_D$age), bin_size_age,column = "sat_age")
pdf_exit_F_fw1 <- pdfxn(exited_particles_F, max(exited_particles_F$age), bin_size_age,column = "sat_age")
pdf_exit_G_fw1 <- pdfxn(exited_particles_G, max(exited_particles_G$age), bin_size_age,column = "sat_age")
pdf_exit_H_fw1 <- pdfxn(exited_particles_H, max(exited_particles_H$age), bin_size_age,column = "sat_age")

pdf_exit_A_fw1$scen <- "Homogeneous"
pdf_exit_B_fw1$scen <- "Two-layered"
pdf_exit_C_fw1$scen <- "Three-layered"
pdf_exit_F_fw1$scen <- "Variable Soil"
pdf_exit_D_fw1$scen <- "Low-K Exponential Decay"
pdf_exit_H_fw1$scen <- "Anisotropic"
pdf_exited_all <- rbind(pdf_exit_A_fw1,pdf_exit_B_fw1,pdf_exit_C_fw1,pdf_exit_F_fw1,pdf_exit_H_fw1,pdf_exit_D_fw1)

pdf_exit_G_fw1$scen <- "High-K Exponential Decay"
pdf_exited_exp <- rbind(pdf_exit_A_fw1,pdf_exit_D_fw1,pdf_exit_G_fw1)

pdf_fig <- ggplot() + geom_line(data = pdf_exited_all, aes(x = sat_age,y = Density_pdf, group=scen,col = scen), size = 0.75) +
  scale_x_log10(name="",limits = c(3,800), breaks = c(3,25,50,100,200,400,600,800,1000),labels = scales::comma,expand=c(0,0)) +
  scale_y_continuous(name="", expand=c(0,0), breaks = seq(0,0.12,0.01), limits = c(0,0.07)) +
  scale_color_manual(values = c("purple", "black","darkorange","dodgerblue","firebrick","green3"))  + labs(color = "Scenario") +
  expand_limits(x = 100, y = 0) + theme_bw() + 
  theme(panel.border = element_rect(colour = "black", size=1, fill=NA), panel.grid.major = element_line(colour="grey", size=0.1), legend.position = "none",
        legend.background = element_rect(linetype="solid", colour ="black"),plot.margin = margin(15,15,15,15),
        title =element_text(size=12, face='bold'),axis.text.x = element_text(color="black",size=12),axis.text.y = element_text(color="black",size=12))
pdf_fig

pdf_fig_exp <- ggplot() + geom_line(data = pdf_exited_exp, aes(x = sat_age,y = Density_pdf, group=scen,col = scen), size = 0.75) +
  scale_x_log10(name="",limits = c(3,800), breaks = c(3,25,50,100,200,400,600,800,1000),labels = scales::comma,expand=c(0,0)) +
  scale_y_continuous(name="", expand=c(0,0), breaks = seq(0,0.12,0.02), limits = c(0,0.12)) +
  scale_color_manual(values = c("khaki4","black","darkorange"))  + labs(color = "Scenario") +
  expand_limits(x = 100, y = 0) + theme_bw() + 
  theme(panel.border = element_rect(colour = "black", size=1, fill=NA), panel.grid.major = element_line(colour="grey", size=0.1), legend.position = "none",
        legend.background = element_rect(linetype="solid", colour ="black"),plot.margin = margin(15,15,15,15),
        title =element_text(size=12, face='bold'),axis.text.x = element_text(color="black",size=12),axis.text.y = element_text(color="black",size=12))
pdf_fig_exp

# -------------------------------------------------------------------------------------------------
# calculating variance time series
# -------------------------------------------------------------------------------------------------

var_bin <- 3
var_spath_A <- var_bin_fxn(exited_particles_A, max(exited_particles_A$age), var_bin,column1 = "sat_age",column2 = "spath_len")
var_spath_B <- var_bin_fxn(exited_particles_B, max(exited_particles_B$age), var_bin,column1 = "sat_age",column2 = "spath_len")
var_spath_C <- var_bin_fxn(exited_particles_C, max(exited_particles_C$age), var_bin,column1 = "sat_age",column2 = "spath_len")
var_spath_D <- var_bin_fxn(exited_particles_D, max(exited_particles_D$age), var_bin,column1 = "sat_age",column2 = "spath_len")
var_spath_F <- var_bin_fxn(exited_particles_F, max(exited_particles_F$age), var_bin,column1 = "sat_age",column2 = "spath_len")
var_spath_G <- var_bin_fxn(exited_particles_G, max(exited_particles_G$age), var_bin,column1 = "sat_age",column2 = "spath_len")
var_spath_H <- var_bin_fxn(exited_particles_H, max(exited_particles_H$age), var_bin,column1 = "sat_age",column2 = "spath_len")

var_spath_A$scen <- "Homogeneous"
var_spath_B$scen <- "Two-layered"
var_spath_C$scen <- "Three-layered"
var_spath_F$scen <- "Variable Soil"
var_spath_D$scen <- "Low-K Exponential Decay"
var_spath_H$scen <- "Anisotropic"

var_spath_G$scen <- "High-K Exponential Decay"

var_bin_1 <- rbind(var_spath_A,var_spath_B,var_spath_C,var_spath_F)
var_bin_1 <- var_bin_1[which(var_bin_1$count > 9),]
var_bin_2 <- rbind(var_spath_A,var_spath_H,var_spath_D)
var_bin_2 <- var_bin_2[which(var_bin_2$count > 9),]
var_bin_all <- rbind(var_spath_A,var_spath_B,var_spath_C,var_spath_F,var_spath_H,var_spath_D)
var_bin_all <- var_bin_all[which(var_bin_all$count > 9),]
var_bin_exp <- rbind(var_spath_A,var_spath_D,var_spath_G)
var_bin_exp <- var_bin_exp[which(var_bin_exp$count > 9),]

# -------------------------------------------------------------------------------------------------
# generating variance plot
# -------------------------------------------------------------------------------------------------

var_bin_fig <- ggplot(data = var_bin_all, aes(x = sat_age,y = variance, group=scen,col = scen)) + geom_line(size = 0.75) + #geom_point(size =0.5) + 
  scale_x_continuous(name="Saturated age (yr)",limits = c(0,800), breaks=c(0,100,200,300,400,500,600,700,800),labels = scales::comma,expand=c(0,0)) +
  ggtitle("Variance of saturated lengths of exited particles") + 
  scale_y_log10(name=bquote('Variance of saturated path lengths ('*m^2*')'), expand=c(0,0), limits = c(1000,1000000000), breaks = c(1000,10000,100000,1000000,10000000,100000000,1000000000,10000000000)) +  
  scale_color_manual(values = c("purple", "black","darkorange","dodgerblue","firebrick","green3"))  + labs(color = "Scenario") +
  expand_limits(x = 10, y = 0) + theme_bw() + 
  theme(panel.border = element_rect(colour = "black", size=1, fill=NA), panel.grid.major = element_line(colour="grey", size=0.1), legend.position = "none",
        legend.background = element_rect(linetype="solid", colour ="black"),plot.margin = margin(5,15,5,5),
        title =element_text(size=12, face='bold'),axis.text.x = element_text(color="black",size=10),axis.text.y = element_text(color="black", size = 10)) +
  theme(axis.title.y = element_text(color="black",face='bold'))
var_bin_fig

var_bin_exp <- ggplot(data = var_bin_exp, aes(x = sat_age,y = variance, group=scen,col = scen)) + geom_line(size = 0.75) + #geom_point(size =0.5) + 
  scale_x_continuous(name="Saturated age (yr)",limits = c(0,800), breaks=c(0,100,200,300,400,500,600,700,800),labels = scales::comma,expand=c(0,0)) +
  ggtitle("Variance of saturated lengths of exited particles") + 
  scale_y_log10(name=bquote('Variance of saturated path lengths ('*m^2*')'), expand=c(0,0), limits = c(1000,1000000000), breaks = c(1000,10000,100000,1000000,10000000,100000000,1000000000,10000000000)) +  
  scale_color_manual(values = c("khaki4", "black","darkorange"))  + labs(color = "Scenario") +
  expand_limits(x = 10, y = 0) + theme_bw() + 
  theme(panel.border = element_rect(colour = "black", size=1, fill=NA), panel.grid.major = element_line(colour="grey", size=0.1), legend.position = "none",
        legend.background = element_rect(linetype="solid", colour ="black"),plot.margin = margin(5,15,5,5),
        title =element_text(size=12, face='bold'),axis.text.x = element_text(color="black",size=10),axis.text.y = element_text(color="black", size = 10)) +
  theme(axis.title.y = element_text(color="black",face='bold'))
var_bin_exp

# -------------------------------------------------------------------------------------------------
# calculating sensitivity metric values
# -------------------------------------------------------------------------------------------------

sens_fp <- data.frame(matrix(NA, nrow = 7, ncol = 4))
colnames(sens_fp)<- c("scen","mean","sd","sens_fp")

sens_fp$scen <- c("Homogeneous", "Two-layered", "Three-layered", "Variable Soil", "Anisotropic", "Low-K Exp Decay", "High-K Exp Decay")

sens_fp$mean[1] <- mean(exited_particles_A$sat_age)
sens_fp$sd[1] <- sd(exited_particles_A$sat_age)

sens_fp$mean[2] <- mean(exited_particles_B$sat_age)
sens_fp$sd[2] <- sd(exited_particles_B$sat_age)

sens_fp$mean[3] <- mean(exited_particles_C$sat_age)
sens_fp$sd[3] <- sd(exited_particles_C$sat_age)

sens_fp$mean[4] <- mean(exited_particles_F$sat_age)
sens_fp$sd[4] <- sd(exited_particles_F$sat_age)

sens_fp$mean[5] <- mean(exited_particles_H$sat_age)
sens_fp$sd[5] <- sd(exited_particles_H$sat_age)

sens_fp$mean[6] <- mean(exited_particles_D$sat_age)
sens_fp$sd[6] <- sd(exited_particles_D$sat_age)

sens_fp$mean[7] <- mean(exited_particles_G$sat_age)
sens_fp$sd[7] <- sd(exited_particles_G$sat_age)

for(i in 1:nrow(sens_fp)){
  pct_chg_mean <- abs((sens_fp$mean[i] - sens_fp$mean[1])/sens_fp$mean[1])
  pct_chg_sd <- abs((sens_fp$sd[i] - sens_fp$sd[1])/sens_fp$sd[1])
  sens_fp$sens_fp[i] <- pct_chg_mean + pct_chg_sd
}

