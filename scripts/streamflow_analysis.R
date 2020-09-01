# -------------------------------------------------------------------------------------------------
# streamflow_analysis.R - calculating discharge flow-duration curves for scenarios
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
source("~/mb_sensitivity/scripts/prob_exc_fxn.R")
source("~/mb_sensitivity/scripts/EcoSLIM_read_fxn_update.R")
source("~/mb_sensitivity/scripts/cell_agg_fxn.R")
source("~/mb_sensitivity/scripts/var_bin_fxn.R")
source("~/mb_sensitivity/scripts/particle_flowpath_fxn.R")

# -------------------------------------------------------------------------------------------------
# loading precipitation data
# -------------------------------------------------------------------------------------------------

precip <- read.table(file = "~/mb_sensitivity/model_runs/CLM/Forcing1D_gr.txt", header = FALSE)
precip <- data.frame(precip[1:8760,c(3)])
colnames(precip) <- c("precip")
precip$hour <- row(precip)
precip$day <- rep(1:365, each = 24)
precip_day <- aggregate(precip$precip, by = list(Category = precip$day), FUN = sum)
colnames(precip_day) <- c("day", "precip")
precip_day$precip <- precip_day$precip*3600
max(precip_day$precip)
for(i in 1:nrow(precip_day)){
  precip_day$cu_prec[i] <- sum(precip_day$precip[1:i])
}
precip_day$date <- seq(as.Date("2008/10/01"), by = "day", length.out = 365)

# -------------------------------------------------------------------------------------------------
# loading discharge data
# -------------------------------------------------------------------------------------------------

wbal_A <- read.table(file = "~/mb_sensitivity/outputs/discharge/wb_A_v6q3.txt", header = TRUE)
wbal_B <- read.table(file = "~/mb_sensitivity/outputs/discharge/wb_B_v5q2.txt", header = TRUE)
wbal_C <- read.table(file = "~/mb_sensitivity/outputs/discharge/wb_C_v5q.txt", header = TRUE)
wbal_D <- read.table(file = "~/mb_sensitivity/outputs/discharge/wb_D_v5q.txt", header = TRUE)
wbal_F <- read.table(file = "~/mb_sensitivity/outputs/discharge/wb_F_v2q.txt", header = TRUE)
wbal_G <- read.table(file = "~/mb_sensitivity/outputs/discharge/wb_G_v2q.txt", header = TRUE)
wbal_H <- read.table(file = "~/mb_sensitivity/outputs/discharge/wb_H_v2q.txt", header = TRUE)

wbal_A$day <- rep(1:365, each = 24)
wbal_A <- aggregate(wbal_A$Total_surface_runoff, by = list(Category = wbal_A$day), FUN = sum)
colnames(wbal_A) <- c("day", "Total_surface_runoff")
wbal_A <- prob_exc(wbal_A)
for(i in 1:nrow(wbal_A)){
  wbal_A$cu_ro[i] <- sum(wbal_A$Total_surface_runoff[1:i])
}

wbal_B$day <- rep(1:365, each = 24)
wbal_B <- aggregate(wbal_B$Total_surface_runoff, by = list(Category = wbal_B$day), FUN = sum)
colnames(wbal_B) <- c("day", "Total_surface_runoff")
wbal_B <- prob_exc(wbal_B)
for(i in 1:nrow(wbal_B)){
  wbal_B$cu_ro[i] <- sum(wbal_B$Total_surface_runoff[1:i])
}

wbal_C$day <- rep(1:365, each = 24)
wbal_C <- aggregate(wbal_C$Total_surface_runoff, by = list(Category = wbal_C$day), FUN = sum)
colnames(wbal_C) <- c("day", "Total_surface_runoff")
wbal_C <- prob_exc(wbal_C)
for(i in 1:nrow(wbal_C)){
  wbal_C$cu_ro[i] <- sum(wbal_C$Total_surface_runoff[1:i])
}

wbal_D$day <- rep(1:365, each = 24)
wbal_D <- aggregate(wbal_D$Total_surface_runoff, by = list(Category = wbal_D$day), FUN = sum)
colnames(wbal_D) <- c("day", "Total_surface_runoff")
wbal_D <- prob_exc(wbal_D)
for(i in 1:nrow(wbal_D)){
  wbal_D$cu_ro[i] <- sum(wbal_D$Total_surface_runoff[1:i])
}

wbal_F$day <- rep(1:365, each = 24)
wbal_F <- aggregate(wbal_F$Total_surface_runoff, by = list(Category = wbal_F$day), FUN = sum)
colnames(wbal_F) <- c("day", "Total_surface_runoff")
wbal_F <- prob_exc(wbal_F)
for(i in 1:nrow(wbal_F)){
  wbal_F$cu_ro[i] <- sum(wbal_F$Total_surface_runoff[1:i])
}

wbal_G$day <- rep(1:365, each = 24)
wbal_G <- aggregate(wbal_G$Total_surface_runoff, by = list(Category = wbal_G$day), FUN = sum)
colnames(wbal_G) <- c("day", "Total_surface_runoff")
wbal_G <- prob_exc(wbal_G)
for(i in 1:nrow(wbal_G)){
  wbal_G$cu_ro[i] <- sum(wbal_G$Total_surface_runoff[1:i])
}

wbal_H$day <- rep(1:365, each = 24)
wbal_H <- aggregate(wbal_H$Total_surface_runoff, by = list(Category = wbal_H$day), FUN = sum)
colnames(wbal_H) <- c("day", "Total_surface_runoff")
wbal_H <- prob_exc(wbal_H)
for(i in 1:nrow(wbal_H)){
  wbal_H$cu_ro[i] <- sum(wbal_H$Total_surface_runoff[1:i])
}


wbal_A$scen <- "Homogeneous"
wbal_B$scen <- "Two-layered"
wbal_C$scen <- "Three-layered"
wbal_D$scen <- "Low-K Exponential Decay"
wbal_F$scen <- "Variable Soil"
wbal_G$scen <- "High-K Exponential Decay"
wbal_H$scen <- "Anisotropic"

outflow_all <- rbind(wbal_A,wbal_B,wbal_C,wbal_F,wbal_D,wbal_H, wbal_G)
outflow_precip <- full_join(outflow_all, precip_day, by = "day")


colnames(outflow_precip) <- c("day","runoff_m3","rank","prob_exc","cu_ro","scen","precip","cu_prec","date")

outflow_precip$runoff_mm <- outflow_precip$runoff_m3*(1000/(90*90*3948))
outflow_precip$runoff_m3h <- outflow_precip$runoff_m3/24
outflow_precip$cu_ro_mm <- outflow_precip$cu_ro*(1000/(90*90*3948))


# -------------------------------------------------------------------------------------------------
# plotting data - log-log
# -------------------------------------------------------------------------------------------------

flow_pub <- ggplot() + geom_line(data = outflow_precip, aes(x = prob_exc,y = runoff_m3h, group=scen,col = scen), size = 1) +
  scale_color_manual(values = c("purple","khaki4","black","darkorange","dodgerblue","firebrick","green3"), guide = guide_legend(ncol = 2))  + labs(color = "Scenario") +
  scale_y_log10(name="",limits = c(400,10000), expand=c(0,0), breaks = c(400,1000,2000,5000,10000),labels = scales::comma) +
  scale_x_log10(name="",limits = c(0.002,1), expand=c(0,0), breaks = c(0.002,0.01,0.05,0.1,0.5,1.0)) +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black", size=1, fill=NA), panel.grid.major = element_line(colour="grey", size=0.1), legend.position="none",
        legend.background = element_rect(linetype="solid", colour ="white"),plot.margin = margin(5,15,5,5),
        axis.text.x = element_text(color="black",size=12),axis.text.y = element_text(color="black",size=12),
        legend.text = element_text(color="black",size=12))
flow_pub


# -------------------------------------------------------------------------------------------------
# calculating sensitivity metric values
# -------------------------------------------------------------------------------------------------

sens_q <- data.frame(matrix(NA, nrow = 7, ncol = 4))
colnames(sens_q)<- c("scen","mean","sd","sens_q")

sens_q$scen <- c("Homogeneous", "Two-layered", "Three-layered", "Variable Soil", "Anisotropic", "Low-K Exp Decay", "High-K Exp Decay")

sens_q$mean[1] <- mean(outflow_precip$runoff_m3h[outflow_precip$scen == "Homogeneous"])
sens_q$sd[1] <- sd(outflow_precip$runoff_m3h[outflow_precip$scen == "Homogeneous"])

sens_q$mean[2] <- mean(outflow_precip$runoff_m3h[outflow_precip$scen == "Two-layered"])
sens_q$sd[2] <- sd(outflow_precip$runoff_m3h[outflow_precip$scen == "Two-layered"])

sens_q$mean[3] <- mean(outflow_precip$runoff_m3h[outflow_precip$scen == "Three-layered"])
sens_q$sd[3] <- sd(outflow_precip$runoff_m3h[outflow_precip$scen == "Three-layered"])

sens_q$mean[4] <- mean(outflow_precip$runoff_m3h[outflow_precip$scen == "Variable Soil"])
sens_q$sd[4] <- sd(outflow_precip$runoff_m3h[outflow_precip$scen == "Variable Soil"])

sens_q$mean[5] <- mean(outflow_precip$runoff_m3h[outflow_precip$scen == "Anisotropic"])
sens_q$sd[5] <- sd(outflow_precip$runoff_m3h[outflow_precip$scen == "Anisotropic"])

sens_q$mean[6] <- mean(outflow_precip$runoff_m3h[outflow_precip$scen == "Low-K Exponential Decay"])
sens_q$sd[6] <- sd(outflow_precip$runoff_m3h[outflow_precip$scen == "Low-K Exponential Decay"])

sens_q$mean[7] <- mean(outflow_precip$runoff_m3h[outflow_precip$scen == "High-K Exponential Decay"])
sens_q$sd[7] <- sd(outflow_precip$runoff_m3h[outflow_precip$scen == "High-K Exponential Decay"])

for(i in 1:nrow(sens_q)){
  pct_chg_mean <- abs((sens_q$mean[i] - sens_q$mean[1])/sens_q$mean[1])
  pct_chg_sd <- abs((sens_q$sd[i] - sens_q$sd[1])/sens_q$sd[1])
  sens_q$sens_q[i] <- pct_chg_mean + pct_chg_sd
}




