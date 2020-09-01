# -------------------------------------------------------------------------------------------------
# Water table elevation calculation
# -------------------------------------------------------------------------------------------------

library(fields)   #for plotting the pfb file
library(ggplot2)
library(reshape2)
library(metR)
library(dplyr)
library(RColorBrewer) # adding radical color ramps
library(gridExtra) # arranging plots

source("~/mb_sensitivity/scripts/PFB-ReadFcn.R")

# setting file names and variables
# output files are not included but can be generated from mb_sensitivity/model_runs
press_file <- "~/parflow_outputs/H_v1_outputs/H_v1.out.press.01018.pfb"
satur_file <- "~/parflow_outputs/H_v1_outputs/H_v1.out.satur.01018.pfb"
nx <- 91
ny <- 70
nz <- 20

# reading layers
layers = read.delim("~/mb_sensitivity/scripts/supporting_files/layers.txt", header = TRUE, sep = "\t", dec = ".")
for(i in 1:nz){
  layers$depth_top[i] <- sum(c(layers$thickness[i:nz]))
  layers$depth_bot[i] <- sum(c(layers$thickness[(i+1):nz]))
}
layers$depth_bot[nz] <- 0
layers$layer <- 21 - layers$layer

# reading pressures and saturation files
press <- melt(data.frame(readpfb(press_file, verbose = F)))
satur <- melt(data.frame(readpfb(satur_file, verbose = F)))

press_sat.df <- data.frame(x=rep(1:nx),y=rep(1:ny,each=nx),z=rep(1:nz,each=nx*ny),
                         press=press$value,satur=satur$value)
system.time(
subset_particles <- subset(press_sat.df, z == 20))

# water table elevation function - takes a while, but you only need to do it once 
wt_elev.df <- data.frame(x=rep(1:nx),y=rep(1:ny,each=nx),wt_elev=0)

load("~/mb_sensitivity/scripts/supporting_files/watershed_mask.Rda")


system.time(
  for(i in 1:nx){
    print(paste("x =",i))
    for(j in 1:ny){
      if(watershed_mask$flowpath[watershed_mask$X_cell == i & watershed_mask$Y_cell == j] == 0){
        wt_elev.df$wt_elev[wt_elev.df$x == i & wt_elev.df$y == j] <- 9999
      } else {
        for(k in 1:nz){
          subset.df <- subset(press_sat.df, z == k)
          if(subset.df$satur[subset.df$x == i & subset.df$y == j] < 1){
            wt_elev.df$wt_elev[wt_elev.df$x == i & wt_elev.df$y == j] <-
              press_sat.df$press[press_sat.df$x == i & press_sat.df$y == j & press_sat.df$z == (k-1)] +
              (layers$depth_bot[layers$layer == (k-1)]+layers$depth_top[layers$layer == (k-1)])/2
            break
          } else if(subset.df$satur[subset.df$x == i & subset.df$y == j] == 1 & k ==20){
            wt_elev.df$wt_elev[wt_elev.df$x == i & wt_elev.df$y == j] <-
              press_sat.df$press[press_sat.df$x == i & press_sat.df$y == j & press_sat.df$z == k] +
              layers$depth_top[layers$layer == k]
          } 
        }
      }
    }
  })


load("~/mb_sensitivity/scripts/supporting_files/domain_pr_df.Rda")

wt_elev.df2 <- inner_join(wt_elev.df, slopes, by = c("x" = "X_cell","y" = "Y_cell"))
wt_elev.df2$wt_elev <- wt_elev.df2$wt_elev + wt_elev.df2$elev - 1000
wt_elev.df2$dtw <- wt_elev.df2$elev - wt_elev.df2$wt_elev

#load("~/research/domain/watershed_mask.Rda")
wt_elev.df3 <- inner_join(wt_elev.df2, watershed_mask, by = c("x" = "X_cell","y" = "Y_cell"))
wt_elev.df3$dtw[wt_elev.df3$flowpath == 0] <- 9999


### loading other water table files
load(file="~/mb_sensitivity/outputs/water_tables/wt_A_v5_991.df.Rda")
load(file="~/mb_sensitivity/outputs/water_tables/wt_B_v4_1037.df.Rda")
load(file="~/mb_sensitivity/outputs/water_tables/wt_C_v4_1036.df.Rda")
load(file="~/mb_sensitivity/outputs/water_tables/wt_D_v4_993.df.Rda")
load(file="~/mb_sensitivity/outputs/water_tables/wt_F_v1_997.df.Rda")
load(file="~/mb_sensitivity/outputs/water_tables/wt_G_v1_0993.df.Rda")
load(file="~/mb_sensitivity/outputs/water_tables/wt_H_v1_1018.df.Rda")

# -------------------------------------------------------------------------------------------------
# Plotting
# -------------------------------------------------------------------------------------------------

wt_elev.df3 <- wt_D_v4_993.df

wt_elev.df3$dtw_cuts <- cut(wt_elev.df3$dtw, c(-1,0,2,5,10,20,50,100,200,300,400,1000,Inf), include.lowest = TRUE)
levels(wt_elev.df3$dtw_cuts)

### saving water table file if necessary
# wt_G_v1_0993.df <- wt_elev.df3
# save(wt_G_v1_0993.df, file="~/mb_sensitivity/outputs/water_tables/wt_G_v1_0993.df.Rda")

plot2_colors <- brewer.pal(11, "Spectral")
plot2_colors <- plot2_colors[11:1]
wt_dtw_binplot2 <- ggplot(wt_elev.df3, aes(X.x, Y.x)) + geom_tile(aes(fill = factor(dtw_cuts)), colour = "black") + labs(fill = "Depth to Water (m)") +
  scale_fill_manual(values=c("midnightblue",plot2_colors[2:11],"gray50"),
                    labels=c("< 0","0 - 2","2 - 5","5 - 10","10 - 20","20 - 50","50 - 100","100 - 200","200 - 300","300 - 400","> 400","Outside of Main Basin")) +   #,">400"
  scale_x_continuous(name="X (m)",expand=c(0,0),breaks=c(seq(0,8200,1000)),labels = scales::comma) + 
  scale_y_continuous(name="Y (m)",expand=c(0,0),breaks=c(seq(0,6000,1000)),labels = scales::comma) +
  ggtitle(paste("")) + theme_bw() +
  theme(panel.border = element_rect(colour = "black", size=1, fill=NA), panel.grid.major = element_line(colour="grey", size=0.1), legend.position = "right",
        legend.background = element_rect(linetype="solid", colour ="white"),plot.margin = margin(5,15,5,5),
        title =element_text(size=12),axis.text.x = element_text(color="black",size=10),axis.text.y = element_text(color="black",size=10),legend.text = element_text(color="black",size=12,face = "bold"))
wt_dtw_binplot2


# dtw figures for paper
wt_dtw_pubA <- ggplot(wt_A_v5_991.df, aes(X.x, Y.x)) + geom_tile(aes(fill = dtw_cuts), colour = "black", size = 0.05) + labs(fill = "Depth to Water (m)") +
  scale_fill_manual(values=c("midnightblue",plot2_colors[2:11],"white"), guide = guide_legend(ncol = 12),
                    labels=c("                                 ","                                 ","                                 ","                                 ","                                 ","                                 ","                                 ","                                 ","                                 ",
                             "                                 ","                                 ","                                 ")) +
  scale_x_continuous(name="",expand=c(0,0)) + 
  scale_y_continuous(name="",expand=c(0,0)) + theme_bw() +
  theme(panel.border = element_rect(colour = "black", size=0.5, fill=NA), panel.grid.major = element_line(colour="grey", size=0.1), legend.position="none",
        legend.text = element_text(color="black",size=10),axis.ticks.x=element_blank(),axis.ticks.y=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank())
wt_dtw_pubA

wt_dtw_pubB <- ggplot(wt_B_v4_1037.df, aes(X.x, Y.x)) + geom_tile(aes(fill = dtw_cuts), colour = "black", size = 0.05) + labs(fill = "Depth to Water (m)") +
  scale_fill_manual(values=c("midnightblue",plot2_colors[2:11],"white"),
                    labels=c("< 0","0 - 2","2 - 5","5 - 10","10 - 20","20 - 50","50 - 100","100 - 200","200 - 300","300 - 400","> 400","Outside of Main Basin")) +
  scale_x_continuous(name="",expand=c(0,0)) + 
  scale_y_continuous(name="",expand=c(0,0)) + theme_bw() +
  theme(panel.border = element_rect(colour = "black", size=0.5, fill=NA), panel.grid.major = element_line(colour="grey", size=0.1), legend.position="none",
        legend.text = element_text(color="black",size=12),axis.ticks.x=element_blank(),axis.ticks.y=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank())
wt_dtw_pubB

wt_dtw_pubC <- ggplot(wt_C_v4_1036.df, aes(X.x, Y.x)) + geom_tile(aes(fill = dtw_cuts), colour = "black", size = 0.05) + labs(fill = "Depth to Water (m)") +
  scale_fill_manual(values=c("midnightblue",plot2_colors[2:11],"white"),
                    labels=c("< 0","0 - 2","2 - 5","5 - 10","10 - 20","20 - 50","50 - 100","100 - 200","200 - 300","300 - 400","> 400","Outside of Main Basin")) +
  scale_x_continuous(name="",expand=c(0,0)) + 
  scale_y_continuous(name="",expand=c(0,0)) + theme_bw() +
  theme(panel.border = element_rect(colour = "black", size=0.5, fill=NA), panel.grid.major = element_line(colour="grey", size=0.1), legend.position="none",
        legend.text = element_text(color="black",size=12),axis.ticks.x=element_blank(),axis.ticks.y=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank())
wt_dtw_pubC

wt_dtw_pubF <- ggplot(wt_F_v1_997.df, aes(X.x, Y.x)) + geom_tile(aes(fill = dtw_cuts), colour = "black", size = 0.05) + labs(fill = "Depth to Water (m)") +
  scale_fill_manual(values=c("midnightblue",plot2_colors[2:11],"white"),
                    labels=c("< 0","0 - 2","2 - 5","5 - 10","10 - 20","20 - 50","50 - 100","100 - 200","200 - 300","300 - 400","> 400","Outside of Main Basin")) +
  scale_x_continuous(name="",expand=c(0,0)) + 
  scale_y_continuous(name="",expand=c(0,0)) + theme_bw() +
  theme(panel.border = element_rect(colour = "black", size=0.5, fill=NA), panel.grid.major = element_line(colour="grey", size=0.1), legend.position="none",
        legend.text = element_text(color="black",size=12),axis.ticks.x=element_blank(),axis.ticks.y=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank())
wt_dtw_pubF

wt_dtw_pubH <- ggplot(wt_H_v1_1018.df, aes(X.x, Y.x)) + geom_tile(aes(fill = dtw_cuts), colour = "black", size = 0.05) + labs(fill = "Depth to Water (m)") +
  scale_fill_manual(values=c("midnightblue",plot2_colors[2:11],"white"),
                    labels=c("< 0","0 - 2","2 - 5","5 - 10","10 - 20","20 - 50","50 - 100","100 - 200","200 - 300","300 - 400","> 400","Outside of Main Basin")) +
  scale_x_continuous(name="",expand=c(0,0)) + 
  scale_y_continuous(name="",expand=c(0,0)) + theme_bw() +
  theme(panel.border = element_rect(colour = "black", size=0.5, fill=NA), panel.grid.major = element_line(colour="grey", size=0.1), legend.position="none",
        legend.text = element_text(color="black",size=12),axis.ticks.x=element_blank(),axis.ticks.y=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank())
wt_dtw_pubH

wt_dtw_pubD <- ggplot(wt_D_v4_993.df, aes(X.x, Y.x)) + geom_tile(aes(fill = dtw_cuts), colour = "black", size = 0.05) + labs(fill = "Depth to Water (m)") +
  scale_fill_manual(values=c("midnightblue",plot2_colors[2:10],"white"),
                    labels=c("< 0","0 - 2","2 - 5","5 - 10","10 - 20","20 - 50","50 - 100","100 - 200","200 - 300","300 - 400","> 400","Outside of Main Basin")) +
  scale_x_continuous(name="",expand=c(0,0)) + 
  scale_y_continuous(name="",expand=c(0,0)) + theme_bw() +
  theme(panel.border = element_rect(colour = "black", size=0.5, fill=NA), panel.grid.major = element_line(colour="grey", size=0.1), legend.position="none",
        legend.text = element_text(color="black",size=12),axis.ticks.x=element_blank(),axis.ticks.y=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank())
wt_dtw_pubD

grid.arrange(wt_dtw_pubA,wt_dtw_pubB,wt_dtw_pubC,wt_dtw_pubF,wt_dtw_pubH,wt_dtw_pubD, nrow = 2)


# -------------------------------------------------------------------------------------------------
# plotting High-K exponential decay scenario separately
# -------------------------------------------------------------------------------------------------

wt_dtw_pubG <- ggplot(wt_G_v1_0993.df, aes(X.x, Y.x)) + geom_tile(aes(fill = dtw_cuts), colour = "black", size = 0.05) + labs(fill = "Depth to Water (m)") +
  scale_fill_manual(values=c("midnightblue",plot2_colors[2:11],"white"),
                    labels=c("< 0","0 - 2","2 - 5","5 - 10","10 - 20","20 - 50","50 - 100","100 - 200","200 - 300","300 - 400","> 400","Outside of Main Basin")) +
  scale_x_continuous(name="",expand=c(0,0)) + 
  scale_y_continuous(name="",expand=c(0,0)) + theme_bw() +
  theme(panel.border = element_rect(colour = "black", size=0.5, fill=NA), panel.grid.major = element_line(colour="grey", size=0.1), legend.position="none",
        legend.text = element_text(color="black",size=12),axis.ticks.x=element_blank(),axis.ticks.y=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank())
wt_dtw_pubG


# -------------------------------------------------------------------------------------------------
# calculating sensitivity metric values
# -------------------------------------------------------------------------------------------------

sens_dtw <- data.frame(matrix(NA, nrow = 7, ncol = 4))
colnames(sens_dtw)<- c("scen","mean","sd","sens_dtw")

sens_dtw$scen <- c("Homogeneous", "Two-layered", "Three-layered", "Variable Soil", "Anisotropic", "Low-K Exp Decay", "High-K Exp Decay")

sens_dtw$mean[1] <- mean(wt_A_v5_991.df$dtw[wt_A_v5_991.df$dtw < 9999.0])
sens_dtw$sd[1] <- sd(wt_A_v5_991.df$dtw[wt_A_v5_991.df$dtw < 9999.0])

sens_dtw$mean[2] <- mean(wt_B_v4_1037.df$dtw[wt_B_v4_1037.df$dtw < 9999.0])
sens_dtw$sd[2] <- sd(wt_B_v4_1037.df$dtw[wt_B_v4_1037.df$dtw < 9999.0])

sens_dtw$mean[3] <- mean(wt_C_v4_1036.df$dtw[wt_C_v4_1036.df$dtw < 9999.0])
sens_dtw$sd[3] <- sd(wt_C_v4_1036.df$dtw[wt_C_v4_1036.df$dtw < 9999.0])

sens_dtw$mean[4] <- mean(wt_F_v1_997.df$dtw[wt_F_v1_997.df$dtw < 9999.0])
sens_dtw$sd[4] <- sd(wt_F_v1_997.df$dtw[wt_F_v1_997.df$dtw < 9999.0])

sens_dtw$mean[5] <- mean(wt_H_v1_1018.df$dtw[wt_H_v1_1018.df$dtw < 9999.0])
sens_dtw$sd[5] <- sd(wt_H_v1_1018.df$dtw[wt_H_v1_1018.df$dtw < 9999.0])

sens_dtw$mean[6] <- mean(wt_D_v4_993.df$dtw[wt_D_v4_993.df$dtw < 9999.0])
sens_dtw$sd[6] <- sd(wt_D_v4_993.df$dtw[wt_D_v4_993.df$dtw < 9999.0])

sens_dtw$mean[7] <- mean(wt_G_v1_0993.df$dtw[wt_G_v1_0993.df$dtw < 9999.0])
sens_dtw$sd[7] <- sd(wt_G_v1_0993.df$dtw[wt_G_v1_0993.df$dtw < 9999.0])

for(i in 1:nrow(sens_dtw)){
  pct_chg_mean <- abs((sens_dtw$mean[i] - sens_dtw$mean[1])/sens_dtw$mean[1])
  pct_chg_sd <- abs((sens_dtw$sd[i] - sens_dtw$sd[1])/sens_dtw$sd[1])
  sens_dtw$sens_dtw[i] <- pct_chg_mean + pct_chg_sd
}




