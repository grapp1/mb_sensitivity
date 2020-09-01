# -------------------------------------------------------------------------------------------------
# plotting sensitivity metric values
# -------------------------------------------------------------------------------------------------

# refer to the following scripts for the calculation of the metrics:
# discharge: streamflow_analysis.R
# flowpaths: particle_analysis_1.R
# water tables: water_table_calc.R

library(plotly)
library(ggplot2)

sensitivity_all <- full_join(sens_fp, sens_q, by = "scen")
sensitivity_all <- full_join(sensitivity_all, sens_dtw, by = "scen")

sens_fig1 <- ggplot(sensitivity_all, aes(x = sens_fp,y = sens_dtw)) + geom_point(shape=21, fill=c("black","firebrick","dodgerblue","green","purple","orange","khaki4"), 
                                                        size = 6, stroke = 1) + 
  scale_x_continuous(name="Flowpath sensitivity",limits = c(0,2), breaks = seq(0,2,0.5)) +
  ggtitle("") + 
  scale_y_continuous(name="Water table sensitivity", limits = c(0,2), breaks = seq(0,2,0.5)) + 
  expand_limits(x = 0, y = 0) + theme_bw() + 
  theme(panel.border = element_rect(colour = "black", size=1, fill=NA), panel.grid.major = element_line(colour="grey", size=0.1), legend.position = "none",
        legend.background = element_rect(linetype="solid", colour ="white"),plot.margin = margin(5,15,5,5),
        title =element_text(size=12, face='bold'),axis.text.x = element_text(color="black",size=12),axis.text.y = element_text(color="black",size=12),legend.text = element_text(color="black",size=12,face = "bold"))

sens_fig2 <- ggplot(sensitivity_all, aes(x = sens_q,y = sens_dtw)) + geom_point(shape=21, fill=c("black","firebrick","dodgerblue","green","purple","orange","khaki4"), 
                                                                          size = 6, stroke = 1) + 
  scale_x_continuous(name="Discharge sensitivity",limits = c(0,2), breaks = seq(0,2,0.5)) +
  ggtitle("") + 
  scale_y_continuous(name="Water table sensitivity", limits = c(0,2), breaks = seq(0,2,0.5)) +
  expand_limits(x = 0, y = 0) + theme_bw() + 
  theme(panel.border = element_rect(colour = "black", size=1, fill=NA), panel.grid.major = element_line(colour="grey", size=0.1), legend.position = "none",
        legend.background = element_rect(linetype="solid", colour ="white"),plot.margin = margin(5,15,5,5),
        title =element_text(size=12, face='bold'),axis.text.x = element_text(color="black",size=12),axis.text.y = element_text(color="black",size=12),legend.text = element_text(color="black",size=12,face = "bold"))

sens_fig3 <- ggplot(sensitivity_all, aes(x = sens_fp,y = sens_q)) + geom_point(shape=21, fill=c("black","firebrick","dodgerblue","green","purple","orange","khaki4"), 
                                                                          size = 6, stroke = 1) + 
  scale_x_continuous(name="Flowpath sensitivity",limits = c(0,2), breaks = seq(0,2,0.5)) +
  ggtitle("") + 
  scale_y_continuous(name="Discharge sensitivity", limits = c(0,2), breaks = seq(0,2,0.5)) +
  expand_limits(x = 0, y = 0) + theme_bw() + 
  theme(panel.border = element_rect(colour = "black", size=1, fill=NA), panel.grid.major = element_line(colour="grey", size=0.1), legend.position = "none",
        legend.background = element_rect(linetype="solid", colour ="white"),plot.margin = margin(5,15,5,5),
        title =element_text(size=12, face='bold'),axis.text.x = element_text(color="black",size=12),axis.text.y = element_text(color="black",size=12),legend.text = element_text(color="black",size=12,face = "bold"))

grid.arrange(sens_fig1,sens_fig2,sens_fig3, nrow = 1)



