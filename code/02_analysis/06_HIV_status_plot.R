
#Clear workspace 
rm(list = ls())

#Load library 
library(ggplot2)
library(cowplot)
library(scales)
library(foreign)

#Graphs
gg <- "C:/Repositories/HEC/figures/"
tt <- "C:/Repositories/HEC//tables/"

#WD
setwd("E:/Data/PMTCT/HEC/hec")

#Data: 
HIV <- read.dta("HIV_est.dta")
head(HIV, 10)
HIV$est <- factor(HIV$est)

#Change the order of the legend
levels(HIV$est)
HIV$est <- factor(HIV$est, levels = rev(levels(HIV$est)))

hiv <-  ggplot(HIV, aes(month, cif, group=est, color=est)) +
        geom_ribbon(aes(ymin=cif_l, ymax=cif_u), alpha=.2, colour=NA) +
        geom_line(aes(color=est), size=1.3) +
        ylab("Cumulative incidence of HIV infection") + xlab("Age (in months)") +
        scale_x_continuous(breaks=c(0, 6, 12, 18, 24, 30),
        labels=c("0", "6", "12", "18", "24", "30"), limits=c(0,30)) +
        scale_color_manual(values=c("#8c510a", "#5ab4ac")) +
        scale_y_continuous(labels = percent_format()) +
        theme(legend.position=c(0.10, .95),legend.title=element_blank(),
          axis.title=element_text(size=15), 
          axis.text=element_text(size=15), 
          legend.text=element_text(size=13)) 

hiv

#Save Plot
tiff(filename=paste0(gg, "/hiv.tiff"), width = 4666, height =3500, units = "px", res=600)
hiv
dev.off()


HIV
  