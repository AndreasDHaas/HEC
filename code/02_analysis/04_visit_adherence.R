#Packages 
  #install.packages("ggplot2")
  #install.packages("gridExtra")


#Clear workspace 
  rm(list =  setdiff(ls(), c("a")))

#Load library 
  library(ggplot2)
  library(plyr)
  library(gridExtra)

#Graphs
  gg <- "C:/Repositories/HEC/figures/"
  tt <- "C:/Repositories/HEC//tables/"

#WD
  setwd("E:/Data/PMTCT/HEC/hec")

#Plots (pre-Opton B+) 
  
  #Import test data date 
    adh <- read.dta("adh.dta")
  
  #add data values to bars 
  
    #position 
  
      #prc  
        adh <- ddply(adh, .(v), 
          transform, pos.prc = cumsum(prc) - (0.5 * prc) +1
        )
  
      #CI 
        adh <- ddply(adh, .(v), 
           transform, pos.ci = cumsum(prc) - (0.5 * prc) - 0.8
        )
      
  #plot 
      b <- ggplot(data = adh, aes(x = v, y = prc, fill = attend)) +
            geom_bar(stat="identity") + 
            scale_fill_manual(values=c("#d8b365", "#c7eae5")) +
            scale_x_discrete(limits=c(1:13), labels=c("1.5\n[1-2]","2.5\n[2-3]","3.5\n[3-4]","4.5\n[4-5]", "6\n[5-7]", "9\n[7-10]", "12\n[10-13]", "15\n[13-16]", "18\n[16-19]", "21\n[19-22]", "24\n[22-25]", "27\n[25-28]", "30\n[28-30]")) +
            scale_y_continuous(limits=c(-0.25, 100.25), expand = c(0, 0)) +
            labs(x = "Age (in months) [time window]", y = "Percentage", fill = "Attendance           ") +
            geom_text(data = adh, aes(x = v, y = pos.prc, label = lab_prc), size=2.5) +
            geom_text(data = adh, aes(x = v, y = pos.ci, label = lab_ci), size=2.5)
      
  #print plot 
      b
      
  #Save Plot
      tiff(filename=paste0(gg, "/adherence.tiff"), width = 6000, height =4500, units = "px", res=600)
      b  
      dev.off()
      
  #Combine
      grid.arrange(a, b, ncol=1, nrow =2)
 