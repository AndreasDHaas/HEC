#Packages 
  #install.packages("mstate")
  #install.packages("transMat")
  #install.packages("colorspace")
  #install.packages('testit')
  #install.packages("gridExtra")
  #install.packages("tidyr")
  #install.packages("xlsx")
  #install.packages("cowplot")

#Clear workspace 
  rm(list = ls())

#Load library 
  library(foreign)
  library(mstate)
  library(colorspace)
  library(testit)
  library(gridExtra)
  library(tidyr)
  library(ggplot2)
  library(datasets)
  library(xlsx)
  library(cowplot)
  library(scales)
  
#Graphs
  gg <- "C:/Repositories/HEC/figures/"
  tt <- "C:/Repositories/HEC//tables/"
  
#WD
  setwd("E:/Data/PMTCT/HEC/hec")

### Multistate model for HIV exposed infants 
  
  #States (n=5)
    #1. Not enrolled 
    #2. Under follow-up 
    #3. LTF
    #4. Discharged
    #5. Dead
    #6. Started ART
  
  #Transitions (n=4) 

    #			        			              		  |----------------[2]Loss to follow-up 
    #Birth----------[1]Enrolled & retained -|----------------[3]Dead 				      	
    #										                    |----------------[4]Discharged
    #                                       |----------------[5]Sarted ART

  #Data: 
    wide <- read.dta("hcc_states.dta")
    head(wide, 10)
    
  #Median (IQR) Age at censoring 
    quantile(wide$LTF60_t, c(.50, .25, .75))
    
  #Median (IQR) time from enrolment to censoring 
    quantile((wide$LTF60_t-wide$enrol_t), c(.50, .25, .75))

  #Transition matrix 
    tmat <- transMat(x = list(c(2), c(3,4,5,6), c(), c(), c(), c()), names = c("Birth", "FUP", "LTF", "DIS", "Death", "ART")) 
    tmat
    
  #Covariates 
    cov <- c("SEX", "EA", "BW", "AP", "AL", "AB", "ABC", "LOC")
    
  #Reshape data form wide to long format
    long <- msprep(data = wide, trans = tmat, time =c(NA, "enrol_t", "LTF60_t", "dis_t", "dead_t", "ART_t"), 
            status=c(NA, "enrol_s", "LTF60_s", "dis_s", "dead_s", "ART_s"), keep=c(cov, "id", "EA_r"))
            head(long,20)
            
  #Append transition-specific covariats 
    long <- expand.covs(long, cov, append=TRUE, longnames=FALSE)
    head(long)
    
  #Assert that Tstart < Tstop
    assert("Tstart < Tstop", long[,5] < long[,6])

  #Estimate cumulative hazards with Cox ph-model
    c1 <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans), data = long, method = "breslow")
       
   #Estimate transition hazards with msfit function of mstate package 
    msf1 <- msfit(object = c1, vartype = "aalen", trans = tmat)
  
   #Plot cumulative hazards
    plot(msf1, las = 1, lty = rep(1:2, c(8, 4)), xlab = "Days since birth")
      
  #Calculate trasition probabilities 
    pt1 <- probtrans(msf1, predt = 0, method = "aalen")
      
  #Extract cumulative probabilities from list 
    est <- pt1[[1]]                                     # extract dataframe from list
      
  #Reshape long 
    est$factor <- factor(est$time)
    est_long <- gather(est, outcome, prob, pstate1:pstate6, factor_key=TRUE)
    keep <- c("time", "outcome", "prob", "factor")
    l <- est_long[keep]
    
  #Order 
    l$order[l$outcome == "pstate1"] <- "1"
    l$order[l$outcome == "pstate2"] <- "2"
    l$order[l$outcome == "pstate4"] <- "3"
    l$order[l$outcome == "pstate3"] <- "4"
    l$order[l$outcome == "pstate6"] <- "5"
    l$order[l$outcome == "pstate5"] <- "6"
    l <- l[order(l$order, l$time),] 
      
  #order of legend 
    l$outcome <- factor(l$outcome, levels=c("pstate5", "pstate6", "pstate3", "pstate4", "pstate2", "pstate1"))

  #Plot 
    a <- ggplot(l, aes(x=time,y=prob,group=outcome,fill=outcome)) +
         geom_area(position="fill") +
         scale_x_discrete(limits=c(0, 6, 12, 18, 24, 30), expand = c(0, 0)) +
         scale_y_continuous(labels = percent_format(), limits=c(-0.005, 1.005), expand = c(0, 0)) +
         labs(x = "Age (in months)", y = "Percentage of children", fill = "State") +
         scale_fill_manual(values=c("#545454", "#8c510a", "#d8b365", "#5ab4ac", "#c7eae5", "#f0f0f0"), labels=c("Died", "Initiated ART", "Lost to follow-up" ,"Discharged HIV-","Retained in care", "Not yet enrolled")) +
         theme(axis.title=element_text(size=15), axis.text=element_text(size=15))
    
   #Print plot
     a
      
   #Save Plot
     tiff(filename=paste0(gg, "/status.tiff"), width = 4666, height =3500, units = "px", res=600)
     a  
     dev.off()
    
    ###Point estimates and CIs at 6, 12, 18, 24 and 30 months  
      
        #At relevant time points
          est$time2 <- round(est$time, digits=2)
          est <-  est[est$time2 %in% c(6, 12, 18, 24, 30.03), ]   # extract rows for months 

        #Calculate CIs, round, multiply by 100
            for (i in 1:6) {
            pstate <- paste0("pstate", i)
            cil <- paste0("p", i, "_cil")
            ciu <- paste0("p", i, "_ciu")
            se <- paste0("se", i)
            est[,cil] <- round((est[,pstate] - 1.96* est[,se])*100, digits=1)   # calculate 95%-CIs
            est[,ciu] <- round((est[,pstate] + 1.96* est[,se])*100, digits=1)   # calculate 95%-CIs
            est[,pstate] <- round((est[,pstate] *100), digits=1)   # calculate 95%-CIs
          }
         
        #Results 
            #1. Not enrolled 
            not_enrolled <- subset(est, select = c(time, pstate1, p1_cil, p1_ciu)) 
            not_enrolled
            
           #2. Enrolled & under follow-up 
            ric <- subset(est, select = c(time, pstate2, p2_cil, p2_ciu)) 
            ric
            
           #3. LTF
            ltf <- subset(est, select = c(time, pstate3, p3_cil, p3_ciu)) 
            ltf
            
           #4. Discharged
            dis <- subset(est, select = c(time, pstate4, p4_cil, p4_ciu)) 
            dis
            
           #5. Dead
            dead <- subset(est, select = c(time, pstate5, p5_cil, p5_ciu)) 
            dead
            
          #6. ART
            art <- subset(est, select = c(time, pstate6, p6_cil, p6_ciu)) 
            art
            
          #Transitions
            #1. Birth -> Enrolment 
            #2. Enrolment -> LTF
            #3. Enrolment -> Discharge 
            #4. Enrolment -> Death 

###----------------------------Univariable coxph model ---------------------------###


#SEX (#level.#transition)
uSEX <- coxph(Surv(Tstart, Tstop, status) ~  
                SEX1.1 + SEX2.1 +
                SEX1.2 + SEX2.2 +
                SEX1.3 + SEX2.3 +
                SEX1.4 + SEX2.4 +  
                SEX1.5 + SEX2.5 +  
                strata(trans), data = long, method = "breslow")

#Birth weight 
uBW <- coxph(Surv(Tstart, Tstop, status) ~  
               BW1.1 + BW2.1 +
               BW1.2 + BW2.2 +
               BW1.3 + BW2.3 +
               BW1.4 + BW2.4 +  
               BW1.5 + BW2.5 +  
               strata(trans), data = long, method = "breslow") 

#ARVs during pregnancy 
uAP <- coxph(Surv(Tstart, Tstop, status) ~  
               AP1.1 + AP2.1 + AP3.1 +
               AP1.2 + AP2.2 + AP3.2 +
               AP1.3 + AP2.3 + AP3.3 +  
               AP1.4 + AP2.4 + AP3.4 +   
               AP1.5 + AP2.5 + AP3.5 +  
               strata(trans), data = long, method = "breslow") 

#ARVs during labour 
uAL <- coxph(Surv(Tstart, Tstop, status) ~  
               AL1.1 + AL2.1 + AL3.1 +
               AL1.2 + AL2.2 + AL3.2 +
               AL1.3 + AL2.3 + AL3.3 +  
               AL1.4 + AL2.4 + AL3.4 +   
               AL1.5 + AL2.5 + AL3.5 +   
               strata(trans), data = long, method = "breslow") 

#ARVs at birth for baby 
uAB <- coxph(Surv(Tstart, Tstop, status) ~  
               AB1.1 + AB2.1 +
               AB1.2 + AB2.2 +
               AB1.3 + AB2.3 +
               AB1.4 + AB2.4 +  
               AB1.5 + AB2.5 + 
               strata(trans), data = long, method = "breslow") 

#ARVs for baby after birth 
uABC <- coxph(Surv(Tstart, Tstop, status) ~  
               ABC1.1 + ABC2.1 +
               ABC1.2 + ABC2.2 +
               ABC1.3 + ABC2.3 +
               ABC1.4 + ABC2.4 +  
               ABC1.5 + ABC2.5 + 
               strata(trans), data = long, method = "breslow") 

#Level of care
uLOC <- coxph(Surv(Tstart, Tstop, status) ~  
                LOC1.1 + LOC2.1 + LOC3.1 +
                LOC1.2 + LOC2.2 + LOC3.2 +
                LOC1.3 + LOC2.3 + LOC3.3 +  
                LOC1.4 + LOC2.4 + LOC3.4 +  
                LOC1.5 + LOC2.5 + LOC3.5 + 
                strata(trans), data = long, method = "breslow") 

#Enrollment age  
uEA <- coxph(Surv(Tstart, Tstop, status) ~  
               EA.2 +
               EA.3 +
               EA.4 +
               EA.5 +
               strata(trans), data = long, method = "breslow") 

#-----------wrirte estimates in table--------------#

###------------factors--------------###

#Check which covariates are factor variabls    
covs <- wide[cov]
factors <- sapply(covs, is.factor)
factors <- which(factors)
fnames <- names(factors)

#---generate CIs and rounded HRs---#

for (covariate in fnames){
print(covariate)  
obj <- paste0("u", covariate) # define name of R object (i.e. uSEX) 
print(obj)
c <- summary(get(obj))$coefficients[, 1] #oefficients
hr <- format(round(summary(get(obj))$coefficients[, 2], 2), nsmall = 2) #HR = exp(coef)
se <- summary(get(obj))$coefficients[, 3] #SE (coef)
p <- format(round(summary(get(obj))$coefficients[, 5], 4), nsmall = 4) #p-Value
lb <- exp(c - 1.96 * se) #lower bound  of CI
ub <- exp(c + 1.96 * se) #upper bound of CI
ub[ub>100] = 100
ci <- paste0("(", format(round(lb, 2), nsmall = 2), "-", format(round(ub, 2), nsmall = 2), ")") #formated CI
assign(obj, cbind(hr, ci, p))
print(get(obj))
}

#---Put estimates for each transition in seperate matrix---#

#matrix for each transition 
for (trans in 1:5){
  print(paste("Transition:", trans))
  regex <- paste0(".", trans, "$")
  print(paste("Regular expression:", regex))
  for (covariate in fnames){
    print(covariate)  
    obj <- paste0("u", covariate) # define name of R object (i.e. uSEX) 
    print(obj)
    assign(paste0(obj, "s", trans), get(obj)[grepl(regex, rownames(get(obj))), ])
    print(get(paste0(obj, "s", trans)))
  }
}

#---Add rows for baseline levels of factor variables and lable levels---#

#Put labels in columns
j = 0 
for (covariate in fnames){
  j <- j+1 
  i = factors[j]
  print(covariate)  
  assign(paste0("lab", covariate), matrix(paste0("   ", levels(covs[,i])), ncol=1))
  print(get(paste0("lab", covariate)))
}

#Add rows for baseline level and title row 
base <- c("1", "", "")
for (covariate in fnames){  
  title <- c(covariate, "", "", "")
    for (trans in 1:5){
      obj <- paste0("u", covariate, "s", trans) # define name of R object (i.e. uSEXs1) 
        if (any(grep(covariate, obj)) == 1) { # if varname is in object 
          assign(obj, rbind(base, get(obj))) # add base row 
          labelling <- get(obj) # put object in dummy
          rownames(labelling)[1] <- paste0(covariate, "0.", trans) # assign rownames to dummy
          assign(obj, labelling) # overwrite object with dummy 
          assign(obj, cbind(get(paste0("lab", covariate)), get(obj))) 
          assign(obj, rbind(title, get(obj)))
        }
    }
}

#Print without NAs 
print(as.matrix(uABCs5), na.print="", quote =FALSE)

###------------continous--------------###

#Check which covariates are factor variabls    
continous <- sapply(covs, is.numeric)
continous <- which(continous)
cnames <- names(continous)

#---generate CIs and rounded HRs---#

for (covariate in cnames){
  print(covariate)  
  obj <- paste0("u", covariate) # define name of R object (i.e. uSEX) 
  print(obj)
  c <- summary(get(obj))$coefficients[, 1] #oefficients
  hr <- format(round(summary(get(obj))$coefficients[, 2], 2), nsmall = 2) #HR = exp(coef)
  se <- summary(get(obj))$coefficients[, 3] #SE (coef)
  p <- format(round(summary(get(obj))$coefficients[, 5], 4), nsmall = 4) #p-Value
  lb <- exp(c - 1.96 * se) #lower bound  of CI
  ub <- exp(c + 1.96 * se) #upper bound of CI
  ci <- paste0("(", format(round(lb, 2), nsmall = 2), "-", format(round(ub, 2), nsmall = 2), ")") #formated CI
  assign(obj, cbind(hr, ci, p))
  print(get(obj))
}

#---Put estimates for each transition in seperate matrix---#

#matrix for each transition 
for (trans in 1:5){
  print(paste("Transition:", trans))
  regex <- paste0(".", trans, "$")
  print(paste("Regular expression:", regex))
  for (covariate in cnames){
    print(covariate)  
    obj <- paste0("u", covariate) # define name of R object (i.e. uSEX) 
    print(obj)
    assign(paste0(obj, "s", trans), get(obj)[grepl(regex, rownames(get(obj))), ])
    print(get(paste0(obj, "s", trans)))
  }
}

#---Add lable and columname ---#

#Put labels in columns
for (covariate in cnames){
  print(covariate)  
  assign(paste0("lab", covariate), matrix(covariate, ncol=1))
  print(get(paste0("lab", covariate)))
}

#Add title column 
for (covariate in cnames){  
  for (trans in 2:5){
    obj <- paste0("u", covariate, "s", trans) # define name of R object (i.e. uSEXs1) 
    if (any(grep(covariate, obj)) == 1) { # if varname is in object 
      assign(obj, c(get(paste0("lab", covariate)), get(obj))) 
    }
  }
}

###--------------Prepare columns for final table---------------------####

###-------Transition 1--------###

#Empty placeholder for EA
d_EA <- c("EA", "NA", "", "")

#Append rows 
u1 <- rbind(uSEXs1, d_EA, uBWs1, uAPs1, uALs1, uABs1, uABCs1, uLOCs1)

#Include empty columns 
u1 <- cbind(u1[,1], "", u1[,2], u1[,3])

#Overwrite rownames 
rownames(u1) <- rep("", nrow(u1))

#Print without NAs 
print(as.matrix(u1), na.print="", quote =FALSE)

###-------Transition 2-4--------###

for (i in 2:5){
    #Append rows 
     assign(paste0("u", i), rbind(get(paste0("uSEXs", i)), get(paste0("uEAs", i)), 
                                  get(paste0("uBWs", i)),  get(paste0("uAPs", i)), get(paste0("uALs", i)),
                                  get(paste0("uABs", i)),  get(paste0("uABCs", i)), get(paste0("uLOCs", i)) 
                                 )
            ) 
    #Include empty columns 
    assign(paste0("u", i), cbind(get(paste0("u", i))[,1], "", get(paste0("u", i))[,2],  
                                 get(paste0("u", i))[,3]
                                 )
           )
}
      
#Overwrite rownames 
rownames(u2) <- rep("", nrow(u2))
rownames(u3) <- rep("", nrow(u3))
rownames(u4) <- rep("", nrow(u4))
rownames(u5) <- rep("", nrow(u5))

#Print without NAs 
print(as.matrix(u1), na.print="", quote =FALSE) #1. Birth -> enrolment 
print(as.matrix(u2), na.print="", quote =FALSE) #2. Enrolment -> LTF
print(as.matrix(u3), na.print="", quote =FALSE) #3. Enrolment -> Discharge 
print(as.matrix(u4), na.print="", quote =FALSE) #4. Enrolment -> Death 
print(as.matrix(u5), na.print="", quote =FALSE) #4. Enrolment -> ART 

###----------------------------Multivariable coxph model ---------------------------###

multi <- coxph(Surv(Tstart, Tstop, status) ~  
                SEX1.1 + SEX2.1 +
                SEX1.2 + SEX2.2 +
                SEX1.3 + SEX2.3 +
                SEX1.4 + SEX2.4 +  
                SEX1.5 + SEX2.5 +  
                BW1.1 + BW2.1 +
                BW1.2 + BW2.2 +
                BW1.3 + BW2.3 +
                BW1.4 + BW2.4 +  
                BW1.5 + BW2.5 + 
                AP1.1 + AP2.1 + AP3.1 +
                AP1.2 + AP2.2 + AP3.2 +
                AP1.3 + AP2.3 + AP3.3 +  
                AP1.4 + AP2.4 + AP3.4 + 
                AP1.5 + AP2.5 + AP3.5 +
                AL1.1 + AL2.1 + AL3.1 +
                AL1.2 + AL2.2 + AL3.2 +
                AL1.3 + AL2.3 + AL3.3 +  
                AL1.4 + AL2.4 + AL3.4 + 
                AL1.5 + AL2.5 + AL3.5 + 
                AB1.1 + AB2.1 +
                AB1.2 + AB2.2 +
                AB1.3 + AB2.3 +
                AB1.4 + AB2.4 + 
                AB1.5 + AB2.5 + 
                ABC1.1 + ABC2.1 +
                ABC1.2 + ABC2.2 +
                ABC1.3 + ABC2.3 +
                ABC1.4 + ABC2.4 + 
                ABC1.5 + ABC2.5 + 
                LOC1.1 + LOC2.1 + LOC3.1 +
                LOC1.2 + LOC2.2 + LOC3.2 +
                LOC1.3 + LOC2.3 + LOC3.3 +  
                LOC1.4 + LOC2.4 + LOC3.4 +
                LOC1.5 + LOC2.5 + LOC3.5 +
                EA.2 +
                EA.3 +
                EA.4 +
                EA.5 +
                strata(trans), data = long, method = "breslow")

#-----------wrirte estimates in table--------------#

#---generate CIs and rounded HRs---#

c <- summary(multi)$coefficients[, 1]
hr <- format(round(summary(multi)$coefficients[, 2], 2), nsmall = 2) #HR = exp(coef)
se <- summary(multi)$coefficients[, 3] #SE (coef)
p <- format(round(summary(multi)$coefficients[, 5], 4), nsmall = 4) #p-Value
lb <- exp(c - 1.96 * se) #lower bound  of CI
ub <- exp(c + 1.96 * se) #upper bound of CI
ub[ub>100] = 100
ci <- paste0("(", format(round(lb, 2), nsmall = 2), "-", format(round(ub, 2), nsmall = 2), ")") #formated CI
m <- cbind(hr, ci, p)
m

###------------factors--------------###

#---Put estimates for each transition and each variable in seperate matrix---#

 regex <- paste0("SEX", "[0-9].", "1", "$")
 regex
 test <- m[grepl(regex, rownames(m)), ] 
 test
 
#matrix for each transition 
for (trans in 1:5){
  print(paste("Transition:", trans))
    for (covariate in fnames){
      regex <- paste0(covariate, "[0-9].", trans, "$")
      print(paste("Regular expression:", regex))
      print(covariate)  
      obj <- paste0("m", covariate) # define name of R object (i.e. uSEX) 
      print(obj)
      assign(paste0(obj, "s", trans), m[grepl(regex, rownames(m)), ])
      print(get(paste0(obj, "s", trans)))
  }
}

#---Add rows for baseline levels of factor variables and lable levels---#

#Add rows for baseline level and title row 
for (covariate in fnames){  
  title <- c(covariate, "", "", "")
  for (trans in 1:5){
    if (!(covariate =="Y" & trans ==1)) {   #exclude variables not included in multivariable analysis 
      obj <- paste0("m", covariate, "s", trans) # define name of R object (i.e. mSEXs1) 
      if (any(grep(covariate, obj)) == 1) { # if varname is in object 
        assign(obj, rbind(base, get(obj))) # add base row 
        labelling <- get(obj) # put object in dummy
        rownames(labelling)[1] <- paste0(covariate, "0.", trans) # assign rownames to dummy
        assign(obj, labelling) # overwrite object with dummy 
        assign(obj, cbind(get(paste0("lab", covariate)), get(obj))) 
        assign(obj, rbind(title, get(obj)))
      }
    }
  }
}

#Print without NAs 
print(as.matrix(mLOCs5), na.print="", quote =FALSE)
print(as.matrix(mSEXs5), na.print="", quote =FALSE)
print(as.matrix(mAPs5), na.print="", quote =FALSE)
print(as.matrix(mALs5), na.print="", quote =FALSE)
print(as.matrix(mABs5), na.print="", quote =FALSE)
print(as.matrix(mABCs5), na.print="", quote =FALSE)

###------------continous--------------###

#---Put estimates for each transition in seperate matrix---#

#matrix for each transition 
for (trans in 1:5){
  print(paste("Transition:", trans))
  for (covariate in cnames){
    regex <- paste0(covariate,".", trans, "$")
    print(paste("Regular expression:", regex))
    print(covariate)  
    obj <- paste0("m", covariate) # define name of R object (i.e. uSEX) 
    print(obj)
    assign(paste0(obj, "s", trans), m[grepl(regex, rownames(m)), ])
    print(get(paste0(obj, "s", trans)))
  }
}

#---Add lable and columname ---#

#Add title column 
for (covariate in cnames){  
  for (trans in 1:5){
    obj <- paste0("m", covariate, "s", trans) # define name of R object (i.e. uSEXs1) 
    if (any(grep(covariate, obj)) == 1) { # if varname is in object 
      assign(obj, c(get(paste0("lab", covariate)), get(obj))) 
    }
  }
}

###------------------Multivariate tables---------------------####

###-------Transition 1--------###

#Append rows 
m1 <- rbind(mSEXs1, d_EA, mBWs1, mAPs1, mALs1, mABs1, mABCs1, mLOCs1)

#Include empty columns 
m1 <- cbind(m1[,1], "", m1[,2], m1[,3])

#Overwrite rownames 
rownames(m1) <- rep("", nrow(m1))

#Print without NAs 
print(as.matrix(m1), na.print="", quote =FALSE)

###-------Transition 2-4--------###

for (i in 2:5){
  #Append rows 
  assign(paste0("m", i), rbind(get(paste0("mSEXs", i)), get(paste0("mEAs", i)), 
                               get(paste0("mBWs", i)),  get(paste0("mAPs", i)), get(paste0("mALs", i)),
                               get(paste0("mABs", i)),  get(paste0("mABCs", i)), get(paste0("mLOCs", i)) 
  )
  ) 
  #Include empty columns 
  assign(paste0("m", i), cbind(get(paste0("m", i))[,1], "", get(paste0("m", i))[,2], 
                               get(paste0("m", i))[,3]
  )
  )
}

#Overwrite rownames 
rownames(m2) <- rep("", nrow(m2))
rownames(m3) <- rep("", nrow(m3))
rownames(m4) <- rep("", nrow(m4))
rownames(m5) <- rep("", nrow(m5))

#Print without NAs 
print(as.matrix(m2), na.print="", quote =FALSE)
print(as.matrix(m3), na.print="", quote =FALSE)
print(as.matrix(m4), na.print="", quote =FALSE)
print(as.matrix(m5), na.print="", quote =FALSE)


###-------------------Final table---------------------###

#Univar

#Rename lables 
u1[,1] <- ifelse(u1[,1]=="SEX", "Gender", u1[,1])
u1[,1] <- ifelse(u1[,1]=="EA", "Age at enrolment (months)", u1[,1])
u1[,1] <- ifelse(u1[,1]=="Y", "Year of birth", u1[,1])
u1[,1] <- ifelse(u1[,1]=="BW", "Birth weight (kg)", u1[,1])
u1[,1] <- ifelse(u1[,1]=="AP", "Antepartum ARV exposure", u1[,1])
u1[,1] <- ifelse(u1[,1]=="AL", "Intrapartum ARV exposure", u1[,1])
u1[,1] <- ifelse(u1[,1]=="AB", "Prophelactic ARVs at birth", u1[,1])
u1[,1] <- ifelse(u1[,1]=="ABC", "Prophelactic ARVs after birth", u1[,1])
u1[,1] <- ifelse(u1[,1]=="LOC", "Facility type", u1[,1])                

#Print without NAs 
print(as.matrix(u1), na.print="", quote =FALSE)

#Drop first column 
u2 <- u2[, 2:4]
u3 <- u3[, 2:4]
u4 <- u4[, 2:4]
u5 <- u5[, 2:4]

#Merge columns 
uni_final <- cbind(u1, u2, u3, u5, u4)

#remove embedded blanks 
uni_final <- apply(uni_final,2,function(x)gsub("-  ", "-",x))
uni_final <- apply(uni_final,2,function(x)gsub("-100.00", "- >100",x))

#Print 
print(as.matrix(uni_final), na.print="", quote =FALSE)

#Export table 
write.xlsx(uni_final, paste0(tt, "uni_cox_ph.xlsx")) 

#Mulitvar

#Rename lables 
m1[,1] <- ifelse(m1[,1]=="SEX", "Gender", m1[,1])
m1[,1] <- ifelse(m1[,1]=="EA", "Age at enrolment (months)", m1[,1])
m1[,1] <- ifelse(m1[,1]=="Y", "Year of birth", m1[,1])
m1[,1] <- ifelse(m1[,1]=="BW", "Birth weight (kg)", m1[,1])
m1[,1] <- ifelse(m1[,1]=="AP", "Antepartum ARV exposure", m1[,1])
m1[,1] <- ifelse(m1[,1]=="AL", "Intrapartum ARV exposure", m1[,1])
m1[,1] <- ifelse(m1[,1]=="AB", "Prophelactic ARVs at birth", m1[,1])
m1[,1] <- ifelse(m1[,1]=="ABC", "Prophelactic ARVs after birth", m1[,1])
m1[,1] <- ifelse(m1[,1]=="LOC", "Facility type", m1[,1])                

#Print without NAs 
print(as.matrix(m1), na.print="", quote =FALSE)

#Drop first column 
m2 <- m2[, 2:4]
m3 <- m3[, 2:4]
m4 <- m4[, 2:4]
m5 <- m5[, 2:4]

#Merge columns 
multi_final <- cbind(m1, m2, m3, m5, m4)

#remove embedded blanks 
mulit_final <- apply(multi_final,2,function(x)gsub("-  ", "-",x))
multi_final <- apply(mulit_final,2,function(x)gsub("-100.00", "- >100",x))

#Print 
print(as.matrix(multi_final), na.print="", quote =FALSE)

#Export
write.xlsx(mulit_final, paste0(tt, "multi_cox_ph.xlsx")) 
