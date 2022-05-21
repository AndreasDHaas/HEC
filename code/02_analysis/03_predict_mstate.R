
#Covariates 
cov <- c("AP", "AL", "AB", "ABC")

#Reshape data form wide to long format
long <- msprep(data = wide, trans = tmat, time =c(NA, "enrol_t", "LTF60_t", "dis_t", "dead_t", "ART_t"), 
               status=c(NA, "enrol_s", "LTF60_s", "dis_s", "dead_s", "ART_s"), keep=c(cov, "id", "EA_r"))
head(long,20)

#Append transition-specific covariats 
long <- expand.covs(long, cov, append=TRUE, longnames=FALSE)
head(long)

#Change fnames
fnames <- c("AP", "AL", "AB", "ABC")

#Transitions
#1. Birth -> Enrolment 
#2. Enrolment -> LTF
#3. Enrolment -> Discharge 
#4. Enrolment -> Death 

###----------------------------Multivariable coxph model ---------------------------###

multi <- coxph(Surv(Tstart, Tstop, status) ~  
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

regex <- paste0("AP", "[0-9].", "1", "$")
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
print(as.matrix(mAPs1), na.print="", quote =FALSE)
print(as.matrix(mALs1), na.print="", quote =FALSE)

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
m1 <- rbind(mAPs1, mALs1, mABs1, mABCs1)
m1

#Include empty columns 
m1 <- cbind(m1[,1], "", m1[,2], m1[,3])

#Overwrite rownames 
rownames(m1) <- rep("", nrow(m1))

#Print without NAs 
print(as.matrix(m1), na.print="", quote =FALSE)

###-------Transition 2-5--------###

for (i in 2:5){
  #Append rows 
  assign(paste0("m", i), rbind(get(paste0("mAPs", i)), get(paste0("mALs", i)),
                               get(paste0("mABs", i)),  get(paste0("mABCs", i))
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

#Rename lables 

m1[,1] <- ifelse(m1[,1]=="AP", "Antepartum ARV exposure", m1[,1])
m1[,1] <- ifelse(m1[,1]=="AL", "Intrapartum ARV exposure", m1[,1])
m1[,1] <- ifelse(m1[,1]=="AB", "Prophelactic ARVs at birth", m1[,1])
m1[,1] <- ifelse(m1[,1]=="ABC", "Prophelactic ARVs after birth", m1[,1])

#Print without NAs 
print(as.matrix(m1), na.print="", quote =FALSE)

#Drop first column 
m2 <- m2[, 2:4]
m3 <- m3[, 2:4]
m4 <- m4[, 2:4]
m5 <- m5[, 2:4]

#Merge columns 
final <- cbind(m1, m2, m3, m5, m4)

#remove embedded blanks 
final <- apply(final,2,function(x)gsub("-  ", "-",x))
final <- apply(final,2,function(x)gsub("-100.00", "- >100",x))

#Print 
print(as.matrix(final), na.print="", quote =FALSE)
write.xlsx(final, paste0(tt, "Tbl_S1.xlsx")) 


###---------------Predictions for high risk and low risk child------------###

###Define high and low risk child 

#High risk child
#AP = "No antepartum ARVs"
#AL = "No intrapartum ARVs"
#AB = "No ARVs at birth"
#ABC = "None"

#Low risk child
#AP = "ART 4+ weeks"
#AL = "ART"
#AB = "ARVs"
#ABC = "ARVs"

###---------High risk children--------#####

#Select high risk child 
wh_hr <- which(long$AP =="None" & long$AL =="None" & long$AB =="None" & long$ABC =="None")

#Prepare date frame (number of rows equal to number of transitions and all covariates that were used in full model)
hr <- long[rep(wh_hr[1], 5), 9:12]
hr$trans <- 1:5 # add column that specifies to which stratam in the coxph object each transition corresponds
hr

#Estimate transiton prob for high risk child (see Wreeded et al. Jour of Stat Soft 2011, pp.17-20)
attr(hr, "trans") <- tmat
hr <- expand.covs(hr, cov, longnames = FALSE)
hr$strata <- hr$trans
msf_hr <- msfit(multi, hr, trans = tmat)
pt_hr <- probtrans(msf_hr, predt = 0)  

#Extract cumulative probabilities from list 
est_hr <- pt_hr[[1]]                                     # extract dataframe from list

#Reshape long 
est_hr$factor <- factor(est_hr$time)
est_long_hr <- gather(est_hr, outcome, prob, pstate1:pstate6, factor_key=TRUE)
keep <- c("time", "outcome", "prob", "factor")
l_hr <- est_long_hr[keep]
head(est_hr, 100)

#Order 
l_hr$order[l_hr$outcome == "pstate1"] <- "1"
l_hr$order[l_hr$outcome == "pstate2"] <- "2"
l_hr$order[l_hr$outcome == "pstate4"] <- "3"
l_hr$order[l_hr$outcome == "pstate3"] <- "4"
l_hr$order[l_hr$outcome == "pstate6"] <- "5"
l_hr$order[l_hr$outcome == "pstate5"] <- "6"
l_hr <- l_hr[order(l_hr$order, l_hr$time),] 

#order of legend 
l_hr$outcome <- factor(l_hr$outcome, levels=c("pstate5", "pstate6", "pstate3", "pstate4", "pstate2", "pstate1"))

#Plot 
hr_p <- ggplot(l_hr, aes(x=time,y=prob,group=outcome,fill=outcome)) +
  geom_area(position="fill") +
  scale_x_discrete(limits=c(0, 6, 12, 18, 24, 30), expand = c(0, 0)) +
  scale_y_continuous(labels = percent_format(), limits=c(-0.005, 1.005), expand = c(0, 0)) +
  labs(x = "Age (in months)", y = "Percentage of children", fill = "State") +
  scale_fill_manual(values=c("#545454", "#8c510a", "#d8b365", "#5ab4ac", "#c7eae5", "#f0f0f0"), labels=c("Died", "Initiated ART", "Lost to follow-up" ,"Discharged HIV-","Retained in care", "Not yet enrolled")) 

#Print plot
hr_p

#Save Plot
tiff(filename=paste0(gg, "/hr.tiff"), width = 4666, height =3500, units = "px", res=600)
hr_p  
dev.off()

###---------Low risk children--------#####

#Select low risk child 
wh_lr <- which(long$AP =="ART 4+w" & long$AL =="ART" & long$AB =="ARVs" & long$ABC =="ARVs")

#Prepare date frame (number of rows equal to number of transitions and all covariates that were used in full model)
lr <- long[rep(wh_lr[1], 5), 9:12]
lr$trans <- 1:5 # add column that specifies to which stratam in the coxph object each transition corresponds
lr

#Estimate transiton prob for low risk child (see Wreeded et al. Jour of Stat Soft 2011, pp.17-20)
attr(lr, "trans") <- tmat
lr <- expand.covs(lr, cov, longnames = FALSE)
lr$strata <- lr$trans
msf_lr <- msfit(multi, lr, trans = tmat)
pt_lr <- probtrans(msf_lr, predt = 0)  

#Extract cumulative probabilities from list 
est_lr <- pt_lr[[1]]                                     # extract dataframe from list

#Reshape long 
est_lr$factor <- factor(est_lr$time)
est_long_lr <- gather(est_lr, outcome, prob, pstate1:pstate6, factor_key=TRUE)
keep <- c("time", "outcome", "prob", "factor")
l_lr <- est_long_lr[keep]

#Order 
l_lr$order[l_lr$outcome == "pstate1"] <- "1"
l_lr$order[l_lr$outcome == "pstate2"] <- "2"
l_lr$order[l_lr$outcome == "pstate4"] <- "3"
l_lr$order[l_lr$outcome == "pstate3"] <- "4"
l_lr$order[l_lr$outcome == "pstate6"] <- "5"
l_lr$order[l_lr$outcome == "pstate5"] <- "6"
l_lr <- l_lr[order(l_lr$order, l_lr$time),] 

#order of legend 
l_lr$outcome <- factor(l_lr$outcome, levels=c("pstate5", "pstate6", "pstate3", "pstate4", "pstate2", "pstate1"))

#Plot 
lr_p <- ggplot(l_lr, aes(x=time,y=prob,group=outcome,fill=outcome)) +
  geom_area(position="fill") +
  scale_x_discrete(limits=c(0, 6, 12, 18, 24, 30), expand = c(0, 0)) +
  scale_y_continuous(labels = percent_format(), limits=c(-0.005, 1.005), expand = c(0, 0)) +
  labs(x = "Age (in months)", y = "Percentage of children", fill = "State") +
  scale_fill_manual(values=c("#545454", "#8c510a", "#d8b365", "#5ab4ac", "#c7eae5", "#f0f0f0"), labels=c("Died", "Initiated ART", "Lost to follow-up" ,"Discharged HIV-","Retained in care", "Not yet enrolled")) 

#Print plot
lr_p

#Save Plot
tiff(filename=paste0(gg, "/lr.tiff"), width = 4666, height =3500, units = "px", res=600)
lr_p  
dev.off() 

#arrange the three plots in a single row
prow <- plot_grid( lr_p + theme(legend.position="none"),
                   hr_p + theme(legend.position="none"),
                   align = 'vh',
                   labels = c("A", "B"),
                   hjust = -1,
                   nrow = 1
)
prow

#Legend
legend <- get_legend(lr_p + theme(legend.position="right"))

#Combine with common legend
p <- plot_grid(prow, legend, rel_widths = c(2, .3))
p

#Save Plot
tiff(filename=paste0(gg, "/lr_hr.tiff"), width = 6666, height =2500, units = "px", res=600)
p  
dev.off() 

###Point estimates and CIs at time points 

###High risk 

#At relevant time points
est_hr$time2 <- round(est_hr$time, digits=2)
est_hr <-  est_hr[est_hr$time2 %in% c(2, 6, 12, 18, 24, 30.03), ]   # extract rows for months 

head(est_hr)

#Calculate CIs, round, multiply by 100
for (i in 1:6) {
  pstate <- paste0("pstate", i)
  cil <- paste0("p", i, "_cil")
  ciu <- paste0("p", i, "_ciu")
  se <- paste0("se", i)
  est_hr[,cil] <- round((est_hr[,pstate] - 1.96* est_hr[,se])*100, digits=2)   # calculate 95%-CIs
  est_hr[,ciu] <- round((est_hr[,pstate] + 1.96* est_hr[,se])*100, digits=2)   # calculate 95%-CIs
  est_hr[,pstate] <- round((est_hr[,pstate] *100), digits=2)   # calculate 95%-CIs
}

#Results 
#1. Not enrolled 
not_enrolled <- subset(est_hr, select = c(time, pstate1, p1_cil, p1_ciu)) 
not_enrolled

#2. Enrolled & under follow-up 
ric <- subset(est_hr, select = c(time, pstate2, p2_cil, p2_ciu)) 
ric

#3. LTF
ltf <- subset(est_hr, select = c(time, pstate3, p3_cil, p3_ciu)) 
ltf

#4. Discharged
dis <- subset(est_hr, select = c(time, pstate4, p4_cil, p4_ciu)) 
dis

#5. Dead
dead <- subset(est_hr, select = c(time, pstate5, p5_cil, p5_ciu)) 
dead  

#6. ART
art <- subset(est_hr, select = c(time, pstate6, p6_cil, p6_ciu)) 
art 


###Low risk 

#At relevant time points
est_lr$time2 <- round(est_lr$time, digits=2)
est_lr <-  est_lr[est_lr$time2 %in% c(2, 6, 12, 18, 24, 30.03), ]   # extract rows for months 

#Calculate CIs, round, multiply by 100
for (i in 1:6) {
  pstate <- paste0("pstate", i)
  cil <- paste0("p", i, "_cil")
  ciu <- paste0("p", i, "_ciu")
  se <- paste0("se", i)
  est_lr[,cil] <- round((est_lr[,pstate] - 1.96* est_lr[,se])*100, digits=2)   # calculate 95%-CIs
  est_lr[,ciu] <- round((est_lr[,pstate] + 1.96* est_lr[,se])*100, digits=2)   # calculate 95%-CIs
  est_lr[,pstate] <- round((est_lr[,pstate] *100), digits=2)   # calculate 95%-CIs
}

#Results 
#1. Not enrolled 
not_enrolled <- subset(est_lr, select = c(time, pstate1, p1_cil, p1_ciu)) 
not_enrolled

#2. Enrolled & under follow-up 
ric <- subset(est_lr, select = c(time, pstate2, p2_cil, p2_ciu)) 
ric

#3. LTF
ltf <- subset(est_lr, select = c(time, pstate3, p3_cil, p3_ciu)) 
ltf

#4. Discharged
dis <- subset(est_lr, select = c(time, pstate4, p4_cil, p4_ciu)) 
dis

#5. Dead
dead <- subset(est_lr, select = c(time, pstate5, p5_cil, p5_ciu)) 
dead  

#5. Dead
art <- subset(est_lr, select = c(time, pstate6, p6_cil, p6_ciu)) 
art  

