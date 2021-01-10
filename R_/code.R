#RETURN TO EDUCATION
#By Dovydas Riazanovas; dovyas.riazanovas@evaf.stud.vu.lt
#THis code is yet to be optimized.


library("readxl")
library("dplyr")
library("tidyverse")
library("data.table")


par(mfrow=c(1,1))
household_data <- read_xls("SILC 2018 household (1).xls")
personal_data <- read_xls("SILC 2018 personal.xls")


##############EDITING DATA##########################
household_data <- select(household_data, c(HB030,HY040G,HY050G,HY070G,HY060G,HY080G,HY090G,HY110G))

# Combine household data and personal data, for future purposes.

personal_data <- merge(household_data,personal_data,by="HB030")
rm(household_data)

# EDIT PERSONAL DATA
personal_data <- select(personal_data, -c(RB050,RB220,RB230,RB240,RL010,RL020,RL030,RL040,RL050,RL060,PB190,PB200,PE020,PE030,PH010, PH020,PH030,PH040,
                                          PH050,PH060,PH070,PL015,PL020,PL025,PL031,PL040,PL051,PL060,PL073,PL074,PL075,PL076,PL080,PL085,PL086,
                                          PL087,PL088,PL089,PL090,PL100,PL111,PL120,PL190,PL200,PL130,PL140,PL150,PY010N,PY030G,PY035G,PY021G,
                                          PY050N,PY050N,PY090N,PY120N, PD020,PD030,PD050,PD060,PD070,PD080,SL))





# We filter the work age for female & for male
personal_data_FEMALE <- filter(personal_data, AGE >=16 & AGE <64)
personal_data_MALE <- filter(personal_data, RB090==1 & AGE ==64)
personal_data <- rbind(personal_data_FEMALE,personal_data_MALE)
rm(personal_data_FEMALE,personal_data_MALE)

# We calculate the gross income of a person
income<- select(personal_data, c(PB030, PY010G,PY020G,PY050G,PY080G,PY090G,PY100G,PY110G,PY120G,PY130G,PY140G,HY040G,HY050G,HY070G,HY060G,HY080G,HY090G,HY110G))
personal_data <- select(personal_data, -c(PE010,PY010G,PY020G,PY050G,PY080G,PY090G,PY100G,PY110G,PY120G,PY130G,PY140G,HY040G,HY050G,HY070G,HY060G,HY080G,HY090G,HY110G))

income$Gross <- rowSums(income[,2:18])
income <- select(income, -c(PY010G,PY020G,PY050G,PY080G,PY090G,PY100G,PY110G,PY120G,PY130G,PY140G,HY040G,HY050G,HY070G,HY060G,HY080G,HY090G,HY110G))
personal_data <- merge(personal_data,income,by="PB030")
personal_data$Gross <- as.numeric(personal_data$Gross)
rm(income)

#change the education level from qualification to years of study.

personal_data$PE040[personal_data$PE040 == 0] <- 1
personal_data$PE040[personal_data$PE040 == 100] <- 2
personal_data$PE040[personal_data$PE040 == 200] <- 3
personal_data$PE040[personal_data$PE040 == 300] <- 4
personal_data$PE040[personal_data$PE040 == 344] <- 5
personal_data$PE040[personal_data$PE040 == 352] <- 6
personal_data$PE040[personal_data$PE040 == 354] <- 7
personal_data$PE040[personal_data$PE040 == 400] <- 8
personal_data$PE040[personal_data$PE040 == 450] <- 9
personal_data$PE040[personal_data$PE040 == 600] <- 10
personal_data$PE040[personal_data$PE040 == 700] <- 11
personal_data$PE040[personal_data$PE040 == 800] <- 12


x_axis <- c("0","4","10","12","12*1","12*2","12*3","12*4","13","13-16","15-18","17-22")


#######PROVE THAT EDUCATION INCREASES INCOME################

personal_data <- na.omit(personal_data)
R2E <- personal_data %>%
  group_by(PE040) %>%
  summarize(BASE = median(Gross))

test <- cor.test(personal_data$PE040,personal_data$Gross) #GROSS test
test1 <- cor.test(R2E$PE040,R2E$BASE) #R2E test

plot(R2E$PE040,R2E$BASE,xlab="Education (years)",ylab="R2E measurement",ylim=c(0,25000),col="black",type="l",lwd=2,xaxt="n") 
axis(1,at=1:12,labels=x_axis,par(las=3),cex.axis=0.8) 



####FILTER URBAN AND RURAL, TEST IT##################
urban <- filter(personal_data,M_K == 1)
rural <- filter(personal_data,M_K == 2)
urban <- select(urban,c(PE040,Gross))
rural <- select(rural,c(PE040,Gross))
urban <- na.omit(urban)
rural <- na.omit(rural)


rurals <- rural %>%
  group_by(PE040) %>%
  summarize(BASE_RURAL = median(Gross))

urbans <- urban %>%
  group_by(PE040) %>%
  summarize(BASE_URBAN = median(Gross))


R2E <- merge(R2E,rurals, by="PE040")
R2E <- merge(R2E,urbans, by="PE040")

test2 <- t.test(R2E$BASE_RURAL, R2E$BASE_URBAN)

#Prove that there is a difference (or not) between urban and rural places.
plot(R2E$PE040,R2E$BASE_URBAN,xlab="Education (years)",ylab="R2E measurement",ylim=c(0,25000),col="blue",type="l",lwd=2,xaxt="n") 
axis(1,at=1:12,labels=x_axis,par(las=3),cex.axis=0.8)
lines(R2E$PE040,R2E$BASE_RURAL,col="red",lwd=2)
legend("topleft", legend=c("Rural", "Urban"),
       col=c("red", "blue"), lty=1:1)

rm(urbans)+rm(rurals)+rm(urban)+rm(rural)
R2E <- select(R2E,-c("BASE_RURAL","BASE_URBAN"))

######################################################################################-
#NOW WE OVERVIEW SPECIFIC COUNTIES AND THE URBAN AND THE RURAL return to education. 


par(mfrow=c(4,3))

#THE STANDARD CHANGE RATE PERCENTAGE WILL BE USED IN FUTURE RESEARCH.
############ALYTUS##########################################################################

Alytus <- filter(personal_data, AP==1)
Alytus1 <- Alytus %>%
  group_by(PE040) %>%
  summarize(ALYTUS = median(Gross))

R2E <- left_join(R2E, Alytus1, by = "PE040")
rm(Alytus)+rm(Alytus1)

 
#urban
Alytus_URBAN<- filter(personal_data,AP==1 & M_K ==1)
Alytus_URBAN1 <- Alytus_URBAN %>%
  group_by(PE040) %>%
  summarize(MEDIAN = median(Gross)) %>%
  mutate(STANDARD_CHANGE_PERCENTAGE = (MEDIAN/lag(MEDIAN) - 1) * 100) 

Alytus_URBAN <- merge(Alytus_URBAN,Alytus_URBAN1, by="PE040")

#rural
Alytus_RURAL<- filter(personal_data,AP==1 & M_K ==2)
Alytus_RURAL1 <- Alytus_RURAL %>%
  group_by(PE040) %>%
  summarize(MEDIAN = median(Gross)) %>%
  mutate(STANDARD_CHANGE_PERCENTAGE = (MEDIAN/lag(MEDIAN) - 1) * 100) 

Alytus_RURAL <- merge(Alytus_RURAL,Alytus_RURAL1, by="PE040")



plot(Alytus_URBAN$PE040,Alytus_URBAN$MEDIAN,xlab="Education (years)",ylab="R2E measurement",ylim=c(0,20000),main="Alytus",col="blue",type="l",lwd=2,xaxt="n") 
axis(1,at=1:12,labels=x_axis,par(las=3),cex.axis=0.8) 
lines(Alytus_RURAL$PE040,Alytus_RURAL$MEDIAN,col="red",lwd=2)


############KAUNAS##########################################################################

Kaunas <- filter(personal_data, AP==2)
Kaunas1 <- Kaunas %>%
  group_by(PE040) %>%
  summarize(KAUNAS = median(Gross))

R2E <- left_join(R2E, Kaunas1, by = "PE040")
rm(Kaunas)+rm(Kaunas1)


#urban
Kaunas_URBAN<- filter(personal_data,AP==2 & M_K ==1)
Kaunas_URBAN1 <- Kaunas_URBAN %>%
  group_by(PE040) %>%
  summarize(MEDIAN = median(Gross)) %>%
  mutate(STANDARD_CHANGE_PERCENTAGE = (MEDIAN/lag(MEDIAN) - 1) * 100) 

Kaunas_URBAN <- merge(Kaunas_URBAN,Kaunas_URBAN1, by="PE040")

#rural
Kaunas_RURAL<- filter(personal_data,AP==2 & M_K ==2)
Kaunas_RURAL1 <- Kaunas_RURAL %>%
  group_by(PE040) %>%
  summarize(MEDIAN = median(Gross)) %>%
  mutate(STANDARD_CHANGE_PERCENTAGE = (MEDIAN/lag(MEDIAN) - 1) * 100) 

Kaunas_RURAL <- merge(Kaunas_RURAL,Kaunas_RURAL1, by="PE040")



plot(Kaunas_URBAN$PE040,Kaunas_URBAN$MEDIAN,xlab="Education (years)",ylab="R2E measurement",ylim=c(0,25000),main="Kaunas",col="blue",type="l",lwd=2,xaxt="n") 
axis(1,at=1:12,labels=x_axis,par(las=3),cex.axis=0.8) 
lines(Kaunas_RURAL$PE040,Kaunas_RURAL$MEDIAN,col="red",lwd=2)


############Klaipeda##########################################################################

KLP <- filter(personal_data, AP==3)
KLP1 <- KLP %>%
  group_by(PE040) %>%
  summarize(KLAIPEDA = median(Gross))


R2E <- left_join(R2E, KLP1, by = "PE040")

rm(KLP)+rm(KLP1)

#urban
KLP_URBAN<- filter(personal_data,AP==3 & M_K ==1)
KLP_URBAN1 <- KLP_URBAN %>%
  group_by(PE040) %>%
  summarize(MEDIAN = median(Gross)) %>%
  mutate(STANDARD_CHANGE_PERCENTAGE = (MEDIAN/lag(MEDIAN) - 1) * 100) 

KLP_URBAN <- merge(KLP_URBAN,KLP_URBAN1, by="PE040")

#rural
KLP_RURAL<- filter(personal_data,AP==3 & M_K ==2)
KLP_RURAL1 <- KLP_RURAL %>%
  group_by(PE040) %>%
  summarize(MEDIAN = median(Gross)) %>%
  mutate(STANDARD_CHANGE_PERCENTAGE = (MEDIAN/lag(MEDIAN) - 1) * 100) 

KLP_RURAL <- merge(KLP_RURAL,KLP_RURAL1, by="PE040")



plot(KLP_URBAN$PE040,KLP_URBAN$MEDIAN,xlab="Education (years)",ylab="R2E measurement",ylim=c(0,30000),main="Klaipeda",col="blue",type="l",lwd=2,xaxt="n") 
axis(1,at=1:12,labels=x_axis,par(las=3),cex.axis=0.8) 
lines(KLP_RURAL$PE040,KLP_RURAL$MEDIAN,col="red",lwd=2)


############Marijampole##########################################################################

MA <- filter(personal_data, AP==4)
MA1 <- MA %>%
  group_by(PE040) %>%
  summarize(MARIJAMPOLE = median(Gross))


R2E <- left_join(R2E, MA1, by = "PE040")

rm(MA)+rm(MA1)

#urban
MA_URBAN<- filter(personal_data,AP==4 & M_K ==1)
MA_URBAN1 <- MA_URBAN %>%
  group_by(PE040) %>%
  summarize(MEDIAN = median(Gross)) %>%
  mutate(STANDARD_CHANGE_PERCENTAGE = (MEDIAN/lag(MEDIAN) - 1) * 100) 

MA_URBAN <- merge(MA_URBAN,MA_URBAN1, by="PE040")

#rural
MA_RURAL<- filter(personal_data,AP==4 & M_K ==2)
MA_RURAL1 <- MA_RURAL %>%
  group_by(PE040) %>%
  summarize(MEDIAN = median(Gross)) %>%
  mutate(STANDARD_CHANGE_PERCENTAGE = (MEDIAN/lag(MEDIAN) - 1) * 100) 

MA_RURAL <- merge(MA_RURAL,MA_RURAL1, by="PE040")



plot(MA_URBAN$PE040,MA_URBAN$MEDIAN,xlab="Education (years)",ylab="R2E measurement",ylim=c(0,20000),main="Marijampole",col="blue",type="l",lwd=2,xaxt="n") 
axis(1,at=1:12,labels=x_axis,par(las=3),cex.axis=0.8) 
lines(MA_RURAL$PE040,MA_RURAL$MEDIAN,col="red",lwd=2)


############Panevezys##########################################################################

PAN <- filter(personal_data, AP==5)
PAN1 <- PAN %>%
  group_by(PE040) %>%
  summarize(PANEVEZYS = median(Gross)) 

R2E <- left_join(R2E, PAN1, by = "PE040")

rm(PAN)+rm(PAN1)

#urban
PAN_URBAN<- filter(personal_data,AP==5 & M_K ==1)
PAN_URBAN1 <- PAN_URBAN %>%
  group_by(PE040) %>%
  summarize(MEDIAN = median(Gross)) %>%
  mutate(STANDARD_CHANGE_PERCENTAGE = (MEDIAN/lag(MEDIAN) - 1) * 100) 

PAN_URBAN <- merge(PAN_URBAN,PAN_URBAN1, by="PE040")

#rural
PAN_RURAL<- filter(personal_data,AP==5 & M_K ==2)
PAN_RURAL1 <- PAN_RURAL %>%
  group_by(PE040) %>%
  summarize(MEDIAN = median(Gross)) %>%
  mutate(STANDARD_CHANGE_PERCENTAGE = (MEDIAN/lag(MEDIAN) - 1) * 100) 

PAN_RURAL <- merge(PAN_RURAL,PAN_RURAL1, by="PE040")



plot(PAN_URBAN$PE040,PAN_URBAN$MEDIAN,xlab="Education (years)",ylab="R2E measurement",ylim=c(0,20000),col="blue",main="Panevezys",type="l",lwd=2,xaxt="n") 
axis(1,at=1:12,labels=x_axis,par(las=3),cex.axis=0.8)
lines(PAN_RURAL$PE040,PAN_RURAL$MEDIAN,col="red",lwd=2)


############SIAULIAI##########################################################################

SIAUL <- filter(personal_data, AP==6)
SIAUL1 <- SIAUL %>%
  group_by(PE040) %>%
  summarize(SIAULIAI = median(Gross))

R2E <- left_join(R2E, SIAUL1, by = "PE040")

rm(SIAUL)+rm(SIAUL1)

#urban
SIAUL_URBAN<- filter(personal_data,AP==6 & M_K ==1)
SIAUL_URBAN1 <- SIAUL_URBAN %>%
  group_by(PE040) %>%
  summarize(MEDIAN = median(Gross)) %>%
  mutate(STANDARD_CHANGE_PERCENTAGE = (MEDIAN/lag(MEDIAN) - 1) * 100) 

SIAUL_URBAN <- merge(SIAUL_URBAN,SIAUL_URBAN1, by="PE040")

#rural
SIAUL_RURAL<- filter(personal_data,AP==6 & M_K ==2)
SIAUL_RURAL1 <- SIAUL_RURAL %>%
  group_by(PE040) %>%
  summarize(MEDIAN = median(Gross)) %>%
  mutate(STANDARD_CHANGE_PERCENTAGE = (MEDIAN/lag(MEDIAN) - 1) * 100) 

SIAUL_RURAL <- merge(SIAUL_RURAL,SIAUL_RURAL1, by="PE040")



plot(SIAUL_URBAN$PE040,SIAUL_URBAN$MEDIAN,xlab="Education (years)",ylab="R2E measurement",main="Siauliai",ylim=c(0,20000),col="blue",type="l",lwd=2,xaxt="n") 
axis(1,at=1:12,labels=x_axis,par(las=3),cex.axis=0.8)
lines(SIAUL_RURAL$PE040,SIAUL_RURAL$MEDIAN,col="red",lwd=2)


############Taurage##########################################################################

TAUR <- filter(personal_data, AP==7)
TAUR1 <- TAUR %>%
  group_by(PE040) %>%
  summarize(TAURAGE = median(Gross)) 

R2E <- left_join(R2E, TAUR1, by = "PE040")

rm(TAUR)+rm(TAUR1)

#urban
TAUR_URBAN<- filter(personal_data,AP==7 & M_K ==1)
TAUR_URBAN1 <- TAUR_URBAN %>%
  group_by(PE040) %>%
  summarize(MEDIAN = median(Gross)) %>%
  mutate(STANDARD_CHANGE_PERCENTAGE = (MEDIAN/lag(MEDIAN) - 1) * 100) 

TAUR_URBAN <- merge(TAUR_URBAN,TAUR_URBAN1, by="PE040")

#rural
TAUR_RURAL<- filter(personal_data,AP==7 & M_K ==2)
TAUR_RURAL1 <- TAUR_RURAL %>%
  group_by(PE040) %>%
  summarize(MEDIAN = median(Gross)) %>%
  mutate(STANDARD_CHANGE_PERCENTAGE = (MEDIAN/lag(MEDIAN) - 1) * 100) 

TAUR_RURAL <- merge(TAUR_RURAL,TAUR_RURAL1, by="PE040")



plot(TAUR_URBAN$PE040,TAUR_URBAN$MEDIAN,xlab="Education (years)",ylab="R2E measurement",main="Taurage",ylim=c(0,40000),col="blue",type="l",lwd=2,xaxt="n") 
axis(1,at=1:12,labels=x_axis,par(las=3),cex.axis=0.8)
lines(TAUR_RURAL$PE040,TAUR_RURAL$MEDIAN,col="red",lwd=2)


############Telsiai##########################################################################

TELS <- filter(personal_data, AP==8)
TELS1 <- TELS %>%
  group_by(PE040) %>%
  summarize(TELSIAI = median(Gross)) 

R2E <- left_join(R2E, TELS1, by = "PE040")

rm(TELS)+rm(TELS1)

#urban
TELS_URBAN<- filter(personal_data,AP==8 & M_K ==1)
TELS_URBAN1 <- TELS_URBAN %>%
  group_by(PE040) %>%
  summarize(MEDIAN = median(Gross)) %>%
  mutate(STANDARD_CHANGE_PERCENTAGE = (MEDIAN/lag(MEDIAN) - 1) * 100) 

TELS_URBAN <- merge(TELS_URBAN,TELS_URBAN1, by="PE040")

#rural
TELS_RURAL<- filter(personal_data,AP==8 & M_K ==2)
TELS_RURAL1 <- TELS_RURAL %>%
  group_by(PE040) %>%
  summarize(MEDIAN = median(Gross)) %>%
  mutate(STANDARD_CHANGE_PERCENTAGE = (MEDIAN/lag(MEDIAN) - 1) * 100) 

TELS_RURAL <- merge(TELS_RURAL,TELS_RURAL1, by="PE040")



plot(TELS_URBAN$PE040,TELS_URBAN$MEDIAN,xlab="Education (years)",ylab="R2E measurement",main="Telsiai",ylim=c(0,20000),col="blue",type="l",lwd=2,xaxt="n") 
axis(1,at=1:12,labels=x_axis,par(las=3),cex.axis=0.8)
lines(TELS_RURAL$PE040,TELS_RURAL$MEDIAN,col="red",lwd=2)


############Utena##########################################################################

UTEN <- filter(personal_data, AP==9)
UTEN1 <- UTEN %>%
  group_by(PE040) %>%
  summarize(UTENA = median(Gross)) 

R2E <- left_join(R2E, UTEN1, by = "PE040")

rm(UTEN)+rm(UTEN1)

#urban
UTEN_URBAN<- filter(personal_data,AP==9 & M_K ==1)
UTEN_URBAN1 <- UTEN_URBAN %>%
  group_by(PE040) %>%
  summarize(MEDIAN = median(Gross)) %>%
  mutate(STANDARD_CHANGE_PERCENTAGE = (MEDIAN/lag(MEDIAN) - 1) * 100) 

UTEN_URBAN <- merge(UTEN_URBAN,UTEN_URBAN1, by="PE040")

#rural
UTEN_RURAL<- filter(personal_data,AP==9 & M_K ==2)
UTEN_RURAL1 <- UTEN_RURAL %>%
  group_by(PE040) %>%
  summarize(MEDIAN = median(Gross)) %>%
  mutate(STANDARD_CHANGE_PERCENTAGE = (MEDIAN/lag(MEDIAN) - 1) * 100) 

UTEN_RURAL <- merge(UTEN_RURAL,UTEN_RURAL1, by="PE040")



plot(UTEN_URBAN$PE040,UTEN_URBAN$MEDIAN,xlab="Education (years)",ylab="R2E measurement",main="Utena",ylim=c(0,20000),col="blue",type="l",lwd=2,xaxt="n") 
axis(1,at=1:12,labels=x_axis,par(las=3),cex.axis=0.8)
lines(UTEN_RURAL$PE040,UTEN_RURAL$MEDIAN,col="red",lwd=2)


############Vilnius##########################################################################

VLN <- filter(personal_data, AP==10)
VLN1 <- VLN %>%
  group_by(PE040) %>%
  summarize(VILNIUS = median(Gross)) 

R2E <- left_join(R2E, VLN1, by = "PE040")

rm(VLN)+rm(VLN1)

#urban
VLN_URBAN<- filter(personal_data,AP==10 & M_K ==1)
VLN_URBAN1 <- VLN_URBAN %>%
  group_by(PE040) %>%
  summarize(MEDIAN = median(Gross)) %>%
  mutate(STANDARD_CHANGE_PERCENTAGE = (MEDIAN/lag(MEDIAN) - 1) * 100) 

VLN_URBAN <- merge(VLN_URBAN,VLN_URBAN1, by="PE040")

#rural
VLN_RURAL<- filter(personal_data,AP==10 & M_K ==2)
VLN_RURAL1 <- VLN_RURAL %>%
  group_by(PE040) %>%
  summarize(MEDIAN = median(Gross)) %>%
  mutate(STANDARD_CHANGE_PERCENTAGE = (MEDIAN/lag(MEDIAN) - 1) * 100) 

VLN_RURAL <- merge(VLN_RURAL,VLN_RURAL1, by="PE040")



plot(VLN_URBAN$PE040,VLN_URBAN$MEDIAN,xlab="Education (years)",ylab="R2E measurement",main="Vilnius",ylim=c(0,20000),col="blue",type="l",lwd=2,xaxt="n") 
axis(1,at=1:12,labels=x_axis,par(las=3),cex.axis=0.8)
lines(VLN_RURAL$PE040,VLN_RURAL$MEDIAN,col="red",lwd=2)







##########P-VALUE TABLE (TABLE 2)############################
#We create a table that represents the p-values of our t.test for counties.
base <- c(1,sprintf(unlist(lapply(1:10, function(i) t.test(R2E[, i+2], R2E$BASE)$p.value)),fmt = '%#.3f')) # su base
alytus <- c(" ",1,sprintf(unlist(lapply(1:9, function(i) t.test(R2E[, i+3], R2E$ALYTUS)$p.value)),fmt = '%#.3f')) # su base
kaunas <- c(" "," ",1,sprintf(unlist(lapply(1:8, function(i) t.test(R2E[, i+4], R2E$KAUNAS)$p.value)),fmt = '%#.3f')) # su base
klaipeda <- c(" "," "," ",1,sprintf(unlist(lapply(1:7, function(i) t.test(R2E[, i+5], R2E$KLAIPEDA)$p.value)),fmt = '%#.3f')) # su base
marijampole <- c(" "," "," "," ",1,sprintf(unlist(lapply(1:6, function(i) t.test(R2E[, i+6], R2E$MARIJAMPOLE)$p.value)),fmt = '%#.3f')) # su base
panevezys <- c(" "," "," "," "," ",1,sprintf(unlist(lapply(1:5, function(i) t.test(R2E[, i+7], R2E$PANEVEZYS)$p.value)),fmt = '%#.3f')) # su base
siauliai <- c(" "," "," "," "," "," ",1,sprintf(unlist(lapply(1:4, function(i) t.test(R2E[, i+8], R2E$SIAULIAI)$p.value)),fmt = '%#.3f')) # su base
taurage <- c(" "," "," "," "," "," "," ",1,sprintf(unlist(lapply(1:3, function(i) t.test(R2E[, i+9], R2E$TAURAGE)$p.value)),fmt = '%#.3f')) # su base
telsiai <- c(" "," "," "," "," "," "," "," ",1,sprintf(unlist(lapply(1:2, function(i) t.test(R2E[, i+10], R2E$TELSIAI)$p.value)),fmt = '%#.3f')) # su base
utena <- c("","","","","","","","","",1,sprintf(unlist(lapply(1:1, function(i) t.test(R2E[, i+11], R2E$UTENA)$p.value)),fmt = '%#.3f')) # su base
vilnius <- c("","","","","","","","","","",1)
countie_p_value <- data.table(X=colnames(R2E)[2:12],Base=base,Alytus=alytus,Kaunas=kaunas,Klaipeda=klaipeda,Marijampole=marijampole,Panevezys=panevezys,Siauliai=siauliai,
                Taurage=taurage,Telsiai=telsiai,Utena=utena,Vilnius=vilnius)
