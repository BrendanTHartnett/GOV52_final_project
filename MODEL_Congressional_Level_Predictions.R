library(haven)
library(ggplot2)
library(dplyr)
library(car)
library(dataverse) 
library(readxl)
library(tidyverse)
library(tidycensus)
library(lme4)
library(tidyr)
library(arm) # inverse
library(sf)
library(modelr)
library(inlmisc)
library(AddSearchButton)
library(shiny)
library(leaflet)
library(rgdal)
library(sp)
library(readr)
#Load BLW respondents geocoded by district BLWatDist <- read_sf("Districtdat.shp")
dat <- BLWatDist
dat$biden_winner <- dat$bdn_wnn
dat$biden_winner[dat$biden_winner=="Definitely not the rightful winner"] <- "1"
dat$biden_winner[dat$biden_winner=="Probably not the rightful winner"] <- "1"
dat$biden_winner[dat$biden_winner=="Probably the rightful winner"] <- "0"
dat$biden_winner[dat$biden_winner=="Definitely the rightful winner"] <- "0"
dat$biden_winner <- as.numeric(dat$biden_winner)
table(dat$biden_winner)


## RACE X GENDER ####
#Recode gender to binary (is.MALE = 1 )
dat$male <- NA
dat$male <- 0
dat$male[dat$gender=="Male"] <- 1
#Recode race to factor
dat$ethnicity <- dat$ethncty
dat$race <- NA
dat$race[dat$ethnicity=="White"] <- "White"
dat$race[dat$ethnicity=="Asian/Pacific Islander"] <- "Asian"
dat$race[dat$ethnicity=="Hispanic/Latino/Chicano/a" | dat$hispanc=="Yes"] <- "Hispanic"
dat$race[dat$ethnicity=="Black or African American"] <- "Black"
dat$race[dat$ethnicity=="Other" | dat$ethnicity=="Multi-racial" | dat$ethnicity=="American Indian or Alaska Native"] <- "Other"
#Code for Gender X Race
table(dat$hispanc)
dat$raceXgender[dat$race=="White" & dat$male==1] <- "White male"
dat$raceXgender[dat$race=="Asian" & dat$male==1] <- "Asian male"
dat$raceXgender[dat$hispanc=="Yes" & dat$male==1] <- "Hispanic male"
dat$raceXgender[dat$race=="Black" & dat$male==1] <- "Black male"
dat$raceXgender[dat$race=="Other" & dat$male==1] <- "Other male"
dat$raceXgender[dat$race=="White" & dat$male==0] <- "White female"
dat$raceXgender[dat$race=="Asian" & dat$male==0] <- "Asian female"
dat$raceXgender[dat$hispanc=="Yes" & dat$male==0] <- "Hispanic female"
dat$raceXgender[dat$race=="Black" & dat$male==0] <- "Black female"
dat$raceXgender[dat$race=="Other" & dat$male==0] <- "Other female"
table(dat$raceXgender)
#Recode raceXGender to binary for application to census 
dat$WHITEMALE <- 0
dat$WHITEMALE[dat$raceXgender=="White male"] <- 1
dat$WHITEFEMALE <- 0
dat$WHITEFEMALE[dat$raceXgender=="White female"] <- 1
dat$BLACKMALE <- 0
dat$BLACKMALE[dat$raceXgender=="Black male"] <- 1
dat$BLACKFEMALE <- 0
dat$BLACKFEMALE[dat$raceXgender=="Black female"] <- 1
dat$ASIANMALE <- 0
dat$ASIANMALE[dat$raceXgender=="Asian male"] <- 1
dat$ASIANFEMALE <- 0
dat$ASIANFEMALE[dat$raceXgender=="Asian female"] <- 1
dat$HISPANICMALE <- 0
table(dat$raceXgender)
dat$HISPANICMALE[dat$raceXgender=="Hispanic male"] <- 1
dat$HISPANICFEMALE <- 0
dat$HISPANICFEMALE[dat$raceXgender=="Hispanic female"] <- 1
dat$OTHERMALE <- 0
dat$OTHERMALE[dat$raceXgender=="Other male"] <- 1
dat$OTHERFEMALE <- 0
dat$OTHERFEMALE[dat$raceXgender=="Other female"] <- 1


## EDUCATION ####
#Recode Education
table(dat$educ7)
dat$edu <- NA
dat$edu[dat$educ7=="Did not graduate from high school"] <- "No high school"
dat$edu[dat$educ7=="High school diploma or the equivalent (GED)"] <- "High school"
dat$edu[dat$educ7=="Some college"] <- "Some college"
dat$edu[dat$educ7=="Associate's degree"] <- "Some college"
dat$edu[dat$educ7=="Bachelor's degree"] <- "Bachelor's"
dat$edu[dat$educ7=="Master's degree"] <- "Master's"
dat$edu[dat$educ7=="Professional or doctorate degree"] <- "Master's"
table(dat$edu)
#Create dummy variable for if their highest level of education was high school
#where those with GED, High school degree, or no highschool degree = 1 
dat$highschool <- NA
dat$highschool <- 0 
dat$highschool[dat$edu=="No high school"] <- 1
dat$highschool[dat$edu=="High school"] <- 1
table(dat$highschool)


# AGE ####
#Create age categories  
table(dat$age)
dat$agecat <- NA
dat$agecat[dat$age<25] <- "Under 25"
dat$agecat[dat$age > 24 & dat$age<35] <- "Under 35"
dat$agecat[dat$age > 34 & dat$age<55] <- "Under 55"
dat$agecat[dat$age > 54 & dat$age<65] <- "Under 65"
dat$agecat[dat$age>64] <- "Under 99" #Really just over 65, but this way it is in alphabetical order
table(dat$agecat)
#Recode ages to binary  
dat$under35 <- 0
dat$under35[dat$agecat=="Under 25"] <- 1
dat$under35[dat$agecat=="Under 35"] <- 1
dat$under55 <- 0
dat$under55[dat$agecat=="Under 55"] <- 1
dat$under65 <- 0
dat$under65[dat$agecat=="Under 65"] <- 1
dat$under99 <- 0
dat$under99[dat$agecat=="Under 99"] <- 1


## Employment ####
#Recode employment to labor statistics
table(dat$employ)
dat$employment <- NA
dat$employment <- "Not in labor force"
dat$employment[dat$employ=="Student"] <- "Student"
dat$employment[dat$employ=="Full-time"] <- "Employed"
dat$employment[dat$employ=="Part-time"] <- "Employed"
dat$employment[dat$employ=="Unemployed"] <- "Unemployed"
table(dat$employment)
#Create binary for labor status
dat$Employed <- 0 
dat$Employed[dat$employment=="Employed"] <- 1
dat$Unemployed <- 0
dat$Unemployed[dat$employment=="Unemployed"] <- 1
dat$NotInLaborForce <- 0
dat$NotInLaborForce[dat$employment=="Not in labor force"] <- 1
#Ultimately, I only use NotInLaborForce as an explanatory variable



## 2020 VOTE CHOICE ####
table(dat$pres_vt)
dat$pres_vote <- dat$pres_vt
table(dat$pres_vote)
#Make dummy variable for voted for Trump
dat$Trump <- NA
dat$Trump <- 0
dat$Trump[dat$pres_vote=="Donald Trump"] <- 1
table(dat$Trump)



# HHI ####
#Make dummy for if HHI is under 40k
table(dat$fmnc_nw)
dat$faminc_new <- dat$fmnc_nw
dat$less40k <- NA
dat$less40k <- 0
dat$less40k[dat$faminc_new=="Less than $10,000"] <- 1
dat$less40k[dat$faminc_new=="$10,000 - $19,999"] <- 1
dat$less40k[dat$faminc_new=="$20,000 - $29,999"] <- 1
dat$less40k[dat$faminc_new=="$30,000 - $39,999"] <- 1
table(dat$less40k)
#Make dummy for if HHI is over 100k
dat$over100k <- NA
dat$over100k <- 0
dat$over100k[dat$faminc_new=="$100,000 - $119,999"] <- 1
dat$over100k[dat$faminc_new=="$120,000 - $149,999"] <- 1
dat$over100k[dat$faminc_new=="$120,000 - $149,999"] <- 1
dat$over100k[dat$faminc_new=="$150,000 - $199,999"] <- 1
dat$over100k[dat$faminc_new=="$200,000 - $249,999" | dat$faminc_new=="$250,000 - $349,999" | dat$faminc_new=="$350,000 - $499,999" | dat$faminc_new=="$500,000 or more"] <- 1
table(dat$over100k)


#Change input_states to NAME to match tidycensus
table(dat$JOIN_FID)
dat$disID <- as.character(dat$JOIN_FID)
dat$NAME <- dat$disID


#Change for Census Block Model
dat$BLACK <- dat$BLACKFEMALE + dat$BLACKMALE
dat$WHITE <- dat$WHITEFEMALE + dat$WHITEMALE
dat$HISPANIC <- dat$HISPANICFEMALE + dat$HISPANICMALE
table(dat$HISPANIC)
#Formal Model -- this does not contain age or gender
plearn9 <- glmer(formula = biden_winner ~  (1 | NAME) + highschool + WHITE + BLACK + HISPANIC + NotInLaborForce + less40k + over100k + Trump, data=dat, family=binomial(link="logit"))
plearn9
summary(plearn9)




### CENSUS DATA ##############################
### Download  files for congressional districts with census datacongdat <- read_sf("House_district_level_data")
edudat <- congdat
edudat$FlessthanHS <- edudat$Sum_fnone +  edudat$Sum_f4 + edudat$Sum_f6 + edudat$Sum_f8 + edudat$Sum_f9 + edudat$Sum_f10 + edudat$Sum_f11 + edudat$Sum_f12
table(edudat$FlessthanHS)
edudat$Fhighschool <- edudat$Sum_fHS
edudat$Fsomecollege <- edudat$Sum_fsc1 + edudat$Sum_fsc2 + edudat$Sum_fass
edudat$Fbachelorsdegree <- edudat$Sum_fbach
table(edudat$Fbachelorsdegree)
edudat$Fgradschool <- edudat$Sum_fprof + edudat$Sum_fdoc + edudat$Sum_fmas
table(edudat$Fgradschool)

edudat$MlessthanHS <- edudat$Sum_Mnone +  edudat$Sum_M4 + edudat$Sum_M6 + edudat$Sum_M8 + edudat$Sum_M9 + edudat$Sum_M10 + edudat$Sum_M11 + edudat$Sum_M12
table(edudat$MlessthanHS)
edudat$Mhighschool <- edudat$Sum_MHS
edudat$highschool <- edudat$MlessthanHS + edudat$Mhighschool + edudat$Fhighschool + edudat$FlessthanHS
edudat$Msomecollege <- edudat$Sum_Msc1 + edudat$Sum_Msc2 + edudat$Sum_Mass
edudat$Mbachelorsdegree <- edudat$Sum_Mbach
table(edudat$Mbachelorsdegree)
edudat$Mgradschool <- edudat$Sum_Mprof + edudat$Sum_Mdoc + edudat$Sum_Mmas
table(edudat$Mgradschool)

edudat$TOTALEDU <- edudat$MlessthanHS + edudat$Mhighschool + edudat$Msomecollege + edudat$Mbachelorsdegree + edudat$Mgradschool + edudat$FlessthanHS + edudat$Fhighschool + edudat$Fsomecollege + edudat$Fbachelorsdegree + edudat$Fgradschool
edudat$highschool <- edudat$highschool/edudat$TOTALEDU

empdat <- edudat

#Recode variable for those not in labor force
empdat$NotInLaborForce <-empdat$Sum_NILF
#Create summary variable
empdat$Employed <- empdat$Sum_employ + empdat$Sum_armed
empdat$Unemployed <- empdat$Sum_unempl
empdat$Total <- empdat$Employed + empdat$Unemployed + empdat$NotInLaborForce
#Convert to Percentage not in LF
empdat$NotInLaborForce <- empdat$NotInLaborForce/empdat$Total

srdat <- empdat
srdat$WHITE <- srdat$WhitePct
srdat$BLACK <- srdat$BlackPct
srdat$HISPANIC <- srdat$Hispanic




fdat <- srdat


fdat$TRUMP <- fdat$Sum_REPUBL/fdat$Sum_TOTAL

fdat$NAME <- as.character(fdat$FID_1_1)

hhidat <- fdat
table(hhidat$Sum_k20)
hhidat$less401k <- hhidat$Sum_k10 + hhidat$Sum_k15 + hhidat$Sum_k20 + hhidat$Sum_k25 + hhidat$Sum_k30 + hhidat$Sum_k35 + hhidat$Sum_k40
hhidat$less40k <- hhidat$less401k
hhidat$over100k <- hhidat$Sum_k12 + hhidat$Sum_k51 + hhidat$Sum_k02 + hhidat$Sum_kri

hhidat$TOTALhhi <- hhidat$less40k + hhidat$over100k + hhidat$Sum_k45 + hhidat$Sum_k50 + hhidat$Sum_k60 + hhidat$Sum_k75 + hhidat$Sum_k11

hhidat$less40k <- hhidat$less40k/hhidat$TOTALhhi
hhidat$over100k <- hhidat$over100k/hhidat$TOTALhhi

Census <- hhidat


## MRP MODEL ####

state_ranefs <- array(NA, c(428, 1))

# set state names as row names
dimnames(state_ranefs) <- list(c(Census$NAME), 'effect')

# assign state random effects to array while preserving NAs
for (i in Census$NAME) {
  
  state_ranefs[i, ] <- ranef(plearn9)$NAME[i, 1]
  
}
state_ranefs[, 1][is.na(state_ranefs[, 1])] <- 0
plearn9 <- plearn9
summary(plearn9)
Census$Tru
Census$cellpred22 <- invlogit(fixef(plearn9)['(Intercept)']  + state_ranefs[Census$NAME, 1] + (fixef(plearn9)['Trump'] *Census$TRUMP) + (fixef(plearn9)['highschool'] *Census$highschool) +  (fixef(plearn9)['WHITE'] *Census$WHITE) + (fixef(plearn9)['BLACK'] *Census$BLACK) + (fixef(plearn9)['HISPANIC'] *Census$HISPANIC)  + (fixef(plearn9)['NotInLaborForce'] *Census$NotInLaborForce) + (fixef(plearn9)['less40k'] *Census$less40k) + (fixef(plearn9)['over100k'] *Census$over100k))
Census$cellpred22


