library(readr)
library(haven)
library(ggplot2)
library(dplyr)
library(car)

#LOAD DATA
###ds1 <- read_csv("ds1.csv") # Load Wave 1 
###ds2 <- read_csv("ds2.csv") # Load Wave 2

#Merge datawaves
data <- bind_rows(ds1, ds2)
dat <- data

#Recode rejection of election results ot binary: 0 = accept, 1 = reject
table(dat$biden_winner)
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
table(dat$male)
#Recode race to factor
dat$race[dat$ethnicity=="White"] <- "White"
dat$race[dat$ethnicity=="Asian/Pacific Islander"] <- "Asian"
dat$race[dat$ethnicity=="Hispanic/Latino/Chicano/a" | dat$hispanic=="Yes"] <- "Hispanic"
dat$race[dat$ethnicity=="Black or African American"] <- "Black"
dat$race[dat$ethnicity=="Other" | dat$ethnicity=="Multi-racial" | dat$ethnicity=="American Indian or Alaska Native"] <- "Other"
table(dat$race)
table(dat$hispanic)
#Code for Gender X Race
dat$raceXgender[dat$race=="White" & dat$male==1] <- "White male"
dat$raceXgender[dat$race=="Asian" & dat$male==1] <- "Asian male"
dat$raceXgender[dat$race=="Hispanic" & dat$male==1] <- "Hispanic male"
dat$raceXgender[dat$hispanic=="Yes" & dat$male==1] <- "Hispanic male"
dat$raceXgender[dat$race=="Black" & dat$male==1] <- "Black male"
dat$raceXgender[dat$race=="Other" & dat$male==1] <- "Other male"
dat$raceXgender[dat$race=="White" & dat$male==0] <- "White female"
dat$raceXgender[dat$race=="Asian" & dat$male==0] <- "Asian female"
dat$raceXgender[dat$race=="Hispanic" & dat$male==0] <- "Hispanic female"
dat$raceXgender[dat$hispanic=="Yes" & dat$male==0] <- "Hispanic female"
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
table(dat$pres_vote)
#Make dummy variable for voted for Trump
dat$Trump <- NA
dat$Trump <- 0
dat$Trump[dat$pres_vote=="Donald Trump"] <- 1
table(dat$Trump)



# HHI ####
#Make dummy for if HHI is under 40k
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
dat$NAME <- dat$inputstate


#Model Used for prediction
Model <- lm(biden_winner ~  factor(NAME) + highschool + under35 + under55 + under65 + WHITEMALE + WHITEFEMALE + BLACKMALE + BLACKFEMALE + HISPANICMALE + HISPANICFEMALE + NotInLaborForce + less40k + over100k + Trump, data=dat)
Model
summary(Model)