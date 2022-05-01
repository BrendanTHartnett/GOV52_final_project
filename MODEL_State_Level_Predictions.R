library(ggplot2)
library(dplyr)
library(car)
library(dataverse) 
library(tidyverse)
library(tidycensus)
library(lme4)
library(tidyr)
library(arm) # inverse
library(sf)
library(modelr)



# Load BLW data:      dat <- read_csv("BLW_data_clean_and_model.R")

dat$NAME <- tolower(dat$NAME)
plearn8 <- glmer(formula = biden_winner ~  (1 | NAME) + highschool + under35 + under55 + under65 + WHITEMALE + WHITEFEMALE + BLACKMALE + BLACKFEMALE + HISPANICMALE + HISPANICFEMALE + NotInLaborForce + less40k + over100k + Trump, data=dat, family=binomial(link="logit"))
plearn8
summary(plearn8)


# Load Census Data : dat <- read_csv("ACS_StateLevelData.csv")

state_ranefs <- array(NA, c(51, 1))


dimnames(state_ranefs) <- list(c(Census$NAME), 'effect')


# assign state random effects to array while preserving NAs
for (i in Census$NAME) {
  
  state_ranefs[i, ] <- ranef(plearn8)$NAME[i, 1]
  
}


Census$cellpred2 <- invlogit(fixef(plearn8)['(Intercept)']  + state_ranefs[Census$NAME, 1] + (fixef(plearn8)['Trump'] *Census$percent) + (fixef(plearn8)['highschool'] *Census$highschool) + (fixef(plearn8)['under35'] *Census$under35) + (fixef(plearn8)['under55'] *Census$under55) + (fixef(plearn8)['under65'] *Census$under65) + (fixef(plearn8)['WHITEMALE'] *Census$WHITEMALE) + (fixef(plearn8)['WHITEFEMALE'] *Census$WHITEFEMALE) + (fixef(plearn8)['BLACKMALE'] *Census$BLACKMALE) + (fixef(plearn8)['BLACKFEMALE'] *Census$BLACKFEMALE) + (fixef(plearn8)['HISPANICMALE'] *Census$HISPANICMALE) + (fixef(plearn8)['HISPANICFEMALE'] *Census$HISPANICFEMALE) + (fixef(plearn8)['NotInLaborForce'] *Census$NotInLaborForce) + (fixef(plearn8)['less40k'] *Census$less40k) + (fixef(plearn8)['over100k'] *Census$over100k))
Census$cellpred2


#Load US STATES shp::::    USSTATES <- read_sf("tl_2016_us_state.shp")
USSTATES$State <- USSTATES$NAME
USSTATES$NAME <- tolower(USSTATES$NAME)
table(USSTATES$State)

usstar <- merge(USSTATES, Census, by.x = "NAME", by.y = "NAME")
table(USSTATES$NAME)