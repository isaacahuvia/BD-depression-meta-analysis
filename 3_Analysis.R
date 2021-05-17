########################
##  BD Meta-Analysis  ##
##      Analysis      ##
########################

####  Startup  ####
rm(list = ls())
library(easypackages)
libraries("tidyverse", "robumeta")



####  Load Data  ####
out <- readRDS("C:\\Users\\isaac\\Google Drive\\Research\\Projects\\Body Dissatisfaction Meta-Analysis\\BD-depression-meta-analysis\\Data\\Effect Sizes.rds")
df <- readRDS("C:\\Users\\isaac\\Google Drive\\Research\\Projects\\Body Dissatisfaction Meta-Analysis\\BD-depression-meta-analysis\\Data\\Data With Effect Sizes.rds")



####  Analysis  ####
## Meta-analysis with robumeta
#https://cran.r-project.org/web/packages/robumeta/vignettes/robumetaVignette.pdf

#Intercept-only model
model.IO.old <- robu(formula = effect ~ 1,
                 data = out, 
                 studynum = studyID, 
                 var.eff.size = variance, 
                 rho = .8, #What should we have here? Doesn't seem to make a diff in output
                 small = T)

print(model.IO.old)

model.IO <- robu(formula = effectSize.post ~ 1, 
                 data = df, 
                 studynum = studyID, 
                 var.eff.size = variance, 
                 rho = .8, #What should we have here? Doesn't seem to make a diff in output
                 small = T)

print(model.IO)

dev.off()
forest.robu(model.IO,
            es.lab = "effectLabel",
            study.lab = "article")

## Moderation tests
## For the following variables where >=3 per group
#Publication status (peer-reviewed vs not)
#Study country
#Sample source
#Participant age (mean age)
#Participant gender (percent female)
#Participant race (percent non-white)
#Participant sexual orientation (percent sexual minority)
#Intervention type
#Risk screening
#Respondent
#Depression-related construct
#Control condition
#Intervention delivery format
#Intervention target
#Provider type
#Pre-intervention facilitator training (present vs. absent)
#Facilitator supervision/consultation (present vs. absent)
#Intervention length (minutes)
#Intervention length (sessions)
#Intervention length (days)
#Treatment completion rate (compliance rate among those assigned to treatment)
#Follow-up length (3 wk vs 3-12 wk vs 12 wk)
#Study completion rate (post-intervention data availability among all those in study)

## Also (not pre-registered)
#Mean type (adjusted vs. non-adjusted)
#Randomized units (TBD; find appropriate cutpoint)

#Outcome
robu(formula = effect ~ 1 + outcome, 
     data = out, 
     studynum = studyID, 
     var.eff.size = variance, 
     rho = .8, 
     small = T)

#Mean type
robu(formula = effect ~ 1 + meanType, 
     data = out, 
     studynum = studyID, 
     var.eff.size = variance, 
     rho = .8, 
     small = T)

#Randomized units
robu(formula = effect ~ 1 + (randUnits > 30), 
     data = out, 
     studynum = studyID, 
     var.eff.size = variance, 
     rho = .8, 
     small = T)

#Percent female
#Randomized units
robu(formula = effect ~ 1 + pctFemale, 
     data = out, 
     studynum = studyID, 
     var.eff.size = variance, 
     rho = .8, 
     small = T)
