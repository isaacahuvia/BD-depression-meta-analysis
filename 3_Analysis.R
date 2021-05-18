########################
##  BD Meta-Analysis  ##
##      Analysis      ##
########################

####  Startup  ####
rm(list = ls())
library(easypackages)
libraries("tidyverse", "robumeta", "magrittr")



####  Load Data  ####
df <- readRDS("C:\\Users\\isaac\\Google Drive\\Research\\Projects\\Body Dissatisfaction Meta-Analysis\\BD-depression-meta-analysis\\Data\\Data With Effect Sizes.rds")



####  Analysis  ####
## Meta-analysis with robumeta
## https://cran.r-project.org/web/packages/robumeta/vignettes/robumetaVignette.pdf

## Post-test
#Intercept-only model
model.IO <- robu(formula = effectSize.post ~ 1, 
                 data = df, 
                 studynum = studyID, 
                 var.eff.size = variance.post, 
                 rho = .8, #What should we have here? Doesn't seem to make a diff in output
                 small = T)

print(model.IO)

dev.off()
forest.robu(model.IO,
            es.lab = "effectLabel",
            study.lab = "article")


## Moderation tests
modTest <- function(var) robu(formula = as.formula(paste0("effectSize.post ~ 1 + ", var)),
                              data = df,
                              studynum = studyID,
                              var.eff.size = variance.post,
                              rho = .8,
                              small = T)

## For the following variables where >=3 per group
#Publication status (peer-reviewed vs not)
count(df, pubStatus)

#Study country
count(df, country)
df %<>%
        mutate(country = case_when(country == 1 ~ 0,
                                   country == 3 ~ 1,
                                   T ~ NA_real_))
modTest("country")

#Sample source
count(df, sampleSource)

#Participant age (mean age)
count(df, meanAge)
df %<>%
        mutate(meanAge = meanAge - mean(meanAge, na.rm = T))
modTest("meanAge")

#Participant gender (percent female)
count(df, pctFemale)
modTest("pctFemale")

#Participant race (percent non-white)
count(df, pctNonwhite)
modTest("pctNonwhite")

#Participant sexual orientation (percent sexual minority)
count(df, pctSexualMinority)

#Intervention type
count(df, interventionType)
df %<>%
        mutate(interventionType = case_when(interventionType == 2 ~ 0,
                                            interventionType == 4 ~ 1,
                                            T ~ NA_real_))
modTest("interventionType")

#Risk screening
count(df, riskScreen)
df %<>%
        mutate(riskScreen = case_when(riskScreen == 1 ~ 0,
                                      riskScreen == 3 ~ 1,
                                      T ~ NA_real_))
modTest("riskScreen")

#Blind assignment
count(df, blindAssign)
modTest("blindAssign")

#Depression-related construct
count(df, outcome)
modTest("outcome")

#Control condition
count(df, controlCond)

#Intervention delivery format
count(df, deliveryFormat)

#Intervention target - REDO AFTER RECODING
count(df, target)
modTest("target")

#Provider type
count(df, provider)

#Pre-intervention facilitator training (present vs. absent)
count(df, preTraining)

#Facilitator supervision/consultation (present vs. absent)
count(df, supervision)
modTest("supervision")

#Intervention length (minutes)
count(df, length.minutes)
modTest("length.minutes")

#Intervention length (sessions)
count(df, length.sessions)
modTest("length.sessions")

#Intervention length (days)
count(df, length.days)
modTest("length.days")

#Treatment completion rate (compliance rate among those assigned to treatment)
count(df, trtCompletion)
modTest("trtCompletion")

#Study completion rate (post-intervention data availability among all those in study)
df %<>%
        mutate(studyCompletion = (t.post.n + c.post.n) / (t.pre.n + c.pre.n))
count(df, studyCompletion)
modTest("studyCompletion")


## Also (not pre-registered)
#Mean type (adjusted vs. non-adjusted)
count(df, meanType)
modTest("meanType")

#Randomized units (TBD; find appropriate cutpoint)
df %<>%
        mutate(largeRCT = (t.randUnits + c.randUnits) > 30)
count(df, largeRCT)
modTest("country")


robu(formula = effectSize.post ~ 
             1 + 
             country + 
             meanAge +
             pctFemale +
             pctNonwhite +
             interventionType +
             riskScreen +
             blindAssign +
             # outcome + 
             # target + 
             # supervision + 
             # length.minutes + 
             # length.days + 
             # length.sessions +
             # trtCompletion + 
             # # studyCompletion + 
             meanType,
     data = df,
     studynum = studyID,
     var.eff.size = variance.post,
     rho = .8,
     small = T)





## Follow-up
#For follow-up data, reshape to long dataset, code for follow-up time-point, and keep only follow-up outcomes
df.fu <- df %>%
        select(-c(effectSize.post, variance.post)) %>%
        pivot_longer(cols = effectSize.fu1:time.fu2,
                     names_to = c("var", "followUp"),
                     names_sep = "\\.") %>%
        pivot_wider(names_from = "var",
                    values_from = "value") %>%
        drop_na(effectSize, variance, time) %>%
        mutate(effectLabel = paste0(effectLabel, " (", time, " weeks)"))

#Intercept-only model
model.IO <- robu(formula = effectSize ~ 1, 
                 data = df.fu, 
                 studynum = studyID, 
                 var.eff.size = variance, 
                 rho = .8, #What should we have here? Doesn't seem to make a diff in output
                 small = T)

print(model.IO)

dev.off()
forest.robu(model.IO,
            es.lab = "effectLabel",
            study.lab = "article")


