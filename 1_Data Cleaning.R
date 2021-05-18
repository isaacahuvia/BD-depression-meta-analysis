########################
##  BD Meta-Analysis  ##
##   Data Cleaning    ##
########################

####  Startup  ####
rm(list = ls())
library(easypackages)
libraries("tidyverse", "openxlsx")



####  Load Data  ####
raw <- read.xlsx("C:\\Users\\isaac\\Google Drive\\Research\\Projects\\Body Dissatisfaction Meta-Analysis\\BD-depression-meta-analysis\\Data\\210515 Article Coding (Pre-Consensus).xlsx",
                 sheet = "Coding - Consensus", 
                 startRow = 3,
                 na.strings = c("NA", "NR"))



####  Clean Data  ####
## Select, recode variables
df <- raw %>%
  transmute(article = Article,
            year = Study.year,
            pubStatus = Publication.status,
            country = Country,
            sampleSource = Sample.source,
            optIn = `Opt-in`,
            compensation = Compensation,
            meanAge = Mean.age,
            pctFemale = Percent.female,
            pctNonwhite = 100 - Percent.white,
            pctSexualMinority = Percent.sexual.minority,
            interventionType = Intervention.type,
            riskScreen = Risk.screening,
            blindAssign = Blind.assignment,
            measure = Depression.measure,
            respondent = Respondent,
            randUnits = Randomized.units,
            trtCompletion = Completion.rate,
            controlCond = Control.condition,
            interventionParticipant = Intervention.participant,
            deliveryFormat = Intervention.delivery.format,
            target = Intervention.target,
            provider = Provider.type,
            manual = Treatment.manual,
            manualAvail = Manual.availability,
            preTraining = `Pre-intervention.facilitator.training`,
            supervision = `Facilitator.supervision/consultation`,
            length.minutes = `Intervention.length.(minutes)`,
            length.sessions = `Intervention.length.(sessions)`,
            length.days = `Intervention.length.(days)`,
            group = gsub("\\s.*$", "", Group),
            groupDetailed = Group,
            outcome = case_when(`Depression-related.construct` == 1 ~ "Depression Symptomatology",
                                `Depression-related.construct` == 2 ~ "Trait Negative Affect",
                                T ~ NA_character_),
            meanType = `Mean.Type`,
            pre.n = `n.(pre)`,
            pre.mean = `mean.(pre)`,
            pre.sd = `sd.(pre)`,
            post.n = `n.(post)`,
            post.mean = `mean.(post)`,
            post.sd = `sd.(post)`,
            fu1.time = `time.(fu1)`,
            fu1.n = `n.(fu1)`,
            fu1.mean = `mean.(fu1)`,
            fu1.sd = `sd.(fu1)`,
            fu2.time = `time.(fu2)`,
            fu2.n = `n.(fu2)`,
            fu2.mean = `mean.(fu2)`,
            fu2.sd = `sd.(fu2)`) %>%
  #Filter out articles with missing outcomes
  drop_na(post.n:post.sd) %>%
  #Create a unique numeric ID for each article (robu will require a numeric, not character, ID)
  group_by(article) %>%
  mutate(studyID = cur_group_id()) %>%
  ungroup()


## Reshape data
#We want one row for each effect size, with all the study- and group-level characteristics on there too
#First, split the data into three datasets with a common study ID
#df.study: Study-level characteristics (anything not specific to the treatment or control group)
df.study <- df %>%
  select(
    #Study ID (to use as an identifier during merge)
    studyID, 
    #Study variables
    article:blindAssign, meanType) %>%
  filter(!duplicated(.))

#df.trt: Group- and measure-level characteristics for treatment groups only
df.treatment <- df %>%
  #Filter to treatment group only
  filter(group == "Treatment") %>%
  #Select relevant variables
  select(
    #Study ID (to use as an identifier during merge)
    studyID,
    #Measure information that will also be used as identifiers during merge
    #(Because we want to match treatment and control groups that measure the same outcome)
    outcome, measure:respondent,
    #Treatment group information
    trtCompletion, interventionParticipant:length.days, groupDetailed, randUnits, pre.n:fu2.sd
    ) %>%
  #Rename variables
  rename_at(vars(groupDetailed:fu2.sd), ~ paste0("t.", .))

#df.trt: Group- and measure-level characteristics for control groups only
df.control <- df %>%
  #Filter to control group only
  filter(group == "Control") %>%
  #Select relevant variables
  select(
    #Study ID (to use as an identifier during merge)
    studyID,
    #Measure information that will also be used as identifiers during merge
    #(Because we want to match treatment and control groups that measure the same outcome)
    outcome, measure:respondent,
    #Control group information
    controlCond, groupDetailed, randUnits, pre.n:fu2.sd
  ) %>%
  #Rename variables
  rename_at(vars(groupDetailed:fu2.sd), ~ paste0("c.", .))

## Merge on study and measure
#Create dataset of treatment:control group pairs
df.pairs <- left_join(df.treatment,
                      df.control,
                      by = c("studyID", "outcome", "measure", "respondent"))

#Add on study variables
df.pairs <- left_join(df.study,
                      df.pairs,
                      by = "studyID")



####  Save Data  ####
saveRDS(df.pairs,
        "C:\\Users\\isaac\\Google Drive\\Research\\Projects\\Body Dissatisfaction Meta-Analysis\\BD-depression-meta-analysis\\Data\\Clean Data (Paired).rds")
