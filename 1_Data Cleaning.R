########################
##  BD Meta-Analysis  ##
##   Data Cleaning    ##
########################

####  Startup  ####
rm(list = ls())
library(easypackages)
libraries("tidyverse", "openxlsx")



####  Load Data  ####
raw <- read.xlsx("C:\\Users\\isaac\\Google Drive\\Research\\Projects\\Body Dissatisfaction Meta-Analysis\\BD-depression-meta-analysis\\Data\\210602 Article Coding (Pre-Consensus).xlsx",
                 sheet = "Coding - Consensus", 
                 startRow = 3,
                 na.strings = c("NA", "NR"))



####  Clean Data  ####
## Select, recode variables
df <- raw %>%
  transmute(article = Article,
            year = Study.year,
            pubStatus = case_when(Publication.status == 1 ~ "Peer-reviewed",
                                  Publication.status == 2 ~ "Not peer-reviewed"),
            country = case_when(Country == 1 ~ "USA",
                                Country == 2 ~ "Other North America",
                                Country == 3 ~ "Other (Non-North America)"),
            sampleSource = case_when(Sample.source == 1 ~ "Community sample",
                                     Sample.source == 2 ~ "Outpatient sample",
                                     Sample.source == 3 ~ "Inpatient sample",
                                     Sample.source == 4 ~ "Other",
                                     Sample.source == 5 ~ "Composite",
                                     Sample.source == 6 ~ "Unknown"),
            optIn = case_when(`Opt-in` == 1 ~ "Opt-in",
                              `Opt-in` == 2 ~ "Expectated to participate"),
            compensation = case_when(Compensation == 1 ~ "Participants compensated",
                                     Compensation == 2 ~ "Participants not compensated"),
            meanAge = Mean.age,
            pctFemale = Percent.female,
            pctNonwhite = 100 - Percent.white,
            pctSexualMinority = Percent.sexual.minority,
            interventionType = case_when(Intervention.type == 1 ~ "Treatment",
                                         Intervention.type == 2 ~ "Indicated prevention",
                                         Intervention.type == 3 ~ "Selective prevention",
                                         Intervention.type == 4 ~ "Universal prevention"),
            riskScreen = case_when(Risk.screening == 1 ~ "No risk screening",
                                   Risk.screening == 2 ~ "Screen on ED symptoms",
                                   Risk.screening == 3 ~ "Screen on BD",
                                   Risk.screening == 4 ~ "Screen on depressive symptoms",
                                   Risk.screening == 5 ~ "Multiple/other"),
            blindAssign = case_when(Blind.assignment == 1 ~ "Blind assignment",
                                    Blind.assignment == 2 ~ "No blind assignment"),
            measure = Depression.measure,
            respondent = case_when(Respondent == 1 ~ "Youth-reported",
                                   Respondent == 2 ~ "Parent-reported",
                                   Respondent == 3 ~ "Clinician-reported",
                                   Respondent == 4 ~ "Other"),
            randUnits.group = Randomized.units,
            trtCompletion = Completion.rate,
            controlCond = case_when(Control.condition == 1 ~ "Waitlist/no treatment",
                                    Control.condition == 2 ~ "Psychoeducation",
                                    Control.condition == 3 ~ "Placebo",
                                    Control.condition == 4 ~ "Other"),
            interventionParticipant = case_when(Intervention.participant == 1 ~ "Youth",
                                                Intervention.participant == 2 ~ "Parent"),
            deliveryFormat = case_when(Intervention.delivery.format == 1 ~ "Self-administered",
                                       Intervention.delivery.format == 2 ~ "Group",
                                       Intervention.delivery.format == 3 ~ "Individual",
                                       Intervention.delivery.format == 4 ~ "Digital/online",
                                       Intervention.delivery.format == 5 ~ "Other"),
            target = case_when(Intervention.target == 1 ~ "Body dissatisfaction",
                               Intervention.target == 2 ~ "Other"),
            provider = case_when(Provider.type == 1 ~ "Self-administered",
                                 Provider.type == 2 ~ "Clinician-administered",
                                 Provider.type == 3 ~ "Lay-administered",
                                 Provider.type == 4 ~ "Other"),
            manual = case_when(Treatment.manual == 1 ~ "Treatment manual used",
                               Treatment.manual == 2 ~ "No treatment manual"),
            manualAvail = case_when(Manual.availability == 1 ~ "Manual publicly available",
                                    Manual.availability == 2 ~ "Manual not publicly available"),
            preTraining = case_when(`Pre-intervention.facilitator.training` == 1 ~ "Training",
                                    `Pre-intervention.facilitator.training` == 2 ~ "No training"),
            supervision = case_when(`Facilitator.supervision/consultation` == 1 ~ "Supervision",
                                    `Facilitator.supervision/consultation` == 2 ~ "No supervision"),
            length.minutes = `Intervention.length.(minutes)`,
            length.sessions = `Intervention.length.(sessions)`,
            length.days = `Intervention.length.(days)`,
            group = gsub("\\s.*$", "", Group),
            groupDetailed = Group,
            outcome = case_when(`Depression-related.construct` == 1 ~ "Depression Symptomatology",
                                `Depression-related.construct` == 2 ~ "Trait Negative Affect",
                                T ~ NA_character_),
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
  mutate(studyID = cur_group_id(),
         studyCompletion.post = 100 * sum(post.n) / sum(pre.n),
         studyCompletion.fu1 = 100 * sum(fu1.n) / sum(pre.n),
         studyCompletion.fu2 = 100 * sum(fu2.n) / sum(pre.n),
         randUnits = if_else(mean(randUnits.group) >= 30, ">= 30 per group", "< 30 per group")) %>%
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
    article:blindAssign, studyCompletion.post, studyCompletion.fu1, studyCompletion.fu2, randUnits) %>%
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
    trtCompletion, interventionParticipant:length.days, groupDetailed, pre.n:fu2.sd
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
    controlCond, groupDetailed, pre.n:fu2.sd
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
