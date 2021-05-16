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
            interventionType = Intervention.type,
            riskScreen = Risk.screening,
            blindAssign = Blind.assignment,
            measure = Depression.measure,
            respondent = Respondent,
            construct = `Depression-related.construct`,
            randUnits = Randomized.units,
            trtCompletion = Completion.rate,
            
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
            fu1.n = `n.(fu1)`,
            fu1.mean = `mean.(fu1)`,
            fu1.sd = `sd.(fu1)`,
            fu2.n = `n.(fu2)`,
            fu2.mean = `mean.(fu2)`,
            fu2.sd = `sd.(fu2)`) %>%
  #Filter out articles with missing outcomes
  drop_na(post.n:post.sd) %>%
  #Create a unique numeric ID for each article (robu will require a numeric, not character, ID)
  group_by(article) %>%
  mutate(studyID = cur_group_id()) %>%
  ungroup()



####  Save Data  ####
saveRDS(df,
        "C:\\Users\\isaac\\Google Drive\\Research\\Projects\\Body Dissatisfaction Meta-Analysis\\BD-depression-meta-analysis\\Data\\Clean Data.rds")
