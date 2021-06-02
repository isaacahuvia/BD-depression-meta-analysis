######################################
## Calculate Inter-rater Agreement  ##
######################################

####  Startup  ####
rm(list = ls())
library(easypackages)
libraries("tidyverse", "openxlsx", "irr")



####  Load Data  ####
dfI <- read.xlsx("C:\\Users\\isaac\\Google Drive\\Research\\Projects\\Body Dissatisfaction Meta-Analysis\\Independent Article Coding for IRR - Isaac.xlsx",
                 startRow = 3,
                 na.strings = c("NA", "NR", "unknown"))

dfL <- read.xlsx("C:\\Users\\isaac\\Google Drive\\Research\\Projects\\Body Dissatisfaction Meta-Analysis\\Independent Article Coding for IRR - Laura.xlsx",
                 startRow = 3,
                 na.strings = c("NA", "NR", "unknown"))



####  Calculate IRR  ####
studyNames <- c("Study.year", "Publication.status", "Country", "Sample.source", "Opt-in", "Compensation", "Mean.age", "Minimum.age", "Maximum.age", "Percent.female", "Percent.white", "Percent.black", "Percent.hispanic", "Percent.asian", "Percent.Native.American", "Percent.other", "Percent.multi", "Percent.not.reported", "Percent.sexual.minority", "Intervention.type", "Risk.screening", "Blind.assignment")
outcomeNames <- c("Respondent", "Depression-related.construct")
groupNames <- c("Randomized.units", "n.(assigned)", "n.(started)", "n.(completed)", "Control.condition", "Intervention.delivery.format", "Intervention.target", "Provider.type", "Treatment.manual", "Manual.availability", "Pre-intervention.facilitator.training", "Facilitator.supervision/consultation", "Intervention.length.(minutes)", "Intervention.length.(sessions)", "Intervention.length.(days)", "Completion.rate")

nominalNames <- c("Study.year", "Publication.status", "Country", "Sample.source", "Opt-in", "Compensation", "Intervention.type" ,"Risk.screening", "Blind.assignment", "Respondent", "Depression-related.construct", "Control.condition", "Intervention.delivery.format", "Intervention.target", "Provider.type", "Treatment.manual", "Manual.availability", "Pre-intervention.facilitator.training", "Facilitator.supervision/consultation")
continuousNames <- setdiff(names(dfI), nominalNames)

out <- tibble(
  name = c(studyNames, outcomeNames, groupNames),
  IRR = NA_real_,
  pctAgree = NA_real_
) %>%
  mutate(category = case_when(name %in% studyNames ~ "Study",
                              name %in% outcomeNames ~ "Outcome",
                              name %in% groupNames ~ "Group"),
         class = case_when(name %in% nominalNames ~ "Nominal",
                           name %in% continuousNames ~ "Continuous"))
  
for(i in 1:nrow(out)) {
  
  mat = cbind(dfI[,out$name[i]], dfL[,out$name[i]])
  
  if(all(is.na(mat))) {
    
    out$IRR[i] = NA
    out$pctAgree[i] = NA
    
  } else if(out$class[i] == "Nominal") {
    
    k = kappa2(mat)
    out$IRR[i] = k$value
    out$pctAgree[i] = mean(c(mat[,1] == mat[,2] | (is.na(mat[,1]) & is.na(mat[,2]))), na.rm = T)
    
  } else if(out$class[i] == "Continuous") {
    
    storage.mode(mat) = "numeric"
    icc = icc(mat)
    out$IRR[i] = icc$value
    out$pctAgree[i] = mean(c(mat[,1] == mat[,2] | (is.na(mat[,1]) & is.na(mat[,2]))), na.rm = T)
    
  }
  
}

out %>%
  mutate(pctAgree = if_else(pctAgree > IRR, pctAgree, NA_real_)) %>%
  pivot_longer(cols = IRR:pctAgree,
               names_to = "metric") %>%
  ggplot(aes(x = value, y = name)) +
  geom_point(aes(color = metric)) +
  facet_grid(category ~ .,
             scales = "free",
             space = "free",
             switch = "y") +
  scale_y_discrete(name = NULL) +
  theme(strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0)) +
  geom_vline(xintercept = .8)
