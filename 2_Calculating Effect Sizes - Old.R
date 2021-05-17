################################
##      BD Meta-Analysis      ##
##  Calculating Effect Sizes  ##
################################

####  Startup  ####
rm(list = ls())
library(easypackages)
libraries("tidyverse")



####  Load Data  ####
df <- readRDS("C:\\Users\\isaac\\Google Drive\\Research\\Projects\\Body Dissatisfaction Meta-Analysis\\BD-depression-meta-analysis\\Data\\Clean Data.rds")



####  Calculate Effect Sizes  ####
out <- tibble(
  studyID = NULL,
  effect = NULL,
  effectLabel = NULL,
  variance = NULL
)

for(x in unique(df$studyID)) {
  
  temp.allOutcomes <- df[df$studyID == x,]
  
  for(y in unique(temp.allOutcomes$outcome)) {
    
    temp <- temp.allOutcomes[temp.allOutcomes$outcome == y,]
    
    temp.t <- temp[temp$group == "Treatment",]
    temp.c <- temp[temp$group == "Control",]
    
    for(i in 1:nrow(temp.t)) {
      
      for(j in 1:nrow(temp.c)) {
        
        if(temp$meanType[1] == "Unadjusted") {
          
          #Calculating treatment group pre-post effect size
          #https://www.itl.nist.gov/div898/software/dataplot/refman2/auxillar/hedgeg.htm
          y1.t = temp.t$pre.mean[i]
          y2.t = temp.t$post.mean[i]
          
          n1.t = temp.t$pre.n[i]
          n2.t = temp.t$post.n[i]
          
          s1.t = temp.t$pre.sd[i]
          s2.t = temp.t$post.sd[i]
          
          sp.t = sqrt((((n1.t - 1) * s1.t^2) + ((n2.t - 1) * s2.t^2)) / ((n1.t - 1) + (n2.t - 1)))
          
          g.t = (y2.t - y1.t) / sp.t
          
          #Calculating control group pre-post effect size
          y1.c = temp.c$pre.mean[j]
          y2.c = temp.c$post.mean[j]
          
          n1.c = temp.c$pre.n[j]
          n2.c = temp.c$post.n[j]
          
          s1.c = temp.c$pre.sd[j]
          s2.c = temp.c$post.sd[j]
          
          sp.c = sqrt((((n1.c - 1) * s1.c^2) + ((n2.c - 1) * s2.c^2)) / ((n1.c - 1) + (n2.c - 1)))
          
          g.c = (y2.c - y1.c) / sp.c
          
          #Calculate between-group effect size by taking the difference
          #Adjust for small sample size?
          g = g.t - g.c
          
        } else if(temp$meanType[1] == "Adjusted") {
          
          y.t = temp.t$post.mean[i]
          y.c = temp.c$post.mean[j]
          
          n.t = temp.t$post.n[i]
          n.c = temp.c$post.n[j]
          
          s.t = temp.t$post.sd[i]
          s.c = temp.c$post.sd[j]
          
          sp = sqrt((((n.t - 1) * s.t^2) + ((n.c - 1) * s.c^2)) / ((n.t - 1) + (n.c - 1)))
          
          g = (y.t - y.c) / sp
          
        }
        
        newRow <- tibble(
          
          studyID = x,
          effect = g,
          effectLabel = paste0(if(temp$meanType[1] == "Adjusted")"*", temp.t$groupDetailed[i], " vs ", temp.c$groupDetailed[j], ", ", temp$outcome[1]),
          variance = 1 / (n2.t + n2.c) #Not sure this is how it works!
          
        )
        
        out <- rbind(out, newRow)
        
      }
      
    }
    
  }
  
}


## Merge on study information
#Issue - how do we merge on the group-specific information?
out <- left_join(out, 
                 df %>%
                   select(studyID, article:riskScreen) %>%
                   filter(!duplicated(.)),
                 by = "studyID")



####  Save Data  ####
saveRDS(out,
        "C:\\Users\\isaac\\Google Drive\\Research\\Projects\\Body Dissatisfaction Meta-Analysis\\BD-depression-meta-analysis\\Data\\Effect Sizes.rds")
