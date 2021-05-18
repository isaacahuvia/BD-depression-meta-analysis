################################
##      BD Meta-Analysis      ##
##  Calculating Effect Sizes  ##
################################

####  Startup  ####
rm(list = ls())
library(easypackages)
libraries("tidyverse")



####  Load Data  ####
df <- readRDS("C:\\Users\\isaac\\Google Drive\\Research\\Projects\\Body Dissatisfaction Meta-Analysis\\BD-depression-meta-analysis\\Data\\Clean Data (Paired).rds")



####  Calculate Effect Sizes  ####
## Define functions
calculateES.unadjusted <- function(t.y1, t.y2, t.n1, t.n2, t.s1, t.s2,
                                   c.y1, c.y2, c.n1, c.n2, c.s1, c.s2) {
  
  #Calculate treatment group pre-post effect size 
  t.sPooled = sqrt((((t.n1 - 1) * t.s1^2) + ((t.n2 - 1) * t.s2^2)) / ((t.n1 - 1) + (t.n2 - 1)))
  t.g = (t.y2 - t.y1) / t.sPooled
  
  #Calculate control group pre-post effect size
  c.sPooled = sqrt((((c.n1 - 1) * c.s1^2) + ((c.n2 - 1) * c.s2^2)) / ((c.n1 - 1) + (c.n2 - 1)))
  c.g = (c.y2 - c.y1) / c.sPooled
  
  #Calculate between-group effect size by taking the difference
  #Adjust for small sample size?
  g = t.g - c.g
  
  return(g)
  
}

calculateES.adjusted <- function(t.y2, t.n2, t.s2,
                                 c.y2, c.n2, c.s2) {
  
  #Calculate between-group effect size using adjusted means
  sPooled = sqrt((((t.n2 - 1) * t.s2^2) + ((c.n2 - 1) * c.s2^2)) / ((t.n2 - 1) + (c.n2 - 1)))
  g = (t.y2 - c.y2) / sPooled
  
}

df.ES <- df %>%
  mutate(
    
    effectLabel = paste0(if_else(meanType == "Adjusted", "*", ""), t.groupDetailed, " vs ", c.groupDetailed, ", ", outcome),
    
    #Calculate effect size (post-test)
    effectSize.post = case_when(
      
      meanType == "Unadjusted" ~ calculateES.unadjusted(t.y1 = t.pre.mean, t.n1 = t.pre.n, t.s1 = t.pre.sd,
                                                        t.y2 = t.post.mean, t.n2 = t.post.n, t.s2 = t.post.sd,
                                                        c.y1 = c.pre.mean, c.n1 = c.pre.n, c.s1 = c.pre.sd,
                                                        c.y2 = c.post.mean, c.n2 = c.post.n, c.s2 = c.post.sd),
      meanType == "Adjusted" ~ calculateES.adjusted(t.y2 = t.post.mean, t.n2 = t.post.n, t.s2 = t.post.sd,
                                                    c.y2 = c.post.mean, c.n2 = c.post.n, c.s2 = c.post.sd)
      
    ),
    
    #Calculate effect size variance (post-test) - don't think this is quite how it works!!
    variance.post = 1 / (t.post.n + c.post.n),
    
    #Calculate effect size (follow-up 1)
    effectSize.fu1 = case_when(
      
      meanType == "Unadjusted" ~ calculateES.unadjusted(t.y1 = t.pre.mean, t.n1 = t.pre.n, t.s1 = t.pre.sd,
                                                        t.y2 = t.fu1.mean, t.n2 = t.fu1.n, t.s2 = t.fu1.sd,
                                                        c.y1 = c.pre.mean, c.n1 = c.pre.n, c.s1 = c.pre.sd,
                                                        c.y2 = c.fu1.mean, c.n2 = c.fu1.n, c.s2 = c.fu1.sd),
      meanType == "Adjusted" ~ calculateES.adjusted(t.y2 = t.fu1.mean, t.n2 = t.fu1.n, t.s2 = t.fu1.sd,
                                                    c.y2 = c.fu1.mean, c.n2 = c.fu1.n, c.s2 = c.fu1.sd)
      
    ),
    
    #Calculate effect size variance (follow-up 1) - don't think this is quite how it works!!
    variance.fu1 = 1 / (t.fu1.n + c.fu1.n),
    
    time.fu1 = t.fu1.time,
    
    #Calculate effect size (last follow-up)
    effectSize.fu2 = case_when(
      
      meanType == "Unadjusted" ~ calculateES.unadjusted(t.y1 = t.pre.mean, t.n1 = t.pre.n, t.s1 = t.pre.sd,
                                                        t.y2 = t.fu2.mean, t.n2 = t.fu2.n, t.s2 = t.fu2.sd,
                                                        c.y1 = c.pre.mean, c.n1 = c.pre.n, c.s1 = c.pre.sd,
                                                        c.y2 = c.fu2.mean, c.n2 = c.fu2.n, c.s2 = c.fu2.sd),
      meanType == "Adjusted" ~ calculateES.adjusted(t.y2 = t.fu2.mean, t.n2 = t.fu2.n, t.s2 = t.fu2.sd,
                                                    c.y2 = c.fu2.mean, c.n2 = c.fu2.n, c.s2 = c.fu2.sd)
      
    ),
    
    #Calculate effect size variance (last follow-up) - don't think this is quite how it works!!
    variance.fu2 = 1 / (t.fu2.n + c.fu2.n),
    
    time.fu2 = t.fu2.time
    
  )



####  Save Data  ####
saveRDS(df.ES,
        "C:\\Users\\isaac\\Google Drive\\Research\\Projects\\Body Dissatisfaction Meta-Analysis\\BD-depression-meta-analysis\\Data\\Data With Effect Sizes.rds")
