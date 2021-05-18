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
#Calculate Cohen's d using pre- and post-test data (for studies where this is available)
#Done by taking the mean pre-post change in the treatment group minus the mean pre-post change in the control group, divided by the pooled pretest standard deviation.
  #See Morris, S. B. (2008). Estimating effect sizes from pretest-posttest-control group designs. Organizational research methods, 11(2), 364-386.
calculateD.t1t2 <- function(y1.t, y2.t, n1.t, s1.t,
                            y1.c, y2.c, n1.c, s1.c) {
  
  #Calculate pre-post change in treatment group
  diff.t = y2.t - y1.t
  
  #Calculate pre-post change in control group
  diff.c = y2.c - y1.c
  
  #Calculate pooled pre-test standard deviation
  sPooled = sqrt((((n1.t - 1) * s1.t^2) + ((n1.c - 1) * s1.c^2)) / ((n1.c - 1) + (n1.t - 1)))
  
  #Calculate effect size
  d = (diff.t - diff.c) / sPooled
  
  return(d)
  
}

#Calculate Cohen's d using post-test data only
#Done by taking difference in means divided by the pooled posttest standard deviation
  #See Borenstein, Michael, Larry V. Hedges, Julian P. T. Higgins, and Hannah R. Rothstein. 2009. Introduction to Meta-Analysis. John Wiley & Sons.
calculateD.t2 <- function(y2.t, n2.t, s2.t,
                          y2.c, n2.c, s2.c) {
  
  #Calculate between-group effect size using post-test (adjusted) means
  sPooled = sqrt((((n2.t - 1) * s2.t^2) + ((n2.c - 1) * s2.c^2)) / ((n2.t - 1) + (n2.c - 1)))
  d = (y2.t - y2.c) / sPooled
  
  return(d)
  
}

#Calculate correction factor J for Hedges g
  #See Borenstein, Michael, Larry V. Hedges, Julian P. T. Higgins, and Hannah R. Rothstein. 2009. Introduction to Meta-Analysis. John Wiley & Sons.
calculateJ <- function(n.t, n.c) {
  
  df = n.t + n.c - 2
  J = 1 - (3 / ((4 * df) - 1))
  
}

#Calculate variance of Cohen's d
  #See Borenstein, Michael, Larry V. Hedges, Julian P. T. Higgins, and Hannah R. Rothstein. 2009. Introduction to Meta-Analysis. John Wiley & Sons.
calculateVarD <- function(n.t, n.c, d) {
  
  varD = ((n.t + n.c) / (n.t * n.c)) + ((d^2) / (2 * (n.t + n.c)))
  
  return(varD)
  
}

df.ES <- df %>%
  mutate(
    
    effectLabel = paste0(if_else(meanType == "Adjusted", "*", ""), t.groupDetailed, " vs ", c.groupDetailed, ", ", outcome),
    
    #Calculate Cohen's d effect size (post-test)
    d.post = case_when(
      
      meanType == "Unadjusted" ~ calculateD.t1t2(y1.t = t.pre.mean, y2.t = t.post.mean, n1.t = t.pre.n, s1.t = t.pre.sd,
                                                 y1.c = c.pre.mean, y2.c = c.post.mean, n1.c = c.pre.n, s1.c = c.pre.sd),
      meanType == "Adjusted" ~ calculateD.t2(y2.t = t.post.mean, n2.t = t.post.n, s2.t = t.post.sd,
                                             y2.c = c.post.mean, n2.c = c.post.n, s2.c = c.post.sd)
      
    ),
    
    #Calculate Cohen's d effect size (post-test), this one using only post-test data
    d.post_alt = calculateD.t2(y2.t = t.post.mean, n2.t = t.post.n, s2.t = t.post.sd,
                               y2.c = c.post.mean, n2.c = c.post.n, s2.c = c.post.sd),
    
    #Calculate variance of Cohen's d effect size
    varD.post = calculateVarD(n.t = t.post.n, n.c = c.post.n, d = d.post),
    varD.post_alt = calculateVarD(n.t = t.post.n, n.c = c.post.n, d = d.post_alt),
    
    #Calculate correction factor J for Hedge's g
    J.post = calculateJ(n.t = t.post.n, n.c = c.post.n),
    
    #Calculate Hedge's g using Cohen's d and correction factor J
      #See Borenstein, Michael, Larry V. Hedges, Julian P. T. Higgins, and Hannah R. Rothstein. 2009. Introduction to Meta-Analysis. John Wiley & Sons.
    g.post = d.post * J.post,
    g.post_alt = d.post_alt * J.post,
    
    #Calculate variance of Hedge's g using variance of Cohen's d and correction factor J
      #See Borenstein, Michael, Larry V. Hedges, Julian P. T. Higgins, and Hannah R. Rothstein. 2009. Introduction to Meta-Analysis. John Wiley & Sons.
    varG.post = J.post^2 * varD.post,
    varG.post_alt = J.post^2 * varD.post_alt
    
  )



####  Save Data  ####
saveRDS(df.ES,
        "C:\\Users\\isaac\\Google Drive\\Research\\Projects\\Body Dissatisfaction Meta-Analysis\\BD-depression-meta-analysis\\Data\\Data With Effect Sizes.rds")
