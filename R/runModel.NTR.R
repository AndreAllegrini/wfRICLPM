library(lavaan)
library(devtools)

source_url("https://github.com/AndreAllegrini/wfRICLPM/blob/master/R/RICLPM.R") #lavaan model

load(url("https://github.com/AndreAllegrini/wfRICLPM/tree/master/matrices/covobs_NTR.RData")) #load RData

wf_clpm.cov <- covObs #variance covariance matrix 
wf_clpm.n <- 17113 #NTR sample size 

RICLPM_git <- lavaan(RICLPM, 
               sample.cov = wf_clpm.cov, 
               sample.nobs = wf_clpm.n,
               missing = 'ml',
               int.ov.free = F,
               int.lv.free = F,
               auto.fix.first = F,
               auto.fix.single = F,
               auto.cov.lv.x = F,
               auto.cov.y = F,
               auto.var = F)

summary(RICLPM_git, standardized = TRUE)
