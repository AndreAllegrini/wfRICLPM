

load(url("https://github.com/AndreAllegrini/wfRICLPM/tree/master/matrices/covobs_NTR.wf.RData")) #load RData

wf_clpm.cov <- covObs #variance covariance matrix 
wf_clpm.n <- list(DZ=11072,MZ=6056) #NTR sample size 

wf_RICLPM_git <- lavaan(wf_RICLPM, 
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


summary(wf_RICLPM_git, standardized = TRUE)
