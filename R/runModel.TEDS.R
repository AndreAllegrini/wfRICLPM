library(lavaan)

load(url("https://github.com/AndreAllegrini/wfRICLPM/tree/master/matrices/covobs_TEDS.wf.RData")) #load RData

wf_clpm.cov <- covObs #variance covariance matrix 
wf_clpm.n <- list(DZ=5465,MZ=2979) #TEDS sample size 


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

