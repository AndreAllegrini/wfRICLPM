# wfRICLPM

wfRICLPM is a function to create a RI-CLPM lavaan model including sibling regressions overtime, while taking into account the fact that family members resemble eachother. See below for model specification. 

Things to update: 

* constrained version overtime 
* only (DZ) siblings (no twins)
* add Falconer's formula

Source as: 

```{r}
source('https://github.com/AndreAllegrini/wfRICLPM/tree/master/R/wfRICLPM.func.R')
```

wfRICLPM takes in a list of traits and measurement occasions, and a vector of siblings subscripts (currently limited to 2, e.g. sibSub = c("i", "j")) 

```{r}

varNames = list(Trait_1 = c("t11", "t12", "t13"),
                Trait_2 = c("t21", "t22", "t23"),
                Trait_3 = c("t31", "t32", "t33"))

sibSub = c("a", "b")
```

and outputs a list containing wfRICLPM building blocks, and the full model to be fed to lavaan. 

Current version assumes monozygotic and dyzogotyc twin paris are supplied.

## Run as: 

```{r}

obj <- wfRICLPM(varNames = varNames, sibSub = sibSub, constrained = FALSE)

#print model 
cat(obj$model)

#load lavaan
library(lavaan)

#run model
fit <- lavaan(obj$model,  
          data = data, # your data
          group = "zygozity", # name of zygosity varaible, function assumes a character vector 'MZ' vs 'DZ'
          missing = 'ML', # how you handle missingness - here full information maximum likelyhood 
          estimator = 'MLR', # lavaan estimator here maximum likelihood estimation with robust SE and scaled test statistic 
          int.ov.free = F, # a number of default lavaan options are set to FALSE
          int.lv.free = F,
          auto.fix.first = F,
          auto.fix.single = F,
          auto.cov.lv.x = F,
          auto.cov.y = F,
          auto.var = F) 
```


## Test manuscript models: 

We provide models and summary level data (variance/covariance matrices) to replicate results from the manuscript.
Models (both RICLPM and wfRICLPM) were based on four traits and three measurement occasions.

You can test the RICLPM models using the followign code: 

### RICLPM

```{r}
library(devtools)
library(lavaan)

source_url("https://github.com/AndreAllegrini/wfRICLPM/tree/master/R/RICLPM_TEDS_NTR.R") #lavaan model

load(url("https://github.com/AndreAllegrini/wfRICLPM/tree/master/data/CorMAT_TEDS.RData")) #load var/covar matrix TEDS 

wf_clpm.cov <- covObs #variance covariance matrix 
wf_clpm.n <- 8549 #TEDS sample size 

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

```



### wfRICLPM

You can test the wfRICLPM function using the following code:

```{r}
library(devtools)
library(lavaan)

load(url("https://github.com/AndreAllegrini/wfRICLPM/tree/master/data/CorMat_zyg_NTR.RData")) #load var/covar matrix by zygosity

wf_clpm.cov <- covObs #variance covariance matrix 
wf_clpm.n <- list(MZ=5900,DZ=10791) #NTR sample size by zigosity

#define variables for wfRICLPM function
var_groups <- list(
  CND=c("CON_t1",  "CON_t2",  "CON_t3"),
  HYP=c("HYP_t1",  "HYP_t2",  "HYP_t3"),
  EMO=c("EMp_t1",  "EMp_t2",  "EMp_t3"),
  PER=c("PEp_t1",  "PEp_t2",  "PEp_t3"))

#define subscrypts for function
sibSub = c("a", "b")

obj <- wfRICLPM(varNames = varNames, sibSub = sibSub, constrained = FALSE)

#print model 
cat(obj$model)

wfRICLPM_test <- lavaan(obj$model, 
               sample.cov = wf_clpm.cov, 
               sample.nobs = wf_clpm.n,
               missing = 'ML',
               estimator = 'MLR',
               int.ov.free = F,
               int.lv.free = F,
               auto.fix.first = F,
               auto.fix.single = F,
               auto.cov.lv.x = F,
               auto.cov.y = F,
               auto.var = F)

summary(wfRICLPM_test, standardized = TRUE)

```



Note this work was built upon the [riclpmr](http://johnflournoy.science/riclpmr/) package.

Please check the following resources: 
 
• [Blog RI-CLPM](https://jflournoy.github.io/2017/10/20/riclpm-lavaan-demo/)

• [Generate syntax for RI-CLPM](https://github.com/jflournoy/riclpmr)

