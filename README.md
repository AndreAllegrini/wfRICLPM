
# wfRICLPM

wfRICLPM is a function to create a RI-CLPM lavaan model including sibling regressions overtime, while taking into account the fact that family members resemble eachother. See below for the model specification. 

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

We provide models and summary level data (correlation matrices) to replicate results from the manuscript.
Models (both RICLPM and wfRICLPM) were based on four traits and three measurement occasions.

You can test the manuscript models using the code below.

### RICLPM

Source the RICLPM model and summary data for TEDS: 

```{r}
library(devtools)

load(url("https://github.com/AndreAllegrini/wfRICLPM/tree/master/data/CovMat_TEDS.RData")) #load var/covar matrix TEDS 

```

This is a correlation matrix of TEDS variables called CorMatTEDS

#### Visualize the data

This is a correlation plot of the data used in the RICLPM. Four traits (externalizing, attention, internalizing, and social problems) measured at three time points (t1, t2 and t3). A general pattern of positive correlations can be observed, with expected stronger correlations for repeated measurements of the same trait. 


```{r, eval = F, echo = F}

library(corrplot)
library(ggplot2)

load('data/CovMat_TEDS.Rdata')

png('plots/corMat_TEDS.png', res=350, height = 2000, width = 2000)

corrplot(cov2cor(CorMatTEDS) ,method = "square", type = 'upper', diag = FALSE, 
         addCoef.col = "black", number.cex = .7, 
         tl.col = "black",cl.lim = c(0,1))

dev.off()

```

![](plots/corMat_TEDS.png?raw=true)

Fit the RICLPM model:

```{r}

library(lavaan)

source_url("https://github.com/AndreAllegrini/wfRICLPM/tree/master/R/RICLPM_TEDS_NTR.R") #source lavaan model called: RICLPM

riclpm.n <- 8549 # specify TEDS sample size 

RICLPM_fit <- lavaan(RICLPM, 
               sample.cov = CorMatTEDS, 
               sample.nobs = riclpm.n,
               int.ov.free = F,
               int.lv.free = F,
               auto.fix.first = F,
               auto.fix.single = F,
               auto.cov.lv.x = F,
               auto.cov.y = F,
               auto.var = F)

summary(RICLPM_fit, standardized = TRUE)

```


### wfRICLPM

You can test the wfRICLPM function using the following code:

```{r}

library(devtools)
library(lavaan)

load(url("https://github.com/AndreAllegrini/wfRICLPM/tree/master/data/CorMat_zyg_NTR.RData")) #load var/covar matrix by zygosity

```


This is a list object containing covariance matrices of variables employed for MZ and DZ twins. 

#### Visualize the data

Plot of correlations by zigosity (MZ vs DZ twins), for four traits (EXT, ATT, INT, SOC) measured at three time points (t1, t2 and t3), for both siblings (sibling "a" and "b"), as for the RICLPM model in singletons.

Upper triangle shows correlations for MZ twins, lower triangle shows correlations in DZs. 

Top left and bottom right squares are phenotypic correlations for twin 'a' and twin 'b' respectively, and are approximate symmetic matrices. While top right and bottom left squares are cross twin correlations, with expected stronger correlations (darker squares) for MZs. 


```{r eval=F, echo = F, fig.height=12, fig.width=12}

library(RColorBrewer)

load('data/CovMat_zyg_NTR.RData')
      
CorMatNTR$MZ <- cov2cor(CorMatNTR$MZ)
CorMatNTR$DZ <- cov2cor(CorMatNTR$DZ)

CorMatNTR$MZ[lower.tri(CorMatNTR$MZ)] <- CorMatNTR$DZ[lower.tri(CorMatNTR$DZ)] 

par(mar = c(5,1,4,2) + 0.1) 

png('plots/TwinCorMat_NTR.png', res=400, height = 8000, width = 8000)

corrplot(CorMatNTR$MZ ,method="square", outline = "black", order = "original",
         tl.col = "black", tl.cex = .8, cl.lim = c(0,1),
         col = brewer.pal(n = 8, name = 'BrBG'), 
         diag = F)
mtext(text = "MZ correlations", side = 3, line = 0, adj = 0.5, las = 0, cex = 1.5 )
mtext(text = "DZ correlations", side = 2, line = 0, adj = 0.5, las = 3, cex = 1.5 )

dev.off()

```

![](plots/TwinCorMat_NTR.png?raw=true)


The following code can be used to generate the wfRICLPM model based on these data

```{r eval=F}

source('R/wfRICLPM.func.R')

#define variables for wfRICLPM function
varNames <- list(
  EXT=c("EXT_t1",  "EXT_t2",  "EXT_t3"),
  ATT=c("ATT_t1",  "ATT_t2",  "ATT_t3"),
  INT=c("INT_t1",  "INT_t2",  "INT_t3"),
  SOC=c("SOC_t1",  "SOC_t2",  "SOC_t3"))

#define subscrypts for function
sibSub = c("a", "b")

obj <- wfRICLPM(varNames = varNames, sibSub = sibSub, constrained = FALSE)

#print model 
cat(obj$model)

```


```{r}

wf_clpm.n <- list(MZ=5900, DZ=10791) #NTR sample size by zigosity

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

Note the wfRICLPM function has been written on insights from the [riclpmr](http://johnflournoy.science/riclpmr/) package.

Please check this [link](https://github.com/jflournoy/riclpmr) for generating syntax for the RI-CLPM, and this [blog](https://jflournoy.github.io/2017/10/20/riclpm-lavaan-demo/) for an explainer on the RICLPM. 
