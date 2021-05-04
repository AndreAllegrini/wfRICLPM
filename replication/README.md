# Tutorial replication

We provide models and summary level data (variance/covariance matrices) to replicate RICLPM and WFRICLPM results from our [manuscript]().

Models (both RICLPM and wfRICLPM) were based on four traits and three measurement occasions.

You can test the manuscript models using the code below.

### RICLPM

Source the RICLPM model and summary data for TEDS: 

```{r}
library(devtools)

load(url("https://github.com/AndreAllegrini/wfRICLPM/tree/master/data/CovMat_TEDS.RData")) #load var/covar matrix TEDS 

```

This is a correlation matrix of TEDS variables called CovMatTEDS

#### Visualize the data

Correlation plot of the data used in the RICLPM, including four traits (externalizing, attention, internalizing, and social problems) measured at three time points (t1, t2 and t3). A general pattern of positive correlations can be observed, with expected stronger correlations for repeated measurements of the same trait. 


```{r, eval = F echo = F}

library(corrplot)
library(ggplot2)

load('../data/CovMat_TEDS.Rdata')

png('../plots/corMat_TEDS.png', res=350, height = 800, width = 800)

corrplot(cov2cor(CorMatTEDS) ,method = "square", type = 'upper', outline = "black", 
         diag = FALSE, addCoef.col = "black", number.cex = .2, tl.cex = .4, cl.cex = .4,
         tl.col = "black",cl.lim = c(0,1))

dev.off()

```

![](../plots/corMat_TEDS.png?raw=true)

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

```{r eval=F echo = F, fig.height=12, fig.width=12}

library(RColorBrewer)

load('../data/CovMat_zyg_NTR.RData')
      
CorMatNTR$MZ <- cov2cor(CorMatNTR$MZ)
CorMatNTR$DZ <- cov2cor(CorMatNTR$DZ)

CorMatNTR$MZ[lower.tri(CorMatNTR$MZ)] <- CorMatNTR$DZ[lower.tri(CorMatNTR$DZ)] 

par(mar = c(5,1,4,2) + 0.1) 

png('../plots/TwinCorMat_NTR.png', res=400, height = 8000, width = 8000)

corrplot(CorMatNTR$MZ ,method="square", outline = "black", order = "original",
         tl.col = "black", tl.cex = 2, cl.lim = c(0,1), cl.cex = 2,
         col = brewer.pal(n = 8, name = 'BrBG'), 
         diag = F)
mtext(text = "MZ correlations", side = 3, line = 0, adj = 0.5, las = 0, cex = 2 )
mtext(text = "DZ correlations", side = 2, line = 0, adj = 0.5, las = 3, cex = 2 )

dev.off()

```

![](../plots/TwinCorMat_NTR.png?raw=true)

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

See [wfRICLPM_TEDS.R](../R/wfRICLPM_TEDS.R) and [wfRICLPM_NTR.R](../R/wfRICLPM_NTR.R) for the model specification.

Run as: 

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
