
# Replication of analyses

[RICLPM](#RICLPM)

[wfRICLPM](#wfRICLPM)

***

We provide models and summary level data (variance/covariance matrices) to replicate RICLPM and wfRICLPM results from our [manuscript](https://psyarxiv.com/t486z/).

Models (both RICLPM and wfRICLPM) are based on four traits and three measurement occasions.

You can test the manuscript models using the code below.

## RICLPM

After downloading the repository, load the summary level data for TEDS: 

```{r}

load("../data/CovMat_TEDS.RData")

```

This is a variance/covariance matrix of TEDS variables called `covTEDS`.

### Plot the data:

Correlation plot of the data used in the RICLPM, including four traits (externalizing, attention, internalizing, and social problems) measured at three time points (t1, t2 and t3). A general pattern of positive correlations can be observed, with expected stronger correlations for repeated measurements of the same trait. 

```{r, eval = F echo = F}

library(corrplot)

load('../data/CovMat_TEDS.Rdata')

png('../plots/corMat_TEDS.png', res=350, height = 800, width = 800)

corrplot(cov2cor(covTEDS), #convert covariance matrix to correlation matrix
         method = "square", type = 'upper', outline = "black", 
         diag = FALSE, addCoef.col = "black", number.cex = .2, tl.cex = .4, cl.cex = .4,
         tl.col = "black",cl.lim = c(0,1))

dev.off()

```

![](../plots/corMat_TEDS.png?raw=true)

### Fit the model:

We now source the RICLPM model and feed it to lavaan after specifying the sample size used in analyses.  

```{r}

library(lavaan)

source("../R/RICLPM_TEDS_NTR.R") #source RICLPM unconstrained lavaan model called: RICLPM_unconst

sample.n <- 8549 # specify TEDS sample size 

RICLPM_fit <- lavaan(RICLPM_unconst, 
               sample.cov = covTEDS, 
               sample.nobs = sample.n,
               int.ov.free = F,
               int.lv.free = F,
               auto.fix.first = F,
               auto.fix.single = F,
               auto.cov.lv.x = F,
               auto.cov.y = F,
               auto.var = F)

summary(RICLPM_fit, standardized = TRUE)

```

## wfRICLPM

You can test the wfRICLPM function using the following code.

Load the NTR summary level data:

```{r}

load("../data/CovMat_zyg_NTR.RData")

```

This is a list containing variance/covariance matrices for monozygotic and dizygotic twins. 

### Plot the data:

Plot of correlations by zigosity (MZ vs DZ twins), for four traits (EXT, ATT, INT, SOC) measured at three time points (t1, t2 and t3), for both siblings (sibling "a" and "b"), as we did for the RICLPM model in unrelated individuals.

Upper triangle shows correlations for MZ twins, lower triangle shows correlations in DZs. 

Top left and bottom right squares are phenotypic correlations for twin "a" and twin "b" respectively, and are approximate symmetic matrices. Top right and bottom left squares are cross twin correlations, with expected stronger correlations (darker squares) for MZs. 

```{r eval=F echo = F, fig.height=12, fig.width=12}

library(RColorBrewer)
      
covMat$MZ <- cov2cor(covMat$MZ) #convert covariance matrix to correlation matrix
covMat$DZ <- cov2cor(covMat$DZ)

covMat$MZ[lower.tri(covMat$MZ)] <- covMat$DZ[lower.tri(covMat$DZ)] 

par(mar = c(5,1,4,2) + 0.1) 

png('../plots/TwinCorMat_NTR.png', res=400, height = 8000, width = 8000)

corrplot(covMat$MZ, method="square", outline = "black", order = "original",
         tl.col = "black", tl.cex = 2, cl.lim = c(0,1), cl.cex = 2,
         col = brewer.pal(n = 8, name = 'BrBG'), 
         diag = F)
mtext(text = "MZ correlations", side = 3, line = 0, adj = 0.5, las = 0, cex = 2 )
mtext(text = "DZ correlations", side = 2, line = 0, adj = 0.5, las = 3, cex = 2 )

dev.off()

```

![](../plots/TwinCorMat_NTR.png?raw=true)

### Run as: 

Generate the wfRICLPM model based on these data.

```{r eval=F}

#source the wfRICLPM function
source('../R/wfRICLPM.R')

#define variables to be fed to function
varNames <- list(
  EXT=c("EXT_t1", "EXT_t2", "EXT_t3"),
  ATT=c("ATT_t1", "ATT_t2", "ATT_t3"),
  INT=c("INT_t1", "INT_t2", "INT_t3"),
  SOC=c("SOC_t1", "SOC_t2", "SOC_t3"))

#define subscrypts 
sibSub = c("a", "b")

obj <- wfRICLPM(varNames = varNames, 
                sibSub = sibSub, 
                constrained = FALSE)

#print model 
cat(obj$model)

```

Fit the unconstrained model after specifying the sample size by zigosity. 

```{r}
groups.n <- list(DZ=10791,MZ=5900) #NTR sample size by zigosity

wfRICLPM_test <- lavaan(obj$model, 
               sample.cov = covMat, 
               sample.nobs = groups.n,
               int.ov.free = F,
               int.lv.free = F,
               auto.fix.first = F,
               auto.fix.single = F,
               auto.cov.lv.x = F,
               auto.cov.y = F,
               auto.var = F)

summary(wfRICLPM_test, standardized = TRUE)

```


See [wfRICLPM_TEDS.R](../R/wfRICLPM_TEDS.R) and [wfRICLPM_NTR.R](../R/wfRICLPM_NTR.R) for the model specification used in the manuscript.

