
# wfRICLPM

wfRICLPM is a function to create a RI-CLPM (**Hamaker et al., 2015**) `lavaan` model including sibling regressions overtime, while taking into account the fact that family members resemble eachother.  


Source as: 

```{r}

source("https://raw.githubusercontent.com/AndreAllegrini/wfRICLPM/master/R/wfRICLPM.R")

```

wfRICLPM takes in a list of traits and measurement occasions, and a vector of siblings subscripts (currently limited to 2, e.g. sibSub = c("i", "j")) 

```{r}

varNames = list(Trait_1 = c("t1_1", "t1_2", "t1_3"),
                Trait_2 = c("t2_1", "t2_2", "t2_3"),
                Trait_3 = c("t3_1", "t3_2", "t3_3"))

sibSub = c("i", "j")
```

and outputs a list containing the wfRICLPM building blocks, and the full model to be fed to lavaan. 

```{r}
obj <- wfRICLPM(varNames = varNames, sibSub = sibSub, constrained = FALSE)

#print model 
cat(obj$model)

```

Current version assumes monozygotic and dizygotic twin pairs are supplied, and that data are in wide format.

Note that you may want to constrain regressions (both within-person and for siblings), as well as residual variances and covarainces to be the same across intervals. You can do that by setting `constrained = TRUE` in the wfRICLPM function.
You can then compare the model fit against the unconstrained model.

### Run model as: 

The model is then fed to `lavaan` specifying the zygosity grouping variable. 

```{r}

#load lavaan
library(lavaan)

fit <- lavaan(obj$model, #wfRICLPM model
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

## Tutorial on manuscript models: 

We provide a [tutorial](replication/README.md) to replicate results from our [manuscript](https://acamh.onlinelibrary.wiley.com/doi/full/10.1002/jcv2.12100) including both RICLPM and wfRICLPM models.

### Acknowledgments

The wfRICLPM function has been written on insights from the [riclpmr](http://johnflournoy.science/riclpmr/) package.

Please do check this [link](https://github.com/jflournoy/riclpmr) for generating syntax for the RI-CLPM, and this [blog](https://jflournoy.github.io/2017/10/20/riclpm-lavaan-demo/) for an excellent explainer on the RICLPM. 

### References 

**Hamaker, E. L., Kuiper, R. M., & Grasman, R. P. (2015).** A critique of the cross-lagged panel model. Psychological methods, 20(1), 102. 
