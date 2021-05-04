
# wfRICLPM

wfRICLPM is a function to create a RI-CLPM lavaan model including sibling regressions overtime, while taking into account the fact that family members resemble eachother.  

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

and outputs a list containing the wfRICLPM building blocks, and the full model to be fed to lavaan. 

Current version assumes monozygotic and dyzogotyc twin pairs are supplied, and that data are in wide format.

### Run as: 

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

## Tutorial on manuscript models: 

We provide a [tutorial](replication/Untitled.Rmd) to replicate results from our [manuscript]() including both RICLPM and WFRICLPM models.


Note the wfRICLPM function has been written on insights from the [riclpmr](http://johnflournoy.science/riclpmr/) package.

Please check this [link](https://github.com/jflournoy/riclpmr) for generating syntax for the RI-CLPM, and this [blog](https://jflournoy.github.io/2017/10/20/riclpm-lavaan-demo/) for an explainer on the RICLPM. 

Things to update: 

* constrained version overtime 
* only (DZ) siblings (no twins)
* add Falconer's formula
