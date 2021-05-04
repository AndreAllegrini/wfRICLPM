
wfRICLPM <- function(varNames = varNames, sibSub = sibSub, constrained = FALSE)
{
    # measurement model - Random intercepts
    RInt <- list()
    listRInames <- NULL
    for (i in c(sibSub))
    {
        for (j in c(names(varNames)))
        {
            RInt[[paste0(j, i)]] <- paste0("ri_", j, i, " =~ ", paste0("1*", varNames[[j]], i, collapse = " + "))
            listRInames <- rbind(listRInames, paste0("ri_", j, i))
        }
    }
    
    RI_txt <- paste(unlist(RInt), collapse = "\n")
    
    # random intercept variances
    RIntVar <- list()
    for (i in c(sibSub))
    {
        for (j in c(names(varNames)))
        {
            RIntVar[[paste0(j, i)]] <- paste0("ri_", j, i, " ~~ ", "c(v_ri_", j, ",v_ri_", j, ")*", 
                "ri_", j, i)
        }
    } 
    
    RIv_txt <- paste(unlist(RIntVar), collapse = "\n")
   
     # random intercept covariances within
    RIntCVarw <- list()
    for (j in sibSub)
    {
        RIn <- listRInames[endsWith(listRInames, (j))]
        # add prefix: equal for both groups and sibs
        for (i in 1:(length(RIn) - 1))
        {
          
         index <-  paste0("c(cw_", 
                gsub(paste0("ri_|", j), "", RIn[(i)]), gsub(paste0("ri_|", j), "", 
                  RIn[(i + 1):length(RIn)]), ",cw_", gsub(paste0("ri_|", j), 
                  "", RIn[(i)]), gsub(paste0("ri_|", j), "", RIn[(i + 1):length(RIn)]), ")*")
                
            RIntCVarw[[paste0(RIn[[i]])]] <- paste0(RIn[[i]], " ~~ ", paste(index, RIn[(i + 1):length(RIn)], collapse = " + "))
        }
    }
   
     RIcw_txt <- paste(unlist(RIntCVarw), collapse = "\n")
    
    # random intercepts covariances between - matrix of labels is symmetric   
    ln <- names(varNames)
    # upper triangle
    upperT <- list()
    # prefix: different for groups, equal for sibs
    for (i in 1:(length(ln)))
    {
        upperT[[paste0(ln[i], sibSub[1])]] <- paste0("ri_", ln[i], sibSub[1], " ~~ ", paste(paste0("c(cb_", 
            ln[i], ln[(i):length(ln)], "_mz,cb_", ln[i], ln[(i):length(ln)], "_dz)*", "ri_", ln[(i):length(ln)], 
            sibSub[2]), collapse = " + "))
    }
    # lower triangle (for prefix correspondence)
    ln <- names(rev(varNames))
    lowerT <- list()
    # prefix: different for groups, equal for sibs
    for (i in 1:(length(ln) - 1))
    {
        lowerT[[paste0(ln[i], sibSub[1])]] <- paste0("ri_", ln[i], sibSub[1], " ~~ ", paste(paste0("c(cb_", 
            ln[(i + 1):length(ln)], ln[i], "_mz,cb_", ln[(i + 1):length(ln)], ln[i], "_dz)*", "ri_", 
            ln[(i + 1):length(ln)], sibSub[2]), collapse = " + "))
    }
   
     RIcb_txt <- paste(unlist(c(upperT, lowerT)), collapse = "\n")
     
       # fixed intercepts
     listFInt <- list() 
     
     for (i in sibSub){
       for (j in paste0(unlist(varNames))){ 
         listFInt[[paste0(j, i)]] <- paste0(j,i, ' ~ c(', j,'_mu,',j,'_mu)*1') } 
       }
   
     FI_txt <- paste(unlist(listFInt), collapse = "\n")
    
     # latent variables from measurement occasions
    listLat <- list()
    for (i in sibSub)
    {
        for (j in paste0(unlist(varNames)))
        {
            listLat[[paste0("lat_", j, i)]] <- paste0("lat_", j, i, " =~ 1*", j, i)
        }
    }
   
     listLat_txt <- paste(unlist(listLat), collapse = "\n")
    
     #within person regressions - equal for siblings and zygosity
     latNames <- sapply(varNames, function(x)
    {
        paste0("lat_", paste0(unlist(x)))
    })
    df <- as.data.frame(latNames, stringsAsFactors = FALSE)  #data 
    dimVars <- dim(as.data.frame(latNames))[1]  #measurement occasions dim
    dimTraits <- dim(as.data.frame(latNames))[2]  #traits dim
    
    # within person net
    net <- list()
    for (j in sibSub)
    {
        for (i in 1:dimTraits)
        {
            dep_var <- df[-1, i]
            lags <- df[-dimVars, ]
            cong <- matrix(NA, nrow = dimVars - 1, ncol = dimTraits)
            for (i in 1:dimTraits)
            {
                cong[, i] <- paste0(dep_var, lags[, i])
            }
            congM <- as.matrix(cong)
            lagsM <- as.matrix(lags)
            pred <- matrix(paste0("c(", gsub("lat_", "", congM), ",", gsub("lat_", "", congM), ")*", 
                lagsM, j), nrow = nrow(congM), byrow = F)
            pred <- apply(pred, 1, paste, collapse = " + ")
            net <- rbind(net, paste0(paste0(dep_var, j), " ~ ", pred))
        }
    }
    
    listLagsW_txt <- paste(unlist(net), collapse = "\n")
    
    #siblings regressions equal across zigosity
    
    # sib net a
    net <- list()
    for (i in 1:dimTraits)
    {
        dep_var <- df[-1, i]
        lags <- df[-dimVars, ]
        cong <- matrix(NA, nrow = dimVars - 1, ncol = dimTraits)
        for (i in 1:dimTraits)
        {
            cong[, i] <- paste0(dep_var, lags[, i])
        }
        congM <- as.matrix(cong)
        lagsM <- as.matrix(lags)
        pred <- matrix(paste0("c(S_", gsub("lat_", "", congM), ",S_", gsub("lat_", "", congM), ")*", lagsM, 
            sibSub[1]), nrow = nrow(congM), byrow = F)
        pred <- apply(pred, 1, paste, collapse = " + ")
        net <- rbind(net, paste0(paste0(dep_var, sibSub[2]), " ~ ", pred))
    }
    
    listLagsSib_a_txt <- paste(unlist(net), collapse = "\n")
    
    # sib net b
    net <- list()
    for (i in 1:dimTraits)
    {
        dep_var <- df[-1, i]
        lags <- df[-dimVars, ]
        cong <- matrix(NA, nrow = dimVars - 1, ncol = dimTraits)
        for (i in 1:dimTraits)
        {
            cong[, i] <- paste0(dep_var, lags[, i])
        }
        congM <- as.matrix(cong)
        lagsM <- as.matrix(lags)
        pred <- matrix(paste0("c(S_", gsub("lat_", "", congM), ",S_", gsub("lat_", "", congM), ")*", lagsM, 
            sibSub[2]), nrow = nrow(congM), byrow = F)
        pred <- apply(pred, 1, paste, collapse = " + ")
        net <- rbind(net, paste0(paste0(dep_var, sibSub[1]), " ~ ", pred))
    
    }
    
    listLagsSib_b_txt <- paste(unlist(net), collapse = "\n")
    
    # latent variables (residual) variance (equal for siblings and zigosity
    e_v <- as.vector(latNames)
    listE <- list()
    for (i in sibSub)
    {
        for (j in e_v)
        {
            listE[[paste0("e_", j, i)]] <- paste0(j, i, " ~~ c(", sub("lat_", "e_", j), ",", sub("lat_", 
                "e_", j), ")*", j, i)
        }
    }
    
    listLat_e_txt <- paste(unlist(listE), collapse = "\n")
    
    # Contemporaneous correlations within person equal for sibs and zigosity
    ConCov <- list()
    df <- as.data.frame(latNames, stringsAsFactors = FALSE)
    nSets <- dim(df)[1]
    nTraits <- dim(df)[2]
   for (S in sibSub){
        for (i in 1:nSets)
        {
            set <- df[i, ]
            for (j in 1:(nTraits - 1))
            {
                index <- paste0("c(", sub("lat_", "r_", set[(j + 1):nTraits]), sub("lat_", "", set[j]), 
                  ",", sub("lat_", "r_", set[(j + 1):nTraits]), sub("lat_", "", set[j]), ")*")
                ConCov <- rbind(ConCov, paste0(set[j], S, " ~~ ", paste(paste0(index, set[(j + 1):nTraits], 
                  S), collapse = " + ")))
            }
        }
   }
    ConCov_w_txt <- paste(ConCov, collapse = "\n")
    
    # Contemporaneous correlations between siblings differ by zygosity - matrix of labels is symmetric   
    UpperTCov <- list()
    df <- as.data.frame(latNames, stringsAsFactors = FALSE)
    nSets <- dim(df)[1]
    nTraits <- dim(df)[2]
    for (i in 1:nSets)
    {
        set <- df[i, ]
        for (j in 1:(nTraits))
        {
            index <- paste0("c(", sub("lat_", "Sr_", set[j]), sub("lat_", "", set[(j):nTraits]), "_mz,", 
                sub("lat_", "Sr_", set[j]), sub("lat_", "", set[(j):nTraits]), "_dz)*")
            UpperTCov <- rbind(UpperTCov, paste0(set[j], sibSub[1], " ~~ ", paste(paste0(index, set[(j):nTraits], 
                sibSub[2]), collapse = " + ")))
        }
    }
    LowerTCov <- list()
    df <- as.data.frame(latNames, stringsAsFactors = FALSE)
    nSets <- dim(df)[1]
    nTraits <- dim(df)[2]
 
        for (i in 1:nSets)
        {
            set <- rev(df[i, ])
            for (j in 1:(nTraits - 1))
            {
                index <- paste0("c(", sub("lat_", "Sr_", set[(j + 1):nTraits]), sub("lat_", "", set[j]), 
                  "_mz,", sub("lat_", "Sr_", set[(j + 1):nTraits]), sub("lat_", "", set[j]), "_dz)*")
                LowerTCov <- rbind(LowerTCov, paste0(set[j], sibSub[1], " ~~ ", paste(paste0(index, set[(j + 
                  1):nTraits], sibSub[2]), collapse = " + ")))
            }
        }
    
    
    ConCov_s_txt <- paste(unlist(c(UpperTCov, LowerTCov)), collapse = "\n")
    
    
    #final model 
   model <-  paste(RI_txt,  RIv_txt,  RIcw_txt,  RIcb_txt,  FI_txt,  listLat_txt,  listLagsW_txt, 
     listLagsSib_a_txt,  listLagsSib_b_txt,  listLat_e_txt,  ConCov_w_txt,  ConCov_s_txt, 
    sep = "\n\n")
    
   
    return(list(RI_txt = RI_txt, RIv_txt = RIv_txt, RIcw_txt = RIcw_txt, RIcb_txt = RIcb_txt, FI_txt = FI_txt, 
        listLat_txt = listLat_txt, listLagsW_txt = listLagsW_txt, listLagsSib_a_txt = listLagsSib_a_txt, 
        listLagsSib_b_txt = listLagsSib_b_txt, listLat_e_txt = listLat_e_txt, ConCov_w_txt = ConCov_w_txt, 
        ConCov_s_txt = ConCov_s_txt, model = model))
}



#define variables for wfRICLPM function
var_groups <- list(
    CND=c("CON_t1",  "CON_t2",  "CON_t3"),
    HYP=c("HYP_t1",  "HYP_t2",  "HYP_t3"),
    EMO=c("EMp_t1",  "EMp_t2",  "EMp_t3"),
    PER=c("PEp_t1",  "PEp_t2",  "PEp_t3"))

#define subscrypts for function
sibSub = c("a", "b")

obj <- wfRICLPM(varNames = var_groups, sibSub = sibSub, constrained = FALSE)


