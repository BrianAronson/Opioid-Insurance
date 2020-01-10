#I) Prepare workspace for running the remainder of code in parallel
    #0) load libraries
        library(fst)
        library(data.table)
        library(scales)
        library(bife)
        library(survival)
        library(biglm)
        library(lme4)
        library(plm)
        library(lmerTest)
        library(parallel)
        library(doParallel)
        library(doSNOW)
    
    #1) Find local directories
        hpc.dir <- file.path("hidden","additional_data","BDA")
        pr.dir <- file.path(hpc.dir, "3_PageRank_formatted")
        form.dir <- file.path(hpc.dir, "4_PageRank_formulas")
        new.dir <- file.path(hpc.dir, "5_PageRank_models"); dir.create(new.dir, showWarnings = F)
    
    #2) Determine number of iterations
        loop <- nrow(readRDS(file.path(form.dir, "df.rds")))
    
    #3) Prepare standard loop functions
        package.names <- names(sessionInfo()$otherPkgs)
        pb <- txtProgressBar(max = loop, style = 3)
        progress <- function(n) setTxtProgressBar(pb, n); opts <- list(progress = progress)
        cl <- makeCluster(15)
        registerDoSNOW(cl)
    
    #4) Start loop
        ldf <- foreach(
          q = 1:loop,
          .options.snow = opts,
          .packages = package.names
        ) %dopar%
{
#II) Syntax to run in every loop
    #0) Find local directories
        hpc.dir <- file.path("hidden","additional_data","BDA")
        pr.dir <- file.path(hpc.dir, "3_PageRank_formatted")
        form.dir <- file.path(hpc.dir, "4_PageRank_formulas")
        new.dir <- file.path(hpc.dir, "5_PageRank_models"); dir.create(new.dir, showWarnings = F)
    
    #1) Read data
        f.index <- readRDS(file.path(form.dir, "df.rds"))
        df <- read_fst(file.path(pr.dir, "df.fst"), columns = f.index$all.variables[[q]], as.data.table = T)
        
    #2) If running plm, remove ids that only occur in one wave
        if(f.index$form[q] == "plm"){
            patid.by.dv <- df[, lapply(.SD, function(x) length(unique(x))), by = "patid", .SDcols = f.index$DV[q]]
            names(patid.by.dv)[2] <- "V1"
            patid.by.dv <- patid.by.dv[V1 !=1, ]
            df <- df[patid %in% patid.by.dv$patid, ]
        }
        
    #3) Estimate model
        if(f.index$form[q] == "plm"){
          mod <- plm(f.index$full.formula[[q]], data = df, index = c("patid"))  
        }
        if(f.index$form[q] == "glmer") {
          mod <- lmer(f.index$full.formula[[q]], data = df)  
        }
        if(f.index$form[q] == "cox"){
          mod <- coxph(f.index$full.formula[[q]], data = df)  
        }
        
    #4) create function for pulling aics from plm
        AIC_adj <- function(mod){
          n.N   <- nrow(mod$model)
          u.hat <- residuals(mod)
          s.sq  <- log( (sum(u.hat^2)/(n.N)))
          p     <-  length(coef(mod)) + 1
          aic <- 2*p  +  n.N * (  log(2*pi) + s.sq  + 1 ) 
          return(aic)
        }
        
    #5) Put AICs in f.index
        if(f.index$form[q] == "plm"){
            aic <- AIC_adj(mod)
        }else{
            aic <- AIC(mod)
        }
        
    #6) Create functions to format models
        #a) stars function
            prstars<-function(x){
              ifelse(x<.001,"***",ifelse(x<.01,"**",ifelse(x<.05,"*","")))
            }
        #b) prettymod
            prettymod <- function(model, type){
              if(type == "plm" | type == "glmer"){
                #i) pull summary stats
                    model.sum <- summary(model)
                #ii) extract coefficients
                    model.coef <- model.sum$coefficients
                #iii) format
                    model.format <- model.coef
                    varnames <- row.names(model.format)
                    model.format <- data.table(model.format)
                    model.format <- model.format[,.(
                      variable = varnames,
                      coefficient = sprintf("%.4f", (Estimate)),
                      significance = prstars(`Pr(>|t|)`)
                    ) ]
              }
              if(type == "cox"){
                #i) pull summary stats
                    model.sum <- summary(model)
                #ii) extract coefficients
                    model.coef <- model.sum$coefficients
                #iii) format
                    model.format <- model.coef
                    varnames <- row.names(model.format)
                    model.format <- data.table(model.format)
                    model.format <- model.format[,.(
                      variable = varnames,
                      coefficient = sprintf("%.4f", (coef)),
                      significance = prstars(`Pr(>|z|)`)
                    ) ]
              }
              return(model.format)
            }
            
    #7) Format model
        mod <- prettymod(mod, f.index$form[q])

    #8) extract key coefficient
        est <- mod$coefficient[grep("^iv", mod$variable)]
          
    #9) append key results to a list and save
        results <- list(mod, aic, est)
    
    #10) save result
        saveRDS(results, file.path(new.dir, paste("results", q ,".rds", sep= "")))
        gc()
    
    #11) Output empty string for parrallel function to continue
      return(" ")
}
        
#III) Close parallel connections
  stopCluster(cl) 
  closeAllConnections()
  gc()
