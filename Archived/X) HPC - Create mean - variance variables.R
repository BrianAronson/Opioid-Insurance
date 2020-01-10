# #Outline:
#

#0) Prepare workspace
    #a) load libraries
        library(fst)
        library(data.table)
        library(biglm)
        library(ffbase)
        library(ff)
        
    #b) find local directories
        hpc.dir <- file.path("hidden")
        log.dir <- file.path(hpc.dir, "7_PageRank_sorted_logvars")
        perc.dir <- file.path(hpc.dir, "7_PageRank_sorted_percvars")
        sd.dir <- file.path(hpc.dir, "7_PageRank_sorted_sd")
        new.log.dir <- file.path(hpc.dir, "8_PageRank_sorted_logvars"); dir.create(new.log.dir, showWarnings = F)
        new.perc.dir <- file.path(hpc.dir, "8_PageRank_sorted_percvars"); dir.create(new.perc.dir, showWarnings = F)
        new.sd.dir <- file.path(hpc.dir, "8_PageRank_sorted_sd"); dir.create(new.sd.dir, showWarnings = F)

        
#1) Determine criteria for splitting data by contiguous PATIDs into 1m row chunks
    #a) identify files to load
        reg.files <- list.files(log.dir)

        
#2) for each type of dataset, for each regfile...
    old.dirs <- c(log.dir, perc.dir, sd.dir)
    new.dirs <- c(new.log.dir, new.perc.dir, new.sd.dir)
    for(k in 1:length(old.dirs)){
        old.dir <- old.dirs[k]
        new.dir <- new.dirs[k]
        for(i in 1:length(reg.files)){
            #a) load data  
                df <- read_fst(file.path(old.dir, reg.files[i]), as.data.table = T)
            #b) identify all variable names
                all.vars <- names(df)
            #c) identify variables to manipulate
                oldvars <- all.vars[!all.vars %in% c("PATID","county_id","component")]
            #d) convert integers to numeric
                current.class <- unlist(df[, lapply(.SD, class), .SDcols = oldvars])
                int.vars <- names(current.class[current.class != "numeric"])
                df[, (int.vars) := lapply(.SD, as.numeric), .SDcols = int.vars]            
            #e) find mean differences
                df[, (oldvars) := lapply(.SD, function(x) x - mean(x, na.rm = T)), .SDcols = oldvars, by="PATID"]            
            #f) save
                write_fst(df, file.path(new.dir, reg.files[i]))
                print(paste(k, i))
        }
    }

    
#3) remove obsolete files and clean ram

    rm(list = setdiff(ls(), "zcode.dir"))
    gc()

