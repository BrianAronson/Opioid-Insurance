#0) Prepare workspace
    #a) load libraries
        library(fst)
        library(data.table)
        library(scales)

    #b) find local directories
        hpc.dir <- file.path("hidden","additional_data","BDA")
        pr.dir <- file.path(hpc.dir, "PageRank")
        new.dir <- file.path(hpc.dir, "3_PageRank_formatted"); dir.create(new.dir, showWarnings = F)


#1) load data
    reg.files <- list.files(pr.dir, full.names = T)
    for(i in 1:length(reg.files)){
        if(i ==1) df <- read_fst(reg.files[i], as.data.table = T)
        if(i > 1) df <- rbind(df, read_fst(reg.files[i], as.data.table = T))
    }
    
#2) rename variables
    #a) drop redundant variables
        df[, c("TOTAL_MME", "first.io") := NULL]
        
    #b) rename rank variables
        names(df) <- gsub("birank.","",names(df))
        rankvars <- c("HITS", "CoHITS", "BGRM", "BiRank")
        for(i in 1:length(rankvars)){
            tname <- grep(paste("\\.", rankvars[i], sep = ""), names(df), value = T) 
            tname2 <- gsub(paste(".", rankvars[i], sep = ""), "", tname)
            tname2 <- paste("iv", rankvars[i], tname2, sep = ".")
            names(df)[names(df) %in% tname] <- tname2 
        }
        tname <- grep("pagerank", names(df), value = T) 
        tname2 <- paste("iv", tname, sep = ".")
        names(df)[names(df) %in% tname] <- tname2 
        
        
    #c) rename other variables
        names(df) <- gsub("unweighted.|.unweighted|.baseline|baseline.|none.|.none","",names(df))

    #d) rename dvs    
        setnames(df,
          c("chronic", # use over 100 mme 90 days
            "conf_count", # number of times hospitalized
            "er_count", # number of times had an "emergency" admission
            "concomitant", # patients filled more than 30 days of drugs
            "IO", "ORD", "MME", "first.ord", "rx_count", "rx_days_count"
          ),
          paste("dv.", 
            c("chronic", # use over 100 mme 90 days
            "conf_count", # number of times hospitalized
            "er_count", # number of times had an "emergency" admission
            "concomitant", # patients filled more than 30 days of drugs
            "io", "ord", "mme", "first.ord", "rx_count", "rx_days_count"
            )
          ,sep = "")
        ) 

    #d) rename network variables
        setnames(df,
                 c("comp.size", "component", "degree", "doc_count", "prov_count"),
                 paste("n.", c("comp.size", "component", "degree", "doc_count", "prov_count"), sep = "")
        )
        
    #e) rename control variables
        names(df)[!grepl("^dv.|^iv.|PATID", names(df))] <- paste("c.",names(df)[!grepl("^dv.|^iv.|PATID", names(df))], sep = "")
        
    #f) set all names to lower
        names(df) <- tolower(names(df))
    
    #g) conver all underscores to dots
        names(df) <- gsub("\\_", ".", names(df))
        

#3) create new variables
    #a) create new variables
        df[, ':='(
               c.n.saw.doc = as.numeric(c.n.comp.size > 0),
               c.n.is.isolate = as.numeric(c.n.comp.size == 1),
               c.n.comp.large = as.numeric(c.n.comp.size >= 50),
               c.age = ifelse(c.age < 0, 0, 
                     ifelse(c.age > 100, 100, c.age))
        )]


    #b) create logged and 99th percentile versions of some skewed variables
        #i) identify variables to transform
            distr <- as.data.frame(t(df[, lapply(.SD, summary)]))
            names(distr) <- c(names(summary(unlist(df[, c(1), with =F]))), "NA")
            distr$var <- row.names(distr)
            probs <- (distr$`3rd Qu.` * 5 < distr$Max.) & distr$Max. != 1
            skewed.vars <- distr$var[probs]
            skewed.vars <- skewed.vars[skewed.vars != "c.n.component"]
            skewed.vars <- unique(c(skewed.vars, grep("bgrm", distr$var, value = T)))
        #ii) create logged versions of those variables
            log.skewed.vars <- paste(skewed.vars, ".log", sep = "")
            df[, (log.skewed.vars) := lapply(.SD, function(x) log(x+1)), .SDcols = skewed.vars]
        #iii) create 99th percentile version of some skewed variables
            p99.skewed.vars <- paste(skewed.vars, ".p99", sep = "")
            df[, (p99.skewed.vars) := lapply(.SD, function(x) ifelse(x >= quantile(x, .99, na.rm = T), 1, 0)), .SDcols = skewed.vars]
            
    #c) fill in empty components
        comp.max <- max(df$c.n.component, na.rm = T)
        df[is.na(c.n.component), c.n.component := (comp.max + 1:.N) ]
    
    #d) create time_id
        df[, time.id := 1 + (c.year.quarter - min(c.year.quarter)) * 4]
        
        
#4) standardize vars
    #a) identify continous variables to standardize
        cont.vars <- df[, lapply(.SD, function(x) length(unique(x)))]
        cont.vars <- names(cont.vars)[cont.vars > 2]
        cont.vars <- setdiff(cont.vars, c("patid","c.county.id","time.id"))
        
    #b) treat pagerank variables differently
        rank.vars <- grep("iv.", cont.vars, value = T)
        
    #c) standardize
        for(j in 1:length(cont.vars)){
            if(cont.vars[j] %in% rank.vars){
                df[c.n.saw.doc == 1, (cont.vars[j]) := lapply(.SD, scale), .SDcols = cont.vars[j]]
                df[c.n.saw.doc == 0, (cont.vars[j]) := 0]
            }else{
                df[, (cont.vars[j]) := lapply(.SD, scale), .SDcols = cont.vars[j]]
            }
          print(j)
        }

        
#5) create lagged dv for each DV
    #a) sort variables by patid and time.id
        setorderv(df, c("patid", "time.id"))  
    #b) identify variables to lag
        dv.names <- grep("dv", names(df), value = T)
        lag.dv.names <- paste(dv.names, ".lag", sep = "")
    #c) lag DVs
        df[, (lag.dv.names) := lapply(.SD, function(x) c(NA, x[-.N])), by = patid, .SDcols = dv.names]
        

#6) order variable names
    df <- df[, order(names(df)), with = F]
    front.vars <- c("patid", "time.id", "c.county.id","c.n.component")
    df <- df[, c(front.vars, setdiff(names(df), front.vars)), with = F]
    setnames(df,
             c("c.county.id","c.n.component"),
             c("county.id","component.id")
    )  
      
#7) change ids to factor
    df[, ':='(
      county.id = factor(county.id),
      patid = factor(patid)
    )]
    
#8) save data
    write_fst(df, file.path(new.dir, "df.fst"))
     
    
