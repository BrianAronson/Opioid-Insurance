#0) Prepare workspace
    #a) load libraries
        library(fst)
        library(data.table)
        library(scales)
        library(bife)
        library(survival)
        library(biglm)
        library(lme4)
        library(plm)

    #b) find local directories
        hpc.dir <- file.path("hidden","additional_data","BDA")
        pr.dir <- file.path(hpc.dir, "3_PageRank_formatted")
        new.dir <- file.path(hpc.dir, "4_PageRank_formulas"); dir.create(new.dir, showWarnings = F)


#1) read data
    df <- read_fst(file.path(pr.dir, "df.fst"), as.data.table = T, to = 1)


#2) pick variables for use in formulas
    #dvs <- c("dv.chronic", "dv.chronic.lag", "dv.concomitant", "dv.concomitant.lag", "dv.conf.count.log", "dv.conf.count.log.lag", "dv.er.count.log", "dv.er.count.log.lag", "dv.first.ord", "dv.first.ord.lag", "dv.io", "dv.io.lag", "dv.mme.log", "dv.mme.log.lag", "dv.ord", "dv.ord.lag", "dv.rx.count.log", "dv.rx.count.log.lag", "dv.rx.days.count.log", "dv.rx.days.count.log.lag")
    #dvs <- c("dv.first.ord", "dv.first.ord.lag", "dv.io", "dv.io.lag", "dv.mme.log", "dv.mme.log.lag", "dv.ord", "dv.ord.lag", "dv.rx.count.log", "dv.rx.count.log.lag")
    dvs <- c("dv.io", "dv.ord", "dv.mme.log") #"dv.first.ord", , "dv.rx.count.log"
    controls <- "c.age + c.any.rx + c.cancer + c.female + c.hepc + c.hiv + c.mat.user + c.palliative.care + c.psych.disorder + c.year.quarter" #county.id
    network.controls <- "c.n.degree.log + c.n.doc.count.log" #c.n.prov.count.log + c.n.is.isolate + c.n.saw.doc + c.n.comp.large + 
    ivs <- grep("^iv", names(df) , value = T)
    ivs <- grep("log$|p99$", ivs , value = T)
    ivs <- ivs[!grepl("rx.days.count", ivs)]
    
    iv.weights <- gsub("iv.bgrm|iv.birank|iv.cohits|iv.hits|iv.pagerank", "", ivs)
    iv.weights <- gsub("^\\.", "", iv.weights)
    iv.weights[iv.weights != ""] <- paste("dv.",iv.weights[iv.weights != ""], sep = "")
    iv.weights[iv.weights %in% setdiff(iv.weights, names(df))] <- ""


#3) create a data.frame with all combinations of dvs and ivs for use when creating formulas
    #base formula 
        f.index <- data.table(
          DV = rep(dvs, each = length(ivs)),
          IV = rep(ivs, length(dvs))
        )
    #add counterweights
        reps <- nrow(f.index)
        f.index <- f.index[rep(1:reps, 2), ]
        f.index$iv.weight <- c(rep("", reps), rep(iv.weights, length(dvs)))
    #add network controls
        reps <- nrow(f.index)
        f.index <- f.index[rep(1:reps, 2), ]
        f.index$n.controls <- c(rep(0, reps), rep(1, reps))
    #add model types
        reps <- nrow(f.index)
        f.index <- f.index[rep(1:reps, 3), ]
        f.index$form <- rep(c("plm", "glmer", "cox"), each = reps)
    #remove duplicates and order
        f.index <- f.index[!duplicated(f.index),]
        f.index <- f.index[order(DV, form, n.controls, iv.weight, IV), ]
        
        
#4) create function to identify variables in formula
    text.form <- function(formula){
        text <- paste(formula, collapse = "+")
        text <- gsub("\\~|\\+|\\|", ",", text)
        text <- gsub(" ", "", text)
        text <- gsub("^,,", "", text)
        text <- gsub("Surv\\(", "", text)
        text <- gsub("\\)", "", text)
        text <- gsub(",,", ",", text)
        text <- gsub("\\(1\\,","", text)
        unlist(strsplit(text, ","))
    }

    
#5) remove nonsense formulas
    #a) weighted forms for weight dvs
        f.index <- f.index[!(DV == "dv.mme.log" & grepl("mme|rx.count|rx.days.count", IV)), ]
        f.index <- f.index[!(DV == "dv.rx.count.log" & grepl("mme|rx.count|rx.days.count", IV)), ]

    
#6) create formulas
    l.vars <- list()
    l.formulas <- list()
    for(i in 1:nrow(f.index)){
        #intialize formula  
            form <- paste("~", f.index$IV[i], "+", f.index$iv.weight[i], "+", controls)
        #add network controls
            if(f.index$n.controls[i] == 1) form <- paste(form, "+", network.controls)
        #add formula specific syntax
            if(f.index$form[i] == "cox"){
              form <- paste("formula(Surv(time.id,", f.index$DV[i], ")", form)
              form <- gsub("\\+ c.year.quarter","",form)
            } else{
              form <- paste("formula(", f.index$DV[i], form)
            }
            if(f.index$form[i] == "glmer"){
              form <- paste(form, "+ (1|patid))")
            }else{
              form <- paste(form, ")")
            }
        # create formula and append to l.formulas
            assignment.text <- paste("l.formulas[[", i, "]]", "<-", form, sep = "")
            eval(parse(text = assignment.text))
        # identify all unique variables in formula + patid
            l.vars[[i]] <- c("patid", text.form(l.formulas[[i]]))
    }
    
    
#7) put formulas and variables in f.index
    f.index$full.formula <- l.formulas
    l.vars <- lapply(l.vars, function(x) x[x != ""])
    f.index$all.variables <- l.vars
    
    
#8) save data
    saveRDS(f.index, file.path(new.dir, "df.rds"))
    