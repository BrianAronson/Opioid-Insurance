#0) Prepare workspace
    #a) load libraries
        library(docshop)
        library(biglm)
        library(ffbase)
        library(ff)

    #b) find local directories
        main.dir <- file.path("hidden")
        object.dir <- file.path("additional_data", "BDA", "PageRank")
        my.dir <- file.path(main.dir, "PageRank")
                

#1) create functions to format results
    #a) stars function
        prstars<-function(x){
          ifelse(x<.001,"***",ifelse(x<.01,"**",ifelse(x<.05,"*","")))
        }

    #b) Pretty model function
        pretty.mod.fun<-function(model){
        #i) pull summary stats
            model.sum <- summary(model)
        #ii) extract coefficients
            model.coef <- model.sum$coefficients
        #iii) format
            model.format <- model.coef
            varnames <- row.names(model.format)
            model.format <- data.table(model.format)
            model.format <- model.format[,.(
              var = varnames,
              coef = sapply(Estimate,function(x) sprintf("%.2f", x)),
              se = sapply(`Std. Error`,function(x) paste("(",sprintf("%.2f", x),")",sep="")),
              pval = prstars(`Pr(>|t|)`)
            ) ]
            return(model.format)
        }

        pretty.mod.fun.biglm<-function(model){
        #i) pull summary stats
            model.sum <- summary(model)
        #ii) extract coefficients
            model.coef <- model.sum$mat
        #iii) format
            model.format <- model.coef
            varnames <- row.names(model.format)
            model.format <- data.table(model.format)
            model.format <- model.format[,.(
              var = varnames,
              coef = sapply(Coef, function(x) sprintf("%.2f", x)),
              se = sapply(SE,function(x) paste("(",sprintf("%.2f", x),")",sep="")),
              pval = prstars(p)
            ) ]
            return(model.format)
        }


    #c) pull all useful info from model and print pretty results
        mod.sumcoefpretty.fun <- function(model, print.results = T){
          #i) pull summary stats
              model.sum <- summary(model)
          #ii) extract coefficients
              model.coef <- model.sum$coefficients
          #iii) format
              model.format <- model.coef
              varnames <- row.names(model.format)
              model.format <- data.table(model.format)
              model.format <- model.format[,.(
                var = varnames,
                coef = sapply(Estimate,function(x) sprintf("%.2f", x)),
                se = sapply(`Std. Error`,function(x) paste("(",sprintf("%.2f", x),")",sep="")),
                pval = prstars(`Pr(>|t|)`)
              ) ]
          #iv) put results in list
              l.model <- list(model.sum, model.coef, model.format)
          #v) print pretty results
              if(print.results == T){
                  print(model.format)
              }
          #vi) return list
              return(l.model)
        }


#2) create formulas
    #a) base formula
        formula.mod0.base <- formula(IO ~ hepc + hiv + chronic + concomitant + psych_disorder +
                                      any_rx + conf_count + er_count + MAT_user +  #removed low.inc due to missingness
                                      palliative_care + cancer + #female +
                                      comp.1 + comp.1to10 + comp.10to20 + comp.20to50 + comp.50to100 + comp.100to1000 + comp.1000P+
                                      age + TOTAL_MME + t1birank.MME.BiRank
                             )

    #b) add t1, t2, and t3 pageranks
        #i) identify rank variables to add
            rankvars <- c("pagerank.unweighted.none", "birank.unweighted.CoHITS", "birank.unweighted.BiRank",
                          "birank.MME.CoHITS", "birank.MME.BiRank")
        #ii) make list of formulas with each rank variable at t1, t2, and t3
            l.base.formulas <- list()
            for(i in 1:length(rankvars)){
              for(j in 1:3){
                  var.name <- paste("t", j, rankvars[i], sep = "")
                  list.name <- paste("l.base.formulas[[", ((i - 1) * 3 + j), "]]", sep = "")
                  base.formula.name <- "formula.mod0.base"
                  assignment.text <- paste(list.name," <- update(", base.formula.name, ",~. + ", var.name, ")", sep="")
                  eval(parse(text = assignment.text))
              }
            }

    #c) make list of formulas that adds equally lagged MME to base formulas
        l.mme.formulas <- list()
        for(i in 1:length(rankvars)){
          for(j in 1:3){
            var.name <- "TOTAL_MME"
            list.name <- paste("l.mme.formulas[[", ((i - 1) * 3 + j), "]]", sep = "")
            base.formula.name <- paste("l.base.formulas[[", ((i - 1) * 3 + j), "]]", sep = "")
            assignment.text <- paste(list.name," <- update(", base.formula.name, ",~. + ", var.name, ")", sep="")
            eval(parse(text = assignment.text))
          }
        }
        

#3) read data
    # reg.files <- list.files(my.dir, full.names = T)
    reg.files <- s3.list.files(my.dir, full.names = T)
    reg.files <- reg.files[grepl("reg", reg.files)]
    reg.files <- reg.files[!grepl(".df", reg.files)]
    for(i in 1:length(reg.files)){
        df <- as.ffdf(s3read_any(reg.files))
        if(i == 1){
            reg.df <- df
        }else{
            reg.df <- ffdfappend(reg.df, df, adjustvmode=F)
        }
    }


#4) run regression
    system.time(mod1 <- bigglm.ffdf(formula.mod0.base,
                                family = binomial(), #(link = "cloglog"),
                                maxit = 25, chunksize=1000000,
                                data = df)
    )

    
#5) save results
    s3write_using(mod1, FUN = saveRDS, object = file.path(object.dir, "mod1.rds"), bucket = "hidden")
