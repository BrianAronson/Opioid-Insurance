#1) Find model predictions from ff.df
    end <- 0
    i <- 1
    while(end != nrow(reg.df)){
        chunk <- 1000000
        begin <- chunk*(i-1) + 1
        end <- chunk * i
        end <- ifelse(end > nrow(reg.df), nrow(reg.df), end)
        if(i==1){
            probs <- predict(r.mod1, reg.df[1:chunk,])
            DV <- reg.df$IO[1:chunk]
        }else{
            probs <- c(probs, predict(r.mod1, reg.df[begin:end,]))
            DV <- c(DV, reg.df$IO[begin:end])
        }
        print(round(i*chunk/nrow(reg.df), 2))
        i <- i + 1
    }
             
        
#2) Calculate AUC from ff.df
    #a) find model predictions
        end <- 0
        i <- 1
        while(end != nrow(reg.df)){
            chunk <- 1000000
            begin <- chunk*(i-1) + 1
            end <- chunk * i
            end <- ifelse(end > nrow(reg.df), nrow(reg.df), end)
            if(i==1){
                probs <- predict(r.mod1, reg.df[1:chunk,])
                DV <- reg.df$IO[1:chunk]
            }else{
                probs <- c(probs, predict(r.mod1, reg.df[begin:end,]))
                DV <- c(DV, reg.df$IO[begin:end])
            }
            print(round(i*chunk/nrow(reg.df), 2))
            i <- i + 1
        }
    #b) Load ROCR
        library(ROCR)
    #c) Create ROC curve
        pred <- prediction(probs, DV)
    #d) Calculate area under the curve (AUC)
        performance(pred, "auc")@y.values[[1]]
    #e) Plot ROC curve
        perf <- performance(pred,"tpr","fpr")
        plot(perf)


#3) Identify variables in from formula
    text.form <- paste(tformula, collapse = "+")
    text.form <- gsub("\\~|\\+", ",", text.form)
    text.form <- gsub(" ", "", text.form)
    text.form <- gsub("^,,", "", text.form)
    text.form <- unlist(strsplit(text.form, ","))
        
    
#4) Run loop via ssh input
    args = (commandArgs(T))
    q <- eval(parse(text = args[[1]]))
    for(q in 1:150){
        filename <- paste("mod", q, ".rds", sep = "")
        saveRDS(q, filename)
    }
    
    
#5) compute average marginal effects from ffdf
    #a) find model predictions
        end <- 0
        i <- 1
        while(end != nrow(reg.df)){
          #i) set loop parameters
              chunk <- 1000000
              begin <- chunk*(i-1) + 1
              end <- chunk * i
              end <- ifelse(end > nrow(reg.df), nrow(reg.df), end)
          #ii) pull out chunk from dataframe
              tdf <- reg.df[1:chunk,]
          #iii) calculate prediction
              if(i==1){
                init.probs <- predict(r.mod1, tdf)
              }else{
                init.probs <- c(init.probs, predict(r.mod1, tdf))
              }
          print(round(i*chunk/nrow(reg.df), 2))
          i <- i + 1
        }

    #b) find model predictions if one variable was increased by a smidge
        t.var <- text.form[2]
        m.change <- .0000001

        end <- 0
        i <- 1
        while(end != nrow(reg.df)){
          #i) set loop parameters
              chunk <- 1000000
              begin <- chunk*(i-1) + 1
              end <- chunk * i
              end <- ifelse(end > nrow(reg.df), nrow(reg.df), end)
          #ii) pull out chunk from dataframe
              tdf <- data.table(reg.df[1:chunk, ])
              tdf[, (t.var) := lapply(.SD, function(x) x + m.change), .SDcols = t.var]
              tdf[, (t.var), with = F]

          #iii) calculate prediction
              if(i==1){
                marg.probs <- predict(r.mod1, tdf)
              }else{
                marg.probs <- c(marg.probs, predict(r.mod1, tdf))
              }
          print(round(i*chunk/nrow(reg.df), 2))
          i <- i + 1
        }