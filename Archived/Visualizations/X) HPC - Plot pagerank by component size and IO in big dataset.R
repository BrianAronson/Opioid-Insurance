#Outline:


#0) Prepare workspace
    #a) load libraries
        library(data.table)
        library(fst)
        library(ggplot2)
        library(viridis)
        library(scales)
        library(cowplot)
        
    #b) find local directories
        hpc.dir <- file.path("hidden")
        pr.dir <- file.path(hpc.dir, "PageRank")
        new.dir <- file.path(hpc.dir, "PageRank_sorted"); dir.create(new.dir, showWarnings = F)
        fig.dir <- file.path("hidden", Sys.Date()); dir.create(fig.dir, showWarnings = F)

        
#1) Load data
  # df <- read_fst(file.path(pr.dir, "reg2018.1.fst"), as.data.table = T, columns = c("comp.size", "doc_count", "t1birank.MME.BiRank","IO", "comp.1", "comp.1to10", "comp.10to20", "comp.20to50", "comp.50to100", "comp.100to1000", "comp.1000P"))
  #a) identify files to load
      reg.files <- list.files(pr.dir, full.names = T)
  #b) start loop through variable list
      for(i in 1:length(reg.files)){
  #c) load each file
          if(i == 1){
              df <- read_fst(reg.files[i], as.data.table = T, columns = c("comp.size", "doc_count", "t1birank.MME.BiRank","IO", "comp.1", "comp.1to10", "comp.10to20", "comp.20to50", "comp.50to100", "comp.100to1000", "comp.1000P"))
          }else{
              df <- rbind(df, read_fst(reg.files[i], as.data.table = T, columns = c("comp.size", "doc_count", "t1birank.MME.BiRank","IO", "comp.1", "comp.1to10", "comp.10to20", "comp.20to50", "comp.50to100", "comp.100to1000", "comp.1000P")))
          }
          print(i)
          print(object.size(df)/1000000)
      }

      
#2) Prepare data
  #a) createfunction to trim outliers
      outlierfun <- function(x, min = 0, max = .99){
          x[x < quantile(x, min, na.rm = T)] <- quantile(x, min, na.rm = T)
          x[x > quantile(x, max, na.rm = T)] <- quantile(x, max, na.rm = T)
          return(x)
      }
      outlierfun2 <- function(x, min = .05, max= .95){
          x[x < quantile(x, min, na.rm = T)/2] <- quantile(x, min, na.rm = T)
          x[x > quantile(x, max, na.rm = T)*2] <- quantile(x, max, na.rm = T)
          return(x)
      }
      outlierfun3 <- function(x){
          x[x > 4] <- 4
          return(x)
      }


  #b) remove sample who saw no doctors and remove extreme outliers for kaicheng variables
      df <- df[doc_count > 0, ]
      df[comp.size > 1000, comp.size := 1000]

  #c) Determine variables to transform/plot
      cols <- names(df)[grepl("t1|pagerank", names(df))]
      cols <- cols[!grepl("t2|t3|BGER", cols)]

  #d) cap outliers
      df[, (cols) := lapply(.SD, function(x) outlierfun3(x)), .SDcols = cols]

  #e) round to .1
      df[, (cols) := lapply(.SD, function(x) floor(x*8)/8), .SDcols = cols]
      # df[, lapply(.SD, summary), .SDcols = cols]


#3) Run pagerank/birank with other variables
  #a) aggregate frequencies to get data.table with all possible values
      #i) add 0 for all values to make table produce columns for 0s
          a <- df[1, ]
          a <- a[, lapply(.SD, function(x) return(0))]
          df <- rbind(df, a)
      #ii) aggregate
          df[, comp.size2 := (round(as.numeric(comp.size)/10)*10)]
          df[comp.size2 > 100, comp.size2 := 100]
          tdf2 <- data.table(df[, table(t1birank.MME.BiRank, comp.size2)])

          df[, comp.v :=
            ifelse(comp.1==1, "1",
            ifelse(comp.1to10==1, "2-10",
            ifelse(comp.10to20==1, "11-20",
            ifelse(comp.20to50==1, "21-50",
            ifelse(comp.50to100==1, "51-100",
            ifelse(comp.100to1000==1, "101-1000",
            ifelse(comp.1000P==1, "1000P","1")))))))
          ]
          tdf2 <- data.table(df[, table(t1birank.MME.BiRank, comp.v)])
            
        
  #b) make function
      plotfun<-function(fill.x,fill.x.name,title){
          #i) aggregate data
              tdf <- df[, .(N = mean(eval(parse(text = fill.x)))), by = c("t1birank.MME.BiRank", "comp.v")]
              tdf3 <- tdf2[! paste(tdf2$t1birank.MME.BiRank, tdf2$comp.v) %in% paste(tdf$t1birank.MME.BiRank, tdf$comp.v), ]
              tdf <- rbind(tdf, tdf3)
              # tdf[, comp.size2 := as.numeric(comp.size2)]
              tdf[, t1birank.MME.BiRank := as.numeric(t1birank.MME.BiRank)]
              tdf$comp.v <- factor(tdf$comp.v, levels = c("1","2-10", "11-20","21-50","51-100","101-1000","1000P"))
              
              # quantile(tdf$N, .01)
              # tdf$N[tdf$N < quantile(tdf$N, .2) / 2] <- quantile(tdf$N, .2) / 2
              tdf$N[tdf$N > quantile(tdf$N, .9) * 2] <- quantile(tdf$N, .9) * 2
              tdf <- tdf[t1birank.MME.BiRank!=0, ]
              
          #ii) plot
              p <- ggplot(tdf, aes(y = comp.v, x = t1birank.MME.BiRank)) +
                  geom_raster(aes(fill = N)) +
                  scale_fill_gradientn(colors = viridis(10)) +
                  labs(x = "BiRank.MME (mean-centered)", y = "Component Size", fill = "Value") +
                  ggtitle(title) +
                  theme(
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.background = element_blank(),
                      legend.text = element_text(size = 12),
                      text = element_text(family = "serif"),
                      legend.title = element_text(size = 12, face = "bold"),
                      title = element_text(size = 12, face = "bold"),
                      axis.text.x = element_text(size = 11.5),
                      axis.text.y = element_text(size = 11.5),
                      axis.title.x = element_text(size = 12,face = "bold",margin = margin(t = 10, r = 0, b = 0, l = 0)),
                      axis.title.y = element_text(size = 13.5,face = "bold",margin = margin(t = 0, r = 15, b = 0, l = 0)),
                      strip.text.x = element_text(size = 14))
                  # scale_x_discrete(breaks = (0:8)/4) 
                  # scale_y_discrete(breaks = (0:8)/4)
          #iii) save
              title2<-gsub("\n","",title)
              ggsave(tsave1 <- file.path(fig.dir,paste(title2,".png",sep = "")),
                     p, width = 6.5*1.2, height = 4*1.2, units = "in"
              )
              browseURL(tsave1)
          return(p)
      }

  #c) plot a variety of variables
      p1 <- plotfun(fill.x="IO", fill.x.name="IO", title="Mean IO by Component Size and Birank")
      

  #d) aggregate data
      tdf <- df[, .(N = mean(IO)), by = c("t1birank.MME.BiRank", "comp.v")]
      tdf3 <- tdf2[! paste(tdf2$t1birank.MME.BiRank, tdf2$comp.v) %in% paste(tdf$t1birank.MME.BiRank, tdf$comp.v), ]
      tdf <- rbind(tdf, tdf3)
      tdf[, t1birank.MME.BiRank := as.numeric(t1birank.MME.BiRank)]
      tdf$comp.v <- factor(tdf$comp.v, levels = c("1","2-10", "11-20","21-50","51-100","101-1000","1000P"))
      tdf$N[tdf$N > quantile(tdf$N, .9) * 2] <- quantile(tdf$N, .9) * 2
      tdf <- tdf[t1birank.MME.BiRank!=0, ]
      
      
  #e) aggregate data for ploting
      df[, comp.size2 := (round(as.numeric(comp.size)/1)*1)]
      df[comp.size2 > 500, comp.size2 := 500]
      tdf <- df[, .(N = mean(IO)), by = c("comp.size2")]
      # tdf3 <- tdf2[! paste(tdf2$t1birank.MME.BiRank, tdf2$comp.size2) %in% paste(tdf$t1birank.MME.BiRank, tdf$comp.size2), ]
      # tdf <- rbind(tdf, tdf3)
      tdf[, comp.size2 := as.numeric(comp.size2)]

  #f) plot
      p <- ggplot(df, aes(x = comp.size2, y = IO)) +
        geom_smooth(se=F, span =.999999)+
        # scale_fill_gradientn(colors = viridis(10)) +
        labs(y = "Rate of IO", x = "Component Size") +
        ggtitle("Component Size by IO") +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          legend.text = element_text(size = 12),
          text = element_text(family = "serif"),
          legend.title = element_text(size = 12, face = "bold"),
          title = element_text(size = 12, face = "bold"),
          axis.text.x = element_text(size = 11.5),
          axis.text.y = element_text(size = 11.5),
          axis.title.x = element_text(size = 12,face = "bold",margin = margin(t = 10, r = 0, b = 0, l = 0)),
          axis.title.y = element_text(size = 13.5,face = "bold",margin = margin(t = 0, r = 15, b = 0, l = 0)),
          strip.text.x = element_text(size = 14))
      # scale_x_discrete(breaks = (0:8)/4)
      # scale_y_discrete(breaks = (0:8)/4)
      #iii) save
      title2<-gsub("\n","","Component Size by IO")
      ggsave(tsave1 <- file.path(fig.dir,paste(title2,".png",sep = "")),
             p, width = 6.5*1.2, height = 4*1.2, units = "in"
      )
      browseURL(tsave1)
      