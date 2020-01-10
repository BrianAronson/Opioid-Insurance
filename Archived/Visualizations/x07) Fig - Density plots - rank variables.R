#0) Prepare workspace
    #a) load libraries
        library(docshop)

    #b) find local directories
        user <- ifelse(grepl("briarons", getwd()), "briarons",
                ifelse(grepl("bda13", getwd()), "bda13",
                "admin"))
        main.dir <- "hidden"
        fig.dir <- "hidden"
        my.dir <- "hidden"


#1) Load data
    df <- read_fst(file.path(my.dir, "2018Q1flawed.fst"), as.data.table = T)

    
#2) Prepare data
    #a) createfunction to trim outliers
        outlierfun<-function(x,min,max){
          x[x<quantile(x,min,na.rm=T)]<-quantile(x,min,na.rm=T)
          x[x>quantile(x,max,na.rm=T)]<-quantile(x,max,na.rm=T)
          return(x)
        }
    #b) remove sample who saw no doctors and remove extreme outliers for kaicheng variables
        df <- df[doc_count>0, ]
        # df <- df[comp.size>10000, ]
    #c) Determine variables to transform/plot
        cols <- names(df)[grepl("t1|pagerank", names(df))]
        cols <- cols[!grepl("t2|t3", cols)]
    #d) cap outliers
        df[, (cols) := lapply(.SD, function(x) outlierfun(x, .01, .99)), .SDcols = cols]

        
#3) Create density plotting function
    densfun <- function(x, title = "Title", x.name = "x", fill.x = "grey90"){
        p <- ggplot() +
            geom_density(data = df, aes(x = eval(parse(text = x))), fill = fill.x) +
            labs(x = x.name, y = "Density", fill = "") +
            ggtitle(title) +
            scale_fill_grey(start = 0, end = .8) +
            theme_bw() +
            theme(
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.line = element_line(colour = "black"),
                legend.text = element_text(size = 12),
                text = element_text(family = "serif"),
                legend.title = element_text(size = 12, face = "bold"),
                title = element_text(size = 12, face = "bold"),
                legend.position = "bottom",
                axis.text.x = element_text(size = 11.5, hjust = 1),
                axis.text.y = element_text(size = 11.5),
                axis.title.x = element_text(size = 12, face="bold", margin = margin(t = 10, r = 0, b = 0, l = 0)),
                axis.title.y = element_text(size = 13.5, face="bold", margin = margin(t = 0, r = 15, b = 0, l = 0)),
                strip.text.x = element_text(size = 14)
            )

        title2<-gsub("\n","",title)
        ggsave(tsave1 <- file.path(fig.dir, paste(title2, ".png",sep="")),
               p, width = 6.5 * 1.2, height = 4 * 1.2, units = "in"
              )
        # browseURL(tsave1)
    }


#4) Plot densities
    # densfun(x = "t1baseline.unweighted.MME", x.name = "t1baseline.unweighted.MME", title = "Pagerank Distribution (Mean-centered)")
    for(i in 1:length(cols)){
        p <- densfun(x = cols[i], x.name = cols[i], title = paste(cols[i], 'Distribution'))
    }
  