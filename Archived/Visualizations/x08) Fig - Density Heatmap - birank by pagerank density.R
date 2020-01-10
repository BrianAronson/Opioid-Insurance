#0) Prepare workspace
    #a) load libraries
        library(docshop)
        library(ggplot2)
        library(viridis)
        library(scales)
    
    #b) find local directories
        user <- ifelse(grepl("briarons", getwd()), "briarons",
                ifelse(grepl("bda13", getwd()), "bda13",
                "admin"))
        main.dir <- "hidden"
        fig.dir <- "hidden"
        my.dir <- "hidden"
        

#1) Load data
    df <- read_fst(file.path(my.dir, "2018Q1.fst"), as.data.table = T)

    
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
            x[x > 2] <- 2
            return(x)
        }
        
    #b) remove sample who saw no doctors and remove extreme outliers for kaicheng variables
        # df <- df[doc_count > 0, ]
        df <- df[comp.size > 10000, ]
        
    #c) Determine variables to transform/plot
        cols <- names(df)[grepl("t1|pagerank", names(df))]
        cols <- cols[!grepl("t2|t3|BGER", cols)]
        
    #d) cap outliers
        df[, (cols) := lapply(.SD, function(x) outlierfun3(x)), .SDcols = cols]
        
    #e) round to .1
        df[, (cols) := lapply(.SD, function(x) floor(x*8)/8), .SDcols = cols]
        # df[, lapply(.SD, summary), .SDcols = cols]

        
#3) Plot pagerank by birank bivariate density
    #a) aggregate frequencies
        #i) add 0 for all values to make table produce columns for 0s 
            a <- df[1,]
            a <- a[, lapply(.SD, function(x) return(0))]
            df <- rbind(df,a)
        #ii) aggregate
            tdf <- data.table(df[,table(pagerank, t1birank.MME.CoHITS)])
            
    #b) calculate density and cap at .05
        tdf[, N := N / sum(N)]
        tdf$N[tdf$N > .05] <- .05
        tdf <- tdf[pagerank >= .5 & pagerank <= 1.5]
        
    #c) assign plot title
        title<-"Pagerank by Birank Density Distribution"
        
    #d) plot 
        p <- ggplot(tdf, aes(y = t1birank.MME.CoHITS, x = pagerank)) +
            geom_raster(aes(fill = N)) +
            scale_fill_gradientn(colors = viridis(10), labels = percent) +
            labs(x = "Pagerank (mean-centered)", y = "Birank (mean-centered)", fill = "Density") +
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
                strip.text.x = element_text(size = 14)) +
            scale_x_discrete(breaks = (2:6)/4) +
            scale_y_discrete(breaks = (0:8)/4)

    #e) save
        title2 <- gsub("\n","",title)
        ggsave(tsave1 <- paste(fig.dir, title2, ".png", sep = ""),
               p, width = 6.5 * 1.2, height = 4 * 1.2, units = "in"
        )
        browseURL(tsave1)