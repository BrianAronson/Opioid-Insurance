#0) Prepare workspace
    #a) load libraries
        library(docshop)
        library(ggplot2)
        library(viridis)
        library(scales)
        library(cowplot)
        
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
        df <- df[doc_count > 0, ]
        df <- df[comp.size > 10000, ]

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
            tdf2 <- data.table(df[, table(pagerank, t1birank.MME.CoHITS)])
  
    #b) make function
        plotfun<-function(fill.x,fill.x.name,title){
            #i) aggregate data
                tdf <- df[, .(N = mean(eval(parse(text = fill.x)))), by = c("t1birank.MME.CoHITS", "pagerank")]
                tdf3 <- tdf2[! paste(tdf2$pagerank, tdf2$t1birank.MME.CoHITS) %in% paste(tdf$pagerank, tdf$t1birank.MME.CoHITS), ]
                tdf<- rbind(tdf, tdf3)
                quantile(tdf$N, .25)
                tdf$N[tdf$N < quantile(tdf$N, .2) / 2] <- quantile(tdf$N, .2) / 2
                tdf$N[tdf$N > quantile(tdf$N, .8) * 2] <- quantile(tdf$N, .8) * 2
            #ii) plot
                p <- ggplot(tdf, aes(y = t1birank.MME.CoHITS, x = pagerank)) +
                    geom_raster(aes(fill = N)) +
                    scale_fill_gradientn(colors = viridis(10)) +
                    labs(x = "Pagerank (mean-centered)", y = "Birank (mean-centered)", fill = "Value") +
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
                    scale_x_discrete(breaks = (0:8)/4) +
                    scale_y_discrete(breaks = (0:8)/4)
            #iii) save
                title2<-gsub("\n","",title)
                ggsave(tsave1 <- file.path(fig.dir,paste(title2,".png",sep = "")),
                       p, width = 6.5*1.2, height = 4*1.2, units = "in"
                )
                browseURL(tsave1)
            return(p)
        }

    #c) plot a variety of variables
        p1 <- plotfun(fill.x="IO", fill.x.name="IO", title="Mean IO by Pagerank and Birank")
        p2 <- plotfun(fill.x="ORD", fill.x.name="ORD", title="Mean ORD by Pagerank and Birank")
        p3 <- plotfun(fill.x="first.io", fill.x.name="First IO", title="Mean First IO by Pagerank and Birank")
        p4 <- plotfun(fill.x="first.ord", fill.x.name="First ORD", title="Mean First ORD by Pagerank and Birank")
        
        # p5 <- plotfun(fill.x="rx_count", fill.x.name="RX Count", title="Mean RX Count by Pagerank and Birank")
        # p6 <- plotfun(fill.x="pharm_count", fill.x.name="Pharmacies", title="Mean Pharmacy Count by Pagerank and Birank")
        # p7 <- plotfun(fill.x="degree", fill.x.name="Degree", title="Mean Degree by Pagerank and Birank")
        # p8 <- plotfun(fill.x="t1degree.unweighted.none", fill.x.name="Bidegree", title="Mean Bidegree by Pagerank and Birank")
        
    #d) plot bipartite and unweighted projection to grid
        p<-plot_grid(p1,p2,p3,p4,ncol=2)
        title <- ggdraw() + draw_label("Pagerank by Birank by Third Variable", fontface = 'bold', size = 20, fontfamily = "serif")
        p <- plot_grid(title, p, nrow = 2, rel_heights = c(0.07, 1)) #put title above graph
        p <- plot_grid(p, scale = .98)
        save_plot(file.path(fig.dir, "Pagerank by Birank by Third Variable.png"), p,
                  base_height = 8,
                  base_aspect_ratio = 1.77)
        browseURL(file.path(fig.dir, "Pagerank by Birank by Third Variable.png"))
