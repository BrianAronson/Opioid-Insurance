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
    df <- read_fst(file.path(my.dir, "2018Q1.fst"), as.data.table = T)

#2) Reduce data to interesting varaibles
    setnames(df,c("t2pagerank.unweighted.none","t3pagerank.unweighted.none"),
             c("t2pagerank", "t3pagerank"))
    keeps <- names(df)[!grepl("baseline|none|BGER|gpagerank|pagerank_lag", names(df))]
    keeps <- keeps[grepl("t1", keeps)]
    keeps <- c("TOTAL_MME","ORD","IO","first.ord","first.io","nplusnplus90_4","pagerank", keeps)
    comp.size <- df$comp.size
    df <- df[, keeps, with=F]
    
#3) Prep data for correlations
    #a) subset df
        df <- df[doc_count > 0, ]
        df <- df[comp.size > 10000, ]
    #b) Find correlations of MME and ORD with all other variables
        cors <- df[, .(
             var = names(df),
             MME = cor(TOTAL_MME, .SD, use = "pairwise.complete.obs"),
             ORD = cor(ORD, .SD, use = "pairwise.complete.obs"),
             IO = cor(IO, .SD, use = "pairwise.complete.obs"),
             first.ord = cor(first.ord, .SD, use = "pairwise.complete.obs"),
             first.io = cor(first.io, .SD, use = "pairwise.complete.obs"),
             NN90 = cor(nplusnplus90_4, .SD, use = "pairwise.complete.obs"),
             pagerank = cor(pagerank, .SD, use = "pairwise.complete.obs")
        )]

    #c) format cormat
         cormat <- cors
         cormat <- as.data.frame(cormat)
         # cormat$var[1:6]<-c("MME","ORD","NN90","pagerank","doc_count","bi_degree")

#3) Graph correlations
    #a) Prep data for graphs
        #i) remove lower triangle
            lower_tri<-cormat
            lower_tri[upper.tri(lower_tri)]<-0
        #ii) Put into three columns
            melted_cormat <- melt(lower_tri, na.rm = F, variable.factor=F)
        #iii) Format results
            melted_cormat$val2 <- melted_cormat$value
            melted_cormat$val2 <- sprintf("%.2f", melted_cormat$val2)
        #iv) Erase duplicates from melted values and format
            melted_cormat$var <- factor(melted_cormat$var, levels = unique(melted_cormat$var))
            melted_cormat$val2[melted_cormat$value==0] <- ""

    #b) Initiate heatmap
        ggheatmap <- ggplot(data = melted_cormat, aes(variable, var, fill = value, label=val2))+
            geom_tile(color = "white")+
            scale_fill_gradient2(low = "#1bc3e5", high = "#e53d1b", midpoint = 0, limit = c(-1,1), space = "Lab", name="Pearson\nCorrelation") +
            theme_minimal()+
              theme(axis.text.x = element_text( angle = 45, vjust = 1, size = 14, hjust = 1),
                    axis.text.y = element_text(size = 14) #axis label text size
              )

    #b) graph with text, prettier labels. etc.
        #i) manually assign title
            title="Correlations with T1 rank variables (full sample)"
        #ii) graph
            p <- ggheatmap +
                geom_text(aes(variable, var, label = val2), color = "black", size = 4.5,hjust=.5) +
                theme(
                    axis.title.x = element_blank(),
                    axis.title.y = element_blank(),
                    panel.grid = element_blank(),
                    panel.border = element_blank(),
                    panel.background = element_blank(),
                    axis.ticks = element_blank(),
                    legend.text = element_text(size = 15),
                    legend.title = element_text(size = 15),
                    plot.title = element_text(size=20, hjust=.38),
                    plot.subtitle = element_text(size=20, hjust=.38),
                    plot.margin = margin(.5, 4, 0, 0, "cm"))+
                guides(fill = guide_colorbar(barwidth = 3, barheight = 15))+
                ggtitle(title)
        #iv) save graph
            ggsave(tsave1 <- file.path(fig.dir, paste(title,".png",sep="")), p, width = 12, height = 9, units = "in")
            browseURL(tsave1)



# conditional correlation
    # tm<-cor(resid(lm(cbind(birank, pagerank) ~., df)))
