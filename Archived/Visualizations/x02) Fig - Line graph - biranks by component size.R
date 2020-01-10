# Show that biranks are totally confounded by components

#0) prepare workspace
    #a) load libraries
        library(docshop)

    #b) find local directories
        user <- ifelse(grepl("briarons", getwd()), "briarons",
                ifelse(grepl("bda13", getwd()), "bda13",
                "admin"))
        main.dir <- "hidden"
        fig.dir <- "hidden"
        my.dir <- "hidden"
        

#1) load data
    df <- read_fst(file.path(my.dir, "2018Q1flawed.fst"), as.data.table = T)

    
    #2) graph
    #a) aggregate data
    tdf <- df[, mean(t1birank.unweighted.BiRank, na.rm = T), by=comp.size]
        tdf <- tdf[!is.na(tdf$comp.size), ]
        
    #b) make plot
        tdf[, comp.size2 := ifelse(comp.size > 200, 200, comp.size)]
            p <- ggplot(tdf, aes(x=comp.size2, y= V1)) +
                  geom_point(size = 2, color="black", fill= "#F8766D", pch=21) +
                  geom_smooth(color = "black", size = 1.5, se=F, method = "lm") +
                  labs(x = "Component Size", y = "Birank") +
                  ggtitle("Raw Birank by Component Size") +
                  theme(text = element_text(family = "serif"),
                        title = element_text(size = 10, face = "bold"),
                        strip.text.x = element_text( size = 9.5, margin = margin(0.05, 0.05, 0.05, 0.05, "cm")),
                        axis.text.x = element_text(size = 8.5),
                        axis.text.y = element_text(size = 9),
                        panel.spacing = unit(.75, "lines")
                  )
            
    #c) save and browse plot
        ggsave(tsave1 <- paste(fig.dir, "Raw Birank by Component Size.png", sep = ""), p, width = 5, height = 3, units = "in")
        browseURL(tsave1)

