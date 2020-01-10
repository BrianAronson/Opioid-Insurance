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


#3) Linegraph: Plot Birank by Pagerank
    #a) prep data
        #i) trim outliers
            outlierfun <- function(x, min = .02, max = .98) {
              x[x < quantile(x, min, na.rm = T)] <- quantile(x, min, na.rm = T)
              x[x > quantile(x, max, na.rm = T)] <- quantile(x, max, na.rm = T)
              return(x)
            }

            df[, ':='(
              gbirank = outlierfun(t1birank.unweighted.BiRank, 0, .995),
              gbirank2 = outlierfun(t1birank.MME.CoHITS, 0, .995),
              gpagerank = outlierfun(t1pagerank.unweighted.none, 0, .995)
            )]

        #ii) round birank and pagerank to decimals
            df[, ':='(
              gbirank = floor(gbirank*20)/20,
              gbirank2 = floor(gbirank2*20)/20,
              gpagerank = floor(gpagerank*20)/20
            )]

        #iii) aggregate birank and pagerank; subset source data
            tdf <- df[comp.size>10000 & doc_count > 0 & !is.na(doc_count), .(BiRank = mean(gbirank), BiRank.MME = mean(gbirank2), N = .N), by = .(PageRank = gpagerank)]
            tdf <- df[doc_count > 0 & !is.na(doc_count), .(BiRank = mean(gbirank), BiRank.MME = mean(gbirank2), N = .N), by = .(PageRank = gpagerank)]
            tdf <- df[, .(BiRank = mean(gbirank), BiRank.MME = mean(gbirank2), N = .N), by = .(PageRank = gpagerank)]
            tdf <- tdf[tdf$PageRank != 0, ]
            

    #b) Plot Birank by Pagerank
        p <- ggplot(tdf, aes(x = PageRank, y = BiRank.MME)) +
          geom_point(size = 2, color="black", fill= "#F8766D", pch=21) +
          geom_smooth(color = "black", size = 1.5, se=F, method = "lm") +
          labs(x = "PageRank", y = "Birank.MME.CoHITS") +
          ggtitle("Birank.MME.CoHITS by Pagerank") +
          theme(text = element_text(family = "serif"),
                title = element_text(size = 10, face = "bold"),
                strip.text.x = element_text( size = 9.5, margin = margin(0.05, 0.05, 0.05, 0.05, "cm")),
                axis.text.x = element_text(size = 8.5),
                axis.text.y = element_text(size = 9),
                panel.spacing = unit(.75, "lines")
          )

    #c) save and browse plot
        ggsave(tsave1 <- paste(fig.dir, "Birank.MME.CoHITS by Pagerank Linegraph.png", sep = ""), p, width = 5, height = 3, units = "in")
        browseURL(tsave1)
