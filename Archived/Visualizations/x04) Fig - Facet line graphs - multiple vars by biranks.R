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


#2) prep data
    #a) trim outliers
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

    #b) round birank and pagerank to decimals
        df[, ':='(
          gbirank = floor(gbirank*20)/20,
          gbirank2 = floor(gbirank2*20)/20,
          gpagerank = floor(gpagerank*20)/20
        )]


#3) Plot bivariate associations (linegraphs) between pagerank, birank, and cohits
    #a) Subset to interesting variables
        #i) subset to largest component
            # df <- df[doc_count > 0 & !is.na(doc_count), ]
            df <- df[comp.size > 10000, ]
        #ii) scale ranks to 0-1 values
            range01 <- function(x) (x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T))
            df[, ':='(
              gbirank = range01(gbirank),
              gbirank2 = range01(gbirank2),
              gpagerank = range01(gpagerank)
            )]
        #iii) subset variables
            df2 <- df[, c("gbirank", "first.ord", "first.io", "ORD", "IO")]
            df3 <- df[, c("gpagerank", "first.ord", "first.io", "ORD", "IO")]
            df4 <- df[, c("gbirank2", "first.ord", "first.io", "ORD", "IO")]

    #b) find mean variable by rank
        df2 <- df2[, lapply(.SD, mean), by = gbirank]
        df3 <- df3[, lapply(.SD, mean), by = gpagerank]
        df4 <- df4[, lapply(.SD, mean), by = gbirank2]

    #c) aggregate and append to one dataframe
        a <- data.table(gather(df2, key = "Variable", value = "Value", -gbirank))
        b <- data.table(gather(df3, key = "Variable", value = "Value", -gpagerank))
        d <- data.table(gather(df4, key = "Variable", value = "Value", -gbirank2))
        names(a)[1] <- "Rank"
        names(b)[1] <- "Rank"
        names(d)[1] <- "Rank"
        a$group <- "birank"
        b$group <- "pagerank"
        d$group <- "birank.mme"
        df4 <- rbind(a, b, d)

    #d) graph
        p <- ggplot(df4, aes(y = Value, x = Rank, color = group, fill = group)) +
          geom_point(size = 1, colour = "black", pch = 21) +
          geom_smooth(size = 1, se = F, span = .5) +
          facet_wrap( ~ Variable, scales = "free") +
          labs(x = "Rank", y = "") +
          ggtitle("Variable Associations with Different Ranks") +
          theme(text = element_text(family = "serif"),
                title = element_text(size = 10, face = "bold"),
                strip.text.x = element_text(size = 9.5, margin = margin(0.05, 0.05, 0.05, 0.05, "cm")),
                axis.text.x = element_text(size = 8.5),
                axis.text.y = element_text(size = 9),
                panel.spacing = unit(.75, "lines"))

    #e) save
        ggsave(tsave1 <- file.path(fig.dir, "b.Variable Associations with Different Ranks.png"), p, width = 7, height = 4, units = "in")
        browseURL(tsave1)
        