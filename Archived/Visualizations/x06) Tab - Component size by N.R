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

#2) find component size by number of people
    data.table(
      `Component Size`=c(
        "1","1-5","5-10","10-20","20-50","50-100","100-1000","1000+"
      ),
      `Number People`=c(
        sum(df2$comp.size == 1, na.rm = T),
        sum(df2$comp.size > 1 & df2$comp.size <= 5, na.rm = T),
        sum(df2$comp.size > 5 & df2$comp.size <= 10, na.rm = T),
        sum(df2$comp.size > 10 & df2$comp.size <= 20, na.rm = T),
        sum(df2$comp.size > 20 & df2$comp.size <= 50, na.rm = T),
        sum(df2$comp.size > 50 & df2$comp.size <= 100, na.rm = T),
        sum(df2$comp.size > 100 & df2$comp.size <= 1000, na.rm = T),
        sum(df2$comp.size > 1000, na.rm = T)
      )
    )
