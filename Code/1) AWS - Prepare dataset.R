#0) Prepare workspace
    #a) load libraries
        library(docshop)
        library(igraph)
        library(ff)
        library(Matrix)
    
    #b) set directories
        main.dir <- file.path("hidden", "additional_data", "BDA")
        my.dir <- file.path(main.dir, "PageRank")
        v3.dir <- file.path("hidden", "derived_v3")
        object.dir <- file.path("additional_data", "BDA", "PageRank.subset")
        
    #c) create directories on S3 
        put_folder(my.dir)
        
        
#1) Load initial info 
    #a) load first io and ord
        df.io <- s3read_csv(file.path(v3.dir, "overdose_patients", "zip5", "zip5_first_encounter_io_quarter.tsv"))
        df.ord <- s3read_csv(file.path(v3.dir, "overdose_patients", "zip5", "zip5_first_encounter_ord_quarter.tsv"))
    #b) set variables to grab
        reg.vars <- c('PATID', 'hepc', 'hiv', 'chronic', 'concomitant', 'psych_disorder', 'any_rx', 'conf_count', 'er_count', 'MAT_user', 'palliative_care', 'cancer', 'TOTAL_MME', 'IO', 'ORD', 'GDR_CD', 'county_id', 'YRDOB', "doc_count")
    #c) set dates to grab
        df.date <- data.table(y = rep(2009:2012, each=4), q = rep(1:4, 4))
        df.date <- df.date[-c(1, 15:16), ]
    #d) pull counties to keep
        good.counties <- as.vector(unlist(fread(file.path("/home","rstudio","Dropbox", "AWS", "Postdoc","counties_to_keep.csv"))))
    #e) pull pagerank functions
        pagerank.funs <- list.files(file.path("/home","rstudio","Dropbox", "AWS", "Postdoc", "pagerankr"), full.names = T)
        for(i in 1:length(pagerank.funs)) source(pagerank.funs[i])


#2) start loop and load data by year-quarter
  for(i in 1:nrow(df.date)){
    #a) prep loop variables
        t.year = df.date$y[i]
        t.quarter = df.date$q[i]
        suffix <- paste(t.year, "q", t.quarter, sep="")
    #b) load key data
        df <- s3read_any(file.path(v3.dir, "doctor_shopping_metrics_compare_reg_tables", "zip5", paste("doctor_shopping_metrics_compare_reg_table_", suffix, ".tsv", sep="")), select = reg.vars)
        edges <- s3read_any(file.path(v3.dir, "opioid_rx_v_pagerank", "zip5", "lag_3_edgelist", paste("zip5_lag_3_edgelist_", suffix, ".tsv", sep = "")))
    #c) convert PATID and NPI to character
        df[, PATID := as.character(PATID)]
        edges[, ':='( 
              PATID = as.character(PATID),
              NPI = as.character(NPI)
        )]

        
#3) subset data to relevant counties
    df <- df[county_id %in% good.counties, ]
    edges <- edges[PATID %in% df$PATID, ]

    
#4) Merge first io and first ord into df
    t.df.io <- df.io[df.io$year == t.year & df.io$quarter == t.quarter, ]
    t.df.ord <- df.ord[df.ord$year == t.year & df.ord$quarter == t.quarter, ]
    df[, first.io := as.numeric(PATID %in% t.df.io$PATID)]
    df[, first.ord := as.numeric(PATID %in% t.df.ord$PATID)]

                    
#5) Measure component sizes
    #a) create a type attribute for making one mode projection
        node.type1 <- data.table(name = as.character(unique(edges$PATID)), type = TRUE)
        node.type2 <- data.table(name = as.character(unique(edges$NPI)), type = FALSE)
        node.type <- rbind(node.type1, node.type2)
    #b) convert to graph object
        g <- graph_from_data_frame(edges, vertices = node.type)
    #c) partition to one mode projection
        bpg <- bipartite_projection(g)
    #d) keep just patient ties
        bpg <- bpg[[2]]
        tdegree <- degree(bpg)
        df.degree <- data.table(PATID = names(tdegree), degree.unweighted.none = tdegree)
    #e) find components
        comps <- components(bpg)
    #f) tally component sizes
        df.comps <- data.table(PATID = as.character(names(comps$membership)), component = comps$membership)
        df.comps[, comp.size := comps$csize[component]]
    #g) put component sizes into df
        df <- merge(df, df.comps, by="PATID", all.x = T)
        rm(bpg, comps, df.comps, g, node.type, node.type1, node.type2)
        gc()
        

#6) Make new variables; delete redundant ones
    #a) remove 0s from comp.size
        df$comp.size[is.na(df$comp.size)] <- 0
    #b) identify component size as dummies, convert characters to numbers
        df[, ':=' (
          PATID = as.numeric(PATID), 
          female = ifelse(GDR_CD == "F", 1, 0), 
          year.quarter = t.year + (t.quarter/4 - .25), 
          age = t.year - YRDOB, 
          YRDOB = NULL, 
          GDR_CD = NULL
        )]
        gc()
        
        
#7) Estimate pagerank scores
    #a) prep datatable with PATID
        pr.df <- data.table(PATID = as.numeric(unique(edges$PATID)))

    #b) create one-mode pagerank scores
        pr.df[, ':='(
              pagerank = pagerank_from_edgelist(edges, verbose = F, remove.weights = T),
              pagerank.rx_count = pagerank_from_edgelist(edges, verbose = F, weight.name = 'rx_count'),
              pagerank.rx_days_count = pagerank_from_edgelist(edges, verbose = F, weight.name = 'rx_days_count'),
              pagerank.MME = pagerank_from_edgelist(edges, verbose = F, weight.name = 'MME')
        )]

    #c) create bipartite pagerank scores
        normalizers <- c('HITS', 'CoHITS', 'BGRM', 'BiRank') #'BGER',
        weights <- c('unweighted', 'rx_count', 'rx_days_count', 'MME')
        for(k in 1:length(normalizers)){
          for(j in 1:length(weights)){
            var.name <- paste("birank", weights[j], normalizers[k], sep = ".")
            pr.df[, (var.name):= bipartite_pagerank_from_edgelist(edges, normalizer = normalizers[k], weight.name = weights[j])]
            print(paste(var.name))
          }
        }
        
    #d) merge into df
        df <- merge(df, pr.df, by = "PATID", all.x = T)

        
#8) Create baseline variables based on edges
    df.degree[, PATID := as.numeric(PATID)]
    df <- merge(df, df.degree, by = "PATID", all.x = T)
    edges[ ,baseline.unweighted.prov_count := 1]
    df.basevars <- edges[, lapply(.SD, sum), by = "PATID", .SDcols = c("MME", "rx_days_count", "rx_count", "baseline.unweighted.prov_count")]
    setnames(df.basevars,
             c("MME", "rx_days_count", "rx_count"),
             c("baseline.unweighted.MME", "baseline.unweighted.rx_count", "baseline.unweighted.rx_days_count")
    )
    df.basevars[, PATID := as.numeric(PATID)]
    df <- merge(df, df.basevars, by = "PATID", all.x = T)
    
    
#9) Mean center biranks
    #a) identify pagerank variables
        vars <- names(df)[grepl("rank", names(df))]
    #b) mean-center all variables by component (all components should have biranks that add up to the same number)
        df[, (vars) := lapply(.SD, function(x) x/mean(x, na.rm = T)), .SDcols = vars, by="component"]
        gc()
    
                  
#10) Impute 0s for patients who did not see doctors
    #a) identify vars
        vars <- df[, lapply(.SD, function(x) sum(is.na(x)))]
        vars <- names(vars)[vars>0]
        vars <- setdiff(vars, c("component", "county_id"))
    #b) set missings to 0
        df[, (vars) := lapply(.SD, function(x) ifelse(is.na(x), 0, x)), .SDcols = vars]
        gc()
        
        
#11) Save 
    s3write_fst(df, file.path(my.dir, paste("reg", t.year, ".", t.quarter, ".fst", sep = "")))
    print(i)
    gc()
  }

    