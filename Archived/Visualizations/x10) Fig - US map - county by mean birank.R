#run by sample
# for(q in 1:3){
q=1  

#0) Prepare workspace
    #a) load libraries
        library(docshop)
        library(geosphere)
        library(ggplot2)
        library(reshape2)
        library(stringr)
        library(sp)
        library(rgeos)
        library(leaflet)
        library(tidycensus)
        library(ggplot2)
        library(scales)
        library(tigris)
        library(sf)
        library(viridis)
        library(cowplot)
        options(tigris_class = "sf")
        options(tigris_use_cache = TRUE)
        
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
        outlierfun<-function(x,min,max){
          x[x<quantile(x,min,na.rm = T)]<-quantile(x,min,na.rm = T)
          x[x>quantile(x,max,na.rm = T)]<-quantile(x,max,na.rm = T)
          return(x)
        }
    #b) remove sample who saw no doctors and remove extreme outliers for kaicheng variables
        if(q==1){
          df <- df[comp.size>10000, ]
          plotname <-"(subcomponent)"
        }else if (q==2){
          df <- df[doc_count>0, ]  
          plotname <-"(doc subsample)"
        }else{
          plotname <-"(full)"
        }

    #c) Determine variables to transform/plot
        cols <- names(df)[grepl("t1|pagerank", names(df))]
        cols <- cols[!grepl("t2|t3|BGER", cols)]
        cols <- c(cols, "TOTAL_MME")
  
    #d) cap outliers
        df[, (cols) :=  lapply(.SD, function(x) outlierfun(x, .01, .99)), .SDcols = cols]
    
        
#3) aggregate doctor shopping data by county
    df2 <- df[, .(
        N = .N,
        mean.ord = mean(ORD),
        mean.IO = mean(IO),
        mean.first.ord = mean(first.ord),
        mean.first.io = mean(first.io),
        mean.nplusnplus90_4 = mean(nplusnplus90_4),
        
        mean.mme = mean(TOTAL_MME),
        median.mme = median(TOTAL_MME),
        max.mme = max(TOTAL_MME),
        p99.mme = quantile(TOTAL_MME, .99),
        p95.mme = quantile(TOTAL_MME, .95),
        
        mean.pagerank = mean(pagerank),
        median.pagerank = median(pagerank),
        max.pagerank = max(pagerank),
        p99.pagerank = quantile(pagerank, .99),
        p95.pagerank = quantile(pagerank, .95),
        
        mean.birank = mean(t1birank.MME.CoHITS),
        median.birank = median(t1birank.MME.CoHITS),
        max.birank = max(t1birank.MME.CoHITS),
        p99.birank = quantile(t1birank.MME.CoHITS, .99),
        p95.birank = quantile(t1birank.MME.CoHITS, .95)
    ), by = "county_id"]
  
      
#4) prepare geography info    
    #a) load geography; Note this object should not be transformed into a data.table
        df.county <- counties(cb = F, year = 2017)
        
    #b) merge with df2
        df2[, GEOID2 :=  county_id]
        df.county$GEOID2 <- as.numeric(df.county$GEOID)
        df.county <- merge(df.county, df2, by = "GEOID2") #, all.x  = T
        
    #c) remove Alaska, hawaii, and puerto rico
        df.fips <- unique(fips_codes[,1:2])
        bad.fips <- df.fips$state_code[df.fips$state %in% c("AK", "HI") | df.fips$state_code > 57]
        df.county <- df.county[!df.county$STATEFP %in% bad.fips, ] 
        
    # #d) load and merge with acs 
    #     acspath <- file.path(main.dir, "ACS", "ACS (county).fst")
    #     acs <- read.fst(acspath, as.data.table = T)
    #     acs$GEOID2 <- as.numeric(acs$county)
    #     df3 <- merge(df.county, acs, by = "GEOID2")    
    
#5) prepare data for graphing
        df.county$mean.ord <- outlierfun(df.county$mean.ord, 0, .97)
        df.county$mean.io <- outlierfun(df.county$mean.ord, 0, .97)
        df.county$mean.first.ord <- outlierfun(df.county$mean.first.ord, 0, .97)
        df.county$mean.first.io <- outlierfun(df.county$mean.first.ord, 0, .97)
        df.county$mean.pagerank <- outlierfun(df.county$mean.pagerank, .05, .98)
        
        
        df.county$median.pagerank <- outlierfun(df.county$median.pagerank, .05, .98)
        df.county$max.pagerank <- outlierfun(df.county$max.pagerank, .05, .98)
        df.county$p95.pagerank <- outlierfun(df.county$p95.pagerank, .05, .98)
        df.county$p99.pagerank <- outlierfun(df.county$p99.pagerank, .05, .98)
        df.county$mean.birank <- outlierfun(df.county$mean.birank, .05, .98)
        df.county$median.birank <- outlierfun(df.county$median.birank, .05, .98)
        df.county$max.birank <- outlierfun(df.county$max.birank, .05, .98)
        df.county$p95.birank <- outlierfun(df.county$p95.birank, .05, .98)
        df.county$p99.birank <- outlierfun(df.county$p99.birank, .05, .98)
        df.county$mean.nplusnplus90_4 <- outlierfun(df.county$mean.nplusnplus90_4, 0, .98)
        df.county$mean.mme <- outlierfun(df.county$mean.mme, 0, .98)

#6) make function to plot data    
    plotfun<-function(fill.var, fill.name = "", fig.title = ""){
        p <- ggplot()+
            geom_sf(
                data = df.county,
                eval(parse(text = paste("aes(fill = ",fill.var,")",sep = ""))),
                size = 0.05,
                color = "grey50"
            )+
                coord_sf(crs = sf::st_crs(df.county), datum = NA)+
                scale_fill_gradient2(midpoint = median(eval(parse(text = paste("df.county$",fill.var,sep = ""))),na.rm = T),low = "white",mid = "#FFF6F6",high = "#cc0000",na.value = "white")+#,midpoint = 20,limits = c(0,125)
                theme_void() +
                theme(axis.line = element_line(colour = "black"),
                      panel.grid = element_blank(),
                      panel.border = element_blank(),
                      panel.background = element_blank(),
                      # legend.position = "none",
                      plot.title = element_text(hjust = 0.5),
                      text = element_text(family = "serif", face = "bold", size = 11),
                      axis.title = element_blank())+
                labs(fill = fill.name,title = fig.title)
            
            title2 <- file.path(fig.dir,paste(fig.title,".png",sep = ""))
            ggsave(title2,p,width = 16,height = 9,units = "in")
            # browseURL(title2)
        return(p)
    }
    
#7) plot data
    p2 <- plotfun(fill.var = "mean.ord", fill.name = "ORD", fig.title = "Mean ORD by County")
    p1 <- plotfun(fill.var = "mean.io", fill.name = "IO", fig.title = "Mean IO by County")
    p4 <- plotfun(fill.var = "mean.first.ord", fill.name = "First ORD", fig.title = "Mean First ORD by County")
    p3 <- plotfun(fill.var = "mean.first.io", fill.name = "First IO", fig.title = "Mean First IO by County")
    
    # p2 <- plotfun(fill.var = "mean.mme", fill.name = "Total MME", fig.title = "Mean Total MME by County")
    # p3 <- plotfun(fill.var = "mean.pagerank", fill.name = "PageRank", fig.title = "Mean PageRank by County")
    # p4 <- plotfun(fill.var = "mean.birank", fill.name = "BiRank", fig.title = "Mean BiRank by County")
    # p3.1 <- plotfun(fill.var = "median.pagerank", fill.name = "PageRank", fig.title = "Median PageRank by County")
    # p4.1 <- plotfun(fill.var = "median.birank", fill.name = "BiRank", fig.title = "Median BiRank by County")
    # p3.2 <- plotfun(fill.var = "max.pagerank", fill.name = "PageRank", fig.title = "Max PageRank by County")
    # p4.2 <- plotfun(fill.var = "max.birank", fill.name = "BiRank", fig.title = "Max BiRank by County")
    # p3.3 <- plotfun(fill.var = "p95.pagerank", fill.name = "PageRank", fig.title = "95th Percentile PageRank by County")
    # p4.3 <- plotfun(fill.var = "p95.birank", fill.name = "BiRank", fig.title = "95th Percentile BiRank by County")
    # p3.4 <- plotfun(fill.var = "p99.pagerank", fill.name = "PageRank", fig.title = "99th Percentile PageRank by County")
    # p4.4 <- plotfun(fill.var = "p99.birank", fill.name = "BiRank", fig.title = "99th Percentile BiRank by County")
    
    # p5 <- plotfun(fill.var = "mean.nplusnplus90_4", fill.name = "% in nplusnplus90_4", fig.title = "Mean nplusnplus90_4 by County")
    
        
#8) group plots    
    title2 <- paste("Rate of Doctor Shopping Indicator by County", plotname)
    #a) means
        p <- plot_grid(p1, p2, p3, p4, ncol = 2)
        title <- ggdraw() + draw_label(title2, fontface = 'bold',size = 20,fontfamily = "serif")
        p <- plot_grid(title, p, nrow = 2, rel_heights = c(0.07, 1)) #put title above graph
        p <- plot_grid(p, scale = .98)
        save_plot(file.path(fig.dir, paste("Means map", plotname, ".png",sep="")), p, 
                  base_height = 8, 
                  base_aspect_ratio = 1.77)
        browseURL(file.path(fig.dir, paste("Means map", plotname, ".png",sep="")))
        
#     #b) medians
#         p <- plot_grid(p1, p2, p3.1, p4.1, ncol = 2)
#         title <- ggdraw() + draw_label(title2, fontface = 'bold',size = 20,fontfamily = "serif")
#         p <- plot_grid(title, p, nrow = 2, rel_heights = c(0.07, 1)) #put title above graph
#         p <- plot_grid(p, scale = .98)
#         save_plot(file.path(fig.dir, paste("Median map", plotname, ".png",sep="")), p, 
#                   base_height = 8, 
#                   base_aspect_ratio = 1.77)
#         
#     #c) max
#         p <- plot_grid(p1, p2, p3.2, p4.2, ncol = 2)
#         title <- ggdraw() + draw_label(title2, fontface = 'bold',size = 20,fontfamily = "serif")
#         p <- plot_grid(title, p, nrow = 2, rel_heights = c(0.07, 1)) #put title above graph
#         p <- plot_grid(p, scale = .98)
#         save_plot(file.path(fig.dir, paste("Max map", plotname, ".png",sep="")), p, 
#                   base_height = 8, 
#                   base_aspect_ratio = 1.77)
#         
#     #d) 95th
#         p <- plot_grid(p1, p2, p3.3, p4.3, ncol = 2)
#         title <- ggdraw() + draw_label(title2, fontface = 'bold',size = 20,fontfamily = "serif")
#         p <- plot_grid(title, p, nrow = 2, rel_heights = c(0.07, 1)) #put title above graph
#         p <- plot_grid(p, scale = .98)
#         save_plot(file.path(fig.dir, paste("P95 map", plotname, ".png",sep="")), p, 
#                   base_height = 8, 
#                   base_aspect_ratio = 1.77)
#         
#     #e) 99th
#         p <- plot_grid(p1, p2, p3.4, p4.4, ncol = 2)
#         title <- ggdraw() + draw_label(title2, fontface = 'bold',size = 20,fontfamily = "serif")
#         p <- plot_grid(title, p, nrow = 2, rel_heights = c(0.07, 1)) #put title above graph
#         p <- plot_grid(p, scale = .98)
#         save_plot(file.path(fig.dir, paste("P99 map", plotname, ".png",sep="")), p, 
#                   base_height = 8, 
#                   base_aspect_ratio = 1.77)
# }