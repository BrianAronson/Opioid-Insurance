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
    df <- read_fst(file.path(my.dir, "reg2011.1.fst"), as.data.table = T)

#2) aggregate doctor shopping data by county
    df2 <- df[, .(
        N = .N,
        mean.ord = mean(ORD),
        mean.io = mean(IO),
        mean.first.ord = mean(first.ord),
        mean.first.io = mean(first.io),
        mean.mme = mean(TOTAL_MME),
        median.mme = median(TOTAL_MME),
        max.mme = max(TOTAL_MME),
        p99.mme = quantile(TOTAL_MME, .99),
        p95.mme = quantile(TOTAL_MME, .95)
    ), by = "county_id"]
  
      
#3) prepare geography info    
    #a) load geography; Note this object should not be transformed into a data.table
        df.county <- counties(cb = F, year = 2017)
        df.state <- states()
        
    #b) merge with df2
        df2[, GEOID2 :=  county_id]
        df.county$GEOID2 <- as.numeric(df.county$GEOID)
        df.county <- merge(df.county, df2, by = "GEOID2", all.x  = T)
        
    #c) remove Alaska, hawaii, and puerto rico
        df.fips <- unique(fips_codes[,1:2])
        bad.fips <- df.fips$state_code[df.fips$state %in% c("AK", "HI") | df.fips$state_code > 57]
        df.county <- df.county[!df.county$STATEFP %in% bad.fips, ] 
        df.state <- df.state[!df.state$STATEFP %in% bad.fips, ] #<- 

#4) prepare data for graphing
        outlierfun <- function(x,min,max){
            low <- quantile(x, min, na.rm = T)
            high <- quantile(x, max, na.rm = T)
            x <- ifelse(x < low, low, x)
            x <- ifelse(x > high, high, x)
        }
        df.county$mean.ord <- outlierfun(df.county$mean.ord, 0, .97)
        df.county$mean.io <- outlierfun(df.county$mean.ord, 0, .97)
        df.county$mean.first.ord <- outlierfun(df.county$mean.first.ord, 0, .97)
        df.county$mean.first.io <- outlierfun(df.county$mean.first.ord, 0, .97)
        df.county$mean.mme <- outlierfun(df.county$mean.mme, 0, .98)

#5) focus on appelatia
    app.counties <- c("01007","01009","01015","01017","01019","01021","01027","01029","01033","01037","01043","01049","01051","01055","01057","01059","01065","01071","01073","01075","01077","01079","01083","01087","01089","01093","01095","01103","01107","01111","01115","01117","01121","01123","01125","01127","01133","13011","13013","13015","13045","13047","13055","13057","13083","13085","13097","13105","13111","13115","13117","13119","13123","13129","13135","13137","13139","13143","13147","13149","13157","13187","13195","13213","13223","13227","13233","13241","13257","13281","13291","13295","13311","13313","21001","21011","21013","21019","21025","21043","21045","21049","21051","21053","21057","21061","21063","21065","21069","21071","21079","21087","21089","21095","21099","21109","21115","21119","21121","21125","21127","21129","21131","21133","21135","21137","21147","21151","21153","21159","21165","21169","21171","21173","21175","21181","21189","21193","21195","21197","21199","21201","21203","21205","21207","21231","21235","21237","24001","24023","24043","28003","28009","28013","28017","28019","28025","28057","28069","28081","28087","28093","28095","28097","28103","28105","28107","28115","28117","28139","28141","28145","28155","28159","28161","36003","36007","36009","36013","36015","36017","36023","36025","36077","36095","36097","36101","36107","36109","37003","37005","37009","37011","37021","37023","37027","37039","37043","37059","37067","37075","37087","37089","37099","37111","37113","37115","37121","37149","37161","37169","37171","37173","37175","37189","37193","37197","37199","39001","39007","39009","39013","39015","39019","39025","39029","39031","39053","39059","39067","39071","39073","39075","39079","39081","39087","39099","39105","39111","39115","39119","39121","39127","39131","39141","39145","39155","39157","39163","39167","42003","42005","42007","42009","42013","42015","42019","42021","42023","42025","42027","42031","42033","42035","42037","42039","42047","42049","42051","42053","42057","42059","42061","42063","42065","42067","42069","42073","42079","42081","42083","42085","42087","42089","42093","42097","42099","42103","42105","42107","42109","42111","42113","42115","42117","42119","42121","42123","42125","42127","42129","42131","45007","45021","45045","45073","45077","45083","47001","47007","47009","47011","47013","47015","47019","47025","47027","47029","47031","47035","47041","47049","47051","47057","47059","47061","47063","47065","47067","47073","47087","47089","47091","47093","47099","47101","47105","47107","47111","47115","47121","47123","47129","47133","47137","47139","47141","47143","47145","47151","47153","47155","47159","47163","47171","47173","47175","47177","47179","47185","51005","51017","51021","51023","51027","51035","51045","51051","51063","51071","51077","51089","51091","51105","51121","51141","51155","51163","51167","51169","51173","51185","51191","51195","51197","54001","54003","54005","54007","54009","54011","54013","54015","54017","54019","54021","54023","54025","54027","54029","54031","54033","54035","54037","54039","54041","54043","54045","54047","54049","54051","54053","54055","54057","54059","54061","54063","54065","54067","54069","54071","54073","54075","54077","54079","54081","54083","54085","54087","54089","54091","54093","54095","54097","54099","54101","54103","54105","54107","54109")
    app.states <- unique(substr(app.counties,1,2))
    df.county <- df.county[df.county$GEOID2 %in% as.numeric(app.counties),]
    df.county2 <- df.county[as.numeric(df.county$INTPTLAT) < 38.6 & as.numeric(df.county$INTPTLAT) >34.96 & 
                        as.numeric(df.county$INTPTLON) > -86.2 & df.county$STATEFP != "45", ]
    df.county2 <- df.county2[df.county2$GEOID2 %in% as.numeric(app.counties),]
    df.state <- df.state[df.state$GEOID %in% substr(app.counties,1,2),]

    counties <- c(df.county2$county_id)
    counties <- counties[!is.na(counties)]
    fwrite(data.table(counties), "C:/Users/admin/Desktop/counties_to_keep.csv", col.names = F)
    
    
#6) prepare data for mapping        
    mapRange1 <- c(range(st_coordinates(df.county)[,1]),range(st_coordinates(df.county)[,2]))
    df.county2 <- st_union(df.county2)
    df.county2 <- st_union(
                    st_cast(
                       st_cast(
                          st_boundary(st_sfc(df.county2)),
                       "LINESTRING"),
                    "POLYGON")
                   )
    
#6) make function to plot data    
    plotfun<-function(fill.var, fill.name = "", fig.title = ""){
        p <- ggplot()+
            geom_sf(
                data = df.county,
                eval(parse(text = paste("aes(fill = ",fill.var,")",sep = ""))),
                size = 0.05,
                color = "grey50"
            )+
            geom_sf(
                data = df.state,
                size = .2,
                fill = NA,
                color = "black"
            )+
            geom_sf(
                data = df.county2,
                fill = NA,
                size = 2,
                color = "blue"
            )+
            coord_sf(xlim = mapRange1[1:2], ylim = mapRange1[3:4], datum = NA) +
            scale_fill_gradient2(midpoint = median(eval(parse(text = paste("df.county$",fill.var,sep = ""))),na.rm = T),low = "white",mid = "#FFF6F6",high = "#cc0000",na.value = "white")+#,midpoint = 20,limits = c(0,125)
            theme_void() +
            theme(axis.line = element_line(colour = "black"),
                  panel.grid = element_blank(),
                  panel.border = element_blank(),
                  panel.background = element_blank(),
                  plot.title = element_text(hjust = 0.5),
                  text = element_text(family = "serif", face = "bold", size = 11),
                  axis.title = element_blank())+
            labs(fill = fill.name,title = fig.title)
            title2 <- file.path(fig.dir,paste(fig.title,".png",sep = ""))
            ggsave(title2,p,width = 16,height = 9,units = "in")
            browseURL(title2)
        return(p)
    }
    
    
#7) plot data
    p2 <- plotfun(fill.var = "mean.ord", fill.name = "ORD", fig.title = "Mean ORD by County")
    p1 <- plotfun(fill.var = "mean.io", fill.name = "IO", fig.title = "Mean IO by County")
    p4 <- plotfun(fill.var = "mean.first.ord", fill.name = "First ORD", fig.title = "Mean First ORD by County")
    p3 <- plotfun(fill.var = "mean.first.io", fill.name = "First IO", fig.title = "Mean First IO by County")
    
  