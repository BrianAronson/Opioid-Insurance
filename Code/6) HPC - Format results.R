#0) Prepare workspace
    #a) load libraries
        library(fst)
        library(data.table)
        library(ggplot2)
        library(cowplot)
        
    #b) find local directories
        hpc.dir <- file.path("hidden","additional_data","BDA")
        form.dir <- file.path(hpc.dir, "4_PageRank_formulas")
        mod.dir <- file.path(hpc.dir, "5_PageRank_models")
        fig.dir <- file.path(hpc.dir, "6_PageRank_figures"); dir.create(fig.dir, showWarnings = F)
        
#1) Read data
    #a) read f.index
        f.index <- readRDS(file.path(form.dir, "df.rds"))
    #b) read models
        #find files
            all.files <- list.files(mod.dir, full.names = T)
        #sort file names
            ind <- strsplit(all.files, "results")
            ind <- sapply(ind, function(x) x[[2]])
            ind <- gsub("[.rds]", "",ind)
            ind <- as.numeric(ind)
            all.files <- all.files[order(ind)]
        #read the files
            l.mod <- list()
            for(i in 1:length(all.files)){
                l.mod[[i]] <- readRDS(all.files[i])
                print(i)
            }
    #c) put model results in f.index
        f.index <- f.index[1:length(all.files), ]
        f.index$aic <- sapply(l.mod, function(x) x[[2]])
        f.index$est <- sapply(l.mod, function(x) x[[3]])
        f.index$model <- lapply(l.mod, function(x) x[[1]])
    #d) remove unecessary info from f.index
        f.index[, c("full.formula", "all.variables") := NULL]
    #e) add necessary info
        f.index$anyweight <- ifelse(f.index$iv.weight == "",0,1)
        f.index$rawvar <- ifelse(grepl("mme|count", f.index$IV), 0, 1)
        f.index$IV <- gsub("iv\\.", "", f.index$IV)
        f.index$DV <- gsub("dv\\.", "", f.index$DV)
        f.index$DV <- toupper(f.index$DV)
        

#2) Graph results
    #a) Make function to create figures based on various model parameters
      f.graph <- function(f.DV, f.anyweight, f.n.controls, f.form, f.rawvar, browse = F){
          #i) prep data
              title <- paste("Model = ", f.form, "; DV = ", f.DV , sep = "")
              if(f.rawvar == 1) df <- f.index[DV == f.DV & anyweight == f.anyweight & n.controls == f.n.controls & form == f.form & rawvar == f.rawvar, ]
              if(f.rawvar == 0) df <- f.index[DV == f.DV & anyweight == f.anyweight & n.controls == f.n.controls & form == f.form, ]
              rows <- min(c(nrow(df), 10))
              df <- df[order(aic), ][1:rows, ]
          #ii) graph results
              p <- ggplot(df, aes(x = reorder(IV, aic), y = aic)) +
                geom_point(stat="identity", size = 2.5) +
                geom_text(size = 5, aes(label=ifelse(sign(as.numeric(df$est))==1, "+", "-")), position=position_dodge(width=0.9), vjust=-0.5) +
                theme_bw() +
                theme(axis.text = element_text(angle = 45, hjust = 1, size = 10),
                  axis.text.x = element_text(angle = 45, hjust = 1)) +
                labs(x = "", y = "") +
                ggtitle(title)
              if(browse) browseURL(tsave1)
              return(p)
      }

  #b) Create figures from simplest models
      #i) Create individual figures
          p1 <- f.graph(f.DV = "IO", f.anyweight = 0, f.n.controls = 0, f.form = "cox", f.rawvar = 1)
          p2 <- f.graph(f.DV = "IO", f.anyweight = 0, f.n.controls = 0, f.form = "glmer", f.rawvar = 1)
          p3 <- f.graph(f.DV = "IO", f.anyweight = 0, f.n.controls = 0, f.form = "plm", f.rawvar = 1)
          p4 <- f.graph(f.DV = "ORD", f.anyweight = 0, f.n.controls = 0, f.form = "cox", f.rawvar = 1)
          p5 <- f.graph(f.DV = "ORD", f.anyweight = 0, f.n.controls = 0, f.form = "glmer", f.rawvar = 1)
          p6 <- f.graph(f.DV = "ORD", f.anyweight = 0, f.n.controls = 0, f.form = "plm", f.rawvar = 1)
          p7 <- f.graph(f.DV = "MME.LOG", f.anyweight = 0, f.n.controls = 0, f.form = "cox", f.rawvar = 1)
          p8 <- f.graph(f.DV = "MME.LOG", f.anyweight = 0, f.n.controls = 0, f.form = "glmer", f.rawvar = 1)
          p9 <- f.graph(f.DV = "MME.LOG", f.anyweight = 0, f.n.controls = 0, f.form = "plm", f.rawvar = 1)
      #ii) append individual figures to one graph
          p <- plot_grid(p1,p2,p3,p4,p5,p6,p7,p8,p9,nrow=3)
          save_plot(file.path(fig.dir,"Best Fitting 10 Models for IV (no c.network, unweighted).png"),p,
                    base_height=8,
                    base_aspect_ratio = 1.5)
          browseURL(file.path(fig.dir,"Best Fitting 10 Models for IV (no c.network, unweighted).png"))
          dev.off()

  #c) Create figures from models with network controls  
      #i) Create individual figures
          p1 <- f.graph(f.DV = "IO", f.anyweight = 0, f.n.controls = 1, f.form = "cox", f.rawvar = 1)
          p2 <- f.graph(f.DV = "IO", f.anyweight = 0, f.n.controls = 1, f.form = "glmer", f.rawvar = 1)
          p3 <- f.graph(f.DV = "IO", f.anyweight = 0, f.n.controls = 1, f.form = "plm", f.rawvar = 1)
          p4 <- f.graph(f.DV = "ORD", f.anyweight = 0, f.n.controls = 1, f.form = "cox", f.rawvar = 1)
          p5 <- f.graph(f.DV = "ORD", f.anyweight = 0, f.n.controls = 1, f.form = "glmer", f.rawvar = 1)
          p6 <- f.graph(f.DV = "ORD", f.anyweight = 0, f.n.controls = 1, f.form = "plm", f.rawvar = 1)
          p7 <- f.graph(f.DV = "MME.LOG", f.anyweight = 0, f.n.controls = 1, f.form = "cox", f.rawvar = 1)
          p8 <- f.graph(f.DV = "MME.LOG", f.anyweight = 0, f.n.controls = 1, f.form = "glmer", f.rawvar = 1)
          p9 <- f.graph(f.DV = "MME.LOG", f.anyweight = 0, f.n.controls = 1, f.form = "plm", f.rawvar = 1)
      #ii) append individual figures to one graph
          p <- plot_grid(p1,p2,p3,p4,p5,p6,p7,p8,p9,nrow=3)
          save_plot(file.path(fig.dir,"Best Fitting 10 Models for IV (yes c.network, unweighted).png"),p,
                    base_height=8,
                    base_aspect_ratio = 1.5)
          browseURL(file.path(fig.dir,"Best Fitting 10 Models for IV (yes c.network, unweighted).png"))
          dev.off()
          
      
  #d) Create figures from models with node weights but no network controls
      #i) Create individual figures
          p1 <- f.graph(f.DV = "IO", f.anyweight = 1, f.n.controls = 0, f.form = "cox", f.rawvar = 0)
          p2 <- f.graph(f.DV = "IO", f.anyweight = 1, f.n.controls = 0, f.form = "glmer", f.rawvar = 0)
          p3 <- f.graph(f.DV = "IO", f.anyweight = 1, f.n.controls = 0, f.form = "plm", f.rawvar = 0)
          p4 <- f.graph(f.DV = "ORD", f.anyweight = 1, f.n.controls = 0, f.form = "cox", f.rawvar = 0)
          p5 <- f.graph(f.DV = "ORD", f.anyweight = 1, f.n.controls = 0, f.form = "glmer", f.rawvar = 0)
          p6 <- f.graph(f.DV = "ORD", f.anyweight = 1, f.n.controls = 0, f.form = "plm", f.rawvar = 0)
          p <- plot_grid(p1,p2,p3,p4,p5,p6,nrow=2)
      #ii) append individual figures to one graph
          save_plot(file.path(fig.dir,"Best Fitting 10 Models for IV (no c.network, weighted).png"),p,
                    base_height=8,
                    base_aspect_ratio = 1.5)
          browseURL(file.path(fig.dir,"Best Fitting 10 Models for IV (no c.network, weighted).png"))
          dev.off()
          
          
  #e) Create figures from models with node weights and network controls
      #i) Create individual figures
          p1 <- f.graph(f.DV = "IO", f.anyweight = 1, f.n.controls = 1, f.form = "cox", f.rawvar = 0)
          p2 <- f.graph(f.DV = "IO", f.anyweight = 1, f.n.controls = 1, f.form = "glmer", f.rawvar = 0)
          p3 <- f.graph(f.DV = "IO", f.anyweight = 1, f.n.controls = 1, f.form = "plm", f.rawvar = 0)
          p4 <- f.graph(f.DV = "ORD", f.anyweight = 1, f.n.controls = 1, f.form = "cox", f.rawvar = 0)
          p5 <- f.graph(f.DV = "ORD", f.anyweight = 1, f.n.controls = 1, f.form = "glmer", f.rawvar = 0)
          p6 <- f.graph(f.DV = "ORD", f.anyweight = 1, f.n.controls = 1, f.form = "plm", f.rawvar = 0)
      #ii) append individual figures to one graph
          p <- plot_grid(p1,p2,p3,p4,p5,p6,nrow=2)
          save_plot(file.path(fig.dir,"Best Fitting 10 Models for IV (yes c.network, weighted).png"),p,
                    base_height=8,
                    base_aspect_ratio = 1.5)
          browseURL(file.path(fig.dir,"Best Fitting 10 Models for IV (yes c.network, weighted).png"))
          dev.off()
          

          