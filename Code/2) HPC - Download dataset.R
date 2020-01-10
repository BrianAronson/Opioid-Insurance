#0) Prepare workspace
    #a) load libraries
        # library(docshop)
        library(aws.s3)
        
    #b) identify directories
        s3.dir <- file.path("hidden","additional_data","BDA")
        s3reg.dir <- file.path(s3.dir, "PageRank")
        s3object.dir <- file.path("additional_data", "BDA", "PageRank")
        hpc.dir <- file.path("hidden","additional_data","BDA")
        pr.dir <- file.path(hpc.dir, "PageRank.sub")

        
#1) Save files to hpc
    #a) make function for identifying s3 files
        s3.list.files <- function(x, full.names=F, max.files=1000){
          #1) make function for finding extracting bucket name (in case included in x)
              prefix.fun <- function(x) gsub("hidden", "", x)
          #2) find files in bucket
              bucket.path <- get_bucket("hidden", max = max.files, prefix = prefix.fun(x))
          #3) extract file names
              key.names <- as.vector(sapply(bucket.path, function(x) x[[1]]))
          #4) remove folder names from key.names
              key.names <- key.names
              key.names <- key.names[!grepl("/$", key.names)]
          #5) create versions of file names including full path and excluding full path
              full.return.names <- file.path("hidden", key.names)
              short.return.names <- gsub("(.*/\\s*(.*$))", "\\2", key.names)
          #6) return file names according to whether full.names set to TRUE
              if(full.names){
                   return(full.return.names)
              }else{
                  return(short.return.names)
              }
        }
        
    #b) identify files to load
        reg.files <- s3.list.files(s3reg.dir, full.names = T)
        reg.files <- reg.files[grepl("reg", reg.files)]
        reg.files <- reg.files[grepl("fst", reg.files)]
        reg.files <- reg.files[!grepl(".df", reg.files)]
        reg.files <- reg.files[!grepl("sorted", reg.files)]
        reg.files <- reg.files[1:13]

        
    #c) convert to file names and new names that are compatible with save_object
        s3.files <- gsub("hidden", "", reg.files)
        local.files <- gsub("additional_data/BDA/", "", s3.files)
        local.files <- file.path(hpc.dir, local.files)


    #d) Save each file to the hpc
        for(i in 1:length(reg.files)){
            save_object(file = local.files[i], object = s3.files[i], bucket = "hidden")
            print(i)
        }
        