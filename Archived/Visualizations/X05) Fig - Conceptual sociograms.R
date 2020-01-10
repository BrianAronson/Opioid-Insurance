#NOTE: This script will no longer work with 2018Q1.fst; I will need to update edgelist paths

#0) prepare workspace
    #a) load libraries
        library(docshop)
        library(igraph)
        library(ggnetwork)
        library(ggraph)

    #b) find local directories
        user <- ifelse(grepl("briarons", getwd()), "briarons",
                ifelse(grepl("bda13", getwd()), "bda13",
                "admin"))
        main.dir <- "hidden"
        fig.dir <- "hidden"
        my.dir <- "hidden"
        
    #b) set new directories
        my.dir <- file.path(main.dir, "PageRank")


#1) load data
    df <- read_fst(file.path(my.dir, "2018Q1.fst"), as.data.table = T)

#1) load pdmp and patient data for q1
    edgl<-s3read_csv("hidden")
    df<-s3read_csv("hidden")
    
#2) find a county with a relatively high number of doctor shoppers
    a<-df[,.(shoppers=sum(nplusnplus90_4),pop=.N),by="county_id"]
    a$shop.prop<-a$shoppers/a$pop
    a<-a[order(a$shop.prop),]
    a[a$shoppers>2,]
    a[a$shoppers>3 & a$pop<1500,]
    a[a$shoppers>2 & a$pop<500,]
    a[a$shoppers==2 & a$pop<100,]
    tmp<-a[a$shoppers>1 & a$pop<500,]$county_id
        #good examples might be county_id 22023 and 51550

#3) subset edgelist and df to good county
    df2<-df[df$county_id == tmp[22],] #9
    # df2<-df[df$county_id == tmp[22],]
    edgl2<-edgl[edgl$PATID %in% df2$PATID,]
    

#4) Finalize preparation of graph object and corresponding edgelist
    #a) remove people only connected to one doctor and doctors only connected to one patient
        temp.table<-data.table(table(edgl2$PATID))
        # temp.edges<-temp.table$V1[temp.table$N>1]
        temp.edges<-temp.table$V1[temp.table$N>0]
        edgl3<-edgl2[as.character(edgl2$PATID) %in% temp.edges,]
    #b) Turn into graph object
        edges<-data.frame(Var1=edgl3$PATID,Var2=edgl3$NPI,value=edgl3$WEIGHT)    
        edges[,1]<-as.character(edges[,1])
        edges[,2]<-as.character(edges[,2])
        g <- graph.data.frame(edges, directed = F)
        V(g)$type <- V(g)$name %in% edges[,1] #the first column of edges is TRUE type
        E(g)$weight <- as.numeric(edges[,3])
    #c) subset data to largest connected component
        dg1 <- decompose.graph(g)
        g1<-dg1[[which.max(sapply(dg1,gsize))]]
    #d) remove doctors only connected to one patient
        # g1<-igraph::delete_vertices(g1,igraph::degree(g1)==1)
        g1<-igraph::delete_vertices(g1,igraph::degree(g1)==1 & substr(V(g1)$name,1,1)=="J")
        onedegree<-igraph::degree(g1)==1
        onedegree[1:4]<-F
        # onedegree[1:length(onedegree)]<-F
        g1<-igraph::delete_vertices(g1,onedegree)
        g1<-igraph::delete_vertices(g1,igraph::degree(g1)==1 & substr(V(g1)$name,1,1)=="J")
        gsize(g1)
        i
        i<-i+1
        
        
    #e) update edglist to correspond to graph
        g1.edges<-edges[edges$Var1 %in% V(g1)$name & edges$Var2 %in% V(g1)$name,]
        #merge current docshopping stats
        g1.edges
        df3<-df2[as.character(df2$PATID) %in% g1.edges$Var1,]
        df3<-df3[,c("PATID","pagerank","birank","degree","bidegree","doc_count","pharm_count")]
        tnames<-V(g1)$name[V(g1)$name %in% as.character(df3$PATID)]
          #already in same order
        V(g1)$name
    #f) update edge names and edglist names
        V(g1)$names<-c(paste("P",1:length(unique(g1.edges$Var1)),sep=""),paste("D",1:length(unique(g1.edges$Var2)),sep=""))
        df.names<-data.frame(Var1=V(g1)$name,nVar1=V(g1)$names)
        g1.edges<-merge(g1.edges,df.names,by="Var1")
        df.names<-data.frame(Var2=V(g1)$name,nVar2=V(g1)$names)
        g1.edges<-merge(g1.edges,df.names,by="Var2")
        g1.edges<-data.frame(Var1=g1.edges$nVar1,Var2=g1.edges$nVar2,weight1=g1.edges$value)
        g1.edges<-g1.edges[order(g1.edges$Var1,g1.edges$Var2),]
    #g) recreate vertex and edge attributes for graphing
        g1<-graph.data.frame(g1.edges, directed = F)    
        V(g1)$type <- V(g1)$name %in% g1.edges[,1] #the first column of edges is TRUE type
        V(g1)$names<-V(g1)$name
        V(g1)$textcols<-ifelse(V(g1)$type,"white","black")
        E(g1)$weight1<-g1.edges$weight1
    #h) save edgelist
        write.fst(g1.edges,file.path(pg.dir,"edgl2.fst"))
        write.csv(g1.edges,file.path(pg.dir,"edgl2.csv"))

        

#5) Prepare weights of one-mode projection
    #a) create one mode projection of graph
        l.ties<-list()
        g1.edges$Var1<-as.character(g1.edges$Var1)
        g1.edges$Var2<-as.character(g1.edges$Var2)
        names(g1.edges)[3]<-"init.weight"
        for(i in 1:nrow(g1.edges)){
            #identify tie
                t.tie<-g1.edges[i,]
            #identify ties of tie
                t.transties<-g1.edges[g1.edges[,2]==t.tie$Var2,]
            #create edges
                t.transties$Var2<-t.transties$Var1
                t.transties$Var1<-t.tie$Var1
                #ties based on counts
                    t.transties$unw.alter.ties<-nrow(t.transties) #tie count
                    t.transties$unw.weight<-1 #unweighted tie
                    t.transties$unw.newman.weight<-t.transties$unw.weight/(t.transties$unw.alter.ties-1) #newman based on tie count
                #ties based on weights
                    # t.transties$w.alter.ties<-t.transties$init.weight #the alter's weight to the doctor
                    # t.transties$w.weight<-t.tie$init.weight #ego's weight to doctor
                    # t.transties$w.trans.ties<-t.transties$w.weight*t.transties$w.alter.ties #transitive weight; the ego's weighted tie to doctor x alter's weighted tie to doctor
                    # t.transties$w.newman.weight<-t.transties$w.trans.ties/(sum(t.transties$w.trans.ties)-1) #newman weight; weight of transitive tie to doctor/number of ties to doctor
                #remove unecessary stuff
                    t.transties$init.weight<-NULL
                    t.transties$unw.alter.ties<-NULL
                    t.transties$w.alter.ties<-NULL
                    t.transties$w.weight<-NULL
            #append to list
                l.ties[[i]]<-t.transties
        }
    #b) append to df
        g2.edges<-rbindlist(l.ties)
    #c) remove self ties
        g2.edges<-g2.edges[g2.edges$Var1!=g2.edges$Var2,]
    #d) aggregate redundant ties
        g3.edges<-data.table(g2.edges)
        g4.edges<-g3.edges[,.(unw.weight=sum(unw.weight),unw.newman.weight=sum(unw.newman.weight)),by=c("Var1","Var2")] #w.trans.ties=sum(w.trans.ties), w.newman.weight=sum(w.newman.weight)
        g2.edges<-as.data.frame(g4.edges)
        
        

        
#6) create symmetric weights for birank
    #a) make data
        # ex<-data.frame(Var1=c("a","b","d","b","c","b","c","d"),Var2=c("x","x","x","y","y","z","z","z"))
        ex<-g1.edges[,c(1,2)]
        names(ex)<-names(ex)[c(2,1)]

    #b) create one mode projected hyperbolic weights for each mode
        #i) var 1's weighted ties to var 2
            l.ties<-list()
            for(i in 1:nrow(ex)){
                #identify tie
                    t.tie<-ex[i,]
                #identify ties of tie
                    t.transties<-ex[ex$Var1==t.tie$Var1,]
                #ties based on counts
                    t.transties$unw.alter.ties<-nrow(t.transties) #tie count
                    t.transties$unw.weight<-1 #unweighted tie
                    t.transties$bi.weight<-t.transties$unw.weight/(t.transties$unw.alter.ties) #newman based on tie count
                #append to list
                    l.ties[[i]]<-t.transties
            }
            #b) append to df
                t.edges<-rbindlist(l.ties)
                temp1<-t.edges
                temp1<-temp1[!duplicated(temp1)]
                temp1<-temp1[,c(1,2,5)]

        #ii) Var2's weighted ties to var 1
            l.ties<-list()
            for(i in 1:nrow(ex)){
                #identify tie
                    t.tie<-ex[i,]
                #identify ties of tie
                    t.transties<-ex[ex$Var2==t.tie$Var2,]
                #ties based on counts
                    t.transties$unw.alter.ties<-nrow(t.transties) #tie count
                    t.transties$unw.weight<-1 #unweighted tie
                    t.transties$bi.weight<-t.transties$unw.weight/(t.transties$unw.alter.ties) #newman based on tie count
                #append to list
                    l.ties[[i]]<-t.transties
            }
            #bind, remove duplicates, and save results to unique
                t.edges<-rbindlist(l.ties)
                temp2<-t.edges
                temp2<-temp2[!duplicated(temp2)]
                temp2<-temp2[,c(1,2,5)]

    #c) make empty ties with weights=0 for null ties
        tdf<-data.frame(Var1=rep(unique(temp1$Var1),each=length(unique(temp1$Var2))),
                        Var2=rep(unique(temp1$Var2),length(unique(temp1$Var1))),
                        bi.weight=0)
        tdf1<-tdf[!paste(tdf$Var1,tdf$Var2) %in% paste(temp1$Var1,temp1$Var2),]
        temp1<-rbind(temp1,tdf1)
        tdf2<-tdf[!paste(tdf$Var1,tdf$Var2) %in% paste(temp2$Var1,temp2$Var2),]
        temp2<-rbind(temp2,tdf2)
        
        temp1$Var1<-as.character(temp1$Var1)
        temp1$Var2<-as.character(temp1$Var2)
        temp2$Var1<-as.character(temp2$Var1)
        temp2$Var2<-as.character(temp2$Var2)
        temp1<-temp1[order(temp1$Var1,temp1$Var2),]
        temp2<-temp2[order(temp2$Var1,temp2$Var2),]
        temp3<-temp1
        temp3$bi.weight2<-temp2$bi.weight

    #d) estimate symetrically weighted ties for a mode projection, based on var2
        #prep output
            tdf<-data.frame(Var1=rep(unique(temp1$Var2),each=length(unique(temp1$Var2))),Var2=rep(unique(temp1$Var2),length(unique(temp1$Var2))),bi.weight=0)
            for(i in 1:nrow(tdf)){
                Var1<-tdf$Var1[i]
                Var2<-tdf$Var2[i]
                tdf$bi.weight[i]<-sum(temp3$bi.weight[temp3$Var2==Var1] * temp3$bi.weight2[temp3$Var2==Var2])
            }
            
#7) merge birank edges into other edges
    g3.edges<-merge(g2.edges,tdf,by=c("Var1","Var2"),all=T)
    g3.edges[is.na(g3.edges)]<-0
    cor(g3.edges[,3:5])
    cor(g3.edges[g3.edges$unw.weight>0,3:5])

#8) Format graph of one-mode projections
    #b) convert to graph
        g2 <- graph.data.frame(g3.edges[g3.edges$unw.weight>0,], directed = T) #the only difference from birank is no self-loops, but this matters a lot for pagerank...
        V(g2)$names<-V(g2)$name
        
    #c) get pageranks based on weighting schemes
        V(g2)$size0<-page.rank(g2)$vector
        V(g2)$size1<-page.rank(g2,weights = E(g2)$unw.weight)$vector
        V(g2)$size2<-page.rank(g2,weights = E(g2)$unw.newman.weight)$vector
        V(g2)$size3<-page.rank(g2,weights = E(g2)$bi.weight)$vector
        # V(g2)$size3<-page.rank(g2,weights = E(g2)$w.trans.ties)$vector
        # V(g2)$size4<-page.rank(g2,weights = E(g2)$w.newman.weight)$vector
        
    #d) rescale vertices and edges
        #i) vertice
            node.rescale<-function(x) x*(10/mean(x))
            V(g2)$size0<-node.rescale(V(g2)$size0)
            V(g2)$size1<-node.rescale(V(g2)$size1)
            V(g2)$size2<-node.rescale(V(g2)$size2)
            V(g2)$size3<-node.rescale(V(g2)$size3)
            # V(g2)$size3<-node.rescale(V(g2)$size3)
            # V(g2)$size4<-node.rescale(V(g2)$size4)
            
        #ii) edge
            edge.rescale<-function(x) x*(1.5/mean(x[x!=0]))
            E(g2)$unw.weight<-edge.rescale(E(g2)$unw.weight)
            E(g2)$unw.newman.weight<-edge.rescale(E(g2)$unw.newman.weight)
            E(g2)$bi.weight<-edge.rescale(E(g2)$bi.weight)
            # E(g2)$w.trans.ties<-edge.rescale(E(g2)$w.trans.ties)
            # E(g2)$w.newman.weight<-edge.rescale(E(g2)$w.newman.weight)
            
            g3 <- graph.data.frame(g3.edges[g3.edges$bi.weight>0,], directed = T) 
            V(g2)$size4<-page.rank(g3,weights = E(g3)$bi.weight)$vector
            V(g2)$size4<-node.rescale(V(g2)$size4)
            
            V(g2)$size5<- node.rescale(V(g2)$size4^3*V(g2)$size3)
            
            cor(V(g2)$size4,V(g2)$size3)
            cor(V(g2)$size4,V(g2)$size2)
            cor(V(g2)$size4,V(g2)$size1)
            cor(V(g2)$size4,V(g2)$size0)

            cor(V(g2)$size3,V(g2)$size2)
            cor(V(g2)$size3,V(g2)$size1)
            cor(V(g2)$size3,V(g2)$size0)
            
            
            V(g2)$o.pagerank<-df3$pagerank
            V(g2)$o.birank<-df3$birank
            V(g2)$o.degree<-df3$degree
            V(g2)$o.bidegree<-df3$bidegree
            V(g2)$o.doc_count<-df3$doc_count
            
            V(g2)$o.birank<-node.rescale(V(g2)$o.birank)
            
            
            tdf<-as.data.frame(vertex.attributes(g2))
            tdf<-tdf[c(-1,-2)]
            tdf[,1:5]<-round(tdf[,1:5],2)
            round(cor(tdf),2)
            round(tdf[,c("size3","size4","size5","o.birank")],2)
            
            cor(V(g2)$o.birank,V(g2)$size4^3*V(g2)$size3)
            cor(V(g2)$o.birank,V(g2)$size3)
            cor(V(g2)$o.birank,V(g2)$size2)
            cor(V(g2)$o.birank,V(g2)$size1)
            

#9) Create one-mode plotting function
    plot.one<-function(title,weight){
        #a) graph
            set.seed(1); p<-
            ggraph(g1) +
            geom_edge_link(color = "grey50",show.legend = F) + 
            geom_node_point(aes(fill=type),color="black",size=10,shape=21)+
            geom_nodetext(aes(x = x, y = y, label=names),color="black")+
            scale_fill_manual(values = c("#f0e130","#add8e6"),labels=c("Doctor","Patient"),name="Node Type")+
            ggtitle(paste(title,"\n",sep=""))+
            theme_blank()+
            theme(text=element_text(family="serif"),
                plot.title = element_text(size=20,face="bold",hjust = 0.5),
                legend.title = element_text(size=13,face="bold"),
                legend.text = element_text(size=11)
        ); p
        #b) save
            ggsave(tsave1 <- paste(fig.dir,title,".png",sep = ""),
                            p, width = 6.5, height = 4, units = "in"
                )
            browseURL(tsave1)
            
        #c) return object
            return(p)
    }

    
#10) create function to plot two mode network
    plot.two<-function(title,w,nsize=8){
        #a) graph
            eval(parse(text=
              paste('set.seed(1); p<-
                ggraph(g2) +
                  geom_edge_link(aes(width=',w,'),color = "grey50",show.legend = F)+
                  geom_node_point(size=',nsize,',fill="#add8e6",color="black",shape=21)+
                  geom_nodetext(aes(x = x, y = y, label=names),color="black")+
                  ggtitle(paste(title,"\n",sep=""))+
                  scale_edge_width(range = c(.5, 2.5))+
                  theme_blank()+
                  theme(text=element_text(family="serif"),
                        plot.title = element_text(size=20,face="bold",hjust = 0.5),
                        legend.title = element_text(size=14,face="bold"),
                        legend.text = element_text(size=11)
                        ); p
                    '
              )
            ))
        
        #b) save
            title2<-gsub("\n","",title)
            ggsave(tsave1 <- paste(fig.dir,title2,".png",sep = ""),
                            p, width = 6.5*1.2, height = 4*1.2, units = "in"
                )
            browseURL(tsave1)
            
        #c) return object
            return(p)
    }

    

    
#11) plot networks
    #a) two-mode network
        pc1<-plot.one(title="Two Mode Doctor-Patient Network",weight="NULL")
        # plot.one("Weighted Two Mode Doctor-Patient Network","weight1")
    
    #b) one mode projection based on unweighted ties
        pc2<-plot.two(title="a) Unweighted One Mode Projection",w = 1,nsize = "V(g2)$size0") #Unweighted Patient Ties Based on One-\nMode Projection of Doctor-Patient Network\n (Nodes scaled to Pagerank)
        pc3<-plot.two(title="b) Count-Weighted Edges",w = "unw.weight",nsize="V(g2)$size1") #Weighted Patient Ties Based on One-\n Mode Projection of Doctor-Patient Network\n (Nodes scaled to Pagerank)
        pc4<-plot.two(title="c) Newman-Weighted Edges",w = "unw.newman.weight",nsize="V(g2)$size2") #Newman Weighted Patient Ties Based on One-\n Mode Projection of Doctor-Patient Network\n (Nodes scaled to Pagerank)
        pc5<-plot.two(title="d) Symmetrically-Weighted Edges",w = "bi.weight",nsize="V(g2)$size5") # Symmetrically Weighted Patient Ties Based on One-\n Mode Projection of Doctor-Patient Network\n (Nodes scaled to Birank)
        
                
        p1<-plot.two(title="a) Unweighted Edges",w = 1,nsize = "V(g2)$size0") #Unweighted Patient Ties Based on One-\nMode Projection of Doctor-Patient Network\n (Nodes scaled to Pagerank)
        p2<-plot.two(title="b) Count-Weighted Edges",w = "unw.weight",nsize="V(g2)$size1") #Weighted Patient Ties Based on One-\n Mode Projection of Doctor-Patient Network\n (Nodes scaled to Pagerank)
        p3<-plot.two(title="c) Newman-Weighted Edges",w = "unw.newman.weight",nsize="V(g2)$size2") #Newman Weighted Patient Ties Based on One-\n Mode Projection of Doctor-Patient Network\n (Nodes scaled to Pagerank)
        p4<-plot.two(title="d) Symmetrically-Weighted Edges",w = "bi.weight",nsize="V(g2)$size5") # Symmetrically Weighted Patient Ties Based on One-\n Mode Projection of Doctor-Patient Network\n (Nodes scaled to Birank)
        # plot.two(title="e) Symmetrically Weighted Patient Ties Based on One-\n Mode Projection of Doctor-Patient Network\n (Nodes scaled to Birank; Excludes self loops)",w = "bi.weight",nsize="V(g2)$size3")
        # plot.two(title="f) Symmetrically Weighted Patient Ties Based on One-\n Mode Projection of Doctor-Patient Network\n (Nodes scaled to Birank)",w = "bi.weight",nsize="V(g2)$size4")
        
        # plot.two(title="h) Symmetrically Weighted Patient Ties Based on One-\n Mode Projection of Doctor-Patient Network\n (Nodes scaled to Real Birank)",w = "bi.weight",nsize="V(g2)$o.birank")
        
    #c) plot 4 projections to grid
        library(cowplot)
        p<-plot_grid(p1,p2,p3,p4,ncol=2)
        title <- ggdraw() + draw_label("One Mode Projections of Doctor-Patient Network", fontface='bold',size=20,fontfamily="serif")
        p<-plot_grid(title, p,nrow=2,rel_heights=c(0.07, 1)) #put title above graph
        p<-plot_grid(p,scale = .98)

        #save
        save_plot("C:/Users/admin/Desktop/One Mode Projections of Doctor-Patient Network.png",p,
                  base_height=8,
                  base_aspect_ratio = 1.6)

    #d) plot bipartite and unweighted projection to grid
        p<-plot_grid(pc1,pc2,ncol=2)
        title2<-"a) Bipartite to One Mode Projection of Doctor-Patient Network"
        title <- ggdraw() + draw_label(title2, fontface='bold',size=20,fontfamily="serif")
        p<-plot_grid(title, p,nrow=2,rel_heights=c(0.12, 1)) #put title above graph
        p<-plot_grid(p,scale = .98)
        save_plot(paste(fig.dir,title2,".png",sep = ""),p,base_height=8, base_aspect_ratio = 2)
        
        p<-plot_grid(pc1,pc3,ncol=2)
        title2<-"b) Bipartite to One Mode Projection of Doctor-Patient Network"
        title <- ggdraw() + draw_label(title2, fontface='bold',size=20,fontfamily="serif")
        p<-plot_grid(title, p,nrow=2,rel_heights=c(0.12, 1)) #put title above graph
        p<-plot_grid(p,scale = .98)
        save_plot(paste(fig.dir,title2,".png",sep = ""),p,base_height=8, base_aspect_ratio = 2)
        
        p<-plot_grid(pc1,pc4,ncol=2)
        title2<-"c) Bipartite to One Mode Projection of Doctor-Patient Network"
        title <- ggdraw() + draw_label(title2, fontface='bold',size=20,fontfamily="serif")
        p<-plot_grid(title, p,nrow=2,rel_heights=c(0.12, 1)) #put title above graph
        p<-plot_grid(p,scale = .98)
        save_plot(paste(fig.dir,title2,".png",sep = ""),p,base_height=8, base_aspect_ratio = 2)
        
        p<-plot_grid(pc1,pc5,ncol=2)
        title2<-"d) Bipartite to One Mode Projection of Doctor-Patient Network"
        title <- ggdraw() + draw_label(title2, fontface='bold',size=20,fontfamily="serif")
        p<-plot_grid(title, p,nrow=2,rel_heights=c(0.12, 1)) #put title above graph
        p<-plot_grid(p,scale = .98)
        save_plot(paste(fig.dir,title2,".png",sep = ""),p,base_height=8, base_aspect_ratio = 2)
        
