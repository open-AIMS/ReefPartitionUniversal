
site_clust3<-function(x, saveDirectory){
  library(sfnetworks)
  library(igraph)
  library(tidygraph)
  library(spdep)
  library(raster)
  
  if (any(is.na(x$Depth_standard)==TRUE)){
    x$Depth_standard[is.na(x$Depth_standard)==TRUE]<-mean(x$Depth_standard,na.rm=TRUE)
  }
  if (any(is.na(x$Depth)==TRUE)){
    x$Depth[is.na(x$Depth)==TRUE]<-mean(x$Depth,na.rm=TRUE)
  }
  x$ID<-1:nrow(x)
  
  #H3 hexagon average size
  hex_size<-data.frame(Res=c(7:15),
                       Size=c(5161293,737327,105332,15047,2149,307.09,43.87,6.267,0.895))
  
  MinCounts<-round(site_size/hex_size$Size[hex_size$Res==resolution])
  
  n_clust <- round(nrow(x) / MinCounts)
  
  #MinCounts<-200  #How many hexagons as a minimum resolution (hexagon=307m2)
  Extrapolation<-FALSE
  
  if (nrow(x)>10000){
    Extrapolation<-TRUE
    SamplePoints<-sample(c(1:nrow(x)),10000)
    x_old<-x
    x<-x[SamplePoints,]
    
    MinCounts<-MinCounts*(10000/nrow(x_old))
    n_clust <- round(nrow(x_old) / MinCounts)
  } 
  if (nrow(x)<1.5*MinCounts){
    x$site_id<-as.factor(paste(x$Reef[1],x$habitat[1],1,sep="_"))
  } else {
    
    coords <- st_centroid(st_geometry(x))
    
    
    #################################
    #### Step 1: Create a graph #####
    #################################
    
    
    # Using triangulation
    tri<-tri2nb(coords)
    Costs_tri<-nbcosts(tri,data = data.frame(Depth=x$Depth),method="manhattan")#,X_standard=x$X_standard,Y_standard=x$Y_standard))
    Costs_tri<-unlist(Costs_tri)
    
    library(expp)
    Edges_tri<-neighborsDataFrame(tri)
    detach("package:expp", unload = TRUE) #AKC - I am needing to do this before expp and raster have conflicting functions
    
    Edges_tri2<-data.frame(from=as.numeric(Edges_tri$id),to=as.numeric(Edges_tri$id_neigh),weights=Costs_tri)
    Network_withEdgesTri<-sfnetwork(x,Edges_tri2[,c(1,2)],directed=FALSE)
    E(Network_withEdgesTri)$weight<-Costs_tri
    
    png(file = paste(saveDirectory,"/maps/",unique(x$Reef),"_",unique(x$habitat),"_network_larger.png",sep=""), width = 2000, height = 2500)
    plot(Network_withEdgesTri)
    dev.off()
    
    
    Network_withEdgesTri<-Network_withEdgesTri %>%
      activate("edges") %>%
      mutate(length = edge_length())
    Length_m<-as.numeric(E(Network_withEdgesTri)$length)
    Length_scaled<-scale(as.numeric(E(Network_withEdgesTri)$length))
    Weight_scaled<-scale(as.numeric(E(Network_withEdgesTri)$weight))
    Euclidean_weight<-NA
    for (i in 1:length(Length_m)){
      if (Length_m[i]>30){
        Euclidean_weight[i]<-sqrt(20*Length_scaled[i,1]^2+Weight_scaled[i,1]^2)
      } else {
        Euclidean_weight[i]<-sqrt(Length_scaled[i,1]^2+Weight_scaled[i,1]^2)
      }
    }
    
    Eucliden_weight_old<-sqrt(Length_scaled^2+Weight_scaled^2)
    #Eucliden_weight_old<-sqrt(Length_scaled^2+0.001*Weight_scaled^2) #enable if you want to ignore depth
    E(Network_withEdgesTri)$weight<-Eucliden_weight_old
    
    # Edges_tri2<-data.frame(from=as.numeric(Edges_tri_new$id),to=as.numeric(Edges_tri_new$id_neigh),weights=Costs_tri[as.numeric(E(Network_withEdgesTri)$length)<1000])
    # Network_withEdgesTri<-sfnetwork(x,Edges_tri2[,c(1,2)],directed=FALSE)
    # E(Network_withEdgesTri)$weight<-Edges_tri2[,3]
    # 
    # #png(file = paste("maps/",unique(x$Reef),"_",unique(x$habitat),"_network.png",sep=""), width = 2000, height = 2500)
    # plot(Network_withEdgesTri)
    # #dev.off()
    
    #################################################
    #### Step 3: Create a spanning tree/ forest #####
    #################################################
    
    #Triangulation
    mst_tri<-mst(Network_withEdgesTri,weights=E(Network_withEdgesTri)$weight)#+Length_scaled)
    mst_tri2<-as_sfnetwork(mst_tri)
    
    png(file = paste(saveDirectory,"/maps/",unique(x$Reef),"_",unique(x$habitat),"_SpanningTree_larger.png",sep=""), width = 2000, height = 2500)
    plot(mst_tri2)
    dev.off()
    
    #############################
    #### Step 4: Clustering #####
    #############################
    
    num_cores <- parallel::detectCores(logical = FALSE) - 2L
    spdep::set.coresOption(num_cores)
    spdep::set.mcOption(FALSE)

    # 1. Create the cluster using the number of cores set in Step 1
    cl <- parallel::makeCluster(spdep::get.coresOption()) 

    # 2. Tell spdep to use this specific cluster object
    spdep::set.ClusterOption(cl)

    #triangulation
    clust_start_time <- Sys.time()
    clus10_tri <- skater(edges = as_edgelist(mst_tri), data = data.frame(Depth_standard=x$Depth_standard), ncuts=n_clust) # this seems quite intensive in terms of time
    clust_end_time <- Sys.time()
    
    spdep::set.ClusterOption(NULL)
    parallel::stopCluster(cl)

    sites <- as.factor(paste(x$Reef[1],x$habitat[1],clus10_tri$groups,sep="_"))

    # k_neighbor_list <- knearneigh(coords, k = 7)
    # links_knn <- knn2nb(k_neighbor_list)
    # links_matrix_knn <- as.matrix(expp::neighborsDataFrame(links_knn)[, c("id", "id_neigh")])
    # res_hclust <- constr.hclust(
    #     d = dist(x$Depth_standard),
    #     method = "flexible",
    #     beta = -1,
    #     links = links_matrix_knn,
    #     coords = x[, c("X_standard", "Y_standard")]
    # )
    # hclust_sites <- cutree(res_hclust, k = n_clust)
    # hclust_sites <- as.factor(paste(x$Reef[1],x$habitat[1], hclust_sites,sep="_"))
    # # png(file = paste(saveDirectory,"/maps/",unique(x$Reef),"_",unique(x$habitat),"_clusters_larger.png",sep=""), width = 2000, height = 2500)
    # # plot((x %>% mutate(clus = clus10_tri$groups))['clus'], main = paste("Cluster number equals ",length(table(clus10_tri$groups)),sep=""))
    # # dev.off()
    
    # redcap_clust <- scl_redcap(st_drop_geometry(x[, c("X_standard", "Y_standard")]), dist(x$Depth_standard), n_clust)
    
    
    if (Extrapolation==TRUE){
      skater_sites <- class::knn(data.frame(x)[,c("X_standard","Y_standard")], 
                          data.frame(x_old)[,c("X_standard","Y_standard")], sites)
        # hclust_sites <- class::knn(data.frame(x)[,c("X_standard","Y_standard")], 
        #                   data.frame(x_old)[,c("X_standard","Y_standard")], hclust_sites)
      x<-x_old
    }
    
    x$skater_site_id=skater_sites
    # x$hclust_site_id=hclust_sites
    x$site_id = x$skater_site_id
    x$hab_skater_time <- clust_end_time - clust_start_time
    x$npixels <- nrow(x)
  }
  
  return(x)
}