
site_hrclust_mst<-function(x, saveDirectory, alpha=0.4, n_clust=(round(nrow(x) / 200))){
  library(igraph)
  library(spdep)
  library(adespatial)
   library(sfnetworks)
  library(igraph)
  library(tidygraph)
  library(spdep)
  
  if (any(is.na(x$Depth_standard)==TRUE)){
    x$Depth_standard[is.na(x$Depth_standard)==TRUE]<-mean(x$Depth_standard,na.rm=TRUE)
  }
  if (any(is.na(x$Depth)==TRUE)){
    x$Depth[is.na(x$Depth)==TRUE]<-mean(x$Depth,na.rm=TRUE)
  }
  x$ID<-1:nrow(x)
  

  #H3 hexagon average size
#   hex_size<-data.frame(Res=c(7:15),
#                        Size=c(5161293,737327,105332,15047,2149,307.09,43.87,6.267,0.895))
  
#   MinCounts<-round(site_size/hex_size$Size[hex_size$Res==resolution])
  
#   n_clust <- round(nrow(x) / MinCounts)
    
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
    
    Network_withEdgesTri<-Network_withEdgesTri %>%
      activate("edges") %>%
      mutate(length = edge_length())
    Length_m<-as.numeric(E(Network_withEdgesTri)$length)
    Length_scaled<-scale(as.numeric(E(Network_withEdgesTri)$length))
    Weight_scaled<-scale(as.numeric(E(Network_withEdgesTri)$weight))
    
    depth_weight <- alpha
    xy_weight <- 1 - alpha

    Eucliden_weight_old<-sqrt((xy_weight * Length_scaled)^2+(depth_weight * Weight_scaled)^2)
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
    
    # 2. Extract the two-column matrix for the 'links' argument
    # 'id' is the 'from' node, 'id_neigh' is the 'to' node.
    links_matrix_mst <- as_edgelist(mst_tri)

    depth_data <- x$Depth_standard

    D_depth <- dist(x$Depth_standard, method="manhattan")
    # 1. Calculate the Geographic Distance Matrix (D_Geo)
    D_geo <- dist(st_drop_geometry(x[, c("X_standard", "Y_standard")]))

    # 3. Create the Combined Dissimilarity Matrix
    D_combined <- depth_weight * D_depth + D_geo * xy_weight

    res_hclust <- constr.hclust(
        d = D_combined,
        method = "flexible",
        beta = -1,
        links = links_matrix_mst,
        coords = x[, c("X_standard", "Y_standard")]
    )
    hclust_sites <- cutree(res_hclust, k = n_clust)
    hclust_sites <- as.factor(paste(x$Reef[1],x$habitat[1], hclust_sites,sep="_"))
    # png(file = paste(saveDirectory,"/maps/",unique(x$Reef),"_",unique(x$habitat),"_clusters_larger.png",sep=""), width = 2000, height = 2500)
    # plot((x %>% mutate(clus = clus10_tri$groups))['clus'], main = paste("Cluster number equals ",length(table(clus10_tri$groups)),sep=""))
    # dev.off()

    x$hclust_site_id=hclust_sites
    x$npixels <- nrow(x)
  
  return(x)
}

site_hrclust<-function(x, saveDirectory, alpha=0.4, beta=-1, n_clust=(round(nrow(x) / 200))){
  library(igraph)
  library(spdep)
  library(adespatial)
  
  if (any(is.na(x$Depth_standard)==TRUE)){
    x$Depth_standard[is.na(x$Depth_standard)==TRUE]<-mean(x$Depth_standard,na.rm=TRUE)
  }
  if (any(is.na(x$Depth)==TRUE)){
    x$Depth[is.na(x$Depth)==TRUE]<-mean(x$Depth,na.rm=TRUE)
  }
  x$ID<-1:nrow(x)
  

  #H3 hexagon average size
#   hex_size<-data.frame(Res=c(7:15),
#                        Size=c(5161293,737327,105332,15047,2149,307.09,43.87,6.267,0.895))
  
#   MinCounts<-round(site_size/hex_size$Size[hex_size$Res==resolution])
  
#   n_clust <- round(nrow(x) / MinCounts)
    
    coords <- st_centroid(st_geometry(x))

    # k_neighbor_list <- knearneigh(coords, k = 7)
    # links_knn <- knn2nb(k_neighbor_list)
    # 1. Create the data frame of connected indices
    tri<-tri2nb(coords)
    library(expp)
    Edges_tri<-neighborsDataFrame(tri)
    detach("package:expp", unload = TRUE) #AKC - I am needing to do this before expp and raster have conflicting functions
    
    # 2. Extract the two-column matrix for the 'links' argument
    # 'id' is the 'from' node, 'id_neigh' is the 'to' node.
    links_matrix_expp <- as.matrix(Edges_tri[, c("id", "id_neigh")])

    depth_data <- x$Depth_standard

    # 2. Get the adjacency list from the KNN object
    # This lists all neighbors for each site
    adj_list <- links_matrix_expp

    # 3. Create a vector to store the smoothed depth values
    smoothed_depth <- numeric(length(depth_data))

    # 4. Loop through each site to calculate the local average depth
    for (i in 1:length(depth_data)) {
        # Get the indices of the neighbors for the current site i
        neighbors <- adj_list[[i]] 
        
        # Include the site itself in the averaging
        local_indices <- c(i, neighbors)
        
        # Calculate the mean depth across the site and its neighbors
        smoothed_depth[i] <- mean(depth_data[local_indices], na.rm = TRUE)
    }

    d_attributes_manhattan <- dist(x$Depth_standard, method="manhattan")
    # 1. Calculate the Geographic Distance Matrix (D_Geo)
    D_Geo <- dist(st_drop_geometry(x[, c("X_standard", "Y_standard")]))

    # 3. Create the Combined Dissimilarity Matrix
    D_combined <- alpha * d_attributes_manhattan + D_Geo * (1 - alpha)

    links_matrix_knn <- as.matrix(links_matrix_expp)
    res_hclust <- constr.hclust(
        d = D_combined,
        method = "flexible",
        beta = beta,
        links = links_matrix_knn,
        coords = x[, c("X_standard", "Y_standard")]
    )
    hclust_sites <- cutree(res_hclust, k = n_clust)
    hclust_sites <- as.factor(paste(x$Reef[1],x$habitat[1], hclust_sites,sep="_"))
    # png(file = paste(saveDirectory,"/maps/",unique(x$Reef),"_",unique(x$habitat),"_clusters_larger.png",sep=""), width = 2000, height = 2500)
    # plot((x %>% mutate(clus = clus10_tri$groups))['clus'], main = paste("Cluster number equals ",length(table(clus10_tri$groups)),sep=""))
    # dev.off()

    x$hclust_site_id=hclust_sites
    x$npixels <- nrow(x)
  
  return(x)
}