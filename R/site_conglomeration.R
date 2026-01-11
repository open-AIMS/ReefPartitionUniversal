site_conglomeration <- function(site_polygons, min_site_area) {
    site_polygons$area <- st_area(site_polygons)

    RowsToRemove<-c()
    ExtraSites<-c("a","b","c","d","e","f")
    NewSites<-site_polygons[1,] #data.frame("site_id","habitat","area","UNIQUE_ID","Reef","geometry")
    site_polygons_crs= sf::st_crs(site_polygons)

    for (i in 1:nrow(site_polygons)){
        #print(i)
        if (as.numeric(site_polygons$area[i]) < min_site_area*307/10^6){ #Removes sites that are smaller than a minimum threshold (50 hexagons * 307m²/10⁶) #that 50 is now parameter, use parameter name instead
        RowsToRemove<-c(RowsToRemove,i)
        } else {
        if (class(site_polygons$geometry[i])[1]=="sfc_MULTIPOLYGON"){ #Processing Multi-polygons:
            #When a site consists of multiple polygons (sfc_MULTIPOLYGON)
            #Separates multi-polygons into individual polygons
            #Assigns new IDs using letters (a,b,c,d,e,f)
            #Creates new rows for each separated polygon
            NewPolygons<-PostProcessing(polygon=site_polygons[i,],min_site_area,site_polygons_crs)
            
            RowsToRemove<-c(RowsToRemove,i)
        
            NewRows<-site_polygons[i,]%>% slice(rep(1:n(), each = nrow(NewPolygons)))
            for (m in 1:nrow(NewPolygons)){
            NewRows$geometry[m]<-NewPolygons$geometry[m]
            }
            
            NewRows$site_id<-paste0(NewRows$site_id,ExtraSites[1:nrow(NewPolygons)])
            
            NewSites<-rbind(NewSites,NewRows)
        }
        }
        
    }
    site_polygons<-site_polygons[-RowsToRemove,]
    NewSites<-NewSites[-1,]
    site_polygons<-rbind(site_polygons,NewSites)
}

PostProcessing<-function(polygon, min_site_area=50,site_polygons_crs=4326){
  
  PolygonSeperate<-data.frame(index=1:length(polygon$geometry[[1]]))
  NewPolygons<-data.frame(index=1)
  NumberPolygons<-1
  PolygonSeperate$area<-NA
  PolygonSeperate$geometry<-NA
  NewPolygons$geometry<-NA
  PolygonSeperate$Number<-NA
  NewPolygons$area<-NA

    # Separate the polygons that are contained in the target multipolygon feature
    # into individual polygon elements in a data frame.
  for (lists in 1:length(polygon$geometry[[1]])){
    PolygonSeperate$geometry[lists]<-st_sfc(st_polygon(polygon$geometry[[1]][[lists]]))%>% 
      sf::st_set_crs(site_polygons_crs)
    PolygonSeperate$area[lists]<-nrow(polygon$geometry[[1]][[lists]][[1]])
  }
  
  while(nrow(PolygonSeperate)>0){
    LargestIndex<-which(PolygonSeperate$area==max(PolygonSeperate$area))[1]
    Dist<-NA
    for (parts in 1:nrow(PolygonSeperate)){
      Dist[parts]<-100000*st_distance(PolygonSeperate$geometry[[LargestIndex]],PolygonSeperate$geometry[[parts]])
    }
    
    if (any(Dist[-LargestIndex]<100)){
      #combine polygons into multipolygon
      Indices<-which(Dist<100)
      Multi<-st_union(st_sfc(PolygonSeperate$geometry[Indices]))
      NewPolygons[NumberPolygons,]<-NumberPolygons
      NewPolygons$geometry[NumberPolygons]<-Multi
      NewPolygons$area[NumberPolygons]<-sum(PolygonSeperate$area[Indices])
      PolygonSeperate<-PolygonSeperate[-Indices,]
      NumberPolygons<-NumberPolygons+1
    } else {
      NewPolygons[NumberPolygons,]<-NumberPolygons
      NewPolygons$geometry[NumberPolygons]<-PolygonSeperate$geometry[LargestIndex]
      NewPolygons$area[NumberPolygons]<-PolygonSeperate$area[LargestIndex]
      PolygonSeperate<-PolygonSeperate[-LargestIndex,]
      NumberPolygons<-NumberPolygons+1
    }
  }
  
  NewPolygons<-NewPolygons[which(NewPolygons$area>min_site_area),] #minmum polygon size to filter with
  
  return(NewPolygons)
}
