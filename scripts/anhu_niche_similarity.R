#function to test for niche similarity (see Warren et al. 2008)
#pts1 & pts2 should be dataframe with columns long & lat (in that order).
#bg1, bg2, bgpred are the background climate grids (must be same CRS as pts1)
#dispersal_dist is the expected dispersal distance in meters (for buffering available habitats)
#clip_poly is a SpatialPolygons object to clip buffered areas - i.e. a coastlines or country bounds shapefile
#nreps is the number of randomized models to run
#in the output ecdf1 and ecdf2 are the quantile of the observed overlap within the null distribution

nicheSimilarity <- function(pts1,pts2,bg1,bg2,bgpred,pred1=NULL,pred2=NULL,dispersal_dist,clip_poly,nreps){
  require(rgeos);require(raster);require(magrittr)
  print("calculating observed niche overlap")
  #get original models and accessible areas
  if(!is.null(pred1)){
    observed_overlap <- nicheOverlap(pred1,pred2)[1]
  } else {
    model1 <- maxent(bg1,pts1)
    model2 <- maxent(bg2,pts2)
    pred1 <- predict(model1,bgpred)
    pred2 <- predict(model2,bgpred)
    observed_overlap <- nicheOverlap(pred1,pred2)[1]
  }
  
  #get possible areas by buffering a convex hull and clipping to coastlines (or other clip poly)
  print("starting randomization tests")
  area1 <- pts1[chull(pts1),] %>% Polygon() %>% list() %>% Polygons(1) %>% list() %>% SpatialPolygons(proj4string = crs(bg1))
  area2 <- pts2[chull(pts2),] %>% Polygon() %>% list() %>% Polygons(1) %>% list() %>% SpatialPolygons(proj4string = crs(bg1))
  area1 <- spTransform(area1,"+proj=utm +zone=19 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  area2 <- spTransform(area2,"+proj=utm +zone=19 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  area1 <- gBuffer(area1,width=dispersal_dist)
  area2 <- gBuffer(area2,width=dispersal_dist)
  area1 <- spTransform(area1,crs(bg1))
  area2 <- spTransform(area2,crs(bg1))
  area1 <- intersect(area1,clip_poly)
  area2 <- intersect(area2,clip_poly)
  
  #run randomization tests in both directions
  null1 <- c();null2 <- c()
  for(i in 1:nreps){
    rpts1 <- spsample(area1,nrow(pts2),"random")
    print(paste(i))
    rmodel1 <- maxent(bg1,rpts1)
    rpred1 <- predict(rmodel1,bgpred)
    null1[i] <- nicheOverlap(pred2,rpred1)[1]
    rpts2 <- spsample(area2,nrow(pts1),"random")
    rmodel2 <- maxent(bg2,rpts2)
    rpred2 <- predict(rmodel2,bgpred)
    null2[i] <- nicheOverlap(pred1,rpred2)
  }
  ecdf1 <- ecdf(null1)(observed_overlap)
  ecdf2 <- ecdf(null2)(observed_overlap)
  return(list(obs=observed_overlap,null1=null1,null2=null2,quantile1=ecdf1,quantile2=ecdf2))
}


