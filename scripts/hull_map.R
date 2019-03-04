library(rgeos);library(concaveman);library(raster);library(magrittr)

clip_poly <- shapefile("~/Documents/worldclim/country_outlines/cntry06/cntry06.shp") %>% crop(extent(-155,-87,22,61.5))
dispersal_dist <- 1e5
first <- T
ranges <- data.frame(long=numeric(),lat=numeric(),order=integer(),hole=logical(),piece=character(),group=character(),maxyear=numeric(),polygon=numeric())
combo2 <- subset(combo,month %in% c(12,1,2,3) & source != "eBird")
year_breaks <- c(1940,1950,1960,1970,1980,1990,2000,2010)
for(i in year_breaks){
  pts1 <- subset(combo2,year<=i)[,c("long","lat")] %>% 
           SpatialPoints(proj4string = crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")) %>% 
            crop(extent(-155,-85,22,61.5))
  
  #cluster points at ~500km distance and drop clusters with <= 10 individual reports (typically 5 years of CBC records)
  dists <- as.dist(spDists(pts1,longlat = T))
  clust <- hclust(dists)
  clust <- cutree(clust,h=200)
  pts <- as.data.frame(pts1)
  pts$clust <- clust
  inds_per_clust <- ddply(pts,.(clust),summarize,ninds=length(long))
  clusts_to_keep <- subset(inds_per_clust,ninds >= 10)
  pts <- subset(pts,clust %in% clusts_to_keep$clust)
  
  #cluster remaining points with 1000km distance and estimate concave hulls
  pts$clust2 <- pts[,-3] %>% as.matrix() %>% spDists(longlat=T) %>% as.dist() %>% hclust() %>% cutree(h=500)
  
  #estimate concave hull polygons for each cluster, buffer by 50km, clip to coastlines, format as df for ggplot, add to the ranges df
  for(j in unique(pts$clust2)){
    area1 <- unique(pts[pts$clust2==j,c(1,2)]) %>% as.matrix() %>% 
              concaveman(concavity=1.75) %>% 
                Polygon() %>% list() %>% Polygons(1) %>% list() %>% 
                  SpatialPolygons(proj4string = crs(clip_poly)) %>%
                    spTransform("+proj=utm +zone=19 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") %>% 
                      gBuffer(width=5e4) %>% 
                        spTransform(crs(clip_poly)) %>% 
                          intersect(.,clip_poly) %>% 
                            crop(extent(-155,-87,22,61.5)) %>% 
                              fortify()
    area1$piece <- as.character(area1$piece)
    area1$group <- paste0(i,"_",j,"_",as.character(area1$group))
    area1$maxyear <- i
    area1$polygon <- j
    ranges <- rbind(ranges,area1)
  }
}
ranges$maxyear <- factor(ranges$maxyear,levels=year_breaks)
ranges$group <- factor(ranges$group,levels=rev(levels(factor(ranges$group))))

#print pdf map
pdf("figures/Figure_1.pdf",width=3.5,height=3.25)
m <- ggplot()+coord_map()+theme_minimal()+
  xlab("")+ylab("")+
  theme(legend.position=c(0.16,0.45),
        legend.background = element_blank(),
        legend.key.width = unit(6,"mm"),
        legend.key.height = unit(5,"mm"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.border = element_blank(),
        text=element_text(size=10))+
  scale_fill_brewer(palette = "RdYlBu",name="Year",direction = -1)+
  scale_x_continuous(breaks=c(-140,-130,-120,-110,-100,-90),limits = c(-143,-91))+
  scale_y_continuous(breaks=c(20,30,40,50,60),limits = c(22.5,59.5))+
  geom_polygon(data=ranges,aes(x=long,y=lat,group=group,fill=maxyear))+
  geom_path(data=map_data("state"),aes(x=long,y=lat,group=group),lwd=0.25,col="grey")+
  geom_path(data=map_data("world"),aes(x=long,y=lat,group=group),lwd=0.25)
ggdraw()+
  draw_plot(m,-0.05,0,1,1)+
  draw_image("anhu_summer.jpg",.5,.45,.5,.5)
dev.off()

#print gif panels
for(i in year_breaks){
  tmp <- ranges[as.numeric(as.character(ranges$maxyear))<=i,]
  col <- rev(brewer.pal(n=length(year_breaks),name = "RdYlBu"))[which(year_breaks<=i)]
  tiff(paste0("hull_gif/",i,".tif"),width=4,height=4,res=600,units="in")
  map <- map_data("world")
  m <- ggplot()+coord_map()+theme_minimal()+
    xlab("")+ylab("")+
    theme(legend.position=c(0.18,0.45),
          legend.background = element_blank(),
          legend.key.width = unit(6,"mm"),
          legend.key.height = unit(5,"mm"),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.border = element_blank(),
          text=element_text(size=8))+
    scale_fill_manual(values=col,name="Year")+
    xlim(-155,-87)+ylim(22,61.5)+
    geom_polygon(data=tmp,aes(x=long,y=lat,group=group,fill=maxyear))+
    geom_path(data=map_data("state"),aes(x=long,y=lat,group=group),lwd=0.25,col="grey")+
    geom_path(data=map,aes(x=long,y=lat,group=group),lwd=0.25)
  print(m)
  dev.off()
}

