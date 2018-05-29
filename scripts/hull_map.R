library(rgeos);library(concaveman);library(raster)

clip_poly <- shapefile("~/Documents/worldclim/country_outlines/cntry06/cntry06.shp") %>% crop(extent(-155,-87,22,61.5))
dispersal_dist <- 1e5
first <- T
ranges <- data.frame(long=numeric(),lat=numeric(),order=integer(),hole=logical(),piece=character(),group=character(),maxyear=numeric())
combo2 <- subset(combo,month %in% c(12,1,2,3) & source != "eBird")
year_breaks <- c(1940,1950,1960,1970,1980,1990,2000,2010)
for(i in year_breaks){
  pts1 <- subset(combo2,year<=i)[,c("long","lat")] %>% 
           SpatialPoints() %>% crop(extent(-155,-85,22,61.5)) %>% as.data.frame() %>% as.matrix()
  neighbors <- c() 
  for(j in 1:nrow(pts1)){
    neighbors[j] <- spDistsN1(pts1[-j,],pts1[j,],longlat=T) %>% .[.<=100] %>% length() #get the number of other reports within 100km
  }
  pts1 <- pts1[neighbors>=2,]
  area1 <- concaveman(as.matrix(unique(pts1)),concavity=1.75) %>% Polygon() %>% list() %>% Polygons(1) %>% list() %>% SpatialPolygons(proj4string = crs(clip_poly))
  area1 <- spTransform(area1,"+proj=utm +zone=19 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  area1 <- gBuffer(area1,width=dispersal_dist)
  area1 <- spTransform(area1,crs(clip_poly))
  area1 <- raster::intersect(area1,clip_poly)
  area1 <- crop(area1,extent(-155,-87,22,61.5))
  area1 <- fortify(area1)
  area1$piece <- as.character(area1$piece)
  area1$group <- paste0(i,"_",as.character(area1$group))
  area1$maxyear <- i
  ranges <- rbind(ranges,area1)
}
ranges$maxyear <- factor(ranges$maxyear,levels=year_breaks)
ranges$group <- factor(ranges$group,levels=rev(levels(factor(ranges$group))))

#print pdf map
pdf("figures/hull_map.pdf",width=6,height=5)
m <- ggplot()+coord_map()+theme_minimal()+
  xlab("")+ylab("")+
  theme(legend.position=c(0.18,0.45),
        legend.background = element_blank(),
        legend.key.width = unit(6,"mm"),
        legend.key.height = unit(5,"mm"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.border = element_blank(),
        text=element_text(size=10))+
  scale_fill_brewer(palette = "RdYlBu",name="Year",direction = -1)+
  scale_x_continuous(breaks=c(-140,-130,-120,-110,-100,-90),limits = c(-143,-87))+
  scale_y_continuous(breaks=c(20,30,40,50,60),limits = c(22.5,59.5))+
  geom_polygon(data=ranges,aes(x=long,y=lat,group=group,fill=maxyear))+
  geom_path(data=map_data("state"),aes(x=long,y=lat,group=group),lwd=0.25,col="grey")+
  geom_path(data=map_data("world"),aes(x=long,y=lat,group=group),lwd=0.25)
ggdraw()+
  draw_plot(m,0,0,1,1)+
  draw_image("anhu_summer.jpg",.5,.4,.55,.55)
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

