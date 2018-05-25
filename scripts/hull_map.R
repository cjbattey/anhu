library(rgeos);library(concaveman)

clip_poly <- shapefile("~/Documents/worldclim/country_outlines/cntry06/cntry06.shp") %>% crop(extent(-155,-90,22,61.5))
dispersal_dist <- 1e5
first <- T
ranges <- data.frame(long=numeric(),lat=numeric(),order=integer(),hole=logical(),piece=character(),group=character(),maxyear=numeric())
combo2 <- subset(combo,source != "eBird" & month %in% c(11,12,1,2,3))
for(i in seq(1890,2020,15)){
  if(first==T){
    pts1 <- subset(combo2,year<=i & month %in% c(11,12,1,2,3))[,c("long","lat")]
    first <- F
  } else {
    pts1 <- subset(combo2,year<=i & year>(i-15) & month %in% c(11,12,1,2,3))[,c("long","lat")]
  }
  #area1 <- pts1[chull(pts1),] %>% Polygon() %>% list() %>% Polygons(1) %>% list() %>% SpatialPolygons(proj4string = crs(clip_poly))
  area1 <- concaveman(as.matrix(pts1),concavity = 2) %>% Polygon() %>% list() %>% Polygons(1) %>% list() %>% SpatialPolygons(proj4string = crs(clip_poly))
  area1 <- spTransform(area1,"+proj=utm +zone=19 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  area1 <- gBuffer(area1,width=dispersal_dist)
  area1 <- spTransform(area1,crs(clip_poly))
  area1 <- intersect(area1,clip_poly)
  area1 <- crop(area1,extent(-155,-90,22,61.5))
  area1 <- fortify(area1)
  area1$piece <- as.character(area1$piece)
  area1$group <- paste0(i,"_",as.character(area1$group))
  area1$maxyear <- i
  ranges <- rbind(ranges,area1)
}
ranges$maxyear <- factor(ranges$maxyear,levels=seq(1890,2020,15))
ranges$group <- factor(ranges$group,levels=rev(levels(factor(ranges$group))))

pdf("figures/hull_map.pdf",width=6,height=3.5)
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
  #scale_fill_viridis(discrete=T)+
  scale_fill_brewer(palette = "RdYlBu",name="Year",direction = -1)+
  xlim(-155,-90)+ylim(22,61.5)+
  geom_polygon(data=ranges,aes(x=long,y=lat,group=group,fill=maxyear))+
  geom_path(data=map,aes(x=long,y=lat,group=group),lwd=0.25)
ggdraw()+
  draw_plot(m,-0.1,0,0.75,1)+
  draw_image("IMGP3506.jpg",0.6,0,.35,1)
  #draw_image("anhu_pic.jpg",0,.2,.35,.5)
dev.off()



