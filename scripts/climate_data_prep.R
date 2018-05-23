#prep PRISM data for niche modeling 
setwd("/Volumes/backup/prism/")
library(raster);library(magrittr)

#want: average monthly precip, mean/min/max temp for breeding & nonbreeding seasons, 1895-1920 & 1980-2017

getPRISMaverage <- function(directory,months,years,crop=NULL,outfile=NULL){
  files <- list.files(directory,full.names = T)
  files <- files[grepl("_bil.bil$",files)]
  y <- sapply(files,function(e) unlist(strsplit(basename(e),"_"))[5] %>% substr(1,4) %>% as.numeric())
  names(y) <- NULL
  m <- sapply(files,function(e) unlist(strsplit(basename(e),"_"))[5] %>% substr(5,6) %>% as.numeric())
  names(m) <- NULL
  files <- data.frame(files,y,m,stringsAsFactors = F)
  files <- subset(files,y %in% years & m %in% months)
  print("loading data...")
  stack <- stack(files$files)
  if(!is.null(crop)){ #this might be slower...
    print("cropping...")
    stack <- crop(stack,crop)
  }
  print("calculating average values...")
  out <- mean(stack)
  if(is.null(outfile)){
    var <- unlist(strsplit(basename(files$files[1]),"_"))[2]
    outfile <- paste0(var,"_",months[1],"-",months[length(months)],"_",min(years),"-",max(years),".tif")
  }
  writeRaster(out,outfile)
  #return(out)
}

files <- list.files(full.names = T)
files <- files[grepl("1980",files)]
files <- files[1:5]
foreach(i=files) %dopar% getPRISMaverage(i,months=c(6,7,8,9,10),years=1895:1925)
foreach(i=files) %dopar% getPRISMaverage(i,months=c(11,12,1,2,3,4,5),years=1895:1925)
foreach(i=files) %dopar% getPRISMaverage(i,months=c(6,7,8,9,10),years=1945:1975)
foreach(i=files) %dopar% getPRISMaverage(i,months=c(11,12,1,2,3,4,5),years=1945:1975)

files <- list.files(full.names=T)
files <- files[grepl("2017",files)]
foreach(i=files) %dopar% getPRISMaverage(i,months=c(6,7,8,9,10),years=1995:2015)
foreach(i=files) %dopar% getPRISMaverage(i,months=c(11,12,1,2,3,4,5),years=1995:2015)

prism <- raster("anhu_clim_1890-1920/ppt_11-5_1890-1920.tif")
#housing data
county_census <- fread("/Volumes/backup/prism/nhgis0004_csv/nhgis0004_ts_nominal_county.csv")
map1920 <- shapefile("nhgis0004_shape/nhgis0004_shapefile_tl2000_us_county_1920/US_county_1920.shp")
pop_early <- subset(county_census,county_census$YEAR==1920)
map1920@data <- merge(map1920@data,pop_early,by="GISJOIN",sort=F,all.x=T,all.y=F)
map1920@data$density <- map1920@data$A00AA/map1920@data$SHAPE_AREA*1e6
map1920 <- spTransform(map1920,crs(prism))
density_early <- rasterize(map1920,prism,field="density")

tract_census <- fread("/Volumes/backup/prism/nhgis0004_csv/nhgis0004_ts_nominal_tract.csv")
map1970 <- shapefile("nhgis0004_shape/nhgis0004_shapefile_tl2000_us_tract_1970/US_tract_1970.shp")
pop_mid <- subset(county_census,tract_census$YEAR==1970)
map1970@data <- merge(map1970@data,pop_mid,by="GISJOIN",sort=F,all.x=T,all.y=F)
map1970@data$density <- map1970@data$AV0AA/map1970@data$SHAPE_AREA*1e6
map1970 <- spTransform(map1970,crs(prism))
density_mid <- rasterize(map1970,prism,field="density")

map2010 <- shapefile("nhgis0004_shape/nhgis0004_shapefile_tl2010_us_tract_2010/US_tract_2010.shp")
pop_late <- subset(tract_census,county_census$YEAR==2010)
map2010@data <- merge(map2010@data,pop_late,by="GISJOIN",sort=F,all.x=T,all.y=F)
map2010@data$density <- map2010@data$AV0AA/map2010@data$SHAPE_AREA*1e6
map2010 <- spTransform(map2010,crs(prism))
density_late <- rasterize(map2010,prism,field="density")






tmp <- crop(census,c(-123,-122,47,48))
tmp@data$id <- rownames(tmp@data)
df <- fortify(tmp,region="id")
df <- join(df,tmp@data,by="id")
ggplot(data=df,aes(x=long,y=lat,group=group,fill=B0S001))+geom_polygon()





