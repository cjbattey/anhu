#anhu_niche
library(dismo);library(raster);library(magrittr);library(factoextra);library(cowplot);library(ggridges);library(car)
source("scripts/anhu_niche_similarity.R")
theme_set(theme_classic()+theme(axis.text=element_text(size=7),
                                axis.title=element_text(size=8),
                                strip.background = element_blank(),
                                strip.text = element_text(size=8),
                                legend.text = element_text(size=7),
                                legend.title = element_text(size=8)))

#load historic climate data
clim_early <- list.files("data/anhu_clim_1895-1925/",full.names = T) %>% .[grepl("tmin|tmean|tmax|ppt",.)] %>% stack() %>% crop(extent(-140,-95,24,57))
clim_mid <- list.files("data/anhu_clim_1945-1975/",full.names = T) %>% .[grepl("tmin|tmean|tmax|ppt",.)] %>% stack() %>% crop(extent(-140,-95,24,57))
clim_late <- list.files("data/anhu_clim_1995-2015",full.names = T) %>% .[grepl("tmin|tmean|tmax|ppt",.)] %>% stack() %>% crop(extent(-140,-95,24,57))
names(clim_early) <- c("ppt_breeding","ppt_nonbreeding",
                       "tmax_breeding","tmax_nonbreeding",
                       "tmean_breeding","tmean_nonbreeding",
                       "tmin_breeding","tmin_nonbreeding")
names(clim_mid) <- names(clim_early)
names(clim_late) <- names(clim_early)
usa <- shapefile("~/Documents/worldclim/country_outlines/cntry06/cntry06.shp") %>% spTransform(crs(clim_early))
usa <- usa[usa@data$FIPS_CNTRY=="US",]

pts1 <- subset(combo2,year<=i)[,c("long","lat")] %>% 
  SpatialPoints(proj4string = crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")) %>% 
  crop(extent(-155,-85,22,61.5))

#load occurrence data
cbc <- read.csv("data/CBC/Battey - CBC_Circle_Species_Report_SQL_updated.csv",stringsAsFactors = F)
cbc$year <- cbc$Count_yr+1899
cbc$state <- sapply(cbc$Subnational_code,function(e) strsplit(e,split="-") %>% unlist() %>% .[2])
cbc$surveyID <- paste(cbc$Name,cbc$Count_yr,sep="_")
gbif <- fread("data/gbif/occurrence.txt",data.table = F)
gbif <- gbif[,c("collectionID","datasetID","institutionCode","basisOfRecord","individualCount","year","month","day",
                "verbatimEventDate","stateProvince","county","municipality","locality","verbatimLocality",
                "verbatimElevation","decimalLatitude","decimalLongitude","coordinateUncertaintyInMeters")]
gbif$basisOfRecord <- factor(gbif$basisOfRecord)
gbif <- subset(gbif,coordinateUncertaintyInMeters<10000|is.na(coordinateUncertaintyInMeters))
gbif <- subset(gbif,basisOfRecord=="PRESERVED_SPECIMEN" | (institutionCode=="CLO" & basisOfRecord=="HUMAN_OBSERVATION"))
bbs <- fread("data/BBS/anhu_counts.csv")
routes <- fread("data/BBS/route_locs.csv")
combo <- data.frame(long=c(cbc$Longitude,gbif$decimalLongitude),
                    lat=c(cbc$Latitude,gbif$decimalLatitude),
                    year=c(cbc$year,gbif$year),
                    month=c(rep(12,nrow(cbc)),gbif$month),
                    state=c(cbc$state,gbif$stateProvince),
                    source=c(rep("CBC",nrow(cbc)),as.character(gbif$basisOfRecord)))
combo$source <- as.character(combo$source)
combo <- subset(combo,!is.na(combo$long))

#######################classic stats for shift in mean and variance by climate variable############################
early_brd <- subset(combo,year<=1925 & year>1895 & month %in% c(12,1,2,3,4,5) & source %in% c("CBC","PRESERVED_SPECIMEN"))
early_brd <- SpatialPoints(early_brd[,c("long","lat")])
early_clim_var <- extract(clim_early,early_brd)
early_clim_var <- early_clim_var[,!grepl("nonbreeding",colnames(early_clim_var))]
early_clim_var <- melt(data.frame(early_clim_var))
early_nb <- subset(combo,year<=1925 & year>1895 & month %in% c(7,8,9,10))
early_nb <- SpatialPoints(early_nb[,c("long","lat")])
tmp <- extract(clim_early,early_nb)
tmp <- tmp[,grepl("nonbreeding",colnames(tmp))]
tmp <- melt(data.frame(tmp))
early_clim_var <- rbind(early_clim_var,tmp)
early_clim_var$time <- "1895-1925"

mid_brd <- subset(combo,year<=1975 & year>1945 & month %in% c(12,1,2,3,4,5)  & source %in% c("CBC","PRESERVED_SPECIMEN"))
mid_brd <- SpatialPoints(mid_brd[,c("long","lat")])
mid_brd <- mid_brd[round(runif(length(early_brd),1,length(mid_brd)))] #subsample
mid_clim_var <- extract(clim_mid,mid_brd)
mid_clim_var <- mid_clim_var[,!grepl("nonbreeding",colnames(mid_clim_var))]
mid_clim_var <- melt(data.frame(mid_clim_var))
mid_nb <- subset(combo,year<=1975 & year>1945 & month %in% c(7,8,9,10))
mid_nb <- SpatialPoints(mid_nb[,c("long","lat")])
mid_nb <- mid_nb[round(runif(length(early_nb),1,length(mid_nb)))] #subsample
tmp <- extract(clim_mid,mid_nb)
tmp <- tmp[,grepl("nonbreeding",colnames(tmp))]
tmp <- melt(data.frame(tmp))
mid_clim_var <- rbind(mid_clim_var,tmp)
mid_clim_var$time <- "1945-1975"

late_brd <- subset(combo,year<=2016 & year>1995 & month %in% c(12,1,2,3,4,5)  & source %in% c("CBC","PRESERVED_SPECIMEN"))
late_brd <- SpatialPoints(late_brd[,c("long","lat")])
late_brd <- late_brd[round(runif(length(early_brd),1,length(late_brd)))] #subsample
late_clim_var <- extract(clim_late,late_brd)
late_clim_var <- late_clim_var[,!grepl("nonbreeding",colnames(late_clim_var))]
late_clim_var <- melt(data.frame(late_clim_var))
late_nb <- subset(combo,year<=2016 & year>1995 & month %in% c(7,8,9,10))
late_nb <- SpatialPoints(late_nb[,c("long","lat")])
late_nb <- late_nb[round(runif(length(early_nb),1,length(late_nb)))] #subsample
tmp <- extract(clim_late,late_nb)
tmp <- tmp[,grepl("nonbreeding",colnames(tmp))]
tmp <- melt(data.frame(tmp))
late_clim_var <- rbind(late_clim_var,tmp)
late_clim_var$time <- "1995-2016"

clim_var <- rbind(early_clim_var,mid_clim_var,late_clim_var)
clim_var$comp <- "observed"

#extract climate vars for no-range-shift conditions (eg extract from early reports across time periods)
# mid_noshift <- extract(clim_mid,early_brd)
# mid_noshift <- mid_noshift[,!grepl("nonbreeding",colnames(mid_noshift))]
# mid_noshift <- melt(data.frame(mid_noshift))
# tmp <- extract(clim_mid,early_nb)
# tmp <- tmp[,grep("nonbreeding",colnames(tmp))]
# tmp <- melt(data.frame(tmp))
# mid_noshift <- rbind(mid_noshift,tmp)
# mid_noshift$time <- "1945-1975"
# 
# late_noshift <- extract(clim_late,early_brd)
# late_noshift <- late_noshift[,!grepl("nonbreeding",colnames(late_noshift))]
# late_noshift <- melt(data.frame(late_noshift))
# tmp <- extract(clim_late,early_nb)
# tmp <- tmp[,grep("nonbreeding",colnames(tmp))]
# tmp <- melt(data.frame(tmp))
# late_noshift <- rbind(late_noshift,tmp)
# late_noshift$time <- "1995-2016"
# 
# clim_var_noshift <- rbind(early_clim_var,mid_noshift,late_noshift)
# clim_var_noshift$comp <- "no range shift"
# 
# clim_var <- rbind(clim_var,clim_var_noshift)
# 
# #extract stats for range shift but no climate change conditions (eg extract early climate data from all reports)
# mid_nocc <- extract(clim_early,mid_brd)
# mid_nocc <- mid_nocc[,!grepl("nonbreeding",colnames(mid_nocc))]
# mid_nocc <- melt(data.frame(mid_nocc))
# tmp <- extract(clim_early,mid_nb)
# tmp <- tmp[,grep("nonbreeding",colnames(tmp))]
# tmp <- melt(data.frame(tmp))
# mid_nocc <- rbind(mid_nocc,tmp)
# mid_nocc$time <- "1945-1975"
# 
# late_nocc <- extract(clim_early,late_brd)
# late_nocc <- late_nocc[,!grepl("nonbreeding",colnames(late_nocc))]
# late_nocc <- melt(data.frame(late_nocc))
# tmp <- extract(clim_early,late_nb)
# tmp <- tmp[,grep("nonbreeding",colnames(tmp))]
# tmp <- melt(data.frame(tmp))
# late_nocc <- rbind(late_nocc,tmp)
# late_nocc$time <- "1995-2016"
# 
# clim_var_nocc <- rbind(early_clim_var,mid_nocc,late_nocc)
# clim_var_nocc$comp <- "no climate change"
# 
# clim_var <- rbind(clim_var,clim_var_nocc)


#test for difference in mean and variance across time periods with ANOVA and Levene's test
pvals <- c();levpvals <- c()
for(i in unique(clim_var$variable)){
  var <- subset(clim_var,variable==i & comp=="observed")
  model <- aov(value~time,data=var)
  p <- summary(model)[[1]]["Pr(>F)"][[1]][1]
  pvals <- append(pvals,p)
  
  lev <- leveneTest(var$value,var$time)
  levpvals <- append(levpvals,lev[3][[1]][1])
}
names(pvals) <- unique(clim_var$variable)
names(levpvals) <- unique(clim_var$variable)
pvals <- round(sapply(pvals,function(e) p.adjust(e,method="holm",n=8)),5)
levpvals <- round(sapply(levpvals,function(e) p.adjust(e,method="holm",n=8)),8)
pvals
levpvals

#suptable <- data.frame(anova=pvals,levene=levpvals)
#test for difference in mean and variance bw observed and no-range-shift scenarios within time periods


#plot
quantiles <- ddply(clim_var,.(variable,time,comp),summarize,
                   med=median(value,na.rm=T),
                   low=quantile(value,0.025,na.rm=T),
                   high=quantile(value,0.975,na.rm=T),
                   min=min(value,na.rm=T),
                   max=max(value,na.rm=T))
cv<- clim_var
cv$value[cv$variable %in% c('ppt_nonbreeding') & cv$value >100] <- NA #drop precipitation outliers
cv$value[cv$variable %in% c('ppt_breeding') & cv$value >200] <- NA #drop precipitation outliers

clim_var_plot <- ggplot(data=subset(cv,comp=="observed"),aes(x=value,y=time))+
  theme(axis.title = element_blank(),legend.position="bottom")+
  theme(axis.text.y = element_blank(),
        axis.ticks.y=element_blank())+
  facet_wrap(~variable,scales = "free",nrow=2)+
  scale_y_discrete(expand=c(0.2,0))+
  #scale_fill_manual(values=c("violet","forestgreen","cornflowerblue"),name="")+
  #scale_color_manual(values=c("violet","forestgreen","cornflowerblue"),guide=F)+
  geom_vline(data=subset(quantiles,time=="1895-1925"),aes(xintercept=low),linetype=2)+
  geom_vline(data=subset(quantiles,time=="1895-1925"),aes(xintercept=high),linetype=2)+
  geom_density_ridges(lwd=0.25,scale=0.8,fill="grey",col="black")+
  geom_segment(data=quantiles,aes(x=med,xend=med,y=time,yend=as.integer(factor(time))-.25))+
  geom_segment(data=quantiles,aes(x=low,xend=low,y=time,yend=as.integer(factor(time))-.15))+
  geom_segment(data=quantiles,aes(x=high,xend=high,y=time,yend=as.integer(factor(time))-.15))
pdf("figures/clim_var_density.pdf",width=6,height=3)
print(clim_var_plot)
dev.off()


###################### map of sites with minimum breeding temp in 95% CI of early records by time period ##################
cutoff <- subset(early_clim_var,variable=="tmin_breeding" & time=="1895-1925") %>% 
            .$value %>% 
              quantile(0.025,na.rm=T)


temp_map_early <- clim_early$tmin_breeding >= cutoff
temp_map_early <- as.data.frame(temp_map_early,xy=T)
temp_map_mid <- clim_mid$tmin_breeding >= cutoff 
temp_map_mid <- as.data.frame(temp_map_mid,xy=T)
temp_map_late <- clim_late$tmin_breeding >= cutoff 
temp_map_late <- as.data.frame(temp_map_late,xy=T)
temp_map_early$time <- "1895-1925"
temp_map_mid$time <- "1945-1975"
temp_map_late$time <- "1995-2015"

min_temp_maps <- rbind(temp_map_early,temp_map_mid,temp_map_late)
map <- map_data("state")

locs <- subset(combo,month %in% c(12,1,2,3,4,5)) 
locs <- ddply(locs,.(year,long,lat),function(e) e[1,])
locs$time <- NA
locs$time[locs$year>=1895 & locs$year<=1925] <- "1895-1925"
locs$time[locs$year>=1945 & locs$year<=1975] <- "1945-1975"
locs$time[locs$year>=1995 & locs$year<=2015] <- "1995-2015"
locs <- subset(locs,!is.na(locs$time))
usa <- shapefile("~/Documents/cb_2017_us_nation_20m/cb_2017_us_nation_20m.shp")
locs <- SpatialPointsDataFrame(locs[,c("long","lat")],locs[,3:ncol(locs)],proj4string = crs(usa))
locs <- intersect(locs,usa)
locs <- as.data.frame(locs)

p <- ggplot()+
  theme_classic()+
  theme(axis.title = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_text(size=8),
        strip.background = element_blank())+
  facet_wrap(~time,ncol=3)+
  scale_fill_manual(values=c("NA","forestgreen","NA"),guide=FALSE)+
  xlim(-125,-95)+ylim(24.5,49.9)+
  geom_path(data=map,aes(x=long,y=lat,group=group),col="grey",lwd=0.25)+
  geom_raster(data=min_temp_maps,aes(x=x,y=y,fill=layer))
  geom_point(data=locs,aes(x=long,y=lat),shape=21,stroke=0.2,size=1)

pdf("~/Dropbox/anhu/figures/min_temp_maps.pdf",width=6,height=2.5,useDingbats = F)
print(p)
# ggdraw()+
#   draw_plot(clim_var_plot,0,.44,1,.55)+
#   draw_plot(p,0.05,0,.9,.46)
dev.off()

###################### PCA on climate variables ###################
#range <- #tbd

clim_early_list <- lapply(1:8,function(e) as.data.frame(clim_early[[e]]))
clim_mid_list <-  lapply(1:8,function(e) as.data.frame(clim_mid[[e]]))
clim_late_list <-  lapply(1:8,function(e) as.data.frame(clim_late[[e]]))
clim_early_df <- do.call(cbind,clim_early_list)
clim_mid_df <- do.call(cbind,clim_mid_list)
clim_late_df <- do.call(cbind,clim_late_list)
climdf <- rbind(clim_early_df,clim_mid_df,clim_late_df)
climdf <- na.omit(climdf)
climpc <- prcomp(climdf[,3:ncol(climdf)],center=T,scale=T)

early <- subset(combo,year<=1925 & year>1895 & month %in% c(12,1,2,3,4,5))
early <- SpatialPoints(early[,c("long","lat")])
early_clim_var <- extract(clim_early,early)
earlyclimpc <- data.frame(predict(climpc,early_clim_var))
earlyclimpc$time <- "1895-1925"

mid <- subset(combo,year<=1975 & year>1945 & month %in% c(12,1,2,3,4,5))
mid <- SpatialPoints(mid[,c("long","lat")])[round(runif(length(early),1,nrow(mid)))]
mid_clim_var <- extract(clim_mid,mid)
midclimpc <- data.frame(predict(climpc,mid_clim_var))
midclimpc$time <- "1945-1975"

late <- subset(combo,year<=2016 & year>1995 & month %in% c(12,1,2,3,4,5))
late <- SpatialPoints(late[,c("long","lat")])[round(runif(length(early),1,nrow(late)))]
late_clim_var <- extract(clim_late,late)
lateclimpc <- data.frame(predict(climpc,late_clim_var))
lateclimpc$time <- "1995-2016"

allclimpc <- rbind(earlyclimpc,midclimpc,lateclimpc)
#allclimpc$time <- factor(allclimpc$time,levels=c("1995-2016","1945-1975","1895-1925"))
climpc_plot <- ggplot(data=allclimpc,aes(x=PC1,y=PC2))+
  facet_wrap(~time)+theme(strip.background = element_blank(),
                          strip.text=element_text(size=8),
                          axis.text=element_text(size=7),
                          axis.title=element_text(size=8))+
  xlab("PC1 (91%)")+ylab("PC2 (6%)")+
  #geom_density_2d()
  #stat_bin_hex()
  geom_point(shape=1)

pdf("figures/pca_3panel.pdf",width=6.5,height=2,useDingbats = F)
ggdraw()+
  draw_plot(climpc_plot,0,0,0.7,1)+
  draw_plot(varplot,0.7,0,.3,1)
dev.off()

var_cors <- as.data.frame(cor(climdf, climpc$x))
arrows <- data.frame(x1 = rep(0,8), y1 = rep(0,8), x2 = var_cors$PC1, 
                    y2 = var_cors$PC2)
circle <- function(center = c(0, 0), npoints = 100) {
  r = 1
  tt = seq(0, 2 * pi, length = npoints)
  xx = center[1] + r * cos(tt)
  yy = center[1] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}
varplot <- ggplot() + coord_equal()+
  xlab(paste("PC1",round(summary(climpc)['importance'][[1]][2],4)*100,"%"))+
  ylab(paste("PC2",round(summary(climpc)['importance'][[1]][5],4)*100,"%"))+
  geom_path(data = circle(c(0, 0), npoints = 100), aes(x = x, y = y), colour = "gray65") + 
  geom_segment(data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2), colour = "gray65") + 
  geom_text_repel(data = var_cors, aes(x = PC1, y = PC2, label = rownames(var_cors)),size=3)


climpc_var <- melt(data.frame(climpc$rotation))

climpc_map <- cbind(climpc$x[,1:2],climdf[,1:2])
climpc_map <- melt(climpc_map,id.vars=c("x","y"))
climpc_mapplot <- ggplot(data=climpc_map,aes(x=x,y=y,fill=value))+coord_equal()+
  facet_wrap(~variable,nrow=2)+
  theme(axis.ticks = element_blank(),axis.text = element_blank(),
        axis.title=element_blank(),axis.line=element_blank(),
        legend.position = "bottom",legend.direction = "horizontal")+
  scale_fill_gradient(low="white",high="black")+
  geom_raster()
climpc_mapplot <- climpc_mapplot+guides(fill=guide_colorbar(barwidth = unit(30,"mm"),barheight=unit(4,"mm")))

#plot_grid(climpc_plot,climpc_mapplot,rel_widths = c(0.7,0.2))
p <- ggdraw()+
  draw_plot(climpc_plot,0,0,0.7,1)+
  draw_plot(climpc_mapplot,0.7,0,0.3,1.05)
pdf("figures/clim_pca.pdf",width=6.5,height=2.5)
print(p)
dev.off()


################################ Maxent Cross-Prediction #################################
#get wintering occurrence records, crop to available climate data, & subsample to equal sizes
early <- SpatialPoints(early[,c("long","lat")])
tmp <- extract(clim_early[[1]],early)
early <- early[!is.na(tmp),] %>% as.data.frame()
mid <- subset(combo,year<1975 & year >=1945 & month %in% c(11,12,1,2,3,4,5))
mid <- SpatialPoints(mid[,c("long","lat")])
tmp <- extract(clim_early[[1]],mid)
mid <- mid[!is.na(tmp),]
mid <- mid[sample(1:length(mid),nrow(early))] %>% as.data.frame()
late <- subset(combo,year>=1995 & month %in% c(11,12,1,2,3,4,5) & source %in% c("Museum Specimens","CBC"))
late <- SpatialPoints(late[,c("long","lat")])
tmp <- extract(clim_early[[1]],late)
late <- late[!is.na(tmp),]
late <- late[sample(1:length(late),nrow(early))] %>% as.data.frame()
plot(clim_late[[1]])+points(late,col="blue")+points(mid,col="green")+points(early,col="red")

#train maxent models on historic climate & occurrence
model_early <- maxent(clim_early,early,removeDuplicates=T)
model_mid <- maxent(clim_mid,mid,removeDuplicates=T)
model_late <- maxent(clim_late,late,removeDuplicates=T)

#predict suitability across all time periods
pred_early_early <- predict(model_early,clim_early)
pred_early_mid <- predict(model_early,clim_mid)
pred_early_late <- predict(model_early,clim_late)
pred_mid_early <- predict(model_mid,clim_early)
pred_mid_mid <- predict(model_mid,clim_mid)
pred_mid_late <- predict(model_mid,clim_late)
pred_late_early <- predict(model_late,clim_early)
pred_late_mid <- predict(model_late,clim_mid)
pred_late_late <- predict(model_late,clim_late)

# #tests for niche equivalency and overlap
# a <- nicheEquivalency(early[,c("long","lat")],mid[,c("long","lat")],clim_late,n=10,model=maxent)
# b <- nicheEquivalency(early[,c("long","lat")],late[,c("long","lat")],clim_late,n=10,model=maxent)
# c <- nicheEquivalency(mid[,c("long","lat")],late[,c("long","lat")],clim_late,n=10,model=maxent)

#tests for niche similarity (warning: slow AF. run overnight.)
simtest_early_mid <- nicheSimilarity(early,mid,clim_early,clim_mid,clim_late,pred_early_early,pred_early_mid,1e5,usa,100)
simtest_early_late <- nicheSimilarity(early,late,clim_early,clim_late,clim_late,pred_early_early,pred_early_mid,1e5,usa,100)
simtest_mid_late <- nicheSimilarity(mid,late,clim_mid,clim_late,clim_late,pred_early_early,pred_early_mid,1e5,usa,100)

tmp <- data.frame(a$null.distribution)
ggplot(data=tmp,aes(x=D))+xlim(0,1)+geom_histogram(fill="grey",lwd=0.5)+geom_vline(xintercept=a$statistic[1],col="red")

#are internal comparisons (predicting bw time periods for the same model) more consistent than external (diff models)?
internal_overlap <- c(
  nicheOverlap(pred_early_early,pred_early_mid),
  nicheOverlap(pred_early_early,pred_early_late),
  nicheOverlap(pred_early_mid,pred_early_late),
  nicheOverlap(pred_mid_early,pred_mid_late),
  nicheOverlap(pred_mid_early,pred_mid_mid),
  nicheOverlap(pred_mid_mid,pred_mid_late),
  nicheOverlap(pred_late_early,pred_late_late),
  nicheOverlap(pred_late_mid,pred_late_late),
  nicheOverlap(pred_late_early,pred_late_mid)
)
external_overlap <- c(
  nicheOverlap(pred_early_late,pred_mid_late),
  nicheOverlap(pred_early_late,pred_late_late),
  nicheOverlap(pred_mid_late,pred_late_late),
  nicheOverlap(pred_early_early,pred_mid_early),
  nicheOverlap(pred_early_early,pred_late_early),
  nicheOverlap(pred_mid_early,pred_late_early),
  nicheOverlap(pred_early_mid,pred_mid_mid),
  nicheOverlap(pred_early_mid,pred_late_mid),
  nicheOverlap(pred_mid_mid,pred_late_mid)
)
overlaps <- data.frame(overlap=c(internal_overlap,external_overlap),comparison=c(rep("internal",9),rep("external",9)))
ggplot(data=overlaps,aes(y=overlap,x=comparison))+geom_boxplot(fill=NA)+geom_sina()#hard yes

################# niche model figure ######################
#gather raster data for ggplotting 
pred_df <- data.frame(x=numeric(),y=numeric(),layer=numeric(),training=character(),testing=character())
pred_list <- list(pred_early_early,pred_mid_mid,pred_late_late)
training <- c("1895-1925","1945-1975","1995-2015")
testing <- rep(training,3)
for(i in 1:length(pred_list)){
  a <- data.frame(rasterToPoints(pred_list[[i]]))
  a$training <- training[ceiling(i/3)]
  a$testing <- testing[i]
  pred_df <- rbind(pred_df,a)
}

#gather model parameter rankings for plots
early_var <- data.frame(model_early@results);mid_var <- data.frame(model_mid@results);late_var <- data.frame(model_late@results)
vars <- list(early_var,mid_var,late_var)
var_df <- data.frame(val=numeric(),var=character(),time=character())
for(i in 1:length(vars)){
  v <- vars[[i]]
  ev <- v[grep("importance",rownames(v)),]
  ev <- data.frame(val=ev,var=rownames(v)[grep("importance",rownames(v))],stringsAsFactors = F)
  ev$var <- sapply(ev$var,function(e) strsplit(e,"\\.") %>% unlist() %>% .[1])
  ev$time <- i
  var_df <- rbind(var_df,ev)
}
var_df$time <- mapvalues(var_df$time,from=c(1,2,3),to=c("1895-1925","1945-1975","1995-2015"))
var_df$linemin <- 0
var_df$linemax <- max(var_df$val)
hlines <- data.frame(y=unique(var_df$var),x=rep(0,8),xend=rep(80,8))

#plot 'em
var_plot <- ggplot(data=var_df,aes(x=val,y=var))+
  theme(strip.background = element_blank(),
        text=element_text(size=8),
        axis.text = element_text(size=8))+
  xlab("Variable Importance")+ylab("")+
  facet_grid(~time)+
  geom_segment(aes(x=linemin,xend=linemax,y=var,yend=var),col="grey",linetype=2)+
  geom_point()

state <- map_data("state")
raster_plot <- ggplot()+
  theme_minimal()+
  theme(panel.background = element_blank(),
        #axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        strip.background = element_blank(),
        text = element_text(size=8),
        legend.position="left")+
  ylab("")+xlab("")+
  facet_grid(.~testing)+
  scale_fill_viridis(option="inferno",direction=-1,name="Predicted\nSuitability")+
  #scale_fill_distiller(palette="YlOrRd",direction=1,name="Predicted\nSuitability")+
  scale_x_continuous(position="top",limits=c(min(pred_df$x),max(pred_df$x)))+
  scale_y_continuous(position="right",limits=c(min(pred_df$y),max(pred_df$y)))+
  geom_tile(data=pred_df,aes(x=x,y=y,fill=layer))+
  geom_path(data=state,aes(x=long,y=lat,group=group),lwd=0.1,col="black")

tiff("anhu_niche.tif",width=6.5,height=4,units="in",res=300)
ggdraw()+
  draw_plot(raster_plot,0.05,0.5,0.95,0.55)+
  draw_plot(var_plot,0.01,0,0.9,0.5)
dev.off()

######################################################################
######## alternate version showing all pairwise comparisons ##########
#gather raster data for ggplotting 
pred_df <- data.frame(x=numeric(),y=numeric(),layer=numeric(),training=character(),testing=character())
pred_list <- list(pred_early_early,pred_early_mid,pred_early_late,
                  pred_mid_early,pred_mid_mid,pred_mid_late,
                  pred_late_early,pred_late_mid,pred_late_late)
training <- c("1895-1925","1945-1975","1995-2015")
testing <- rep(training,3)
for(i in 1:length(pred_list)){
  a <- data.frame(rasterToPoints(pred_list[[i]]))
  a$training <- training[ceiling(i/3)]
  a$testing <- testing[i]
  pred_df <- rbind(pred_df,a)
}

#gather model parameter rankings for plots
early_var <- data.frame(model_early@results);mid_var <- data.frame(model_mid@results);late_var <- data.frame(model_late@results)
vars <- list(early_var,mid_var,late_var)
var_df <- data.frame(val=numeric(),var=character(),time=character())
for(i in 1:length(vars)){
  v <- vars[[i]]
  ev <- v[grep("importance",rownames(v)),]
  ev <- data.frame(val=ev,var=rownames(v)[grep("importance",rownames(v))],stringsAsFactors = F)
  ev$var <- sapply(ev$var,function(e) strsplit(e,"\\.") %>% unlist() %>% .[1])
  ev$time <- i
  var_df <- rbind(var_df,ev)
}
var_df$time <- mapvalues(var_df$time,from=c(1,2,3),to=c("1895-1925","1945-1975","1995-2015"))
var_df$linemin <- 0
var_df$linemax <- max(var_df$val)
hlines <- data.frame(y=unique(var_df$var),x=rep(0,8),xend=rep(80,8))

#plot 'em
var_plot <- ggplot(data=var_df,aes(x=val,y=var))+
  theme(strip.background = element_blank(),
        text=element_text(size=8),
        axis.text = element_text(size=8))+
  xlab("Variable Importance")+ylab("")+
  facet_grid(~time)+
  geom_segment(aes(x=linemin,xend=linemax,y=var,yend=var),col="grey",linetype=2)+
  geom_point()

state <- map_data("state")
raster_plot <- ggplot()+
  theme_minimal()+
  theme(panel.background = element_blank(),
        #axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        strip.background = element_blank(),
        text = element_text(size=8),
        legend.position="left")+
  xlab("Training Time Period")+ylab("Prediction Time Period")+
  facet_grid(testing~training)+
  scale_fill_viridis(option="inferno",direction=-1,name="Predicted\nSuitability")+
  #scale_fill_distiller(palette="YlOrRd",direction=1,name="Predicted\nSuitability")+
  scale_x_continuous(position="top",limits=c(min(pred_df$x),max(pred_df$x)))+
  scale_y_continuous(position="right",limits=c(min(pred_df$y),max(pred_df$y)))+
  geom_tile(data=pred_df,aes(x=x,y=y,fill=layer))+
  geom_path(data=state,aes(x=long,y=lat,group=group),lwd=0.1,col="black")

tiff("anhu_niche_full.tif",width=6.5,height=7,units="in",res=300)
ggdraw()+
  draw_plot(raster_plot,0.05,0.25,0.95,0.75)+
  draw_plot(var_plot,0,0,0.9,0.25)
dev.off()





