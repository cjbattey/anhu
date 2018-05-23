#anhu_niche
library(dismo);library(raster);library(magrittr)
source("scripts/anhu_niche_similarity.R")

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

#get occurrence records, crop to available climate data, & subsample to equal sizes
early <- subset(combo,year<=1925 & year>1895 & month %in% c(11,12,1,2,3,4,5))
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

