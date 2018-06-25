#nest records
library(ggridges);library(dplyr);library(ggforce);library(RColorBrewer);library(ggsignif)

#ebird
ebird <- fread("data/ebd_annhum_relFeb-2018/ebd_annhum_relFeb-2018.txt",data.table = F)
ebird <- subset(ebird,ebird$`BREEDING BIRD ATLAS CODE` %in% c("NY","NE","ON"))
ebird$year <- as.numeric(substr(ebird$`OBSERVATION DATE`,1,4))
ebird$month <- as.numeric(substr(ebird$`OBSERVATION DATE`,6,7))
ebird$jday <- as.Date(ebird$`OBSERVATION DATE`,"%Y-%m-%d") %>% format("%j") %>% as.numeric()
nest <- ebird
nest$range <- apply(nest,1,function(e){
  if(e[15] %in% c("British Columbia","Oregon","Washington")){
    "PNW"
  } else if(e[15]=="California" & e[26]<40.5){
    "Native"
  }  else if(e[15] %in% c("Baja California","Baja California Sur")){
    "Native"
  } else if (e[15] %in% c("Arizona","New Mexico","Nevada","Texas")){
    "SW"
  } else {
    "Other"
  }
})
nest <- ddply(nest,.(LATITUDE,LONGITUDE,STATE_PROVINCE,year,range),summarize,jday=min(jday))

#vertnet
vertnet <- fread("~/Downloads/anhu_vertnet_nests_may2018.txt",data.table = F)
vertnet <- subset(vertnet,!is.na(month))
range <- c()
for(i in 1:nrow(vertnet)){
  row <- vertnet[i,]
  if(row$stateprovince=="California"){
    range[i] <- "Native"
  } else if(row$stateprovince %in% c("Washington","Oregon","British Columbia")){
    range[i] <- "PNW"
  } else if(row$stateprovince %in% c("Arizona","Nevada","New Mexico","Texas")){
    range[i] <- "SW"
  } else {
    range[i] <- "Other"
  }}
vertnet$range <- range
vertnet$jday <- as.Date(vertnet$eventdate,"%Y-%m-%d") %>% format("%j")
vertnet <- ddply(vertnet,c("decimallatitude","decimallongitude","stateprovince","year","range"),summarize,jday=min(jday))
names(vertnet) <- names(nest)
nest <- rbind(nest,vertnet)
nest$jday <- as.numeric(nest$jday)

#drop vagrant(?) reports
nest <- subset(nest,range != "Other" & !is.na(jday))
breeding_day <- c()
for(i in 1:nrow(nest)){
  e <- nest[i,]
  if(e[6]>306){
    breeding_day[i] <- e[6]-306
  } else {
    breeding_day[i] <- e[6]+59
  }
}
nest$breeding_day <- unlist(breeding_day)
  
#wilcox test for differences bw pnw and california nesting season
wilcox.test(formula=breeding_day~range,data=subset(nest,range %in% c("Native","PNW")),conf.int=T) 
wilcox.test(formula=breeding_day~range,data=subset(nest,range %in% c("Native","SW")),conf.int=T)

#downsample california
diffs <- c()
ca <- subset(nest,range=="Native");pnw <- subset(nest,range=="PNW");sw <- subset(nest,range=="SW")
for(i in 1:1e4){
  tmp <- ca[sample(1:nrow(ca),124,replace = F),]
  diffs[i] <- quantile(tmp$breeding_day,0.1)-quantile(sw$breeding_day,0.1)
  #diffs[i] <- median(tmp$breeding_day)-median(sw$breeding_day)
}
quantile(diffs,0.95)
ggplot(data=data.frame(diffs),aes(x=diffs))+theme_minimal()+
  geom_density(fill="grey")+
  #geom_segment(aes(x=quantile(diffs,0.975),xend=quantile(diffs,0.975),y=0,yend=..density..))
  geom_vline(aes(xintercept=quantile(diffs,0.975)),col="white",linetype=2,lwd=1)

wilcox.test(formula=breeding_day~range,data=subset(tmp,range %in% c("Native","PNW")),conf.int=T)
wilcox.test(formula=breeding_day~range,data=subset(nest,range %in% c("Native","SW")),conf.int=T)
nest$total <- apply(nest,1,function(e){if(e[5]=="Native"){882}else if(e[5]=="PNW"){124}else if(e[5]=="SW"){181}})
#figure 3 - nest map 
nest$range <- factor(nest$range,levels=c("PNW","Native","SW"))
map <- map_data("world");state <- map_data("state")

medians <- ddply(nest,.(range),summarize,median=median(breeding_day,rm=T),quant_low=quantile(breeding_day,0.1),quant_high=quantile(breeding_day,0.9))
medians$range <- factor(medians$range,levels=c("PNW","Native","SW"))
monthdays <- data.frame(jday=c(1,32,60,91,121,152,182,213,244,274,305,335),
                        breeding_day=c(60,91,119,150,180,211,241,272,303,333,1,29),
                        month=c("January","February","March","April","May","June","July",
                                "August","September","October","November","December"),
                        y=6.3)
legend <- data.frame(range=factor(c("PNW","Native","SW"),levels=c("PNW","Native","SW")),breeding_day=c(1,1,1),text=c("PNW","Native","SW"))
nest_map <- ggplot()+coord_map()+
             theme_minimal()+theme(legend.position="none",#c(0.15,0.2),
                                   legend.direction = "horizontal",
                                   legend.background = element_blank(),
                                   axis.ticks=element_blank(),
                                   axis.text = element_blank(),
                                   text=element_text(size=8),
                                   axis.title = element_blank(),
                                   panel.border = element_blank())+
             scale_color_manual(values=brewer.pal(3,"Dark2")[c(1,3,2)])+
             xlim(min(nest$LONGITUDE,na.rm=T)-1,max(nest$LONGITUDE,na.rm=T)+1)+
             ylim(min(nest$LATITUDE,na.rm=T)-1,max(nest$LATITUDE,na.rm=T)+1)+
             geom_path(data=state,aes(x=long,y=lat,group=group),lwd=0.25,col="grey")+
             geom_path(data=map,aes(x=long,y=lat,group=group),lwd=0.5)+
             geom_point(data=nest,aes(x=LONGITUDE,y=LATITUDE,col=range),shape=1,size=0.9)
nest_map <- nest_map+guides(color=guide_legend(override.aes = list(size=4,shape=16),title = "Range"))

nest_histograms <- ggplot(data=nest,aes(x=breeding_day,fill=range))+
  theme_classic()+theme(text=element_text(size=8),legend.position = c(.7,.7))+
  scale_fill_manual(values=brewer.pal(3,"Dark2")[c(3,1,2)])+
  scale_color_brewer(palette = "Dark2",guide=F)+
  ylab("Active Nests")+xlab("Days from Nov 1")+
  geom_histogram(data=nest[nest$range=="Native",],bins=50,alpha=0.7)+
  geom_histogram(data=nest[nest$range=="SW",],bins=50,alpha=0.7)+
  geom_histogram(data=nest[nest$range=="PNW",],bins=50,alpha=0.7)+
  geom_segment(data=medians,aes(x=median,xend=median,y=0,yend=-3,col=range),lwd=0.35)

nest_histogram_circle <- ggplot(data=nest,aes(x=breeding_day,y=as.integer(range)+2,fill=range))+coord_polar()+
  theme(text=element_text(size=8),
        legend.position = "none",
        axis.title.y=element_blank(),
        panel.grid.major =element_line(colour = "grey",size=0.5),
        panel.grid.major.y=element_blank(),
        axis.text=element_blank(),
        panel.border = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())+
  scale_fill_manual(values=brewer.pal(3,"Dark2")[c(3,1,2)])+
  scale_y_discrete(expand=c(-0.1,0),limits=c(0,5))+
  scale_x_continuous(breaks=c(1,32,60,91,121,152,182,213,244,274,305,335),limits=c(1,365))+
  geom_density_ridges(stat = "binline", bins = 100, scale = 0.99, draw_baseline = T)+
  geom_segment(data=medians,aes(x=median,xend=median,y=as.integer(range)+2,
                                yend=as.integer(range)+1.6),col="black",lwd=1)+
  geom_segment(data=medians,aes(x=quant_low,xend=quant_low,y=as.integer(range)+2,
                                yend=as.integer(range)+1.6),col="black",linetype=6,lwd=1)+
  geom_segment(data=medians,aes(x=quant_high,xend=quant_high,y=as.integer(range)+2,
                                yend=as.integer(range)+1.6),col="black",linetype=6,lwd=1)+
  geom_text(data=monthdays,aes(x=breeding_day+15,y=y,label=month,fill=NA),col="black",size=2.5)+
  geom_point(data=data.frame(x=0,y=0),aes(x=x,y=y,fill=NA),col="grey")+
  geom_label(data=legend,aes(label=text),size=2.5)
 
nest_points <- ggplot(data=nest,aes(x=range,y=breeding_day))+
  theme_minimal()+
  theme(text=element_text(size=8),axis.text = element_text(size=8))+
  scale_color_manual(values=brewer.pal(3,"Dark2")[c(1,3,2)])+
  ylab("Days from Nov 1")+xlab("Range")+
  geom_sina(scale=F,maxwidth=.8,method="count",shape=21,aes(col=range))+
  geom_boxplot(fill=NA,outlier.colour = NA,notch = T)+
  #geom_violin(fill=NA,outlier.shape = NA,draw_quantiles = 0.5,scale = "count")+
  geom_signif(comparisons=list(c("Native","PNW")),
              map_signif_level = T,y_position = 320)

nest_circles <- ggplot(data=nest,aes(x=breeding_day,y=as.integer(range)+2,col=range))+coord_polar()+
  theme(text=element_text(size=8),
        panel.background = element_rect(color="white"),
        panel.grid.major=element_line(color="grey",size=0.25),
        axis.line = element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        legend.position = "none",
        legend.background = element_rect(fill = "white"),
        legend.title = element_blank())+
  xlab("")+ylab("")+
  scale_y_continuous(breaks=c(3,4,5),limits=c(1,6.5))+
  scale_x_continuous(breaks=c(1,32,60,91,121,152,182,213,244,274,305,335),limits=c(1,365))+
  scale_color_manual(values=brewer.pal(3,"Dark2")[c(1,3,2)])+
  geom_point(position=position_jitter(height=0.3),alpha=0.4)+
  geom_segment(data=medians,aes(x=median,xend=median,y=as.integer(range)+1.5,yend=as.integer(range)+2.5),
               col="black")+
  geom_segment(data=medians,aes(x=quant_low,xend=quant_low,y=as.integer(range)+1.5,
                                yend=as.integer(range)+2.5),col="black",linetype=6)+
  geom_segment(data=medians,aes(x=quant_high,xend=quant_high,y=as.integer(range)+1.5,
                                yend=as.integer(range)+2.5),col="black",linetype=6)+
  geom_text(data=monthdays,aes(x=breeding_day+15,y=y,label=month),col="black",size=2.5)+
  geom_label(data=legend,aes(label=text),size=2.5)

#png("figures/Figure_3.png",width=3,height=2.5,units = "in",res=600)
pdf("figures/Figure_3.pdf",width=3.5,height=3.5,useDingbats = F)
ggdraw()+
  draw_plot(nest_circles,-0.1,-0.1,1.15,1.15)
dev.off()

pdf("figures/phenology_fig_demo.pdf",width=6.5,height=6.5)
plot_grid(nest_histograms,nest_points,nest_circles,nest_histogram_circle)
dev.off()
