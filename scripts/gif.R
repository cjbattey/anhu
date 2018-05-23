map <- map_data("world");states <- map_data("state")
pal <- colorRampPalette(rev(brewer.pal(11, "RdYlBu")))
sites <- subset(anhu,Name %in% c("Nanaimo","Seattle","Tacoma","Portland","Salem","Redding","Oakland","Santa Barbara"))
sites$Name <- factor(sites$Name,levels=c("Nanaimo","Seattle","Tacoma","Portland","Salem","Redding","Oakland","Santa Barbara"))
for(i in 1950:2016){
  dat <- subset(anhu,year==i)
  p <- ggplot()+coord_map()+theme_minimal()+
    theme(legend.position = c(0.1,0.4),
          legend.background = element_blank(),
          text=element_text(size=8),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank())+
    xlim(-130,-105)+ylim(27,52)+
    scale_color_gradientn(name="growth\nrate",colors=pal(100),limits=c(min(anhu$exp_growth_rate),max(anhu$exp_growth_rate)))+
    scale_size_continuous(name="abundance\nindex",limits=c(0,4))+
    geom_path(data=states,aes(x=long,y=lat,group=group),lwd=0.3,col="grey")+
    geom_path(data=map,aes(x=long,y=lat,group=group),lwd=0.5)+
    geom_point(data=dat,aes(x=Longitude,y=Latitude,size=abundance_index,col=exp_growth_rate),alpha=0.9)+
    geom_point(data=dat,aes(x=Longitude,y=Latitude,size=abundance_index),shape=21,stroke=0.3,alpha=0.3)+
    geom_segment(aes(x=-120,xend=-110,y=51,yend=51))+
    annotate(geom="text",x=-120,y=50,label="1950")+
    annotate(geom="text",x=-110,y=50,label="2016")+
    annotate(geom="point",x=-120+((dat$year[1]-1950)*.1515152),y=51)
  
dat2 <- subset(anhu,Name %in% c("San Bernardino Valley","Santa Barbara","Oakland","Redding",
                               "Salem","Portland","Seattle","Vancouver"))
dat2$Name <- sapply(dat2$Name,function(e){str_wrap(e,width=13)})
dat2$Name <- factor(dat2$Name,levels=rev(c("San\nBernardino\nValley","Santa Barbara","Oakland","Redding",
                                           "Salem","Portland","Seattle","Vancouver")))
dat2 <- ddply(dat2,.(Name),function(e){
  minyear <- min(e$year)
  if(minyear>1950){
    missyears <- e[rep(1,minyear-1950),]
    missyears$abundance_index <- 0
    missyears$exp_model <- NA
    missyears$year <- 1950:(minyear-1)
    rbind(e,missyears)
  } else {
    e
  }
})

t <- ggplot()+
    theme_minimal()+theme(strip.background = element_blank(),
                          strip.text = element_text(size=8),
                          axis.text.x=element_text(angle=45,hjust=1,vjust=1),
                          axis.title.x=element_blank(),
                          plot.background = element_blank())+
    ylab("Birds per Party-Hour")+xlim(1950,2016)+
    facet_wrap(~Name,ncol=2,scales="free_y")+xlab("Year")+
    geom_point(data=subset(dat2,year<=i & abundance_index==0),aes(x=year,y=abundance_index),col="grey",shape=1)+
    geom_point(data=subset(dat2,year<=i & abundance_index>0),aes(x=year,y=abundance_index),col="black",shape=1)+
    geom_line(data=subset(dat2,year<=i),aes(x=year,y=exp_model),col="red")


  png(paste0("gif/",i,".png"),width=7,height=5,units = "in",res=300)
  #print(p)
  print(ggdraw()+
    draw_plot(p,0,0,0.6,1)+
    draw_plot(t,0.58,.05,.42,.9))
  dev.off()
}
