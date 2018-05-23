#hindcast exponential models to 1930
anhu2 <- ddply(anhu,.(Name),function(e){
  site <- e$Name[1]
  model <- exponential_models[[site]]
  minyear <- min(e$year)
  newdat <- e[rep(1,minyear-1930),]
  newdat$site_yr <- (1930-min(e$year)):-1
  newdat$abundance_index <- NA
  newdat$year <- 1930:(min(e$year)-1)
  newdat$exp_model <- predict(model,newdat)
  return(rbind(e,newdat))
})
d <- subset(anhu2,Latitude>42 & best_model=="Exponential" & delta_aic>2) %>% 
  ddply(.(Name,Longitude,Latitude),summarize,minyear=min(year[exp_model>=min(anhu$abundance_index)]))
ggplot()+theme_bw()+theme(panel.grid=element_blank())+coord_map()+
  xlim(-130,-120)+ylim(42,52)+
  theme(legend.position = c(.025,.43),
        legend.background = element_blank(),
        legend.title = element_text(size=8),
        axis.ticks = element_blank(),
        axis.text=element_blank(),
        text=element_text(size=8),
        panel.border = element_blank())+
  xlab("")+ylab("")+
  scale_color_viridis(name="Population\nGrowth\nRate",direction = -1,option = "inferno")+
  geom_path(data=state,aes(x=long,y=lat,group=group),lwd=0.1,col="grey")+
  geom_path(data=map,aes(x=long,y=lat,group=group),lwd=0.2)+
  geom_point(data=d,aes(x=Longitude,y=Latitude,col=minyear),size=4,alpha=0.8)