
#Figure 2 - population growth
tmp <- subset(anhu,!is.na(anhu$best_model.X1))
map <- map_data("world");state <- map_data("state")
pal <- grDevices::colorRampPalette(color=c("steelblue4","skyblue","lightgoldenrod1","orangered","red4"),
                                   bias=1,space="rgb",interpolate="linear")
plotdata <- ddply(tmp,.(Name),summarize,
                  min_year=min(Count_yr),
                  exp_rate_p=exp_rate_p[1],
                  log_rate_p=log_rate_p[1],
                  log_K_p=log_K_p[1],
                  exp_growth_rate=exp_growth_rate[1],
                  log_growth_rate=log_growth_rate[1],
                  log_k=log_K[1],
                  best_model=best_model.X1[1],
                  second_model=best_model.X2[1],
                  long=Longitude[1],
                  lat=Latitude[1],
                  delta_aic=delta_aic[1],
                  aic_lin=aic_lin[1],
                  aic_exp=aic_exp[1],
                  aic_log=aic_log[1],
                  mse=mse[1],
                  mae=mae[1],
                  meanabund=mean(abundance_index,na.rm=T))
plotdata$best_model[plotdata$best_model=="Linear"] <- "Constant"
plotdata$second_model[plotdata$second_model=="Linear"] <- "Constant"
plotdata$best_model <- factor(plotdata$best_model,levels=c("Constant","Logistic","Exponential"))
plotdata$second_model <- factor(plotdata$second_model,levels=c("Constant","Logistic","Exponential"))
#set column to scale alpha if delta_aic is < 2
plotdata$aic_alpha_scale <- plotdata$delta_aic
plotdata$aic_alpha_scale[plotdata$aic_alpha_scale>2] <- 2
#plotdata <- arrange(plotdata,best_model)

plot1 <- ggplot()+theme_bw()+theme(panel.grid=element_blank())+coord_map()+
  #xlim(min(plotdata$long)-1,max(plotdata$long)+1)+ylim(min(plotdata$lat)-1,max(plotdata$lat)+1)+
  xlim(-130,-94)+ylim(27,52)+
  theme(legend.position = c(.025,.43),
        legend.background = element_blank(),
        legend.title = element_text(size=8),
        axis.ticks = element_blank(),
        axis.text=element_blank(),
        text=element_text(size=8),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank())+
  xlab("")+ylab("")+
  #scale_fill_gradientn(colors=pal(nrow(plotdata)))+
  #scale_fill_distiller(palette = "YlGnBu")+
  scale_fill_viridis(name="Population\nGrowth\nRate",direction = -1,option = "inferno")+
  geom_path(data=state,aes(x=long,y=lat,group=group),lwd=0.2,col="grey")+
  geom_path(data=map,aes(x=long,y=lat,group=group),lwd=0.2)+
  stat_summary_2d(data=plotdata,aes(x=long,y=lat,z=exp_growth_rate),bins=60)
  #geom_point(data=plotdata,aes(x=long,y=lat,col=exp_growth_rate),size=0.6)
plot1 <- plot1+guides(fill=guide_colourbar(barwidth = unit(4,"mm"),barheight = unit(20,"mm")))

# plot2 <- ggplot()+theme_bw()+theme(panel.grid=element_blank())+coord_map()+
#   #xlim(min(plotdata$long)-1,max(plotdata$long)+1)+ylim(min(plotdata$lat)-1,max(plotdata$lat)+1)+
#   xlim(-130,-94)+ylim(27,52)+
#   theme(legend.position = c(.025,.43),
#         legend.background = element_blank(),
#         legend.title = element_text(size=8),
#         axis.ticks = element_blank(),
#         axis.text=element_blank(),
#         text=element_text(size=8),
#         panel.border = element_blank(),
#         panel.background = element_blank(),
#         plot.background = element_blank())+
#   xlab("")+ylab("")+
#   #scale_fill_viridis(name="AIC(Exponential\nGrowth Model)",direction = 1,option = "viridis")+
#   #scale_color_distiller(name="AIC(Exponential\nGrowth Model)",direction = -1,palette="RdYlBu")+
#   scale_fill_gradientn(colors=rev(pal(nrow(plotdata))),name="AIC(Exponential\nGrowth Model)")+
#   geom_path(data=state,aes(x=long,y=lat,group=group),lwd=0.2,col="grey")+
#   geom_path(data=map,aes(x=long,y=lat,group=group),lwd=0.2)+
#   stat_summary_2d(data=plotdata,aes(x=long,y=lat,z=mae),bins=60)
#   #geom_point(data=plotdata,aes(x=long,y=lat,col=aic_exp),size=0.6)
# plot2 <- plot2+guides(fill=guide_colourbar(barwidth = unit(4,"mm"),barheight = unit(20,"mm")))
plot2 <- ggplot()+theme_bw()+theme(panel.grid=element_blank())+coord_map()+
  xlim(-130,-94)+ylim(27,52)+
  theme(legend.position = c(.04,.3),
        legend.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text=element_blank(),
        legend.text=element_text(size=8),
        legend.title=element_text(size=8),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank())+
  xlab("")+ylab("")+
  scale_color_manual(name="Model",values=c(brewer.pal(3,"Dark2")[3],brewer.pal(3,"Dark2")[2],brewer.pal(3,"Dark2")[1]))+
  geom_path(data=state,aes(x=long,y=lat,group=group),lwd=0.2,col="grey")+
  geom_path(data=map,aes(x=long,y=lat,group=group),lwd=0.2)+
  geom_point(data=plotdata[plotdata$delta_aic>=2,],aes(x=long,y=lat,col=best_model),size=.7)+
  geom_point(data=plotdata[plotdata$delta_aic<2,],aes(x=long,y=lat,col=best_model),size=0.8,shape=1)
  #geom_point(data=plotdata[plotdata$delta_aic<2,],aes(x=long,y=lat,col=second_model),shape=21,size=0.8,stroke=0.4)

plot2 <- plot2+guides(color=guide_legend(override.aes=list(size=4)))

best_model_summary <- ddply(anhu,.(state),function(e){
  logistic <- nrow(subset(e,best_model.X1=="Logistic"))/nrow(subset(e,!is.na(e$best_model.X1)))
  exponential <- nrow(subset(e,best_model.X1=="Exponential"))/nrow(subset(e,!is.na(e$best_model.X1)))
  constant <- nrow(subset(e,best_model.X1=="Linear"))/nrow(subset(e,!is.na(e$best_model.X1)))
  c(Logistic=logistic,Exponential=exponential,Linear=constant)
}) %>% melt()
best_model_summary <- subset(best_model_summary,state %in% c("CA","AZ","TX","OR","WA","BC"))
best_model_summary$state <- factor(best_model_summary$state,levels=c("CA","AZ","NV","NM","TX","OR","WA","BC"))
model_bar_plot <- ggplot(data=best_model_summary,aes(x=state,y=value,fill=variable))+
  theme(axis.title=element_blank(),
        text=element_text(size=7),
        axis.text=element_text(size=7),
        legend.position = "none",
        plot.background = element_rect(fill="white"))+
  scale_y_continuous(breaks=c(0,.5,1))+
  geom_bar(stat="identity")+
  scale_fill_brewer(palette = "Dark2",name="Best Model (AIC)")

mean(subset(best_model_summary,state %in% c("CA") & variable=="Exponential")$value)
mean(subset(best_model_summary,state %in% c("CA") & variable=="Linear")$value)
mean(subset(best_model_summary,state %in% c("OR","WA","BC") & variable=="Exponential")$value)
mean(subset(best_model_summary,state %in% c("TX","AZ","NM") & variable=="Exponential")$value)

dat <- subset(anhu,Name %in% c("El Paso","Santa Catalina Mountains","Tucson Valley",
                               "Santa Barbara","Santa Cruz County","Oakland","Redding",
                              "Eugene","Portland","Seattle","Nanaimo","Vancouver"))
dat$Name <- sapply(dat$Name,function(e){str_wrap(e,width=14)})
dat$Name <- apply(dat,1,function(e){if(!is.na(e[16]) & e[16]=="CA"){paste0(e[1],"*")}else{e[1]}})
dat$Name <- factor(dat$Name,levels=rev(c("El Paso","Santa Catalina\nMountains","Tucson Valley",
                                     "Santa Barbara*","Santa Cruz\nCounty*","Oakland*","Redding*",
                                     "Eugene","Portland","Seattle","Nanaimo","Vancouver")))
dat <- subset(dat,!is.na(Name))
zeros <- subset(dat,abundance_index==0)
dat <- subset(dat,abundance_index>0)
time_plots <- ggplot()+
  theme_minimal()+theme(strip.background = element_blank(),
                        strip.text = element_text(size=8),
                        axis.text.x=element_text(angle=45,hjust=1,vjust=1),
                        axis.title.x=element_blank(),
                        plot.background = element_blank())+
  xlim(1950,2017)+
  ylab("Birds per Party-Hour")+
  facet_wrap(~Name,ncol=4,scales="free_y")+xlab("Year")+
  geom_point(data=zeros,aes(x=year,y=abundance_index),col="grey",shape=1)+
  geom_point(data=dat,aes(x=year,y=abundance_index),col="black",shape=1)+
  geom_line(data=dat[dat$best_model.X1=="Exponential",],aes(x=year,y=exp_model),col="#D95F02")+
  geom_line(data=dat[dat$best_model.X1=="Exponential",],aes(x=year,y=exp_CI_low),col="#D95F02",linetype=2,lwd=0.35)+
  geom_line(data=dat[dat$best_model.X1=="Exponential",],aes(x=year,y=exp_CI_high),col="#D95F02",linetype=2,lwd=0.35)+
  geom_line(data=dat[dat$best_model.X1=="Logistic",],aes(x=year,y=log_model),col="#1B9E77")+
  geom_line(data=dat[dat$best_model.X1=="Logistic",],aes(x=year,y=log_CI_low),col="#1B9E77",linetype=2,lwd=0.35)+
  geom_line(data=dat[dat$best_model.X1=="Logistic",],aes(x=year,y=log_CI_high),col="#1B9E77",linetype=2,lwd=0.35)+
  geom_line(data=dat[dat$best_model.X1=="Linear",],aes(x=year,y=lin_model),col="#7570B3")+
  geom_line(data=dat[dat$best_model.X1=="Linear",],aes(x=year,y=lin_CI_low),col="#7570B3",linetype=2,lwd=0.35)+
  geom_line(data=dat[dat$best_model.X1=="Linear",],aes(x=year,y=lin_CI_high),col="#7570B3",linetype=2,lwd=0.35)

#png("figures/Figure_2.png",width=6,height=6,units="in",res=600)
pdf("figures/Figure_2_no_limits_constantlinmodel.pdf",width=6,height=6,useDingbats = F)
ggdraw()+
  draw_plot(plot1,0,.55,.5,.45)+
  draw_plot(plot2,.5,.55,.5,.45)+
  draw_plot(model_bar_plot,.722,.79,.26,.16)+
  draw_plot(time_plots,0,0,1,.6)+
  draw_plot_label(c("A","B","C"),x=c(0,.5,0),y=c(.98,.98,.6))
dev.off()


########################################## Supplementary Figures #################################

#Supplementary Figure 1 - expanded first report map
map <- map_data("world");state <- map_data("state")
pdf("Figure_S1.pdf",width=6,height=4.65,useDingbats = F)
#png("Figure_S1.png",width=6,height=4.65,units="in",res=600)
p <- ggplot()+coord_map()+
  theme_bw()+theme(panel.grid=element_blank(),
                   legend.position = "right",
                   text=element_text(size=8),
                   panel.border = element_blank(),
                   axis.text = element_blank(),
                   axis.ticks = element_blank(),
                   axis.title = element_blank(),
                   strip.background = element_blank())+
  facet_wrap(~source,nrow=2)+
  #xlim(-132,-94)+ylim(27,54)+
  ylim(min(combo$lat)-1,max(combo$lat)+1)+xlim(min(combo$long)-1,max(combo$long)+1)+
  xlim(-152,-60)+ylim(20,64)+
  scale_fill_viridis(name="First\nOccurrence\nRecord")+
  geom_path(data=state,aes(x=long,y=lat,group=group),lwd=0.1,col="grey")+
  geom_path(data=map,aes(x=long,y=lat,group=group),lwd=0.2)+
  stat_summary_2d(data=combo,aes(x=long,y=lat,z=year),fun="min",bins=70,alpha=0.9)
p <- p+guides(fill=guide_colourbar(barwidth = unit(4,"mm"),barheight = unit(20,"mm")))
print(p)
dev.off()

#Supplementary Figure 2-7 - faceted plots of observed v predicted abundance over time, split by state and model type (takes a minute or two)
anhu <- ddply(anhu,.(Name),function(e){e$state <- unique(e$state)[1];e})
anhu2 <- subset(anhu,state %in% c("CA","AZ","TX","WA","OR","BC","AK"))
for(i in unique(anhu2$state)){
  dat2 <- subset(anhu2,state==i)
  zeros <- subset(dat2,abundance_index==0)
  dat2 <- subset(dat2,abundance_index>0)
  if(nrow(dat2)>1){
    pdf(paste0("sup_figs/anhu_cbc_best_model_",i,".pdf"),width=7.5,height=ceiling((length(unique(dat2$Name))/4))*1.5,useDingbats = F)
    print(ggplot()+
            theme_minimal()+theme(strip.background = element_blank(),
                                  strip.text = element_text(size=8),
                                  axis.text.x=element_text(angle=45,hjust=1,vjust=1))+
            facet_wrap(~Name,scales="free_y",ncol=4)+xlab("Year")+
            xlim(1950,2017)+
            geom_point(data=zeros,aes(x=year,y=abundance_index),col="grey",shape=1)+
            geom_point(data=dat2,aes(x=Count_yr+1900,y=abundance_index),col="black",shape=1)+
            geom_line(data=dat2[dat2$best_model.X1=="Exponential",],aes(x=Count_yr+1900,y=exp_model),col="#D95F02")+
            geom_line(data=dat2[dat2$best_model.X1=="Exponential",],aes(x=Count_yr+1900,y=exp_CI_low),col="#D95F02",linetype=2,lwd=0.35)+
            geom_line(data=dat2[dat2$best_model.X1=="Exponential",],aes(x=Count_yr+1900,y=exp_CI_high),col="#D95F02",linetype=2,lwd=0.35)+
            geom_line(data=dat2[dat2$best_model.X1=="Logistic",],aes(x=Count_yr+1900,y=log_model),col="#1B9E77")+
            geom_line(data=dat2[dat2$best_model.X1=="Logistic",],aes(x=Count_yr+1900,y=log_CI_low),col="#1B9E77",linetype=2,lwd=0.35)+
            geom_line(data=dat2[dat2$best_model.X1=="Logistic",],aes(x=Count_yr+1900,y=log_CI_high),col="#1B9E77",linetype=2,lwd=0.35)+
            geom_line(data=dat2[dat2$best_model.X1=="Linear",],aes(x=Count_yr+1900,y=lin_model),col="#7570B3")+
            geom_line(data=dat2[dat2$best_model.X1=="Linear",],aes(x=Count_yr+1900,y=lin_CI_low),col="#7570B3",linetype=2,lwd=0.35)+
            geom_line(data=dat2[dat2$best_model.X1=="Linear",],aes(x=Count_yr+1900,y=lin_CI_high),col="#7570B3",linetype=2,lwd=0.35)+
            geom_line(data=dat2[dat2$best_model.X2=="Exponential" & dat2$delta_aic<2,],aes(x=Count_yr+1900,y=exp_model),col="#D95F02")+
            geom_line(data=dat2[dat2$best_model.X2=="Exponential" & dat2$delta_aic<2,],aes(x=Count_yr+1900,y=exp_CI_low),col="#D95F02",linetype=2,lwd=0.35)+
            geom_line(data=dat2[dat2$best_model.X2=="Exponential" & dat2$delta_aic<2,],aes(x=Count_yr+1900,y=exp_CI_high),col="#D95F02",linetype=2,lwd=0.35)+
            geom_line(data=dat2[dat2$best_model.X2=="Logistic" & dat2$delta_aic<2,],aes(x=Count_yr+1900,y=log_model),col="#1B9E77")+
            geom_line(data=dat2[dat2$best_model.X2=="Logistic" & dat2$delta_aic<2,],aes(x=Count_yr+1900,y=log_CI_low),col="#1B9E77",linetype=2,lwd=0.35)+
            geom_line(data=dat2[dat2$best_model.X2=="Logistic" & dat2$delta_aic<2,],aes(x=Count_yr+1900,y=log_CI_high),col="#1B9E77",linetype=2,lwd=0.35)+
            geom_line(data=dat2[dat2$best_model.X2=="Linear" & dat2$delta_aic<2,],aes(x=Count_yr+1900,y=lin_model),col="#7570B3")+
            geom_line(data=dat2[dat2$best_model.X2=="Linear" & dat2$delta_aic<2,],aes(x=Count_yr+1900,y=lin_CI_low),col="#7570B3",linetype=2,lwd=0.35)+
            geom_line(data=dat2[dat2$best_model.X2=="Linear" & dat2$delta_aic<2,],aes(x=Count_yr+1900,y=lin_CI_high),col="#7570B3",linetype=2,lwd=0.35)
    )
    dev.off()
  }
}


#histograms of growth rates
ggplot(data=anhu,aes(x=exp_growth_rate,y=state))+geom_density_ridges()







