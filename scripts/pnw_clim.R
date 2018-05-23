#pnw climate charts
library(zoo)
clim <- read.csv("~/Downloads/1340303.csv")
clim$DATE <- as.Date(as.character(clim$DATE),format="%Y-%m-%d")
# clim$row <- 1:nrow(clim)
clim <- subset(clim,NAME %in% c("EUGENE MAHLON SWEET FIELD, OR US","MONROE, WA US") & !is.na(TMIN))

clim$rm <- rollmean(clim$TMIN,k = 10,fill=NA)
png("pnw_mintemp_plots.png",units="in",width=6,height=4,res=600)
ggplot(data=clim,aes(x=DATE,y=rm))+
  theme_minimal()+theme(strip.background = element_blank())+
  facet_wrap(~NAME)+
  geom_line(lwd=0.2)
dev.off()
