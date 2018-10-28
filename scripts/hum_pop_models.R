#analysis of range shifts in Anna's hummingbird, ~1900-present
library(ggplot2);library(data.table);library(plyr);library(raster);
library(magrittr);library(viridis);library(minpack.lm);library(gridExtra);
library(scales);library(cowplot);library(propagate);library(foreach);
library(doMC);library(stringr);library(ggforce);library(RColorBrewer)
source("scripts/ggthemes.R")
registerDoMC(cores=8)

#################################### load data #####################################
#load and format CBC data
cbc <- read.csv("data/CBC/Battey - CBC_Circle_Species_Report_SQL_updated.csv",stringsAsFactors = F)
cbc$year <- cbc$Count_yr+1899
cbc$state <- sapply(cbc$Subnational_code,function(e) strsplit(e,split="-") %>% unlist() %>% .[2])
cbc$surveyID <- paste(cbc$Name,cbc$Count_yr,sep="_")
effort1 <- read.csv("data/CBC/Battey - CBC_Effort_Report_SQL_updated-1.csv",stringsAsFactors = F)
effort1$surveyID <- paste(effort1$Name,effort1$Count_yr,sep="_")
effort3 <- read.csv("data/CBC/ANHU-CBC_Effort_Report_SQL_updated-1.csv",stringsAsFactors = F)
effort3$surveyID <- paste(effort3$Name,effort3$Count_yr,sep="_")
effort <- rbind(effort1,effort3[effort3$surveyID %in% effort1$surveyID==F,])
effort <- effort[!duplicated(paste(effort$Name,effort$Count_yr)),]
effort2 <- read.csv("data/CBC/Battey - CBC_Effort_Report_SQL_updated-2.csv",stringsAsFactors = F)
effort2$surveyID <- paste(effort2$Name,effort2$Count_yr,sep="_")
effort4 <- read.csv("data/CBC/ANHU-CBC_Effort_Report_SQL_updated-2.csv",stringsAsFactors = F)
effort4$surveyID <- paste(effort4$Name,effort4$Count_yr,sep="_")
effort2 <- rbind(effort2,effort4[effort4$surveyID %in% effort2$surveyID==F,])
effort2 <- ddply(effort2,.(Name,Count_yr),summarize,Distance=sum(as.numeric(Distance)),Hours=sum(as.numeric(Hours)))
effort <- merge(effort2,effort,by=c("Name","Count_yr"))
cbc <- merge(cbc,effort,by=c("Name","Count_yr"),all.x=T,all.y=T)

#cbc filters and abundance index
cbc$Hours[cbc$Hours==0] <- NA
cbc_dropped <- subset(cbc,is.na(Hours)|is.na(how_many))
cbc <- subset(cbc,SCI_NAME=="Calypte anna" & !is.na(how_many) & !is.na(Hours))
cbc$abundance_index <- cbc$how_many/cbc$Hours

#cbc effort for no-report years
eff <- read.csv("data/CBC/AllYears-CBC_Effort_Report_no_obs.csv",stringsAsFactors = F)
eff <- subset(eff,Name %in% unique(cbc$Name))
eff$surveyID <- paste(eff$Name,eff$Count_yr,sep="_")
eff <- ddply(eff,.(Name,Count_yr,surveyID),summarize,Hours=sum(as.numeric(Hours)))
eff$Hours[eff$Hours==0] <- NA
eff$Hours <- as.numeric(eff$Hours)
eff$how_many <- 0
eff$abundance_index <- eff$how_many/eff$Hours
eff$year <- eff$Count_yr+1899
eff <- subset(eff,surveyID %in% cbc$surveyID.x==F)
eff$SCI_NAME <- "Calypte anna"

#get GBIF and BBS data
gbif <- fread("data/gbif/occurrence.txt",data.table = F)
gbif <- gbif[,c("collectionID","datasetID","institutionCode","basisOfRecord","individualCount","year","month","day",
                "verbatimEventDate","stateProvince","county","municipality","locality","verbatimLocality",
                "verbatimElevation","decimalLatitude","decimalLongitude","coordinateUncertaintyInMeters")]
gbif$basisOfRecord <- factor(gbif$basisOfRecord)
gbif <- subset(gbif,coordinateUncertaintyInMeters<10000|is.na(coordinateUncertaintyInMeters))
bbs <- fread("data/BBS/anhu_counts.csv")
routes <- fread("data/BBS/route_locs.csv")
bbs <- join(bbs,routes,by=c("Country","State","Route"))
bbs_yrs_per_site <- ddply(bbs,.(Country,State,Route),summarize,n=length(Latitude))
bbs_good_sites <- subset(bbs_yrs_per_site,n>=10)
bbs <- subset(bbs,paste(bbs$State,bbs$Route) %in% paste(bbs_good_sites$State,bbs_good_sites$Route))
bbs_first_report_yr <- ddply(bbs,.(State,Route),summarize,first_report_yr=min(Year))
bbs <- merge(bbs,bbs_first_report_yr,by=c("State","Route"))
bbs$site_yr <- bbs$Year-bbs$first_report_yr+1

#combined dataset
combo <- data.frame(long=c(cbc$Longitude,gbif$decimalLongitude,bbs$Longitude),
                    lat=c(cbc$Latitude,gbif$decimalLatitude,bbs$Latitude),
                    year=c(cbc$year,gbif$year,bbs$Year),
                    month=c(rep(12,nrow(cbc)),gbif$month,rep(6,nrow(bbs))),
                    state=c(cbc$state,gbif$stateProvince,bbs$State),
                    source=c(rep("CBC",nrow(cbc)),as.character(gbif$basisOfRecord),rep("BBS",nrow(bbs))))
combo$source <- as.character(combo$source)
combo <- subset(combo,source %in% c("CBC","BBS","HUMAN_OBSERVATION","PRESERVED_SPECIMEN"))
combo$source[combo$source=="HUMAN_OBSERVATION"] <- "eBird"
combo$source[combo$source=="PRESERVED_SPECIMEN"] <- "Museum Specimens"
combo$source <- factor(combo$source)
combo <- subset(combo,!is.na(lat) & lat != 0 & long != 0 & !is.na(year))

################################ fit demographic models to CBC data #############################
anhu <- cbc

#anhu <- read.csv("anhu_CBC_model_params.csv")
anhu <- subset(anhu,!is.infinite(anhu$abundance_index) & !is.na(anhu$abundance_index) & year>=1950)
anhu <- ddply(anhu,.(Name),function(e) {if(nrow(e)>=15){
  #e <- e[e$abundance_index<(mean(e$abundance_index)+2*sd(e$abundance_index)),]
  e <- e[e$abundance_index != min(e$abundance_index) & e$abundance_index != max(e$abundance_index),]
  e$site_yr <- e$Count_yr-min(e$Count_yr)+1
  e}}) 

logistic_models <- dlply(anhu,.(Name),
                         function(e) {
                           model <- try(nlsLM(formula=abundance_index~(K*N0)/((K-N0)*exp(-r*site_yr)+N0), 
                                              data=e,
                                              start=list(K=2,r=0.01,N0=.01),
                                              #lower=c(0,-1,min(anhu$abundance_index)),
                                              #upper=c(max(anhu$abundance_index),10,10),
                                              control=nls.control(maxiter=1000)))})
logistic_models <- logistic_models[summary(logistic_models)[,2]=="nls"] #drop models that failed to optimize
anhu <- subset(anhu,Name %in% names(logistic_models)) #subset original data to preserve row order for easy merging

exponential_models <- dlply(anhu,.(Name),
                            function(e) {
                              model <- try(nlsLM(formula=abundance_index~N0*exp(rate*site_yr), 
                                                 data=e,
                                                 start=list(rate=0.01,N0=.01),
                                                 #lower=c(-1,min(anhu$abundance_index)),
                                                 #upper=c(10,10),
                                                 control=nls.control(maxiter=1000)))})
# exponential_models <- exponential_models[summary(exponential_models)[,2]=="nls"] #drop models that failed to optimize
# anhu <- subset(anhu,Name %in% names(exponential_models)) #subset original data to preserve row order for easy merging
# logistic_models <- logistic_models[names(logistic_models) %in% names(exponential_models)] #in case some models also fail exponential fits

linear_models <- dlply(anhu,.(Name),
                       function(e) {
                         model <- try(nlsLM(formula=abundance_index~rate*site_yr+N0,
                                            data=e,
                                            start=list(rate=0.01,N0=0.01),
                                            #lower=c(-2,min(anhu$abundance_index)),
                                            #upper=c(10,10),
                                            control=nls.control(maxiter=1000)))})
# linear_models <- dlply(anhu,.(Name),
#                        function(e) {
#                          model <- try(nlsLM(formula=abundance_index~N0,
#                                             data=e,
#                                             start=list(N0=0.3),
#                                             lower=c(min(anhu$abundance_index)),
#                                             upper=c(10),
#                                             control=nls.control(maxiter=1000)))})

#get confidence intervals for all models (takes ~2 minutes on 8 cores)
registerDoMC(cores=8)
nlsCIlog <- function(i,nboot=100){
  model <- logistic_models[[i]]
  dat <- subset(anhu,Name==names(logistic_models)[[i]])
  pred <- predict(model,dat)
  bootset <- data.frame(matrix(nrow=nrow(dat),ncol=nboot))
  for(i in 1:nboot){
    boot <- dat[sample(1:nrow(dat),nrow(dat),replace = T),] 
    m <- try(nlsLM(formula=abundance_index~(K*N0)/((K-N0)*exp(-r*site_yr)+N0), 
                   data=boot,
                   start=list(K=2,r=0.01,N0=.01),
                   #lower=c(0,-1,min(anhu$abundance_index)),
                   #upper=c(max(anhu$abundance_index),10,10),
                   control=nls.control(maxiter=1000)))
    if(class(m)=="nls"){
      bootset[,i] <- predict(m,dat)
    }
  }
  low <- apply(bootset,1,function(e) quantile(e,0.025,na.rm=T))
  high <- apply(bootset,1,function(e) quantile(e,0.975,na.rm=T))
  print("done!")
  return(data.frame(log_model=pred,log_CI_low=low,log_CI_high=high))
}
nlsCIexp <- function(i,nboot=100){
  model <- exponential_models[[i]]
  dat <- subset(anhu,Name==names(exponential_models)[[i]])
  pred <- predict(model,dat)
  bootset <- data.frame(matrix(nrow=nrow(dat),ncol=nboot))
  for(i in 1:nboot){
    boot <- dat[sample(1:nrow(dat),nrow(dat),replace = T),] 
    m <- try(nlsLM(formula=abundance_index~N0*exp(rate*site_yr), 
                   data=boot,
                   start=list(rate=0.01,N0=.01),
                   #lower=c(-1,min(anhu$abundance_index)),
                   #upper=c(10,10),
                   control=nls.control(maxiter=1000)))
    if(class(m)=="nls"){
      bootset[,i] <- predict(m,dat)
    }
  }
  low <- apply(bootset,1,function(e) quantile(e,0.025,na.rm=T))
  high <- apply(bootset,1,function(e) quantile(e,0.975,na.rm=T))
  return(data.frame(exp_model=pred,exp_CI_low=low,exp_CI_high=high))
}
nlsCIlin <- function(i,nboot=100){
  model <- linear_models[[i]]
  dat <- subset(anhu,Name==names(logistic_models)[[i]])
  pred <- predict(model,dat)
  bootset <- data.frame(matrix(nrow=nrow(dat),ncol=nboot))
  for(i in 1:nboot){
    boot <- dat[sample(1:nrow(dat),nrow(dat),replace = T),] 
    # m <- try(nlsLM(formula=abundance_index~N0,
    #                data=boot,
    #                start=list(N0=0.01),
    #                lower=c(min(anhu$abundance_index)),
    #                upper=c(10),
    #                control=nls.control(maxiter=1000)))
    m <- try(nlsLM(formula=abundance_index~rate*site_yr+N0,
                   data=boot,
                   start=list(rate=0.01,N0=0.01),
                   #lower=c(-2,min(anhu$abundance_index)),
                   #upper=c(10,10),
                   control=nls.control(maxiter=1000)))
    if(class(m)=="nls"){
      bootset[,i] <- predict(m,dat)
    }
  }
  low <- apply(bootset,1,function(e) quantile(e,0.025,na.rm=T))
  high <- apply(bootset,1,function(e) quantile(e,0.975,na.rm=T))
  return(data.frame(lin_model=pred,lin_CI_low=low,lin_CI_high=high))
}
log_pred <- foreach(i=1:length(logistic_models),.combine="rbind") %dopar% nlsCIlog(i)
anhu <- cbind(anhu,log_pred)
exp_pred <- foreach(i=1:length(logistic_models),.combine="rbind") %dopar% nlsCIexp(i)
anhu <- cbind(anhu,exp_pred)
lin_pred <- foreach(i=1:length(logistic_models),.combine="rbind") %dopar% nlsCIlin(i)
anhu <- cbind(anhu,lin_pred)

#select the best model for each circle with AIC
best_models <- data.frame(matrix(ncol=3));delta_aic <- c()
aic_logistic <- c();aic_exponential <- c();aic_linear <- c()
Names <- c()
for(i in 1:length(linear_models)){
  aic <- AIC(logistic_models[[i]],
             exponential_models[[i]],
             linear_models[[i]])
  aic_logistic[i] <- aic$AIC[1]
  aic_exponential[i] <- aic$AIC[2]
  aic_linear[i] <- aic$AIC[3]
  aic$model <- c("Logistic","Exponential","Linear")
  aic <- arrange(aic,AIC)
  delta_aic[i] <- aic$AIC[2]-aic$AIC[1]
  best_models[i,] <- aic$model
  Names[i] <- names(linear_models)[i]
}
best_models <- data.frame(Name=Names,best_model=best_models,delta_aic=delta_aic,
                          aic_log=aic_logistic,aic_exp=aic_exponential,aic_lin=aic_linear)
anhu <- merge(anhu,best_models,by="Name")

model_coeffs <- data.frame(Name=names(exponential_models),
                           exp_growth_rate=sapply(exponential_models,function(e) coefficients(e)[1]),
                           log_growth_rate=sapply(logistic_models,function(e) coefficients(e)[2]),
                           log_K=sapply(logistic_models,function(e) coefficients(e)[1]),
                           lin_slope=sapply(linear_models,function(e) coefficients(e)[1]),
                           exp_N0=sapply(exponential_models,function(e) coefficients(e)[2]),
                           exp_rate_p=sapply(exponential_models,function(e) summary(e)$coeff[7]),
                           log_rate_p=sapply(logistic_models,function(e) summary(e)$coeff[11]),
                           log_K_p=sapply(logistic_models,function(e) summary(e)$coeff[10]),
                           lin_rate_p=sapply(linear_models,function(e) summary(e)$coeff[7]))
anhu <- merge(anhu,model_coeffs,by="Name")

#are nw growth rates related to the date of colonization?
nw <- subset(anhu,abundance_index>0 & !is.na(abundance_index))
nw <- ddply(anhu,.(Name),summarize,first_report=min(year),rate=exp_growth_rate[1])
lm(rate~first_report,nw) %>% summary()
ggplot(data=nw,aes(x=first_report,y=rate))+
  theme(axis.text=element_text(size=8),
        axis.title = element_text(size=8))+
  geom_point(shape=1)+
  geom_smooth(method="lm",col="black",lwd=0.75)

#are nw rates higher than native rates? 
nw <- subset(anhu,state %in% c("OR","WA","BC"))
nw <- ddply(nw,.(Name),summarize,rate=exp_growth_rate[1])
nw$range <- "PNW"
ca <- subset(anhu,state=="CA")
ca <- ddply(ca,.(Name),summarize,rate=exp_growth_rate[1])
ca$range <- "Native"
sw <- subset(anhu,state %in% c("AZ","NV","NM","TX"))
sw <- ddply(sw,.(Name),summarize,rate=exp_growth_rate[1])
sw$range <- "SW"
nwc <- rbind(nw,ca,sw)
wilcox.test(sw$rate,ca$rate,conf.int=T) #nope for the SW
wilcox.test(nw$rate,ca$rate,conf.int=T) #yep for the PNW

pdf("figures/growth_rate_boxplots.pdf",width=3.5,height=3,useDingbats = F)
p <- ggplot(data=nwc,aes(x=range,y=rate))+
  ylab("Population Growth Rate")+theme(text=element_text(size=10),axis.text = element_text(size=10))+
  geom_sina(shape=21,stroke=0.5,col="grey")+geom_boxplot(notch=T,fill=NA,outlier.colour = NA)
print(p)
dev.off()

#add zero-report years 
anhu <- join(anhu,eff,type="full",by=c("Name","year","Count_yr"))



