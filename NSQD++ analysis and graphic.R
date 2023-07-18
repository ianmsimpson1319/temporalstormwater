# simple random sampling with replacement #
# creates nsqd ++ data set for each pollutant #
install.packages(c("dplyr", "tidyr", "Kendall", "dunn.test", "doBy", "ggplot2", "ggpubr"))
library("dplyr")
library("tidyr")
library("Kendall")
library("dunn.test")
library("doBy")
library("ggplot2")
library("ggpubr")


setwd("C:/Users/isimpson/OneDrive - University of Tennessee/Documents/UTK/temporal stormwater quality/Data")
nsqd <- read.csv("NSQD plus.csv")
nsqd$Decade <- as.factor(nsqd$Decade)
nsqd$Decade_modified <- as.factor(nsqd$Decade_modified)

#rename columns#
names(nsqd)[names(nsqd)=="TSS..mg.L."]<-"TSS"
names(nsqd)[names(nsqd)=="TKN..mg.L."]<-"TKN"
names(nsqd)[names(nsqd)=="TN..mg.L."]<-"TN"
names(nsqd)[names(nsqd)=="TP..mg.L."]<-"TP"
names(nsqd)[names(nsqd)=="OP..mg.L."]<-"OP"
names(nsqd)[names(nsqd)=="OG..mg.L."]<-"OG"
names(nsqd)[names(nsqd)=="Cu..ug.L."]<-"Cu"
names(nsqd)[names(nsqd)=="Pb..ug.L."]<-"Pb"
names(nsqd)[names(nsqd)=="Zn..ug.L."]<-"Zn"



###############################################################################################################################
#                                                 create NSQD ++ data frames                                                  #
###############################################################################################################################

# TSS data set - NSQD++ #
solids <- nsqd[-c(2,8,13:20)]
solids <- subset(solids, Land.Use!="Freeway" & Land.Use!="Open Space" & Land.Use!="Institutional" & Land.Use!="Industrial")
solids<- solids[rowSums(is.na(solids))==0,]
solids <- subset(solids, NCDC.Region %in% c("Ohio Valley", "Southeast", "Southwest"))
set.seed(19950221)
solids <- solids %>%
  group_by(Land.Use, NCDC.Region, Decade_modified)%>%
  slice_sample(n=50, replace=T)

# TP data set - NSQD++ #
phosphorus <- nsqd[-c(2,8,12,13,14,16:20)]
phosphorus <- subset(phosphorus, Land.Use %in% c("Commercial", "Residential"))
phosphorus <- phosphorus[rowSums(is.na(phosphorus))==0,]
phosphorus <- subset(phosphorus, NCDC.Region %in% c("Ohio Valley", "Southeast", "Southwest"))
set.seed(19950221)
phosphorus <- phosphorus %>%
  group_by(Land.Use, NCDC.Region, Decade_modified)%>%
  slice_sample(n=50, replace=T)

# TKN data set - NSQD++ #
nitrogen <- nsqd[-c(2,8,12,14:20)]
nitrogen <- subset(nitrogen, Land.Use!="Freeway" & Land.Use!="Open Space" & Land.Use!="Institutional" & Land.Use!="Industrial")
nitrogen <- nitrogen[rowSums(is.na(nitrogen))==0,]
nitrogen <- subset(nitrogen, NCDC.Region %in% c("Ohio Valley", "Southeast", "Southwest"))
set.seed(19950221)
nitrogen <- nitrogen %>%
  group_by(Land.Use, NCDC.Region, Decade_modified)%>%
  slice_sample(n=50, replace=T)

# Copper data set - NSQD++ #
copper <- nsqd[-c(2,8,12:16,18:20)]
copper <- subset(copper, Land.Use=="Commercial")
copper <- copper[rowSums(is.na(copper))==0,]
copper <- subset(copper, NCDC.Region %in% c("Ohio Valley", "Southwest", "Southeast"))
copper <- subset(copper, Decade_modified!="4")
set.seed(19950221)
copper <- copper %>%
  group_by(Land.Use, NCDC.Region, Decade_modified)%>%
  slice_sample(n=50, replace=T)

# lead data set - NSQD++ #
lead <- nsqd[-c(2,8,12:17,19,20)]
lead <- subset(lead, Land.Use=="Residential")
lead <- lead[rowSums(is.na(lead))==0,]
lead <- subset(lead, NCDC.Region %in% c("Northwest", "South", "Southwest"))
lead <- subset(lead, Decade_modified %in% c("1", "2", "3"))
set.seed(19950221)
lead <- lead %>%
  group_by(Land.Use, NCDC.Region, Decade_modified)%>%
  slice_sample(n=50, replace=T)
  
# zinc data set - NSQD++ #
zinc <- nsqd[-c(2,8,12:18,20)]
zinc <- subset(zinc, Land.Use=="Commercial")
zinc <- zinc[rowSums(is.na(zinc))==0,]
zinc<-subset(zinc, NCDC.Region %in% c("Ohio Valley", "Southeast"))
set.seed(19950221)
zinc <- zinc %>%
  group_by(Land.Use, NCDC.Region, Decade_modified)%>%
  slice_sample(n=50, replace=T)


#       write .csvs           #
#write.csv(solids, "C:/Users/isimpson/OneDrive - University of Tennessee/Documents/UTK/temporal stormwater quality/Data/GitHub\\NSQD++_TSS.csv", row.names=F)
#write.csv(nitrogen, "C:/Users/isimpson/OneDrive - University of Tennessee/Documents/UTK/temporal stormwater quality/Data/GitHub\\NSQD++_TKN.csv", row.names=F)
#write.csv(phosphorus, "C:/Users/isimpson/OneDrive - University of Tennessee/Documents/UTK/temporal stormwater quality/Data/GitHub\\NSQD++_TP.csv", row.names=F)
#write.csv(copper, "C:/Users/isimpson/OneDrive - University of Tennessee/Documents/UTK/temporal stormwater quality/Data/GitHub\\NSQD++_Cu.csv", row.names=F)
#write.csv(lead, "C:/Users/isimpson/OneDrive - University of Tennessee/Documents/UTK/temporal stormwater quality/Data/GitHub\\NSQD++_Pb.csv", row.names=F)
#write.csv(zinc, "C:/Users/isimpson/OneDrive - University of Tennessee/Documents/UTK/temporal stormwater quality/Data/GitHub\\NSQD++_Zn.csv", row.names=F)

# test for normality #
shapiro.test(solids$TSS)
shapiro.test(phosphorus$TP)
shapiro.test(nitrogen$TKN)
shapiro.test(copper$Cu)
shapiro.test(lead$Pb)
shapiro.test(zinc$Zn)

###############################################################################################################################








###############################################################################################################################
#                                                Monotonic trend analysis                                                     #
###############################################################################################################################

#stats#
solids <- solids[order(as.Date(solids$Date, format="%m/%d/%Y")),]
MannKendall(solids$TSS)

phosphorus <- phosphorus[order(as.Date(phosphorus$Date, format="%m/%d/%Y")),]
MannKendall((phosphorus$TP))

nitrogen <- nitrogen[order(as.Date(nitrogen$Date, format="%m/%d/%Y")),]
MannKendall(nitrogen$TKN)

copper <- copper[order(as.Date(copper$Date, format="%m/%d/%Y")),]
MannKendall(copper$Cu)

lead <- lead[order(as.Date(lead$Date, format="%m/%d/%Y")),]
MannKendall(lead$Pb)

zinc <- zinc[order(as.Date(zinc$Date, format="%m/%d/%Y")),]
MannKendall(zinc$Zn)


###############################################################################################################################






###############################################################################################################################
#                                                    Step trend analysis                                                      #
###############################################################################################################################


dunn.test(solids$TSS, g=solids$Decade_modified, kw=T, label=T, table=T, alpha=0.1, method="sidak")
dunn.test(phosphorus$TP, g=phosphorus$Decade_modified, kw=T, label=T, table=T, alpha=0.1, method="sidak")
dunn.test(nitrogen$TKN, g=nitrogen$Decade_modified, kw=T, label=T, table=T, alpha=0.1, method="sidak")
dunn.test(copper$Cu, g=copper$Decade_modified, kw=T, label=T, table=T, alpha=0.1, method="sidak")
dunn.test(lead$Pb, g=lead$Decade_modified, kw=T, label=T, table=T, alpha=0.1, method="sidak")
dunn.test(zinc$Zn, g=zinc$Decade_modified, kw=T, label=T, table=T, alpha=0.1, method="sidak")

#plot#
#Figure 3#
TSSplot <- ggplot(solids, aes(as.Date(Date, format="%m/%d/%Y"), TSS))+
  geom_boxplot(aes(group=Decade_modified), na.rm=T, outlier.shape=NA, fill=NA, color="black", lwd=1, fatten=1)+
  #geom_smooth(na.rm=T, method="loess", formula=y~x, color="black")+
  #geom_point(aes(color=NCDC.Region, shape=Land.Use), na.rm=T)+
  #scale_shape_manual(values=c(16,8), name="LULC")+
  #scale_color_manual(values=c("cyan2", "purple", "coral"), name="NCDC Region")+
  theme_bw()+
  labs(x="Date", y="TSS EMC (mg/L)")+
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16, face="bold"), axis.title.x=element_blank())+
  coord_cartesian(ylim=c(0,450))
TSSplot

TPplot <- ggplot(phosphorus, aes(as.Date(Date, format="%m/%d/%Y"), TP))+
  geom_boxplot(aes(group=Decade_modified), na.rm=T, outlier.shape=NA, fill=NA, color="black", lwd=1, fatten=1)+
  #geom_smooth(na.rm=T, method="loess", formula=y~x, color="black")+
  #geom_point(aes(color=NCDC.Region, shape=Land.Use), na.rm=T)+
  #scale_shape_manual(values=c(16,8), name="LULC")+
  #scale_color_manual(values=c("cyan2", "purple", "coral"), name="NCDC Region")+
  theme_bw()+
  labs(x="Date", y="TP EMC (mg/L)")+
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16, face="bold"), axis.title.x=element_blank())+
  coord_cartesian(ylim=c(0,1.5))
TPplot

TKNplot <- ggplot(nitrogen, aes(as.Date(Date, format="%m/%d/%Y"), TKN))+
  geom_boxplot(aes(group=Decade_modified), na.rm=T, outlier.shape=NA, fill=NA, color="black", lwd=1, fatten=1)+
  #geom_smooth(na.rm=T, method="loess", formula=y~x, color="black")+
  #geom_point(aes(color=NCDC.Region, shape=Land.Use), na.rm=T)+
  #scale_shape_manual(values=c(16,8), name="LULC")+
  #scale_color_manual(values=c("cyan2", "purple", "coral"), name="NCDC Region")+
  theme_bw()+
  labs(x="Date", y="TKN EMC (mg/L)")+
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16, face="bold"), axis.title.x=element_blank())+
  coord_cartesian(ylim=c(0,6))
TKNplot

start="1980-01-01"
end="2021-07-01"
Cuplot <- ggplot(copper, aes(as.Date(Date, format="%m/%d/%Y"), Cu))+
  geom_boxplot(aes(group=Decade_modified), na.rm=T, outlier.shape=NA, fill=NA, color="black", lwd=1, fatten=1)+
  #geom_smooth(na.rm=T, method="loess", formula=y~x, color="black")+
  #geom_point(aes(color=NCDC.Region, shape=Land.Use), na.rm=T)+
  #scale_shape_manual(values=c(16,8), name="LULC")+
  #scale_color_manual(values=c("cyan2", "purple", "coral"), name="NCDC Region")+
  theme_bw()+
  labs(x="Date", y="Cu EMC (µg/L)")+
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16, face="bold"), axis.title.x=element_blank())+
  coord_cartesian(ylim=c(0,125))+
  scale_x_date(limits=as.Date(c(start,end)))
Cuplot

Pbplot <- ggplot(lead, aes(as.Date(Date, format="%m/%d/%Y"), Pb))+
  geom_boxplot(aes(group=Decade_modified), na.rm=T, outlier.shape=NA, fill=NA, color="black", lwd=1, fatten=1)+
  #geom_smooth(na.rm=T, method="loess", formula=y~x, color="black")+
  #geom_point(aes(color=NCDC.Region, shape=Land.Use), na.rm=T)+
  #scale_shape_manual(values=8, name="LULC")+
  #scale_color_manual(values=c("deepskyblue2", "bisque", "coral"), name="NCDC Region")+
  theme_bw()+
  labs(x="Date", y="Pb EMC (µg/L)")+
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16, face="bold"), axis.title.x=element_blank())+
  coord_cartesian(ylim=c(0,350))+
  scale_x_date(limits=as.Date(c(start,end)))
Pbplot

Znplot <- ggplot(zinc, aes(as.Date(Date, format="%m/%d/%Y"), Zn))+
  geom_boxplot(aes(group=Decade_modified), na.rm=T, outlier.shape=NA, fill=NA, color="black", lwd=1, fatten=1)+
  #geom_smooth(na.rm=T, method="loess", formula=y~x, color="black")+
  #geom_point(aes(color=NCDC.Region, shape=Land.Use), na.rm=T)+
  #scale_shape_manual(values=16, name="LULC")+
  #scale_color_manual(values=c("cyan2", "purple"), name="NCDC Region")+
  theme_bw()+
  labs(x="Date", y="Zn EMC (µg/L)")+
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16, face="bold"), axis.title.x=element_blank())+
  coord_cartesian(ylim=c(0,900))
Znplot

ggarrange(TSSplot, TPplot, TKNplot, Cuplot, Pbplot, Znplot, common.legend=T)


#decadal medians#
summaryBy(TSS ~ Decade_modified, data=solids, FUN=function(x) round(median(x),2))
summaryBy(TP ~ Decade_modified, data=phosphorus, FUN=function(x) round(median(x),2))
summaryBy(TKN ~ Decade_modified, data=nitrogen, FUN=function(x) round(median(x),2))
summaryBy(Cu ~ Decade_modified, data=copper, FUN=function(x) round(median(x),2))
summaryBy(Pb ~ Decade_modified, data=lead, FUN=function(x) round(median(x),2))
summaryBy(Zn ~ Decade_modified, data=zinc, FUN=function(x) round(median(x),2))

###############################################################################################################################


