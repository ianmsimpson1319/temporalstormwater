install.packages(c("ggplot2", "scales", "lubridate", "date", "plyr", "dplyr", "tidyr", "Kendall", "reshape2", "cowplot", "ggpubr", "doBy", "reshape2", "reshape"))
library("ggplot2")
library("scales")
library("lubridate")
library('date')
library("plyr")
library("dplyr")
library("tidyr")
library("Kendall")
library("reshape")
library("cowplot")
library("ggpubr")
library("doBy")
library("reshape2")
library("reshape")


setwd("C:/Users/isimpson/OneDrive - University of Tennessee/Documents/UTK/temporal stormwater quality/Data")
nsqd <- read.csv("NSQD plus.csv")

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


## temporal water quality paper ##
# section 2.1 #
ddply(nsqd, .(State), nrow)
nsqd$Decade <- as.factor(nsqd$Decade)
ddply(nsqd, .(Decade), nrow)
nsqd$Decade_modified <- as.factor(nsqd$Decade_modified)
ddply(nsqd, .(Land.Use), nrow)
############################################################



####decade plots####
#Figure 2#

#TNplot<- ggplot(nsqd, aes(as.Date(Date, format="%m/%d/%Y"), TN))+
  geom_point(aes(color=NCDC.Region, shape=Land.Use))+
  geom_boxplot(aes(group=Decade), na.rm=T, outlier.shape=NA, fill=NA, color="black", lwd=1, fatten=1)+
  geom_smooth(na.rm=T, method="loess", formula=y~x, color="black")+
  theme_bw()+
  labs(x="Date", y="TN EMC (mg/L)")+
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16, face="bold"), axis.title.x=element_blank())+
  coord_cartesian(ylim=c(0,10))+
  scale_shape_manual(values=c(16, 17, 15, 3, 18, 8), name="LULC")+
  scale_color_manual(values=c("chartreuse", "deepskyblue2", "cyan2", "bisque", "purple", "coral", "deeppink", "grey"), name="NCDC Region")
#TNplot


TSSplot <- ggplot(nsqd, aes(as.Date(Date, format="%m/%d/%Y"), TSS))+
  geom_point(aes(color=NCDC.Region, shape=Land.Use), na.rm=T)+
  geom_boxplot(aes(group=Decade), na.rm=T, outlier.shape=NA, fill=NA, color="black", lwd=1, fatten=1)+
  geom_smooth(na.rm=T, method="loess", formula=y~x, color="black")+
  theme_bw()+
  labs(x="Date", y="TSS EMC (mg/L)")+
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16, face="bold"), axis.title.x=element_blank())+
  coord_cartesian(ylim=c(0,350))+
  scale_shape_manual(values=c(16, 17, 15, 3, 18, 8), name="LULC")+
  scale_color_manual(values=c("chartreuse", "deepskyblue2", "cyan2", "bisque", "purple", "coral", "deeppink", "grey"), name="NCDC Region")+
  theme(legend.title=element_text(size=16, face="bold"), legend.text=element_text(size=16))
TSSplot

TKNplot<- ggplot(nsqd, aes(as.Date(Date, format="%m/%d/%Y"), TKN))+
  geom_point(aes(color=NCDC.Region, shape=Land.Use), na.rm=T)+
  geom_boxplot(aes(group=Decade), na.rm=T, outlier.shape=NA, fill=NA, color="black", lwd=1, fatten=1)+
  geom_smooth(na.rm=T, method="loess", formula=y~x, color="black")+
  theme_bw()+
  labs(x="Date", y="TKN EMC (mg/L)")+
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16, face="bold"), axis.title.x=element_blank())+
  scale_shape_manual(values=c(16, 17, 15, 3, 18, 8), name="LULC")+
  scale_color_manual(values=c("chartreuse", "deepskyblue2", "cyan2", "bisque", "purple", "coral", "deeppink", "grey"), name="NCDC Region")+
  coord_cartesian(ylim=c(0,5))+
  theme(legend.title=element_text(size=16, face="bold"), legend.text=element_text(size=16))
TKNplot


TPplot <- ggplot(nsqd, aes(as.Date(Date, format="%m/%d/%Y"), TP))+
  geom_point(aes(color=NCDC.Region, shape=Land.Use), na.rm=T)+
  geom_boxplot(aes(group=Decade), na.rm=T, outlier.shape=NA, fill=NA, color="black", lwd=1, fatten=1)+
  geom_smooth(na.rm=T, method="loess", formula=y~x, color="black")+
  theme_bw()+
  labs(x="Date", y="TP EMC (mg/L)")+
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16, face="bold"), axis.title.x=element_blank())+
  coord_cartesian(ylim=c(0,1))+
  scale_shape_manual(values=c(16, 17, 15, 3, 18, 8), name="LULC")+
  scale_color_manual(values=c("chartreuse", "deepskyblue2", "cyan2", "bisque", "purple", "coral", "deeppink", "grey"), name="NCDC Region")+
  theme(legend.title=element_text(size=16, face="bold"), legend.text=element_text(size=16))
TPplot

Cuplot<- ggplot(nsqd, aes(as.Date(Date, format="%m/%d/%Y"), Cu))+
  geom_point(aes(color=NCDC.Region, shape=Land.Use), na.rm=T)+
  geom_boxplot(aes(group=Decade), na.rm=T, outlier.shape=NA, fill=NA, color="black", lwd=1, fatten=1)+
  geom_smooth(na.rm=T, method="loess", formula=y~x, color="black")+
  theme_bw()+
  labs(x="Date", y="Cu EMC (µg/L)")+
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16, face="bold"), axis.title.x=element_blank())+
  coord_cartesian(ylim=c(0,100))+
  scale_shape_manual(values=c(16, 17, 15, 3, 18, 8), name="LULC")+
  scale_color_manual(values=c("chartreuse", "deepskyblue2", "cyan2", "bisque", "purple", "coral", "deeppink", "grey"), name="NCDC Region")+
  theme(legend.title=element_text(size=16, face="bold"), legend.text=element_text(size=16))
Cuplot


Pbplot<- ggplot(nsqd, aes(as.Date(Date, format="%m/%d/%Y"), Pb))+
  geom_point(aes(color=NCDC.Region, shape=Land.Use), na.rm=T)+
  geom_boxplot(aes(group=Decade), na.rm=T, outlier.shape=NA, fill=NA, color="black", lwd=1, fatten=1)+
  geom_smooth(na.rm=T, method="loess", formula=y~x, color="black")+
  theme_bw()+
  labs(x="Date", y="Pb EMC (µg/L)")+
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16, face="bold"), axis.title.x=element_blank())+
  coord_cartesian(ylim=c(0,250))+
  scale_shape_manual(values=c(16, 17, 15, 3, 18, 8), name="LULC")+
  scale_color_manual(values=c("chartreuse", "deepskyblue2", "cyan2", "bisque", "purple", "coral", "deeppink", "grey"), name="NCDC Region")+
  theme(legend.title=element_text(size=16, face="bold"), legend.text=element_text(size=16))
Pbplot

Znplot<- ggplot(nsqd, aes(as.Date(Date, format="%m/%d/%Y"), Zn))+
  geom_point(aes(color=NCDC.Region, shape=Land.Use), na.rm=T)+
  geom_boxplot(aes(group=Decade), na.rm=T, outlier.shape=NA, fill=NA, color="black", lwd=1, fatten=1)+
  geom_smooth(na.rm=T, method="loess", formula=y~x, color="black")+
  theme_bw()+
  labs(x="Date", y="Zn EMC (µg/L)")+
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16, face="bold"), axis.title.x=element_blank())+
  scale_shape_manual(values=c(16, 17, 15, 3, 18, 8), name="LULC")+
  scale_color_manual(values=c("chartreuse", "deepskyblue2", "cyan2", "bisque", "purple", "coral", "deeppink", "grey"), name="NCDC Region")+
  coord_cartesian(ylim=c(0,500))+
  theme(legend.title=element_text(size=16, face="bold"), legend.text=element_text(size=16))
Znplot

ggarrange(TSSplot, TPplot, TKNplot, Cuplot, Pbplot, Znplot, common.legend=T, legend="bottom")



# medians by decade #
medians <- nsqd %>%
  group_by(Decade) %>%
  summarize(TSS=median(TSS, na.rm=T),
            TP=median(TP, na.rm=T),
            TKN=median(TKN, na.rm=T),
            Cu=median(Cu, na.rm=T),
            Pb=median(Pb, na.rm=T),
            Zn=median(Zn, na.rm=T))
medians


# Mann-Kendall Tests#
solids <- nsqd[-c(2,13:20)]
solids<- solids[rowSums(is.na(solids))==0,]
solids <- solids[order(as.Date(solids$Date, format="%m/%d/%Y")),]
MannKendall(solids$TSS)

nitrogen <- nsqd[-c(2,12,14:20)]
nitrogen <- nitrogen[rowSums(is.na(nitrogen))==0,]
nitrogen <- nitrogen[order(as.Date(nitrogen$Date, format="%m/%d/%Y")),]
MannKendall(nitrogen$TKN)

phosphorus <- nsqd[-c(2,12,13,14,16:20)]
phosphorus <- phosphorus[rowSums(is.na(phosphorus))==0,]
phosphorus <- phosphorus[order(as.Date(phosphorus$Date, format="%m/%d/%Y")),]
MannKendall(phosphorus$TP)

copper <- nsqd[-c(2,12:16,18:20)]
copper <- copper[rowSums(is.na(copper))==0,]
copper <- copper[order(as.Date(copper$Date, format="%m/%d/%Y")),]
MannKendall(copper$Cu)

lead <- nsqd[-c(2,12:17,19,20)]
lead <- lead[rowSums(is.na(lead))==0,]
lead <- lead[order(as.Date(lead$Date, format="%m/%d/%Y")),]
MannKendall(lead$Pb)

zinc <- nsqd[-c(2,12:18,20)]
zinc <- zinc[rowSums(is.na(zinc))==0,]
zinc <- zinc[order(as.Date(zinc$Date, format="%m/%d/%Y")),]
MannKendall(zinc$Zn)



#NSQD+ variability#
quantile(nsqd$TSS, na.rm=T, probs = c(0.1,0.25,0.5,0.75,0.9))
sqrt(var(nsqd$TSS, na.rm=T))/mean(nsqd$TSS, na.rm=T)

quantile(nsqd$TP, na.rm=T, probs = c(0.1,0.25,0.5,0.75,0.9))
sqrt(var(nsqd$TP, na.rm=T))/mean(nsqd$TP, na.rm=T)

quantile(nsqd$TKN, na.rm=T, probs = c(0.1,0.25,0.5,0.75,0.9))
sqrt(var(nsqd$TKN, na.rm=T))/mean(nsqd$TKN, na.rm=T)

quantile(nsqd$Cu, na.rm=T, probs = c(0.1,0.25,0.5,0.75,0.9))
sqrt(var(nsqd$Cu, na.rm=T))/mean(nsqd$Cu, na.rm=T)

quantile(nsqd$Pb, na.rm=T, probs = c(0.1,0.25,0.5,0.75,0.9))
sqrt(var(nsqd$Pb, na.rm=T))/mean(nsqd$Pb, na.rm=T)

quantile(nsqd$Zn, na.rm=T, probs = c(0.1,0.25,0.5,0.75,0.9))
sqrt(var(nsqd$Zn, na.rm=T))/mean(nsqd$Zn, na.rm=T)


#limit data set to years 1994 - 2000#
nsqdtrim <- nsqd[nsqd$Year > "1994" & nsqd$Year < "2000",]
ddply(nsqdtrim, .(Land.Use), nrow)

