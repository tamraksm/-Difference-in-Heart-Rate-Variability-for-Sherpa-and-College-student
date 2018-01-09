library(XLConnect)
require(xlsx)
library(readxl)
library(stringr)
library(dplyr)
library(ggplot2)
library(reshape)

setwd("C:/Users/Ryan/Desktop/Qi/660")
shadata<-read_excel("HRV Nepal 2017.xlsx",sheet ="Sherpa 4500m rest")[,1:8]
studata<-read_excel("HRV Nepal 2017.xlsx",sheet ="Student 4500m rest")[,1:18]

shadata1<-shadata%>%
          filter(Subject=="Oxygen Saturation" | Subject=="Mean HR* per min" | 
          Subject=="RMSSD ms" | Subject=="pNN50 %" | Subject=="LFHF" |
          Subject=="Poincare plot, SD1 ms" | Subject=="Poincare plot, SD2 ms")
shadata1[5,1]="LFHF_FFT"
shadata1[6,1]="LFHF_AR"
shadata1<-t(shadata1)
shaname<-shadata1[1,]
shadata2<-shadata1[-1,]
colnames(shadata2)<-shaname
shadata22<-apply(shadata2,2,as.numeric)
cor(shadata22)
pairs(shadata22)

studata1<-studata%>%
  filter(Name=="Oxygen Saturation" | Name=="Mean HR* per min" | 
           Name=="RMSSD ms" | Name=="pNN50 %" | Name=="LFHF" |
           Name=="Poincare plot, SD1 ms" | Name=="Poincare plot, SD2 ms")
studata1[5,1]="LFHF_FFT"
studata1[6,1]="LFHF_AR"
studata1<-t(studata1)
stuname<-studata1[1,]
studata2<-studata1[-1,]
colnames(studata2)<-shaname
studata3<-apply(studata2,2,as.numeric)

cor(studata3)
pairs(shadata22)

finaldata<-rbind(shadata22,studata3)
sherpa<- c(1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
finaldata<-data.frame(cbind(finaldata,sherpa))
pairs(finaldata)
boxplot(finaldata[,2:4])

finaldata$sherpa<-as.factor(finaldata$sherpa)
levels(finaldata$sherpa)<-list(Sherpa="1",Student="0")
mdata <- reshape::melt(finaldata,id=c("sherpa"))

# boxplot2<-mdata%>%
#   filter(variable=="Oxygen.Saturation"|variable=="Mean.HR..per.min" | variable=="RMSSD.ms")
# Oxygen.Saturation Mean.HR..per.min RMSSD.ms pNN50.. LFHF_FFT  LFHF_AR Poincare.plot..SD1.ms
# Poincare.plot..SD2.ms sherpa

####PLOT PART###
oxygen<-mdata%>%
     filter(variable=="Oxygen.Saturation")
ggplot()+
geom_boxplot(aes(x=variable,y=value,col=sherpa),data=oxygen,size=2,width=0.5)+
  labs(title="Oxygen:Sherpa .vs. Student",
       x="Predictor",y="Oxygen Value")+
  theme(legend.text = element_text(size=17),
        plot.title =element_text(size=60),
        axis.title = element_text(size=14))+
  theme_bw()+
  theme(legend.text = element_text(size=20),
        plot.title =element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20))

Mean_HR<-mdata%>%
  filter(variable=="Mean.HR..per.min")
ggplot()+
  geom_boxplot(aes(x=variable,y=value,col=sherpa),data=Mean_HR,size=2,width=0.5)+
  labs(title="Mean_HR:Sherpa .vs. Student",
       x="Predictor",y="Mean_HR")+
  theme_bw()+
  theme(legend.text = element_text(size=17),
        plot.title =element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20))

SSD<-mdata%>%
  filter(variable=="RMSSD.ms")
ggplot()+
  geom_boxplot(aes(x=variable,y=value,col=sherpa),data=SSD,size=2,width=0.5)+
  labs(title="RMSSD:Sherpa .vs. Student",
       x="Response",y="RMSSD.ms")+
  theme_bw()+
  theme(legend.text = element_text(size=17),
        plot.title =element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20))

PNN<-mdata%>%
  filter(variable=="pNN50..")
ggplot()+
  geom_boxplot(aes(x=variable,y=value,col=sherpa),data=PNN,size=2,width=0.5)+
  labs(title="PNN 50%:Sherpa .vs. Student",
       x="Response",y="pNN50%")+
  theme_bw()+
  theme(legend.text = element_text(size=17),
        plot.title =element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20))

LFHF_FFT<-mdata%>%
  filter(variable=="LFHF_FFT")
ggplot()+
  geom_boxplot(aes(x=variable,y=value,col=sherpa),data=LFHF_FFT,size=2,width=0.5)+
  labs(title="LFHF_FFT:Sherpa .vs. Student",
       x="Response",y="LFHF_FFT")+
  theme_bw()+
  theme(legend.text = element_text(size=17),
        plot.title =element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20))

LFHF_AR<-mdata%>%
  filter(variable=="LFHF_AR")
ggplot()+
  geom_boxplot(aes(x=variable,y=value,col=sherpa),data=LFHF_AR,size=2,width=0.5)+
  labs(title="LFHF_AR:Sherpa .vs. Student",
       x="Response",y="LFHF_AR")+
  theme_bw()+
  theme(legend.text = element_text(size=17),
        plot.title =element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20))

SD1<-mdata%>%
  filter(variable=="Poincare.plot..SD1.ms")
ggplot()+
  geom_boxplot(aes(x=variable,y=value,col=sherpa),data=SD1,size=2,width=0.5)+
  labs(title="Poincare.SD1:Sherpa .vs. Student",
       x="Response",y="SD1")+
  theme_bw()+
  theme(legend.text = element_text(size=17),
        plot.title =element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20))

SD2<-mdata%>%
  filter(variable=="Poincare.plot..SD2.ms")
ggplot()+
  geom_boxplot(aes(x=variable,y=value,col=sherpa),data=SD2,size=2,width=0.5)+
  labs(title="Poincare.SD2:Sherpa .vs. Student",
       x="Response",y="SD2")+
  theme_bw()+
  theme(legend.text = element_text(size=20),
        plot.title =element_text(size=25),
        axis.title = element_text(size=25),
        axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20))



##
ggplot()+
  geom_point(aes(x=Oxygen.Saturation,y=RMSSD.ms,col=sherpa),data=finaldata)+
  facet_grid(.~sherpa)+
  theme_bw()

ggplot()+
  geom_point(aes(x=Mean.HR..per.min,y=RMSSD.ms,col=sherpa),data=finaldata)+
  facet_grid(.~sherpa)+
  theme_bw()

colMeans(finaldata[,2:8])
apply(finaldata[,2:8],2,sd)
