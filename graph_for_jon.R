library(openxlsx)
library(ggplot2)
library(ggthemes)
library(extrafont)
library("ggrepel")
library(grid)
#run the modules code before executing here 
#### Put the full dataset in x

x=YOURPATH


historical_disclosure = function(df,year)
{
  h=subset(df,df['Date.Ranking.Year']==year)
  h = hist(as.matrix(h['Disclosure.S12.Category']))
  h$density = h$counts/sum(h$counts)*100
  plot(h,freq=FALSE)
  
  return(h)
}


l1=historical_disclosure(x,2015)
l1=data.frame(l1$mids,l1$density)
l1 = l1[l1$l1.density > 0,]
l1$mids=c("Complete and assured    ","Complete and unassured     ","Incomplete  ","No data   ")
l1$year=c(2015,2015,2015,2015)
colnames(l1)=c("Disclosure Category code","Percentage","Disclosure Category","year")

l2=historical_disclosure(x,2013)
l2=data.frame(l2$mids,l2$density)
l2 = l2[l2$l2.density > 0,]
l2$mids=c("Complete and assured    ","Complete and unassured     ","Incomplete  ","No data   ")
l2$year=c(2013,2013,2013,2013)
colnames(l2)=c("Disclosure Category code","Percentage","Disclosure Category","year")

l3=historical_disclosure(x,2011)
l3=data.frame(l3$mids,l3$density)
l3 = l3[l3$l3.density > 0,]
l3$mids=c("Complete and assured    ","Complete and unassured     ","Incomplete  ","No data   ")
l3$year=c(2011,2011,2011,2011)

colnames(l3)=c("Disclosure Category code","Percentage","Disclosure Category","year")

l= rbind(l1,l2,l3)
l$`Disclosure Category`[l$`Disclosure Category`==1.1]='Complete and Assured     .'
l$`Disclosure Category`[l$`Disclosure Category`==1.9]='Incomplete    .'
l$`Disclosure Category`[l$`Disclosure Category`==2.9]='No data    .'
l$`Disclosure Category`[l$`Disclosure Category`==3.9]='Complete and Unassured    .'
ggplot()+geom_bar(data=l,aes(x=as.character(l$year) ,y=l$Percentage,fill=l$`Disclosure Category`),stat='identity',width =0.8)+coord_flip()+theme(axis.title.x=element_text(family = "Franklin Gothic Book",size= 16.5),axis.title.y=element_text(family = "Franklin Gothic Book",size= 16.5),axis.text.x=element_text(family = "Franklin Gothic Book",size= 14),axis.text.y=element_text(family = "Franklin Gothic Book",size= 14),plot.title=element_text(family = "Franklin Gothic Book",size= 20,hjust = 0),axis.text = element_text(family = "Franklin Gothic Book",size=16.5,colour = 'black'),panel.grid.major.x=element_blank(),panel.grid.major.y=element_blank(), panel.background=element_rect(fill="white"),legend.title=element_blank(),legend.text=element_text(family = "Franklin Gothic Book",size= 15),axis.text = element_text(family = "Franklin Gothic Book",size=18),legend.position="bottom",axis.ticks =element_blank())+xlab("")+ylab("")+scale_fill_manual(values = c("#3790C5","#87BCDC","#AFD3E8","#DCDCDC"))+theme(axis.title.y=element_text(margin=margin(0,20,0,0)))+guides(colour = guide_legend(override.aes = list(size=6,linetype=1)))+ylab("Percentage of companies")+xlab(" Year ")+ggtitle("Historical disclosure: ")



# carbon footprint over t time & target 6X8
target<-c(0.80)# variable
intensity<-c(portfolio.metric.value*1.6, portfolio.metric.value*0.9, portfolio.metric.value*1.2,portfolio.metric.value, portfolio.metric.value*target) #dummy numbers 
year<-c("2012","2013","2014","2015","2016/target")
type<-c("Mesured value","Mesured value","Mesured value","Mesured value","Targe")
df<-data.frame(intensity,year)

df$color="#AFD3E8"
df$color[df$year=='2016/target']='#DCDCDC'
df$group='ok'
ggplot(data=df,aes(x=year,y=intensity))+geom_bar(stat="identity", width=0.65,fill=df$color) + labs(x= "Year")+theme(axis.title.y=element_text(margin=margin(0,20,0,0)),axis.title.x=element_text(margin=margin(15,0,20,0))) +theme(plot.title=element_text(family = "Franklin Gothic Book",colour = "black",size=18),axis.title.x = element_text(family = "Franklin Gothic Book",colour = "black",size=15),axis.title.y = element_text(family = "Franklin Gothic Book",colour = "black",size=15),axis.text = element_text(family = "Franklin Gothic Book",colour = "black",size=12),panel.grid.major.x=element_blank(),panel.grid.major.y=element_blank(), panel.background=element_rect(fill="white"),legend.title=element_blank(),legend.position="bottom",axis.ticks =element_blank())+guides(colour = guide_legend(override.aes = list(size=6,linetype=1)))+geom_line(data=df,aes(x=df$year,y=df$intensity),group=df$group)+ylab("Carbon Intensity")+ggtitle("Fund footprint over time")


# carbon footprint over t time & benchmark & ET tileted version 6X8
benchmark<-c(portfolio.metric.value*0.92)#dummy number
ETtilt<-c(0.75)#variable, 75% for a tracker
intensity<-c(portfolio.metric.value*1.6, portfolio.metric.value*1.3,portfolio.metric.value*1.6*ETtilt, portfolio.metric.value*0.9, benchmark*1,portfolio.metric.value*0.9*ETtilt, portfolio.metric.value*1.2, benchmark*1.05,portfolio.metric.value*1.2*ETtilt, portfolio.metric.value, benchmark,portfolio.metric.value*ETtilt) #dummy numbers 
year<-c("2012","2012","2012","2013","2013","2013","2014","2014","2014","2015","2015","2015")
type<-c("Company XYZ","Benchmark","ET tilt","Company XYZ","Benchmark","ET tilt","Company XYZ","Benchmark","ET tilt","Company XYZ","Benchmark","ET tilt")

df<-data.frame(intensity,year,type)

df$type <- factor(df$type, levels = c("Company XYZ","Benchmark","ET tilt"))
g=df[df$type=="Company XYZ",]
ggplot()+geom_bar(data=g,aes(x=g$year,y=g$intensity),stat="identity", width=0.5,fill='light blue')+theme(plot.title=element_text(family = "Franklin Gothic Book",colour = "black",size=18),legend.background=element_rect(fill='white'),legend.text=element_text(family = "Franklin Gothic Book",colour = "black",size=12),axis.text = element_text(family = "Franklin Gothic Book",colour = "black",size=12),panel.grid.major.x=element_blank(),panel.grid.major.y=element_blank(), panel.background=element_rect(fill="white"),legend.title=element_blank(),legend.position="bottom",axis.ticks =element_blank(),title=element_text(family = "Franklin Gothic Book",colour = "black",size=15))+geom_line(data=df,aes(x=df$year,y=df$intensity,group=df$type,color=df$type))+ylab("Carbon intensity")+xlab("Year")+theme(axis.title.y=element_text(margin=margin(0,20,0,0)))+guides(colour = guide_legend(override.aes = list(size=6,linetype=1)))+ggtitle("Fund footprint over time with ETindex adjustements")





