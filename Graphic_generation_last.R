library(openxlsx)
library(ggplot2)
library(ggthemes)
library(extrafont)
library("ggrepel")
library(grid)
font_import()

#Disclosure Section in the report:

historical_disclosure = function(df,year)
{
  h=subset(df,df['Date.Ranking.Year']==year)
  h = hist(as.matrix(h['Disclosure.S12.Category']))
  h$density = h$counts/sum(h$counts)*100
  plot(h,freq=FALSE)
  
  return(h)
}

historical_disclosure_S3_cat = function(df,year)
{
  h=subset(df,df['Date.Ranking.Year']==year)
  h=h$Disclosure.Number.of.Scope.3.Categories
  h=as.data.frame(ftable(h))
  h$year=year
  h$Freq=h$Freq[1:nrow(h)]/sum(h$Freq)
  
  return(h)
}
pyram=function(x,y1,y2)
{
  data=historical_disclosure_S3_cat(x,y1)
  data$Freq=data$Freq*100
  data2=historical_disclosure_S3_cat(x,y2)
  data2$Freq=-data2$Freq*100
  
  plt=ggplot()+geom_bar(data=data,aes(x=data$h,y=data$Freq),fill='#EC8D4A',stat='identity')+geom_bar(data=data2,aes(x=data2$h,y=data2$Freq),stat='identity',fill='#87BCDC')+coord_flip()+theme(panel.grid.major=element_blank(),axis.ticks.y=element_blank(),axis.text = element_text(family = "Franklin Gothic Book",size=17))+xlab("Number of categories")+ylab("Percentage")+scale_x_discrete(breaks=c(0,5,10,15))+scale_y_continuous(breaks=c(-80,-50,0,50,80),labels=abs(c(-80,-50,0,50,80)))
  return(plt)
  }



disclosure_sector=function(df,year,sector)
{
  h=subset(df,(df['Date.Ranking.Year']==year & df['Current.SASB.SICS.Sector']==sector))
  h = hist(as.matrix(h['Disclosure.S12.Category']))         
  h$density = h$counts/sum(h$counts)*100
  plot(h,freq=FALSE)
  return (h)
}

disclosure_geo=function(df,year,geo)
{
  h=subset(df,(df['Date.Ranking.Year']==year & df['Geo.Region']==geo))
  h = hist(as.matrix(h['Disclosure.S12.Category']))         
  h$density = h$counts/sum(h$counts)*100
  plot(h,freq=FALSE)
  return (h)
}

Scopes_breakdown=function(df,year)
{ df=subset(df,df['Date.Ranking.Year']==year)
  S3=100*df["Intensity.Accepted.Scope.3"]/df["Intensity.Accepted.Scope.1.2.3"]
  S3=c(100-mean(as.matrix(S3)),mean(as.matrix(S3)))
  barplot(S3,horiz=TRUE, xlim =c(0,100))
  return (S3)
}


Scopes_historical_disclosure=function(df,year)
{
  df=subset(df,df['Date.Ranking.Year']==year)
  s1=sum(is.na(df$Intensity.Reported.Scope.1.2))
  s1=100*(1-s1/nrow(df))
  s3=sum(is.na(df$Intensity.Reported.Scope.3))
  s3=100*(1-s3/nrow(df))
  h=c(s1,s3)
  d=data.frame(h,row.names = c('Scopes 1 and 2','Scope 3'))
  return (d)
}
Scopes_historical_disclosure_sector=function(df,year,sector)
{
  df=subset(df,(df['Date.Ranking.Year']==year & df['Current.SASB.SICS.Sector']==sector))
  s1=sum(is.na(df$Intensity.Reported.Scope.1.2))
  s1=100*(1-s1/nrow(df))
  s3=sum(is.na(df$Intensity.Reported.Scope.3))
  s3=100*(1-s3/nrow(df))
  h=c(s1,s3)
  d=data.frame(h,row.names = c('Scopes 1 and  2','Scope 3'))
  return (d)
}

Scopes_historical_disclosure_geo=function(df,year,geo)
{
  df=subset(df,(df['Date.Ranking.Year']==year & df['Geo.Region']==geo))
  s1=sum(is.na(df$Intensity.Reported.Scope.1.2))
  s1=100*(1-s1/nrow(df))
  s3=sum(is.na(df$Intensity.Reported.Scope.3))
  s3=100*(1-s3/nrow(df))
  h=c(s1,s3)
  d=data.frame(h,row.names = c('Scopes 1 and  2','Scope 3'))
  return (d)
}


#Rankings section:

Ranking_table_top10=function(df,year)
{
  df=subset(df,df['Date.Ranking.Year']==year)
  df=df[order(df$Intensity.Accepted.Scope.1.2.3),]
  d=data.frame(df$ID.Company.Name,df$Intensity.Accepted.Scope.3,df$Intensity.Accepted.Scope.1.2.3,df$Intensity.Accepted.Scope.1.2)
  d$Rank=c(1:nrow(d))
  return (d) 
}

Ranking_table_top10_sector=function(df,year,sector)
{
  df=subset(df,(df['Date.Ranking.Year']==year & df['Current.SASB.SICS.Sector']==sector))
  df=df[order(df$Intensity.Accepted.Scope.1.2.3),]
  d=data.frame(df$ID.Company.Name,df$Intensity.Accepted.Scope.3,df$Intensity.Accepted.Scope.1.2.3,df$Intensity.Accepted.Scope.1.2)
  d$Rank=c(1:nrow(d))
  return (d) 
}

Ranking_table_top10_geo=function(df,year,geo)
{
  df=subset(df,(df['Date.Ranking.Year']==year & df['Geo.Region']==geo))
  df=df[order(df$Intensity.Accepted.Scope.1.2.3),]
  d=data.frame(df$ID.Company.Name,df$Intensity.Accepted.Scope.3,df$Intensity.Accepted.Scope.1.2.3,df$Intensity.Accepted.Scope.1.2)
  d$Rank=c(1:nrow(d))
  
  return (d)
}

move_raw=function(df,y1,y2)
{
  df1=subset(df,(df['Date.Ranking.Year']==y1 & df['Disclosure.S12.Category']==1))
  df2=subset(df,(df['Date.Ranking.Year']==y2 & df['Disclosure.S12.Category']==1))
  
  df1=df1[order(df1$Intensity.Accepted.Scope.1.2.3),]
  df2=df2[order(df2$Intensity.Accepted.Scope.1.2.3),]
  df1$Rank1=c(1:nrow(df1))
  df2$Rank2=c(1:nrow(df2))
  m=(merge(df1, df2, by = 'ID.Company.Name'))
  m=data.frame(m$ID.Company.Name,(base1$Rank1-base1$Rank2),col.names = c('Company Name','Rank Change'))
  
  return (m) 
}

move_rank=function(df,y1,y2)
{
  df1=subset(df,(df['Date.Ranking.Year']==y1 & !(is.na(df$Ranking.GL)) & df['Disclosure.S12.Category']==1))
  df2=subset(df,(df['Date.Ranking.Year']==y2 & !(is.na(df$Ranking.GL)) & df['Disclosure.S12.Category']==1))
  m=(merge(df1, df2, by = 'ID.Company.Name'))
  
  m$rank.diff=-(m$Ranking.GL.x-m$Ranking.GL.y)
  return(m)
}

move_rank_geo=function(df,y1,y2,geo)
{
  df1=subset(df,(df['Date.Ranking.Year']==y1 & !(is.na(df$Ranking.GL)) & df['Disclosure.S12.Category']==1 & df['Geo.Region']==geo))
  df2=subset(df,(df['Date.Ranking.Year']==y2 & !(is.na(df$Ranking.GL)) & df['Disclosure.S12.Category']==1 & df['Geo.Region']==geo))
  m=(merge(df1, df2, by = 'ID.Company.Name'))
  
  m$rank.diff=-(m$Ranking.GL.x-m$Ranking.GL.y)
  return(m)
}

move_rank_sector=function(df,y1,y2,sector)
{
  df1=subset(df,(df['Date.Ranking.Year']==y1 & !(is.na(df$Ranking.GL)) & df['Disclosure.S12.Category']==1 & df['Current.SASB.SICS.Sector']==sector))
  df2=subset(df,(df['Date.Ranking.Year']==y2 & !(is.na(df$Ranking.GL)) & df['Disclosure.S12.Category']==1 & df['Current.SASB.SICS.Sector']==sector))
  m=(merge(df1, df2, by = 'ID.Company.Name'))
  
  m$rank.diff=-(m$Ranking.GL.x-m$Ranking.GL.y)
  return(m)
}

graph_historical=function(x)
{
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
  #PLOTING LINE !
  
  plt=ggplot()+geom_bar(data=l,aes(x=l$year,y=l$Percentage,fill=l$`Disclosure Category`),stat='identity',width =1.2 )+coord_flip()+scale_x_continuous(breaks = c(2011,2013,2015))+theme(panel.grid.major.x=element_blank(),panel.grid.major.y=element_blank(), panel.background=element_rect(fill="white"),legend.title=element_blank(),legend.text=element_text(family = "Franklin Gothic Book",size= 10),axis.text = element_text(family = "Franklin Gothic Book",size=17),legend.position="bottom",axis.ticks =element_blank())+xlab("")+ylab("")+scale_fill_manual(values = c("#3790C5","#87BCDC","#AFD3E8","#D7E9F3"))
  return(plt)
  
}

l=read.xlsx("C:\\Users\\ZinouInBeirut\\Desktop\\ET-WORK\\Report\\Pre2016_ET_Data_Clean_Master.xlsx")
l=graph_historical(l)
ggplot(l,aes(x=l$year,y=l$Percentage,fill=l$`Disclosure Category`))+geom_bar(stat='identity',width = 1)+coord_flip()+scale_fill_discrete(guide = guide_legend(title = "Disclosure categories: "))+scale_x_continuous(breaks = c(2011,2013,2015))+xlab("Years")+ylab("Percentage")+ggtitle("Historical Disclosure rates")+theme(legend.position="bottom")

graph_geo=function(x)
{
  geo=c('EU','RoW','AP','BC','NorthA')
  i=1
  d=data.frame()
  while(i<length(geo))
  {
    
    k=disclosure_geo(x,2015,geo[i])
    i=i+1
    d=rbind(d,data.frame(k$mids,k$density,geo[i]))
  }
  d=d[d$k.density>0,]
  d$k.mids[d$k.mids<1.5]='Complete and Assured'
  d$k.mids[d$k.mids>1.5 & d$k.mids<2.5]='Complete and Unassured'
  d$k.mids[d$k.mids>2.5 & d$k.mids<3.5]='Incomplete'
  d$k.mids[d$k.mids<=4 & d$k.mids>3.5 ]='No data'
  plt=ggplot()+geom_bar(data=d,aes(x=d$geo.i.,y=d$k.density,fill=d$k.mids),stat='identity')+coord_flip()+scale_fill_manual(values = c("#3790C5","#87BCDC","#AFD3E8","#D7E9F3"))

  return(plt)
}


graph_sector=function(x)
{
  sectors=c('Consumption','Financials','Health Care','Infrastructure','Non-Renewable Resources','Renewable Resources and Alternative Energy','Resource Transformation','Services','Technology and Communications','Transportation')
  i=1
  d=data.frame()
  while(i<length(sectors))
  {
    k=disclosure_sector(x,2015,sectors[i])
    i=i+1
    d=rbind(d,data.frame(k$mids,k$density,sectors[i]))
  }
  d=d[d$k.density>0,]
  d$k.mids[d$k.mids==1.25]='Complete and Assured'
  d$k.mids[d$k.mids==2.75]='Incomplete'
  d$k.mids[d$k.mids==3.75]='No data'
  d$k.mids[d$k.mids==1.75]='Complete and Unassured'
  plt=ggplot()+geom_bar(data=d,aes(x=d$sectors.i.,y=d$k.density,fill=d$k.mids),width = 0.62,stat='identity')+coord_flip()+scale_fill_manual(values = c("#0C5952","#6D9B97","#9EBDBA","#CEDEDC"))+theme(panel.grid.major.x=element_blank(),panel.grid.major.y=element_blank(), panel.background=element_rect(fill="white"),legend.title=element_blank(),legend.text=element_text(family = "Franklin Gothic Book",size= 12),axis.text = element_text(family = "Franklin Gothic Book",size=17,face = "plain",colour = "black"),legend.position="bottom",axis.ticks =element_blank())+xlab("")+ylab("")
  return(plt)
}

