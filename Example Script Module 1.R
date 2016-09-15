#rm(list=ls())

#basic setup
library(openxlsx)
library(ggplot2)
library(ggthemes)
options(stringsAsFactors=FALSE)
# drivedirectory="/Users/JHETINDEX/Google Drive/"
 drivedirectory="C:/Users/ZinouInBeirut/Google Drive"
# drivedirectory="C:/Users/jonat/Google Drive/"
analyticsfolder=paste(c(drivedirectory,"/ET-Analytics/"),collapse="")
source(paste(c(analyticsfolder,"Module Functions.R"),collapse="")) 

#read in data
portfolioworkbooklocation=paste(c(analyticsfolder,"PortfolioWorkbook_Template.xlsx"),collapse="")
portchar=read.xlsx(portfolioworkbooklocation,sheet="Portfolio.Characteristics",detectDates = T)
portcarbonriskfactorbeta=read.xlsx(portfolioworkbooklocation,sheet="Portfolio.Carbon.Factor.Beta",detectDates = T)
valuesdata=read.xlsx(portfolioworkbooklocation,sheet="Portfolio.Values",detectDates = T) #values of each security each week, cumulative returns
port2valuesmatch=match(portchar$Name,valuesdata$Name)
portvalues=apply(valuesdata[port2valuesmatch,2:ncol(valuesdata)],2,as.numeric)

#setup portfolio object
portfolio=NULL
portfolio$revenue=portchar$'Sales.(P)'
portfolio$companyequityvalue=portchar$'Market.Cap.(P)'
portfolio$companydebtvalue=portchar$'Total.Debt.(P)'; portfolio$companydebtvalue[which(is.na(portfolio$companydebtvalue))]=0 #incorrect inferrence, just temp
portfolio$intensity.S123.Max=portchar$'UD-Rankings.Intensity.S123.(P)'; portfolio$intensity.S123.Max[which(is.na(portfolio$intensity.S123.Max))]=5000 #incorrect inferrence, just temp
portfolio$intensity.S123.Avg=portfolio$intensity.S123.Max*0.75 #temp
portfolio$intensity.S123.Min=portfolio$intensity.S123.Max*0.5 #temp
portfolio$intensity.S123.Stdev=portfolio$intensity.S123.Max*0.1 #temp
portfolio$totalvalue=100000000
portfolio$weight=rep(1,length(portfolio$revenue))/length(portfolio$revenue)
portfolio$carbon.beta=portcarbonriskfactorbeta$'Beta.(ex-ante).(P)'
portfolio$sector.SASB=portchar$'UD-SASB.SICS.SECTOR'; portfolio$sector.SASB[which(is.na(portfolio$sector.SASB))]="Diversified"
# p$subsector.SASB
portfolio$BB.Company.ID=portchar$'Bloomberg.Company.Identifier.(P)'
portfolio$free.float.percent=portchar$'Free.Float.Percent.(P)'/100
portfolio$ultimate.parent.name=portchar$'Ultimate.Parent.Company.Name'

#specific setup
options=NULL
options$inference=switch(1,"Max","Avg","Min")
options$scope=switch(1,"Scope 1+2+3","Scope 1+2","Scope 3")
options$companyvalue=switch(2,"Equity Only","Equity + Debt")
options$metric=switch(5,"Total Emissions","Portfolio Emissions-intensity of Revenue","Weighted Average Emissions-Intensity of Revenue","Weighted Average Emissions-Intensity of Market Value","Carbon Risk Factor Beta")
options$target.reduction=0.05

#to add: proper error handling of if there are missing values in any required vectors...
# portfolio.metric.value=
portfolio.metric.value=calcPortfolioFootprint(portfolio,options)

#calculate range values
range="Value only"
if (range=="Max and Min") {
  options$inference="Avg"
  portfolio.metric.value=calcPortfolioFootprint(portfolio,options)
  options$inference="Max"
  portfolio.metric.value.max=calcPortfolioFootprint(portfolio,options)
  options$inference="Min"
  portfolio.metric.value.min=calcPortfolioFootprint(portfolio,options)
  portfolio.metric.value.error=calcPortfolioFootprintError(portfolio,options)
} else if (range=="Error") {
  portfolio.metric.value.error=calcPortfolioFootprintError(portfolio,options)
}

#calculate target number
if (!is.null(options$targetreduction)) {target.metric.value=portfolio.metric.value*(1-target.reduction)}

#Module X - Carbon risk factor
hist(portfolio$carbon.beta,50)
sum(portfolio$weight*portfolio$carbon.beta) #weighted average carbon risk factor beta

hist(log(portfolio$intensity.S123.Max),30)

#add ranks to portfolio
portfolio$ranks=calcRanks(portfolio,options)

unique(portfolio$sector.SASB)
plot(portfolio$carbon.beta)
plot(portfolio$ranks,portfolio$carbon.beta)
sectorind=which(portfolio$sector.SASB=="Non-Renewable Resources")
points(sectorind,portfolio$carbon.beta[sectorind],col='red',pch=16)
sectorind=which(portfolio$sector.SASB=="Technology and Communications")
points(sectorind,portfolio$carbon.beta[sectorind],col='blue',pch=16)
sectorind=which(portfolio$sector.SASB=="Renewable Resources and Alternative Energy")
points(sectorind,portfolio$carbon.beta[sectorind],col='green',pch=16)
sectorind=which(portfolio$sector.SASB=="Consumption")
points(sectorind,portfolio$carbon.beta[sectorind],col='orange',pch=16)
sectorind=which(portfolio$sector.SASB=="Services")
points(sectorind,portfolio$carbon.beta[sectorind],col='turquoise',pch=16)

# optimisation
#setup portfolio with factor model information
portfolio=addPortfolioHFA(portfolio,portvalues)
#get optimized portfolio
maxreweightfactor=10 #10
minreweightfactor=0.1
minweightfactor=0.1
concertina=T #
concertinaceil=T
sectorneutral=F
concertinaslopeparam=5 #5
minup=0.375 #0.375
maxup=5 #5
companyconcertina=T
SectorSelect=F
options$metric=switch(3,"Total Emissions","Portfolio Emissions-intensity of Revenue","Weighted Average Emissions-Intensity of Revenue","Weighted Average Emissions-Intensity of Market Value")
optimport=getoptimizedweights(portfolio,options,
                              secmaxrwf=1.35,
                              targetreduc=0.5, #amount of reduction in carbon metric to  make
                              Benchmark=F, #use ET benchmark approach or not
                              FFremove=F, #fossil free or not
                              RelativeToConv=T, 
                              concertina=T, #rankings-based approach (reweights follow monotonic line based on rankings)
                              maxTE=0.30, #maximum allowed tracking error
                              TOBACCOremove=T, #remove tobacco or not
                              delind=NULL, #select constituents in the portfolio to force zero weights for
                              sign=1 #1: default, positive; -1: reverse sign (e.g. to maximize metric, e.g. carbon beta)
                              )
calcPortfolioFootprint(optimport,options)/calcPortfolioFootprint(portfolio,options)

sum(optimport$weight*optimport$carbon.beta) #weighted average carbon 

portfolio$weight
optimport$weight

optimoptions=options
optimoptions$metric=switch(2,"Total Emissions","Portfolio Emissions-intensity of Revenue","Weighted Average Emissions-Intensity of Revenue","Weighted Average Emissions-Intensity of Market Value")
optimport$ranks=calcRanks(optimport,optimoptions)

plot(optimport$ranks,optimport$carbon.beta)
plot(optimport$ranks,optimport$weight/portfolio$weight)


#data columns to add in BB: s12cat, cast parent name, what is diff between sales and 12M rev?, 

#output information on which securities form original portfolio were not used in process (e.g. not found in BB)
#handle blank in sector.SASB


##########################
#Biggest intensities in the portfolio (10 securities)

rank_securities<-data.frame(portfolio$ranks, portfolio$ultimate.parent.name, portfolio$intensity.S123.Max)
names(rank_securities)<-c("Rank","Company","Intensity")
rank_securities<-rank_securities[order(rank_securities$Rank),]
numsecurities=length(rank_securities$Rank)
last10_securities<-rank_securities[numsecurities-0:9,]
last10_securities$Company<-factor(last10_securities$Company, levels = c(last10_securities$Company[10],last10_securities$Company[9],
                                                                      last10_securities$Company[8],last10_securities$Company[7],
                                                                      last10_securities$Company[6],last10_securities$Company[5],
                                                                      last10_securities$Company[4],last10_securities$Company[3],
                                                                      last10_securities$Company[2],last10_securities$Company[1]) )

ggplot(data=last10_securities,aes(x=last10_securities$Company,y=last10_securities$Intensity))+geom_bar(stat="identity", width=0.5, fill="#6D9B97") + labs(title="Module 8 10 most carbon intensive companies", x= "Companies", y ="Carbon Intensity [tCO2eq/M$ revenue]") +theme(axis.text = element_text(family = "Franklin Gothic Book",colour = "black"),panel.grid.major.x=element_blank(),panel.grid.major.y=element_blank(), panel.background=element_rect(fill="white"),legend.title=element_blank(),legend.position="bottom",axis.ticks =element_blank())+ theme(axis.text.x=element_text(angle=90))


##########################
#evaluate the exposition to a sector ///// risk for each sector to consider

sector_weight<-data.frame(portfolio$sector.SASB,portfolio$weight)
names(sector_weight)<-c("Sector","Weight")
sector_weight<-aggregate(sector_weight$Weight, by=list(sector_weight$Sector), "sum")
names(sector_weight)<-c("Sector","Total Weight")

xx="sector.SASB"
portfolio$xx
portfolio[[xx]]
sector_weight$colors="#6D9B97"
sector_weight$colors[sector_weight$Sector=='Health Care']='Green'
ggplot(data=sector_weight,aes(x=sector_weight$Sector,y=sector_weight$`Total Weight`))+geom_bar(stat="identity", width=0.5, fill=sector_weight$colors) + labs(title="Input for Module 5 Exposure by sector", x= "Companies", y ="Weight]") + theme(axis.text.x=element_text(angle=90)) +theme(axis.text = element_text(family = "Franklin Gothic Book",colour = "black"),panel.grid.major.x=element_blank(),panel.grid.major.y=element_blank(), panel.background=element_rect(fill="white"),legend.title=element_blank(),legend.position="bottom",axis.ticks =element_blank())


##########################
# carbon footprint
# basis carbon footprint
intensity<-c(portfolio.metric.value) 
year<-c("2015")
df<-data.frame(intensity,year)

ggplot(data=df,aes(x=year,y=intensity))+geom_bar(stat="identity", width=0.5, fill="light blue") + labs(title="Module 1 Carbon Footptrint", x= "Year", y ="Carbon Intensity [tCO2eq/M$ revenue]") +theme(axis.text = element_text(family = "Franklin Gothic Book",colour = "black"),panel.grid.major.x=element_blank(),panel.grid.major.y=element_blank(), panel.background=element_rect(fill="white"),legend.title=element_blank(),legend.position="bottom",axis.ticks =element_blank())

# carbon footprint over t time 
intensity<-c(portfolio.metric.value*1.6, portfolio.metric.value*0.9, portfolio.metric.value*1.2,portfolio.metric.value) #dummy numbers 
year<-c("2012","2013","2014","2015")
df<-data.frame(intensity,year)

ggplot(data=df,aes(x=year,y=intensity))+geom_bar(stat="identity", width=0.5, fill="light blue") + labs(title="Module 1 Carbon Footptrint", x= "Year", y ="Carbon Intensity [tCO2eq/M$ revenue]") +theme(axis.text = element_text(family = "Franklin Gothic Book",colour = "black"),panel.grid.major.x=element_blank(),panel.grid.major.y=element_blank(), panel.background=element_rect(fill="white"),legend.title=element_blank(),legend.position="bottom",axis.ticks =element_blank())

# carbon footprint over t time & target
target<-c(0.80)# variable
intensity<-c(portfolio.metric.value*1.6, portfolio.metric.value*0.9, portfolio.metric.value*1.2,portfolio.metric.value, portfolio.metric.value*target) #dummy numbers 
year<-c("2012","2013","2014","2015","2016/target")
type<-c("Mesured value","Mesured value","Mesured value","Mesured value","Targe")
df<-data.frame(intensity,year)

df$color="light blue"
df$color[df$year=='2016/target']='grey'

ggplot(data=df,aes(x=year,y=intensity))+geom_bar(stat="identity", width=0.5,fill=df$color) + labs(title="Module 1 Carbon Footptrint", x= "Year", y ="Carbon Intensity [tCO2eq/M$ revenue]") +theme(axis.text = element_text(family = "Franklin Gothic Book",colour = "black"),panel.grid.major.x=element_blank(),panel.grid.major.y=element_blank(), panel.background=element_rect(fill="white"),legend.title=element_blank(),legend.position="bottom",axis.ticks =element_blank())

##########################
#adding a benchmark and a ET tilted version

# basis carbon footprint & benchmark & ET tileted version
benchmark<-c(portfolio.metric.value*0.92)#dummy number
ETtilt<-c(0.75)#variable, 75% for a tracker
intensity<-c(portfolio.metric.value,benchmark, portfolio.metric.value*ETtilt) 
year<-c("2015","2015","2015")
type<-c("Company XYZ","Benchmark","ET tilt")

df<-data.frame(intensity,year,type)

df$type <- factor(df$type, levels = c("Company XYZ","Benchmark","ET tilt"))

ggplot(data=df,aes(x=year,y=intensity, fill=type))+geom_bar(stat="identity", width=0.5,,position=position_dodge(width=0.6)) + labs(title="Module 1 Carbon Footptrint", x= "Year", y ="Carbon Intensity [tCO2eq/M$ revenue]") +theme(axis.text = element_text(family = "Franklin Gothic Book",colour = "black"),panel.grid.major.x=element_blank(),panel.grid.major.y=element_blank(), panel.background=element_rect(fill="white"),legend.title=element_blank(),legend.position="bottom",axis.ticks =element_blank())

# carbon footprint over t time & benchmark & ET tileted version
benchmark<-c(portfolio.metric.value*0.92)#dummy number
ETtilt<-c(0.75)#variable, 75% for a tracker
intensity<-c(portfolio.metric.value*1.6, portfolio.metric.value*1.3,portfolio.metric.value*1.6*ETtilt, portfolio.metric.value*0.9, benchmark*1,portfolio.metric.value*0.9*ETtilt, portfolio.metric.value*1.2, benchmark*1.05,portfolio.metric.value*1.2*ETtilt, portfolio.metric.value, benchmark,portfolio.metric.value*ETtilt) #dummy numbers 
year<-c("2012","2012","2012","2013","2013","2013","2014","2014","2014","2015","2015","2015")
type<-c("Company XYZ","Benchmark","ET tilt","Company XYZ","Benchmark","ET tilt","Company XYZ","Benchmark","ET tilt","Company XYZ","Benchmark","ET tilt")

df<-data.frame(intensity,year,type)

df$type <- factor(df$type, levels = c("Company XYZ","Benchmark","ET tilt"))

ggplot(data=df,aes(x=year,y=intensity, fill=type))+geom_bar(stat="identity", width=0.5,,position=position_dodge(width=0.6)) + labs(title="Module 1 Carbon Footptrint", x= "Year", y ="Carbon Intensity [tCO2eq/M$ revenue]") +theme(axis.text = element_text(family = "Franklin Gothic Book",colour = "black"),panel.grid.major.x=element_blank(),panel.grid.major.y=element_blank(), panel.background=element_rect(fill="white"),legend.title=element_blank(),legend.position="bottom",axis.ticks =element_blank())

##########################
#adding an error bar

# basis carbon footprint & error
intensity<-c(portfolio.metric.value) 
year<-c("2015")
df<-data.frame(intensity,year)

error<- aes(ymax = intensity+portfolio.metric.value.error, ymin=intensity-portfolio.metric.value.error)
dodge <- position_dodge(width=0.6)

ggplot(data=df,aes(x=year,y=intensity))+geom_bar(stat="identity", width=0.5, fill="light blue") + labs(title="Module 1 Carbon Footptrint", x= "Year", y ="Carbon Intensity [tCO2eq/M$ revenue]") +theme(axis.text = element_text(family = "Franklin Gothic Book",colour = "black"),panel.grid.major.x=element_blank(),panel.grid.major.y=element_blank(), panel.background=element_rect(fill="white"),legend.title=element_blank(),legend.position="bottom",axis.ticks =element_blank()) + geom_errorbar(error, position=dodge, width=0.2)


# carbon footprint over t time & error
intensity<-c(portfolio.metric.value*1.6, portfolio.metric.value*0.9, portfolio.metric.value*1.2,portfolio.metric.value) #dummy numbers 
year<-c("2012","2013","2014","2015")
df<-data.frame(intensity,year)


error<- aes(ymax = intensity+portfolio.metric.value.error, ymin=intensity-portfolio.metric.value.error)#check how to make the portfolio.metric.value.error proportional to intensity
dodge <- position_dodge(width=0.6)

ggplot(data=df,aes(x=year,y=intensity))+geom_bar(stat="identity", width=0.5, fill="light blue") + labs(title="Module 1 Carbon Footptrint", x= "Year", y ="Carbon Intensity [tCO2eq/M$ revenue]") +theme(axis.text = element_text(family = "Franklin Gothic Book",colour = "black"),panel.grid.major.x=element_blank(),panel.grid.major.y=element_blank(), panel.background=element_rect(fill="white"),legend.title=element_blank(),legend.position="bottom",axis.ticks =element_blank())  + geom_errorbar(error, position=dodge, width=0.2)

##########################
# optimized weight vs initial weight

# Reweight of optimized portfolio

## ADD OPTIMAL LINE ## 

rank<-c(portfolio$ranks,portfolio$ranks)
weight_percentage<-c(portfolio$weight/portfolio$weight,optimport$weight/portfolio$weight)
type<- vector("numeric",580)
type[1:290]<-"Initial portfolio"
type[291:580]<-"Optimized portfolio"
df<-data.frame(rank, weight_percentage, type)
ggplot(data=df,aes(x=rank,y=weight_percentage, fill=type)) + geom_point() + labs(title="Module 11 Reweight of optimized portfolio", x= "Rank", y ="% of intial weight") +theme(axis.text = element_text(family = "Franklin Gothic Book",colour = "black"),panel.grid.major.x=element_blank(),panel.grid.major.y=element_blank(), panel.background=element_rect(fill="white"),legend.title=element_blank(),legend.position="bottom",axis.ticks =element_blank())

##########################
# Carbon risk factor beta

CRFB<-c(sum(portfolio$weight*portfolio$carbon.beta),sum(optimport$weight*optimport$carbon.beta))
year<-c("2015","2015")
type<-c("Initial portfolio","Optimized portfolio")
df<-data.frame(CRFB, year, type)

ggplot(data=df,aes(x=year,y=CRFB, fill=type))+geom_bar(stat="identity", width=0.5,,position=position_dodge(width=0.6)) + labs(title="Module 19 Carbon Risk Factor", x= "Year", y ="Carbon Risk Factor") +theme(axis.text = element_text(family = "Franklin Gothic Book",colour = "black"),panel.grid.major.x=element_blank(),panel.grid.major.y=element_blank(), panel.background=element_rect(fill="white"),legend.title=element_blank(),legend.position="bottom",axis.ticks =element_blank())

##########################
# Carbon risk factor beta histogram
rank<-c(portfolio$ranks)
df<-data.frame(rank,portfolio$carbon.beta)

ggplot(df, aes(x=portfolio$carbon.beta)) + geom_histogram(bins = 50)+theme(panel.grid.major.x=element_blank(),panel.grid.major.y=element_blank(), panel.background=element_rect(fill="white"),legend.title=element_blank(),legend.position="bottom",axis.ticks =element_blank())
ggplot(df, aes(x=portfolio$carbon.beta)) + geom_density(kernel = "gaussian")+theme(panel.grid.major.x=element_blank(),panel.grid.major.y=element_blank(), panel.background=element_rect(fill="white"),legend.title=element_blank(),legend.position="bottom",axis.ticks =element_blank())

##########################
# CORPORATE SCORECARD
## Corp footprint basic 1.0


corp=read.xlsx("C:\\Users\\ZinouInBeirut\\Google Drive\\Tasks\\Scorecard Code\\example_corp.xlsx")

#footprint including the future objective
ggplot()+geom_bar(data=corp,aes(x=as.character( corp$Date.Ranking.Year) ,y=corp$Intensity.Reported.Scope.1.2, fill=corp$Scope),stat="identity",width = 0.8)+coord_flip()+scale_fill_manual(values = c("#0C5952","#6D9B97","#9EBDBA","#CEDEDC"))+theme(panel.grid.major.x=element_blank(),panel.grid.major.y=element_blank(), panel.background=element_rect(fill="white"),legend.title=element_blank(),legend.position="bottom",axis.ticks =element_blank())+xlab("")+ylab("")+ylim(0,8000)

#Basic
basic=subset(corp,corp$Date.Ranking.Year==2015)

ggplot()+geom_bar(data=basic,aes(x=basic$Date.Ranking.Year,y=basic$Intensity.Reported.Scope.1.2,fill=basic$Scope),stat='identity')+coord_flip()+scale_fill_manual(values = c("#0C5952","#6D9B97","#9EBDBA","#CEDEDC"))+theme(panel.grid.major.x=element_blank(),panel.grid.major.y=element_blank(), panel.background=element_rect(fill="white"),legend.title=element_blank(),legend.position="bottom",axis.ticks =element_blank())+xlab("")+ylab("")+ylim(0,8000)+scale_x_continuous(breaks=c(2015))

#footprint
corp=corp[1:9,]
ggplot()+geom_bar(data=corp,aes(x=corp$Date.Ranking.Year,y=corp$Intensity.Reported.Scope.1.2, fill=corp$Scope),stat="identity",width = 0.8)+coord_flip()+scale_fill_manual(values = c("#0C5952","#6D9B97","#9EBDBA","#CEDEDC"))+theme(panel.grid.major.x=element_blank(),panel.grid.major.y=element_blank(), panel.background=element_rect(fill="white"),legend.title=element_blank(),legend.position="bottom",axis.ticks =element_blank())+xlab("")+ylab("")+ylim(0,8000)+scale_x_continuous(breaks=c(2011,2013,2015,2016))




