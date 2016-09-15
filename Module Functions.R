options(stringsAsFactors=FALSE)
library("linprog")
library("quadprog")

######################################################################################
#Function to calculate the portfolio footprint with the input options
######################################################################################
calcPortfolioFootprint <- function(p,options,sign=1) {
  if (!is.numeric(p$weight)) {p$weight=NA} #handle case when optimizer returns text error for weights
  
  #calculate desired final number
  metricparams=getFootprintMetricParameters(p,options)
  portfolio.metric.value=(sum(metricparams$numeratorvector*p$weight)+metricparams$numeratorconstant)/
    (sum(metricparams$denominatorvector*p$weight)+metricparams$denominatorconstant)
  return(sign*portfolio.metric.value)
}

getFootprintMetricParameters <- function(p,options) {
  #calculate the intensity to use
  intensitytouse=getIntensitytouse(p,options)
  
  #define percentage ownership based on choice for total company value definition
  companyvalue=getCompanyvaluetouse(p,options)

  metricparams=NULL
  metricparams$numeratorvector=switch(options$metric,
    "Total Emissions"=p$totalvalue/companyvalue*p$revenue*intensitytouse,
    "Portfolio Emissions-intensity of Revenue"=p$totalvalue/companyvalue*p$revenue*intensitytouse,
    "Weighted Average Emissions-Intensity of Revenue"=intensitytouse, 
    "Weighted Average Emissions-Intensity of Market Value"=intensitytouse*p$revenue/companyvalue,
    "Carbon Risk Factor Beta"=p$carbon.beta
  )  
  metricparams$numeratorconstant=0
  metricparams$denominatorvector=switch(options$metric,
    "Total Emissions"=rep(0,length(intensitytouse)),
    "Portfolio Emissions-intensity of Revenue"=p$totalvalue/companyvalue*p$revenue,
    "Weighted Average Emissions-Intensity of Revenue"=rep(0,length(intensitytouse)), 
    "Weighted Average Emissions-Intensity of Market Value"=rep(0,length(intensitytouse)),
    "Carbon Risk Factor Beta"=rep(0,length(intensitytouse))
  )  
  metricparams$denominatorconstant=switch(options$metric,"Total Emissions"=1,"Portfolio Emissions-intensity of Revenue"=0,"Weighted Average Emissions-Intensity of Revenue"=1,"Weighted Average Emissions-Intensity of Market Value"=1,"Carbon Risk Factor Beta"=1  ) 
  
  return(metricparams)
}

getIntensitytouse<-function(p,options) {
  #calculate the intensity to use
  intensitytouse=switch(paste(options$scope,options$inference),
    "Scope 1+2+3 Max"=p$intensity.S123.Max,"Scope 1+2 Max"=p$intensity.S12.Max,"Scope 3 Max"=p$intensity.S3.Max,
    "Scope 1+2+3 Avg"=p$intensity.S123.Avg,"Scope 1+2 Avg"=p$intensity.S12.Avg,"Scope 3 Avg"=p$intensity.S3.Avg,
    "Scope 1+2+3 Min"=p$intensity.S123.Min,"Scope 1+2 Min"=p$intensity.S12.Min,"Scope 3 Min"=p$intensity.S3.Min)
  return(intensitytouse)
}

getCompanyvaluetouse<-function(p,options) {
  companyvalue=switch(options$companyvalue,
  "Equity Only"=p$companyequityvalue,
  "Equity + Debt"=p$companyequityvalue+p$companydebtvalue)
  return(companyvalue)
}

######################################################################################
#Function to calculate the error in the portfolio footprint with the input options
######################################################################################
calcPortfolioFootprintError <- function(p,options) {
  #calculate the intensity to use
  intensitytouse=switch(options$scope,"Scope 1+2+3"=p$intensity.S123.Stdev^2,"Scope 1+2"=p$intensity.S12.Stdev^2,"Scope 3"=p$intensity.S3.Stdev^2)
  
  #define percentage ownership based on choice for total company value definition
  companyvalue=switch(options$companyvalue,"Equity Only"=p$companyequityvalue,"Equity + Debt"=p$companyequityvalue+p$companydebtvalue)
  percentownership=p$totalvalue*p$weight/companyvalue
  
  #calculate desired final number
  carbonbetaSE=0.0015 #temporary estimate
  portfolio.metric.value=switch(options$metric,
    "Total Emissions"=sum(percentownership^2*intensitytouse*p$revenue^2),
    "Portfolio Emissions-intensity of Revenue"=sum(percentownership^2*intensitytouse*p$revenue^2)/sum(percentownership*revenue)^2,
    "Weighted Average Emissions-Intensity of Revenue"=sum(p$weight^2*intensitytouse), #weighted average revenue intensity
    "Weighted Average Emissions-Intensity of Market Value"=sum(p$weight^2*intensitytouse*p$revenue^2/companyvalue^2), #weight average market value intensity #sum(percentownership*intensity.S123*revenue)/sum(marketvalue) same as sum(weight*intensity.S123*revenue/companyequityvalue
    "Carbon Risk Factor Beta"=sum(p$weight^2*carbonbetaSE)
  )  
  
  return(sqrt(portfolio.metric.value))
}

###################################################################
#Functions to calculate factor model for input portfolio
###################################################################
try(source(paste(drivedirectory,"2. ET Index Analytics/0. Data & Code/1. Code/F1_HFA functionsv3.r",sep="")),T)
addPortfolioHFA<-function(portfolio,values) {
  totreturns=(values[,-1]-values[,-ncol(values)])/values[,-ncol(values)] #calculate returns
  totreturns[which(is.infinite(totreturns))]=NA #ignore infinite returns
  tend=ncol(totreturns); tind=max(1,(tend-51)):tend  
  hfares=gethfares(totreturns,1:nrow(totreturns),tind,NF=3,niter=3,F)
  portfolio$factorloadings=sqrt(52)*hfares$Beta
  portfolio$idiosyncraticvar=52*hfares$Phi
  portfolio$factormean=52*hfares$MuZ
  return(portfolio)
}
gethfares<-function(totreturns,sind,tind,NF,niter,print) {
  dX=totreturns[sind,tind]; NT=dim(dX)[2]; NI=dim(dX)[1]
	sigX=rowMeans(dX^2,na.rm=T) #which(sigX<=0)
	mu=rowMeans(dX,na.rm=T)
	dXzero=dX
	dXzero[which(is.na(dXzero),arr.ind=T)]=0 #create special dX with NA's set to be zero as this allows vectorization without changing the calc result
	dXlogical=!is.na(dX)
	Beta0=0.01*sigX*matrix(0.1+0.02*rnorm(NI*NF),ncol=NF)
	weights=rep(1,NI); 
	
	#setup sgroups which define which factors need to be orthogonal to each other for identification purposes
	sgroups=NULL; if (NF>1) { for (fg in 1:(NF-1))  {sgroups=c(sgroups,lapply((1+fg):NF,function(x) c(fg,x)))} }
	Sigma0=diag(NF);
  minphi=1e-6;  convg<<- 1e-6; maxbeta<<- 0;
	Phi0=max(minphi,sigX)
	resHFA=F1_MLFAfullRegParallel(weights,Beta0,Sigma0,Phi0,rep(0,NI),rep(0,NF),dX,sgroups,niter,printres=print,dXzero,dXlogical,PCA=T,ISOTROPIC=F)
	BetaHFA=resHFA$Beta
	PhiHFA=resHFA$Phi
	SigmaHFA=resHFA$Sigma
	M1HFA=resHFA$M1
	MuEHFA=resHFA$MuE
	MuZHFA=resHFA$MuZ
		
	return(list(Beta=BetaHFA,Phi=PhiHFA,Sigma=SigmaHFA,MuE=MuEHFA,MuZ=MuZHFA,M1HFA=M1HFA))
}

###################################################################
#Functions to optimise input portfolio, given factor model
###################################################################
calcRanks<-function(p,options=NULL) { #add rank to securities in portfolio
  #work out ranking by creating pseudo intensity which can be simply ranked to get ranking based on intens, then s12cat, then mvc (as tiebreakers)
  if (is.null(p$intensitytouse)) {p$intensitytouse=getIntensitytouse(p,options)}
  if (is.null(p$companyvalue)) {p$companyvalue=getCompanyvaluetouse(p,options)}
  pseudointens=p$intensitytouse
  if (!is.null(p$s12cat)) {
    dp=diff(pseudointens); mind=min(abs(dp[which(dp!=0)]),na.rm=T); maxc=max(abs(p$s12cat))
  	pseudointens=pseudointens+0.1*(mind/maxc)*p$s12cat
  } else { print("Warning: no S12 category info available, proceeding to rank without...")}
  if (!is.null(p$companyvalue)) {
	  dp=diff(pseudointens); mind=min(abs(dp[which(dp!=0)]),na.rm=T); maxc=max(abs(p$companyvalue))
	  pseudointens=pseudointens+0.1*(mind/maxc)*p$companyvalue	
  } else { print("Warning: no company value info available, proceeding to rank without...")}
	ranks=rank(pseudointens,ties.method="min")  
}

addConventionalMinMaxWeights<-function(p,TOBACCOremove,FFremove,SectorSelect,delind) {
  p$convweights=p$weight; p$convweights=p$convweights/sum(p$convweights); p$convweights0=p$convweights  
  p$maxweights=pmin(1,p$convweights0*maxreweightfactor) #c(100%,20x original w,max weight ceiling)
  p$minweights=minreweightfactor*p$convweights0 #rep(minreweightfactor*min(p$convweights0),length(p$convweights0)); # 0.1 * smallest orig w (msci carb method)	
  
  if (TOBACCOremove) {
  	tobaccoind=which(p$subsector.SASB=="Tobacco")
  	if (length(tobaccoind)>0) {p$convweights[tobaccoind]=0; p$convweights=p$convweights/sum(p$convweights); p$minweights[tobaccoind]=0; p$maxweights[tobaccoind]=0;}
  }
  if (FFremove) {
  	ffind=which(p$subsector.SASB=="Oil and Gas" | p$subsector.SASB=="Coal")
    if (length(ffind)>0) {p$convweights[ffind]=0; p$convweights=p$convweights/sum(p$convweights); p$minweights[ffind]=0; p$maxweights[ffind]=0;}
  }
  if (SectorSelect) {
    ffind=which(p$subsector.SASB=="Technology" & p$industry.SASB=="E-Commerce")
    if (length(ffind)>0) {p$convweights[-ffind]=0; p$convweights=p$convweights/sum(p$convweights); p$minweights[-ffind]=0; p$maxweights[-ffind]=0;}
  }
	#non-renewables cap
	nonrnwind=which(p$sector.SASB=="Non-Renewable Resources")
	if (length(nonrnwind)>0 & !concertina) {
		p$maxweights[nonrnwind]=pmin(p$maxweights[nonrnwind],p$convweights0[nonrnwind])
		p$minweights[nonrnwind]=pmin(p$minweights[nonrnwind],p$maxweights[nonrnwind])
	}
	#green industries floor
	greenind=which(!is.na(match(p$industry.SASB,c("Solar Energy","Wind Energy","Geothermal Energy","Fuel Cells & Industrial Batteries"))))
	if (length(greenind)>0 & !concertina) {
		p$minweights[greenind]=pmax(p$minweights[greenind],p$convweights0[greenind])
		p$maxweights[greenind]=pmax(p$minweights[greenind],p$maxweights[greenind])
	}
  if (!is.null(delind)) {
    p$convweights[delind]=0; p$convweights=p$convweights/sum(p$convweights); p$minweights[delind]=0; p$maxweights[delind]=0
  }
  return(p)
}

getoptimizedweights<-function(p,options,secmaxrwf=1.35,targetreduc,Benchmark=F,FFremove=F,RelativeToConv=F,rankingsbased=F,concertina=F,maxTE=0.30,TOBACCOremove=T,delind=NULL,sign=1){
  #ensure portfolio is initialised with required info
  p$intensitytouse=getIntensitytouse(p,options)
  p$companyvalue=getCompanyvaluetouse(p,options)
  p$ranks=calcRanks(p)
  p=addConventionalMinMaxWeights(p,TOBACCOremove=F,FFremove=F,SectorSelect=F,delind)
  
  #calculate base intensities
  ptemp=p; ptemp$weight=p$convweights0; baseintens0=calcPortfolioFootprint(ptemp,options,sign=sign)	
  ptemp=p; ptemp$weight=p$convweights;	baseintens=calcPortfolioFootprint(ptemp,options,sign=sign)  
  if (RelativeToConv) {baseintens=baseintens0}
  
  #calc base sectorweights before deleting individual stocks, so that conv portfolio sector allocation
  #is respected even with lower number of stocks
  sectors=unique(p$sector.SASB);
  sectorweights0=sapply(1:length(sectors),FUN=function(x) sum(p$convweights[which(sectors[x]==p$sector.SASB)]))
	sectorweightsmax=sectorweights0; sectorweightsmin=sectorweights0
  #sector weight constraints
	sectorweightsmax=sectorweights0+max(sectorweights0)*(secmaxrwf-1)
	sectorweightsmin=sectorweights0*0
	renewableind=which(sectors=="Renewable Resources and Alternative Energy")
	if (length(renewableind)>0) {sectorweightsmax[renewableind]=20*sectorweights0[renewableind]}
  
  reducweights=p$convweights #set weights equal to market cap weights with basic constraints as base case
  #estimate optimized weights only if target reduction is different from 0
  if (targetreduc==0) {
  	reducperc=0
    TE=estimateTE(p,p$convweights0,reducweights)
		p$minredperc=NA; p$maxredperc=NA    
  } else {
  	if (RelativeToConv) {convweightsInt=p$convweights0} else {convweightsInt=p$convweights}
  	
  	#define the ranks to use based on both the true ranks and which stocks are showing as non-eligible for portfolio (0 convweight)
    if (concertina) {
  		useranks=p$ranks
      zeroind=which(p$convweights==0)
      useranks[zeroind]=max(useranks)+1:length(zeroind)
  		useranks=rank(useranks,ties.method='min')
    } else {useranks=NULL}
  	
    p$compids=calcUniqueCompanyID(p)
  	constraints=genConstraints(p,options,
  		convweightsInt,teconvweights=p$convweights0,sectorweightsmax=sectorweightsmax,sectorweightsmin=sectorweightsmin,
      sectors=sectors,ranks=useranks,compids=p$compids,minup=minup,sign=sign)
   
  	#maxreduction
  	p$weightsmaxred=getmaxminredweights(length(p$revenue),constraints)
    ptemp=p; ptemp$weight=p$weightsmaxred; intensmaxred=calcPortfolioFootprint(ptemp,options,sign=sign)  
    truemaxredperc=1-intensmaxred/baseintens0; relmaxredperc=1-intensmaxred/baseintens
	 	#minreduction
  	p$weightsminred=getmaxminredweights(length(p$revenue),constraints,minreduc=T)
    ptemp=p; ptemp$weight=p$weightsminred; intensminred=calcPortfolioFootprint(ptemp,options,sign=sign) 
  	trueminredperc=1-intensminred/baseintens0; relminredperc=1-intensminred/baseintens
  	if (is.na(truemaxredperc) & is.na(trueminredperc)) {trueminredperc=truemaxredperc=0}
    if (is.na(relmaxredperc) & is.na(relminredperc)) {relminredperc=relmaxredperc=0}
 
    #get optimal weights for target reduction, reducing the reduction amount as necessary to control tracking error
		if (!FFremove & !SectorSelect) { maxreduc=truemaxredperc; minreduc=trueminredperc} else {	maxreduc=relmaxredperc;	minreduc=relminredperc}
  	#make regular Tracker at least 25% reduction
		if (!RelativeToConv) {minreduc=min(max(0.25,minreduc),maxreduc)}
	if (sign!= -1){	if (targetreduc>maxreduc) {reducperc=maxreduc-0.001} else if (targetreduc<minreduc) {reducperc=minreduc+0.001} else {reducperc=targetreduc}} else {reducperc=targetreduc}
		#set Benchmark to midpoint or 50% whichever is greater
  	if (Benchmark) {
  		if (FFremove) {reducperc=(minreduc+maxreduc)/2} else { reducperc=0.5}
  		reducperc=min(max(minreduc,reducperc),maxreduc)
  	}

  	#run the optimisation
  	while(sign*reducperc>0) {
  	  print(reducperc)
      reducweights=try(getOptimWeightsDev(p,constraints,1-reducperc-0.1,1-reducperc+0.00001,sign),T) # min intens, max intens
    	if (is.numeric(reducweights)) {
				minwprobind=which(reducweights==p$minweights & reducweights!=p$maxweights) #identifies reducweights stuck at minimum that is different from max
    		if (length(minwprobind)>0) { #check if there was problem with min constraints
    			maxweights[minwprobind]=p$minweights[minwprobind] #fixes issue by setting maxw to minw so the weights for these securities will now be minw by default
			  	constraints=genConstraints(p,options,convweightsInt,sectorweightsmax=sectorweightsmax,sectorweightsmin=sectorweightsmin,sectors=sectors,ranks=useranks,compids=p$compids,minup=minup,sign=sign)
    		} else { TE=estimateTE(p,p$convweights0,reducweights); if (TE>maxTE) { reducperc=reducperc-sign*0.01  } else {  break }} #continue if TE is OK
    	} else { reducperc=reducperc-sign*0.01	} #reduce the reduction that is being attempted and try again
    }
  	if (!is.numeric(reducweights)) {reducweights=p$convweights} #set to p$convweights if no optimization was possible  	
  } #reduction code, when targetreduc>0
  
  optimport=p; optimport$weight=reducweights
  
  print(c(minreduc,maxreduc,reducperc))
  
  return(optimport)
}

##############################################################################################################################
#Function to find min/max reduction possible given constraints, handling idiosyncrasies of getMaxredWeightsDev
##############################################################################################################################
getmaxminredweights<-function(numstocks,constraints,minreduc=F) {
  weightsmaxmin=try(getMaxredWeightsDev(numstocks,constraints,minreduc),T)
  if (is.numeric(weightsmaxmin)) {
  	if (!is.null(weightsmaxmin)) {
    	if (!is.na(weightsmaxmin[1])) {
    		proceed=T
    	} else {proceed=F}
  	} else {proceed=F}
	} else {proceed=F}
	if (!proceed) { 
		minup=0
		weightsmaxmin=try(getMaxredWeightsDev(numstocks,constraints,minreduc=T),T)
	}  
  return(weightsmaxmin)
}

######################################################################################
#Linear-fractional_programming to find min/max reduction possible given constraints
######################################################################################
getMaxredWeightsDev<-function(numstocks,constraints,minreduc=F) {
  lp=solveLP(cvec=c(constraints$lpc,constraints$lpalpha),bvec=constraints$bvecJmaxmin,Amat=constraints$AmatJmaxmin,const.dir=constraints$consvec,lpSolve=T,maximum=minreduc,verbose=0,tol=1e-2)
	lpweights=lp$solution[1:numstocks]; lpweights=lpweights/sum(lpweights)
  return(lpweights)
}

######################################################################################
#Function to minimize TE for a fixed reduction with weight constraints
######################################################################################
getOptimWeightsDev<-function(p,cons,intpercmin,intpercmax,sign=1) {  
	tol=cons$tol*0.0001
	aintvectup=sign*((intpercmax+ tol)*(sum(cons$lpc*cons$wI)+cons$lpalpha)/(sum(cons$lpd*cons$wI)+cons$lpbeta)*(cons$lpd+cons$lpbeta)  - (cons$lpc+cons$lpalpha)) #tilted intens must be smaller than max plus a tiny bit
	aintvectdn=-sign*((intpercmin-tol)*(sum(cons$lpc*cons$wI)+cons$lpalpha)/(sum(cons$lpd*cons$wI)+cons$lpbeta)*(cons$lpd+cons$lpbeta)  - (cons$lpc+cons$lpalpha)) #tilted intens must be greater than min minus a tiny bit
  numstocks=length(p$idiosyncraticvar)
	Amatadd=cbind(rep(1,numstocks),-rep(1,numstocks),aintvectup,aintvectdn) #selects which stocks are included in each constraint
	bvecadd=c(1-tol,-1-tol,-tol,-tol) #constraint values
	Amat=cbind(Amatadd,cons$AmatJoptim)
	bvec=c(bvecadd,cons$bvecJoptim)
	
	DmatJ=p$factorloadings%*%t(p$factorloadings)+diag(p$idiosyncraticvar) #+.0000001*diag(numstocks)  #UD[ind,]%*%t(UD[ind,]) #U%*%diag(Da^2)%*%t(U)
	iter=0
	repeat{	if (det(DmatJ)> 1e-120) { break} else {	
			iter=iter+1
			DmatJ=1.3*DmatJ
	}	}#avoids small determinant issue

	dvecJ=DmatJ%*%cons$wTE
	qs=solve.QP(Dmat=DmatJ,dvec=dvecJ,Amat=Amat,bvec=bvec,meq=0)
	optimweights=pmax(0,qs$solution)

	#set any weights that are too small to minweights 
	optimweights=optimweights/sum(optimweights)
	toosmall=which(optimweights<pmax(0,cons$minweights)); if (length(toosmall)>0) {optimweights[toosmall]=cons$minweights[toosmall]}
	optimweights=optimweights/sum(optimweights)
	
	#set any weights that are too big to maxweights
	toobig=which(optimweights>cons$maxweights); if (length(toobig)>0) {optimweights[toobig]=cons$maxweights[toobig]}
	optimweights=optimweights/sum(optimweights)	
	
	return(optimweights)
}

######################################################################################
#Function to estimate TE between two portfolios on the SAME universe
######################################################################################
estimateTE<-function(p,weights1,weights2) {
  #P=idiosyncraticvar
	#tiltedw=v, Rtilt=v%*%X, R=w%*%X, TE=sqrt(Var[(Rtilt-R)^2])=sqrt(Var[(v-w)%*%X%*%t(X)%*%t(v-w)]
	#X~B%*%Z+P,  E[X]=B%*%MuZ, E[X%*%t(X)]=B%*%t(B)+P+B%*%MuZ%*%t(MuZ)%*%t(B)
	#==>TE=sqrt((v-w)%*%(B%*%t(B)+P)%*%t(v-w))=sqrt(sum((v-w)^2*P)+((v-w)%*%B)%*%t((v-w)%*%B))
	
	dw=(weights2-weights1)
	dwload=dw%*%(p$factorloadings)
	return(1*sqrt(sum(dw^2*p$idiosyncraticvar)+sum(dwload^2))) 
}

######################################################################################
#Function to assign a unique company number to each security in a portfolio
######################################################################################
calcUniqueCompanyID <- function(p) {
  if (!is.null(p$CAST.Parent.Name)) {
    namesalesvect=paste(p$CAST.Parent.Name,p$'Sales.(P)',sep="")
  } else {
    print("Warning: CAST Parent Name not available, proceeding to assign companies without...")
    namesalesvect=paste(p$BB.Company.ID,p$'Sales.(P)',sep="")
  }
  uniquecompanylist=unique(namesalesvect)
  compids=match(namesalesvect,uniquecompanylist)
  return(compids)
}

#######################################################################################################################################
#Function to calculate the constraints for portfolio optimisation
#######################################################################################################################################
genConstraints<-function(p,options,intensconvweights,teconvweights,
  sectorweightsmax=NULL,sectorweightsmin=NULL,sectors=NULL,ranks=NULL,
	compids=NULL,minup=0,sign=1) {
	if (is.null(compids) & !is.null(ranks)) {compids=1:length(ranks)}

	tol=1e-3
  numstocks=length(p$revenue)
  metricparams=getFootprintMetricParameters(p,options)
	lpd=metricparams$denominatorvector
	lpc=sign*metricparams$numeratorvector
  lpbeta=metricparams$denominatorconstant
  lpalpha=metricparams$numeratorconstant
	wI=intensconvweights
	wTE=teconvweights
	
	AmatJoptim=NULL; bvecJoptim=NULL

	#maxmin
	bvecJmaxmin=c(0,0,1)
	AmatJmaxmin=rbind(c(rep(1,numstocks),-1), #sum(y)==t
							c(rep(0,numstocks),1), #t>=0
		          c(lpd,lpbeta))              #d*y+beta*t=1
	consvec=c("==",">=","==")
	
	#max/min weight constraints
	tolm=tol*0.0001
	if (!is.null(p$minweights)) {
		AmatJoptim=diag(rep(1,numstocks))
		AmatJmaxmin=rbind(AmatJmaxmin,cbind(diag(rep(1,numstocks)),-p$minweights+tolm))
		bvecJoptim=p$minweights-tolm
		consvec=c(consvec,rep(">=",numstocks))
		bvecJmaxmin=c(bvecJmaxmin,rep(0,numstocks))
	}
	if (!is.null(p$maxweights)) {
		AmatJoptim=cbind(AmatJoptim,diag(rep(-1,numstocks)))
		AmatJmaxmin=rbind(AmatJmaxmin,cbind(diag(rep(-1,numstocks)),p$maxweights+tolm))
		bvecJoptim=c(bvecJoptim,-p$maxweights-tolm)
		consvec=c(consvec,rep(">=",numstocks))
		bvecJmaxmin=c(bvecJmaxmin,rep(0,numstocks))	
	}
	
	#sector constraints
	tolsc=tol*0.0001
  sectorvect=p$sector.SASB
	if (!is.null(sectorweightsmax)) {
		AmatJoptim=cbind(AmatJoptim,sapply(1:length(sectors),function(x) -1*(sectorvect==sectors[x])))
		AmatJmaxmin=rbind(AmatJmaxmin,cbind(t(sapply(1:length(sectors),function(x) 1*(sectorvect==sectors[x]))),-sectorweightsmax))		
		bvecJoptim=c(bvecJoptim,-sectorweightsmax-tolsc)
		consvec=c(consvec,rep("<=",length(sectorweightsmax)))
		bvecJmaxmin=c(bvecJmaxmin,rep(0,length(sectorweightsmax)))
	}
	if (!is.null(sectorweightsmin)) {
		AmatJoptim=cbind(AmatJoptim,sapply(1:length(sectors),function(x) 1*(sectorvect==sectors[x])))
		AmatJmaxmin=rbind(AmatJmaxmin,cbind(t(sapply(1:length(sectors),function(x) 1*(sectorvect==sectors[x]))),-sectorweightsmin)) #771
		bvecJoptim=c(bvecJoptim,sectorweightsmin-tolsc)
		consvec=c(consvec,rep(">=",length(sectorweightsmin)))
		bvecJmaxmin=c(bvecJmaxmin,rep(0,length(sectorweightsmin)))
	}	
	
	#concertina setup
	if (!is.null(ranks)) {
		if (sectorneutral) {
			for (ss in sectors[c(1:10)]) {
					sectorinds=which(sectorvect==ss)
					if (length(sectorinds)>2) {
						concertinaCs=getConcertinaConstraints(ranks[sectorinds],compids[sectorinds],p$maxweights[sectorinds],p$minweights[sectorinds],tol,wTE[sectorinds],minup)
						Ao=matrix(0,ncol=ncol(concertinaCs$AmatJoptim),nrow=nrow(AmatJoptim))
						Ao[sectorinds,]=concertinaCs$AmatJoptim
						AmatJoptim=cbind(AmatJoptim,Ao)
						Am=matrix(0,nrow=nrow(concertinaCs$AmatJmaxmin),ncol=ncol(AmatJmaxmin))
						Am[,sectorinds]=concertinaCs$AmatJmaxmin[,1:length(sectorinds),drop=F]
						Am[,ncol(Am)]=concertinaCs$AmatJmaxmin[,1+length(sectorinds),drop=F]
						AmatJmaxmin=rbind(AmatJmaxmin,Am)
						bvecJoptim=c(bvecJoptim,concertinaCs$bvecJoptim)
						bvecJmaxmin=c(bvecJmaxmin,concertinaCs$bvecJmaxmin)
						consvec=c(consvec,concertinaCs$consvec)
					}
			}
		} else {
			concertinaCs=getConcertinaConstraints(ranks,compids,p$maxweights,p$minweights,tol,wTE,minup)
			AmatJoptim=cbind(AmatJoptim,concertinaCs$AmatJoptim)
			AmatJmaxmin=rbind(AmatJmaxmin,concertinaCs$AmatJmaxmin)
			bvecJoptim=c(bvecJoptim,concertinaCs$bvecJoptim)
			bvecJmaxmin=c(bvecJmaxmin,concertinaCs$bvecJmaxmin)
			consvec=c(consvec,concertinaCs$consvec)
		}
	} #end concertina setup
			
	constraints=list(tol=tol,wI=wI,wTE=wTE,maxweights=p$maxweights,minweights=p$minweights,lpd=lpd,lpc=lpc,
		AmatJoptim=AmatJoptim,AmatJmaxmin=AmatJmaxmin,bvecJoptim=bvecJoptim,bvecJmaxmin=bvecJmaxmin,consvec=consvec,lpalpha=lpalpha,lpbeta=lpbeta)
	
  return(constraints)
}

#######################################################################################################################################
#######################################################################################################################################
getConcertinaConstraints<-function(ranks,compids,maxweights,minweights,tol,wTE,minup){
  	numstocks=length(ranks)
		ind0=(1:length(ranks))
		rankind=order(ranks) #index of nth ranked company
		companies=unique(compids)
		numcompanies=length(companies)
		companyranks=match(companies,compids[rankind])
		comprankind=order(companyranks)
		rankedcompanies=companies[comprankind]
		leadstockrankind=match(rankedcompanies,compids) #leadstockind of nth ranked company, just take leading stock as first stock of company in whatever order the compids/ranks come in
		numactivecompconstraints=length(which(maxweights[leadstockrankind]>0 & maxweights[leadstockrankind]>minweights[leadstockrankind]))-1
		leadstockindforeachstock=leadstockrankind[match(compids,rankedcompanies)]
		if (length(companies)<length(ranks)) {
			nonleadstockind=which(leadstockindforeachstock!=ind0)
		}else{nonleadstockind=NULL}
		
		rankedcomps=compids[rankind]
		leadstockranks=match(compids,rankedcomps) #lead stock rank for each stock
		leadstockinds=which(maxweights[rankind[unique(leadstockranks)]]>0) 

		comprank=match(leadstockranks,sort(unique(leadstockranks))) #company rank for each stock
		nextleadstockind=rankind[leadstockranks[match(comprank+1,comprank)]] #index of the lead stock in the next company in the ranking,nextleadstockind[x] means index of the stock of company with rank x+1
		numstocksslope=length(which(maxweights>0))
		numcompsslope=length(which(maxweights[rankind[unique(leadstockranks)]]>0))-1 #set so that slope is only over those companies that aren't divested #and -1 as slope is between pairs of companies		
		numcomps=length(unique(leadstockranks))		
		concertinaslope=concertinaslopeparam^(1/numactivecompconstraints)
			
		#set weights of each stock against the next company's lead stock
		#use absolute concertina for companies with better ranks so that the minimum is more like a straight line for them 
		tolc1=tol*0.0001
		AmatJoptim=NULL;	AmatJmaxmin=NULL; bvecJoptim=NULL; bvecJmaxmin=NULL; consvec=NULL;
		adim0=0
		adivind=floor(3*numactivecompconstraints/4)
		Aaddmat=cbind(sapply(1:adivind,function(x) {
			a=rep(0,numstocks);
			a[leadstockrankind[x]]=max(c(1/wTE[leadstockrankind[x]],0),na.rm=T); 
			a[leadstockrankind[x+1]]=-max(c(1/wTE[leadstockrankind[x+1]],0),na.rm=T); 
			return(a)
		}))
		AmatJoptim=cbind(AmatJoptim,Aaddmat)
		adim1=dim(AmatJoptim)[2]
		AmatJmaxmin=rbind(AmatJmaxmin,cbind(t(Aaddmat),-rep(minup/(adim1-adim0)-tolc1,adim1-adim0))) #bottom addition is to do with extra constraint for maxmin linprog
		bvecJoptim=c(bvecJoptim,rep(minup/(adim1-adim0)-tolc1,adim1-adim0))	
		bvecJmaxmin=c(bvecJmaxmin,rep(0,adim1-adim0))
		consvec=c(consvec,rep(">=",adim1-adim0))
		#use relative concertina for companies with worse ranks to let their reweights get close to the minimum
		adim0=max(dim(AmatJoptim)[2],0)
		Aaddmat=cbind(sapply((adivind+1):numactivecompconstraints,function(x) {
			a=rep(0,numstocks);
			a[leadstockrankind[x]]=max(c(1/wTE[leadstockrankind[x]],0),na.rm=T); 
			a[leadstockrankind[x+1]]=-concertinaslope*max(c(1/wTE[leadstockrankind[x+1]],0),na.rm=T); 
			return(a)
		}))
		AmatJoptim=cbind(AmatJoptim,Aaddmat)
		adim1=dim(AmatJoptim)[2]
		AmatJmaxmin=rbind(AmatJmaxmin,cbind(t(Aaddmat),rep(-(concertinaslope-1)*minweightfactor-tolc1,adim1-adim0)))
		bvecJoptim=c(bvecJoptim,rep(-(concertinaslope-1)*minweightfactor-tolc1,adim1-adim0))
		bvecJmaxmin=c(bvecJmaxmin,rep(0,adim1-adim0))	
		consvec=c(consvec,rep(">=",adim1-adim0))
		
		#cap on jumps in concertina
		tolc2=tol*0.0001
		adim0=max(dim(AmatJoptim)[2],0)
		if (concertinaceil) {
			Aaddmat=cbind(sapply(1:numactivecompconstraints,function(x) {
				a=rep(0,numstocks); 
				a[leadstockrankind[x]]=-1/wTE[leadstockrankind[x]]; a[leadstockrankind[x+1]]=1/wTE[leadstockrankind[x+1]]; 
				return(a)  
			}))
      AmatJoptim=cbind(AmatJoptim,Aaddmat)
			adim1=dim(AmatJoptim)[2]
			AmatJmaxmin=rbind(AmatJmaxmin,cbind(t(Aaddmat),rep(maxup/(adim1-adim0)+tolc2,adim1-adim0)))
			bvecJoptim=c(bvecJoptim,rep(-maxup/(adim1-adim0)-tolc2,adim1-adim0))	
			bvecJmaxmin=c(bvecJmaxmin,rep(0,adim1-adim0))
			consvec=c(consvec,rep(">=",adim1-adim0))
		}
		
# 	#	set weights of stocks against next stock in same company
		adim0=dim(AmatJoptim)[2]
		tolc3=tol*0.0001
		if (companyconcertina & length(nonleadstockind)>0) {	
			Aaddmat=cbind(sapply(nonleadstockind,function(x) {	
					a=rep(0,numstocks); y=leadstockindforeachstock[x]
					a[y]=1; a[x]=-(1-tolc3)*max(c(wTE[y]/wTE[x],0),na.rm=T) #regular concertina to keep stocks in same company managed relative to each other
					return(a)
				}))			
			Aaddmat=cbind(Aaddmat,sapply(nonleadstockind,function(x) {	
					a=rep(0,numstocks); y=leadstockindforeachstock[x]
					#if the next lead stock is not the next stock in the pseudo rankings (i.e. if there are more stocks in the company)
					a[y]=-1;a[x]=(1+tolc3)*max(c(wTE[y]/wTE[x],0),na.rm=T);		#regular concertina to keep stocks in same company managed relative to each other
					return(a)
				}))
		AmatJoptim=cbind(AmatJoptim,Aaddmat)
		adim1=dim(AmatJoptim)[2]
		AmatJmaxmin=rbind(AmatJmaxmin,cbind(t(Aaddmat),rep(0,adim1-adim0)))
		bvecJoptim=c(bvecJoptim,rep(0,adim1-adim0))
		bvecJmaxmin=c(bvecJmaxmin,rep(0,adim1-adim0))
		consvec=c(consvec,rep(">=",adim1-adim0))
		}
	concertinaconstraints=list(AmatJoptim=AmatJoptim,AmatJmaxmin=AmatJmaxmin,bvecJoptim=bvecJoptim,bvecJmaxmin=bvecJmaxmin,consvec=consvec)
}

