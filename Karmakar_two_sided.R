library(blockingChallenge)
data(wls)
library(optmatch)
wls4match <- wls
## This code replicates the blocking algorithm used in the paper
## Karmakar, Small, and Rosenbaum (2018).
## Create the distance matrix
distmat1 <- smahal(wls4match[,"gwiiq_bm"]) ## IQ
## Father's and mother's edu and parent's income
distmat2 <- smahal(wls4match[,c("edfa57q.NoNA", "edmo57q.NoNA", "bmpin1.NoNA",
                                ## Father's and mother's edu and parent's income
                                "incg400", "incg250")])
## Indicators for income in the top 5
## occupation score
distmat2.2 <- smahal(wls4match[,c("ocsf57.NoNA", "ocpf57.NoNA")])
## missing indicators
distmat3 <- smahal(wls4match[,c("edfa57q.miss", "edmo57q.miss",
                                "bmpin1.miss", "ocsf57.miss", "ocpf57.miss")])
## The IQ = gwiiq_bm is given more weight.
## parents' education and parent's income
distmat = distmat1*10+6*distmat2+3*distmat2.2+2*distmat3
## creating the blocks. This can take about 30min to run.
## May take more time depending of the computation power of the system.
set.seed(0841)
res.20.2 = makeblocks(distmat, bsize=25, Itr=250, maxItr=250, .data=wls4match)
## Here we provide the code for the
## analysis of the wls data for the effect of
## Catholic schooling on earning later in life.
## This code reproduces the computation presented
## in the paper Karmakar, Small, and Rosenbaum (2018).
## Some reorganization of the data set
d <- res.20.2$.data
religion<-d$relfml*1
religion<-factor(religion,levels=c(1,0),
                 labels=c("Catholic","Other"),ordered=T)
urban<-d$res57Dic*1
urban<-factor(urban,levels=c(1,0),labels=c("Urban","Rural"),ordered=T)
school<-factor(d$hsdm57*1,levels=c(1,0),
               labels=c("Catholic","Public"),ordered=T)
d<-cbind(d,religion,urban,school)
rm(religion,urban,school)
d$yrwg74[d$yrwg74<0]<-NA
## Create table 1
d3<-d[d$strata>0,]
dim(d3)
t1<-rep(table(d3$urban),4)
t2<-rep(table(d3$religion:d3$urban),2)
t3<-table(d3$school:d3$religion:d3$urban)
t1a<-rep(round(100*tapply(1*(d3$school=="Catholic"),d3$urban,mean)),4)
t2a<-rep(round(100*tapply(1*(d3$school=="Catholic"),d3$religion:d3$urban,mean)),2)
t3a<-round(100*tapply(1*(d3$school=="Catholic"),
                      d3$school:d3$religion:d3$urban,mean))
tab1<-cbind(t3,t2,t1,t3a,t2a,t1a)
tab1<-tab1[c(1,5,3,7,2,6,4,8),c(3,2,1,6,5,4)]
round(tab1)
rm(d3)
## Create plots in Figure 1
## A supplementary function
densityit<-function(x,main="",yl=""){
  who<-d$strata>0
  s<-d$strata[who]
  x<-x[who]
  a<-aov(x~factor(s))
  x<-x[order(s)]
  xm<-matrix(x,25,length(unique(s)))
  xbar<-apply(xm,2,mean,na.rm=TRUE)
  xc<-xm-outer(rep(1,25),xbar,"*")
  fval<-round(as.vector(anova(a)$F),1)[1]
  adj<-mean(x,na.rm=TRUE)+as.vector(xc)
  tmp<-c(x,adj)
  mx<-max(tmp,na.rm=TRUE)
  mn<-min(tmp,na.rm=TRUE)
  sub<-paste(" F = ",fval)
  sdall<-round(sd(x,na.rm=TRUE),1)
  sdwithin<-round(sqrt(anova(a)$Mean[2]),1)
  sub<-paste("st. dev: all=",sdall,", within=",sdwithin,sub,sep="")
  plot(density(adj[!is.na(adj)],adjust=4),xlim=c(mn,mx),
       main=main,xlab=yl,sub=sub,cex.sub=.9)
  lines(density(x[!is.na(x)]),lty=2)
  abline(v=mean(x,na.rm=TRUE),lty=1,col="grey")
}
## Plot for IQ
densityit(d$gwiiq_bm,main="IQ Before High School",yl="IQ")
## The following code shows the steps
## of calculating the upper bounds of one p-values in Table 2
## of the paper Karmakar, Small, and Rosenbaum (2018).
## Please see the paper for details of the analysis and the
## relevant references.
library(senstrat)
library(sensitivitymv)
## Some preliminary
d2<-d[d$strata>0,]
d2$strata<-factor(d2$strata)
wages<-d2$yrwg74/10 # convert from hundreds to thousands
wages[wages<0]<-NA
d2<-cbind(d2,wages)
d2<-d2[!is.na(d2$wages),]
rm(wages)
library(MASS)
## van Elteren's rank test for two sample
## tests with multiple strata
VEwilcoxon<-function (y, z, st, tau = 0) {
  ymiss<-is.na(y)
  y<-y[!ymiss]
  z<-z[!ymiss]
  st<-st[!ymiss]
  if (is.factor(st)) st <- as.integer(st)
  ust <- sort(unique(st))
  nst <- length(ust)
  if (tau != 0) y <- y - tau * z
  sc <- rep(NA, length(y))
  for (i in 1:nst) {
    who <- (st == ust[i])
    yi <- y[who]
    vi <- rank(yi)/(length(yi)+1)
    sc[who] <- vi
  }
  list(sc=sc,z=z,st=st)
}

###### two-sided test
calculatePval.two_sided <- function(y, gamma){
  
  ws<-VEwilcoxon(y,1*(urban=="Urban"),strata)
  rurban.gr <-senstrat(ws$sc,ws$z,ws$st,gamma=gamma,alternative = "greater",detail=TRUE)
  rurban.ls <-senstrat(ws$sc,ws$z,ws$st,gamma=gamma,alternative = "less",detail=TRUE)
  purban<- 2*min(rurban.gr$LinearBoundResult[1], rurban.ls$LinearBoundResult[1])
  
  ws<-VEwilcoxon(y,1*(religion=="Catholic"),strata:urban)
  rreligion.gr<-senstrat(ws$sc,ws$z,ws$st,gamma=gamma,alternative = "greater",detail=TRUE)
  rreligion.ls<-senstrat(ws$sc,ws$z,ws$st,gamma=gamma,alternative = "less",detail=TRUE)
  preligion<- 2*min(rreligion.gr$LinearBoundResult[1], rreligion.ls$LinearBoundResult[1])
  
  ws<-VEwilcoxon(y,1*(school=="Catholic"),strata:religion:urban)
  rdirect.gr<-senstrat(ws$sc,ws$z,ws$st,gamma=gamma,alternative = "greater",detail=TRUE)
  rdirect.ls<-senstrat(ws$sc,ws$z,ws$st,gamma=gamma,alternative = "less",detail=TRUE)
  pdirect<- 2*min(rdirect.gr$LinearBoundResult[1], rdirect.ls$LinearBoundResult[1])
  
  pcomb<-truncatedP(c(preligion,purban,pdirect))
  result<-c(purban,preligion,pdirect,pcomb)
  names(result)<-c("urban","religion","direct","combination")
  round(result, 4)
}

attach(d2)
## Stratified analysis with covariate adjustment
l2bmpin1.NoNA<-log2(1+bmpin1.NoNA)
## Testing for H0: beta=$0
md<-rlm(wages~gwiiq_bm+edfa57q.miss+edmo57q.miss+bmpin1.miss+
          ocsf57.miss+edfa57q.NoNA+edmo57q.NoNA+l2bmpin1.NoNA+
          ocsf57.NoNA+ocpf57.NoNA+strata)
y<-md$residual
pvalues_0 <- calculatePval.two_sided(y, gamma=1)

## Testing for H0: beta=$500
wages<-d2$yrwg74
wages1 = (wages*100)-500*1*(school=="Catholic")
md<-rlm(wages1~gwiiq_bm+edfa57q.miss+edmo57q.miss+bmpin1.miss+
          ocsf57.miss+edfa57q.NoNA+edmo57q.NoNA+l2bmpin1.NoNA+
          ocsf57.NoNA+ocpf57.NoNA+strata)
y<-md$residual
pvalues_500 <- calculatePval.two_sided(y, gamma=1)

print(data.frame(Case = c("H0:beta=0", "H0:beta=500"),
                 urban = c(pvalues_0[1], pvalues_500[1]),
                 religion = c(pvalues_0[2], pvalues_500[2]),
                 direct = c(pvalues_0[3], pvalues_500[3]),
                 combination = c(pvalues_0[4], pvalues_500[4]),
                 check.names = FALSE), digits = 6)

