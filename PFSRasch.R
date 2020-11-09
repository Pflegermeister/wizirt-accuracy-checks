library('mirt')
library('irtoys')
library('PerFit')
library('ltm')
set.seed(100)
irt=function(t,a,b,c){return(c+(1-c)/(1+exp(a*(b-t))))}
sim1dat=function(N,t,a,b,c)#Simulated one IRT data with true values t,a,b,c
{ I=length(a)
y=matrix(0,N,I)
for (i in 1:N){
p=irt(t[i],a,b,c)
y[i,]=rbinom(I,size=1,prob=p)}
return(y)}
nitem=170
ncol=3*nitem+3
dat=read.csv('data/Scores_F2.csv')
d=dat[,174:343]
#*************************** Rasch Model ********************************
scores.rasch = rasch(d) #Fit the Rasch model to the combined data
itparm=coef(scores.rasch)#Estimated item parameters
b=itparm[,1]#Difficulty parameters
a=itparm[,2]#Common slope parameter
# Compute Critical Values from Simulated Data
ninrep=2000#nrow(dat)
drep=sim1dat(ninrep,rnorm(ninrep),a,b,rep(0,nitem))
scores.rasch = rasch(drep) #Fit the Rasch model to the combined data
itparm=coef(scores.rasch)#Estimated item parameters
br=itparm[,1]#Difficulty parameters
ar=itparm[,2]#Common slope parameter
itparmsr=cbind(ar,br,rep(0,nitem))
thetaestr=mlebme(drep,itparmsr)[,1]
Hts = Ht(drep)[[1]]$PFscores
lzs=lzstar(drep,Ability=thetaestr,IP=itparmsr)[[1]]$PFscores
U3s=U3(drep,Ability=thetaestr,IP=itparmsr)[[1]]$PFscores
cH=quantile(Hts,0.05)
cL=qnorm(0.05)#quantile(lzs,0.05)
cU=quantile(U3s,0.95)
cat("\n",cH)
cat("\n",cL)
cat("\n",cU)
# Compute PFSs for Original data
itparms=cbind(a,b,rep(0,nitem))
#Compute the ability estimates for the combined data using `irtoys'
thetaest=mlebme(d,itparms)[,1]
#Compute PFSs Using PerFit
Hts = Ht(d)[[1]]$PFscores
lzs=lzstar(d,Ability=thetaest,IP=itparms)[[1]]$PFscores
U3s=U3(d,Ability=thetaest,IP=itparms)[[1]]$PFscores
# Compute Percent Significant
flag=dat[,1]
cat("\n\n",100*length(Hts[Hts<cH])/length(Hts),"\n")
cat(100*length(Hts[Hts<cH & flag==1])/length(Hts[flag==1]),"\n")
cat(100*length(Hts[Hts<cH & flag==0])/length(Hts[flag==0]),"\n")
cat(100*length(U3s[U3s>cU])/length(Hts),"\n")
cat(100*length(U3s[U3s>cU & flag==1])/length(U3s[flag==1]),"\n")
cat(100*length(U3s[U3s>cU & flag==0])/length(U3s[flag==0]),"\n")
cat(100*length(lzs[lzs<cL])/length(lzs),"\n")
cat(100*length(lzs[lzs<cL & flag==1])/length(lzs[flag==1]),"\n")
cat(100*length(lzs[lzs<cL & flag==0])/length(lzs[flag==0]),"\n")

