library(FLsz)

dirEx=system.file("example", package="FLsz", mustWork=TRUE)
 
params=FLPar(c(linf=94.7,k=0.258,t0=-1.354,a=3.119e-5,b=2.88,mat50len=62,m=0.3,lc=60))
  
t2sz=read.csv(paste(dirEx,"ALBM_t2szFreqs_v1.csv",sep="/"))
dat =read.csv(paste(dirEx,"mnLen.csv",            sep="/"),sep=";")
dat$n=1/(dat$se)^2
  
p.<-transform(ddply(subset(t2sz,YearC>1974), .(ClassFrq,YearC), function(x) sum(x$Nr)), Year=YearC-decade(YearC), Decade=decade(YearC))
p.<-ddply(p., .(Year, Decade), function(x) transform(x, Nr=V1/(sum(V1))))

ggplot(p.) + 
      geom_histogram(aes(ClassFrq,weight=Nr),binwidth=5,fill="darkgreen",colour="black") + 
      facet_grid(Year~Decade)                            +
      scale_x_continuous(limit=c(20,100))               +
      geom_vline(aes(xintercept=V1),
                 data=ddply(p., .(Year, Decade), function(x) with(x, sum(ClassFrq*Nr)/sum(Nr))),
                 colour="red",size=1.5)+
      scale_x_continuous(breaks=c(50,75), limits=c(20, 100)) +
      scale_y_continuous(breaks=c(0.5)) +
      xlab("Catch-at-Size") +ylab("Proportion")

ggplot(dat) + geom_point(aes(year,ln)) + geom_line(aes(year,ln)) +
              geom_point(aes(year,nominal),colour="blue") + geom_line(aes(year,nominal),colour="blue") +
              geom_linerange(aes(year,ymin=ln-se*2,ymax=ln+se*2)) +
              xlab("Year") + ylab("Mean Length (m)")
 
albSz=FLsz(dat,grw=params,breaks=c(1990,2002))
  
bounds(albSz)[1:3,"initial"]=0.1
    
n(albSz)[,ac(c(1977,2009))]=0

## this might fail if windows path where R lib installed has spaces etc. 
albSz=fit(albSz)

## if so specify a directory that you know exists
#albSz=fit(albSz,dir="c:/temp")
  
diags(albSz)
 
plot(albSz)
 
albJK=transform(albSz,obs=jacknife(albSz@obs))
albJK=fit(albJK)
  
plot(albJK)