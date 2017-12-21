Power.F <-
function(df1,df2,ncp,alpha,Figure=TRUE){
  
  cr1=qf(1-alpha,df1=df1,df2=df2)
  power1 = pf(cr1,df1=df1,df2=df2,ncp=ncp,lower.tail = FALSE)
  
  if (Figure == TRUE){
    ef=df2*(df1+ncp)/(df1*(df2-2))
    vf1=(df1+ncp)^2+(df1+2*ncp)*(df2-2); 
    vf2= (df2-2)^2*(df2-4)
    vf=2*vf1/vf2 * (df2/df1)^2
    
    
    xlim=ef+3*sqrt(vf)
    x=seq(0,xlim,0.01)
    density=df(x,df1=df1, df2=df2)
    density1=df(x,df1=df1, df2=df2,ncp=ncp)
    
    plot(x,density,type="l",lwd=3,main="Distributions under H0 and H1") 
    points(x,density1,type="l",col="red",lwd=3)
    abline(v=cr1,col="blue")
    
    region.x=x[ x > cr1]
    region.y=density[ x > cr1]
    region.x=c(cr1,region.x,tail(region.x,1))
    region.y=c(0,region.y,0)
    polygon(region.x,region.y,density=-1,col="gray")
    
    region.x=x[ x > cr1]
    region.y=density1[ x > cr1]
    region.x=c(cr1,region.x,tail(region.x,1))
    region.y=c(0,region.y,0)
    polygon(region.x,region.y,density=10,col="red")
    abline(v=cr1,col="blue",lwd=3)
    abline(h=0,lwd=1)
  }
  return(list(Power=power1,Crit.val=cr1))}
