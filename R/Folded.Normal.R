Folded.Normal <-
function(m,s,Figure=TRUE){
  options(warn=-1)
  x=seq(-m-4*s,m+4*s,length.out = 30)
  d=ifelse(x > 0, 1, 0) * (dnorm(x,mean=m,sd=s) + dnorm(-x,mean=-m,sd=s))
  if(Figure == TRUE) {plot(x,d,type="l",xlim=c(0,max(x)),main="Folded Normal Distribution",lwd=2)
    abline(v=m,col="blue"); abline(h=0)}
  d=d/sum(d)
  dat1=cbind(x,d)
  dat2=dat1[ dat1[,1] > 0, ] 
  return(list(x=dat2[,1],w=dat2[,2]))
}
