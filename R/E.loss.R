E.loss <-
function(alphavec,betavec,p,k,Figure=TRUE){
LOSS=cbind(alphavec,betavec,abs(p*alphavec+(1-p)*k*betavec))
index= LOSS[which.min(LOSS[,3]),]
alphas=index[1];betas=index[2]; names(alphas) = NULL; names(betas) = NULL

if (Figure == TRUE) {
plot(betavec,alphavec,type="l",xlim=c(0,1),col=1,lwd=2,ylab="alpha",xlab="beta",main="Optimal Level of Significance") 
points(index[2],index[1],col="red",pch=15)
abline(v=seq(0,1,0.1), col="lightgray", lty="dotted")
abline(h=seq(0,1,0.1), col="lightgray", lty="dotted")
abline(h = 0.05, col = "blue", lwd = 1)
}
return(list(alpha.opt = round(alphas,digits = 5), beta.opt = round(betas,digits = 5)))
}
