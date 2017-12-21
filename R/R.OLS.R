R.OLS <-
function(y,x,Rmat,rvec){
  
  y=as.matrix(y);x=as.matrix(x);n=nrow(y); 
  
  x=cbind(rep(1,n),x)
  
  mat1=solve( t(x) %*% x )
  b =  mat1 %*% t(x) %*% y
  e=y-x%*%b
  s2=sum(e^2)/(n-ncol(x)); #s2=sum(e^2)/n
  var.b = s2 * mat1
  t.stat = b / sqrt(diag(var.b))
  tem = y-mean(y)
  R2 = 1-sum(e^2)/sum(tem^2)
  
  lamda1=solve(s2*Rmat %*% mat1 %*% t(Rmat)); 
  lamda2=Rmat%*%b - rvec
  
  f1=t(lamda2)%*%lamda1 %*% lamda2
  f.stat=f1/nrow(Rmat); pval=pf(f.stat,df1=nrow(Rmat),df2=n-ncol(x),lower.tail=FALSE)
  f=cbind(f.stat,pval); colnames(f)=c("stat","p-val")

  br=b-mat1%*%t(Rmat)%*% (s2*lamda1)%*%lamda2
  er=y-x%*%br
  s2r=sum(er^2)/(n-ncol(x))
  R2r = 1-sum(er^2)/sum(tem^2)
  #var.br = s2r * mat1 - s2r^2 * mat1 %*% t(Rmat) %*% lamda1 %*% Rmat %*% mat1
  #t.statr = br / sqrt(diag(var.br))
  
  col.name = c("H1", "H0"); row.name=c("c",paste("X",1:(ncol(x)-1),sep=""))
  bmat=cbind(b,br); colnames(bmat)=col.name; rownames(bmat)=row.name
  #tratio= cbind(t.stat,t.statr); tratio[is.nan(tratio)] = NA; tratio[is.infinite(tratio)] = NA; 
  #colnames(tratio)=col.name; rownames(tratio)=row.name
  R2=cbind(R2,R2r); colnames(R2)=col.name
  resid = cbind(e,er); colnames(resid)=col.name
  
  ncp=t(lamda2)%*%lamda1%*%lamda2 #ncp=n*(R2[1]-R2[2])/(1-R2[1]) #the two are equal only when the s2=sum(e^2)/n
    
  return(list(coef=bmat,Rsq=R2,resid=resid,F.stat=f,ncp=ncp))
}
