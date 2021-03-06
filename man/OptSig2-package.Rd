\name{OptSig-package}
\alias{OptSig-package}
\alias{OptSig}
\docType{package}
\title{
\packageTitle{OptSig}
}
\description{
\packageDescription{OptSig}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{OptSig}
\packageIndices{OptSig}
The package accompanies the paper: Kim and Choi, 2020, Choosing the Level of Significance: A Decision-theoretic Approach. Abacus. Wiley. 

It oprovides functions for the optimal level of significance for the test for linear restiction in a regeression model. 

Other basic statistical tests, including those for population mean and proportion, are also covered using the functions from the pwr package.

}
\author{
\packageAuthor{OptSig}

Maintainer: \packageMaintainer{OptSig}
}
\references{
Kim and Choi, 2020, Choosing the Level of Significance: A Decision-theoretic Approach: Abacus: a Journal of Accounting, Finance and Business Studies. Wiley. 
<https://doi.org/10.1111/abac.12172>

Cohen, J. (1988). Statistical power analysis for the behavioral sciences (2nd ed.). Hillsdale,NJ: Lawrence Erlbaum.

Stephane Champely (2017). pwr: Basic Functions for Power Analysis. R package version 1.2-1.
  https://CRAN.R-project.org/package=pwr
}

\keyword{ package }
\seealso{
Leamer, E. 1978, Specification Searches: Ad Hoc Inference with Nonexperimental Data, Wiley, New York.

Kim, JH and Ji, P. 2015, Significance Testing in Empirical Finance: A Critical Review and Assessment, Journal of Empirical Finance 34, 1-14.
<DOI:http://dx.doi.org/10.1016/j.jempfin.2015.08.006>

Kim, Jae H., 2020, Decision-theoretic hypothesis testing: A primer with R package OptSig, The American Statistician. 
<https://doi.org/10.1080/00031305.2020.1750484.>

}
\examples{
data(data1)
y=data1$lnoutput; x=cbind(data1$lncapital,data1$lnlabor)
# Restriction matrices to test for constant returns to scale
Rmat=matrix(c(0,1,1),nrow=1); rvec=matrix(0.94,nrow=1)
# Model Estimation and F-test
M=R.OLS(y,x,Rmat,rvec) 

# Degrees of Freedom and estimate of non-centrality parameter 
K=ncol(x)+1; T=length(y)
df1=nrow(Rmat);df2=T-K; NCP=M$ncp

# Optimal level of Significance: Under Normality
OptSig.F(df1,df2,ncp=NCP,p=0.5,k=1, Figure=TRUE)
}
