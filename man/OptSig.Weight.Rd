\name{OptSig.Weight}
\alias{OptSig.Weight}
\title{
Weighted Optimal Significance Level for the F-test based on the assumption of normality in the error term
}
\description{
The function calculates the weighted optimal level of significance for the F-test 

The weights are obtained from a folded-normal distribution with mean m and staradrd deviation delta

}
\usage{
OptSig.Weight(df1, df2, m, delta = 2, p = 0.5, k = 1, Figure = TRUE)
}
\arguments{
 \item{df1}{ the first degrees of freedom for the F-distribution}
  \item{df2}{ the second degrees of freedom for the F-distribution}
  \item{m}{ a value of of the non-centality paramter, the mean of the folded-normal distribution}
  \item{delta}{ standard deviation of the folded-normal distribution}
  \item{p}{ prior probability for H0, default is p = 0.5}
  \item{k}{ relative loss from Type I and II errors, k = L2/L1, default is k = 1}
  \item{Figure}{ show graph if TRUE (default); No graph if FALSE }
}
\details{
See Kim and Choi (2020) 
}
\value{
\item{alpha.opt }{Optimal level of significance}
\item{crit.opt }{Critical value at the optimal level}
}
\references{
Kim and Choi, 2020, Choosing the Level of Significance: A Decision-theoretic Approach, Abacus, Wiley. 
<https://doi.org/10.1111/abac.12172>

}
\author{
Jae H. Kim
}
\note{
The figure shows the folded-normal distribution
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
Leamer, E. 1978, Specification Searches: Ad Hoc Inference with Nonexperimental Data, Wiley, New York.

Kim, JH and Ji, P. 2015, Significance Testing in Empirical Finance: A Critical Review and Assessment, Journal of Empirical Finance 34, 1-14.
<DOI:http://dx.doi.org/10.1016/j.jempfin.2015.08.006>

Kim, Jae H., 2020, Decision-theoretic hypothesis testing: A primer with R package OptSig, The American Statistician. 
<https://doi.org/10.1080/00031305.2020.1750484.>


}
\examples{
data(data1)
# Define Y and X
y=data1$lnoutput; x=cbind(data1$lncapital,data1$lnlabor)
# Restriction matrices to test for constant returns to scale
Rmat=matrix(c(0,1,1),nrow=1); rvec=matrix(0.94,nrow=1)
# Model Estimation and F-test
M=R.OLS(y,x,Rmat,rvec) 

# Degrees of Freedom and estimate of non-centrality parameter 
K=ncol(x)+1; T=length(y)
df1=nrow(Rmat);df2=T-K; NCP=M$ncp

OptSig.Weight(df1,df2,m=NCP,delta=3,p=0.5,k=1,Figure=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ htest }% use one of  RShowDoc("KEYWORDS")

