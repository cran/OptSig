\name{Power.F}
\alias{Power.F}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Function to calculate the power of an F-test
}
\description{
This function calculates the power of an F-test, given the value of non-centrality parameter
}
\usage{
Power.F(df1, df2, ncp, alpha, Figure = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{df1}{ the first degrees of freedom for the F-distribution}
  \item{df2}{ the second degrees of freedom for the F-distribution}
  \item{ncp}{ a value of of the non-centality paramter}
  \item{alpha}{ the level of significance}
  \item{Figure}{ show graph if TRUE (default); No graph if FALSE }
}
\details{
See Kim and Choi (2017) 
}
\value{
  \item{Power }{Power of the test}
  \item{Crit.val  }{Critical value at alpha level of signifcance}

}
\references{
Kim and Choi, 2017, Choosing the Level of Significance: A Decision-theoretic Approach: https://papers.ssrn.com/sol3/papers.cfm?abstract_id=2652773
}
\author{
Jae H. Kim
}
\note{
See Application Section and Appendix of Kim and Choi (2017)
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
Leamer, E. 1978, Specification Searches: Ad Hoc Inference with Nonexperimental Data, Wiley, New York.

Kim, JH and Ji, P. 2015, Significance Testing in Empirical Finance: A Critical Review and Assessment, Journal of Empirical Finance 34, 1-14.
<DOI:http://dx.doi.org/10.1016/j.jempfin.2015.08.006>
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

Power.F(df1,df2,ncp=NCP,alpha=0.20747,Figure=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ htest }% __ONLY ONE__ keyword per line
