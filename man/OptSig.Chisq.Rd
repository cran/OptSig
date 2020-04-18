\name{OptSig.Chisq}
\alias{OptSig.Chisq}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Optimal Significance Level for a Chi-square test
}
\description{
The function calculates the optimal level of significance for a Ch-square test 
}
\usage{
OptSig.Chisq(w=NULL, N=NULL, ncp=NULL, df, p = 0.5, k = 1, Figure = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{w}{Effect size, Cohen's w}
  \item{N}{Total number of observations}
  \item{ncp}{ a value of the non-centality paramter} 
  \item{df}{ the degrees of freedom}
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
\item{beta.opt }{Type II error probability at the optimal level}
}
\references{
Kim and Choi, 2020, Choosing the Level of Significance: A Decision-theoretic Approach: Abacus: a Journal of Accounting, Finance and Business Studies. Wiley. 
<https://doi.org/10.1111/abac.12172>

}
\author{
Jae. H Kim
}
\note{
Applicable to any Chi-square test
Either ncp or w (with N) should be given. 

The black curve in the figure is the line of enlightened judgement: see Kim and Choi (2020).
The red dot inticates the optimal significance level that minimizes the expected loss: (alpha.opt,beta.opt).
The blue horizontal line indicates the case of alpha = 0.05 as a reference point.     

}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
Leamer, E. 1978, Specification Searches: Ad Hoc Inference with Nonexperimental Data, Wiley, New York.

Cohen, J. (1988). Statistical power analysis for the behavioral sciences (2nd ed.). Hillsdale,NJ: Lawrence Erlbaum.

Kim, JH and Ji, P. 2015, Significance Testing in Empirical Finance: A Critical Review and Assessment, Journal of Empirical Finance 34, 1-14.
<DOI:http://dx.doi.org/10.1016/j.jempfin.2015.08.006>

Kim, Jae H., 2020, Decision-theoretic hypothesis testing: A primer with R package OptSig, The American Statistician. 
<https://doi.org/10.1080/00031305.2020.1750484.>

}

\examples{
# Optimal level of Significance for the Breusch-Pagan test: Chi-square version
data(data1)                 # call the data: Table 2.1 of Gujarati (2015)

# Extract Y and X
y=data1$lnoutput; x=cbind(data1$lncapital,data1$lnlabor)

# Restriction matrices for the slope coefficents sum to 1
Rmat=matrix(c(0,1,1),nrow=1); rvec=matrix(1,nrow=1)

# Model Estimation
M=R.OLS(y,x,Rmat,rvec); print(M$coef)

# Breusch-Pagan test for heteroskedasticity
e = M$resid[,1]                  # residuals from unrestricted model estimation

# Restriction matrices for the slope coefficients being 0
Rmat=matrix(c(0,0,1,0,0,1),nrow=2); rvec=matrix(0,nrow=2)

# Model Estimation for the auxilliary regression
M1=R.OLS(e^2,x,Rmat,rvec); 

# Degrees of Freedom and estimate of non-centrality parameter 
df1=nrow(Rmat); NCP=M1$ncp

# LM stat and p-value
LM=nrow(data1)*M1$Rsq[1,1]
pval=pchisq(LM,df=df1,lower.tail = FALSE)

OptSig.Chisq(df=df1,ncp=NCP,p=0.5,k=1, Figure=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ htest }% use one of  RShowDoc("KEYWORDS")

