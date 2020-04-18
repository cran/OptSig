\name{Power.Chisq}
\alias{Power.Chisq}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to calculate the power of a Chi-square test
}
\description{
This function calculates the power of a Chi-square test, given the value of non-centrality parameter
}
\usage{
Power.Chisq(df, ncp, alpha, Figure = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{ degree of freedom}
  \item{ncp}{ a value of of the non-centality paramter}
  \item{alpha}{ the level of significance}
  \item{Figure}{ show graph if TRUE (default); No graph if FALSE }
}
\details{
See Kim and Choi (2020) 
}
\value{
 \item{Power }{Power of the test}
  \item{Crit.val  }{Critical value at alpha level of signifcance}
}
\references{
Kim and Choi, 2020, Choosing the Level of Significance: A Decision-theoretic Approach, Abacus, Wiley.
<https://doi.org/10.1111/abac.12172>

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

Kim, Jae H., 2020, Decision-theoretic hypothesis testing: A primer with R package OptSig, The American Statistician. 
<https://doi.org/10.1080/00031305.2020.1750484.>

}

\examples{
Power.Chisq(df=5,ncp=5,alpha=0.05,Figure=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ htest }% __ONLY ONE__ keyword per line
