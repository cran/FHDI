\name{FHDI_CellProb}
\alias{FHDI_CellProb}
\title{Joint cell probabilities for multivariate incomplete categorical data}

\description{
Calculate the joint cell probabilities for multivariate missing data using the expectation maximization algorithm.  
}
\usage{
FHDI_CellProb(datz, w=NULL, id=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datz}{multivariate incomplete categorical data.}
  \item{w}{samping weight. Default = 1.0 if NULL. a scalar or w(nrow_y).}
  \item{id}{index for each unit. Default = 1:nrow_y if NULL.}
}
\details{
The joint cell probabilities are estimated using EM by weighting method. The algorithm computes the maximum likelihood estimates
of the joint cell probabilities under missing at random assumption. 
Note that the variable reduction (ver. >= 1.4) with sure independence screening method is not applicable to separate CellProb task.}
\value{
  \item{cellpr}{table of the joint cell probability. name of cell is linked to the user-defined categories in "k": e.g., name "325" denotes 3rd, 2nd, 5th categories for three variables, respectively, whereas "a1c" denotes 10th, 1st, 12th categories.}
  \item{w}{reprint of the sampling weights "w" initially defined by the user.}
}
\references{
Im, J., Cho, I.H. and Kim, J.K. (2018). FHDI: An \bold{R} Package for Fractional Hot-Deck Imputation. \emph{The R Journal}. 10(1), pp. 140-154; 
Im, J., Kim, J.K. and Fuller, W.A. (2015). Two-phase sampling approach to fractional hot deck imputation, \emph{Proceeding of the Survey Research Methods Section}, Americal Statistical Association, Seattle, WA.;  
Ibrahim, J.G. (1990). Incomplete data in generalized linear models. \emph{Journal of the American Statistical Assocation} \bold{85}, 765-769. 
}
\author{
Dr. Cho, In Ho (maintainer)
\email{icho@iastate.edu}
Dr. Kim, Jae Kwang
\email{jkim@iastate.edu}
Dr. Im, Jong Ho
\email{ijh38@yonsei.ac.kr}
Yicheng Yang, Graduate Research Assistant
}
\examples{
### Toy Example ### 
# y : trivariate variables
# r : indicator corresponding to missingness in y

set.seed(1345) 
n=100 
rho=0.5 
e1=rnorm(n,0,1) 
e2=rnorm(n,0,1) 
e3=rgamma(n,1,1) 
e4=rnorm(n,0,sd=sqrt(3/2))

y1=1+e1 
y2=2+rho*e1+sqrt(1-rho^2)*e2 
y3=y1+e3 
y4=-1+0.5*y3+e4

r1=rbinom(n,1,prob=0.6) 
r2=rbinom(n,1,prob=0.7) 
r3=rbinom(n,1,prob=0.8) 
r4=rbinom(n,1,prob=0.9)

y1[r1==0]=NA 
y2[r2==0]=NA 
y3[r3==0]=NA 
y4[r4==0]=NA

daty=cbind(y1,y2,y3,y4)

result_CM=FHDI_CellMake(daty, k=5, s_op_merge="fixed")
datz=result_CM$cell
result_CP=FHDI_CellProb(datz)
names(result_CP)
}
\keyword{imputation}
\keyword{missing data}
\keyword{joint probability}
\keyword{cellprob}
\keyword{EM algorithm}