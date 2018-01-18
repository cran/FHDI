\name{FHDI-package}
\alias{FHDI-package}
\alias{FHDI}
\docType{package}
\title{Fractional Hot Deck Imputation}
\description{Perform fractional hot deck imputation 
Perform fully efficient fractional imputation}
\details{
FHDI_Driver(daty, datr=NULL, datz=NULL, s_op_imputation="FEFI", i_op_variance=0, M = 5, k = 5, w = NULL, id = NULL, s_op_merge="fixed");
}
\seealso{FHDI_CellMake and FHDI_CellProb}
\author{
Author: Jongho Im [aut], Inho Cho [aut, cre], Jaekwang Kim [aut]
\email{icho@iastate.edu}}
\references{
Im, J., Kim, J.K. and Fuller, W.A. (2015). Two-phase sampling approach to fractional hot deck imputation, \emph{Proceeding of the Survey Research Methods Section}, Americal Statistical Association, Seattle, WA. 
Im, J., Cho, I.H. and Kim, J.K. (2017). FHDI: An \bold{R} Package for Fractional Hot-Deck Imputation. \emph{The R Journal}. Under review.
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

result_FEFI=FHDI_Driver(daty, s_op_imputation="FEFI", k=3)
result_FHDI=FHDI_Driver(daty, s_op_imputation="FHDI", M=5, k=3)
}
\keyword{FHDI}
