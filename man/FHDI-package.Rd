\name{FHDI-package}
\alias{FHDI-package}
\alias{FHDI}
\docType{package}
\title{Fractional Hot Deck Imputation}
\description{Perform fractional hot deck imputation or perform fully efficient fractional imputation. This package is partially supported by the NSF grant CSSI 1931380.}
\details{
FHDI_Driver(daty, datr=NULL, datz=NULL, s_op_imputation="FEFI", i_op_SIS=0, 
     s_op_SIS="global", s_op_cellmake="knn", top_corr_var=100, i_op_variance=1, M=5, k=5, w=NULL, id=NULL, 
	 s_op_merge="fixed", categorical=NULL)
}
\seealso{FHDI_CellMake and FHDI_CellProb}
\author{
Author: Inho Cho [aut, cre], Jaekwang Kim [aut], Jongho Im [aut], Yicheng Yang [aut]
\email{icho@iastate.edu}}
\references{
Im, J., Cho, I.H. and Kim, J.K. (2018). FHDI: An \bold{R} Package for Fractional Hot-Deck Imputation. \emph{The R Journal}. 10(1), pp. 140-154; 
Im, J., Kim, J.K. and Fuller, W.A. (2015). Two-phase sampling approach to fractional hot deck imputation, \emph{Proceeding of the Survey Research Methods Section}, Americal Statistical Association, Seattle, WA. 
}
\examples{
### Toy Example ### 
# y : multi-variate vector
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
result_FHDI_merging=FHDI_Driver(daty, s_op_imputation="FHDI", s_op_cellmake="merging", M=5, k=3)
FEFI_SIS=FHDI_Driver(daty, i_op_SIS=2, s_op_SIS="intersection", k=3)

names(result_FEFI)
names(result_FHDI)
names(result_FHDI_merging)
names(FEFI_SIS)
}
\keyword{FHDI}
