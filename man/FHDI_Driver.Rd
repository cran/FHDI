\name{FHDI_Driver}
\alias{FHDI_Driver}
\title{Fractional Hot Deck Imputation}

\description{
Fully efficient fractional imputation (FEFI) or fractional hot deck imputation (FHDI) is implemented to fill in missing values in a incomplete data.
}
\usage{
FHDI_Driver(daty, datr=NULL, datz=NULL, s_op_imputation="FEFI",
 i_op_variance=1, M=5, k=5, w=NULL, id=NULL,
 s_op_merge="fixed")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{daty}{raw data matrix (nrow_y, ncol_y) containing missing values.}
  \item{datr}{response indicator matrix with the same dimensions as daty. Each response is recorded with 0 for missing value and 1 for observed value. If NULL, automatically filled with 1 or 0 according to daty} 
  \item{datz}{imputation cell matrix. If daty is a set of continuous data, datz can be obtained using \code{\link{FHDI_CellMake}.}}
  \item{s_op_imputation}{"FEFI" for fully efficient fractional imputation or "FHDI" for fractional hot deck imputation.}
  \item{i_op_variance}{1: perform Jackknife variance estimation; 0: no variance estimation.}
  \item{M}{the number of donors for FHDI with default 5.}
  \item{k}{the number of total categories per variable. Default: 5; maximum: 35. a scalar or k(ncol_y).}
  \item{w}{samping weight for each data row. Default = 1.0 if NULL. a scalar or w(nrow_y).}
  \item{id}{index for each row. Default = 1:nrow_y if NULL.}
  \item{s_op_merge}{option for random cell make. Default = "fixed" using the same seed number; "rand" using random seed number.}
}
\details{
In the FEFI method, all possible donors are assigned to each missing unit with the FEFI fractional weights. In the FHDI method, 
M (>1) donors are selected with the probability proportional to the FEFI fractional weights. Thus, the imputed values have equal
fractional weights in general.

The jackknife replicated weights are produced as the default output. The replicated weights are presented by the
product of replicated sampling weights and replicated fractional weights. Thus, the replicated weights can be directly used
to compute the variance estimate of the estimators.
}
\value{
  \item{ipmat}{matrix of ID, donor id (FID), weight (WGT), fractional weight (FWGT) and fractionally imputed data.}
  \item{y}{an imputed data with the shape of single imputation.}
  \item{Mean_and_SD}{Column mean of imputed matrix and Jackknife variance estimate of the mean of each variable}
  \item{vrst}{matrix of Jackknife replicate weights}
}
\references{
Im, J., Kim, J.K. and Fuller, W.A. (2015). Two-phase sampling approach to fractional hot deck imputation, \emph{Proceeding of the Survey Research Methods Section}, Americal Statistical Association, Seattle, WA. 
Im, J., Cho, I.H. and Kim, J.K. (2017). FHDI: A \bold{R} Package for Fractional Hot-Deck Imputation for multivariate missing data. \emph{Journal of Statistical Software}. Under review.
}
\author{
Dr. Im, Jongho
\email{jonghoim@iastate.edu}
Dr. Cho, Inho
\email{icho@iastate.edu}
Dr. Kim, Jaekwang
\email{jkim@iastate.edu}
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
\keyword{imputation}
\keyword{missing data}