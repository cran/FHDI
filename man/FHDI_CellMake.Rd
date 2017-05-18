\name{FHDI_CellMake}
\alias{FHDI_CellMake}
\title{Imputation cell creation}

\description{
Perform a categorization procedure on the continuous raw data and then create imputation cells through a built-in merge algorithm.   
}
\usage{
FHDI_CellMake(daty, datr=NULL, k=5, w=NULL, id=NULL, s_op_merge="fixed")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{daty}{raw data matrix (nrow_y, ncol_y) containing missing values.}
  \item{datr}{response indicator matrix with the same dimensions as daty. Each response is recorded with 0 for missing value and 1 for observed value. If NULL, automatically filled with 1 or 0 according to daty} 
  \item{k}{the number of total categories per variable. Default: 5; maximum: 35. a scalar or k(ncol_y).}
  \item{w}{samping weight for each data row. Default = 1.0 if NULL. a scalar or w(nrow_y).}
  \item{id}{index for each row. Default = 1:nrow_y if NULL.}
  \item{s_op_merge}{option for random cell make. Default = "fixed" using the same seed number; "rand" using random seed number.}
}
\details{
This function creates imputation cells with the given number of category k. 
If the input value k is given a scalar, the same number of category is applied into all variables for initial discretization.
Imputation cells are created to assign at least two donors on each missing unit. 
The donors have the same cell values with the observed parts of the missing unit.
}
\value{
  \item{y}{raw data (nrow_y, ncol_y) attached with id and weights.}
  \item{z}{categorized matrix of y.}
  \item{uox}{unique patterns of donors who are fully observed.}
  \item{mox}{unique patterns of recipients who have at least one missing item.}
}
\references{
Im, J., Kim, J.K. and Fuller, W.A. (2015). Two-phase sampling approach to fractional hot deck imputation, \emph{Proceeding of the Survey Research Methods Section}, Americal Statistical Association, Seattle, WA. 
Im, J., Cho, I.H. and Kim, J.K. (2017). FHDI: A \bold{R} Package for Fractional Hot-Deck Imputation for multivariate missing data. \emph{Journal of Statistical Software}. Under review.
}
\author{
Dr. Im, Jong Ho
\email{jonghoim@iastate.edu}
Dr. Cho, In Ho
\email{icho@iastate.edu}
Dr. Kim, Jae Kwang
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

result_CM=FHDI_CellMake(daty, s_op_merge="fixed",k=3)
}
\keyword{imputation}
\keyword{missing data}
\keyword{categorization}
\keyword{cellmake}