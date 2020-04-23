\name{FHDI_Driver}
\alias{FHDI_Driver}
\title{Fractional Hot Deck Imputation}

\description{
Fully efficient fractional imputation (FEFI) or fractional hot deck imputation (FHDI) is implemented to fill in missing values in a incomplete data.
}
\usage{
FHDI_Driver(daty, datr=NULL, datz=NULL, s_op_imputation="FEFI", 
 i_op_SIS=0, s_op_SIS = "global",
 i_op_variance=1, M=5, k=5, w=NULL, id=NULL,
 s_op_merge="fixed", categorical=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{daty}{raw data matrix (nrow_y, ncol_y) containing missing values. Each row must have at least one observed value, and no completely missing (blank) rows are allowed.}
  \item{datr}{response indicator matrix with the same dimensions as daty. Each response is recorded with 0 for missing value and 1 for observed value. If NULL, automatically filled with 1 or 0 according to daty.} 
  \item{datz}{imputation cell matrix. If daty is a set of continuous data, datz can be obtained using \code{\link{FHDI_CellMake}.}}
  \item{s_op_imputation}{"FEFI" for fully efficient fractional imputation or "FHDI" for fractional hot deck imputation.}
  \item{i_op_SIS}{(FHDI Version >= 1.4) the desired number of reduced variables after the sure independence screening per each missing pattern. Default = 0 means no variable reduction and uses all variables. Range must be <= ncol_y.}
  \item{s_op_SIS}{(FHDI Version >= 1.4) "intersection" for sure independence screening with an intersection of simple correlation, "union" for sure independence screening with a union of simple correlation, or default = "global" for sure independence screening with a global ranking of simple correlation.}
  \item{i_op_variance}{1: perform Jackknife variance estimation; 0: no variance estimation.}
  \item{M}{the number of donors for FHDI with default 5.}
  \item{k}{the number of total categories per variable. Default = 5. The maximum is 35 since 9 integers (1-9) and 26 alphabet letters (a-z) are used. When a scalar value is given, all variables will have the same number of categories, while when a vector is given, i.e. k(ncol_y), each variable may have different number of categories.}
  \item{w}{samping weight for each row of daty. Default = 1.0 if NULL. When a scalar value is given, all rows will have the same weight, while when a vector is given, i.e. w(nrow_y), each row may have a different sampling weight.}
  \item{id}{index for each row. Default = 1:nrow_y if NULL.}
  \item{s_op_merge}{option for random cell make. Default = "fixed" using the same seed number; "rand" using a purely random seed number.}
  \item{categorical}{(FHDI Version >1.3) index vector indicating non-collapsible categorical variables. Default = zero vector of size ncol_y. For instance, when categorical=c(1,0), the first variable (i.e., 1st column) is considered strictly non-collapsible categorical, and thus no automatic cell-collapse will take place while the second variable (i.e., 2nd column) is considered as continuous or collapsible categorical variable.}
}
\details{
In the FEFI method, all possible donors are assigned to each missing unit with the FEFI fractional weights. In the FHDI method, 
M (>1) donors are selected with the probability proportional to the FEFI fractional weights. Thus, the imputed values have equal
fractional weights in general.

The jackknife replicated weights are produced as the default output. The replicated weights are presented by the
product of replicated sampling weights and replicated fractional weights. Thus, the replicated weights can be directly used
to compute the variance estimate of the estimators.
From versrion >= 1.4, the sure independence screening method (Fan and Lv 2008) has been incorporated to perform variable reduction for each missing pattern, which is useful for high dimensional (i.e. big-p) data sets.
}
\value{
  \item{fimp.data}{imputation results with fractional weights in a form of matrix consisting of ID, donor id (FID), weight (WGT), fractional weight (FWGT), and fractionally imputed data.}
  \item{simp.data}{imputed data in the format of single imputation. The same shape as daty.}
  \item{imp.mean}{the mean estimates of each variable (first row) and the estimated standard error of each variable (second row). If input argument "i_op_variance=0" then this output is not produced.}
  \item{rep.weight}{replication fractional weights for variance estimation. If input argument "i_op_variance=0" then this output is not produced.}
  \item{M}{reprint of the number of donors M for FHDI defined by the user.}
  \item{s_op_imputation}{reprint of the option "s_op_imputation" initially defined by the user.}  
  \item{i_op_merge}{reprint of the option "i_op_merge" initially defined by the user.}  
  \item{i_op_SIS}{reprint of the option "i_op_SIS" initially defined by the user.}
  \item{s_op_SIS}{reprint of the option "s_op_SIS" initially defined by the user.}
}
\references{
Im, J., Cho, I.H. and Kim, J.K. (2018). FHDI: An \bold{R} Package for Fractional Hot-Deck Imputation. \emph{The R Journal}. 10(1), pp. 140-154; 
Im, J., Kim, J.K. and Fuller, W.A. (2015). Two-phase sampling approach to fractional hot deck imputation, \emph{Proceeding of the Survey Research Methods Section}, Americal Statistical Association, Seattle, WA. 
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
FEFI_SIS=FHDI_Driver(daty, i_op_SIS=2, s_op_SIS="intersection", k=3)

names(result_FEFI)
names(result_FHDI)
names(FEFI_SIS)

}
\keyword{imputation}
\keyword{missing data}