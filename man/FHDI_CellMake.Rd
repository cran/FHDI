\name{FHDI_CellMake}
\alias{FHDI_CellMake}
\title{Imputation Cell Creation}

\description{
Perform a categorization procedure on the continuous raw data and then create imputation cells through a built-in merge algorithm. This package is partially supported by the NSF grant CSSI 1931380.
}
\usage{
FHDI_CellMake(daty, datr=NULL, k=5, w=NULL, id=NULL, i_op_SIS=0, 
              s_op_SIS="global", s_op_cellmake="knn", top_corr_var=100, 
              s_op_merge="fixed", categorical=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{daty}{raw data matrix (nrow_y, ncol_y) containing missing values. Each row must have at least one observed value, and no completely missing (blank) rows are allowed.}
  \item{datr}{response indicator matrix with the same dimensions as daty. Each response is recorded with 0 for missing value and 1 for observed value. If NULL, automatically filled with 1 or 0 according to daty.} 
  \item{k}{the number of total categories per variable. Default = 5. The maximum is 35 since 9 integers (1-9) and 26 alphabet letters (a-z) are used. When a scalar value is given, all variables will have the same number of categories, while when a vector is given, i.e., k(ncol_y), each variable may have a different number of categories.}
  \item{w}{sampling weight for each row of daty. Default = 1.0 if NULL. When a scalar value is given, all rows will have the same weight, while when a vector is given, i.e., w(nrow_y), each row may have a different sampling weight.}
  \item{id}{index for each row. Default = 1:nrow_y if NULL.}
  \item{i_op_SIS}{(FHDI Version >= 1.4) the desired number of reduced variables after the sure independence screening per each missing pattern. Default = 0 means no variable reduction and uses all variables. Range must be <= ncol_y.}  
  \item{s_op_SIS}{(FHDI Version >= 1.4) "intersection" for sure independence screening with an intersection of simple correlation, "union" for sure independence screening with a union of simple correlation, or default = "global" for sure independence screening with a global ranking of simple correlation.}
  \item{s_op_cellmake}{(FHDI Version >= 1.4) option for different methods of cell construction with deficient donors. "merging" for adopting cell collapsing and merging or default = "knn" for adopting the k-nearest neighbors in terms of the Euclidean distance.}
  \item{top_corr_var}{(FHDI Version >= 1.4) the number of top-ranking variables based on simple correlation with default 100.}
  \item{s_op_merge}{option for random cell make. Default = "fixed" using the same seed number; "rand" using a purely random seed number.}
  \item{categorical}{(FHDI Version >1.3) index vector indicating non-collapsible categorical variables. Default = zero vector of size ncol_y. For instance, when categorical=c(1,0), the first variable (i.e., 1st column) is considered strictly to be non-collapsible and categorical, thus no automatic cell-collapse will take place while the second variable (i.e., 2nd column) is considered as a continuous or collapsible categorical variable.}
}
  
\details{
This function creates imputation cells with the given number of categories k. 
If the input value k is given a scalar, the same number of categories is applied to all variables for initial discretization. 
Imputation cells are created to assign at least two donors on each missing unit. 
The donors have the same cell values with the observed parts of the missing unit. 
From version >= 1.4, the sure independence screening method (Fan and Lv 2008) has been incorporated to perform variable reduction for each missing pattern, which is useful for high dimensional (i.e., big-p) datasets. 
Besides, we provide an alternative method using k-nearest neighbors to speed up the convergence of cell construction with deficient donors, which is useful for big-p datasets.
}
\value{
  \item{data}{matrix of raw data (nrow_y, ncol_y) attached with id and weights, w.}
  \item{cell}{categorized matrix of y. A real value is categorized into 1~k categories with 0 meaning missing value.}
  \item{cell.resp}{unique patterns of respondents (donors) that are fully observed.}
  \item{cell.non.resp}{unique patterns of nonrespondents that have at least one missing item.}
  \item{w}{reprint of the sampling weights "w" initially defined by the user.}
  \item{s_op_merge}{reprint of the option "s_op_merge" initially defined by the user.}  
  \item{i_op_SIS}{reprint of the option "i_op_SIS" initially defined by the user.}
  \item{s_op_SIS}{reprint of the option "s_op_SIS" initially defined by the user.}
  \item{s_op_cellmake}{reprint of the option "s_op_cellmake" initially defined by the user.}
  \item{top_corr_var}{reprint of the option "top_corr_var" initially defined by the user.}
  \item{cell.selected}{list of selected variables for each unique pattern of nonrespondents that have at least one missing item. Note that all the observed variables of a unique missing pattern will be selected if i_op_SIS is greater than the number of observed variables of the unique missing pattern; otherwise, the deficient selected variables are replaced by 0s.}
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

result_CM=FHDI_CellMake(daty, k=3, s_op_merge="fixed")
result_CM_merging=FHDI_CellMake(daty, k=3, s_op_cellmake ="merging", s_op_merge="fixed")
result_CM_reduced=FHDI_CellMake(daty, k=3, i_op_SIS=2, s_op_SIS="intersection", s_op_merge="fixed")
names(result_CM)
names(result_CM_merging)
names(result_CM_reduced)
}
\keyword{imputation}
\keyword{missing data}
\keyword{categorization}
\keyword{cellmake}