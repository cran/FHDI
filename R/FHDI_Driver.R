FHDI_Driver<-function(daty, datr=NULL, datz=NULL, s_op_imputation="FEFI", i_op_SIS = 0, s_op_SIS = "global", s_op_cellmake = "knn", top_corr_var = 100,
                      i_op_variance=1, M=5, k=5, w=NULL, id=NULL, s_op_merge="fixed", categorical=NULL)
{
#Description------------------------------UPDATE: Aug 18, 2020 
#
# main driver for Fully Efficient Fractional Imputation (FEFI) and 
#                 Fractional Hot Deck Imputation (FHDI)
# Perform Cell Make, Cell Prob,  FEFI/FHDI imputation, and 
#         Jackknife Var Est. (if requested) 
#
#IN   : double daty[,] 	= original data matrix containing missing units
#IN   : double datr[,] 	= indices for the observed unit (1) and missing unit (0) 
#IN   : double datz[,]  = user-defined category matrix (this will override Cell Make function)
#IN   : int    M 		= the number of donors used for FHDI. Default = 5
#IN   : int    k		= if a single number, used as the total categories per variable. Default = 5 (max = 35)
#                         if a vector of size nrow, use it as it is
#IN   : string  s_op_imputation = FEFI; FHDI
#IN   : int  i_op_variance  = 1; #1:perform variance estimation; 0:skip variance estimation
#IN   : double w		= if a single number, all weights have the same weights
#                         if a vector of size nrow, use it as it is 
#IN   : int    id		= if a single integer, all indices will be sequential from 1 to nrow
#                         if a vector of size nrow, use it as it is 
#IN   : string  s_op_merge = rand (default = fixed)
#               if requested, do random donor selection in Merge algorithm of Cell Make 
#
#IN   : int    categorical	= a index vector with size of ncol(daty)
#                             when a column has 1, the variable is non-collapsible categorical 
#							 when a column has 0, the variable is collapsible categorical or continuous
#                             the default is all 0
#
#IN   : int     i_op_SIS  = 0; #0: perform FHDI without variable selection; !0: perform FHDI with user-defined
#                                  number of selected variables. Default = 0   
#IN   : string  s_op_SIS  = 3; #1: SIS with intersection; 2: SIS with intersection 3: SIS with global ranking
#IN   : int     s_op_cellmake = 1; #1: perform cell construction with merging; 2: perform cell construction
#                                      with k-nearest-neighbor
#IN   : int     top_corr_var = the number of top ranks of variables based on correlation. Default = 100
#  
#OUT  : List of 
#       rbind_ipmat(4+ncol) // ID, FID, WGT, FWGT, Imputed Variables
#       cured data matrix(nrow, ncol)
#       Mean and Standard Error (2,ncol)
#       rbind_vrst(nrow)    // Jackknife variance estimates  (returned when i_option_variation_R = 1)
#----------------------------------------------

#----------------
#(1: all FEFI/FHDI; 2: CellMake; 3: CellProb; 4: all FEFI/FHDI using datz)
#----------------
i_option_perform = 1; #main control option 

#----------------
#daty matrix
#-----
#make sure the input data is in matrix
#-----
daty <- data.matrix(daty);

#----------------
#datr matrix
#-----
#when user defined datr
#-----
if(!is.null(datr))
{
	datr <- data.matrix(datr);
	if(nrow(datr) != nrow(daty) || ncol(datr) != ncol(daty)) 
	{
		print("Error! datr has different dimensions from daty! so user-defined datr cannot be used"); 
	  return(NULL); 
	}
}
#-----
#when datr is NOT Defined
#-----
if(is.null(datr))
{
	datr = matrix(1, nrow(daty), ncol(daty)); #default is 1 meaning the observed cells
	datr[is.na(daty)==T] = 0 ; #put 0 to cells where daty has NA 
}


#----------------
#datz matrix
#----------------
z_UserDefined = matrix(0.0, nrow(daty), ncol(daty)); 
if(!is.null(datz)) 
{
	z_UserDefined<-data.matrix(datz); 
	if(nrow(z_UserDefined) == nrow(daty) && ncol(z_UserDefined) == ncol(daty))
	{i_option_perform = 4;}
	
	if(nrow(z_UserDefined) != nrow(daty) || ncol(z_UserDefined) != ncol(daty)) 
	{print("Caution! datz has different dimensions from daty! so user-defined datz is not used"); }
}

#----
#basic option setting
#----
if(s_op_imputation == "FEFI"){i_option_imputation = 1;}
if(s_op_imputation == "FHDI"){i_option_imputation = 2;}
#i_option_imputation = i_op_imputation; #1:FEFI;  2:FHDI
i_option_variance  = i_op_variance ; #1:perform variance estimation; 0:skip variance estimation
ncol_y = ncol(daty);
nrow_y = nrow(daty);
ncol_r = ncol(datr);
nrow_r = nrow(datr);

i_option_merge = 0; #random merge algorithm. Default = 0 
if(s_op_merge == "rand") {i_option_merge = 1; set.seed(NULL);} 
if(s_op_merge == "fixed"){i_option_merge = 0; set.seed(123);}

i_option_SIS = i_op_SIS; #0: no variable selection; !0: perform variable selection

if(s_op_SIS == "intersection") {s_option_SIS = 1;} #perform SIS with intersection
if(s_op_SIS == "union") {s_option_SIS = 2;} #perform SIS with union
if(s_op_SIS == "global") {s_option_SIS = 3;} #perform SIS with global ranking

if(s_op_cellmake == "merging"){s_option_cellmake = 1;} #1: cell make with merging;
if(s_op_cellmake == "knn"){s_option_cellmake = 2;} #2: cell make with KNN

top_corr = top_corr_var; # Default = 100
#---------
#Error check
#---------
if(is.null(FHDI_Error_Check(ncol_y, ncol_r, nrow_y, nrow_r, M, k, id, w, 
                            s_op_imputation, i_op_SIS, s_op_SIS, s_op_cellmake, top_corr_var)))
{return(NULL);}



#------------
#make a vector form of input data
#------------
if(length(k)==1) k = rep(k, ncol_y)
if(is.null(id))  id = 1:nrow_y
if(is.null(w))   w = rep(1.0, nrow_y)

#-----------
#non-collapsible categorical variable consideration
#-----------
NonCollapsible_categorical = rep(0, ncol_y); #default 
if(is.null(categorical)) NonCollapsible_categorical = rep(0, ncol_y)
if(!is.null(categorical))
{
	#size check
	if(length(categorical) != ncol_y)#incorrect size
	{
		print("Error! check the size of categorical[], the index vector for non-collapsible variables!")
		return(NULL); 
	}
	#value check
	if(length(categorical) == ncol_y) #correct size 
	{
		n_categorical_zero = 0; 
		n_categorical_one  = 0; 
		n_categorical_wrong = 0; 
		for(i in 1:ncol_y)
		{
			if(categorical[i] == 0) n_categorical_zero = n_categorical_zero + 1; 
			if(categorical[i] == 1) n_categorical_one  = n_categorical_one  + 1;
			if(categorical[i] !=0 && categorical[i] != 1) n_categorical_wrong = n_categorical_wrong + 1;
		}
		if(n_categorical_wrong >= 1)
		{
			print("Error! check values of categorical[]; must be either 0 or 1 !")
			return(NULL); 		
		}
	}
	
	#only when correct definition of categorical[]
	for(i in 1:ncol_y) NonCollapsible_categorical[i] = categorical[i]; 
}
  
#-------------------
#to avoid NA argument error in external C++ function
#-------------------
for(i in 1:ncol_y){
	for(j in 1:nrow_y){
		r_i = datr[j,i]; #one unit of r
		#below is replaced with a short integer to avoid a possible error 
		#if(r_i==0) daty[j,i]=123456789123456789; #replace NA unit with this long number 
		if(r_i==0) daty[j,i]=1234567899; #replace NA unit with this long number 

		}
}

#----------------------
#call FHDI_test as the separate function
#Jan 11, 2017
#----------------------
#output_FHDI <- FHDI_test(daty, datr, k, w, M, i_option_imputation, i_option_variance,
#                         id);
output_FHDI <- .Call("CWrapper", daty, datr, z_UserDefined, i_option_perform,
                nrow_y, ncol_y, k, w, M, 
                i_option_imputation, i_option_variance, id, 
			          NonCollapsible_categorical, i_option_SIS, s_option_SIS,
				        i_option_merge, s_option_cellmake, top_corr)

#abnormal ending
if(is.null(output_FHDI))
{
	print("Error took place during FHDI_Driver!"); 
	return(NULL); 
}
				
				
#----------------------
#put column names to output with those of daty 
#----------------------
column_name_of_y = vector();
for(i in 1:ncol_y){ 
	if(!is.nan(colnames(daty)[i])) column_name_of_y[i] = colnames(daty)[i];
	if(is.nan(colnames(daty)[i])) column_name_of_y[i] = paste("V",i,sep="");
	}
#cured data with size of (nrow, ncol)
colnames(output_FHDI[[2]])<-column_name_of_y
#ipmat with size of (nrow, 4+ncol)
colnames(output_FHDI[[1]])<-c("ID", "FID", "WT", "FWT", column_name_of_y)

#S3 class
final=list(fimp.data=output_FHDI[[1]],simp.data=output_FHDI[[2]])

if(i_op_variance!=0)  final=c(final,list(imp.mean=output_FHDI[[3]],rep.weight=output_FHDI[[4]]))

final=c(final,list(M=M,s_op_imputation=s_op_imputation,s_op_merge=s_op_merge, i_op_SIS =i_op_SIS, s_op_SIS =s_op_SIS, 
                   s_op_cellmake = s_op_cellmake, top_corr_var = top_corr_var))
class(final)=append(class(final),"Driver")

return(final)
}

#==========================================================
#==========================================================
#==========================================================
#==========================================================
#==========================================================


#==========================================================
#==========================================================
#==========================================================
#==========================================================
#==========================================================


#==========================================================
#==========================================================
#==========================================================
#==========================================================
#==========================================================

FHDI_Error_Check <- function(ncol_y, ncol_r, nrow_y, nrow_r, M, k, id, w, 
                             s_op_imputation, i_op_SIS, s_op_SIS, s_op_cellmake, top_corr_var)
{
	#---------
	#Error check
	#---------
	if(ncol_y != ncol_r || nrow_y != nrow_r)
	{print("ERROR! daty and datr must have the same dimensions"); 
	 return(NULL);}

	if(length(k) >1  && length(k) != ncol_y)
	{print("ERROR! the size of k is not the same as the number of variables"); 
	 return(NULL);}
	if(length(k)==1)
	{
		if(k<1 || k>35) # as of Feb 2017, maximum of k is 35
		{
			print("ERROR! k is out of the allowed range of [1,35]");
			return(NULL);
		}
	}
	
	if(length(M)==1)
	{
		if(M<1 || M>nrow_y) 
		{
			print("ERROR! M is out of the allowed range of [1,n]");
			return(NULL);
		}
	}	
	
	if(length(id) ==1)
	{print("ERROR! the size of id is not the same as the number of raw data"); 
	 return(NULL);}
	if(length(id) >1 && length(id) != nrow_y)
	{print("ERROR! the size of id is not the same as the number of raw data"); 
	 return(NULL);}
	if(length(w) == 1)
	{print("ERROR! the size of w is not the same as the number of raw data"); 
	 return(NULL); }
	if(length(w) > 1 && length(w) != nrow_y)
	{print("ERROR! the size of w is not the same as the number of raw data"); 
	 return(NULL); }

	if(s_op_imputation != "FEFI" && s_op_imputation != "FHDI")
	{print("ERROR! imputation method is different from FEFI and FHDI"); 
	 return(NULL); }
	 
  if(i_op_SIS > ncol_y)
  {print("ERROR! i_op_SIS is out of the allowed range [0,number of variables]");
    return(NULL); }
  
  if(i_op_SIS < 0)
  {print("ERROR! i_op_SIS is out of the allowed range [0,number of variables]");
    return(NULL);}
  
  if(i_op_SIS%%1 !=0)
  {print("ERROR! i_op_SIS is not an integer in the allowed range [0,number of variables]");
    return(NULL);}
  
  if(s_op_SIS != "intersection" && s_op_SIS != "union" && s_op_SIS != "global")
  {print("ERROR! sure independence screening method is different from intersection, union, and global");
    return(NULL);}
  
  if(s_op_cellmake != "merging" && s_op_cellmake != "knn"){
    print("ERROR! method of cell construction is different from merging and knn");
    return(NULL);}
  
  if(top_corr_var <= 0){
    print("ERROR! top_corr_var must be positive");
    return(NULL);}
  
  if(top_corr_var%%1 !=0){
    print("ERROR! top_corr_var is not an integer");
    return(NULL);}
  
	
	return(1); 
}
