FHDI_CellMake<-function(daty, datr=NULL, k=5,
                        w=NULL, id=NULL, s_op_merge="fixed")	
{
#Description------------------------------update: April 12, 2018
# main driver for Fully Efficient Fractional Imputation (FEFI) and 
#                 Fractional Hot Deck Imputation (FHDI)
# Perform (1) Cell Make ONLY!
#
#IN   : double daty[,] 	= original data matrix containing missing units
#IN   : double datr[,] 	= indices for the observed unit (1) and missing unit (0)
#IN   : int    k		= if a single number, used as the total categories per variable. Default = 5 (max = 35)
#                         if a vector of size nrow, use it as it is
#IN   : double w		= if a single number, all weights have the same weights
#                         if a vector of size nrow, use it as it is 
#IN   : int    id		= if a single integer, all indices will be sequential from 1 to nrow
#                         if a vector of size nrow, use it as it is 
#IN   : string  s_op_merge = rand (default = fixed)
#               if requested, do random donor selection in Merge algorithm of Cell Make 
#OUT  : List of 
#   	[[1]] = ID, WGT, original data matrix 	(nrow, 2+ncol)
#		[[2]] = categorized matrix 				(nrow, ncol)
#		[[3]] = uox, the observed patterns 		(.., ncol)
#		[[4]] = mox, the missing patterns		(.., ncol)
#----------------------------------------------

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
		return; 
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



#----
#basic option setting
#----
#if(s_op_imputation == "FEFI"){i_option_imputation = 1;}
#if(s_op_imputation == "FHDI"){i_option_imputation = 2;}
#i_option_imputation = i_op_imputation; #1:FEFI;  2:FHDI
#i_option_variance  = i_op_variance ; #1:perform variance estimation; 0:skip variance estimation
ncol_y = ncol(daty);
nrow_y = nrow(daty);
ncol_r = ncol(datr);
nrow_r = nrow(datr);

i_option_merge = 0; #random merge algorithm. Default = 0 
if(s_op_merge == "rand") {i_option_merge = 1; set.seed(NULL);} 
if(s_op_merge == "fixed"){i_option_merge = 0; set.seed(123);}

#-----------
#below is dummy value. Not used for CellMake function separately. 
#-----------
i_option_imputation = 1;
i_option_variance = 1; 
M = 5;  
s_op_imputation = "FEFI"

#---------
#Error check
#---------
if(is.null(FHDI_Error_Check(ncol_y, ncol_r, nrow_y, nrow_r, M, k, id, w, 
                            s_op_imputation)))
{return(NULL);}

#------------
#make a vector form of input data
#------------
if(length(k)==1) k = rep(k, ncol_y)
if(is.null(id))  id = 1:nrow_y
if(is.null(w))   w = rep(1.0, nrow_y)

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
						 
output_FHDI_CellMake <- .Call("CWrapper_CellMake", daty, datr, nrow_y, ncol_y, k, w, M, 
                i_option_imputation, i_option_variance, id, i_option_merge)						 

				
#abnormal ending
if(is.null(output_FHDI_CellMake))
{
	print("Error took place during FHDI_CellMake! "); 
	return(NULL); 
}
 
#-------------------
#reset to NA in raw data matrix
#-------------------
#List[[1]] is the ID, WGT, raw data
#-------------------
for(i in 1:ncol_y){
	for(j in 1:nrow_y){
		r_i = datr[j,i]; #one unit of r
		if(r_i==0) output_FHDI_CellMake[[1]][j,i+2]=NA;  
	}
}

#----------------------
#put column names to output with those of daty 
#----------------------
column_name_of_y = vector();
for(i in 1:ncol_y){ 
	if(!is.nan(colnames(daty)[i])) column_name_of_y[i] = colnames(daty)[i];
	if(is.nan(colnames(daty)[i])) column_name_of_y[i] = paste("V",i,sep="");
	}
#ID, WGT, original data with size of (nrow, 2+ncol)
colnames(output_FHDI_CellMake[[1]])<-c("ID", "WT", column_name_of_y)
#categorized matrix with size of (nrow, ncol)
colnames(output_FHDI_CellMake[[2]])<-column_name_of_y
#uox matrix with size of (.., ncol)
colnames(output_FHDI_CellMake[[3]])<-column_name_of_y
#mox matrix with size of (.., ncol)
colnames(output_FHDI_CellMake[[4]])<-column_name_of_y

final=list(data=output_FHDI_CellMake[[1]],cell=output_FHDI_CellMake[[2]],cell.resp=output_FHDI_CellMake[[3]],
      cell.non.resp=output_FHDI_CellMake[[4]],w=w,s_op_merge="fixed")
class(final)=append(class(final),"CellMake")
return(final);
}


