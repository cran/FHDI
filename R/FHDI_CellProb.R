FHDI_CellProb<-function(datz, w=NULL, id=NULL)	
{
#Description------------------------------Update: April 12, 2018
# main driver for Fully Efficient Fractional Imputation (FEFI) and 
#                 Fractional Hot Deck Imputation (FHDI)
# Perform Cell Prob ONLY!
#
#IN   : double datz[,] 	= categorized values of the original data matrix 
#IN   : double w		= if a single number, all weights have the same weights
#                         if a vector of size nrow, use it as it is 
#IN   : int    id		= if a single integer, all indices will be sequential from 1 to nrow
#                         if a vector of size nrow, use it as it is 
#OUT  : List of 
#       [[1]] names of joint probability cells
#       [[2]] joint probability values  
#----------------------------------------------

#-----
#make sure the input data is in matrix
#-----
datz <- data.matrix(datz);

ncol_z = ncol(datz);
nrow_z = nrow(datz);

#------
#error check
#------

if(length(id) ==1)
{print("ERROR! the size of id is not the same as the number of raw data"); 
 return(NULL);}
if(length(id) >1 && length(id) != nrow_z)
{print("ERROR! the size of id is not the same as the number of raw data"); 
 return(NULL);}
if(length(w) == 1)
{print("ERROR! the size of w is not the same as the number of raw data"); 
 return(NULL); }
if(length(w) > 1 && length(w) != nrow_z)
{print("ERROR! the size of w is not the same as the number of raw data"); 
 return(NULL); }

#------------
#make a vector form of input data
#------------
if(is.null(id))  id = 1:nrow_z
if(is.null(w))   w = rep(1.0, nrow_z)


 #testout
#print("Cell_Prob Only started")

#----------------------
#call FHDI_test as the separate function
#Jan 11, 2017
#----------------------
List_FHDI_CellProb <- .Call("CWrapper_CellProb", datz, nrow_z, ncol_z, w, id);

#abnormal ending
if(is.null(List_FHDI_CellProb))
{
	print("Error took place during FHDI_CellProb! \n"); 
	return(NULL); 
}


#joint probability values
output_FHDI_CellProb <- List_FHDI_CellProb[[2]]
#attach the names to the jp
names(output_FHDI_CellProb)=List_FHDI_CellProb[[1]]

final=list(cellpr=output_FHDI_CellProb,w=w)
class(final)=append(class(final),"CellProb")
return(final)
}

