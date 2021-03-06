//LAST UPDATE: Sept 21, 2020


//!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#define R_NO_REMAP //so that R does not define length(x) which may cause many complie error with fstream

#define i_DEBUGGING 0 //0=no printout; 1=printout for debugging 


#include <R.h>

#include <Rinternals.h>

#include <Rmath.h>  

#include <limits> // For NaN 

#include <iostream> // For  flush 

#include <assert.h> // For assert

#include <algorithm> // For sort 

#include <string>		//For string array

#include <vector>

#include <cmath>

#include <fstream>

#include <cstdlib>

#include <time.h>  //for time()



using namespace std; 

//using std::cout; 

//using std::cerr; 

//using std::endl; 





//Fn===========================================================================
//matrix_utility_FHDI.cc-------------------------------------------------------
//Fn===========================================================================
//below local functions for avoiding error for other compilers 
int    fabs_FHDI(int x)    { if(x>=0)   {return x;} if(x<0)   {return x*-1;}   return x;}
double fabs_FHDI(double x) { if(x>=0.0) {return x;} if(x<0.0) {return x*-1.0;} return x;}


//---------------------
//Collection of basic matrix and vector utilities
//for FHDI program 

//October 5, 2016

//

//Developed by Dr. In-Ho Cho

//All rights reserved

//----------------------

void Copy_dVector(double Source[], int n, double Target[])

{

   for(int i=0; i<n; i++)

   {

      Target[i] = Source[i];

   }

   return;

}



//==============================================================================

//==============================================================================

void Copy_iVector(int Source[], int n, int Target[])

{

   for(int i=0; i<n; i++)

   {

      Target[i] = Source[i];

   }

   return;

}



//==============================================================================

//==============================================================================

void Copy_dMatrix(double** Source, int n_row, int n_col, double** Target)

{

   for(int i_row=0; i_row<n_row; i_row++)

   {

      for(int i_col=0; i_col<n_col; i_col++)

      {

         Target[i_row][i_col] = Source[i_row][i_col];

      }



   }

   return;

}



//==============================================================================

//==============================================================================

void Copy_iMatrix(int** Source, int n_row, int n_col, int** Target)

{

   for(int i_row=0; i_row<n_row; i_row++)

   {

      for(int i_col=0; i_col<n_col; i_col++)

      {

         Target[i_row][i_col] = Source[i_row][i_col];

      }



   }

   return;

}



//==============================================================================

//==============================================================================

double ** New_dMatrix(int n_row, int n_col)

//Description================================

// make new double MATRIX

//============================================

{

   //===============================================

   // NON contiguous dynamic multidimensional array.

   // for MPI version ineffective from June 15 09

   //===============================================

   double ** matrix;



   matrix = new double*[n_row];

   for(int i=0; i<n_row;i++)

   {

      matrix[i] = new double[n_col];



      for(int j=0; j<n_col; j++)

      {

         matrix[i][j] = 0.0;

      }

   }





   /*

   //==========================================

   //Contiguous dynamic multidimensional array

   //which is essential for MPI usage

   //for MPI version effective from June 15 09

   //==========================================

   double ** matrix;



   matrix = new double*[n_row];

   matrix[0] = new double[n_row*n_col];//allocate the total storage as a contiguous block

   for(int i=1; i<n_row; i++)

   {

      matrix[i] = matrix[0] + i*n_col; //matrix[i] points to the entire block of ith row

   }



   for(int i=0; i<n_row;i++)

   {

      for(int j=0; j<n_col; j++)

      {

         matrix[i][j] = 0.0;

      }

   }

   */

   return matrix;

}



//==============================================================================

//==============================================================================

void Del_dMatrix(double ** matrix, int n_row, int n_col)

//Description================================

// delete the double MATRIX

//============================================

{

   for(int i=0; i<n_row; i++)

   {

      delete[] matrix[i];

   }

   delete[] matrix;

}



//==============================================================================

//==============================================================================

int** New_iMatrix(int n_row, int n_col)

//Description================================

// make new integer MATRIX

//============================================

{

   //===============================================

   // NON contiguous dynamic multidimensional array.

   // for MPI version ineffective from June 15 09

   //===============================================

   int ** matrix;

   matrix = new int*[n_row];

   for(int i=0; i<n_row;i++)

   {

      matrix[i] = new int[n_col];



      for(int j=0; j<n_col; j++)

      {

         matrix[i][j] = 0;

      }

   }





   /*

   //==========================================

   //Contiguous dynamic multidimensional array

   //which is essential for MPI usage

   //for MPI version effective from June 15 09

   //==========================================

   int** matrix ;

   matrix = new int*[n_row];

   matrix[0] = new int[n_row*n_col]; //allocate the total storage of entire block



   for(int i=1; i<n_row; i++)

   {

      matrix[i] = matrix[0] + i*n_col; //allocate sequential addr starting from [0]

   }



   for(int i=0; i<n_row; i++)

   {

      for(int j=0; j<n_col; j++)

      {

         matrix[i][j] = 0 ;

      }

   }

   */

   return matrix;

}



//==============================================================================

//==============================================================================

void Del_iMatrix(int ** matrix, int n_row, int n_col)

//Description================================

// delete the integer MATRIX

//============================================

{

   for(int i=0; i<n_row; i++)

   {

      delete[] matrix[i];

   }

   delete[] matrix;

}

//==============================================================================

//==============================================================================

int Find_iValue(int** i_matrix, int n_row, int n_col,

                char s_rowcol, int n_rowcol, int i_value)

// Description======================

//IN   : int** i_matrix[n_row][n_col]

//OUT  : (i, n_rowcol) = position of the 'i_value'

//     or(n_rowcol, i)

//

//   by searching i_matrix[:][n_rowcol] when s_rowcol="r"

//             or i_matrix[n_rowcol][:] when s_rowcol="c"

//

//Note: -1 is returned when there is no matched value

//===================================

{

   if(s_rowcol == 'r')

   {

      for(int i=0; i<n_row; i++)

      {

         if(i_matrix[i][n_rowcol] == i_value)

         {

            return i;

         }

      }

   }

   else if(s_rowcol == 'c')

   {

      for(int i=0; i<n_col; i++)

      {

         if(i_matrix[n_rowcol][i] == i_value)

         {

            return i;

         }

      }



   }

   return -1; //when no matched value

}



//==============================================================================

//==============================================================================

int Find_dValue(double** d_matrix, int n_row, int n_col,

                char s_rowcol, int n_rowcol, double d_value)

// Description======================

//IN   : double** d_matrix[n_row][n_col]

//OUT  : (i, n_rowcol) = position of the 'd_value'

//     or(n_rowcol, i)

//   by searching d_matrix[:][n_rowcol] when s_rowcol="row"

//             or d_matrix[n_rowcol][:] when s_rowcol="col"

//===================================

{

   if(s_rowcol == 'r')

   {

      for(int i=0; i<n_row; i++)

      {

         double d_temp=0.0;

         d_temp = fabs_FHDI(d_matrix[i][n_rowcol] - d_value);

         if(d_temp < 10.E-10)

         {

            return i;

         }

      }

   }

   else if(s_rowcol == 'c')

   {

      for(int i=0; i<n_col; i++)

      {

         double d_temp=0.0;

         d_temp = fabs_FHDI(d_matrix[n_rowcol][i] - d_value);

         if(d_temp < 10.E-10)

         {

            return i;

         }

      }



   }

   return -1; //when no matched value



}

//==============================================================================

//==============================================================================

int iMaxValue(int** i_matrix, int n_row, int n_col, char s_where,

              int n_begin, int n_end, int n_at)

//Description=================

//

// find maximum int value in the int matrix

// (1) s_where ="row" ; search through row(within n_begin~n_end) at n_at col

// (2) s_where ="col" ; search through col(within n_begin~n_end) at n_at row

// (3) s_where ="all" ; search all matrix

//

//IN   : int** i_matrix[n_row][n_col]

//OUT  : int maximum value

//============================



{

   int i_maximum =0;

   int i_temp=0;



   if(s_where =='r')

   {

      for(int i=n_begin;i<=n_end; i++)

      {

         if(i_temp < i_matrix[i][n_at]) i_temp = i_matrix[i][n_at] ;

      }

   }

   else if(s_where =='c')

   {

      for(int i=n_begin;i<=n_end; i++)

      {

         if(i_temp < i_matrix[n_at][i]) i_temp = i_matrix[n_at][i] ;

      }

   }

   else if(s_where =='a')

   {

      for(int i=0;i<n_row; i++)

      {

         for(int j=0; j<n_col; j++)

         {

            if(i_temp < i_matrix[i][j]) i_temp = i_matrix[i][j] ;

         }

      }



   }



   i_maximum = i_temp;

   return i_maximum;

}



//==============================================================================

//==============================================================================

int iMinValue(int** i_matrix, int n_row, int n_col, char s_where,

              int n_begin, int n_end, int n_at)

//Description=================

//

// find minimum int value in the int matrix

// note: do search among only positive values

//

// (1) s_where ="row" ; search through row(within n_begin~n_end) at n_at col

// (2) s_where ="col" ; search through col(within n_begin~n_end) at n_at row

// (3) s_where ="all" ; search all matrix

//

//IN   : int** i_matrix[n_row][n_col]

//OUT  : int minimum value

//============================



{

   int i_minimum =0;

   int i_temp=0;//max_previous;



   if(s_where =='r')

   {

      //initiallize i_temp value

      for(int i=n_begin;i<=n_end; i++)

      {

         if(i_matrix[i][n_at]>0)

         {

            i_temp = i_matrix[i][n_at] ;

            break; //exit this loop

         }

      }



      for(int i=n_begin;i<=n_end; i++)

      {



         if(i_temp > i_matrix[i][n_at]&& i_matrix[i][n_at]>0)

            i_temp = i_matrix[i][n_at] ;

      }

   }

   else if(s_where =='c')

   {

      //initialize i_temp value

      for(int i=n_begin;i<=n_end; i++)

      {

         if(i_matrix[n_at][i]>0)

         {

            i_temp = i_matrix[n_at][i] ;

            break; //exit this loop

         }

      }



      for(int i=n_begin;i<=n_end; i++)

      {

         if(i_temp > i_matrix[n_at][i]&& i_matrix[n_at][i]>0)

            i_temp = i_matrix[n_at][i] ;

      }

   }

   else if(s_where =='a')

   {

      //initialize i_temp

      for(int i=0;i<n_row; i++)

      {

         for(int j=0; j<n_col; j++)

         {

            if(i_matrix[i][n_at]>0)

            {

              i_temp = i_matrix[i][j] ;

              break; //exit this loop

            }

         }

      }



      for(int i=0;i<n_row; i++)

      {

         for(int j=0; j<n_col; j++)

         {

            if(i_temp > i_matrix[i][j]&& i_matrix[i][n_at]>0)

               i_temp = i_matrix[i][j] ;

         }

      }

   }



   i_minimum = i_temp;

   return i_minimum;



}



//==============================================================================

//==============================================================================

void Fill_dVector(double *d_vector, const int n_size, const double value)

{

   for(int i=0; i<n_size; i++)

   {

      d_vector[i] = value;

   }



   return;

}



//==============================================================================

//==============================================================================

void Fill_iVector(int i_vector[], const int n_size, const int value)

{

   for(int i=0; i<n_size; i++)

   {

      i_vector[i] = value;

   }



   return;

}



//==============================================================================

//==============================================================================

void Fill_dMatrix(double** d_Matrix, int n_row, int n_col, double value)

{

   for(int i=0; i<n_row; i++)

   {

      for(int j=0; j<n_col; j++) d_Matrix[i][j] = value;

   }

   return;

}

//==============================================================================

//==============================================================================

void Fill_iMatrix(int** i_Matrix, int n_row, int n_col, int value)

{

   for(int i=0; i<n_row; i++)

   {

      for(int j=0; j<n_col; j++) i_Matrix[i][j] = value;

   }

   return;

}



//==============================================================================

//==============================================================================

void Inverse_dMatrix(double** d_Mat, const int n, double** d_Inv)

//Description===================

//return inverse matrix of the n*n matrix

//using Gauss-Jordan elimination

//

// if diagonal term is zero and too small

// perform pivoting with largest value on the columns

//

//IN    : double** d_Mat[n][n]

//OUT   : double** d_Inv[n][n]

//==============================

{

   const double eps=1.e-15 ;





   //make d_Inv unity matrix

   for(int i=0; i<n; i++)

   {

      for(int j=0; j<n; j++) d_Inv[i][j] =0.0;



      d_Inv[i][i]=1.0 ;

   }







   double c=0.0 ; //coeff.



   for(int i_diag=0; i_diag<n; i_diag++)

   {

      c = d_Mat[i_diag][i_diag];



      //================

      //when diagonal term is too small or zero, then needs pivoting

      //================

      if(fabs_FHDI(c)<eps )

      {

         //find max on current column ======

         double d_temp= c;  int i_loc=i_diag;



         for(int i=(n-1); i>i_diag; i--)//from the nth ~ (i_diag+1)

         {

            if(fabs_FHDI(d_temp)< fabs_FHDI(d_Mat[i][i_diag]) )

            {

               i_loc = i ;

               d_temp = d_Mat[i][i_diag] ;

            }

         }



         //When Pivot is necessary!

         if(i_loc != i_diag)

         {

            for(int i=0; i<n; i++)

            {

               d_temp    = d_Mat[i_diag][i] ; //store temporarily

               d_Mat[i_diag][i] = d_Mat[i_loc][i] ;//exchange with the max.

               d_Mat[i_loc][i]  = d_temp ;



               d_temp    = d_Inv[i_diag][i] ; //store temporarily

               d_Inv[i_diag][i] = d_Inv[i_loc][i] ;//exchange with the max.

               d_Inv[i_loc][i]  = d_temp ;

            }

         }

         else if(i_loc == i_diag) //can't find max value than current diagonal term

         {

            Rprintf("Error! no pivoting is possible with current mat. in invers matrix");

            return;

         }

      }





      c = d_Mat[i_diag][i_diag];  //get original or exchanged one





      //make current diag. term 1.0

      for(int i=0; i<n; i++)

      {

         d_Mat[i_diag][i]= d_Mat[i_diag][i] /c; //divide [i_diag]th row terms with the diagonal term

         d_Inv[i_diag][i]= d_Inv[i_diag][i] /c;

      }



      //Lower term elimination================

      //eliminate below terms of [i_diag]th col

      if(i_diag == n-1) continue; // don't need below for the last diagonal



      for(int j=i_diag+1; j<n; j++)

      {

         c=d_Mat[j][i_diag] ; //jth row first term



         for(int i=0; i<n; i++)   //all jth row terms

         {

            d_Mat[j][i] = d_Mat[j][i] -c* d_Mat[i_diag][i];

            d_Inv[j][i] = d_Inv[j][i] -c* d_Inv[i_diag][i];

         }

      }

   }



   //Upper term elimination================

   //subtract upper terms of [i_diag]th col

   for(int i_diag=1; i_diag<n; i_diag++) //note: begin from the second row

   {

      for(int j=0; j<i_diag; j++)

      {

         c=d_Mat[j][i_diag] ; //jth row first term



         for(int i=0; i<n; i++)   //all jth row terms

         {

            d_Mat[j][i] = d_Mat[j][i] -c* d_Mat[i_diag][i];

            d_Inv[j][i] = d_Inv[j][i] -c* d_Inv[i_diag][i];

         }

      }

   }



   return; 

}



//==============================================================================

//==============================================================================

void dMatrix_Mul_AB(double** A, int n_row, int n_col1,

                double** B, int n_col2,

                double** AB)

//Description================================

//  matrix multiplication

//  C = A*B

//IN   :double** A(n_row, n_col1)

//     :double** B(n_col1, n_col2)

//OUT  :double** AB(n_row, n_col2)

//===========================================

{

   const double tolerance = 10.E-15;

   double d_temp=0.0;



   for(int ic=0; ic<n_col2; ic++)

   {

      for(int ir=0; ir<n_row; ir++)

      {

         d_temp=0.0;



         for(int i=0; i<n_col1; i++)

         {

            d_temp = d_temp + A[ir][i]*B[i][ic] ;

         }

         if(fabs_FHDI(d_temp) < tolerance ) d_temp =0.0 ; //delete numerical error



         AB[ir][ic] = d_temp ;

      }

   }



   return;

}



//==============================================================================

//==============================================================================





void dMatrix_Mul_AtB(double** A, int n_row, int n_col1,

                double** B, int n_col2,

                double** AtB)

//Description================================

//  matrix multiplication

//  AtB = transpose(A)*B

//IN   :double** A(n_row, n_col1)

//     :double** B(n_row, n_col2)

//OUT  :double** AtB(n_col1, n_col2)

//===========================================

{

   const double tolerance = 10.E-15;

   double d_temp=0.0;



   for(int ic=0; ic<n_col2; ic++)

   {

      for(int ir=0; ir<n_col1; ir++)

      {

         d_temp=0.0;



         for(int i=0; i<n_row; i++)

         {

            d_temp = d_temp + A[i][ir]*B[i][ic] ;

         }



         if(fabs_FHDI(d_temp) < tolerance ) d_temp =0.0 ; //delete numerical error

         AtB[ir][ic] = d_temp ;

      }

   }



   return;

}



//==============================================================================

//==============================================================================



void dMatrix_Mul_AtBA(double** A, const int n_row, const int n_col,

                      double** B,

                      double** AtBA)

//Description================================

//  matrix multiplication

//  AtBA = transpose(A)*B*A

//IN   :double** A(n_row, n_col)

//     :double** B(n_row, n_row)

//OUT  :double** AtBA(n_col, n_col)

//===========================================

{



   const double tolerance = 10.E-15;

   double d_temp=0.0;



   //double AtB[n_col][n_row] ;

   double** AtB = New_dMatrix(n_col,n_row) ;



   for(int i=0; i<n_col; i++) //initialize

   {

      for(int j=0; j<n_row; j++)

      {

         AtB[i][j] = 0.0 ;

      }

   }



   //AtB

   for(int ic=0; ic<n_row; ic++)

   {

      for(int ir=0; ir<n_col; ir++)

      {

         d_temp=0.0;



         for(int i=0; i<n_row; i++)

         {

            d_temp = d_temp + A[i][ir]*B[i][ic] ;

         }



         if(fabs_FHDI(d_temp) < tolerance ) d_temp =0.0 ; //delete numerical error

         AtB[ir][ic] = d_temp ;

      }

   }



   //AtBA

   d_temp=0.0;



   for(int ic=0; ic<n_col; ic++)

   {

      for(int ir=0; ir<n_col; ir++)

      {

         d_temp=0.0;



         for(int i=0; i<n_row; i++)

         {

            d_temp = d_temp + AtB[ir][i]*A[i][ic] ;

         }

         if(fabs_FHDI(d_temp) < tolerance ) d_temp =0.0 ; //delete numerical error



         AtBA[ir][ic] = d_temp ;

      }

   }

   

   Del_dMatrix(AtB, n_col,n_row) ;



   return;



}



//==============================================================================

//==============================================================================

void dMatrix_dVector_Mul_Av(double** A, int n_row, int n_col,

                            double   v[],

                            double  Av[])

//Description================================

//  matrix & Vector multiplication

//  Av = A*v

//IN   :double** A(n_row, n_col)

//     :double   v(n_col)

//OUT  :double   Av(n_row)

//===========================================

{

   const double tolerance = 10.E-15;

   double d_temp=0.0;



   for(int i_r=0; i_r<n_row; i_r++)

   {

      d_temp =0.0;

      for(int i_c=0; i_c<n_col; i_c++)

      {

         d_temp = d_temp + A[i_r][i_c]*v[i_c] ;

      }

      if(fabs_FHDI(d_temp) < tolerance) d_temp =0.0;



      Av[i_r] = d_temp ;

   }

   return;

}



//==============================================================================

//==============================================================================

void dMatrix_dVector_Mul_Atv(double** A, int n_row, int n_col,

                            double   v[],

                            double  Atv[])

//Description================================

//  Transpose(matrix) & Vector multiplication

//  Atv = Transpose(A)*v

//IN   :double** A(n_row, n_col)

//     :double   v(n_row)

//OUT  :double   Atv(n_col)

//===========================================

{

   const double tolerance = 10.E-15;

   double d_temp=0.0;



   for(int i_c=0; i_c<n_col; i_c++)

   {

      d_temp =0.0;

      for(int i_r=0; i_r<n_row; i_r++)

      {

         d_temp = d_temp + A[i_r][i_c]*v[i_r] ;

      }

      if(fabs_FHDI(d_temp) < tolerance) d_temp =0.0;



      Atv[i_c] = d_temp ;

   }

   return;

}



//==============================================================================

//==============================================================================



double dMaxValue(double** d_matrix, int n_row, int n_col, char s_where,

              int n_begin, int n_end, int n_at)

//Description=================

//

// find maximum double value in the double matrix

// (1) s_where ="row" ; search through row(within n_begin~n_end) at n_at col

// (2) s_where ="col" ; search through col(within n_begin~n_end) at n_at row

// (3) s_where ="all" ; search all matrix

//

//IN   : double** d_matrix[n_row][n_col]

//OUT  : double maximum value

//============================



{

   double d_maximum =0.0;

   double d_temp=0.0;



   if(s_where =='r')

   {

      for(int i=n_begin;i<=n_end; i++)

      {

         if(d_temp < d_matrix[i][n_at]) d_temp = d_matrix[i][n_at] ;

      }

   }

   else if(s_where =='c')

   {

      for(int i=n_begin;i<=n_end; i++)

      {

         if(d_temp < d_matrix[n_at][i]) d_temp = d_matrix[n_at][i] ;

      }

   }

   else if(s_where =='a')

   {

      for(int i=0;i<n_row; i++)

      {

         for(int j=0; j<n_col; j++)

         {

            if(d_temp < d_matrix[i][j]) d_temp = d_matrix[i][j] ;

         }

      }



   }



   d_maximum = d_temp;

   return d_maximum;

}



//==============================================================================

//==============================================================================



void Compare_Two_dMatrix(double** A, double** B, int n_row, int n_col)

{

   for(int i=0; i<n_row; i++)

   {

      for(int j=0; j<n_col; j++)

      {

         if(fabs_FHDI(A[i][j] - B[i][j]) != 0.0)

         {


         }

      }

   }

   //system("PAUSE") ;

}







//=============================================================================

//=============================================================================

void c1A_p_c2B(const double c1, double** A, const int n_row, const int n_col,

               const double c2, double** B,

               double ** M)

//Description=========================================

//  perform

//     M = c1*[A] + c2*[B]

//====================================================

{

   for(int i=0; i<n_row; i++)

   {

      for(int j=0; j<n_col; j++)

      {

         M[i][j] = c1*A[i][j] + c2*B[i][j] ;

      }

   }

}



//=============================================================================

//=============================================================================

double my_dot(const int n, const double* u, const double * v)

//Description===============

//  inner product of two vectors 

//  double = {u}.{v}

//==========================

{

   double d=0.0;



   for(int i=0; i<n; i++)

      d += u[i]*v[i];

   return d;

}





bool Inverse_dMatrix_FHDI(double** d_Mat, const int n, double** d_Inv)

//Description===================

//return inverse matrix of the n*n matrix

//using Gauss-Jordan elimination

//

// if diagonal term is zero and too small

// perform pivoting with largest value on the columns

//

// Note: for FHDI, n = 1 and n =2 cases are separately handled. 

//

//IN    : double** d_Mat[n][n]

//OUT   : double** d_Inv[n][n]

//OUT   : bool b_success = 0 when abrupt exit due to zero digonal term 

//==============================

{

   bool b_success = true; 

   const double eps=1.e-15 ;



   //------------------

   //if n = 1

   //------------------

    if(fabs_FHDI(d_Mat[0][0]) > eps) 

    {

	   d_Inv[0][0] = 1.0/d_Mat[0][0];

	   return b_success; 

	}

    if(fabs_FHDI(d_Mat[0][0]) <= eps) 

    {

	   d_Inv[0][0] = 1.0;

	   b_success = false; 

	   return b_success; 

	}	



   //------------------

   //if n = 2

   //------------------

   const double det2 = d_Mat[0][0]*d_Mat[1][1] - d_Mat[0][1]*d_Mat[1][0];

    if(fabs_FHDI(det2) > eps) 

    {

	   d_Inv[0][0] = d_Mat[1][1]/det2;

	   d_Inv[0][1] = -1.0*d_Mat[0][1]/det2;

	   d_Inv[1][0] = -1.0*d_Mat[1][0]/det2;

	   d_Inv[1][1] = d_Mat[0][0]/det2;

	   

	   return b_success; 

	}

    if(fabs_FHDI(det2) <= eps) 

    {

	   Fill_dMatrix(d_Inv, 2, 2, 1.0);

	   b_success = false; 

	   return b_success; 

	}	

	

	//------------------

	// below is for n > 2

	//------------------

   //make d_Inv unity matrix

   for(int i=0; i<n; i++)

   {

      for(int j=0; j<n; j++) d_Inv[i][j] =0.0;



      d_Inv[i][i]=1.0 ;

   }







   double c=0.0 ; //coeff.



   for(int i_diag=0; i_diag<n; i_diag++)

   {

      c = d_Mat[i_diag][i_diag];



      //================

      //when diagonal term is too small or zero, then needs pivoting

      //================

      if(fabs_FHDI(c)<eps )

      {

         //find max on current column ======

         double d_temp= c;  int i_loc=i_diag;



         for(int i=(n-1); i>i_diag; i--)//from the nth ~ (i_diag+1)

         {

            if(fabs_FHDI(d_temp)< fabs_FHDI(d_Mat[i][i_diag]) )

            {

               i_loc = i ;

               d_temp = d_Mat[i][i_diag] ;

            }

         }



         //When Pivot is necessary!

         if(i_loc != i_diag)

         {

            for(int i=0; i<n; i++)

            {

               d_temp    = d_Mat[i_diag][i] ; //store temporarily

               d_Mat[i_diag][i] = d_Mat[i_loc][i] ;//exchange with the max.

               d_Mat[i_loc][i]  = d_temp ;



               d_temp    = d_Inv[i_diag][i] ; //store temporarily

               d_Inv[i_diag][i] = d_Inv[i_loc][i] ;//exchange with the max.

               d_Inv[i_loc][i]  = d_temp ;

            }

         }

         else if(i_loc == i_diag) //can't find max value than current diagonal term

         {

			//----

			//below condition is added for FHDI

			//----

			if(fabs_FHDI(c) < eps)

			{ 

				Rprintf("Error! no pivoting is possible with current mat. in invers matrix");

				b_success = false; 

				return b_success;

			}

			if(fabs_FHDI(c) >= eps)

			{

				//keep going with current Non-zero diagonal value 

			}

			

         }

      }





      c = d_Mat[i_diag][i_diag];  //get original or exchanged one





      //make current diag. term 1.0

      for(int i=0; i<n; i++)

      {

         d_Mat[i_diag][i]= d_Mat[i_diag][i] /c; //divide [i_diag]th row terms with the diagonal term

         d_Inv[i_diag][i]= d_Inv[i_diag][i] /c;

      }



      //Lower term elimination================

      //eliminate below terms of [i_diag]th col

      if(i_diag == n-1) continue; // don't need below for the last diagonal



      for(int j=i_diag+1; j<n; j++)

      {

         c=d_Mat[j][i_diag] ; //jth row first term



         for(int i=0; i<n; i++)   //all jth row terms

         {

            d_Mat[j][i] = d_Mat[j][i] -c* d_Mat[i_diag][i];

            d_Inv[j][i] = d_Inv[j][i] -c* d_Inv[i_diag][i];

         }

      }

   }



   //Upper term elimination================

   //subtract upper terms of [i_diag]th col

   for(int i_diag=1; i_diag<n; i_diag++) //note: begin from the second row

   {

      for(int j=0; j<i_diag; j++)

      {

         c=d_Mat[j][i_diag] ; //jth row first term



         for(int i=0; i<n; i++)   //all jth row terms

         {

            d_Mat[j][i] = d_Mat[j][i] -c* d_Mat[i_diag][i];

            d_Inv[j][i] = d_Inv[j][i] -c* d_Inv[i_diag][i];

         }

      }

   }



   return b_success; 

}





//Fn===========================================================================

//base_FHDI.h-------------------------------------------------------

//Fn===========================================================================

namespace FHDI

{



//------------------------

// Definitions of local base functions

// for FHDI

//------------------------
bool isnan_FHDI(double x) { return x!=x; } //added to avoid error regarding std::isnan


//-------------------------

//make a table with a given array

// like R's table function

//-------------------------

void table_cpp( std::string cn[], const int nrow, 

		std::vector<std::string> &v_table_row1, std::vector<int> &v_table_row2);

void table_cpp( double* d_source, const int nrow, 

		        std::vector<double> &v_table_row1, std::vector<int> &v_table_row2);

void table_cpp_int(int* d_source, const int nrow,

	std::vector<int> &v_table_row1, std::vector<int> &v_table_row2); // Written by Yicheng Yang

//-------------------------

//make a condensed string expression with a given array

//-------------------------

void Trans(double** z, const int nrow, const int ncol, std::string cn[]); //many rows case

void Trans1(double* z, const int n, std::string &cn); //one row case



//-------------------------

//local function for "which" of R 

//-------------------------

// return ACTUAL location having the same integer as i_target 

//-------------------------

void which(int* i_vector, const int n, const int i_target, std::vector<int> &v_location);

void which(double* d_vector, const int n, const double d_target, std::vector<int> &v_location);

void which(std::vector<std::string> s_vector, const  std::string s_target, 

           std::vector<int> &v_location);

void which(std::string s_array[], const int n, const  std::string s_target, 

           std::vector<int> &v_location);

void whichINV(int* i_vector, const int n, const int i_target, 

           std::vector<int> &v_location);

void whichINV(double* d_vector, const int n, const double d_target, 

           std::vector<int> &v_location);

void whichINVNOT(int* i_vector, const int n, const int i_target,

	       std::vector<int> &v_location); // Written by Yicheng Yang

//------------------------------- 

// Rprint: print out double matrix on output file 

//-------------------------------

void RPrint(double** d_debug, const int nrow, const int ncol, std::ofstream &Testout); 



//------------------------------- 

// Rprint: print out double matrix on R console

//-------------------------------

void RPrint(double** d_debug, const int nrow, const int ncol); 



//------------------------------- 

// Rprint: print out double vector on R console

//-------------------------------

void RPrint(double* d_debug, const int n) ;



//------------------------------- 

// Rprint: print out integer vector on R console

//-------------------------------

void RPrint(int* i_debug, const int n) ;

//------------------------------- 

// Rprint: print out integer matrix on R console

//-------------------------------

void RPrint(int*& i_debug, const int nrow, const int ncol) ;


//------------------------------- 

// Rprint: print out vector of integer on R console

//-------------------------------

void RPrint(std::vector<int> i_debug) ;



//------------------------------- 

// Rprint: print out vector of double on R console

//-------------------------------

void RPrint(std::vector<double> d_debug) ;



//------------------------------- 

// Rprint: print out string array on R console

//-------------------------------

void RPrint(std::string s_debug[], const int n) ;



//------------------------------- 

// Rprint: print out string vector on R console

//-------------------------------

void RPrint(std::vector<std::string> v_sdebug) ;



//------------------------------- 

// Rprint: print one integer on R console

//-------------------------------

void RPrint(const int i_target) ;



//------------------------------- 

// Rprint: print one double on R console

//-------------------------------

void RPrint(const double d_target) ;



//------------------------------- 

// Rprint: print out string on R console

//-------------------------------

void RPrint(const char *vString) ;



//-------------------------------

//basic tools for vector, array

//-------------------------------

int sum_FHDI(std::vector<int> i_source);

int sum_FHDI(int* i_source, const int n_size);

int max_FHDI(std::vector<int> i_source);

int min_FHDI(std::vector<int> i_source);

double max_FHDI(std::vector<double> i_source);// Written by Yicheng Yang

double max_FHDI(double* k, const int n);

double min_FHDI(double* k, const int n);

double second_min_FHDI(double arr[], int n);// Written by Yicheng Yang


int max_FHDI(int* k, const int n);

int min_FHDI(int* k, const int n);



//--------------------------------

//calculate absolute distance^2 between Matrix's row entities and a double 

//--------------------------------

void distance2(double** d_mat, const int nrow, const int ncol, const double d_origin, 

               double* d_distance);

			   

//-------------------

//return order of POSITIVE array in ascending magnitude 

//-------------------

void order_FHDI(int* i_original, const int n);	

void order_FHDI(double* d_original, const int n, int* i_return); 		   

void order_FHDI(double* d_original_0, const int n, std::vector<int> &i_return); // Written by Yicheng Yang

//-------------------

// calculate joint probability using the given weights

//-------------------

void wpct_FHDI(std::string s_0[], const int n, const double* w, 

               std::vector<std::string> &jp_name, std::vector<double> &jp_prob);

			   

//---------------------

//  calculate covariance of matrix in a column-to-column manner

//---------------------

void cov_FHDI(double** x, const int nrow, const int ncol, double** cov);

//---------------------

//  calculate correlation matrix in a column-to-column manner. Written by Yicheng Yang

//---------------------

void correlation_FHDI(double** x, const int nrow, const int ncol, double** cov);





//---------------------

//return the first matches

//---------------------

void match_FHDI(std::string cn[], const int nrow, std::string cn_large[], const int nrow_large, 

		       std::vector<int> &v_match); 

void match_FHDI(std::string cn[], const int nrow, 

                std::vector<std::string> v_cn_large, 

		        std::vector<int> &v_match);			   

void match_FHDI(std::vector<int> v_cn, std::vector<int> v_cn_large,  

		       std::vector<int> &v_match);

void match_FHDI(int* i_cn[], const int nrow, int* i_cn_large[], const int nrow_large, 

		       std::vector<int> &v_match);

void match_FHDI(double* d_cn[], const int nrow, double* d_cn_large[], const int nrow_large, 

		       std::vector<int> &v_match);			   

//------------------

//cumulative sum of array

//------------------

void cumsum_FHDI(double* d_original, const int n, double* d_return);


//------------------

//Make correlation matrix and ranking matrix

//------------------

void Ranking_m(const int nrow, const int ncol, double** x_raw, int** r_raw, double ** correlation_yicheng, int** correlation_ranking); // Written by Yicheng Yang
		   
void Ranking_top(const int nrow_ol, const int ncol, const int top, double** ol_matrix, int** correlation_ranking_top); // Written by Yicheng Yang

//------------------

//Select all variables whose votes reaching i_option_collapsing for SIS with intersection

//------------------
void max_occur(std::vector<int> v_table_name, std::vector<int> v_table_counts, int ncol, int size_i, std::vector<int> v_lm, int v_lm_size, int b1,
	const int i_option_collapsing, std::vector<int> &v_mxl, double** correlation_yicheng, int** correlation_temp2); // Written by Yicheng Yang

void max_occur2(std::vector<int> v_table_name, std::vector<int> v_table_counts, int ncol, int size_i, std::vector<int> v_lm, int v_lm_size,
	const int i_option_collapsing, const int top, int nrow_ol, std::vector<int> &v_mxl, double** ol_matrix, int** correlation_temp2); // Written by Yicheng Yang

//------------------

//Select all variables whose votes reaching i_option_collapsing for SIS with union

//------------------
void max_occur_union(std::vector<int> v_table_name, std::vector<int> v_table_counts, int ncol, int size_i, std::vector<int> v_lm, int v_lm_size, int b1,
	const int i_option_collapsing, std::vector<int> &v_mxl, double** correlation_yicheng, int** correlation_temp2); // Written by Yicheng Yang

void max_occur_union2(std::vector<int> v_table_name, std::vector<int> v_table_counts, int ncol, int size_i, std::vector<int> v_lm, int v_lm_size,
	const int i_option_collapsing, const int top, int nrow_ol, std::vector<int> &v_mxl, double** ol_matrix, int** correlation_temp2); // Written by Yicheng Yang

//------------------

//Select the most i_option_collapsing correlated variables from all observed variables of each mox with intersection

//------------------
void correlated_variable_intersection(const int ncol, const int i_option_collapsing, int i, int* ia_temp,
	double **correlation_yicheng, int** correlation_ranking, std::vector<int> &v_mxl); // Written by Yicheng Yang

void correlated_variable_intersection2(const int ncol, const int i_option_collapsing, const int top, int i, int nrow_ol, int* ia_temp,
	double** ol_matrix, int** correlation_ranking_top, std::vector<int> &v_mxl); // Written by Yicheng Yang

//------------------

//Select the most i_option_collapsing correlated variables from all observed variables of each mox with union

//------------------
void correlated_variable_union(const int ncol, const int i_option_collapsing, int i, int* ia_temp,
	double **correlation_yicheng, int** correlation_ranking, std::vector<int> &v_mxl); // Written by Yicheng Yang

void correlated_variable_union2(const int ncol, const int i_option_collapsing, const int top, int i, int nrow_ol, int* ia_temp,
	double** ol_matrix, int** correlation_ranking_top, std::vector<int> &v_mxl); // Written by Yicheng Yang
//------------------

//Select the most i_option_collapsing correlated variables from all observed variables of each mox with global ranking

//------------------
void correlated_variable_global(const int ncol, const int i_option_collapsing, int* ia_temp,
	double **correlation_yicheng, std::vector<int> &v_mxl);// Written by Yicheng Yang

void correlated_variable_global2(const int ncol, const int i_option_collapsing, int nrow_ol, int* ia_temp,
	double** ol_matrix, std::vector<int> &v_mxl); // Wtitten by Yicheng Yang

} //end of namespace





//Fn===========================================================================

//base_FHDI.cc-------------------------------------------------------

//Fn===========================================================================

namespace FHDI

{



//------------------------

// Definitions of local base functions

// for FHDI

//------------------------



void table_cpp( std::string cn[], const int nrow, 

		std::vector<std::string> &v_table_row1, std::vector<int> &v_table_row2)

//Description=========================================

// make a table of given STRING vector 

//

// Algorithm: count unique items in the given cn[]  

// 

//

// c++ code: 		Dr. Cho, I. 

// All rights reserved

// 

// updated: Oct 6, 2016

//----------------------------------------------------

//IN 	: string cn(nrow)		= vector of string to represent each row of z          

//OUT   : std::vector<string> v_table_row1= 1st row: unique item in string format

//OUT   : std::vector<int> v_table_row2 = 2nd row: count of the unique item  

//====================================================

{

    std::string s_temp;

    //std::string cn_temp[nrow]; 

	std::string * cn_temp = new std::string[nrow]; 

    for(int i=0; i<nrow; i++) { cn_temp[i] = cn[i]; } //make a copy of original cn[]

    

	//-----------

	//internal sorting of the cn_temp[]

	//just like "table" of R

	//-----------

	std::sort(cn_temp, cn_temp+nrow);

	

	

    const std::string s_null = ""; //empty string 

    int i_temp = 0; 

    for(int i=0; i<nrow; i++)

    {

		i_temp = 0; //re-initialize

		s_temp = cn_temp[i]; 

		//-----

		//search s_temp

		//-----

		if(s_temp.compare(s_null) !=0 ) //NOT an empty cell 

		{

			for(int j=i; j<nrow; j++) //count item including myself

			{

				if(s_temp.compare(cn_temp[j]) == 0) //0: equal string

				{

					i_temp++;    //count the same string in cn 

					if(j>i) cn_temp[j] = s_null; //delete the same string just found 

				}

			}

			//store the found unique string and its count 

			if(i_temp > 0) //there is at least one unique item

			{

				v_table_row1.push_back(s_temp); 

				v_table_row2.push_back(i_temp); //actual total number of the unique string 

			}	

		}	

    }

    

	

	delete[] cn_temp;	

    

	return;

}



void table_cpp( double* d_source, const int nrow, 

		        std::vector<double> &v_table_row1, std::vector<int> &v_table_row2)

//Description=========================================

// make a table of given DOUBLE array 

//

// Algorithm: count unique items in the given d_source[]  

// 

//

// c++ code: 		Dr. Cho, I. 

// All rights reserved

// 

// updated: Oct 6, 2016

//----------------------------------------------------

//IN 	: double d_source(nrow)		= double array           

//OUT   : std::vector<double> v_table_row1= 1st row: unique item in double format

//OUT   : std::vector<int>    v_table_row2 = 2nd row: count of the unique item  

//====================================================

{

    double d_temp;

    double* d_source_temp = new double[nrow]; 

    for(int i=0; i<nrow; i++) { d_source_temp[i] = d_source[i]; } //make a copy of original array



	//-----------

	//internal sorting of the cn_temp[]

	//just like "table" of R

	//-----------

	std::sort(d_source_temp, d_source_temp+nrow);



    

    int i_temp = 0; 

	

    for(int i=0; i<nrow; i++)

    {

		i_temp = 0; //re-initialize

		d_temp = d_source_temp[i]; 

		//if(std::isnan(d_temp) !=1) //only meaningful value
		if(isnan_FHDI(d_temp) != 1) //only meaningful value
		{

			//-----

			//search d_temp

			//-----

			for(int j=i; j<nrow; j++) //count item including myself

			{

				if(fabs_FHDI(d_temp - d_source_temp[j])<1e-15) //~0: equal value

				{

					i_temp++;    //count the same double in d_soure 

					if(j>i) d_source_temp[j] = nan(""); //delete the same double just found 

				}

			}

			//store the found unique string and its count 

			if(i_temp > 0) //there is at least one unique item

			{

				v_table_row1.push_back(d_temp); 

				v_table_row2.push_back(i_temp); //actual total number of the unique double 

			}

		}		

		

    }

    	

	delete[] d_source_temp;	

    return;

}


void table_cpp_int(int* d_source, int nrow,

	std::vector<int> &v_table_row1, std::vector<int> &v_table_row2)
	//Description=========================================

	// note this table function is only used in correlated_variables function

	// make a table of given integer array 

	//
	// Algorithm: count unique items in the given d_source[]  

	//

	//

	// c++ code: 		Yicheng Yang.

	// All rights reserved

	//

	// updated: Feb 19, 2020

	//----------------------------------------------------

	//IN 	: integer d_source(nrow)		= integer array        

	//OUT   : std::vector<int> v_table_row1= 1st row: unique item in integer format

	//OUT   : std::vector<int>    v_table_row2 = 2nd row: count of the unique item  
	//====================================================
{

	int d_temp;

	int* d_source_temp = new int[nrow];

	for (int i = 0; i<nrow; i++) { d_source_temp[i] = d_source[i]; } //make a copy of original array

																	 //-----------

																	 //internal sorting of the cn_temp[]

																	 //just like "table" of R

																	 //-----------

	std::sort(d_source_temp, d_source_temp + nrow);


	int i_temp = 0;

	for (int i = 0; i < nrow; i++)
	{
		i_temp = 0; //re-initialize

		d_temp = d_source_temp[i];

		//-----

		//search d_temp

		//-----
		if (d_temp != 1234567890) {

			for (int j = i; j < nrow; j++) //count item including myself
			{

				if (fabs(d_temp - d_source_temp[j]) < 1e-15) //~0: equal value

				{
					i_temp++;    //count the same double in d_soure 

					if (j > i) d_source_temp[j] = 1234567890; //delete the same double just found 

				}

			}

			//store the found unique string and its count 
			if (i_temp > 0) //there is at least one unique item

			{

				v_table_row1.push_back(d_temp);

				v_table_row2.push_back(i_temp); //actual total number of the unique double 

			}

		}

	}

	delete[] d_source_temp;

	return;
}

	
void Trans(double** z, const int nrow, const int ncol, std::string cn[])
//Description=========================================
// make a condensed expression of z
//
// Algorithm:  each row of z will be concatenated as a single string consisting of 35 characters
// 
// Note: as of Oct 2016, NA values (missing data) is marked by a long integer at the parent "r" code
// Note: as of Apr 2017, the use of combination of char and string appears to cause error
//                       in Ubuntu platform
//                       Hence, a uniform use of string is recommended as below
// original R code: Dr. Im, J. and Dr. Kim, J. 
// c++ code: 		Dr. Cho, I. 
// All rights reserved
// 
// updated: April 4, 2017
//----------------------------------------------------
//IN   	: double z(nrow, ncol)  = categorized matrix corresponding to original matrix x
//OUT	: string cn(nrow)		= vector of string to represent each row of z                                
//====================================================
{
	const std::string ch_db[35] = {"1", "2", "3", "4", "5", "6", "7", "8", "9",
							"a", "b", "c", "d", "e", "f", "g", "h", "i",
							"j", "k", "l", "m", "n", "o", "p", "q", "r",
							"s", "t", "u", "v", "w", "x", "y", "z"};
	std::string ch_temp; 
	int  i_temp=0; 
     
	for(int i_row=0; i_row<nrow; i_row++)
	{	
		std::string	s_all;
		
		for(int i_col = 0; i_col<ncol; i_col++)
		{
			i_temp = (int)z[i_row][i_col];
			
			ch_temp = "0"; //default character is zero
			if(i_temp>=1 && i_temp<=35)
			{
				ch_temp = ch_db[i_temp-1]; 
			}
			s_all.append(ch_temp);
		}
		
		//---------
		//store the condensed string
		//---------
		cn[i_row] = s_all; 
	}
	
	return;
	
	
}

void Trans1(double* z, const int n, std::string &cn)
//Description=========================================
// make a condensed expression of a double array, z
//
// Algorithm:  z will be concatenated as a single string consisting of 35 characters
//
// Note: as of Apr 2017, the use of combination of char and string appears to cause error
//                       in Ubuntu platform
//                       Hence, a uniform use of string is recommended as below
// 
// original R code: Dr. Im, J. and Dr. Kim, J. 
// c++ code: 		Dr. Cho, I. 
// All rights reserved
// 
// updated: April 4, 2017
//----------------------------------------------------
//IN   	: double z(n)  =  categorized array corresponding to a row of original matrix x
//OUT	: string cn		  =  a string to represent the given row of z                                
//====================================================
{
	const std::string ch_db[35] = {"1", "2", "3", "4", "5", "6", "7", "8", "9",
							"a", "b", "c", "d", "e", "f", "g", "h", "i",
							"j", "k", "l", "m", "n", "o", "p", "q", "r",
							"s", "t", "u", "v", "w", "x", "y", "z"};
	std::string ch_temp; 
	int  i_temp=0; 
    std::string	s_all; 
	
	for(int i = 0; i<n; i++)
	{
		i_temp = (int)z[i];
			
		ch_temp = "0"; //default character is zero
		if(i_temp>=1 && i_temp<=35)
		{
			ch_temp = ch_db[i_temp-1]; 
		}
		s_all.append(ch_temp);
	}
		
	//---------
	//store the condensed string
	//---------
	cn = s_all; 
	
	return;
}






//-------------------------

//local function for "which" of R 

//-------------------------

// return ACTUAL location having the same integer as i_target 

//-------------------------

void which(int* i_vector, const int n, const int i_target, std::vector<int> &v_location)

{

	if(n<=0) {Rprintf("Error! n<=0! in which()");  return;}

	for(int i=0; i<n; i++)

	{

		if(i_vector[i] == i_target) v_location.push_back(i+1); //actual location 

	}

	return; 

}



//-------------------------

// return ACTUAL location having the same double as d_target 

//-------------------------

void which(double* d_vector, const int n, const double d_target, std::vector<int> &v_location)

{

	if(n<=0) {Rprintf("Error! n<=0! in which()");  return;}

	for(int i=0; i<n; i++)

	{

		if(fabs_FHDI(d_vector[i]-d_target)<1e-15) v_location.push_back(i+1); //actual location 

	}

	

	return; 

}





//-------------------------

// return ACTUAL location having the same STRING as s_target 

//-------------------------

void which(std::vector<std::string> s_vector, const  std::string s_target, 

           std::vector<int> &v_location)

{

	const int n = s_vector.size();

	if(n<=0) {Rprintf("Error! n<=0! in which s_vector()");  return;}

	for(int i=0; i<n; i++)

	{

		if(s_vector[i].compare(s_target) == 0) //0: equal string

		{	v_location.push_back(i+1); } //actual location 

	}

	return; 

}



//-------------------------

// return ACTUAL location having the same STRING in an ARRAY as s_target 

//-------------------------

void which(std::string s_array[], const int n, const  std::string s_target, 

           std::vector<int> &v_location)

{

	if(n<=0) {Rprintf("Error! n<=0! in which s_array()"); return;}

	for(int i=0; i<n; i++)

	{

		if(s_array[i].compare(s_target) == 0) //0: equal string

		{	v_location.push_back(i+1); } //actual location 

	}

	return; 

}



//-------------------------

// return ACTUAL location having the DIFFERENT integer from i_target 

//-------------------------

void whichINV(int* i_vector, const int n, const int i_target, std::vector<int> &v_location)

{

	if(n<=0) {Rprintf("Error! n<=0! in which()");  return;}

	for(int i=0; i<n; i++)

	{

		if(i_vector[i] != i_target) v_location.push_back(i+1); //actual location 

	}

	return; 

}

//-------------------------

// return ACTUAL location having the DIFFERENT double from d_target 

//-------------------------

void whichINV(double* d_vector, const int n, const double d_target, std::vector<int> &v_location)

{

	if(n<=0) {Rprintf("Error! n<=0! in which()");  return;}

	for(int i=0; i<n; i++)

	{

		if(fabs_FHDI(d_vector[i] - d_target) > 1e-15) v_location.push_back(i+1); //actual location 

	}

	return; 

}

//-------------------------

// return ACTUAL location having the same integer from i_target 

// c++ code: Yicheng Yang

// updated: Feb 19,2020
//-------------------------

void whichINVNOT(int* i_vector, const int n, const int i_target, std::vector<int> &v_location)

{

	if (n <= 0) { Rprintf("Error! n<=0! in whichnot()");  return; }

	for (int i = 0; i < n; i++)

	{

		if (i_vector[i] == i_target) v_location.push_back(i + 1); //actual location 

	}

	return;
}


//------------------------------- 

// Rprint: print out double matrix on output file 

//-------------------------------

void RPrint(double** d_debug, const int nrow, const int ncol, std::ofstream &TestOut) 

{

	if(nrow<=0 || ncol<=0) 

	{Rprintf("Error! nrow or ncol<=0! in printing d_debug[][]");}



	for (int i=0; i<nrow; i++) 

	{ 

		for (int j=0; j<ncol; j++) 

		{ 

			Rprintf("%g ",d_debug[i][j] );

		} 
		
		Rprintf(" - nextRow - \n"); 

		 

	} 

	return; 

}





//------------------------------- 

// Rprint: print out double matrix on R console

//-------------------------------

void RPrint(double** d_debug, const int nrow, const int ncol) 

{

	if(nrow<=0 || ncol<=0) {Rprintf("Error! nrow or ncol<=0! in printing d_debug[][]");  return;}

	

	for (int i=0; i<nrow; i++) 

	{ 

		for (int j=0; j<ncol; j++) 

		{ 

			Rprintf("%g ",d_debug[i][j]); Rprintf("      ");

		} 

		Rprintf("- nextRow - \n"); 

	} 

	R_FlushConsole(); 

	R_ProcessEvents(); 

	return; 

}



//------------------------------- 

// Rprint: print out double vector on R console

//-------------------------------

void RPrint(double* d_debug, const int n) 

{

	if(n<=0) {Rprintf("Error! n<=0! in printing d_debug[]");  return;}

	

	for (int i=0; i<n; i++) { Rprintf("%g ",d_debug[i]); Rprintf("      ");} 

	Rprintf(" - nextRow - \n"); 

	R_FlushConsole(); 

	R_ProcessEvents(); 

 

	return; 

}

//------------------------------- 

// Rprint: print out integer vector on R console

//-------------------------------

void RPrint(int* i_debug, const int n) 

{ 

	if(n<=0) {Rprintf("Error! n<=0! in printing i_debug[]");  return;}

	for (int i=0; i<n; i++) { Rprintf("%d ",i_debug[i]); Rprintf("      ");} 

	Rprintf(" - nextRow - \n"); 
	

	R_FlushConsole(); 

	R_ProcessEvents(); 

 

	return; 

}

//------------------------------- 

// Rprint: print out integer matrix on R console

//-------------------------------

void RPrint(int** i_debug, const int nrow, const int ncol) 

{ 

	if(nrow<=0) {Rprintf("Error! nrow<=0! in printing i_debug[][]");  return;}

	for (int i=0; i<nrow; i++) 
	{
		for (int j=0; j<ncol; j++) { Rprintf("%d ",i_debug[i][j]); Rprintf("      ");} 
		Rprintf(" - nextRow - \n"); 
	}

	

	R_FlushConsole(); 

	R_ProcessEvents(); 

 

	return; 

}


//------------------------------- 

// Rprint: print out vector of integer on R console

//-------------------------------

void RPrint(std::vector<int> i_debug) 

{ 	

	const int n = i_debug.size(); 

	if(n<=0) {Rprintf("Error! n<=0! in vector<int>");  return;}

	

	for (int i=0; i<n; i++) { Rprintf("%d ",i_debug[i]); Rprintf("      ");} 

	Rprintf(" - nextRow - \n"); 

	R_FlushConsole(); 

	R_ProcessEvents(); 

 

	return; 

}

//------------------------------- 

// Rprint: print out vector of double on R console

//-------------------------------

void RPrint(std::vector<double> d_debug) 

{ 	

	const int n = d_debug.size(); 

	if(n<=0) {Rprintf("Error! n<=0! in vector<double>");  return;}

	

	for (int i=0; i<n; i++) { Rprintf("%g ",d_debug[i]); Rprintf("      ");} 

	Rprintf(" - nextRow - \n"); 

	R_FlushConsole(); 

	R_ProcessEvents(); 

 

	return; 

}



//------------------------------- 

// Rprint: print out string array on R console

//-------------------------------

void RPrint(std::string s_debug[], const int n) 

{ 	

	if(n<=0) {Rprintf("Error! n<=0! in string[]");return;}

	

	for (int i=0; i<n; i++) 

	{ 

		const char * ch_temp = s_debug[i].c_str();

		Rprintf("%s ", ch_temp); Rprintf("      ");

		//Rprintf("%s ", s_debug[i]); 

		

	} 

	

	R_FlushConsole(); 

	R_ProcessEvents(); 

 

	return; 

}

//------------------------------- 

// Rprint: print out string vector on R console

//-------------------------------

void RPrint(std::vector<std::string> v_sdebug) 

{ 	

	const int n = (int)v_sdebug.size(); 

	if(n<=0) {Rprintf("Error! n<=0! in string[]");  return;}

	

	for (int i=0; i<n; i++) 

	{ 

		const char * ch_temp = v_sdebug[i].c_str();

		Rprintf("%s ", ch_temp); Rprintf("      ");

		//Rprintf("%s ", s_debug[i]); 

		

	} 

	

	R_FlushConsole(); 

	R_ProcessEvents(); 

 

	return; 

}

//------------------------------- 

// Rprint: print one integer on R console

//-------------------------------

void RPrint(const int i_target) 

{ 	

	Rprintf("%d ", i_target);

	

	R_FlushConsole(); 

	R_ProcessEvents(); 

 

	return; 

}



//------------------------------- 

// Rprint: print one double on R console

//-------------------------------

void RPrint(const double d_target) 

{ 	

	Rprintf("%g ", d_target);

	

	R_FlushConsole(); 

	R_ProcessEvents(); 

 

	return; 

}



//------------------------------- 

// Rprint: print out string on R console

//-------------------------------

void RPrint(const char *vString) 

{ Rprintf("%s",vString);  return; } 



//-------------------------------

//basic tools for vector, array

//-------------------------------

int sum_FHDI(std::vector<int> i_source)

{

	int i_sum = 0; 

	int i_n = (int)i_source.size();

	for(int i=0; i<i_n; i++) i_sum += i_source[i];

	

	return i_sum; 

}

int sum_FHDI(int* i_source, const int n_size)

{

	int i_sum = 0; 

	int i_n = n_size;

	for(int i=0; i<i_n; i++) i_sum += i_source[i];

	

	return i_sum; 

}

//---------------------------------

//max value of int vector

//---------------------------------

int max_FHDI(std::vector<int> i_source)

{

	int max = i_source[0]; 

	int i_n = (int)i_source.size();

	for(int i=0; i<i_n; i++) {if(max < i_source[i]) max = i_source[i];}

	

	return max;  

}

//---------------------------------

//min value of int vector

//---------------------------------

int min_FHDI(std::vector<int> i_source)

{

	int min = i_source[0]; 

	int i_n = (int)i_source.size();

	for(int i=0; i<i_n; i++) {if(min>i_source[i]) min = i_source[i];}

	

	return min;  

}

//---------------------------------
//max value of double vector
//
// c++ code: 		Yicheng Yang 
//
// All rights reserved
//
// Updated on Feb 24, 2020
//---------------------------------
double max_FHDI(std::vector<double> i_source)
{
	double max = i_source[0];
	int i_n = (int)i_source.size();
	for (int i = 0; i<i_n; i++) { if (max < i_source[i]) max = i_source[i]; }

	return max;
}

//---------------------------------

//max value of double array

//---------------------------------

double max_FHDI(double* k, const int n)

{

	double max_k = k[0]; 

	for(int i=0; i<n; i++) 

	{

		if(max_k < k[i]) max_k = k[i];

	}

	

	return max_k; 

}

//---------------------------------

//min value of double array

//---------------------------------

double min_FHDI(double* k, const int n)

{

	double min_k = k[0];  

	for(int i=0; i<n; i++) 

	{

		if(min_k > k[i]) min_k = k[i];

	}

	

	return min_k; 

}


//----------------------------------
//second min value of double array written by Yicheng; Note that if arr = {1.1, 2.2, 1.1, 2.3}, it will return 2.2
//---------------------------------
double second_min_FHDI(double arr[], int n) {

	double smallest = 0.0;

	double secondSmallest = 0.0;

	if (arr[0] < arr[1]) {
		smallest = arr[0];
		secondSmallest = arr[1];
	}

	if (arr[0] == arr[1]) {
		smallest = arr[0];
		for (int j = 0; j < n;j++) {
			if (arr[j] != smallest) {
				secondSmallest = arr[j];
			}
		}
	}
	else {
		smallest = arr[1];
		secondSmallest = arr[0];
	}
	for (int i = 0; i < n; i++) {

		if (smallest > arr[i]) {
			secondSmallest = smallest;
			smallest = arr[i];
		}

		else if ((arr[i] < secondSmallest) && (arr[i] > smallest)) {
			secondSmallest = arr[i];
		}
	}
	return secondSmallest;
}



//---------------------------------

//max value of integer array

//---------------------------------

int max_FHDI(int* k, const int n)

{

	int max_k = k[0]; 

	for(int i=0; i<n; i++) 

	{

		if(max_k < k[i]) max_k = k[i];

	}

	

	return max_k; 

}

//---------------------------------

//min value of integer array

//---------------------------------

int min_FHDI(int* k, const int n)

{

	int min_k = k[0]; 

	for(int i=0; i<n; i++) 

	{

		if(min_k > k[i]) min_k = k[i];

	}

	

	return min_k; 

}



//--------------------------------

//calculate absolute distance^2 between Matrix entities and a double 

//--------------------------------

void distance2(double** d_mat, const int nrow, const int ncol, const double d_origin, 

               double* d_distance)

//Description----------------------------------------

//calculate the absolute distance^2 between all entities of the given matrix 

// and a given origin

//IN   : double d_mat(nrow, ncol)   = source matrix with double values

//IN   : double d_origin 		 	= origin 

//OUT  : double d_distance(nrow) = sum(|a - b|^2) per row 

//----------------------------------------------------

{

	Fill_dVector(d_distance, nrow, 0.0);

	double d_sum = 0.0; 

	for(int i=0; i<nrow; i++)

	{

		d_sum = 0.0; //reinitialization

		for(int j=0; j<ncol; j++)

		{

			 d_sum += (d_mat[i][j] - d_origin)*(d_mat[i][j] - d_origin); 

		}

		d_distance[i] = d_sum; 

	}

	return;

}





void order_FHDI(int* i_original, const int n)

//Description ================================

// Order the positive integer array in ascending order

//

//INOUT   : int i_original_0(n) returned with the ordered (Actual) cell numbers     

//          i_original > 0

//=============================================

{

	

	int* i_source = new int[n]; 

	int* i_order  = new int[n]; 

	

	for(int i=0; i<n; i++) 

	{

		i_source[i] = i_original[i]; //backup

		i_order[i] = i+1; //default

	}



	//-----------

	//leverage sorting library

	//-----------

	std::sort(i_source, i_source+n);

	int i_now = 0;

	

	i_order[0] = 1; //first cell location as default

	for(int i=0; i<n; i++)

	{

		i_now = i_source[i];

		//----------------

		//comparisons from the first entiry to now 

		//----------------

		for(int j=0; j<n; j++)

		{

			if(fabs_FHDI(i_now - i_original[j])<1e-3)

			{

				i_order[i] = j+1; //Actual location

				i_original[j] = -1; //dummy value

				break; 

			}				

		}

	}	

	//---prep return

	for(int i=0; i<n; i++) 

	{

		i_original[i] = i_order[i]; //backup

	}

	

	delete[] i_source; 

	delete[] i_order; 

	

	return;

}



void order_FHDI(double* d_original_0, const int n, int* i_return)

//Description ================================

// Order the positive double-precision array in ascending order

//

//IN   : double d_original_0(n) = original array of double-precision float numbers

//              d_original > 0.0 

//OUT  : int i_return(n)    = returned with the ordered (Actual) cell numbers..]   

//

//=============================================

{

	//Note: below backup is different from integer version

	double* d_original = new double[n]; //backup

	Copy_dVector(d_original_0, n, d_original); 

	

	double* d_source = new double[n]; 

	int* i_order  = new int[n]; 

	

	for(int i=0; i<n; i++) 

	{

		d_source[i] = d_original[i]; //backup

		i_order[i] = i+1; //default

	}



	//-----------

	//leverage sorting library

	//-----------

	std::sort(d_source, d_source+n);

	double d_now = 0;

	

	i_order[0] = 1; //first cell location as default

	for(int i=0; i<n; i++)

	{

		d_now = d_source[i];

		//----------------

		//comparisons from the first entiry to now 

		//----------------

		for(int j=0; j<n; j++)

		{

			if(fabs_FHDI(d_now - d_original[j])<1e-15)

			{

				i_order[i] = j+1; //Actual location

				d_original[j] = -1.0; //dummy value

				break; 

			}				

		}

	}	

	//---prep return

	for(int i=0; i<n; i++) 

	{

		i_return[i] = i_order[i]; //backup

	}

	

	delete[] d_original; 

	delete[] d_source; 

	delete[] i_order; 

	

	return;

}


void order_FHDI(double* d_original_0, const int n, std::vector<int> &i_return)
//Description ================================
// Order the positive double-precision array in ascending order
//
//IN   : double d_original_0(n) = original array of double-precision float numbers
//              d_original > 0.0 
//OUT  : int i_return(n)    = returned with the ordered (Actual) cell numbers..]   
//
//=============================================
{
	//Note: below backup is different from integer version
	double* d_original = new double[n]; //backup
	Copy_dVector(d_original_0, n, d_original);

	double* d_source = new double[n];
	int* i_order = new int[n];

	for (int i = 0; i<n; i++)
	{
		d_source[i] = d_original[i]; //backup
		i_order[i] = i + 1; //default
	}

	//-----------
	//leverage sorting library
	//-----------
	std::sort(d_source, d_source + n);
	double d_now = 0;

	i_order[0] = 1; //first cell location as default
	for (int i = 0; i<n; i++)
	{
		d_now = d_source[i];
		//----------------
		//comparisons from the first entiry to now 
		//----------------
		for (int j = 0; j<n; j++)
		{
			if (fabs(d_now - d_original[j])<1e-15)
			{
				i_order[i] = j + 1; //Actual location
				d_original[j] = -1.0; //dummy value
				break;
			}
		}
	}
	//---prep return
	for (int i = 0; i<n; i++)
	{
		i_return.push_back(i_order[i]); //backup
	}

	delete[] d_original;
	delete[] d_source;
	delete[] i_order;

	return;
}
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

void wpct_FHDI(std::string s_0[], const int n, const double* w, 

               std::vector<std::string> &jp_name, std::vector<double> &jp_prob)

//Description=====================================

//  calculate weighted probability of the string array 

//  using the given weight array in w[]

//

//  written by Dr I. Cho

//  All right reserved

//

//  Algorithm: similar to "R" wpct()

//

//IN   : string s_0[n] 	= target array of string

//IN   : double w[n]  	= user-defined weight used for proportional weights

//OUT  : std::vector<std::string> jp_name  = names of joint probability table

//OUT  : std::vector<double>      jp_prob  = weighted joint probability of the table 

//================================================

{

	

	//---------------

	//make a table of s_0[n]

	//---------------

	std::vector<std::string> v_table_row1; //names of the table

	std::vector<int> 		 v_table_row2; //counts of the table

	table_cpp(s_0, n, v_table_row1, v_table_row2);

	const int i_size_v_table = (int)v_table_row2.size();	

	

	//---------------

	//find new accumulated weights for each category

	//---------------

	double* d_weight = new double[i_size_v_table];

	Fill_dVector(d_weight, i_size_v_table, 0.0);

	

	std::string s_temp; 

	int i_count=0; 

	for(int i=0; i<i_size_v_table; i++) //loop for table names 

	{

		s_temp = v_table_row1[i]; 

		i_count = 0; //re-initialize 

		

		//-----------

		//search and get the weight of current string

		//-----------

		for(int j=0; j<n; j++)

		{

			if(s_temp.compare(s_0[j]) == 0) //0 means equal string

			{

				d_weight[i] = d_weight[i] + w[j];  //accumulate the weight of this category

				i_count++; 

				if(i_count == v_table_row2[i]){break;} 

			}

		}

	}	

	

	//-----------------

	//sum of d_weight 

	//-----------------

	double d_sum_w = 0.0; 

	for(int i=0; i<i_size_v_table; i++) d_sum_w += d_weight[i]; 

	if(d_sum_w == 0.0) 
	{Rprintf("Error! zero sum of weights in wpct"); return; }
	

	

	//------------------

	//prep return

	//------------------

	for(int i=0; i<i_size_v_table; i++)

	{

		jp_name.push_back(v_table_row1[i]); 

		jp_prob.push_back(d_weight[i]/d_sum_w); 

	}

		

	//------------------

	//Deallocation

	//------------------

	delete[] d_weight; 



} 

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

void cov_FHDI(double** x, const int nrow, const int ncol, double** cov)

//Description=====================================

//  calculate covariance in a column-to-column manner

//  Note: this generate the "estimated covariance" NOT "population covariance"

//        thus, at the end, 1/(n-1) not 1/n

//  The same as var() of "R"

//

//  written by Dr I. Cho

//  All right reserved

//

//

//IN   : double x[nrow, ncol]  =origianl matrix 

//OUT  : double cov[ncol, ncol]= covariance matrix. cov[0][1] means cov of column 0 and col 1

//================================================

{

	double* x1 = new double[nrow];

	double* x2 = new double[nrow];

	double d_sum = 0.0; 

	Fill_dMatrix(cov, ncol, ncol, 0.0);

	

	//----------

	//get ready two columns

	//----------

	for(int j=0; j<ncol; j++) //from the first column to the second last column

	{

		for(int j_next=j; j_next<ncol; j_next++) //next column including itself 

		{

			for(int i=0; i<nrow; i++) 

			{

				x1[i] = x[i][j];   //jth column

				x2[i] = x[i][j_next] ;//next column

			}

			

			//---

			//each column's mean

			//---

			double x1_mean = 0.0; double x2_mean = 0.0; 

			for(int i=0; i<nrow; i++) 

			{

				x1_mean += x1[i] ;   //jth column

				x2_mean += x2[i] ;   //next column

			}			

			x1_mean = x1_mean/nrow; 

			x2_mean = x2_mean/nrow;



			//-----

			//calculate covariance of two columns

			//-----

			d_sum = 0.0; 

			for(int i_1=0; i_1<nrow; i_1++)

			{

				d_sum += (x1[i_1] - x1_mean)*(x2[i_1] - x2_mean); 

			}

			d_sum = d_sum/(nrow-1); 



			//---------

			//store covariance using symmetry property

			//---------

			cov[j][j_next] = d_sum; 

			cov[j_next][j] = d_sum; 

		}

	}

	

		

	//---------

	//Deallocation

	//---------

	delete[] x1;

	delete[] x2; 

	

	return;

}


void correlation_FHDI(double** x, const int nrow, const int ncol, double** cov)
//Description=====================================
//  calculate covariance in a column-to-column manner
//  Note: this generate the "estimated covariance" NOT "population covariance"
//        thus, at the end, 1/(n-1) not 1/n
//  The same as var() of "R"
//
//  written by Yicheng Yang
//  All right reserved
//
//
//IN   : double x[nrow, ncol]  =origianl matrix 
//OUT  : double cov[ncol, ncol]= correlation matrix. cov[0][1] means correlation of column 0 and col 1
//================================================
{
	double* x1 = new double[nrow];
	double* x2 = new double[nrow];
	double d_sum = 0.0;
	Fill_dMatrix(cov, ncol, ncol, 0.0);

	//----------
	//get ready two columns
	//----------
	for (int j = 0; j<ncol; j++) //from the first column to the second last column
	{
		for (int j_next = j; j_next<ncol; j_next++) //next column including itself 
		{
			for (int i = 0; i<nrow; i++)
			{
				x1[i] = x[i][j];   //jth column
				x2[i] = x[i][j_next];//next column
			}

			//---
			//each column's mean
			//---
			double x1_mean = 0.0; double x2_mean = 0.0;
			for (int i = 0; i<nrow; i++)
			{
				x1_mean += x1[i];   //jth column
				x2_mean += x2[i];   //next column
			}
			x1_mean = x1_mean / nrow;
			x2_mean = x2_mean / nrow;

			//-----
			//calculate covariance of two columns
			//-----
			d_sum = 0.0;
			for (int i_1 = 0; i_1<nrow; i_1++)
			{
				d_sum += (x1[i_1] - x1_mean)*(x2[i_1] - x2_mean);
			}

			//----------------
			//calculate variance of two columns
			//----------------
			double x1_var = 0.0; double x2_var = 0.0;
			double var_sum = 0.0;
			for (int i_2 = 0; i_2 < nrow;i_2++) {
				var_sum = var_sum + (x1[i_2] - x1_mean)*(x1[i_2] - x1_mean);
			}
			x1_var = var_sum;

			var_sum = 0.0;
			for (int i_3 = 0; i_3 < nrow;i_3++) {
				var_sum = var_sum + (x2[i_3] - x2_mean)*(x2[i_3] - x2_mean);
			}
			x2_var = var_sum;
			//---------
			//store covariance using symmetry property
			//---------
			cov[j][j_next] = d_sum / sqrt(x1_var* x2_var);
			cov[j_next][j] = d_sum / sqrt(x1_var* x2_var);
		}

	}


	//---------
	//Deallocation
	//---------
	delete[] x1;
	delete[] x2;

	return;
}


void match_FHDI(std::string cn[], const int nrow, 

                std::string cn_large[], const int nrow_large, 

		        std::vector<int> &v_match)

//Description=========================================

// find a vector of the positions of first matches of cn in cn_large 

//

// Algorithm: the same as "match() in R"  

// 

//

// c++ code: 		Dr. Cho, I. 

// All rights reserved

// 

// updated: Nov 10, 2016

//----------------------------------------------------

//IN 	: string cn(nrow)		        = vector of string 

//IN 	: string cn_large(nrow_large)	= large vector of strings 

//

//OUT   : std::vector<int> v_match = ACTUAL positions of the first matches 

//====================================================

{

    std::string s_temp;

	

    const std::string s_null = ""; //empty string 

    for(int i=0; i<nrow; i++)

    {

		s_temp = cn[i]; 

		//-----

		//search s_temp

		//-----

		if(s_temp.compare(s_null) !=0 ) //NOT an empty cell 

		{

			for(int j=0; j<nrow_large; j++) //find the first match in cn_large

			{

				if(s_temp.compare(cn_large[j]) == 0) //0: equal string

				{

					v_match.push_back(j+1); //+1 for actual location

					break; 

				}

			}

		}	

    }

    	

    return;

}



void match_FHDI(std::string cn[], const int nrow, 

                std::vector<std::string> v_cn_large, 

		        std::vector<int> &v_match)

//Description=========================================

// find a vector of the positions of first matches of cn in cn_large 

//

// Algorithm: the same as "match() in R"  

// 

//

// c++ code: 		Dr. Cho, I. 

// All rights reserved

// 

// updated: Nov 10, 2016

//----------------------------------------------------

//IN 	: string cn(nrow)		        = vector of string 

//IN 	: std::vector<std::string> v_cn_large(nrow_large)	= large vector of strings 

//

//OUT   : std::vector<int> v_match = ACTUAL positions of the first matches 

//====================================================

{

    std::string s_temp, s_temp_large;

	const int nrow_large = (int)v_cn_large.size(); 

	

    const std::string s_null = ""; //empty string 

    for(int i=0; i<nrow; i++)

    {

		s_temp = cn[i]; 

		//-----

		//search s_temp

		//-----

		if(s_temp.compare(s_null) !=0 ) //NOT an empty cell 

		{

			for(int j=0; j<nrow_large; j++) //find the first match in cn_large

			{

				s_temp_large = v_cn_large[j]; 

				if(s_temp.compare(s_temp_large) == 0) //0: equal string

				{

					v_match.push_back(j+1); //+1 for actual location

					break; 

				}

			}

		}	

    }

    	

    return;

}





void match_FHDI(std::vector<int> v_cn, std::vector<int> v_cn_large,  

		       std::vector<int> &v_match)

//Description=========================================

// find a vector of the positions of first matches of cn in cn_large 

//

// Algorithm: the same as "match() in R"  

// 

//

// c++ code: 		Dr. Cho, I. 

// All rights reserved

// 

// updated: Nov 10, 2016

//----------------------------------------------------

//IN 	: std::vector<int> v_cn(nrow)   = vector of integer 

//IN 	: std::vector<int> v_cn_large(nrow_large)	= large vector of integer 

//

//OUT   : std::vector<int> v_match = ACTUAL positions of the first matches 

//====================================================

{

    int i_temp;

	const int nrow 			= (int)v_cn.size();

	const int nrow_large 	= (int)v_cn_large.size();	

	

    for(int i=0; i<nrow; i++)

    {

		i_temp = v_cn[i]; 

		for(int j=0; j<nrow_large; j++) //find the first match in cn_large

		{

			if(i_temp == v_cn_large[j])

			{

				v_match.push_back(j+1); //+1 for actual location

				break; 

			}

		}

    }

    	

    return;

}



void match_FHDI(int* i_cn, const int nrow, int* i_cn_large, const int nrow_large, 

		       std::vector<int> &v_match)

//Description=========================================

// find a vector of the positions of first matches of cn in cn_large 

//

// Algorithm: the same as "match() in R"  

// 

//

// c++ code: 		Dr. Cho, I. 

// All rights reserved

// 

// updated: Nov 10, 2016

//----------------------------------------------------

//IN 	: int cn(nrow)		        = vector of integer

//IN 	: int cn_large(nrow_large)	= large vector of integers 

//

//OUT   : std::vector<int> v_match = ACTUAL positions of the first matches 

//====================================================

{

    int i_temp;

	

    for(int i=0; i<nrow; i++)

    {

		i_temp = i_cn[i]; 

		//-----

		//search i_temp

		//-----

		for(int j=0; j<nrow_large; j++) //find the first match in cn_large

		{

			if(i_temp  == i_cn_large[j]) 

			{

				v_match.push_back(j+1); //+1 for actual location

				break; 

			}

		}

    }

    	

    return;

}



void match_FHDI(double* d_cn, const int nrow, double* d_cn_large, const int nrow_large, 

		       std::vector<int> &v_match)

//Description=========================================

// find a vector of the positions of first matches of cn in cn_large 

//

// Algorithm: the same as "match() in R"  

// 

//

// c++ code: 		Dr. Cho, I. 

// All rights reserved

// 

// updated: Nov 10, 2016

//----------------------------------------------------

//IN 	: double cn(nrow)	        = vector of double

//IN 	: double cn_large(nrow_large)	= large vector of doubles 

//

//OUT   : std::vector<int> v_match = ACTUAL positions of the first matches 

//====================================================

{

    double d_temp;

	

    for(int i=0; i<nrow; i++)

    {

		d_temp = d_cn[i]; 

		//-----

		//search d_temp

		//-----

		for(int j=0; j<nrow_large; j++) //find the first match in cn_large

		{

			if(fabs_FHDI(d_temp - d_cn_large[j])<1e-15) 

			{

				v_match.push_back(j+1); //+1 for actual location

				break; 

			}

		}

    }

    	

    return;

}



void cumsum_FHDI(double* d_original, const int n, double* d_return)

//Description=========================================

// return cumulative sum of the original elements  

//

// Algorithm: the same as "cumsum() in R"  

// 

//

// c++ code: 		Dr. Cho, I. 

// All rights reserved

// 

// updated: Nov 10, 2016

//----------------------------------------------------

//IN 	: double d_original(n)	= original double

//OUT 	: double d_return(n)	= cumlative summation of elements  

//====================================================

{

	double d_sum = 0.0; 

	

	for(int i=0; i<n; i++)

	{

		d_sum += d_original[i]; 

		d_return[i] = d_sum; 

	}

	

	return; 

}




void Ranking_m(const int nrow, const int ncol, double** x_raw, int** r_raw, double ** correlation_yicheng, int** correlation_ranking)

//Description=========================================
//Make correlation matrix correlation_yicheng
//Make ranking matrix of each variable according to correlation matrix in descending order

// c++ code: 		Yicheng Yang 

// All rights reserved

// 

// updated: April 8, 2020

//IN	: double x(nrow, ncol) 	= {y1, y2, ... } total data containing missing values
//IN	: double r(nrow, ncol) 	= {y1, y2, ... } total response inndicators containing missing values
//OUT   : double correlation_yicheng(ncol, ncol);
//OUT   : int correlation_ranking(ncol, ncol-1); // Ranking of correlation of each variable in descending order. 
// Note it excludes itself from ranking
//=====================================================
{
	//----------------
	//Prepare fully observed y matrix
	//---------------------
	std::vector<int> ol;
	int d_temp = 0;
	for (int i_row = 0; i_row < nrow; i_row++)
	{
		d_temp = 1.0;
		for (int i_col = 0; i_col < ncol; i_col++)
		{
			if (r_raw[i_row][i_col] == 0) { d_temp = 0.0; break; } //found zero, i.e. missing cell
		}

		if (fabs(d_temp) > 1e-15) //this row has no missing cells
		{
			ol.push_back(i_row);
		} //actual number of the row having no missing cells
	}
	int nrow_ol = ol.size();

	double** ol_matrix = New_dMatrix(nrow_ol, ncol);

	for (int i = 0;i < nrow_ol;i++) {
		for (int j = 0; j < ncol; j++) {
			ol_matrix[i][j] = x_raw[ol[i]][j];
		}
	}
	//TestOut << "ol_matrix[]" << endl;
	//for (int i = 0; i < nrow_ol; i++)
	//{
	//	for (int j = 0; j < ncol; j++) { TestOut << setw(20) << ol_matrix[i][j]; }
	//	TestOut << endl;
	//}

	//-----------------------
	//Compute corrrlation matrix
	//-----------------------

	//double** correlation_yicheng = New_dMatrix(ncol, ncol);
	correlation_FHDI(ol_matrix, nrow_ol, ncol, correlation_yicheng);
	//TestOut << "correlation matrix: " << endl;
	//for (int i = 0; i < ncol; i++) {
	//	for (int j = 0; j < ncol; j++) {
	//		TestOut << setw(20) << correlation_yicheng[i][j];
	//	}
	//	TestOut << endl;
	//}

	//-----------------------
	//Compute ranking matrix 
	//----------------------

	int** correlation_m_temp = New_iMatrix(ncol, (ncol - 1));

	for (int i = 0; i < ncol; i++) {

		double* d_source_temp = new double[ncol];
		int* i_return = new int[ncol]; //order of score actual loc

		for (int j = 0; j < ncol; j++) {
			d_source_temp[j] = abs(correlation_yicheng[i][j]);// Note the ranking of correlation matrix should be based on absolute value !!!
		}
		order_FHDI(d_source_temp, ncol, i_return);// in ascending order

		// Note i_return_temp must exclude itself priorly in case that i_return have several correlations of 1s. Updated on April 8, 2020 

		std::vector<int> i_return_temp;

		for (int j1 = 0; j1 < ncol; j1++) {

			if (i_return[j1] != (i + 1)) {

				i_return_temp.push_back(i_return[j1]);

			}

		}

		for (int k3 = 0; k3 < (ncol - 1); k3++) {

			correlation_m_temp[i][k3] = i_return_temp[k3];
		}


		delete[] d_source_temp;
		delete[] i_return;
	}

	//TestOut << "correlation_m_temp in ascending order" << endl;
	//for (int kk2 = 0; kk2 < ncol; kk2++) {
	//	for (int kk3 = 0; kk3 < (ncol - 1); kk3++) {
	//		TestOut << setw(20) << correlation_m_temp[kk2][kk3];
	//	}
	//	TestOut << endl;
	//}

	// Reverse the ranking matrix in the descending order
	for (int i = 0; i < ncol; i++) {
		for (int j = 0; j < (ncol - 1); j++) {
			correlation_ranking[i][j] = correlation_m_temp[i][ncol - 2 - j];
		}
	}

	//TestOut << "correlation_ranking in descending order" << endl;
	//for (int kk2 = 0; kk2 < ncol; kk2++) {
	//	for (int kk3 = 0; kk3 < (ncol - 1); kk3++) {
	//		TestOut << setw(20) << correlation_ranking[kk2][kk3];
	//	}
	//	TestOut << endl;
	//}

	Del_dMatrix(ol_matrix, nrow_ol, ncol);
	//Del_dMatrix(correlation_yicheng, ncol, ncol);
	Del_iMatrix(correlation_m_temp, ncol, (ncol - 1));

	return;
}

void Ranking_top(const int nrow_ol, const int ncol, const int top, double** ol_matrix, int** correlation_ranking_top)

//Description=========================================
//Make correlation ranking matrix correlation_ranking
//Make ranking matrix of each variable according to correlation matrix in descending order

//IN	: double x(nrow, ncol) 	= {y1, y2, ... } total data containing missing values
//IN	: double r(nrow, ncol) 	= {y1, y2, ... } total response inndicators containing missing values
//OUT   : double correlation_yicheng(ncol, ncol);
//OUT   : int correlation_ranking(ncol, ncol-1); // Ranking of correlation of each variable in descending order. 
// Note it excludes itself from ranking
//=====================================================
{

	//-----------------------
	//Compute corrrlation matrix
	//-----------------------

	double* x1 = new double[nrow_ol];
	double* x2 = new double[nrow_ol];
	double d_sum = 0.0;
	std::vector<double> cov;

	std::vector<int> i_return_temp;

	for (int j = 0; j<ncol; j++) //from the first column to the second last column
	{
		cov.clear();

		for (int j_next = 0; j_next<ncol; j_next++) //next column including itself 
		{
			for (int i = 0; i<nrow_ol; i++)
			{
				x1[i] = ol_matrix[i][j];   //jth column
				x2[i] = ol_matrix[i][j_next];//next column
			}

			//---
			//each column's mean
			//---
			double x1_mean = 0.0; double x2_mean = 0.0;
			for (int i = 0; i<nrow_ol; i++)
			{
				x1_mean += x1[i];   //jth column
				x2_mean += x2[i];   //next column
			}
			x1_mean = x1_mean / nrow_ol;
			x2_mean = x2_mean / nrow_ol;

			//-----
			//calculate covariance of two columns
			//-----
			d_sum = 0.0;
			for (int i_1 = 0; i_1<nrow_ol; i_1++)
			{
				d_sum += (x1[i_1] - x1_mean)*(x2[i_1] - x2_mean);
			}

			//----------------
			//calculate variance of two columns
			//----------------
			double x1_var = 0.0; double x2_var = 0.0;
			double var_sum = 0.0;
			for (int i_2 = 0; i_2 < nrow_ol;i_2++) {
				var_sum = var_sum + (x1[i_2] - x1_mean)*(x1[i_2] - x1_mean);
			}
			x1_var = var_sum;

			var_sum = 0.0;
			for (int i_3 = 0; i_3 < nrow_ol;i_3++) {
				var_sum = var_sum + (x2[i_3] - x2_mean)*(x2[i_3] - x2_mean);
			}
			x2_var = var_sum;

			//---------
			//store covariance using symmetry property
			//---------
			cov.push_back(d_sum / sqrt(x1_var* x2_var));

			//cov[j][j_next] = d_sum / sqrt(x1_var* x2_var);
			//cov[j_next][j] = d_sum / sqrt(x1_var* x2_var);
		}


		std::vector<int> i_return;
		double* d_source_temp = new double[ncol];

		for (int k = 0; k < ncol; k++) {
			d_source_temp[k] = abs(cov[k]);// Note the ranking of correlation matrix should be based on absolute value !!!
		}

		order_FHDI(d_source_temp, ncol, i_return);

		// Note i_return_temp must exclude itself priorly in case that i_return have several correlations of 1s. Updated on April 8, 2020 

		i_return_temp.clear();

		for (int j1 = 0; j1 < ncol; j1++) {

			if (i_return[j1] != (j + 1)) {

				i_return_temp.push_back(i_return[j1]);

			}

		}


		for (int t = 0; t < top; t++) {
			correlation_ranking_top[j][t] = i_return_temp[ncol - 2 - t]; //exclude the rank of itself
		}


		delete[] d_source_temp;
	}


	//---------
	//Deallocation
	//---------
	delete[] x1;
	delete[] x2;

	return;

}


void max_occur(std::vector<int> v_table_name, std::vector<int> v_table_counts, int ncol, int size_i, std::vector<int> v_lm, int v_lm_size, int b1,
	const int i_option_collapsing, std::vector<int> &v_mxl, double** correlation_yicheng, int** correlation_temp2)

	//Description=========================================
	//Select all variables whose votes reaching i_option_collapsing
	//Algorithm:
	//If number of variables whose votes reaching i_option_collapsing is smaller than number of required correlated variables left
	//select all variables whose votes reaching i_option_collapsing
	//If number of variables whose votes reaching i_option_collapsing is larger than number of required correlated variables left
	//select variables whose votes reaching i_option_collapsing with the highest correlation

	// c++ code: 		Yicheng Yang

	// All rights reserved

	//

	// updated: Feb 23, 2020

	//IN    : int i_option_collapsing = choice of big-p algorithm 
	//                            0= no big-p algorithms
	//                           !0= perform big-p algorithms
	//IN    : int v_table_name = unique ranking name
	//IN    : int v_table_counts = corresponding counts of unique ranking name
	//IN    : int size_i = number of unique ranking names
	//IN    : int v_lm = actual location of missing variables of mox[i]
	//IN    : int v_lm_size = number of missing variables of mox[i]
	//IN    : int b1 = cursor of the "tank"
	//IN    : double correlation_yicheng(ncol, ncol);// correlation matrix
	//IN    : int correlation_temp2( v_lm_size, (ncol - v_lm_size) ); // correlation ranking matrix of missing variables neglecting itself
	//OUT   : int v_mxl(i_option_collapsing); // the actual location of most correlated variables of mox[i]
	//=====================================================

{
	int i_size_v_mxl = v_mxl.size();
	int i_size_left = i_option_collapsing - i_size_v_mxl; // number of required correlated variables left

	std::vector<int> location;// location of all variables reaching v_lm_size occurance

							  //---------------
	for (int i = 0; i < size_i; i++) {
		if ((v_table_counts[i] == v_lm_size) && (v_table_name[i] != 0)) {
			location.push_back(i);
			//TestOut<<"locations: "<< i <<endl;
		}
	}

	int location_size = location.size();

	//if (location_size == 0) Rprintf(" No variables qulified at current tank!  \n");

	//---------------
	//Case 1: if number of qualified variables (reaching v_lm_size occurance) is less than number of required correlated variables left
	if ((location_size < (i_size_left + 1)) && (location_size>0)) { //location_size <= i_size_left

		for (unsigned i = 0; i < location.size();i++) { // Note location.size() should be used because it's unsigned type
			v_mxl.push_back(v_table_name[location[i]]); // add all of them

			for (int k3 = 0; k3 < v_lm_size; k3++) {// set the added one as 0s in original ranking matrix
				for (int k4 = 0; k4 < (ncol - v_lm_size); k4++) {
					if (correlation_temp2[k3][k4] == v_table_name[location[i]]) {
						correlation_temp2[k3][k4] = 0;
					}

				}
			}
			//if (v_mxl.size() == i_option_collapsing) break;
		}
	}//end of the first case

	 //--------------
	 //Case 2: if number of qualified variables (reaching v_lm_size occurance) is larger than number of required correlated variables left
	if (location_size > i_size_left) {

		//TestOut<<"Yicheng here"<<endl;

		//double* d_max = new double[location_size]; // max correlation of missing variables between each qualified variable
		std::vector<double> d_max;

		for (int i = 0; i < location_size; i++) {
			//double* d_temp = new double[v_lm_size];
			std::vector<double> d_temp;

			for (int j = 0; j < v_lm_size; j++) {

				//TestOut << "row: " << v_table_name[location[i]] - 1 << ", col:" << v_lm[j] << endl;

				//int ou = v_table_name[location[i]] - 1;
				d_temp.push_back(abs(correlation_yicheng[v_table_name[location[i]] - 1][v_lm[j] - 1])); // Note 1. location and v_lm have the actual locations 2. Must compare absolute value of correlation
																										//TestOut<<"d_temp["<<i<<"]["<<j<<"]: "<< d_temp[j] <<endl;
			}
			d_max.push_back(max_FHDI(d_temp));
			//TestOut<<"d_max["<<i<<"]: "<< d_max[i]<<endl;
			//delete[] d_temp;
		}

		int d_max_size = d_max.size();
		//Pick i_size_left qualified variables among all of qualified variables
		for (int k = 0; k < i_size_left; k++) {

			int max_l = 0;
			for (int i = 0; i < d_max_size; i++) {

				if (d_max[max_l] < d_max[i]) {
					max_l = i;
				}
			}

			d_max[max_l] = 0;
			//TestOut<<"We choose "<< v_table_name[location[max_l]] <<endl;
			v_mxl.push_back(v_table_name[location[max_l]]); // add the one with max correlation 

															// set the added one as 0s in original ranking matrix
			for (int k3 = 0; k3 < v_lm_size; k3++) {
				for (int k4 = 0; k4 < (ncol - v_lm_size); k4++) {
					if (correlation_temp2[k3][k4] == v_table_name[location[max_l]]) {
						correlation_temp2[k3][k4] = 0;
					}

				}
			}


		}

		//delete[] d_max;
	}//end of the second case

	return;

}


void max_occur2(std::vector<int> v_table_name, std::vector<int> v_table_counts, int ncol, int size_i, std::vector<int> v_lm, int v_lm_size,
	const int i_option_collapsing, const int top, int nrow_ol, std::vector<int> &v_mxl, double** ol_matrix, int** correlation_temp2)

	//Description=========================================
	//Select all variables whose votes reaching i_option_collapsing
	//Algorithm:
	//If number of variables whose votes reaching i_option_collapsing is smaller than number of required correlated variables left
	//select all variables whose votes reaching i_option_collapsing
	//If number of variables whose votes reaching i_option_collapsing is larger than number of required correlated variables left
	//select variables whose votes reaching i_option_collapsing with the highest correlation

	//IN    : int i_option_collapsing = choice of big-p algorithm 
	//                            0= no big-p algorithms
	//                           !0= perform big-p algorithms
	//IN    : int v_table_name = unique ranking name
	//IN    : int v_table_counts = corresponding counts of unique ranking name
	//IN    : int size_i = number of unique ranking names
	//IN    : int v_lm = actual location of missing variables of mox[i]
	//IN    : int v_lm_size = number of missing variables of mox[i]
	//IN    : int b1 = cursor of the "tank"
	//IN    : double correlation_yicheng(ncol, ncol);// correlation matrix
	//IN    : int correlation_temp2( v_lm_size, (ncol - v_lm_size) ); // correlation ranking matrix of missing variables neglecting itself
	//OUT   : int v_mxl(i_option_collapsing); // the actual location of most correlated variables of mox[i]
	//=====================================================

{
	int i_size_v_mxl = v_mxl.size();
	int i_size_left = i_option_collapsing - i_size_v_mxl; // number of required correlated variables left

	std::vector<int> location;// location of all variables reaching v_lm_size occurance

							  //---------------
	for (int i = 0; i < size_i; i++) {
		if ((v_table_counts[i] == v_lm_size) && (v_table_name[i] != 0)) {
			location.push_back(i);
			//TestOut<<"locations: "<< i <<endl;
		}
	}

	int location_size = location.size();

	//if (location_size == 0) TestOut << "No variables qulified at current tank of SIS with intersection" << endl;
	//---------------
	//Case 1: if number of qualified variables (reaching v_lm_size occurance) is less than number of required correlated variables left
	if ((location_size < (i_size_left + 1)) && (location_size>0)) { //location_size <= i_size_left

		for (unsigned i = 0; i < location.size();i++) {
			v_mxl.push_back(v_table_name[location[i]]); // add all of them

			for (int k3 = 0; k3 < v_lm_size; k3++) {// set the added one as 0s in original ranking matrix
				for (int k4 = 0; k4 < top; k4++) {
					if (correlation_temp2[k3][k4] == v_table_name[location[i]]) {
						correlation_temp2[k3][k4] = 0;
					}

				}
			}
			//if (v_mxl.size() == i_option_collapsing) break;
		}
	}//end of the first case

	 //--------------
	 //Case 2: if number of qualified variables (reaching v_lm_size occurance) is larger than number of required correlated variables left
	if (location_size > i_size_left) {

		//double* d_max = new double[location_size]; // max correlation of missing variables between each qualified variable
		std::vector<double> d_max;


		double* x1 = new double[nrow_ol];
		double* x2 = new double[nrow_ol];
		double d_sum = 0.0;

		for (int i = 0; i < location_size; i++) {
			//double* d_temp = new double[v_lm_size];
			std::vector<double> d_temp;

			for (int j = 0; j < v_lm_size; j++) {

				//TestOut << "row: " << v_table_name[location[i]] - 1 << ", col:" << v_lm[j] << endl;

				//int ou = v_table_name[location[i]] - 1;
				for (int k = 0; k<nrow_ol; k++)
				{
					x1[k] = ol_matrix[k][v_table_name[location[i]] - 1];   //jth column
					x2[k] = ol_matrix[k][v_lm[j] - 1];//next column
				}
				//---
				//each column's mean
				//---
				double x1_mean = 0.0; double x2_mean = 0.0;
				for (int i = 0; i<nrow_ol; i++)
				{
					x1_mean += x1[i];   //jth column
					x2_mean += x2[i];   //next column
				}
				x1_mean = x1_mean / nrow_ol;
				x2_mean = x2_mean / nrow_ol;
				//TestOut << "x1_mean is " << x1_mean << ", and x2_mean is " << x2_mean << " at j_next = " << j_next << endl;
				//-----
				//calculate covariance of two columns
				//-----
				d_sum = 0.0;
				for (int i_1 = 0; i_1<nrow_ol; i_1++)
				{
					d_sum += (x1[i_1] - x1_mean)*(x2[i_1] - x2_mean);
				}

				//----------------
				//calculate variance of two columns
				//----------------
				double x1_var = 0.0; double x2_var = 0.0;
				double var_sum = 0.0;
				for (int i_2 = 0; i_2 < nrow_ol;i_2++) {
					var_sum = var_sum + (x1[i_2] - x1_mean)*(x1[i_2] - x1_mean);
				}
				x1_var = var_sum;

				var_sum = 0.0;
				for (int i_3 = 0; i_3 < nrow_ol;i_3++) {
					var_sum = var_sum + (x2[i_3] - x2_mean)*(x2[i_3] - x2_mean);
				}
				x2_var = var_sum;

				//d_temp[j] = abs(d_sum / sqrt(x1_var* x2_var));
				d_temp.push_back(abs(d_sum / sqrt(x1_var* x2_var))); // Note 1. location and v_lm have the actual locations 2. Must compare absolute value of correlation
																	 //TestOut<<"d_temp_Top["<<i<<"]["<<j<<"]: "<< d_temp[j] <<endl;
			}
			d_max.push_back(max_FHDI(d_temp));
			//d_max.push_back(max_FHDI(d_temp, v_lm_size));
			//TestOut<<"d_max["<<i<<"]: "<< d_max[i]<<endl;
			//delete[] d_temp;
		}

		//Pick i_size_left qualified variables among all of qualified variables

		int d_max_size = d_max.size();

		for (int k = 0; k < i_size_left; k++) {

			int max_l = 0;
			for (int i = 0; i < d_max_size; i++) {

				if (d_max[max_l] < d_max[i]) {
					max_l = i;
				}
			}

			d_max[max_l] = 0;
			//TestOut<<"We choose "<< v_table_name[location[max_l]] <<endl;
			v_mxl.push_back(v_table_name[location[max_l]]); // add the one with max correlation 

															// set the added one as 0s in original ranking matrix
			for (int k3 = 0; k3 < v_lm_size; k3++) {
				for (int k4 = 0; k4 < top; k4++) {
					if (correlation_temp2[k3][k4] == v_table_name[location[max_l]]) {
						correlation_temp2[k3][k4] = 0;
					}

				}
			}

		}

		//---------
		//Deallocation
		//---------
		delete[] x1;
		delete[] x2;

		//delete[] d_max;
	}//end of the second case

	return;

}



void max_occur_union(std::vector<int> v_table_name, std::vector<int> v_table_counts, int ncol, int size_i, std::vector<int> v_lm, int v_lm_size, int b1,
	const int i_option_collapsing, std::vector<int> &v_mxl, double** correlation_yicheng, int** correlation_temp2)

	//Description=========================================
	//Select all variables whose votes reaching i_option_collapsing
	//Algorithm:
	//If number of variables whose votes reaching i_option_collapsing is smaller than number of required correlated variables left
	//select all variables whose votes reaching i_option_collapsing
	//If number of variables whose votes reaching i_option_collapsing is larger than number of required correlated variables left
	//select variables whose votes reaching i_option_collapsing with the highest correlation

	// c++ code: 		Yicheng Yang

	// All rights reserved

	//

	// updated: Mar 26, 2020
	//IN    : int i_option_collapsing = choice of big-p algorithm 
	//                            0= no big-p algorithms
	//                           !0= perform big-p algorithms
	//IN    : int v_table_name = unique ranking name
	//IN    : int v_table_counts = corresponding counts of unique ranking name
	//IN    : int size_i = number of unique ranking names
	//IN    : int v_lm = actual location of missing variables of mox[i]
	//IN    : int v_lm_size = number of missing variables of mox[i]
	//IN    : int b1 = cursor of the "tank"
	//IN    : double correlation_yicheng(ncol, ncol);// correlation matrix
	//IN    : int correlation_temp2( v_lm_size, (ncol - v_lm_size) ); // correlation ranking matrix of missing variables neglecting itself
	//OUT   : int v_mxl(i_option_collapsing); // the actual location of most correlated variables of mox[i]
	//=====================================================

{
	int i_size_v_mxl = v_mxl.size();// the actual location of most correlated variables of mox[i]
	int i_size_left = i_option_collapsing - i_size_v_mxl; // number of required correlated variables left

	std::vector<int> location;// location of all variables reaching v_lm_size occurance

	//---------------
	for (int i = 0; i < size_i; i++) {
		if (v_table_name[i] != 0) {
			location.push_back(i);
			//TestOut<<"locations: "<< i <<endl;
		}
	}

	int location_size = location.size();

	//if (location_size == 0) TestOut << "No variables qulified at current tank of SIS with intersection" << endl;
	//---------------
	//Case 1: if number of qualified variables (reaching v_lm_size occurance) is less than number of required correlated variables left
	if ((location_size < (i_size_left + 1)) && (location_size > 0)) { //location_size <= i_size_left

		for (unsigned i = 0; i < location.size();i++) { // Note it's unsigned data type
			v_mxl.push_back(v_table_name[location[i]]); // add all of them

			for (int k3 = 0; k3 < v_lm_size; k3++) {// set the added one as 0s in original ranking matrix
				for (int k4 = 0; k4 < (ncol - v_lm_size); k4++) {
					if (correlation_temp2[k3][k4] == v_table_name[location[i]]) {
						correlation_temp2[k3][k4] = 0;
					}

				}
			}
			//if (v_mxl.size() == i_option_collapsing) break;
		}
	}//end of the first case

	//--------------
	//Case 2: if number of qualified variables (reaching v_lm_size occurance) is larger than number of required correlated variables left
	if (location_size > i_size_left) {

		//TestOut<<"Yicheng here"<<endl;

		//double* d_max = new double[location_size]; // max correlation of missing variables between each qualified variable
		std::vector<double> d_max;

		for (int i = 0; i < location_size; i++) {
			double* d_temp = new double[v_lm_size];
			//std::vector<double> d_temp;

			for (int j = 0; j < v_lm_size; j++) {

				//TestOut << "row: " << v_table_name[location[i]] - 1 << ", col:" << v_lm[j] << endl;

				//int ou = v_table_name[location[i]] - 1;
				d_temp[j] = abs(correlation_yicheng[v_table_name[location[i]] - 1][v_lm[j] - 1]);
				//d_temp.push_back( abs(correlation_yicheng[v_table_name[location[i]]-1][v_lm[j]-1]) ); // Note 1. location and v_lm have the actual locations 2. Must compare absolute value of correlation
				//TestOut<<"d_temp["<<i<<"]["<<j<<"]: "<< d_temp[j] <<endl;
			}
			//d_max.push_back( max_FHDI(d_temp) );
			d_max.push_back(max_FHDI(d_temp, v_lm_size));
			//TestOut<<"d_max["<<i<<"]: "<< d_max[i]<<endl;
			delete[] d_temp;
		}

		int d_max_size = d_max.size();
		//Pick i_size_left qualified variables among all of qualified variables
		for (int k = 0; k < i_size_left; k++) {

			int max_l = 0;
			for (int i = 0; i < d_max_size; i++) {

				if (d_max[max_l] < d_max[i]) {
					max_l = i;
				}
			}

			d_max[max_l] = 0;
			//TestOut<<"We choose "<< v_table_name[location[max_l]] <<endl;
			v_mxl.push_back(v_table_name[location[max_l]]); // add the one with max correlation 

			// set the added one as 0s in original ranking matrix
			for (int k3 = 0; k3 < v_lm_size; k3++) {
				for (int k4 = 0; k4 < (ncol - v_lm_size); k4++) {
					if (correlation_temp2[k3][k4] == v_table_name[location[max_l]]) {
						correlation_temp2[k3][k4] = 0;
					}

				}
			}


		}

		//delete[] d_max;
	}//end of the second case

	return;

}

void max_occur_union2(std::vector<int> v_table_name, std::vector<int> v_table_counts, int ncol, int size_i, std::vector<int> v_lm, int v_lm_size,
	const int i_option_collapsing, const int top, int nrow_ol, std::vector<int> &v_mxl, double** ol_matrix, int** correlation_temp2)

	//Description=========================================
	//Select all variables whose votes reaching i_option_collapsing
	//Algorithm:
	//If number of variables whose votes reaching i_option_collapsing is smaller than number of required correlated variables left
	//select all variables whose votes reaching i_option_collapsing
	//If number of variables whose votes reaching i_option_collapsing is larger than number of required correlated variables left
	//select variables whose votes reaching i_option_collapsing with the highest correlation

	//IN    : int i_option_collapsing = choice of big-p algorithm 
	//                            0= no big-p algorithms
	//                           !0= perform big-p algorithms
	//IN    : int v_table_name = unique ranking name
	//IN    : int v_table_counts = corresponding counts of unique ranking name
	//IN    : int size_i = number of unique ranking names
	//IN    : int v_lm = actual location of missing variables of mox[i]
	//IN    : int v_lm_size = number of missing variables of mox[i]
	//IN    : int b1 = cursor of the "tank"
	//IN    : double correlation_yicheng(ncol, ncol);// correlation matrix
	//IN    : int correlation_temp2( v_lm_size, (ncol - v_lm_size) ); // correlation ranking matrix of missing variables neglecting itself
	//OUT   : int v_mxl(i_option_collapsing); // the actual location of most correlated variables of mox[i]
	//=====================================================

{
	int i_size_v_mxl = v_mxl.size();// the actual location of most correlated variables of mox[i]
	int i_size_left = i_option_collapsing - i_size_v_mxl; // number of required correlated variables left

	std::vector<int> location;// location of all variables reaching v_lm_size occurance

							  //---------------
	for (int i = 0; i < size_i; i++) {
		if (v_table_name[i] != 0) {
			location.push_back(i);
			//TestOut<<"locations: "<< i <<endl;
		}
	}

	int location_size = location.size();

	//if (location_size == 0) TestOut << "No variables qulified at current tank of SIS with intersection" << endl;
	//---------------
	//Case 1: if number of qualified variables (reaching v_lm_size occurance) is less than number of required correlated variables left
	if ((location_size < (i_size_left + 1)) && (location_size>0)) { //location_size <= i_size_left

		for (unsigned i = 0; i < location.size();i++) {
			v_mxl.push_back(v_table_name[location[i]]); // add all of them

			for (int k3 = 0; k3 < v_lm_size; k3++) {// set the added one as 0s in original ranking matrix
				for (int k4 = 0; k4 < top; k4++) {
					if (correlation_temp2[k3][k4] == v_table_name[location[i]]) {
						correlation_temp2[k3][k4] = 0;
					}

				}
			}
			//if (v_mxl.size() == i_option_collapsing) break;
		}
	}//end of the first case

	 //--------------
	 //Case 2: if number of qualified variables (reaching v_lm_size occurance) is larger than number of required correlated variables left
	if (location_size > i_size_left) {

		//TestOut<<"Yicheng here"<<endl;

		//double* d_max = new double[location_size]; // max correlation of missing variables between each qualified variable
		std::vector<double> d_max;

		double* x1 = new double[nrow_ol];
		double* x2 = new double[nrow_ol];
		double d_sum = 0.0;

		for (int i = 0; i < location_size; i++) {
			//double* d_temp = new double[v_lm_size];
			std::vector<double> d_temp;

			for (int j = 0; j < v_lm_size; j++) {

				//TestOut << "row: " << v_table_name[location[i]] - 1 << ", col:" << v_lm[j] << endl;

				//int ou = v_table_name[location[i]] - 1;
				for (int k = 0; k<nrow_ol; k++)
				{
					x1[k] = ol_matrix[k][v_table_name[location[i]] - 1];   //jth column
					x2[k] = ol_matrix[k][v_lm[j] - 1];//next column
				}
				//---
				//each column's mean
				//---
				double x1_mean = 0.0; double x2_mean = 0.0;
				for (int i = 0; i<nrow_ol; i++)
				{
					x1_mean += x1[i];   //jth column
					x2_mean += x2[i];   //next column
				}
				x1_mean = x1_mean / nrow_ol;
				x2_mean = x2_mean / nrow_ol;
				//TestOut << "x1_mean is " << x1_mean << ", and x2_mean is " << x2_mean << " at j_next = " << j_next << endl;
				//-----
				//calculate covariance of two columns
				//-----
				d_sum = 0.0;
				for (int i_1 = 0; i_1<nrow_ol; i_1++)
				{
					d_sum += (x1[i_1] - x1_mean)*(x2[i_1] - x2_mean);
				}

				//----------------
				//calculate variance of two columns
				//----------------
				double x1_var = 0.0; double x2_var = 0.0;
				double var_sum = 0.0;
				for (int i_2 = 0; i_2 < nrow_ol;i_2++) {
					var_sum = var_sum + (x1[i_2] - x1_mean)*(x1[i_2] - x1_mean);
				}
				x1_var = var_sum;

				var_sum = 0.0;
				for (int i_3 = 0; i_3 < nrow_ol;i_3++) {
					var_sum = var_sum + (x2[i_3] - x2_mean)*(x2[i_3] - x2_mean);
				}
				x2_var = var_sum;

				//d_temp[j] = abs(d_sum / sqrt(x1_var* x2_var));
				d_temp.push_back(abs(d_sum / sqrt(x1_var* x2_var))); // Note 1. location and v_lm have the actual locations 2. Must compare absolute value of correlation

			}
			d_max.push_back(max_FHDI(d_temp));

		}

		//Pick i_size_left qualified variables among all of qualified variables

		int d_max_size = d_max.size();

		for (int k = 0; k < i_size_left; k++) {

			int max_l = 0;
			for (int i = 0; i < d_max_size; i++) {

				if (d_max[max_l] < d_max[i]) {
					max_l = i;
				}
			}

			d_max[max_l] = 0;

			v_mxl.push_back(v_table_name[location[max_l]]); // add the one with max correlation 

															// set the added one as 0s in original ranking matrix
			for (int k3 = 0; k3 < v_lm_size; k3++) {
				for (int k4 = 0; k4 < top; k4++) {
					if (correlation_temp2[k3][k4] == v_table_name[location[max_l]]) {
						correlation_temp2[k3][k4] = 0;
					}

				}
			}


		}

		//---------
		//Deallocation
		//---------
		delete[] x1;
		delete[] x2;

		//delete[] d_max;
	}//end of the second case

	return;

}


void correlated_variable_intersection(const int ncol, const int i_option_collapsing, int i, int* ia_temp,
	double **correlation_yicheng, int** correlation_ranking, std::vector<int> &v_mxl)

	//Description=========================================
	//Select the most i_option_collapsing correlated variables from all observed variables of each mox
	//Algorithm:
	//Select the most correlated variables using majority vote. The last one is selected based on the highest correlation
	// c++ code: 		Yicheng Yang

	// All rights reserved

	//

	// updated: Feb 24, 2020
	//IN    : int i_option_collapsing = choice of big-p algorithm 
	//                            0= no big-p algorithms
	//                           !0= perform big-p algorithms
	//IN    : int ia_temp(ncol) = copy of mox[i]
	//IN    : double correlation_yicheng(ncol, ncol);// correlation matrix
	//IN    : int correlation_ranking(ncol, ncol-1); // Ranking of correlation of each variable in descending order
	// Note it excludes itself from ranking
	//OUT   : int v_mxl(i_option_collapsing); // the actual location of most correlated variables of mox[i]
	//=====================================================

{
	std::vector<int> v_lm; //temporary vector for the locaiton of missing values in mox

	whichINVNOT(ia_temp, ncol, 0, v_lm); //get the actual location of missing variables of mox[i] 

	int v_lm_size = v_lm.size(); //number of missing variables in mox[i]

								 //========================================================
								 // Pickout the correlation matrix of missing variables only
								 //========================================================

	int** correlation_temp = New_iMatrix(v_lm_size, (ncol - 1)); // correlation ranking matrix of missing variables neglecting itself

																 //---------------------
																 // correlation ranking matrix of missing variables neglecting all missing variable cells
																 // this ranking matrix is even
	int** correlation_temp2 = New_iMatrix(v_lm_size, (ncol - v_lm_size));

	//TestOut << "v_lm at mox i=  " << i << endl;
	//for (int kk1 = 0; kk1 < v_lm_size; kk1++) {
	//	TestOut << v_lm[kk1] << endl;
	//}

	//========================================================
	// Remove all missing variable cells from the correlation matrix of missing variables
	//========================================================

	// Pickout the correlation ranking matrix of missing variables only
	for (int k1 = 0; k1 < v_lm_size; k1++) {
		for (int k2 = 0; k2 < (ncol - 1); k2++) {
			correlation_temp[k1][k2] = correlation_ranking[v_lm[k1] - 1][k2];
		}
	}

	for (int k3 = 0; k3 < v_lm_size; k3++) {
		for (int k4 = 0; k4 < (ncol - 1); k4++) {
			for (int k5 = 0; k5 < v_lm_size;k5++) {
				if (correlation_temp[k3][k4] == v_lm[k5]) {
					correlation_temp[k3][k4] = 0;
				}
			}

		}
	}

	//TestOut << "correlation ranking matrix removing all missing variables (containing 0s) at mox i=  " << i << endl;
	//for (int kk2 = 0; kk2 < v_lm_size; kk2++) {
	//	for (int kk3 = 0; kk3 < (ncol - 1); kk3++) {
	//		TestOut << setw(20) << correlation_temp[kk2][kk3];
	//	}
	//	TestOut << endl;
	//}

	//Remove all missing variables from ranking matrix
	for (int k6 = 0; k6 < v_lm_size; k6++) {
		for (int k7 = 0; k7 < (ncol - v_lm_size); k7++) {
			for (int k8 = 0; k8 < (ncol - 1); k8++) {
				if (correlation_temp[k6][k8] != 0) {
					correlation_temp2[k6][k7] = correlation_temp[k6][k8];
					correlation_temp[k6][k8] = 0;
					break;
				}
			}
		}
	}

	Del_iMatrix(correlation_temp, v_lm_size, (ncol - 1));
	//TestOut << "correlation ranking matrix removing all missing variables at mox i=" << i << endl;
	//for (int kk2 = 0; kk2 < v_lm_size; kk2++) {
	//	for (int kk3 = 0; kk3 < (ncol - v_lm_size); kk3++) {
	//		TestOut << setw(20) << correlation_temp2[kk2][kk3];
	//	}
	//	TestOut << endl;
	//}

	//=====================================================
	//Get the most i_option_collapsing correlated variables
	//======================================================

	std::vector<int> v_table_name;
	std::vector<int> v_table_counts;

	//std::vector<int> v_table_name1;
	//std::vector<int> v_table_counts1;

	for (int b1 = (i_option_collapsing - 1); b1 < (ncol - v_lm_size); b1++) {
		v_table_name.clear();
		v_table_counts.clear();
		int counter1 = 0;
		int* cor_temp = new int[(b1 + 1)*v_lm_size]; // the vector to store the 'tank' temporaryly 

		for (int a1 = 0; a1 < (b1 + 1); a1++) {

			for (int a2 = 0; a2 < v_lm_size; a2++) {
				cor_temp[counter1] = correlation_temp2[a2][a1];
				//TestOut << "cor_temp[" << counter1 << "]: " << correlation_temp2[a2][a1] << endl;
				counter1++;
			}
		}

		//----------------
		//This table function is only used for correlated variables
		table_cpp_int(cor_temp, (b1 + 1)*v_lm_size, v_table_name, v_table_counts);

		delete[] cor_temp;

		int size_i = v_table_counts.size();

		//for (int c1 = 0; c1 < size_i; c1++) {
		//	TestOut<< "v_table_name: " << v_table_name[c1] <<", v_table_counts: "<< v_table_counts[c1] <<endl;
		//}

		max_occur(v_table_name, v_table_counts, ncol, size_i, v_lm, v_lm_size, b1, i_option_collapsing, v_mxl, correlation_yicheng, correlation_temp2);

		int v_mxl_size = v_mxl.size();

		if (v_mxl_size == i_option_collapsing) break;

	}


	std::sort(v_mxl.begin(), v_mxl.end());
	//TestOut << "correlation_temp2 after max_occur at mox i=  " << i << endl;
	//for (int kk2 = 0; kk2 < v_lm_size; kk2++) {
	//	for (int kk3 = 0; kk3 < (ncol - v_lm_size); kk3++) {
	//		TestOut << setw(20) << correlation_temp2[kk2][kk3];
	//	}
	//	TestOut << endl;
	//}

	//----------------------------------
	//Del_iMatrix(correlation_temp, v_lm_size, (ncol - 1));
	Del_iMatrix(correlation_temp2, v_lm_size, (ncol - v_lm_size));

	return;

}


void correlated_variable_intersection2(const int ncol, const int i_option_collapsing, const int top, int i, int nrow_ol, int* ia_temp,
	double** ol_matrix, int** correlation_ranking_top, std::vector<int> &v_mxl)

	//Description=========================================
	//Select the most i_option_collapsing correlated variables from all observed variables of each mox
	//Algorithm:
	//Select the most correlated variables using majority vote. The last one is selected based on the highest correlation

	//IN    : int i_option_collapsing = choice of big-p algorithm 
	//                            0= no big-p algorithms
	//                           !0= perform big-p algorithms
	//IN    : int ia_temp(ncol) = copy of mox[i]
	//IN    : double correlation_yicheng(ncol, ncol);// correlation matrix
	//IN    : int correlation_ranking(ncol, ncol-1); // Ranking of correlation of each variable in descending order
	// Note it excludes itself from ranking
	//OUT   : int v_mxl(i_option_collapsing); // the actual location of most correlated variables of mox[i]
	//=====================================================

{
	std::vector<int> v_lm; //temporary vector for the locaiton of missing values in mox

	whichINVNOT(ia_temp, ncol, 0, v_lm); //get the actual location of missing variables of mox[i] 

	int v_lm_size = v_lm.size(); //number of missing variables in mox[i]

								 //========================================================
								 // Pickout the correlation matrix of missing variables only
								 //========================================================

	int** correlation_temp = New_iMatrix(v_lm_size, top); // correlation ranking matrix of missing variables neglecting itself

														  //---------------------
														  // correlation ranking matrix of missing variables neglecting all missing variable cells
														  // this ranking matrix is even
	int** correlation_temp2 = New_iMatrix(v_lm_size, top);
	Fill_iMatrix(correlation_temp2, v_lm_size, top, 0);


	//========================================================
	// Remove all missing variable cells from the correlation matrix of missing variables
	//========================================================

	// Pickout the correlation ranking matrix of missing variables only
	for (int k1 = 0; k1 < v_lm_size; k1++) {
		for (int k2 = 0; k2 < top; k2++) {
			correlation_temp[k1][k2] = correlation_ranking_top[v_lm[k1] - 1][k2];
		}
	}

	for (int k3 = 0; k3 < v_lm_size; k3++) {
		for (int k4 = 0; k4 < top; k4++) {
			for (int k5 = 0; k5 < v_lm_size;k5++) {
				if (correlation_temp[k3][k4] == v_lm[k5]) {
					correlation_temp[k3][k4] = 0;
				}
			}

		}
	}


	//Remove all missing variables from ranking matrix
	for (int k6 = 0; k6 < v_lm_size; k6++) {
		for (int k7 = 0; k7 < top; k7++) {
			for (int k8 = 0; k8 < top; k8++) {
				if (correlation_temp[k6][k8] != 0) {
					correlation_temp2[k6][k7] = correlation_temp[k6][k8];
					correlation_temp[k6][k8] = 0;
					break;
				}
			}
		}
	}

	Del_iMatrix(correlation_temp, v_lm_size, top);


	//=====================================================
	//Get the most i_option_collapsing correlated variables
	//======================================================

	std::vector<int> v_table_name;
	std::vector<int> v_table_counts;

	//std::vector<int> v_table_name1;
	//std::vector<int> v_table_counts1;

	for (int b1 = (i_option_collapsing - 1); b1 < top; b1++) {
		v_table_name.clear();
		v_table_counts.clear();
		int counter1 = 0;
		int* cor_temp = new int[(b1 + 1)*v_lm_size]; // the vector to store the 'tank' temporaryly 

		for (int a1 = 0; a1 < (b1 + 1); a1++) {

			for (int a2 = 0; a2 < v_lm_size; a2++) {
				cor_temp[counter1] = correlation_temp2[a2][a1];
				//TestOut << "cor_temp[" << counter1 << "]: " << correlation_temp2[a2][a1] << endl;
				counter1++;
			}
		}

		//----------------
		//This table function is only used for correlated variables
		table_cpp_int(cor_temp, (b1 + 1)*v_lm_size, v_table_name, v_table_counts);

		delete[] cor_temp;

		int size_i = v_table_counts.size();

		//for (int c1 = 0; c1 < size_i; c1++) {
		//	TestOut<< "v_table_name: " << v_table_name[c1] <<", v_table_counts: "<< v_table_counts[c1] <<endl;
		//}

		max_occur2(v_table_name, v_table_counts, ncol, size_i, v_lm, v_lm_size, i_option_collapsing, top, nrow_ol, v_mxl, ol_matrix, correlation_temp2);

		int v_mxl_size = v_mxl.size();

		if (v_mxl_size == i_option_collapsing) break;

	}


	sort(v_mxl.begin(), v_mxl.end());

	int v_mxl_size2 = v_mxl.size();

	if (v_mxl_size2 < i_option_collapsing) {
		Rprintf("ERROE! The intersection of top ranking matrix is not large enough to get user-defined numeber of selected variables. Reducing i_op_SIS or increasing top_corr_var will help. \n");
		//exit(0);
	}
	//TestOut << "correlation_temp2 after max_occur at mox i=  " << i << endl;
	//for (int kk2 = 0; kk2 < v_lm_size; kk2++) {
	//	for (int kk3 = 0; kk3 < (ncol - v_lm_size); kk3++) {
	//		TestOut << setw(20) << correlation_temp2[kk2][kk3];
	//	}
	//	TestOut << endl;
	//}

	//----------------------------------
	//Del_iMatrix(correlation_temp, v_lm_size, (ncol - 1));
	Del_iMatrix(correlation_temp2, v_lm_size, top);

	return;
}


void correlated_variable_union(const int ncol, const int i_option_collapsing, int i, int* ia_temp,
	double **correlation_yicheng, int** correlation_ranking, std::vector<int> &v_mxl)

	//Description=========================================
	//Select the most i_option_collapsing correlated variables from all observed variables of each mox
	//Algorithm:
	//Select the most correlated variables using majority vote. The last one is selected based on the highest correlation
	// c++ code: 		Yicheng Yang

	// All rights reserved

	// updated: Mar 26, 2020

	//IN    : int i_option_collapsing = choice of big-p algorithm 
	//                            0= no big-p algorithms
	//                           !0= perform big-p algorithms
	//IN    : int ia_temp(ncol) = copy of mox[i]
	//IN    : double correlation_yicheng(ncol, ncol);// correlation matrix
	//IN    : int correlation_ranking(ncol, ncol-1); // Ranking of correlation of each variable in descending order
													 // Note it excludes itself from ranking
	//OUT   : int v_mxl(i_option_collapsing); // the actual location of most correlated variables of mox[i]
	//=====================================================

{
	std::vector<int> v_lm; //temporary vector for the locaiton of missing values in mox

	whichINVNOT(ia_temp, ncol, 0, v_lm); //get the actual location of missing variables of mox[i] 

	int v_lm_size = v_lm.size(); //number of missing variables in mox[i]

	//========================================================
	// Pickout the correlation matrix of missing variables only
	//========================================================

	int** correlation_temp = New_iMatrix(v_lm_size, (ncol - 1)); // correlation ranking matrix of missing variables neglecting itself

	//---------------------
	// correlation ranking matrix of missing variables neglecting all missing variable cells
	// this ranking matrix is even
	int** correlation_temp2 = New_iMatrix(v_lm_size, (ncol - v_lm_size));

	//TestOut << "v_lm at mox i=  " << i << endl;
	//for (int kk1 = 0; kk1 < v_lm_size; kk1++) {
	//	TestOut << v_lm[kk1] << endl;
	//}

	//========================================================
	// Remove all missing variable cells from the correlation matrix of missing variables
	//========================================================

	// Pickout the correlation ranking matrix of missing variables only
	for (int k1 = 0; k1 < v_lm_size; k1++) {
		for (int k2 = 0; k2 < (ncol - 1); k2++) {
			correlation_temp[k1][k2] = correlation_ranking[v_lm[k1] - 1][k2];
		}
	}

	for (int k3 = 0; k3 < v_lm_size; k3++) {
		for (int k4 = 0; k4 < (ncol - 1); k4++) {
			for (int k5 = 0; k5 < v_lm_size;k5++) {
				if (correlation_temp[k3][k4] == v_lm[k5]) {
					correlation_temp[k3][k4] = 0;
				}
			}

		}
	}

	//TestOut << "correlation ranking matrix removing all missing variables (containing 0s) at mox i=  " << i << endl;
	//for (int kk2 = 0; kk2 < v_lm_size; kk2++) {
	//	for (int kk3 = 0; kk3 < (ncol - 1); kk3++) {
	//		TestOut << setw(20) << correlation_temp[kk2][kk3];
	//	}
	//	TestOut << endl;
	//}

	//Remove all missing variables from ranking matrix
	for (int k6 = 0; k6 < v_lm_size; k6++) {
		for (int k7 = 0; k7 < (ncol - v_lm_size); k7++) {
			for (int k8 = 0; k8 < (ncol - 1); k8++) {
				if (correlation_temp[k6][k8] != 0) {
					correlation_temp2[k6][k7] = correlation_temp[k6][k8];
					correlation_temp[k6][k8] = 0;
					break;
				}
			}
		}
	}

	Del_iMatrix(correlation_temp, v_lm_size, (ncol - 1));
	//TestOut << "correlation ranking matrix removing all missing variables at mox i=" << i << endl;
	//for (int kk2 = 0; kk2 < v_lm_size; kk2++) {
	//	for (int kk3 = 0; kk3 < (ncol - v_lm_size); kk3++) {
	//		TestOut << setw(20) << correlation_temp2[kk2][kk3];
	//	}
	//	TestOut << endl;
	//}

	//=====================================================
	//Get the most i_option_collapsing correlated variables
	//======================================================

	std::vector<int> v_table_name;
	std::vector<int> v_table_counts;

	//std::vector<int> v_table_name1;
	//std::vector<int> v_table_counts1;


	for (int b1 = 0; b1 < (ncol - v_lm_size); b1++) {
		v_table_name.clear();
		v_table_counts.clear();
		int counter1 = 0;
		int* cor_temp = new int[(b1 + 1)*v_lm_size]; // the vector to store the 'tank' temporaryly 

		for (int a1 = 0; a1 < (b1 + 1); a1++) {

			for (int a2 = 0; a2 < v_lm_size; a2++) {
				cor_temp[counter1] = correlation_temp2[a2][a1];
				//TestOut << "cor_temp[" << counter1 << "]: " << correlation_temp2[a2][a1] << endl;
				counter1++;
			}
		}

		//----------------
		//This table function is only used for correlated variables
		table_cpp_int(cor_temp, (b1 + 1)*v_lm_size, v_table_name, v_table_counts);

		delete[] cor_temp;

		int size_i = v_table_counts.size();

		//for (int c1 = 0; c1 < size_i; c1++) {
		//	TestOut<< "v_table_name: " << v_table_name[c1] <<", v_table_counts: "<< v_table_counts[c1] <<endl;
		//}

		max_occur_union(v_table_name, v_table_counts, ncol, size_i, v_lm, v_lm_size, b1, i_option_collapsing, v_mxl, correlation_yicheng, correlation_temp2);

		int v_mxl_size = v_mxl.size();

		if (v_mxl_size == i_option_collapsing) break;

	}

	sort(v_mxl.begin(), v_mxl.end());
	//TestOut << "correlation_temp2 after max_occur at mox i=  " << i << endl;
	//for (int kk2 = 0; kk2 < v_lm_size; kk2++) {
	//	for (int kk3 = 0; kk3 < (ncol - v_lm_size); kk3++) {
	//		TestOut << setw(20) << correlation_temp2[kk2][kk3];
	//	}
	//	TestOut << endl;
	//}

	//----------------------------------
	//Del_iMatrix(correlation_temp, v_lm_size, (ncol - 1));
	Del_iMatrix(correlation_temp2, v_lm_size, (ncol - v_lm_size));

	return;
}


void correlated_variable_union2(const int ncol, const int i_option_collapsing, const int top, int i, int nrow_ol, int* ia_temp,
	double** ol_matrix, int** correlation_ranking_top, std::vector<int> &v_mxl)

	//Description=========================================
	//Select the most i_option_collapsing correlated variables from all observed variables of each mox
	//Algorithm:
	//Select the most correlated variables using majority vote. The last one is selected based on the highest correlation

	//IN    : int i_option_collapsing = choice of big-p algorithm 
	//                            0= no big-p algorithms
	//                           !0= perform big-p algorithms
	//IN    : int ia_temp(ncol) = copy of mox[i]
	//IN    : double correlation_yicheng(ncol, ncol);// correlation matrix
	//IN    : int correlation_ranking(ncol, ncol-1); // Ranking of correlation of each variable in descending order
	// Note it excludes itself from ranking
	//OUT   : int v_mxl(i_option_collapsing); // the actual location of most correlated variables of mox[i]
	//=====================================================

{
	std::vector<int> v_lm; //temporary vector for the locaiton of missing values in mox

	whichINVNOT(ia_temp, ncol, 0, v_lm); //get the actual location of missing variables of mox[i] 

	int v_lm_size = v_lm.size(); //number of missing variables in mox[i]

								 //========================================================
								 // Pickout the correlation matrix of missing variables only
								 //========================================================

	int** correlation_temp = New_iMatrix(v_lm_size, top); // correlation ranking matrix of missing variables neglecting itself

														  //---------------------
														  // correlation ranking matrix of missing variables neglecting all missing variable cells
														  // this ranking matrix is even
	int** correlation_temp2 = New_iMatrix(v_lm_size, top);
	Fill_iMatrix(correlation_temp2, v_lm_size, top, 0);
	//TestOut << "v_lm at mox i=  " << i << endl;
	//for (int kk1 = 0; kk1 < v_lm_size; kk1++) {
	//	TestOut << v_lm[kk1] << endl;
	//}

	//========================================================
	// Remove all missing variable cells from the correlation matrix of missing variables
	//========================================================

	// Pickout the correlation ranking matrix of missing variables only
	for (int k1 = 0; k1 < v_lm_size; k1++) {
		for (int k2 = 0; k2 < top; k2++) {
			correlation_temp[k1][k2] = correlation_ranking_top[v_lm[k1] - 1][k2];
		}
	}

	for (int k3 = 0; k3 < v_lm_size; k3++) {
		for (int k4 = 0; k4 < top; k4++) {
			for (int k5 = 0; k5 < v_lm_size;k5++) {
				if (correlation_temp[k3][k4] == v_lm[k5]) {
					correlation_temp[k3][k4] = 0;
				}
			}

		}
	}


	//Remove all missing variables from ranking matrix
	for (int k6 = 0; k6 < v_lm_size; k6++) {
		for (int k7 = 0; k7 < top; k7++) {
			for (int k8 = 0; k8 < top; k8++) {
				if (correlation_temp[k6][k8] != 0) {
					correlation_temp2[k6][k7] = correlation_temp[k6][k8];
					correlation_temp[k6][k8] = 0;
					break;
				}
			}
		}
	}

	Del_iMatrix(correlation_temp, v_lm_size, top);


	//=====================================================
	//Get the most i_option_collapsing correlated variables
	//======================================================

	std::vector<int> v_table_name;
	std::vector<int> v_table_counts;

	//std::vector<int> v_table_name1;
	//std::vector<int> v_table_counts1;


	for (int b1 = 0; b1 < top; b1++) {
		v_table_name.clear();
		v_table_counts.clear();
		int counter1 = 0;
		int* cor_temp = new int[(b1 + 1)*v_lm_size]; // the vector to store the 'tank' temporaryly 

		for (int a1 = 0; a1 < (b1 + 1); a1++) {

			for (int a2 = 0; a2 < v_lm_size; a2++) {
				cor_temp[counter1] = correlation_temp2[a2][a1];
				counter1++;
			}
		}

		//----------------
		//This table function is only used for correlated variables
		table_cpp_int(cor_temp, (b1 + 1)*v_lm_size, v_table_name, v_table_counts);

		delete[] cor_temp;

		int size_i = v_table_counts.size();

		//for (int c1 = 0; c1 < size_i; c1++) {
		//	TestOut<< "v_table_name: " << v_table_name[c1] <<", v_table_counts: "<< v_table_counts[c1] <<endl;
		//}

		max_occur_union2(v_table_name, v_table_counts, ncol, size_i, v_lm, v_lm_size, i_option_collapsing, top, nrow_ol, v_mxl, ol_matrix, correlation_temp2);

		int v_mxl_size = v_mxl.size();

		if (v_mxl_size == i_option_collapsing) break;

	}

	sort(v_mxl.begin(), v_mxl.end());

	int v_mxl_size2 = v_mxl.size();

	if (v_mxl_size2 < i_option_collapsing) {
		Rprintf("ERROE! The union of top ranking matrix is not large enough to get user-defined numeber of selected variables. Reducing i_op_SIS or increasing top_corr_var will help. \n");
		//exit(0);
	}


	//----------------------------------
	//Del_iMatrix(correlation_temp, v_lm_size, (ncol - 1));
	Del_iMatrix(correlation_temp2, v_lm_size, (ncol - v_lm_size));

	return;
}


void correlated_variable_global(const int ncol, const int i_option_collapsing, int* ia_temp,
	double **correlation_yicheng, std::vector<int> &v_mxl)

	//Description=========================================
	//Select the most i_option_collapsing correlated variables from all observed variables of each mox
	//Algorithm:
	//Select the most correlated variables using majority vote. The last one is selected based on the highest correlation
	// c++ code: 		Yicheng Yang

	// All rights reserved

	//

	//IN    : int i_option_collapsing = choice of big-p algorithm 
	//                               0= no big-p algorithms
	//                              !0= perform big-p algorithms
	//IN    : int ia_temp(ncol) = copy of mox[i]
	//IN    : double correlation_yicheng(ncol, ncol);// correlation matrix

	//OUT   : int v_mxl(i_option_collapsing); // the actual location of most correlated variables of mox[i]
	//=====================================================

{

	std::vector<int> v_lm; //temporary vector for the locaiton of missing values in mox

	whichINVNOT(ia_temp, ncol, 0, v_lm); //get the actual location of missing variables of mox[i] 

	int v_lm_size = v_lm.size(); //number of missing variables in mox[i]

	std::vector<int> v_lo; //temporary vector for the actual locaiton of observed values in mox[i]

	//for (int a1 = 0; a1 < ncol; a1++) {
	//	for (int a2 = 0; a2 < v_lm_size; a2++) {
	//		if ((a1 + 1) != v_lm[a2]) {
	//			continue;
	//			
	//		}
	//		v_lo.push_back(a1 + 1);
	//	}
	//}
	whichINV(ia_temp, ncol, 0, v_lo); //get the location of Non-zero in mox 

	int v_lo_size = v_lo.size(); //number of observed variables in mox[i]

	//for (int a3 = 0; a3 < v_lo_size; a3++) {
	//	TestOut <<"v_lo["<<a3<<"]: "<< v_lo[a3] << endl;
	//}

	if (v_lm_size + v_lo_size != ncol) {
		Rprintf("Error in correlated_variable_gloabl!!!!!"); return;
	}

	//========================================================
	// Pickout the correlation matrix of missing variables only
	//========================================================

	double** correlation_temp = New_dMatrix(v_lm_size, (ncol - v_lm_size));

	//TestOut << "v_lm at mox i=  "<< endl;
	//for (int kk1 = 0; kk1 < v_lm_size; kk1++) {
	//	TestOut << v_lm[kk1] << endl;
	//}

	for (int k6 = 0; k6 < v_lm_size; k6++) {
		for (int k7 = 0; k7 < (ncol - v_lm_size); k7++) {
			correlation_temp[k6][k7] = abs(correlation_yicheng[v_lm[k6] - 1][v_lo[k7] - 1]);
		}
	}


	std::vector<double> r_star; // vector of the highest correlation of each obeserved variable of current mox
	std::vector<double> r_temp; // temporary vector to hold coreelations of between an observed variable and all missing variables

	//=================================
	// Select out the vector of the highest correlation of each obeserved variable of current mox
	for (int k8 = 0; k8 < v_lo_size; k8++) {
		r_temp.clear();
		for (int k9 = 0; k9 < v_lm_size; k9++) {
			r_temp.push_back(correlation_temp[k9][k8]);
		}

		r_star.push_back(max_FHDI(r_temp));

	}

	//for (int b1 = 0; b1 < r_star.size(); b1++) {
	//	TestOut <<"r_star["<<b1<<"]: "<< r_star[b1] << endl;
	//}
	//===================================
	// Add variables whose correlation is among the top of the largest i_option_collapsing

	for (int k10 = 0; k10 < i_option_collapsing; k10++) {
		int max_corr = 0;

		for (int k11 = 0; k11 < v_lo_size; k11++) {
			if (r_star[max_corr] < r_star[k11]) {
				max_corr = k11;
			}
		}

		//TestOut<<"max_corr: "<< max_corr <<endl;
		v_mxl.push_back(v_lo[max_corr]); // the actual location of the variable

		r_star[max_corr] = 0;
	}


	sort(v_mxl.begin(), v_mxl.end());
	//TestOut << "correlation_temp2 after max_occur at mox i=  " << i << endl;
	//for (int kk2 = 0; kk2 < v_lm_size; kk2++) {
	//	for (int kk3 = 0; kk3 < (ncol - v_lm_size); kk3++) {
	//		TestOut << setw(20) << correlation_temp2[kk2][kk3];
	//	}
	//	TestOut << endl;
	//}

	//----------------------------------
	//Del_iMatrix(correlation_temp, v_lm_size, (ncol - 1));
	Del_dMatrix(correlation_temp, v_lm_size, (ncol - v_lm_size));

	return;
}

void correlated_variable_global2(const int ncol, const int i_option_collapsing, int nrow_ol, int* ia_temp,
	double** ol_matrix, std::vector<int> &v_mxl)

	//Description=========================================
	//Select the most i_option_collapsing correlated variables from all observed variables of each mox
	//Algorithm:
	//Select the most correlated variables using majority vote. The last one is selected based on the highest correlation

	//IN    : int i_option_collapsing = choice of big-p algorithm 
	//                               0= no big-p algorithms
	//                              !0= perform big-p algorithms
	//IN    : int ia_temp(ncol) = copy of mox[i]
	//IN    : double correlation_yicheng(ncol, ncol);// correlation matrix

	//OUT   : int v_mxl(i_option_collapsing); // the actual location of most correlated variables of mox[i]
	//=====================================================

{

	std::vector<int> v_lm; //temporary vector for the locaiton of missing values in mox

	whichINVNOT(ia_temp, ncol, 0, v_lm); //get the actual location of missing variables of mox[i] 

	int v_lm_size = v_lm.size(); //number of missing variables in mox[i]

	std::vector<int> v_lo; //temporary vector for the actual locaiton of observed values in mox[i]

	whichINV(ia_temp, ncol, 0, v_lo); //get the actual location of Non-zero in mox 

	int v_lo_size = v_lo.size(); //number of observed variables in mox[i]

	std::vector<double> r_star; // vector of the highest correlation of each obeserved variable of current mox
	std::vector<double> r_temp; // temporary vector to hold coreelations of between an observed variable and all missing variables

								//=================================
								// Select out the vector of the highest correlation of each obeserved variable of current mox
								//==================================

	double* x1 = new double[nrow_ol];
	double* x2 = new double[nrow_ol];
	double d_sum = 0.0;

	for (int k8 = 0; k8 < v_lo_size; k8++) {

		r_temp.clear();

		for (int k9 = 0; k9 < v_lm_size; k9++) {

			for (int k10 = 0; k10<nrow_ol; k10++)
			{
				x1[k10] = ol_matrix[k10][v_lo[k8] - 1];
				x2[k10] = ol_matrix[k10][v_lm[k9] - 1];
			}

			//---
			//each column's mean
			//---
			double x1_mean = 0.0; double x2_mean = 0.0;
			for (int i = 0; i<nrow_ol; i++)
			{
				x1_mean += x1[i];   //jth column
				x2_mean += x2[i];   //next column
			}
			x1_mean = x1_mean / nrow_ol;
			x2_mean = x2_mean / nrow_ol;
			//TestOut << "x1_mean is " << x1_mean << ", and x2_mean is " << x2_mean << " at j_next = " << j_next << endl;
			//-----
			//calculate covariance of two columns
			//-----
			d_sum = 0.0;
			for (int i_1 = 0; i_1<nrow_ol; i_1++)
			{
				d_sum += (x1[i_1] - x1_mean)*(x2[i_1] - x2_mean);
			}

			//----------------
			//calculate variance of two columns
			//----------------
			double x1_var = 0.0; double x2_var = 0.0;
			double var_sum = 0.0;
			for (int i_2 = 0; i_2 < nrow_ol;i_2++) {
				var_sum = var_sum + (x1[i_2] - x1_mean)*(x1[i_2] - x1_mean);
			}
			x1_var = var_sum;

			var_sum = 0.0;
			for (int i_3 = 0; i_3 < nrow_ol;i_3++) {
				var_sum = var_sum + (x2[i_3] - x2_mean)*(x2[i_3] - x2_mean);
			}
			x2_var = var_sum;

			r_temp.push_back(abs(d_sum / sqrt(x1_var* x2_var)));
			//r_temp.push_back(correlation_temp[k9][k8]);
		}

		r_star.push_back(max_FHDI(r_temp));

	}

	//===================================
	// Add variables whose correlation is among the top of the largest i_option_collapsing
	//====================================

	for (int k10 = 0; k10 < i_option_collapsing; k10++) {
		int max_corr = 0;

		for (int k11 = 0; k11 < v_lo_size; k11++) {
			if (r_star[max_corr] < r_star[k11]) {
				max_corr = k11;
			}
		}

		//TestOut<<"max_corr: "<< max_corr <<endl;
		v_mxl.push_back(v_lo[max_corr]); // the actual location of the variable

		r_star[max_corr] = 0;
	}


	sort(v_mxl.begin(), v_mxl.end());

	int v_mxl_size = v_mxl.size();

	if (v_mxl_size != i_option_collapsing) {
		Rprintf("ERROE! The global ranking of top ranking matrix is not large enough to get user-defined numeber of selected variables. Reducing i_op_SIS or increasing top_corr_var will help. \n");
		//exit(0);
	}


	//---------
	//Deallocation
	//---------
	delete[] x1;
	delete[] x2;

	return;
}

//-----------------------------------------------------
//-----------------------------------------------------
//for replacing unneccessarily large matrix d_rw[][]
//Jan 18, 2019
//----------------------------------------------------- 
class RepWeight_FHDI{
	
public:
	int size_row() const {return _size_row;}
	
	//----------
	//(i,j) operator overloading
	//----------
	double operator() (int i, int j) const
	{
		double d_element = 1.0*_size_row/(_size_row - 1.0); //n/(n-1)
		
		
		if(i == j) {d_element = 0.0;} //zero diagonal
		
		//exceptions
		if(i<0 || i >= _size_row || j<0 || j >= _size_row)
		{ d_element = 0.0;}
	
		return d_element; 
	}
public: 
	RepWeight_FHDI(int n): _size_row(n) { } ; //constructor
	~RepWeight_FHDI() { } ; //desctructor
	
private:
	int _size_row; 
	
private:
	RepWeight_FHDI(const RepWeight_FHDI &); 
	const RepWeight_FHDI & operator = (const RepWeight_FHDI &) ; 
	
};


} //end of namespce 



//Fn===========================================================================

//List_FHDI.h-----------------------------------------------------------------------------

//Rn===========================================================================

//======================

//Compact LIST class declaration 

//to replace "R" list class

//

//

//Note: 1. only the given number of elements are stored in the compact storage

//      2. but the access can happen through the normal index (i,j)  like c++, starting from 0

//      3. all operation on an element outside the size of each list entity is null

//

// Last update: Oct 12, 2016

//

// by Dr. I. Cho 

// All rights reservd

//======================

class List_FHDI{



public:



	int size_row()   const {return _size_row;}  //size of total rows

	int size_block() const {return _v_block.size();} //size of total meaningful data stored in the block



	//----------------

	//initialize all private memory

	//----------------

	void initialize(int new_size_row);

	

    //======================

	//(i,j) operator overloading 

	//to access to the items of the array storage

	//Note: _v_block is one-dimensional vector for the compact LIST

	//      below operation overload of () is necessary to access the private _v_block 

	//======================
    //below appears to cause gcc-ASAN error. inactivated on April 23, 2018
	/*
	double & operator() (int i, int j) //ith list, jth entity (like c++,i.e. from 0)

	{

	

        double d_temp=0.0;

		double& d_element = d_temp ;  //default 



		//================

		//(i,j) is for LIST entity: ith list and jth term 

		//so proper indexing is required 

		//================

		int i_size_of_list = _n_each_row_size[i] ; //get the size of the ith list row

        

		if(j< 0 || j >= i_size_of_list ) //out of width. 

		{

		    //NONE

		}

		else //within width

		{

			//get accumulated location of (i-1)th list row

			int i_sum = 0; for(int k=0; k<i; k++) {i_sum += _n_each_row_size[k];}

			return _v_block[i_sum + j] ;

		}

		

		return d_element;

	} 
    */	



    //========================

	//probably, only get the stored data

	//Note: return is a const double value 

	//========================

	double   operator() (int i, int j) const //like c++ rule, i.e., from 0 

	{

	

		double d_element=0.0; 



		//================

		//(i,j) is for LIST entity: ith list and jth term 

		//so proper indexing is required 

		//================

		int i_size_of_list = _n_each_row_size[i] ; //get the size of the ith list row

        

		if(j< 0 || j >= i_size_of_list ) //out of width. 

		{

			d_element = 0.0;

		}

		else //within width

		{

			int i_sum = 0; for(int k=0; k<i; k++) {i_sum += _n_each_row_size[k];}

			return _v_block[i_sum + j] ;

		}	

		



		return d_element;

	} 



	//==========================

	//	get all the stored non-null values from _v_block 

	//==========================

	void unlist(std::vector<double> & d_value);



	//==========================

	//put entire block into the storage _v_block

	//==========================

	void put_entire_block(std::vector<double> d_value);

	

    //==========================

	//get the stored _v_block at the i_row row of the list 

	//==========================

	void get_block(const int i_row, double* d_value); 

	void get_block_yicheng(const int i_row, std::vector<int> &v_value); //Written by Yicheng Yang

	void get_block(const int i_row, const int n_size_row, const int n_size_col, 

                   double** d_value);	



    //==========================

	//put the new into _v_block's row i_row with n_size entities 

	//==========================

	void put_block(const int i_row, const int n_size, double* d_value); 

	void put_block_yicheng(const int i_row, const int n_size, std::vector<double> v_value); //Written by Yicheng Yang

    void put_block(const int i_row, const int n_size_row, const int n_size_col, 

                   double ** d_value);	

    void put_block(const int i_row, std::vector<double> v_value);

	void put_block(const int i_row, std::vector<int> v_value); // Written by Yicheng Yang

	//==========================

	//get the stored _n_each_row_size at row i_row

	//==========================

	void get_a_row_size(const int i_row, int &i_value); 



	//==========================

	//put the new row size into storage

	//==========================

	void put_a_row_size(const int i_row, int i_value); 

	

	//=====================

	//print out List_FHDI

	//=====================

	void print_List_FHDI();	

	

	//=====================

	//print out ONE row of List_FHDI

	//=====================

	void print_one_List_FHDI(const int i_row);	

	

public:

	List_FHDI(int size_row); //constructor

	~List_FHDI();        //destructor





//================

//data members

//================

private:

	int _size_row; //total row of the current LIST

	std::vector<double> _v_block;

	int* _n_each_row_size ; //array for the size of each row of LIST

	

//================

//below is for avoiding possible error by an automatic task done by compiler

//In fact doing nothing as below makes it stable 

//================

private:

	List_FHDI(const List_FHDI &) ;

	const List_FHDI & operator = (const List_FHDI &) ;

};





//Fn===========================================================================

//List_FHDI.cc-----------------------------------------------------------------------------

//Fn===========================================================================



//======================

//Compact LIST class declaration with the KNOWN ROW numbers

//to replace "R" list class

//

//

//Note: 1. only the given number of elements are stored in the compact storage

//      2. but the access can happen through the normal index (i,j)  like c++, starting from 0

//      3. all operation on an element outside the size of each list entity is null

//

// Last update: Oct 12, 2016

//

// by Dr. I. Cho 

// All rights reservd

//======================



//================

//implementation of List_FHDI class

//================

void List_FHDI::initialize(int new_size_row)

{

	_size_row = new_size_row; 

	

	_n_each_row_size = NULL;

	_n_each_row_size = new int[new_size_row];

	for(int i=0; i<new_size_row; i++) _n_each_row_size[i] = 0 ; 

	

	_v_block.clear(); //return this to size 0

}



void List_FHDI::unlist(std::vector<double> & d_value)

//Description==================================

//	get all the stored non-null values from _v_block 

//  to d_value[]

//  like R's "unlist()" 

//

//IN   : int  i_row    = target row in the list

//OUT  : double d_value[n_size]  where n_size must be known before calling this fn. 

//

//Note: all variable preceded by '_' are private of Grid

//=============================================

{

	int n_size = _v_block.size();

		

	for(int i=0; i<n_size; i++) {d_value.push_back(_v_block[i]);} 



    return;

}



void List_FHDI::put_entire_block(std::vector<double> d_value)

//Description==================================

//	put the entire block into the storage _v_block 

//  from d_value[]

//

//IN   : double d_value[n_size]  where n_size must be known before calling this fn. 

//

//Note: all variable preceded by '_' are private of Grid

//=============================================

{

	int n_size = d_value.size();

		

	for(int i=0; i<n_size; i++) {_v_block.push_back(d_value[i]);} 



    return;

}



void List_FHDI::get_block(const int i_row, double* d_value)

//Description==================================

//	get stored block at the i_row of the list 

//

//IN   : int  i_row    = target row in the list

//OUT  : double d_value[n_size]  where n_size must be known before calling this fn. 

//

//Note: all variable preceded by '_' are private of Grid

//=============================================

{

	int n_size = _n_each_row_size[i_row]; 

	

	//accumulated size of all the previous rows in the list

	int i_sum = 0; for(int k=0; k<i_row; k++) {i_sum += _n_each_row_size[k];}

	

	for(int i=0; i<n_size; i++) {d_value[i] = _v_block[i_sum + i];} 



    return;

}

void List_FHDI::get_block_yicheng(const int i_row, std::vector<int> &v_value)
//Description==================================
//	get stored block at the i_row of the list 
//
//IN   : int  i_row    = target row in the list
//OUT  : double d_value[n_size]  where n_size must be known before calling this fn. 
//
//Note: all variable preceded by '_' are private of Grid
//=============================================
{
	int n_size = _n_each_row_size[i_row];

	//accumulated size of all the previous rows in the list
	int i_sum = 0; for (int k = 0; k < i_row; k++) { i_sum += _n_each_row_size[k]; }

	for (int i = 0; i < n_size; i++) {
		v_value.push_back(_v_block[i_sum + i]);
	}

	return;
}

void List_FHDI::get_block(const int i_row, const int n_size_row, const int n_size_col, 

                          double** d_value)

//Description==================================

//	get stored MATRIX block at the i_row of the list 

//  that was stored by row-first rule

//

//IN   : int  i_row    = target row in the list

//OUT  : double d_value[n_size_row][n_size_col]  where n_size must be known before calling this fn. 

//

//Note: all variable preceded by '_' are private of Grid

//=============================================

{

	int n_size = _n_each_row_size[i_row]; 

	if(n_size != n_size_row*n_size_col) 
	{Rprintf("Error! matrix size is wrong in List_FHDI"); return;}


	//accumulated size of all the previous rows in the list

	int i_sum = 0; for(int k=0; k<i_row; k++) {i_sum += _n_each_row_size[k];}

	

	for(int i=0; i<n_size_col; i++) 

	{

		for(int j=0; j<n_size_row; j++)

			d_value[j][i] = _v_block[i_sum++];

	} 



    return;

}





void List_FHDI::put_block(const int i_row, const int n_size, double * d_value)

//Description==================================

//	put the new row into block

//  Note: 1. if current row i_row was not stored before, just append it by using push_back

//        2. if this row has been stored before, replacement takes place at the row

//

//IN  : int i_row  = target row number of the list (from 0 like c++ index)

//IN  : int n_size = ACTUAL size of the current row 

//IN  : double d_value[i_size] 

//

//Note: all variable preceded by '_' are private of Grid

//=============================================

{

	int n_existing_size = _n_each_row_size[i_row]; 

	//----------------

	//first time input

	//----------------

	if(n_existing_size == 0)

	{

		//----------------

		//store the new data into the _v_block 

		//----------------

		for(int i=0; i<n_size; i++) _v_block.push_back(d_value[i]);

		

		//-----------------

		//update the size of current row of list

		//-----------------

		_n_each_row_size[i_row] = n_size; 

			

	}

	//---------------

	//replace existing stored data

	//---------------

	if(n_existing_size > 0)

	{

		//accumulated size of all the previous rows in the list

		int i_sum = 0; for(int k=0; k<i_row; k++) {i_sum += _n_each_row_size[k];}

		

		for(int i=0; i<n_size; i++) {_v_block[i_sum + i] = d_value[i];} 

	}



    return;

}


void List_FHDI::put_block_yicheng(const int i_row, const int n_size, std::vector<double> v_value)
//Description==================================

//	put the new row into block

//  Note: 1. if current row i_row was not stored before, just insert it by using .insert and update the size as n_size

//        2. if this row has been stored before, just insert it by using .insert and accumlate the size as n_existing_size + n_size

//
//IN  : int i_row  = target row number of the list (from 0 like c++ index)

//IN  : int n_size = ACTUAL size of the current row 

//IN  : vector<double> v_value

//

//Note: all variable preceded by '_' are private of Grid

//=============================================
{
	int n_existing_size = _n_each_row_size[i_row];
	//----------------
	//first time input
	//----------------
	if (n_existing_size == 0)
	{
		//accumulated size of all the previous rows in the list
		int i_sum = 0; for (int k = 0; k < i_row; k++) { i_sum += _n_each_row_size[k]; }

		_v_block.insert(_v_block.begin() + i_sum, v_value.begin(), v_value.end());

		_n_each_row_size[i_row] = n_size;

	}
	//---------------
	//replace existing stored data
	//---------------
	//if ((n_existing_size > 0) && (n_existing_size == n_size))
	//{
	//	//accumulated size of all the previous rows in the list
	//	int i_sum = 0; for (int k = 0; k < i_row; k++) { i_sum += _n_each_row_size[k]; }

	//	for (int i = 0; i < n_size; i++) { _v_block[i_sum + i] = v_value[i]; }
	//}

	if (n_existing_size > 0)
	{
		//accumulated size of all the previous rows in the list
		int i_sum = 0; for (int k = 0; k < (i_row + 1); k++) { i_sum += _n_each_row_size[k]; }

		//std::vector<int>::iterator it;

		_v_block.insert(_v_block.begin() + i_sum, v_value.begin(), v_value.end());

		_n_each_row_size[i_row] = n_existing_size + n_size;
	}

	return;
}


void List_FHDI::put_block(const int i_row, const int n_size_row, const int n_size_col, 

                          double ** d_value)

//Description==================================

//	put the new row matrix into block

//  by using row-first rule 

//

//  Note: 1. if current row i_row was not stored before, just append it by using push_back

//        2. if this row has been stored before, replacement takes place at the row

//

//IN  : int i_row  = target row number of the list (from 0 like c++ index)

//IN  : double d_value[n_size_row, n_size_col] 

//

//Note: all variable preceded by '_' are private of Grid

//=============================================

{

	int n_existing_size = _n_each_row_size[i_row]; 

	

	const int n_size = n_size_row*n_size_col; //total values of matrix 

	

	//----------------

	//first time input

	//----------------

	if(n_existing_size == 0)

	{

		for(int j=0; j<n_size_col; j++) 

		{

			for(int k=0; k<n_size_row; k++)

				_v_block.push_back(d_value[k][j]); 

		}

		

		//-----------------

		//update the size of current row of list

		//-----------------

		_n_each_row_size[i_row] = n_size; 

			

	}

	//---------------

	//replace existing stored data

	//---------------

	if(n_existing_size > 0)

	{

		//accumulated size of all the previous rows in the list

		int i_sum = 0; for(int k=0; k<i_row; k++) {i_sum += _n_each_row_size[k];}

		

		for(int j=0; j<n_size_col; j++) 

		{

			for(int k=0; k<n_size_row; k++)

				_v_block[i_sum++] = d_value[k][j];

		} 

	}



    return;

}



void List_FHDI::put_block(const int i_row, std::vector<double> v_value)

//Description==================================

//	put the new row store in double vector into block

//  Note: 1. if current row i_row was not stored before, just append it by using push_back

//        2. if this row has been stored before, replacement takes place at the row

//

//IN  : int i_row  = target row number of the list (from 0 like c++ index)

//IN  : std::vector<double> d_value[i_size] 

//

//Note: all variable preceded by '_' are private of Grid

//=============================================

{

	const int n_size = (int)v_value.size(); 

	

	int n_existing_size = _n_each_row_size[i_row]; 

	//----------------

	//first time input

	//----------------

	if(n_existing_size == 0)

	{

		//----------------

		//store the new data into the _v_block 

		//----------------

		for(int i=0; i<n_size; i++) _v_block.push_back(v_value[i]);

		

		//-----------------

		//update the size of current row of list

		//-----------------

		_n_each_row_size[i_row] = n_size; 

			

	}

	//---------------

	//replace existing stored data

	//---------------

	if(n_existing_size > 0)

	{

		//accumulated size of all the previous rows in the list

		int i_sum = 0; for(int k=0; k<i_row; k++) {i_sum += _n_each_row_size[k];}

		

		for(int i=0; i<n_size; i++) {_v_block[i_sum + i] = v_value[i];} 

	}



    return;

}

void List_FHDI::put_block(const int i_row, std::vector<int> v_value)// Written by Yicheng
																	//Description==================================
																	//	put the new row store in double vector into block
																	//  Note: 1. if current row i_row was not stored before, just append it by using push_back
																	//        2. if this row has been stored before, replacement takes place at the row
																	//
																	//IN  : int i_row  = target row number of the list (from 0 like c++ index)
																	//IN  : std::vector<double> d_value[i_size] 
																	//
																	//Note: all variable preceded by '_' are private of Grid
																	//=============================================
{
	const int n_size = (int)v_value.size();

	int n_existing_size = _n_each_row_size[i_row];
	//----------------
	//first time input
	//----------------
	if (n_existing_size == 0)
	{
		//----------------
		//store the new data into the _v_block 
		//----------------
		for (int i = 0; i < n_size; i++) _v_block.push_back(v_value[i]);

		//-----------------
		//update the size of current row of list
		//-----------------
		_n_each_row_size[i_row] = n_size;

	}
	//---------------
	//replace existing stored data
	//---------------
	if (n_existing_size > 0)
	{
		//accumulated size of all the previous rows in the list
		int i_sum = 0; for (int k = 0; k < i_row; k++) { i_sum += _n_each_row_size[k]; }

		for (int i = 0; i < n_size; i++) { _v_block[i_sum + i] = v_value[i]; }
	}

	return;
}



void List_FHDI::get_a_row_size(const int i_row, int & i_value)

//Description==================================

//	get stored _n_each_row_size of a row at i_row 

//

//IN   : int i_row 	= the row number of the list

//OUT  : int i_value 

//

//Note: all variable preceded by '_' are private of Grid

//=============================================

{

	i_value = _n_each_row_size[i_row]; 



    return;

}





void List_FHDI::put_a_row_size(const int i_row, int i_value)

//Description==================================

//	put the new size of the list into _n_each_row_size

//  

//

//IN  : int i_row   = the row number of the list

//IN  : int i_value 

//

//Note: all variable preceded by '_' are private of Grid

//=============================================

{

	_n_each_row_size[i_row] = i_value; 



    return;

}



//=====================

//print out List_FHDI

//=====================

void List_FHDI::print_List_FHDI()

{

	int n_row = (*this).size_row(); 

	for(int i=0; i<n_row; i++)

	{

		int i_temp = 0; (*this).get_a_row_size(i, i_temp);

		if(i_temp>0) //only for meaningful row 

		{

			double* d_temp = new double[i_temp];

			(*this).get_block(i, d_temp);

			FHDI::RPrint(i);

			FHDI::RPrint(d_temp, i_temp);

			delete[] d_temp; 

		}

	}

	return; 

}



//=====================

//print out ONE Row of List_FHDI

//=====================

void List_FHDI::print_one_List_FHDI(const int i_row)

{

	int n_row = (*this).size_row(); 

	

	if(i_row < n_row) 

	{

		int i = i_row; //target row number

		

		int i_temp = 0; (*this).get_a_row_size(i, i_temp);

		if(i_temp>0) //only for meaningful row 

		{

			double* d_temp = new double[i_temp];

			(*this).get_block(i, d_temp);

			FHDI::RPrint(i);

			FHDI::RPrint(d_temp, i_temp);

			delete[] d_temp; 

		}

	}

	return; 

}



//=====================

//Constructor

//=====================

List_FHDI::List_FHDI(int size_row)

: _size_row(size_row), _n_each_row_size(new int[size_row])

{

	//=======

	//initialize with 0

	//=======

	for(int i=0; i<size_row; i++) _n_each_row_size[i] = 0 ; 



}



//=====================

//Destructor

//=====================

List_FHDI::~List_FHDI()

{

	delete[] _n_each_row_size ; 

}



//Fn===========================================================================

//List_string_FHDI.h-----------------------------------------------------------------------------

//Fn===========================================================================

//======================

//Compact LIST class declaration 

//to replace "R" list class

//

//

//Note: 1. only the given number of elements are stored in the compact storage

//      2. but the access can happen through the normal index (i,j)  like c++, starting from 0

//      3. all operation on an element outside the size of each list entity is null

//

// Last update: Oct 12, 2016

//

// by Dr. I. Cho 

// All rights reservd

//======================

class List_string_FHDI{



public:



	int size_row()   const {return _size_row;}  //size of total rows

	int size_block() const {return _v_block.size();} //size of total meaningful data stored in the block



	//----------------

	//initialize all private memory

	//----------------

	void initialize(int new_size_row);

	

    //======================

	//(i,j) operator overloading 

	//to access to the items of the array storage

	//Note: _v_block is one-dimensional vector for the compact LIST

	//      below operation overload of () is necessary to access the private _v_block 

	//======================
	//below appears to cause gcc-ASAN error. inactivated on April 23, 2018
	/*
	std::string & operator() (int i, int j) //ith list, jth entity (like c++,i.e. from 0)

	{

	

        std::string s_temp="";

		std::string& s_element = s_temp ;  //default 



		//================

		//(i,j) is for LIST entity: ith list and jth term 

		//so proper indexing is required 

		//================

		int i_size_of_list = _n_each_row_size[i] ; //get the size of the ith list row

        

		if(j< 0 || j >= i_size_of_list ) //out of width. 

		{

		    //NONE

		}

		else //within width

		{

			//get accumulated location of (i-1)th list row

			int i_sum = 0; for(int k=0; k<i; k++) {i_sum += _n_each_row_size[k];}

			return _v_block[i_sum + j] ;

		}

		

		return s_element;

	} 
    */	



    //========================

	//probably, only get the stored data

	//Note: return is a const double value 

	//========================

	std::string   operator() (int i, int j) const //like c++ rule, i.e., from 0 

	{

	

		std::string s_element=""; 



		//================

		//(i,j) is for LIST entity: ith list and jth term 

		//so proper indexing is required 

		//================

		int i_size_of_list = _n_each_row_size[i] ; //get the size of the ith list row

        

		if(j< 0 || j >= i_size_of_list ) //out of width. 

		{

			s_element = "";

		}

		else //within width

		{

			int i_sum = 0; for(int k=0; k<i; k++) {i_sum += _n_each_row_size[k];}

			//return _block[i*_size_col + (j - i_size_of_list)] ;   //this is working for class(i,j) = 111 or so.

			return _v_block[i_sum + j] ;

		}	

		



		return s_element;

	} 



	//==========================

	//	get all the stored non-null values from _v_block 

	//==========================

	void unlist(std::vector<std::string> & s_value);



	//==========================

	//put entire block into the storage _v_block

	//==========================

	void put_entire_block(std::vector<std::string> s_value);

	

    //==========================

	//get the stored _v_block at the i_row row of the list 

	//==========================

	void get_block(const int i_row, std::string s_value[]); 



    //==========================

	//put the new into _v_block's row i_row with n_size entities 

	//==========================

	void put_block(const int i_row, const int n_size, std::string s_value[]); 

    void put_block(const int i_row, std::vector<std::string> s_value);

	

	//==========================

	//get the stored _n_each_row_size at row i_row

	//==========================

	void get_a_row_size(const int i_row, int &i_value); 



	//==========================

	//put the new row size into storage

	//==========================

	void put_a_row_size(const int i_row, int i_value); 

	

	//=====================

	//print out List_string_FHDI

	//=====================

	void print_List_string_FHDI();	

	

	//=====================

	//print out ONE row of List_string_FHDI

	//=====================

	void print_one_List_string_FHDI(const int i_row);	

	

public:

	List_string_FHDI(int size_row); //constructor

	~List_string_FHDI();        //destructor





//================

//data members

//================

private:

	int _size_row; //total row of the current LIST

	std::vector<std::string> _v_block;

	int* _n_each_row_size ; //array for the size of each row of LIST

	

//================

//below is for avoiding possible error by an automatic task done by compiler

//In fact doing nothing as below makes it stable 

//================

private:

	List_string_FHDI(const List_string_FHDI &) ;

	const List_string_FHDI & operator = (const List_string_FHDI &) ;

};



//Fn===========================================================================

//List_string_FHDI.cc-----------------------------------------------------------------------------

//Fn===========================================================================

//======================

//Compact "String" LIST class declaration with the KNOWN ROW numbers

//to replace "R" list class

//

//

//Note: 1. only the given number of elements are stored in the compact storage

//      2. but the access can happen through the normal index (i,j)  like c++, starting from 0

//      3. all operation on an element outside the size of each list entity is null

//

// Last update: Nov 23, 2016

//

// by Dr. I. Cho 

// All rights reservd

//======================



//================

//implementation of List_string_FHDI class

//================

void List_string_FHDI::initialize(int new_size_row)

{

	_size_row = new_size_row; 

	

	_n_each_row_size = NULL;

	_n_each_row_size = new int[new_size_row];

	for(int i=0; i<new_size_row; i++) _n_each_row_size[i] = 0 ; 

	

	_v_block.clear(); //return this to size 0

}



void List_string_FHDI::unlist(std::vector<std::string> & s_value)

//Description==================================

//	get all the stored non-null strings from _v_block 

//  to s_value[]

//  like R's "unlist()" 

//

//IN   : int  i_row    = target row in the list

//OUT  : std::vector<std::string> s_value[n_size]  where n_size must be known before calling this fn. 

//

//Note: all variable preceded by '_' are private of Grid

//=============================================

{

	int n_size = _v_block.size();

		

	for(int i=0; i<n_size; i++) {s_value.push_back(_v_block[i]);} 



    return;

}



void List_string_FHDI::put_entire_block(std::vector<std::string> s_value)

//Description==================================

//	put the entire block into the storage _v_block 

//  from s_value[]

//

//IN   : std::vector<std::string> s_value[n_size]  where n_size must be known before calling this fn. 

//

//Note: all variable preceded by '_' are private of Grid

//=============================================

{

	int n_size = (int)s_value.size();

		

	for(int i=0; i<n_size; i++) {_v_block.push_back(s_value[i]);} 



    return;

}



void List_string_FHDI::get_block(const int i_row, std::string s_value[])

//Description==================================

//	get stored block at the i_row of the list 

//

//IN   : int  i_row    = target row in the list

//OUT  : std::string s_value[n_size]  where n_size must be known before calling this fn. 

//

//Note: all variable preceded by '_' are private of Grid

//=============================================

{

	int n_size = _n_each_row_size[i_row]; 

	

	//accumulated size of all the previous rows in the list

	int i_sum = 0; for(int k=0; k<i_row; k++) {i_sum += _n_each_row_size[k];}

	

	for(int i=0; i<n_size; i++) {s_value[i] = _v_block[i_sum + i];} 



    return;

}





void List_string_FHDI::put_block(const int i_row, const int n_size, std::string s_value[])

//Description==================================

//	put the new row into block

//  Note: 1. if current row i_row was not stored before, just append it by using push_back

//        2. if this row has been stored before, replacement takes place at the row

//

//IN  : int i_row  = target row number of the list (from 0 like c++ index)

//IN  : int n_size = ACTUAL size of the current row 

//IN  : std::string s_value[n_size] 

//

//Note: all variable preceded by '_' are private of Grid

//=============================================

{

	int n_existing_size = _n_each_row_size[i_row]; 

	//----------------

	//first time input

	//----------------

	if(n_existing_size == 0)

	{

		//----------------

		//store the new data into the _v_block 

		//----------------

		for(int i=0; i<n_size; i++) _v_block.push_back(s_value[i]);

		

		//-----------------

		//update the size of current row of list

		//-----------------

		_n_each_row_size[i_row] = n_size; 

			

	}

	//---------------

	//replace existing stored data

	//---------------

	if(n_existing_size > 0)

	{

		//accumulated size of all the previous rows in the list

		int i_sum = 0; for(int k=0; k<i_row; k++) {i_sum += _n_each_row_size[k];}

		

		for(int i=0; i<n_size; i++) {_v_block[i_sum + i] = s_value[i];} 

	}



    return;

}



void List_string_FHDI::put_block(const int i_row, std::vector<std::string> s_value)

//Description==================================

//	put the new row store in vector into block

//  Note: 1. if current row i_row was not stored before, just append it by using push_back

//        2. if this row has been stored before, replacement takes place at the row

//

//IN  : int i_row  = target row number of the list (from 0 like c++ index)

//IN  : std::vector<std::string> s_value[n_size] 

//

//Note: all variable preceded by '_' are private of Grid

//=============================================

{

	const int n_size = (int)s_value.size(); 

	

	int n_existing_size = _n_each_row_size[i_row]; 

	//----------------

	//first time input

	//----------------

	if(n_existing_size == 0)

	{

		//----------------

		//store the new data into the _v_block 

		//----------------

		for(int i=0; i<n_size; i++) _v_block.push_back(s_value[i]);

		

		//-----------------

		//update the size of current row of list

		//-----------------

		_n_each_row_size[i_row] = n_size; 

			

	}

	//---------------

	//replace existing stored data

	//---------------

	if(n_existing_size > 0)

	{

		//accumulated size of all the previous rows in the list

		int i_sum = 0; for(int k=0; k<i_row; k++) {i_sum += _n_each_row_size[k];}

		

		for(int i=0; i<n_size; i++) {_v_block[i_sum + i] = s_value[i];} 

	}



    return;

}





void List_string_FHDI::get_a_row_size(const int i_row, int & i_value)

//Description==================================

//	get stored _n_each_row_size of a row at i_row 

//

//IN   : int i_row 	= the row number of the list

//OUT  : int i_value 

//

//Note: all variable preceded by '_' are private of Grid

//=============================================

{

	i_value = _n_each_row_size[i_row]; 



    return;

}





void List_string_FHDI::put_a_row_size(const int i_row, int i_value)

//Description==================================

//	put the new size of the list into _n_each_row_size

//  

//

//IN  : int i_row   = the row number of the list

//IN  : int i_value 

//

//Note: all variable preceded by '_' are private of Grid

//=============================================

{

	_n_each_row_size[i_row] = i_value; 



    return;

}



//=====================

//print out List_string_FHDI

//=====================

void List_string_FHDI::print_List_string_FHDI()

{

	int n_row = (*this).size_row();

    std::string* s_temp;	

	for(int i=0; i<n_row; i++)

	{

		int i_temp = 0; (*this).get_a_row_size(i, i_temp);

		if(i_temp>0) //only for meaningful row 

		{

			

			s_temp = new std::string[i_temp];

			(*this).get_block(i, s_temp);

			FHDI::RPrint(i);

			FHDI::RPrint(s_temp, i_temp);

			

			delete[] s_temp; 

			

		}

	}

	return; 

}



//=====================

//print out ONE Row of List_string_FHDI

//=====================

void List_string_FHDI::print_one_List_string_FHDI(const int i_row)

{

	int n_row = (*this).size_row(); 

	std::string* s_temp; 

	if(i_row < n_row) 

	{

		int i = i_row; //target row number

		

		int i_temp = 0; (*this).get_a_row_size(i, i_temp);

		if(i_temp>0) //only for meaningful row 

		{

			s_temp = new std::string[i_temp];

			(*this).get_block(i, s_temp);

			FHDI::RPrint(i);

			FHDI::RPrint(s_temp, i_temp);

			

			delete[] s_temp; 

			 

		}

	}

	return; 

}

//=====================

//Constructor

//=====================

List_string_FHDI::List_string_FHDI(int size_row)

: _size_row(size_row), _n_each_row_size(new int[size_row])

{

	//=======

	//initialize with 0

	//=======

	for(int i=0; i<size_row; i++) _n_each_row_size[i] = 0 ; 



}



//=====================

//Destructor

//=====================

List_string_FHDI::~List_string_FHDI()

{

	delete[] _n_each_row_size ; 

}



//Fn===========================================================================

//rbind_FHDI.h-----------------------------------------------------------------------------

//Fn===========================================================================



//======================

//Compact row-based binding of matrix  

//to replace "R" rbind() function

//

//

//Note: 1. only the given number of elements are stored in the compact storage

//      2. but the access can happen through the normal index (i,j)  like c++, starting from 0

//      3. all operation on an element outside the size of each list entity is null

//

// Last update: Oct 27, 2016

//

// by Dr. Cho, I. 

// All rights reservd

//======================

class rbind_FHDI{



public:



	int size_col()   const {return _size_col;}  //size of total columns

	int size_row()   const {return (int)_v_block.size()/_size_col;} //total number of rows

	int size_block() const {return (int)_v_block.size();} //size of total meaningful data stored in the block



	//----------------

	//initialize all private memory

	//----------------

	void initialize(int new_size_col);

	

    //======================

	//(i,j) operator overloading 

	//to access to the items of the array storage

	//Note: _v_block is one-dimensional vector for the compact matrix of columns ncol

	//      below operation overload of () is necessary to access the private _v_block 

	//======================
    //below appears to cause gcc-ASAN error. inactivated on April 23, 2018
	/*
	double & operator() (int i, int j) //ith row, jth col (like c++,i.e. from 0)

	{

	

        double d_temp=0.0;

		double& d_element = d_temp ;  //default 



		//================

		//(i,j) is for an entity: ith row and jth column term 

		//so proper indexing is required 

		//================

		const int i_size_block = size_block(); //total stored values

        if(_size_col*i + 1 > i_size_block ) {return d_element;} //out of total range

		

		if(j< 0 || j >= _size_col ) //out of width. 

		{

		    //NONE

		}

		else //within width

		{

			return _v_block[i*_size_col + j] ;

		}

		

		return d_element;

	}       
    */


    //========================

	//probably, only get the stored data

	//Note: return is a const double value 

	//========================

	double   operator() (int i, int j) const //like c++ rule, i.e., from 0 

	{

	

		double d_element=0.0; 



		//================

		//(i,j) is for an entity: ith row and jth col term 

		//so proper indexing is required 

		//================

		const int i_size_block = size_block(); //total stored values

		if(_size_col*i + 1 > i_size_block ) {return d_element;} //out of total range

		

		if(j< 0 || j >= _size_col ) //out of width. 

		{

		    //NONE

		}

		else //within width

		{

			return _v_block[i*_size_col + j] ;

		}		

		

		return d_element;



	} 



	//==========================

	//	get all the stored non-null values from _v_block 

	//==========================

	void unlist(std::vector<double> & d_value);



	//==========================

	//put entire block into the storage _v_block

	//==========================

	void put_entire_block(std::vector<double> d_value);

	

	//==========================

	//append a row into storage. Column size is fixed 

	//==========================

	void append_block(double* d_value); 

	

    //==========================

	//get the stored _v_block at the i_row row of the matrix 

	//==========================

	void get_block(const int i_row, double* d_value); 



    //==========================

	//append the new matrix onto _v_block's end 

	//MUST have the same column size as _size_col

	//==========================

	void bind_blocks(const int n_row, const int n_col, double ** d_value);





    //==========================

	//return a matrix of the stored entire matrix from _v_block 

	//MUST have the same column size and row size as the stored

	//==========================

	void matrix_rbind(const int n_row, const int n_col, double ** d_value);

	

	//=====================

	//print out rbind_FHDI

	//=====================

	void print_rbind_FHDI();	

	

public:

	rbind_FHDI(int size_col); //constructor

	~rbind_FHDI();        //destructor





//================

//data members

//================

private:

	int _size_col; //fixed number of columns of the matrix

	std::vector<double> _v_block; //store many rows * _size_col data 

	

//================

//below is for avoiding possible error by an automatic task done by compiler

//In fact doing nothing as below makes it stable 

//================

private:

	rbind_FHDI(const rbind_FHDI &) ;

	const rbind_FHDI & operator = (const rbind_FHDI &) ;

};



//Fn===========================================================================

//rbind_FHDI.cc-----------------------------------------------------------------------------

//Fn===========================================================================

//======================

//Compact row-based binding of matrix  

//to replace "R" rbind() function

//

//

//Note: 1. only the given number of elements are stored in the compact storage

//      2. but the access can happen through the normal index (i,j)  like c++, starting from 0

//      3. all operation on an element outside the size of each list entity is null

//

// Last update: Oct 27, 2016

//

// by Dr. I. Cho 

// All rights reservd

//======================

//================

//implementation of rbind_FHDI class

//================

void rbind_FHDI::initialize(int new_size_col)

{

	_size_col = new_size_col; 

	_v_block.clear(); //return this to size 0

}



void rbind_FHDI::unlist(std::vector<double> & d_value)

//Description==================================

//	get all the stored non-null values from _v_block 

//  to d_value[]

//  like R's "unlist()" 

//

//OUT  : double d_value[n_size]  where n_size must be known before calling this fn. 

//

//Note: all variable preceded by '_' are private of Grid

//=============================================

{

	int n_size = (int)_v_block.size();

		

	for(int i=0; i<n_size; i++) {d_value.push_back(_v_block[i]);} 



    return;

}



void rbind_FHDI::put_entire_block(std::vector<double> d_value)

//Description==================================

//	put the entire block into the storage _v_block 

//  from d_value[]

//

//IN   : double d_value[n_size]  where n_size must be known before calling this fn. 

//

//Note: all variable preceded by '_' are private of Grid

//=============================================

{

	int n_size = (int)d_value.size();

		

	for(int i=0; i<n_size; i++) {_v_block.push_back(d_value[i]);} 



    return;

}



void rbind_FHDI::append_block(double* d_value)

//Description==================================

//	append the a row into the storage _v_block 

//  from d_value[]

//

//IN   : double d_value[n_col]  where n_col is the size_col must be known before calling this fn. 

//

//Note: all variable preceded by '_' are private of Grid

//=============================================

{

		

	for(int i=0; i<_size_col; i++) {_v_block.push_back(d_value[i]);} 



    return;

}



void rbind_FHDI::get_block(const int i_row, double* d_value)

//Description==================================

//	get stored block at the i_row of the matrix 

//

//IN   : int  i_row    = target row in the matrix [0,...]

//OUT  : double d_value[n_size]  where n_size must be known before calling this fn. 

//

//Note: all variable preceded by '_' are private of Grid

//=============================================

{

	

	for(int i=0; i< _size_col; i++) {d_value[i] = _v_block[i_row*_size_col + i];} 



    return;

}





void rbind_FHDI::bind_blocks(const int nrow, const int ncol, double ** d_value)

//Description==================================

//	append the new matrix into block

//

//IN  : double d_value[nrow, ncol=_size_col]

//

//Note: all variable preceded by '_' are private of Grid

//=============================================

{

	if(ncol != _size_col)

	{ Rprintf("Error! column does not match!"); return;}

    for(int i=0; i<nrow; i++)

	{

		for(int j=0; j<ncol; j++)

		{

			_v_block.push_back(d_value[i][j]);

		}

	}



    return;

}



void rbind_FHDI::matrix_rbind(const int nrow, const int ncol, double ** d_value)

//Description==================================

//	return the entire matrix from block

//

//OUT  : double d_value[nrow=stored rows, ncol=_size_col]

//

//Note: all variable preceded by '_' are private of Grid

//=============================================

{

	const int i_size_row = (*this).size_row(); //total number of rows

	const int i_size_col = (*this).size_col(); //total number of columns

	if(nrow != i_size_row){ Rprintf("Error! total rows do not match!"); return;}

	if(ncol != i_size_col){ Rprintf("Error! total columns do not match!"); return;}

	

    for(int i=0; i<nrow; i++)

	{

		for(int j=0; j<ncol; j++)

		{

			d_value[i][j] = _v_block[i*i_size_col + j];

		}

	}



    return;

}





//=====================

//print out rbind_FHDI

//=====================

void rbind_FHDI::print_rbind_FHDI()

{

	int n_row = (*this).size_row(); 

	int n_col = (*this).size_col();



	double* d_temp = new double[n_col];



	for(int i=0; i<n_row; i++)

	{

			(*this).get_block(i, d_temp);

			Rprintf("                "); 

			FHDI::RPrint(i);

			FHDI::RPrint(d_temp, n_col);

	}

	

	delete[] d_temp;

	return; 

}



//=====================

//Constructor

//=====================

rbind_FHDI::rbind_FHDI(int size_col)

: _size_col(size_col) 

{

//none 

}



//=====================

//Destructor

//=====================

rbind_FHDI::~rbind_FHDI()

{

	_v_block.clear(); 

}



//Fn===========================================================================

//categorize_cpp.cc-----------------------------------------------------------------------------

//Fn===========================================================================

namespace FHDI 

{

bool categorize_cpp(double** x, const int nrow, const int ncol, double* k, double** z, 
					int* NonCollapsible_categorical)

//Description=========================================

// categorize the data matrix x 

// according to the given number of categories stored in k(ncol)

//

// [Algorithm I] for continuous column (or variable):  
//
// perc: percentiles used to get quantiles, determined by k

// quan: quantiles if k=4, we quan=(Q1,Q2,Q3) have Q1(=1/4), Q2 (=Median) and Q3(=3/4)

// [Algorithm II] for categorical column (or variable):

// if a column consists of all integer values, automatically changed to categorical variable
// and also adjust the k[] 

// Note: as of Dec 2016, NA values (missing data) is marked by a long number at the parent "r" code

//                       the long number is 1234567899

// original R code: Dr. Im, J. and Dr. Kim, J. 

// c++ code: 		Dr. Cho, I. 

// All rights reserved

// 

// updated: April 9, 2018

//----------------------------------------------------

//IN	: double x(nrow, ncol) 	= {y1, y2, ... } total data containing missing values

//INOUT	: double k(ncol)		= a vector of categories of each column of xalloc

//OUT   : double z(nrow, ncol)  = categorized matrix corresponding to original matrix x

//                                initialized with 0.0 

//IN    : int NonCollapsible_categorical[ncol] = index vector of 0: collapsible; 1: Non-Collapsible
//====================================================

{
	//---------------------------------------
	//Automatically Identify Categorical Columns (Variables)
	//---------------------------------------
	//---------------------------------------
	//global storage for Categorical variable
	//original list of category values
	//Note: assuming the largest category is 35 as of April 9, 2018
	//---------------------------------------
	int** i_category_list_original = New_iMatrix(ncol, 35);
	Fill_iMatrix(i_category_list_original, ncol, 35, 0);

	//Index of each column. 0: continuous; [1, 35]: categorical 
	int* i_k_categorical = new int[ncol]; 
	for (int i = 0; i < ncol; i++) i_k_categorical[i] = 0;

	//----------------------
	//Loop for each column
	//----------------------
	double* d_x_one_column = new double[nrow]; //one column of [x]
	for (int i_col = 0; i_col < ncol; i_col++)
	{
		int i_integer_in_row = 0; //total integer counts of current column 
		bool b_categorical = 0; 

		//----
		//get this column
		//----
		for (int i = 0; i < nrow; i++) d_x_one_column[i] = x[i][i_col];

		//----
		//total observed cells in this column
		//----
		int i_total_observed_cells_this_column = 0; 
		for(int i=0; i<nrow; i++)
		{
			//only for the meaningful cell value of current column
			if(fabs_FHDI(d_x_one_column[i] - 1234567899) > 1e-5) 
			{
				i_total_observed_cells_this_column++; 
			}
		}		
		
		//----
		//check all values in this column are integer
		//----
		for (int i_row = 0; i_row < nrow; i_row++)
		{
			double d_x_one = d_x_one_column[i_row];
			double d_round = (double)round(d_x_one);

			//when current cell value is integer & observed cell only  
			if (fabs_FHDI(d_x_one - d_round) < 1E-10 &&
			    fabs_FHDI(d_x_one - 1234567899) > 1e-5)
			{
				i_integer_in_row++; 
			}
		}

		//---------
		//when all values are integer
		//---------
		if (i_integer_in_row == i_total_observed_cells_this_column)
		{
			b_categorical = 1; //this column may be categorical
		}

		//testout
		//cout << "i_col:" << i_col << " i_integer_in_row:" << i_integer_in_row << " b_categorical:" << b_categorical << endl;

		//--------
		//find how many categories are
		//--------
		if (b_categorical) //when this column is categorical
		{
			std::vector<double> v_table_value; 
			std::vector<int> v_table_count; 
			table_cpp(d_x_one_column, nrow,
				      v_table_value, v_table_count);

			int n_size_v_table = v_table_value.size(); //how many different categories
			
			//---
			//exception consideration when the missing cell is counted 
			//as a category in the table
			//---
			bool b_missing_cell_included = 0; 
			if(n_size_v_table>1)
			{   //when the last category turns out to be the NA
				for(int j=0; j<n_size_v_table; j++)
				{
					if(fabs_FHDI(v_table_value[j] - 1234567899) < 1e-5) 
					{
						b_missing_cell_included = 1; 
					}
				}
			}
			if(b_missing_cell_included) n_size_v_table = n_size_v_table - 1;  
			
			//if the categories are less than 35 ---------
			if (n_size_v_table >= 1 && n_size_v_table < 36)
			{
				i_k_categorical[i_col] = n_size_v_table; //how many categories 
			}
			//if the categories are larger than 35 ---------
			//considered as continuous as of April 9th, 2018
			if (n_size_v_table > 35)
			{
				i_k_categorical[i_col] = 0; //0 means continuous 
				n_size_v_table = 0; //reset to zero 
			}

			//testout<<
			/*
			cout << "n_size_v_table :" << n_size_v_table << endl;
			cout << "v_table_value[] :" <<  endl;
			for (int i = 0; i < n_size_v_table; i++) cout << v_table_value[i] << " ";
			cout << endl;
			cout << "v_table_count[] :" << endl;
			for (int i = 0; i < n_size_v_table; i++) cout << v_table_count[i] << " ";
			cout << endl;
			*/

			//-------------
			//store the current column's original category values 
			//which may be not consecutive 
			//-------------
			if (i_k_categorical[i_col] >= 1 && i_k_categorical[i_col] < 36)
			{
				for (int j = 0; j < n_size_v_table; j++)
				{
					i_category_list_original[i_col][j] = static_cast<int>(v_table_value[j]); 
				}
			}
			//clear vector container
			v_table_value.clear(); 
			v_table_count.clear(); 

		}

	}

	//delete local array
	delete[] d_x_one_column;

	//testout
	//RPrint("  i_k_categorical[]: ");
	//RPrint(i_k_categorical, ncol); 
	//RPrint("  i_category_list_original[][]: ");
	//RPrint(i_category_list_original, ncol, 10); 
	
	/*
	cout << "column,   i_k_categorical[]" << endl;
	for (int i = 0; i < ncol; i++)
	{
		cout << i + 1 << "  ,  " << i_k_categorical[i] << endl;
	}
	cout << endl;
	
	//testout
	cout << "column, i_category_list_original[i_col][1:35]" << endl;
	for(int i = 0; i < ncol; i++)
	{
		cout << i + 1<<"  :";
		for (int j = 0; j < 10; j++) cout << i_category_list_original[i][j] << "  ";
		cout << endl;
	}
	cout << endl;
	*/
	
	
	//-------------------------
	//Override original k[] when there are categorical columns
	//-------------------------
	for(int i_col=0; i_col<ncol; i_col++)
	{ 
		//-----------------------
		//When this column is NON-collapsible
		//----------------------
		if(NonCollapsible_categorical[i_col] == 1)
		{
			if(i_k_categorical[i_col] >= 1 && i_k_categorical[i_col] <36)
			{
				k[i_col] = static_cast<double>(i_k_categorical[i_col]); 
				RPrint("Note: Non-collapsible categorical variables are identified, and their {k} may be replaced with actual total categories \n");  
			}
		}
		//-----------------------
		//When this column is COLLAPSIBLE
		//----------------------
		if(NonCollapsible_categorical[i_col] == 0)
		{
			i_k_categorical[i_col] = 0; //nullify the categorical type 
										//and do not touch user-defined k[]
		}
		
	}
	

	double* x_one_column      = new double[nrow]; Fill_dVector(x_one_column, nrow, 0.0);

	double* x_one_column_temp = new double[nrow]; Fill_dVector(x_one_column_temp, nrow, 0.0);

	
	

	for(int i_col=0; i_col<ncol; i_col++)
	{

		for(int i=0; i<nrow; i++) x_one_column[i] = x[i][i_col]; //get one column

		
		
		//------------------------------
		//Algorithm II: Categorical Variable (column)
		//------------------------------
		if(i_k_categorical[i_col] >= 1 && i_k_categorical[i_col] <36) //if this column is categorical
		{
			for(int i=0; i<nrow; i++)
			{
				bool b_update_z = 1; 
				
				//only for the meaningful cell value of current column
				if(fabs_FHDI(x_one_column[i] - 1234567899) > 1e-5) 
				{
					for(int i_m=0; i_m < i_k_categorical[i_col]; i_m++) // as of April 2018, maximum categories = 35
					{
						if(fabs_FHDI(x_one_column[i] - i_category_list_original[i_col][i_m]) < 1e-5) 
						{
							if(b_update_z) z[i][i_col] =  (i_m + 1)*1.0; //Actual Category Number. Stored as double 
							b_update_z = 0; //move to next row 
							
							//testout
							//RPrint(" x_one_column[i]:"); RPrint(x_one_column[i]); 
							//RPrint(" i_category_list_original[i_col][i_m]:"); 
							//RPrint(i_category_list_original[i_col][i_m]); 
						}
					}
				}
			}
		}			
		
		//------------------------------
		//Algorithm I: Continuous Variable (column)
		//------------------------------
		if(i_k_categorical[i_col] == 0 ) //if this column is Continuous
		{

			//----------------

			// omit Not Available (NA) values in each column of x

			//----------------

			int i_temp = 0; 

			for(int i=0; i<nrow; i++) 

			{

				if(fabs_FHDI(x_one_column[i] - 1234567899) > 1e-5) 

				//if(   !std::isnan(x_one_column[i])   ) //non-NA value only	

				{	

					x_one_column_temp[i_temp] = x_one_column[i];

					i_temp++;

				}

			}

			

			//-----------------

			//make percentile except for 1.0

			//-----------------

			int k_one_column = (int)k[i_col];

			if(fabs_FHDI(k_one_column)<=1.0) 

			{RPrint("Error! in categorize_cpp, k_one_column is <=1.0!   "); return 0;} //error check

			double* perc = new double[k_one_column-1]; Fill_dVector(perc, (k_one_column-1), 0.0);



			for(int i=0; i<(k_one_column-1); i++)

			{

				perc[i] = (i+1)*(1.0/k_one_column);

			}

			

			//------------------

			//quantile generation

			//the same as Type 7 (default in R)

			//------------------

			int n_observed = i_temp; //actual size of non-NA data in current column of x

			if(n_observed <= nrow)

			//{ std::sort(&x_one_column_temp[0], &x_one_column_temp[n_observed]); }

			{ std::sort(x_one_column_temp, x_one_column_temp+n_observed); }	

											//Note: sort happens in [begin, end)

											//Note: use <algorithm> of c++ library. formation: sort(*begin, *end)

			if(n_observed > nrow)  //error case 

			{ Rprintf("Error! n_observed > nrow in categorize()   "); return 0; }

					

			

			

			//Note: the last quantile (i.e. 100%) is not included, and thus (k_one_column-1) is used

			double* x_quantile = new double[k_one_column-1]; Fill_dVector(x_quantile, (k_one_column-1), 0.0);

			

			for(int i=0; i<(k_one_column-1); i++)

			{

				double d_h = (n_observed-1)*perc[i] ; //+1 is removed for c++ code 

				x_quantile[i] = x_one_column_temp[int(floor(d_h))] 

								+  (d_h-floor(d_h))*(  x_one_column_temp[int(floor(d_h)+1)]

													 - x_one_column_temp[int(floor(d_h))]   );

			}

			

			//---------------

			//assign z with category values

			// Note: categories = {1, 2, ...} 

			//---------------

			for(int i=0; i<nrow; i++)

			{

				if(fabs_FHDI(x_one_column[i] - 1234567899) > 1e-5) //non-NA value only

				//if(   !std::isnan(x_one_column[i])   ) //non-NA value only

				{

					//---------

					//default category of non-NA unit is 1 as of 0124_2017

					//---------

					z[i][i_col] = 1; //default 

					

					//----------

					//consider each quantile

					//----------

					if(x_one_column[i] < x_quantile[0]){ z[i][i_col] = 1;} //1st category

					if(x_one_column[i] > x_quantile[k_one_column-2]){ z[i][i_col] = k_one_column;} //last category



					for(int j=1; j<(k_one_column-1); j++)

					{

						if(x_quantile[j-1] < x_one_column[i] && x_one_column[i] <= x_quantile[j])

						{

							z[i][i_col] = j+1 ; //(j+1)th quantile. Note: j =[0,k_one_column) 

							break; 

						}

					}

				}
			}

			

			//--------------

			//local Deallocation

			//--------------

			delete[] perc; 

			delete[] x_quantile;	
		} //end of continuous variable (column)			

	}


	//--------------------
	//Error check 
	//for All Zero Row 
	//2019, 06 10
	//--------------------
	for(int i_row=0; i_row<nrow; i_row++)
	{
		double d_sum_temp = 0.0; 
		 
		for(int i_col=0; i_col<ncol; i_col++)	
		{
			d_sum_temp += z[i_row][i_col]; 
		}
		
		if(fabs_FHDI(d_sum_temp) < 1E-15)
		{
			Rprintf("Error! The given data set has row(s) having all missing cells !!!   "); 

			//Rprintf("%d ", i_row + 1);

			return 0;
		}
	}

	//--------------------

	//Deallocation

	//--------------------
	Del_iMatrix(i_category_list_original, ncol, 35);
	
	delete[] i_k_categorical; 

	delete[] x_one_column;      

	delete[] x_one_column_temp; 



	return 1;

}

void categorize_cpp_beforeApril9_2018(double** x, const int nrow, const int ncol, double* k, double** z)

//Description=========================================

// categorize the data matrix x 

// according to the given number of categories stored in k(ncol)

//

// Algorithm:  

// perc: percentiles used to get quantiles, determined by k

// quan: quantiles if k=4, we quan=(Q1,Q2,Q3) have Q1(=1/4), Q2 (=Median) and Q3(=3/4)

// 

// Note: as of Dec 2016, NA values (missing data) is marked by a long number at the parent "r" code

//                       the long number is 1234567899

// original R code: Dr. Im, J. and Dr. Kim, J. 

// c++ code: 		Dr. Cho, I. 

// All rights reserved

// 

// updated: Oct 6, 2016

//----------------------------------------------------

//IN	: double x(nrow, ncol) 	= {y1, y2, ... } total data containing missing values

//IN	: double k(ncol)		= a vector of categories of each column of xalloc

//OUT   : double z(nrow, ncol)  = catorized matrix corresponding to original matrix x

//                                initialized with 0.0 

//====================================================

{

	double* x_one_column      = new double[nrow]; Fill_dVector(x_one_column, nrow, 0.0);

	double* x_one_column_temp = new double[nrow]; Fill_dVector(x_one_column_temp, nrow, 0.0);

	

	

	for(int i_col=0; i_col<ncol; i_col++)

	{

		for(int i=0; i<nrow; i++) x_one_column[i] = x[i][i_col]; //get one column

		

		//----------------

		// omit Not Available (NA) values in each column of x

		//----------------

		int i_temp = 0; 

		for(int i=0; i<nrow; i++) 

		{

			if(fabs_FHDI(x_one_column[i] - 1234567899) > 1e-5) 

			//if(   !std::isnan(x_one_column[i])   ) //non-NA value only	

			{	

				x_one_column_temp[i_temp] = x_one_column[i];

				i_temp++;

			}

		}

		

		//-----------------

		//make percentile except for 1.0

		//-----------------

		int k_one_column = (int)k[i_col];

		if(fabs_FHDI(k_one_column)<=1.0) 

		{Rprintf("Error! in categorize_cpp, k_one_column is <=1.0!"); return;} //error check

		double* perc = new double[k_one_column-1]; Fill_dVector(perc, (k_one_column-1), 0.0);



		for(int i=0; i<(k_one_column-1); i++)

		{

			perc[i] = (i+1)*(1.0/k_one_column);

		}

		

		//------------------

		//quantile generation

		//the same as Type 7 (default in R)

		//------------------

		int n_observed = i_temp; //actual size of non-NA data in current column of x

        if(n_observed <= nrow)

		//{ std::sort(&x_one_column_temp[0], &x_one_column_temp[n_observed]); }

		{ std::sort(x_one_column_temp, x_one_column_temp+n_observed); }	

										//Note: sort happens in [begin, end)

										//Note: use <algorithm> of c++ library. formation: sort(*begin, *end)

		if(n_observed > nrow)  //error case 

		{ Rprintf("Error! n_observed > nrow in categorize()"); return; }

				

		

		

		//Note: the last quantile (i.e. 100%) is not included, and thus (k_one_column-1) is used

		double* x_quantile = new double[k_one_column-1]; Fill_dVector(x_quantile, (k_one_column-1), 0.0);

		

		for(int i=0; i<(k_one_column-1); i++)

		{

			double d_h = (n_observed-1)*perc[i] ; //+1 is removed for c++ code 

			x_quantile[i] = x_one_column_temp[int(floor(d_h))] 

							+  (d_h-floor(d_h))*(  x_one_column_temp[int(floor(d_h)+1)]

							                     - x_one_column_temp[int(floor(d_h))]   );

		}

		

		//---------------

		//assign z with category values

		// Note: categories = {1, 2, ...} 

		//---------------

		for(int i=0; i<nrow; i++)

		{

			if(fabs_FHDI(x_one_column[i] - 1234567899) > 1e-5) //non-NA value only

			//if(   !std::isnan(x_one_column[i])   ) //non-NA value only

			{

				//---------

				//default category of non-NA unit is 1 as of 0124_2017

				//---------

				z[i][i_col] = 1; //default 

				

				//----------

				//consider each quantile

				//----------

				if(x_one_column[i] < x_quantile[0]){ z[i][i_col] = 1;} //1st category

				if(x_one_column[i] > x_quantile[k_one_column-2]){ z[i][i_col] = k_one_column;} //last category



				for(int j=1; j<(k_one_column-1); j++)

				{

					if(x_quantile[j-1] < x_one_column[i] && x_one_column[i] <= x_quantile[j])

					{

						z[i][i_col] = j+1 ; //(j+1)th quantile. Note: j =[0,k_one_column) 

						break; 

					}

				}

			}

		}

		

		//--------------

		//local Deallocation

		//--------------

		delete[] perc; 

		delete[] x_quantile;		

	}



	//--------------------

	//Deallocation

	//--------------------

	delete[] x_one_column;      

	delete[] x_one_column_temp; 



	return;

}





//=========================================================

//=========================================================

//=========================================================

//=========================================================

//=========================================================

bool categorize_cpp(double* x, const int nrow,  double &k, double* z, 
					const int NonCollapsible_categorical_1)

//Description=========================================

// categorize a data ARRAY x 

// according to the given number of category stored in k

//

// Algorithm:  

// perc: percentiles used to get quantiles, determined by k

// quan: quantiles if k=4, we quan=(Q1,Q2,Q3) have Q1(=1/4), Q2 (=Median) and Q3(=3/4)

// 

// Note: as of Oct 2016, NA values (missing data) is marked by 1234567899 at the parent "r" code

//

// original R code: Dr. Im, J. and Dr. Kim, J. 

// c++ code: 		Dr. Cho, I. 

// All rights reserved

// 

// updated: Oct 6, 2016

//----------------------------------------------------

//IN	: double x(nrow) 	= a column data containing missing values

//INOUT	: double k   		= a number of category of the column. May be automatically changed for categorical variable

//OUT   : double z(nrow)    = catorized array corresponding to original array x

//                                initialized with 0.0 

//IN    : int NonCollapsible_categorical_1 = 0: when this column is collapsible; 1: Non-collapsible 
//====================================================

{
	
	//testout
	//RPrint("previoius k :"); RPrint(k); 
	
	//---------------------------------------
	//Automatically Identify Categorical Columns (Variables)
	//---------------------------------------
	//---------------------------------------
	//global storage for Categorical variable
	//original list of category values
	//Note: assuming the largest category is 35 as of April 9, 2018
	//---------------------------------------
	//const int ncol = 1; //for this function only 
	//int** i_category_list_original = New_iMatrix(ncol, 35);
	//Fill_iMatrix(i_category_list_original, ncol, 35, 0);
	int* i_category_list_original = new int[35];
	Fill_iVector(i_category_list_original, 35, 0);

	//Index of each column. 0: continuous; [1, 35]: categorical 
	//int* i_k_categorical = new int[ncol]; 
	//for (int i = 0; i < ncol; i++) i_k_categorical[i] = 0;
	int i_k_categorical = 0; 

	//----------------------
	//Loop for each column
	//----------------------
	double* d_x_one_column = new double[nrow]; //one column of [x]
	//for (int i_col = 0; i_col < ncol; i_col++)
	//{
		int i_integer_in_row = 0; //total integer counts of current column 
		bool b_categorical = 0; 

		//----
		//get this column
		//----
		//for (int i = 0; i < nrow; i++) d_x_one_column[i] = x[i][i_col];
		for (int i = 0; i < nrow; i++) d_x_one_column[i] = x[i]; //for this function only 

		//----
		//total observed cells in this column
		//----
		int i_total_observed_cells_this_column = 0; 
		for(int i=0; i<nrow; i++)
		{
			//only for the meaningful cell value of current column
			if(fabs_FHDI(d_x_one_column[i] - 1234567899) > 1e-5) 
			{
				i_total_observed_cells_this_column++; 
			}
		}		
		
		//----
		//check all values in this column are integer
		//----
		for (int i_row = 0; i_row < nrow; i_row++)
		{
			double d_x_one = d_x_one_column[i_row];
			double d_round = (double)round(d_x_one);

			//when current cell value is integer & observed cell only 
			if (fabs_FHDI(d_x_one - d_round) < 1E-10 &&
			    fabs_FHDI(d_x_one - 1234567899) > 1e-5)
			{
				i_integer_in_row++; 
			}
		}

		//---------
		//when all values are integer
		//---------
		if (i_integer_in_row == i_total_observed_cells_this_column)
		{
			b_categorical = 1; //this column may be categorical
		}

		//testout
		//cout << "i_col:" << i_col << " i_integer_in_row:" << i_integer_in_row << " b_categorical:" << b_categorical << endl;

		//--------
		//find how many categories are
		//--------
		if (b_categorical) //when this column is categorical
		{
			std::vector<double> v_table_value; 
			std::vector<int> v_table_count; 
			table_cpp(d_x_one_column, nrow,
				      v_table_value, v_table_count);

			int n_size_v_table = v_table_value.size(); //how many different categories
			
			//---
			//exception consideration when the missing cell is counted 
			//as a category in the table
			//---
			bool b_missing_cell_included = 0; 
			if(n_size_v_table>1)
			{   //when the last category turns out to be the NA
				for(int j=0; j<n_size_v_table; j++)
				{
					if(fabs_FHDI(v_table_value[j] - 1234567899) < 1e-5) 
					{
						b_missing_cell_included = 1; 
					}
				}
			}
			if(b_missing_cell_included) n_size_v_table = n_size_v_table - 1;  
			
			//if the categories are less than 35 ---------
			if (n_size_v_table > 0 && n_size_v_table < 36)
			{
				//i_k_categorical[i_col] = n_size_v_table; //how many categories 
				i_k_categorical = n_size_v_table; //how many categories 
			}
			//if the categories are larger than 35 ---------
			//considered as continuous as of April 9th, 2018
			if (n_size_v_table > 35)
			{
				//i_k_categorical[i_col] = 0; //0 means continuous 
				i_k_categorical = 0; //0 means continuous 
				n_size_v_table = 0; //reset to zero 
			}

			//testout<<
			/*
			cout << "n_size_v_table :" << n_size_v_table << endl;
			cout << "v_table_value[] :" <<  endl;
			for (int i = 0; i < n_size_v_table; i++) cout << v_table_value[i] << " ";
			cout << endl;
			cout << "v_table_count[] :" << endl;
			for (int i = 0; i < n_size_v_table; i++) cout << v_table_count[i] << " ";
			cout << endl;
			*/

			//-------------
			//store the current column's original category values 
			//which may be not consecutive 
			//-------------
			//if (i_k_categorical[i_col] >= 1 && i_k_categorical[i_col] < 36)
			if (i_k_categorical >= 1 && i_k_categorical < 36)	
			{
				for (int j = 0; j < n_size_v_table; j++)
				{
					//i_category_list_original[i_col][j] = static_cast<int>(v_table_value[j]); 
					i_category_list_original[j] = static_cast<int>(v_table_value[j]); 
					
				}
			}
			//clear vector container
			v_table_value.clear(); 
			v_table_count.clear(); 

		}

	//} //loop for columns inactivated for this function only 

	//delete local array
	delete[] d_x_one_column;

	//testout
	//RPrint("i_k_categorical :"); RPrint(i_k_categorical); 
	//RPrint("i_category_list_original []:"); RPrint(i_category_list_original, 35); 
	
	/*
	cout << "column,   i_k_categorical[]" << endl;
	for (int i = 0; i < ncol; i++)
	{
		cout << i + 1 << "  ,  " << i_k_categorical[i] << endl;
	}
	cout << endl;
	
	//testout
	cout << "column, i_category_list_original[i_col][1:35]" << endl;
	for(int i = 0; i < ncol; i++)
	{
		cout << i + 1<<"  :";
		for (int j = 0; j < 10; j++) cout << i_category_list_original[i][j] << "  ";
		cout << endl;
	}
	cout << endl;
	*/
	
	
	//-------------------------
	//Override original k[] when there are categorical columns
	//-------------------------
	//const int i_col = 0 ;
	//if(i_k_categorical[i_col] >= 1 && i_k_categorical[i_col] <36)
	
	//-------
	//when this column is NON-collapsible
	//-------
	if(NonCollapsible_categorical_1	== 1)
	{
		if(i_k_categorical >= 1 && i_k_categorical <36)		
		{
			//k[i_col] = static_cast<double>(i_k_categorical[i_col]); 
			k = static_cast<double>(i_k_categorical); 
			//Rprintf("Note! Some categorical columns are automatically identified and {k} may be replaced! \n");  
		}
	}
	//-------
	//when this column is Collapsible
	//-------	
	if(NonCollapsible_categorical_1	== 0)
	{
		i_k_categorical = 0; //nullify the categorical type
							 //and do not touch user-defined k
	}
	
	
	
	//below is for matrix version 
	/*
	for(int i_col=0; i_col<ncol; i_col++)
	{ 
		if(i_k_categorical[i_col] >= 1 && i_k_categorical[i_col] <36)
		{
			k[i_col] = static_cast<double>(i_k_categorical[i_col]); 
			Rprintf("Caution! some categorical columns are automatically identified and {k} may be replaced!");  
		}
	}
    */	
	
	//testout
	//RPrint("maybe new k :"); RPrint(k); 

	
	double* x_one_column      = new double[nrow]; Fill_dVector(x_one_column, nrow, 0.0);

	double* x_one_column_temp = new double[nrow]; Fill_dVector(x_one_column_temp, nrow, 0.0);

	

	for(int i=0; i<nrow; i++) x_one_column[i] = x[i]; //get the one column

	//testout
	//RPrint("previous x[]:"); RPrint(x_one_column, nrow); 

	
	//------------------------------
	//Algorithm II: Categorical Variable (column)
	//------------------------------
	//if(i_k_categorical[i_col] >= 1 && i_k_categorical[i_col] <36) //if this column is categorical
	if(i_k_categorical >= 1 && i_k_categorical <36) //if this column is categorical
	{
		for(int i=0; i<nrow; i++)
		{
			bool b_update_z = 1 ; 
			//only for the meaningful cell value of current column
			if(fabs_FHDI(x_one_column[i] - 1234567899) > 1e-5) 
			{
				//for(int i_m=0; i_m < i_k_categorical[i_col]; i_m++) // as of April 2018, maximum categories = 35
				for(int i_m=0; i_m < i_k_categorical; i_m++) // as of April 2018, maximum categories = 35
				{
					//if(fabs_FHDI(x_one_column[i] - i_category_list_original[i_col][i_m]) <1e-5)					
					if(fabs_FHDI(x_one_column[i] - i_category_list_original[i_m]) <1e-5)					
					{
						//if(b_update_z) z[i][i_col] = (i_m + 1); //Actual Category Number 
						if(b_update_z) z[i] = static_cast<double>(i_m + 1); //Actual Category Number //for this function only
						b_update_z = 0; //move to next row 
					}
				}
			}
		}
	}			
		
	//------------------------------
	//Algorithm I: Continuous Variable (column)
	//------------------------------
	//if(i_k_categorical[i_col] == 0 ) //if this column is Continuous	
	if(i_k_categorical == 0 ) //if this column is Continuous	
	{
		
		//----------------

		// omit Not Available (NA) values in each column of x

		//----------------

		int i_temp = 0; 

		for(int i=0; i<nrow; i++) 

		{

			if(fabs_FHDI(x_one_column[i] - 1234567899) > 1e-5) 

			//if(  !std::isnan(x_one_column[i])  ) 	

			{	

				x_one_column_temp[i_temp] = x_one_column[i];

				i_temp++;

			}

		}

		

		//-----------------

		//make percentile except for 1.0

		//-----------------

		int k_one_column = (int)k;

		if(fabs_FHDI(k_one_column)<=1.0) {Rprintf("Error! in categorize_cpp, k_one_column is <=1.0!"); return 0;} //error check



		double* perc = new double[k_one_column-1]; Fill_dVector(perc, (k_one_column-1), 0.0);



		for(int i=0; i<(k_one_column-1); i++)

		{

			perc[i] = (i+1)*(1.0/k_one_column);

		}

		

		//------------------

		//quantile generation

		//the same as Type 7 (default in R)

		//------------------

		int n_observed = i_temp; //actual size of non-NA data in current column of x

        if(n_observed <= nrow)

		//{ std::sort(&x_one_column_temp[0], &x_one_column_temp[n_observed]); }

		{ std::sort(x_one_column_temp, x_one_column_temp+n_observed); }	

										//Note: sort happens in [begin, end)

										//Note: use <algorithm> of c++ library. formation: sort(*begin, *end)

		if(n_observed > nrow)  //error case 

		{ Rprintf("Error! n_observed > nrow in categorize()"); return 0; }

		//Note: the last quantile (i.e. 100%) is not included, and thus (k_one_column-1) is used

		double* x_quantile = new double[k_one_column-1]; Fill_dVector(x_quantile, (k_one_column-1), 0.0);

		

		for(int i=0; i<(k_one_column-1); i++)

		{

			double d_h = (n_observed-1)*perc[i] ; //+1 is removed for c++ code 

			x_quantile[i] = x_one_column_temp[int(floor(d_h))] 

							+  (d_h-floor(d_h))*(  x_one_column_temp[int(floor(d_h)+1)]

							                     - x_one_column_temp[int(floor(d_h))]   );

		}

		

		//---------------

		//assign z with category values

		// Note: categories = {1, 2, ...} 

		//---------------

		for(int i=0; i<nrow; i++)

		{
			//----------
			//Avoid error by updating NA z with non-zero value during cell collapse 
			//----------
			z[i] = 0 ; //for general default  

			
			if(fabs_FHDI(x_one_column[i] - 1234567899) > 1e-5) //non-NA value only

			//if(   !std::isnan(x_one_column[i])   ) //non-NA value only

			{


				//default category of non-NA unit is 1 as of 0124_2017

				//---------

				z[i] = 1; //default 

				

				if(x_one_column[i] < x_quantile[0]){ z[i] = 1;} //1st category

				if(x_one_column[i] > x_quantile[k_one_column-2]){ z[i] = k_one_column;} //last category



				for(int j=1; j<(k_one_column-1); j++)

				{

					if(x_quantile[j-1] < x_one_column[i] && x_one_column[i] <= x_quantile[j])

					{

						z[i] = j+1 ; //(j+1)th quantile. Note: j =[0,k_one_column) 

						break; 

					}

				}

			}
			
		}

	

		//--------------

		//local Deallocation

		//--------------

		delete[] perc; 

		delete[] x_quantile;		
	} //end of continuous variable 
	
	
	//--------------------

	//Deallocation

	//--------------------
	//Del_iMatrix(i_category_list_original, ncol, 35);
	//delete[] i_k_categorical;  
	delete[] i_category_list_original;
	
	delete[] x_one_column;      

	delete[] x_one_column_temp; 



	return 1;

}





//=========================================================

//=========================================================

//=========================================================

//=========================================================

//=========================================================

void categorize_cpp_BeforeApril09_2018(double* x, const int nrow, const double k, double* z)

//Description=========================================

// categorize a data ARRAY x 

// according to the given number of category stored in k

//

// Algorithm:  

// perc: percentiles used to get quantiles, determined by k

// quan: quantiles if k=4, we quan=(Q1,Q2,Q3) have Q1(=1/4), Q2 (=Median) and Q3(=3/4)

// 

// Note: as of Oct 2016, NA values (missing data) is marked by 1234567899 at the parent "r" code

//

// original R code: Dr. Im, J. and Dr. Kim, J. 

// c++ code: 		Dr. Cho, I. 

// All rights reserved

// 

// updated: Oct 6, 2016

//----------------------------------------------------

//IN	: double x(nrow) 	= a column data containing missing values

//IN	: double k   		= a number of category of the column 

//OUT   : double z(nrow)    = catorized array corresponding to original array x

//                                initialized with 0.0 

//====================================================

{

		double* x_one_column      = new double[nrow]; Fill_dVector(x_one_column, nrow, 0.0);

		double* x_one_column_temp = new double[nrow]; Fill_dVector(x_one_column_temp, nrow, 0.0);

	

		for(int i=0; i<nrow; i++) x_one_column[i] = x[i]; //get the one column

		

		//----------------

		// omit Not Available (NA) values in each column of x

		//----------------

		int i_temp = 0; 

		for(int i=0; i<nrow; i++) 

		{

			if(fabs_FHDI(x_one_column[i] - 1234567899) > 1e-5) 

			//if(  !std::isnan(x_one_column[i])  ) 	

			{	

				x_one_column_temp[i_temp] = x_one_column[i];

				i_temp++;

			}

		}

		

		//-----------------

		//make percentile except for 1.0

		//-----------------

		int k_one_column = (int)k;

		if(fabs_FHDI(k_one_column)<=1.0) {Rprintf("Error! in categorize_cpp, k_one_column is <=1.0!"); return;} //error check



		double* perc = new double[k_one_column-1]; Fill_dVector(perc, (k_one_column-1), 0.0);



		for(int i=0; i<(k_one_column-1); i++)

		{

			perc[i] = (i+1)*(1.0/k_one_column);

		}

		

		//------------------

		//quantile generation

		//the same as Type 7 (default in R)

		//------------------

		int n_observed = i_temp; //actual size of non-NA data in current column of x

        if(n_observed <= nrow)

		//{ std::sort(&x_one_column_temp[0], &x_one_column_temp[n_observed]); }

		{ std::sort(x_one_column_temp, x_one_column_temp+n_observed); }	

										//Note: sort happens in [begin, end)

										//Note: use <algorithm> of c++ library. formation: sort(*begin, *end)

		if(n_observed > nrow)  //error case 

		{ Rprintf("Error! n_observed > nrow in categorize()"); return; }

		//Note: the last quantile (i.e. 100%) is not included, and thus (k_one_column-1) is used

		double* x_quantile = new double[k_one_column-1]; Fill_dVector(x_quantile, (k_one_column-1), 0.0);

		

		for(int i=0; i<(k_one_column-1); i++)

		{

			double d_h = (n_observed-1)*perc[i] ; //+1 is removed for c++ code 

			x_quantile[i] = x_one_column_temp[int(floor(d_h))] 

							+  (d_h-floor(d_h))*(  x_one_column_temp[int(floor(d_h)+1)]

							                     - x_one_column_temp[int(floor(d_h))]   );

		}

		

		//---------------

		//assign z with category values

		// Note: categories = {1, 2, ...} 

		//---------------

		for(int i=0; i<nrow; i++)

		{

			if(fabs_FHDI(x_one_column[i] - 1234567899) > 1e-5) //non-NA value only

			//if(   !std::isnan(x_one_column[i])   ) //non-NA value only

			{

				//---------

				//default category of non-NA unit is 1 as of 0124_2017

				//---------

				z[i] = 1; //default 

				

				if(x_one_column[i] < x_quantile[0]){ z[i] = 1;} //1st category

				if(x_one_column[i] > x_quantile[k_one_column-2]){ z[i] = k_one_column;} //last category



				for(int j=1; j<(k_one_column-1); j++)

				{

					if(x_quantile[j-1] < x_one_column[i] && x_one_column[i] <= x_quantile[j])

					{

						z[i] = j+1 ; //(j+1)th quantile. Note: j =[0,k_one_column) 

						break; 

					}

				}

			}

		}

	

		//--------------

		//local Deallocation

		//--------------

		delete[] perc; 

		delete[] x_quantile;		

	//--------------------

	//Deallocation

	//--------------------

	delete[] x_one_column;      

	delete[] x_one_column_temp; 



	return;

}

} //end of namespace






//Fn===========================================================================

//Zmat_Extension_cpp.cc-----------------------------------------------------------------------------

//Fn===========================================================================

namespace FHDI

{



void Zmat_Extension_cpp(double** z, const int nrow, const int ncol, std::string cn[], 

				   int* ml, int* ol, int& i_count_ol, int& i_count_ml, 

				   double** uox, double** mox, int &i_count_uox, int &i_count_mox,

				   const bool b_DEBUG)

//Description=========================================

// make the condensed expression of z

//

// Algorithm:  each row of z will be concatenated as a single string consisting of 35 characters

// 

// Note: as of Oct 2016, NA values (missing data) is marked by a long integer at the parent "r" code

//

// original R code: Dr. Im, J. and Dr. Kim, J. 

// c++ code: 		Dr. Cho, I. 

// All rights reserved

// 

// updated: March 28, 2017

//----------------------------------------------------

//IN   	: double z(nrow, ncol)  = categorized matrix corresponding to original matrix x

//OUT	: string cn(nrow)		= vector of string to represent each row of the caterorized z          

//OUT	: int ml(nrow)			= actual location of rows containing AT LEAST ONE missing cells

//OUT	: int ol(nrow)			= actual location of rows containing ONLY observed cells  

//OUT   : int i_count_ol		= total number of ol rows that containing observed cells

//OUT   : int i_count_ml		= total number of ml rows that containing missing cells 

//OUT   : double uox(nrow, ncol)= sorted unique categorized patterns of observed cells. up to i_count_uox rows are meaningful 

//OUT   : double mox(nrow, ncol)= sorted unique categorized patterns of missing  cells. up to i_count_mox rows are meaningful                           

//OUT   : int i_count_uox		= total number of uox rows that containing meaningful cells

//OUT   : int i_count_mox		= total number of mox rows that containing meaningful cells

//====================================================

{

	//--------------

	//make a condensed expression "cn" of z

	//--------------

	Trans(z, nrow, ncol, cn);


	//testout
	//RPrint("inside Zmat_Extension_cpp");
	//RPrint("  z[][]: in Zmat_Extension_cpp \n"); RPrint(z, nrow, ncol);
	//RPrint("  cn[]: "); RPrint(cn, nrow);
	
	

	//--------------

	//locations of missing cells (ml) and observed cells (ol)

	//--------------

	Fill_iVector(ml, nrow, 0); Fill_iVector(ol, nrow, 0); //Initialization

	double d_temp=0.0; 

	int i_ol_temp = 0; 

	int i_ml_temp = 0;

	for(int i_row=0; i_row<nrow; i_row++)

	{

		d_temp=1.0; 

		for(int i_col=0; i_col<ncol; i_col++)

		{

			if(z[i_row][i_col] == 0) {d_temp=0.0; break;} //found zero, i.e. missing cell

		}

		

		if(fabs_FHDI(d_temp) > 1e-15 ) //this row has no missing cells

		{ol[i_ol_temp] = i_row + 1; i_ol_temp++;} //actual number of the row having no missing cells

		

		if(fabs_FHDI(d_temp) < 1e-15) //this row has AT LEAST one missing cells

		{ml[i_ml_temp] = i_row + 1; i_ml_temp++;}  //actual number of the row having missing cells

	}

	

	if(i_ol_temp ==0) {Rprintf("Error! no observed unit"); return; }


	

	i_count_ol = i_ol_temp; //update the actual value 

	i_count_ml = i_ml_temp; //update the actual value

	

	//---------------------

	//make UNIQUE patterns of z by cn

	//i.e., uox and mox

	//---------------------

	//step . Sort the "cn" in the ascending order 

	//---------------------

	//std::string s_ol[i_ol_temp]; //string vector of observed patterns only

	//std::string s_ml[i_ml_temp]; //string vector of missing patterns only

	std::string *s_ol = new std::string[i_ol_temp]; //string vector of observed patterns only

	std::string *s_ml = new std::string[i_ml_temp]; //string vector of missing patterns only	

	for(int i=0; i<i_ol_temp; i++) {s_ol[i] = cn[ol[i]-1];} //"-1" since ol contains actual row number

	for(int i=0; i<i_ml_temp; i++) {s_ml[i] = cn[ml[i]-1];} //"-1" since ml contains actual row number	

		

	std::sort(s_ol, s_ol+i_ol_temp); //knowing that s_ol[] has i_ol_temp entities

	std::sort(s_ml, s_ml+i_ml_temp); //knowing that s_ml[] has i_ml_temp entities

	

	//------------

	//memorize observed patterns 

	//------------

	i_count_uox = 0; //total number of unique uox 

	std::string s_temp ; 

	for(int i=0; i<i_ol_temp; i++)

	{

		s_temp = s_ol[i]; //get a string 

		for(int j=0; j<nrow; j++) //search all rows 

		{

			//----

			//below condition is needed for finding UNIQUE pattern

			//----

			//if(j==0 && s_temp == cn[j]) 

			//if(i==0 && s_temp == cn[j]) //with first string, find the same string in cn 

			if(i==0 && s_temp.compare(cn[j]) == 0) //0: equal string

			{

				for(int k=0; k<ncol; k++) 

				{uox[i_count_uox][k] = z[j][k]; } //store the found observed pattern

				i_count_uox++; 

				break; 

			}

			//if(j>0 && s_temp == cn[j] && s_temp != cn[j-1])

			//if(i>0 && s_temp == cn[j] && s_temp != s_ol[i-1]) //find UNIQUE matching 

			if(i>0 && s_temp.compare(cn[j]) == 0 && s_temp.compare(s_ol[i-1]) != 0) 

			{

				for(int k=0; k<ncol; k++) 

				{uox[i_count_uox][k] = z[j][k]; } //store the found observed pattern				

				i_count_uox++; 

				break; 

			}

		}

	}

	//Now, i_count_uox means the total number of unique observed patterns



	//------------

	//memorize missing patterns 

	//------------

	i_count_mox = 0; //total number of unique mox 

	 

	for(int i=0; i<i_ml_temp; i++)

	{

		s_temp = s_ml[i]; //get a string 

		for(int j=0; j<nrow; j++) //search all rows 

		{

			//----

			//below condition is needed for finding unique pattern

			//----

			//if(j==0 && s_temp == cn[j]) 

			//if(i==0 && s_temp == cn[j]) //with first string, find matching string in cn

			if(i==0 && s_temp.compare(cn[j]) == 0 ) //0: equal string 

			{

				for(int k=0; k<ncol; k++) 

				{mox[i_count_mox][k] = z[j][k]; } //store the found missing pattern

				i_count_mox++; 

				break; 

			}

			//if(j>0 && s_temp == cn[j] && s_temp != cn[j-1])

			//if(i>0 && s_temp == cn[j] && s_temp != s_ml[i-1]) //find UNIQUE matching string

			if(i>0 && s_temp.compare(cn[j]) == 0 && s_temp.compare(s_ml[i-1]) != 0) //0: equal

			{

				for(int k=0; k<ncol; k++) 

				{mox[i_count_mox][k] = z[j][k]; } //store the found missing pattern				

				i_count_mox++; 

				break;

			}

		}

	}

	//Now, i_count_mox means the total number of unique missing patterns

	

	//----------------

	//additional check for unique observed and missing patterns

	//----------------

	//observed patterns//

	d_temp = 0.0; 

	double** uox_final = New_dMatrix(nrow, ncol); 

	for(int j=0; j<ncol; j++) {uox_final[0][j] = uox[0][j]; } //first row initialization

	int i_count_uox_final = 1; //starting from the second row



	for(int i=1; i<i_count_uox; i++) //starting from the second one

	{

		d_temp = 0.0; //initialize 

		for(int j=0; j<ncol; j++) {d_temp += fabs_FHDI(uox[i][j] - uox[i-1][j]) ;} //difference of adjacent rows

		

		if(d_temp > 1e-3) //adjacent rows are NOT the same each other

		{

			for(int j=0; j<ncol; j++) {uox_final[i_count_uox_final][j] = uox[i][j];} 

			i_count_uox_final++; 

		}

	}

	i_count_uox = i_count_uox_final; //replace with the accurate value

	Copy_dMatrix(uox_final, nrow, ncol, uox);

    

	//missing patterns//

	double** mox_final = New_dMatrix(nrow, ncol); 

	for(int j=0; j<ncol; j++) {mox_final[0][j] = mox[0][j]; } //first row initialization

	int i_count_mox_final = 1; //starting from the second row



	for(int i=1; i<i_count_mox; i++) //starting from the second one

	{

		d_temp = 0.0; //initialize

		for(int j=0; j<ncol; j++) {d_temp += fabs_FHDI(mox[i][j] - mox[i-1][j]) ;} //difference of adjacent rows

		

		if(d_temp > 1e-3) //adjacent rows are NOT the same each other

		{

			for(int j=0; j<ncol; j++) {mox_final[i_count_mox_final][j] = mox[i][j];} 

			i_count_mox_final++; 

		}

	}

	i_count_mox = i_count_mox_final; //replace with the accurate value

	Copy_dMatrix(mox_final, nrow, ncol, mox); 

	

	//------------------

	//Deallocation

	//------------------

	Del_dMatrix(uox_final, nrow, ncol); 

	Del_dMatrix(mox_final, nrow, ncol); 

	delete[] s_ol; 

	delete[] s_ml;

	

	return;

	

}



} //end of namespace



//Fn===========================================================================

//nDAU_cpp.cc-----------------------------------------------------------------------------

//Fn===========================================================================

namespace FHDI{



	bool nDAU_cpp(double** uox, double** mox, const int nrow_uox, const int nrow_mox, const int ncol,

		std::string cn[], int* ol, const int nrow_ol, int i_cellmake,

		std::vector<int> &v_nD, List_FHDI &List_nU, int* tnU,

		bool b_DEBUG)

		//Description=========================================

		// identify information of the missing cells and observed cells

		//

		// Algorithm:  

		// 

		//

		// original R code: Dr. Im, J. and Dr. Kim, J. 

		// c++ code: 		Dr. Cho, I. and Yicheng Yang

		// All rights reserved

		// 

		// updated: Aug 9, 2020

		//----------------------------------------------------

		//IN    : double uox(nrow_uox, ncol)= sorted unique patterns of observed cells. up to i_count_uox rows are meaningful 

		//IN    : double mox(nrow_mox, ncol)= sorted unique patterns of missing  cells. up to i_count_mox rows are meaningful                           

		//IN 	: string cn(nrow)		= vector of string to represent each row of z          

		//IN	: int ol(nrow_ol)		= actual location of rows containing ONLY observed cells    

		//INOUT : std::vector v_nD		= total number of donnors of each missing pattern

		//OUT   : List_FHDI List_nU     = list of observed cells to serve as donors 

		//OUT   : int tnU[nrow_uox]		= table format of the total numbers of donors for each missing rows

		//====================================================

	{

		//initialize 	

		double* snr2 = new double[nrow_mox];

		for (int i = 0; i<nrow_mox; i++) { snr2[i] = i + 1; } //actual row number of mox



															  //------------

															  //make a table of unique strings in cn

															  //------------

		std::vector<std::string> v_table_item_cn;

		std::vector<int>	     v_table_count_cn;



		//observed patterns only

		//std::string s_cn_ol[nrow_ol]; 

		std::string *s_cn_ol = new std::string[nrow_ol];

		for (int i = 0; i<nrow_ol; i++) { s_cn_ol[i] = cn[ol[i] - 1]; } //"-1" for actual location number of row

		std::sort(s_cn_ol, s_cn_ol + nrow_ol); //sort the observed patterns 



		table_cpp(s_cn_ol, nrow_ol,

			v_table_item_cn, v_table_count_cn);





		//-----------

		//null string of width of ncol

		//-----------

		std::string s_zval; for (int i = 0; i<ncol; i++) s_zval.append("0");



		//-------------------

		//Loop for mox (missing cells patterns) rows

		//-------------------

		double* d_temp = new double[ncol];

		int* ia_temp = new int[ncol]; //temporary integer array  



		std::string s_cn0; //temporary string for a row of mox

						   //-------------------------------------

						   //example

						   // mox[i,] 		13 0 1

						   // mxl 			1    3		//location of observed cells in mox

						   // nxl			2         	//number of observed cells in mox

						   // rcn0			d1			//condensed string of the observed cells in mox (from 35 letters, 1-9 and a-z)

						   // cand 		"11" "29" "d1" "b3" "d1" ... 	//condensed strings of uox corresponding to the observed cell columns

						   // oloc			3    5     	//location of cand that has the same as rcn0

						   //-------------------------------------

						   //initialize 



		for (int i = 0; i<nrow_mox; i++)

		{

			for (int j = 0; j<ncol; j++) d_temp[j] = mox[i][j];  //ith row of mox



			Trans1(d_temp, ncol, s_cn0); //condense a row to string 





										 //---------

										 //when current missing row is null string, i.e. "   "

										 //---------

			if (s_cn0.compare(s_zval) == 0) //0: equal string

			{

				//---------------------

				//number of donors; this case all observed cells are possible donors

				//---------------------

				int i_nD_sum = 0;

				for (unsigned k = 0; k<v_table_count_cn.size(); k++) i_nD_sum += v_table_count_cn[k];

				v_nD.push_back(i_nD_sum); //store the number of possible donors into the integer vector to return



										  //------

										  //store a row of nU into the List storage

										  //for this null string row, all observed rows become possible donors

										  //------	

				double* d_nU_temp = new double[nrow_uox];

				for (int k = 0; k<nrow_uox; k++) d_nU_temp[k] = k + 1;

				List_nU.put_block(i, nrow_uox, d_nU_temp);

				delete[] d_nU_temp;





			}



			//----------

			//for general cases for missing units, other than null string

			//----------

			if (s_cn0.compare(s_zval) != 0) //0: equal string

			{

				//-----

				//find non zero cells of current missing row mox

				//-----

				for (int k = 0; k<ncol; k++) ia_temp[k] = (int)mox[i][k];



				//Note: below will contain ACTUAL location of cells with non-zero observed data

				std::vector<int> v_mxl; //temporary vector for the locaiton of non zeros in mox

				whichINV(ia_temp, ncol, 0, v_mxl); //get the location of Non-zero in mox 



												   //-----

												   //total number of non zeros in mox

												   //-----

				const int nxl = v_mxl.size(); //total number of observed units at current row (>0)





				double* d_rcn0_temp = new double[nxl]; //temporary

				for (int k = 0; k<nxl; k++) d_rcn0_temp[k] = mox[i][v_mxl[k] - 1]; //"-1" is for actual location



																				   //-----

																				   //condense non-zero category names in current row of mox

																				   //-----

				std::string s_rcn0;

				Trans1(d_rcn0_temp, nxl, s_rcn0); //condense one row  


												  //deallocate used array or matrix (2018_0416)
				delete[] d_rcn0_temp;




				//--------------

				//make a list of possible donors 

				//--------------

				std::vector<int> v_oloc;

				if (nxl >= 1) //unlike R code, below algorithm suffices for all cases 

				{

					//----

					//get all non-zero cells from all observed rows

					//----

					double * d_t1 = new double[v_mxl.size()];

					std::vector<std::string> v_cand; //vector of found string with condensed non-zero observed data

													 //-----

													 //search all observed rows

													 //of which the same columns are non-zero as the current missing row  

													 //-----

					for (int m = 0; m<nrow_uox; m++)

					{	//Note: "-1" in v_mxl is from the ACTUAL location info in it

						for (unsigned k = 0; k<v_mxl.size(); k++) d_t1[k] = uox[m][v_mxl[k] - 1];



						//------

						//condense the found rows with non-zero observed cell only  

						//------

						std::string s_cand_1;

						Trans1(d_t1, v_mxl.size(), s_cand_1);

						v_cand.push_back(s_cand_1); //add more string to the string vector 

					}



					//--------------

					//Find the rows of v_cand that match the current non-zero missing pattern

					//Note: below will contain ACTUAL locations of the found rows 

					//--------------

					which(v_cand, s_rcn0, v_oloc); //get the locations of observed cells containing s_rcn0 



												   //------------

												   //local deallocation

												   //------------

					delete[] d_t1;

				}



				//----------------

				//Store oloc into LIST named nU 

				// ith row of nU corresponds to ith row of the List

				//----------------

				int i_oloc_temp = (int)v_oloc.size(); //size of current oloc

				if (i_oloc_temp>0) //only for meaningful oloc

				{

					double* d_oloc_temp = new double[i_oloc_temp];

					for (int k = 0; k<i_oloc_temp; k++) d_oloc_temp[k] = v_oloc[k];

					List_nU.put_block(i, i_oloc_temp, d_oloc_temp); //store ith missing row's donors list into the block  

					delete[] d_oloc_temp;

				}

				//---------------------

				//number of donors; this case only the matched observed rows become possible donors

				//---------------------

				int i_temp_tocn_sum = 0;

				for (int k = 0; k<i_oloc_temp; k++) //accumulate all possible donors 

				{
					i_temp_tocn_sum += v_table_count_cn[v_oloc[k] - 1];
				} //-1 for actual loc

				v_nD.push_back(i_temp_tocn_sum); //store into integer vector to return 





			} //end of general missing case, other than null string row case

		} //end of the main loop for i of all missing patterns  



		  //----------------------------

		  //make a table-like information of nU

		  //example:

		  // List_nU

		  // 0:  3,5,7

		  // 1:  1,10

		  // 2:  23,1,3, 5

		  // then,

		  // d_v_nU_unlist_temp: 3,5,7,1,10, 23,1,3,5,...

		  // v_table_item_List_nU ; v_table_count_List_nU

		  // 1                      2 

		  // 3                      2 

		  // 5                      2 

		  // 7                      1 

		  // 10                     1 

		  // 23                     1  	

		  //finally, 

		  //tnU

		  // 2, 2, 2, 1, 1

		  //----------------------------

		std::vector<double> v_nU_unlist;

		List_nU.unlist(v_nU_unlist); //get the list of all entities of List_nU

		int i_size_v_nU_unlist = (int)v_nU_unlist.size();





		//----

		//Error check

		//-----

		if ((i_size_v_nU_unlist <= 0) && (i_cellmake == 2))

		{

			//Rprint("No possible donors with current k. Retry with reduced k \n");

			Rprintf("Causion!!! No possible donors with current k in nDAU_cpp. \n");
			//return 0;

		}

		if ((i_size_v_nU_unlist <= 0) && (i_cellmake == 1))

		{

			//Rprint("No possible donors with current k. Retry with reduced k \n");

			Rprintf("ERROR!!! No possible donors with current k in nDAU_cpp. Retry with reduced k. \n");

			//exit(0);

			return 0;

		}

		double* d_v_nU_unlist_temp = new double[i_size_v_nU_unlist];

		for (int k = 0; k<i_size_v_nU_unlist; k++) d_v_nU_unlist_temp[k] = v_nU_unlist[k]; //a copy of all donors (row numbers) 



		std::vector<double> v_table_item_List_nU; //names of List_nU

		std::vector<int>	v_table_count_List_nU;//counts of List_nU



		table_cpp(d_v_nU_unlist_temp, i_size_v_nU_unlist,

			v_table_item_List_nU, v_table_count_List_nU);

		//testout

		if (b_DEBUG)

		{
			Rprintf("table_cpp has been done ");
		}



		Fill_iVector(tnU, nrow_uox, 0);

		for (int i = 0; i<nrow_uox; i++)

		{

			//-----

			//error check

			//sometimes tnU size is less than v_table_item_List_nU 

			//-----

			if (i >= (int)v_table_item_List_nU.size()) { break; }



			//-----

			//search meaningful locations to be stored into tnU 

			//-----

			double d_t2 = v_table_item_List_nU[i]; //Note: Actual number is stored!

												   //if(std::isnan(d_t2)==1) {break;} //Exit at the end of the table list. only for meaningful number 
			if (isnan_FHDI(d_t2) == 1) { break; } //Exit at the end of the table list. only for meaningful number 


			for (int j = 1; j<nrow_uox + 1; j++) //"+1" is needed for Actual # stored

			{

				if (fabs_FHDI(d_t2 - j) < 1e-15) { tnU[i] = v_table_count_List_nU[i]; break; }

			}

		}









		//-----------------

		//Deallocation

		//-----------------

		delete[] s_cn_ol;

		delete[] snr2;

		delete[] d_temp;

		delete[] ia_temp;

		delete[] d_v_nU_unlist_temp;

		//delete[] tnU;

		return 1;

	}



} //end of namespace



  //Fn===========================================================================

  //nDAU_Bigp_cpp.cc-----------------------------------------------------------------------------

  //Fn===========================================================================

namespace FHDI {
	//#include <math.h>	
	//===========================================================================
	//===========================================================================
	//===========================================================================
	//===========================================================================
	bool nDAU_Bigp_cpp(double** uox, double** mox, const int nrow_uox, const int nrow_mox, const int ncol, const int i_option_collapsing, const int i_option_SIS_type,

		std::string cn[], int* ol, const int nrow_ol, const int top, int i_cellmake,

		std::vector<int> &v_nD, List_FHDI &List_nU, int* tnU, int** codes, int** correlation_ranking_top, double** ol_matrix,

		bool b_DEBUG)

		//Description=========================================

		// identify information of the missing cells and observed cells

		//

		// Algorithm:  

		// 

		//

		// original R code: Dr. Im, J. and Dr. Kim, J. 

		// c++ code: 		Dr. Cho, I. and Yicheng Yang

		// All rights reserved

		// 

		// updated: Aug 11, 2020

		//----------------------------------------------------

		//IN    : double uox(nrow_uox, ncol)= sorted unique patterns of observed cells. up to i_count_uox rows are meaningful 

		//IN    : double mox(nrow_mox, ncol)= sorted unique patterns of missing  cells. up to i_count_mox rows are meaningful                           

		//IN 	: string cn(nrow)		= vector of string to represent each row of z          

		//IN	: int ol(nrow_ol)		= actual location of rows containing ONLY observed cells    

		//IN    : int i_option_collapsing = choice of big-p algorithm 

		//                            0= no big-p algorithms

		//                           !0= perform big-p algorithms

		//IN    : int i_option_SIS_type = 1: SIS with intersection 

		//                           2: SIS with union 

		//                           3: SIS with global ranking 

		//INOUT : std::vector v_nD		= total number of donnors of each missing pattern

		//OUT   : List_FHDI List_nU     = list of observed cells to serve as donors 

		//OUT   : int tnU[nrow_uox]		= table format of the total numbers of donors for each missing rows

		//OUT   : int codes(nrow, i_option_collapsing); // storage to record most correlated variables of mox

		//====================================================

	{

		//initialize 	

		double* snr2 = new double[nrow_mox];

		for (int i = 0; i<nrow_mox; i++) { snr2[i] = i + 1; } //actual row number of mox



															  //------------

															  //make a table of unique strings in cn

															  //------------

		std::vector<std::string> v_table_item_cn;

		std::vector<int>	     v_table_count_cn;



		//observed patterns only

		//std::string s_cn_ol[nrow_ol]; 

		std::string *s_cn_ol = new std::string[nrow_ol];

		for (int i = 0; i<nrow_ol; i++) { s_cn_ol[i] = cn[ol[i] - 1]; } //"-1" for actual location number of row

		std::sort(s_cn_ol, s_cn_ol + nrow_ol); //sort the observed patterns 



		table_cpp(s_cn_ol, nrow_ol,

			v_table_item_cn, v_table_count_cn);





		//-----------

		//null string of width of ncol

		//-----------

		std::string s_zval; for (int i = 0; i<ncol; i++) s_zval.append("0");



		//-------------------

		//Loop for mox (missing cells patterns) rows

		//-------------------

		double* d_temp = new double[ncol];

		int* ia_temp = new int[ncol]; //temporary integer array  



		std::string s_cn0; //temporary string for a row of mox

						   //-------------------------------------

						   //example

						   // mox[i,] 		13 0 1

						   // mxl 			1    3		//location of observed cells in mox

						   // nxl			2         	//number of observed cells in mox

						   // rcn0			d1			//condensed string of the observed cells in mox (from 35 letters, 1-9 and a-z)

						   // cand 		"11" "29" "d1" "b3" "d1" ... 	//condensed strings of uox corresponding to the observed cell columns

						   // oloc			3    5     	//location of cand that has the same as rcn0

						   //-------------------------------------

						   //initialize 


		for (int i = 0; i<nrow_mox; i++)

		{

			for (int j = 0; j<ncol; j++) d_temp[j] = mox[i][j];  //ith row of mox


			Trans1(d_temp, ncol, s_cn0); //condense a row to string 





										 //---------

										 //when current missing row is null string, i.e. "   "

										 //---------

			if (s_cn0.compare(s_zval) == 0) //0: equal string

			{

				//---------------------

				//number of donors; this case all observed cells are possible donors

				//---------------------

				int i_nD_sum = 0;

				for (unsigned k = 0; k<v_table_count_cn.size(); k++) i_nD_sum += v_table_count_cn[k];

				v_nD.push_back(i_nD_sum); //store the number of possible donors into the integer vector to return



										  //------

										  //store a row of nU into the List storage

										  //for this null string row, all observed rows become possible donors

										  //------	

				double* d_nU_temp = new double[nrow_uox];

				for (int k = 0; k<nrow_uox; k++) d_nU_temp[k] = k + 1;

				List_nU.put_block(i, nrow_uox, d_nU_temp);

				delete[] d_nU_temp;





			}



			//----------

			//for general cases for missing units, other than null string

			//----------

			if (s_cn0.compare(s_zval) != 0) {

				int oc = 0; // Get number of observed values in current mox

				for (int k = 0; k < ncol; k++) {

					ia_temp[k] = (int)mox[i][k];

					if (ia_temp[k] > 0) {

						oc++;
					}

				}


				//Note: below will contain ACTUAL location of missing cells

				std::vector<int> v_mxl;

				if (oc < (i_option_collapsing + 1)) { // oc <= i_option_collapsing

					whichINV(ia_temp, ncol, 0, v_mxl); //get the location of Non-zero in mox 
				}

				if (oc > i_option_collapsing) {

					if (i_option_SIS_type == 1) {

						//FHDI::correlated_variable_intersection(ncol, i_option_collapsing, i, ia_temp, correlation_yicheng, correlation_ranking, v_mxl, TestOut);
						FHDI::correlated_variable_intersection2(ncol, i_option_collapsing, top, i, nrow_ol, ia_temp, ol_matrix, correlation_ranking_top, v_mxl);
					}

					if (i_option_SIS_type == 2) {

						//FHDI::correlated_variable_union(ncol, i_option_collapsing, i, ia_temp, correlation_yicheng, correlation_ranking, v_mxl, TestOut);
						FHDI::correlated_variable_union2(ncol, i_option_collapsing, top, i, nrow_ol, ia_temp, ol_matrix, correlation_ranking_top, v_mxl);
					}

					if (i_option_SIS_type == 3) {

						//FHDI::correlated_variable_global(ncol, i_option_collapsing, ia_temp, correlation_yicheng, v_mxl, TestOut);
						FHDI::correlated_variable_global2(ncol, i_option_collapsing, nrow_ol, ia_temp, ol_matrix, v_mxl);
					}

				}

				int v_mxl_size = v_mxl.size();

				for (int b3 = 0; b3 < v_mxl_size; b3++) {
					codes[i][b3] = v_mxl[b3];
				}

				const int nxl = v_mxl.size(); //total number of the most correlated units at current row (>0)

				double* d_rcn0_temp = new double[nxl]; //temporary

				for (int k = 0; k<nxl; k++) d_rcn0_temp[k] = mox[i][v_mxl[k] - 1]; //"-1" is for actual location



																				   //-----

																				   //condense non-zero category names in current row of mox

																				   //-----

				std::string s_rcn0;

				Trans1(d_rcn0_temp, nxl, s_rcn0); //condense one row  


												  //deallocate used array or matrix (2018_0416)
				delete[] d_rcn0_temp;


				//--------------

				//make a list of possible donors 

				//--------------

				std::vector<int> v_oloc;

				if (nxl >= 1) //unlike R code, below algorithm suffices for all cases 

				{

					//----

					//get all non-zero cells from all observed rows

					//----

					double * d_t1 = new double[v_mxl.size()];

					std::vector<std::string> v_cand; //vector of found string with condensed non-zero observed data

													 //-----

													 //search all observed rows

													 //of which the same columns are non-zero as the current missing row  

													 //-----

					for (int m = 0; m<nrow_uox; m++)

					{	//Note: "-1" in v_mxl is from the ACTUAL location info in it

						for (unsigned k = 0; k<v_mxl.size(); k++) d_t1[k] = uox[m][v_mxl[k] - 1];



						//------

						//condense the found rows with non-zero observed cell only  

						//------

						std::string s_cand_1;

						Trans1(d_t1, v_mxl.size(), s_cand_1);

						v_cand.push_back(s_cand_1); //add more string to the string vector 

					}



					//--------------

					//Find the rows of v_cand that match the current non-zero missing pattern

					//Note: below will contain ACTUAL locations of the found rows 

					//--------------

					which(v_cand, s_rcn0, v_oloc); //get the locations of observed cells containing s_rcn0 



												   //------------

												   //local deallocation

												   //------------

					delete[] d_t1;

				}


				//----------------

				//Store oloc into LIST named nU 

				// ith row of nU corresponds to ith row of the List

				//----------------

				int i_oloc_temp = (int)v_oloc.size(); //size of current oloc

				if (i_oloc_temp>0) //only for meaningful oloc

				{

					double* d_oloc_temp = new double[i_oloc_temp];

					for (int k = 0; k<i_oloc_temp; k++) d_oloc_temp[k] = v_oloc[k];

					List_nU.put_block(i, i_oloc_temp, d_oloc_temp); //store ith missing row's donors list into the block  

					delete[] d_oloc_temp;

				}

				//---------------------

				//number of donors; this case only the matched observed rows become possible donors

				//---------------------

				int i_temp_tocn_sum = 0;

				for (int k = 0; k<i_oloc_temp; k++) //accumulate all possible donors 

				{
					i_temp_tocn_sum += v_table_count_cn[v_oloc[k] - 1];
				} //-1 for actual loc

				v_nD.push_back(i_temp_tocn_sum); //store into integer vector to return 


			} //end of general missing case, other than null string row case

		} //end of the main loop for i of all missing patterns  



		  //----------------------------

		  //make a table-like information of nU

		  //example:

		  // List_nU

		  // 0:  3,5,7

		  // 1:  1,10

		  // 2:  23,1,3, 5

		  // then,

		  // d_v_nU_unlist_temp: 3,5,7,1,10, 23,1,3,5,...

		  // v_table_item_List_nU ; v_table_count_List_nU

		  // 1                      2 

		  // 3                      2 

		  // 5                      2 

		  // 7                      1 

		  // 10                     1 

		  // 23                     1  	

		  //finally, 

		  //tnU

		  // 2, 2, 2, 1, 1

		  //----------------------------
		  //TestOut << "List_nU: " << endl;
		  //List_nU.print_List_FHDI();

		std::vector<double> v_nU_unlist;

		List_nU.unlist(v_nU_unlist); //get the list of all entities of List_nU

		int i_size_v_nU_unlist = (int)v_nU_unlist.size();





		//----

		//Error check

		//-----

		if ((i_size_v_nU_unlist <= 0) && (i_cellmake == 2))

		{

			//Rprint("No possible donors with current k. Retry with reduced k \n");

			Rprintf("Causion!!! No possible donors with current k in nDAU_cpp. \n");
			//return 0;

		}

		if ((i_size_v_nU_unlist <= 0) && (i_cellmake == 1))

		{

			//Rprint("No possible donors with current k. Retry with reduced k \n");

			Rprintf("ERROR!!! No possible donors with current k in nDAU_cpp. Retry with reduced k. \n");

			//exit(0);
			return 0;

		}



		double* d_v_nU_unlist_temp = new double[i_size_v_nU_unlist];

		for (int k = 0; k<i_size_v_nU_unlist; k++) d_v_nU_unlist_temp[k] = v_nU_unlist[k]; //a copy of all donors (row numbers) 



		std::vector<double> v_table_item_List_nU; //names of List_nU

		std::vector<int>	v_table_count_List_nU;//counts of List_nU



		table_cpp(d_v_nU_unlist_temp, i_size_v_nU_unlist,

			v_table_item_List_nU, v_table_count_List_nU);

		//testout

		if (b_DEBUG)

		{
			Rprintf("table_cpp has been done ");
		}



		Fill_iVector(tnU, nrow_uox, 0);

		for (int i = 0; i<nrow_uox; i++)

		{

			//-----

			//error check

			//sometimes tnU size is less than v_table_item_List_nU 

			//-----

			if (i >= (int)v_table_item_List_nU.size()) { break; }



			//-----

			//search meaningful locations to be stored into tnU 

			//-----

			double d_t2 = v_table_item_List_nU[i]; //Note: Actual number is stored!

												   //if(std::isnan(d_t2)==1) {break;} //Exit at the end of the table list. only for meaningful number 
			if (isnan_FHDI(d_t2) == 1) { break; } //Exit at the end of the table list. only for meaningful number 


			for (int j = 1; j<nrow_uox + 1; j++) //"+1" is needed for Actual # stored

			{

				if (fabs_FHDI(d_t2 - j) < 1e-15) { tnU[i] = v_table_count_List_nU[i]; break; }

			}

		}







		//-----------------

		//Deallocation

		//-----------------

		delete[] s_cn_ol;

		delete[] snr2;

		delete[] d_temp;

		delete[] ia_temp;

		delete[] d_v_nU_unlist_temp;

		//delete[] tnU;

		return 1;

	}


} //end of namespace


//Fn===========================================================================

//Merge_Extension_cpp.cc-----------------------------------------------------------------------------

//Fn===========================================================================

namespace FHDI{



void Merge_Extension_cpp(const int i_reci, double** uox, const int nrow_uox, 

					     double** mox, const int nrow_mox, int* tnU, 

						 std::string cn[],  int* ol, const int nrow_ol,

						 double** z, const int nrow, const int ncol, 

						 const int i_merge, 

						 const bool b_DEBUG)

//Description=========================================

// Merge the categorized matrix z 

//

// Algorithm: 											

//   For a given missing row at i_reci                 e.g., {12, NA, 4}           

//   Step 1: find columns having the observed cells:   e.g., 1st and 3rd columns   

//   Step 2: over all rows, search other observed at the 1st and 3rd columns 

//   Step 3: calculate relative distance, sum(|a-b|^2) 

//   Step 4: among rows that have the shortest distance, randomly select donors 

//   Step 5: fill the missing cell with the selected donors 

//

//

// original R code: Dr. Im, J. and Dr. Kim, J. 

// c++ code: 		Dr. Cho, I. 

// All rights reserved

// 

// updated: March 28, 2017

//----------------------------------------------------

//IN    : int i_reci = location of row with missing cell that has the least number of donors

//

//IN    : double uox(nrow_uox, ncol)= sorted unique patterns of observed cells. up to i_count_uox rows are meaningful 

//IN    : double mox(nrow_mox, ncol)= sorted unique patterns of missing  cells. up to i_count_mox rows are meaningful                           

//IN    : int tnU[nrow_uox]		= table format of the number of donors

//IN 	: string cn(nrow)		= vector of string to represent each row of z          

//IN	: int ol(nrow_ol)		= actual location of rows containing ONLY observed cells 

//IN    : int i_merge = random donor selection in Merge algorithm in Cell Make

//                            0= no random seed number setting

//						      1= random seed number setting 

//INOUT : double z(nrow, ncol)  = updated category matrix corresponding to original matrix x   

//====================================================

{

	//below setting is for debugging random sampling

	const bool b_random = 0; //0: use rand(); 1:deterministic for debugging 

	

	//---------------

	//initialize random number generator //Window version 
	//Note: R package version, set.seed(...) should be done at R main 

	//---------------

	//if(i_merge == 1) std::srand(time(NULL)); //turn on random seed using C++ standard rand() fn 

	                                    //this will generate purely random numbers and 

										//will be platform-dependent, i.e., different on Win and Linux 

    //if(i_merge == 0) std::srand(123);	    //turn on the fixed seed //This is still platform-dependent 

	                                    //maybe, use Numerical Recipe for platform-independent  

	

	//---------------

	//make a table of cn[full observed cells only]

	//---------------

	//std::string cn_ol[nrow_ol];

	std::string *cn_ol = new std::string[nrow_ol];

	

	for(int i=0; i<nrow_ol; i++) cn_ol[i] = cn[ol[i] - 1]; //Note: -1 for actual loc

	std::vector<std::string> v_table_cn_ol_row1; //names of the table

	std::vector<int> 		 v_table_cn_ol_row2; //counts of the table

	table_cpp(cn_ol, nrow_ol, v_table_cn_ol_row1, v_table_cn_ol_row2);

	const int i_size_v_table_cn_ol_row2 = (int)v_table_cn_ol_row2.size();	

	

	//----------------

	//get a string of the current row having missing cells, which has the least observed donors

	//----------------

	double* d_cn0 = new double[ncol];

	for(int i=0; i<ncol; i++) d_cn0[i] = mox[i_reci][i];

	std::string cn0; 

	Trans1(d_cn0, ncol, cn0);

	

	

	//----------------

	//ACTUAL locations of other missing rows that have the same string as cn0

	//----------------

	std::vector<int> v_mloc;

    which(cn, nrow, cn0, v_mloc);	

	const int i_nml = (int)v_mloc.size(); 

	

	

	

	//-----------------

	//Which columns are NOT missing in mox[i_reci][]

	//-----------------

	double* d_mox_row = new double[ncol]; //temporary array

	for(int i=0; i<ncol; i++) d_mox_row[i] = mox[i_reci][i];

	std::vector<int> v_mxl; //ACTUAL location of non-missing column of mox[i_reci][]

	whichINV(d_mox_row, ncol, 0.0, v_mxl);

	const int i_nxl = (int)v_mxl.size(); //number of non-missing cell on this row

	delete[] d_mox_row; 

	

	

	//-----------------

	//Find the nearest potential donor cells using "fdis"

	//NOTE: below two matrix and array has nrow_uox rows since it is 

	//related to observed cells uox

	//-----------------

	double ** d_cand = New_dMatrix(nrow_uox, ncol); //NOTE: the column may be flexible for below cases 

	double *  d_fdist= new double[nrow_uox];        //distance between entities 

	Fill_dVector(d_fdist, nrow_uox, 0.0);

	

	if(i_nxl == 1) //when the current missing row has only ONE observed cell   

	{

		//------------

		//make a copy of all rows of the one column 

		// that corresponds to the column where the observed cell of current missing row

        // is located 		

		//------------

		for(int i=0; i<nrow_uox; i++) 

		{d_cand[i][0] = uox[i][v_mxl[0]-1]; } //-1 for ACTUAL location 

		

		//calculate distance using |a-b|^2

		const double d_mox_mxl = mox[i_reci][v_mxl[0]-1];

		distance2(d_cand, nrow_uox, i_nxl, d_mox_mxl, 

                  d_fdist);	 

		//---

		//NOTE: only the 1st value contains meaningful distance	

		//to avoid error in finding the minimum distance, 

		//in below, minimum searching needs due consideration

		//---



	}

	

	

	if(i_nxl >1 ) //when current missing row has more than one column that has observed cells 

	{

		//------------

		//make a copy of all rows of all columns that correspond to the observed cells 

		//------------

		for(int i=0; i<nrow_uox; i++) 

		{

			for(int j=0; j<i_nxl; j++) //note: i_nxl is the length of v_mxl

			{ d_cand[i][j] = uox[i][v_mxl[j]-1]; } //-1 for ACTUAL location 

		}

		//-------------

		//calculate distance = sum(|a-b|^2) per row where mox[i][mxl] is the origin

		//-------------	

		double d_sum_dist = 0.0; 

		for(int i=0; i<nrow_uox; i++)

		{

			d_sum_dist = 0.0; //re-initialize

			for(int j=0; j<i_nxl; j++)

			{

				double d_mox_temp = mox[i_reci][v_mxl[j]-1];

				double d_temp1 = d_cand[i][j]; 

				d_sum_dist +=  (d_mox_temp - d_temp1)*(d_mox_temp - d_temp1);

			}

			d_fdist[i] = d_sum_dist; 

		}



	}

	

	

	//------------

	//find the minimum distance

	//------------

	std::vector<int> v_floc; //ACTUAL location of minimum dist. in fdist

	double d_min_fdist = 0.0;

	if(i_nxl>=1) 

	{

		d_min_fdist = min_FHDI(d_fdist, nrow_uox);

		which(d_fdist, nrow_uox, d_min_fdist, v_floc); 

	}

	const int i_size_floc = (int)v_floc.size();

	if(i_size_floc <=0) { Rprintf("Error! floc size is 0!"); return;}



	

	//------------

	//select out a table of the location information of the minimum distance cells

	//------------

	int* i_nf = new int[i_size_floc]; 

    for(int i=0; i<i_size_floc; i++) i_nf[i] = v_table_cn_ol_row2[v_floc[i]-1]; //-1 for actual loc

	const int max_nf = max_FHDI(i_nf, i_size_floc); 



	

	//-----------------------------

	//Case 1: more than 2 rows that have the smallest distance

	//-----------------------------

	if(max_nf>=2)

	{

		//-------------

		//find rows that have max nf

		//-------------

		std::vector<int> v_nf_max; 

		which(i_nf, i_size_floc, max_nf, v_nf_max); //Actual locations which have max of nf

		const int i_size_nf_max = (int)v_nf_max.size(); 

		

		//-------------

		//locations having the minimum distance between missing and observed cells

		//-------------

		std::vector<int> v_xloc;

		for(int i=0; i<i_size_nf_max; i++) v_xloc.push_back(v_floc[v_nf_max[i]-1]); //-1 for actual loc

		const int i_size_xloc = (int)v_xloc.size(); 

		

		//-------------

		//random number within [1, i_size_xloc]

		//Note: this is ACTUAL location

		//-------------

		int i_loc_rand_temp0 = 1; 

		//window version 
		//if(i_merge == 1) i_loc_rand_temp0 = std::rand()%i_size_xloc + 1; //purely random 
		
		//R package version 
		if(i_merge == 1) i_loc_rand_temp0 = (int)floor(Rf_runif(1.0, i_size_xloc)); //purely random 



		if(b_random) i_loc_rand_temp0 = 1 ; //for debugging // 

		const int i_loc_rand_xloc = v_xloc[i_loc_rand_temp0-1]; //-1 for actual loc

		

		//-------------

		//update z 

		//with the randomly selected row of donor  

		//-------------

		for(int i=0; i<i_nml; i++)//row-wise copy. Note: i_nml is the size of v_mloc[]

		{

			for(int j=0; j<i_nxl; j++)//column-wise copy. Note: i_nxl is the size of v_mxl[]

			{

				z[v_mloc[i]-1][v_mxl[j]-1] = uox[i_loc_rand_xloc-1][v_mxl[j]-1]; //-1 for actual location

			}			

		}

		

	}

	

	

	//-----------------------------

	//Case 2: less than 2 rows that have the smallest distance

	//-----------------------------

	if(max_nf<2)

	{

		//-------------

		//find rows that is nf=1

		//-------------

		std::vector<int> v_nf_1; 

		which(i_nf, i_size_floc, 1, v_nf_1); //Actual locations which have 1 in nf

		const int i_size_nf_1 = (int)v_nf_1.size(); 

		

		//-------------

		//reduce floc to have rows with only nf=1

		//-------------

		std::vector<int> v_floc_1; 

		for(int i=0; i<i_size_nf_1; i++) 

		{

			int i_temp=v_nf_1[i]-1 ; //-1 for actual location 

			v_floc_1.push_back(v_floc[i_temp]); 

		}

		const int i_size_v_floc_1 = (int)v_floc_1.size(); 

		//testout

		//RPrint("successful so far5-1 ");

		//RPrint("i_size_nf_1: "); RPrint(i_size_nf_1);

		//RPrint("i_size_v_floc_1: "); RPrint(i_size_v_floc_1);

		

		//-------------

		//random number within [1, i_size_v_floc_1]

		//Note: this is ACTUAL location

		//-------------

		int i_loc_rand_temp = 1; 

		//window version 
		//if(i_merge == 1) i_loc_rand_temp = std::rand()%i_size_v_floc_1 + 1; //purely random 

		//R package version 
		if(i_merge == 1) i_loc_rand_temp = (int)floor(Rf_runif(1.0, i_size_v_floc_1)); //purely random 



		if(b_random) i_loc_rand_temp = 1 ; //for debugging 

		const int i_loc_rand_floc = v_floc_1[i_loc_rand_temp-1]; //-1 for actual location

		

		//-------------

		//Make a donor row and update pcell without the donor cell 

		//with the randomly selected row number  

		//-------------

		double* dcell = new double[ncol];

		for(int i=0; i<ncol; i++) dcell[i] = uox[i_loc_rand_floc-1][i]; //-1 for actual location

		//NOTE: pcell has (nrow_uox -1) rows

		double** pcell = New_dMatrix(nrow_uox-1, ncol); //by excluding the dcell

		for(int i=0; i<nrow_uox; i++)//row-wise copy. 

		{

			//---------------

			//below two conditions it to skip the dcell row

			//---------------

			if(i < (i_loc_rand_floc-1) ) 

			{

				for(int j=0; j<ncol; j++)//column-wise copy. 

				{pcell[i][j] = uox[i][j];} 

			}

			if(i > (i_loc_rand_floc-1) ) //to skip the dcell row 

			{

				for(int j=0; j<ncol; j++)//column-wise copy. 

				{pcell[i-1][j] = uox[i][j];} //note the -1 (one row shift)  		

			}

		}



		//-----------------------

		//calculate relative distance between pcell and dcell

		//-----------------------

		const int nrp = nrow_uox-1;  //total number of rows of pcell

		double* d_sdis = new double[nrp]; 

		Fill_dVector(d_sdis, nrp, 0.0);

		

		//-------------

		//distance between pcell and dcell

		//(1) when pcell is an array

		//-------------

		double sdis = 0.0; 

		if(nrp == 1)

		{

			for(int i=0; i<ncol; i++) 

				sdis += (pcell[0][i] - dcell[i])*(pcell[0][i] - dcell[i]);		

		}

		d_sdis[0] = sdis; 

		//testout

		//RPrint("successful so far5-3 ");				

		

		//-------------

		//distance between pcell and dcell

		//(2) when pcell is indeed a Matrix

		//-------------

		if(nrp > 1)

		{

			//-------------

			//calculate distance = sum(|a-b|^2) per row 

			//-------------	

			double d_sum_sdis = 0.0; 

			for(int i=0; i<nrp; i++)

			{

				d_sum_sdis = 0.0; //re-initialize

				for(int j=0; j<ncol; j++)

				{

					d_sum_sdis +=  (pcell[i][j]-dcell[j])*(pcell[i][j]-dcell[j]);

				}

				d_sdis[i] = d_sum_sdis; //NEEDS to check!

			}			

		}

		

		//-----

		//find the minimum sdis[]

		//-----

		const double d_min_sdis = min_FHDI(d_sdis, nrp);

		std::vector<int> v_sloc; //ACTUAL locations of min sdis 

		which(d_sdis, nrp, d_min_sdis, v_sloc);

		const int i_size_v_sloc = (int)v_sloc.size(); 

		

		//-----

		//exclude floc row from the table of cn[ol]

		//-----

		int* i_socn = new int[i_size_v_table_cn_ol_row2-1]; //reduced vector of tocn

		for(int i =0; i<i_size_v_table_cn_ol_row2; i++)

		{

			if(i < (i_loc_rand_floc-1)) //except for the floc actual location

			{

				i_socn[i] = v_table_cn_ol_row2[i];

			}

			if(i > (i_loc_rand_floc-1)) //except for the floc actual location

			{

				i_socn[i-1] = v_table_cn_ol_row2[i]; //Note: one index shift 

			}

		}

		

		//-----

		//exclude floc row from tnU[]

		//------

		int* i_snU = new int[nrow_uox -1];//reduced array of tnU[]

		for(int i=0; i<nrow_uox; i++)

		{

			if(i < (i_loc_rand_floc-1)) //except for the floc actual location

			{

				i_snU[i] = tnU[i];

			}

			if(i > (i_loc_rand_floc-1)) //except for the floc actual location

			{

				i_snU[i-1] = tnU[i]; //Note: one index shift 

			}

		}

		

		//------

		//select out sloc rows from socn

		//------

		int* i_ns = new int[i_size_v_sloc]; //part of socn at sloc

		for(int i=0; i<i_size_v_sloc; i++) i_ns[i] = i_socn[(int)v_sloc[i]-1]; //-1 for actual location 

		const int max_i_ns = max_FHDI(i_ns, i_size_v_sloc);

		std::vector<int> v_i_ns; //Actual locations of max ns

		which(i_ns, i_size_v_sloc, max_i_ns, v_i_ns);

		const int i_size_v_i_ns = (int)v_i_ns.size();

		int* i_xloc = new int[i_size_v_i_ns];

		for(int i=0; i<i_size_v_i_ns; i++)

		{

			i_xloc[i] = v_sloc[(int)v_i_ns[i]-1]; //-1 for actual location

		}

		//deallocate used array or matrix (2018_0416)
		delete [] i_ns; 

		//------

		//get a random integer between [1, length(x_loc)]

		//------

		int i_loc_rand_temp2 = 1; 

		
		//window version 
		//if(i_merge == 1) i_loc_rand_temp2 = std::rand()%i_size_v_i_ns + 1; //purely random 

		//R package version 
		if(i_merge == 1) i_loc_rand_temp2 = (int)floor(Rf_runif(1, i_size_v_i_ns)); //purely random 
		

		if(b_random) i_loc_rand_temp2 = 1 ; //for debugging

		const int i_loc_rand_xloc  = i_xloc[i_loc_rand_temp2-1]; //-1 for actual location



		//------

		//select out a row at floc from tnU and at xloc from snU

		//------

		const int i_crip1 = tnU[i_loc_rand_floc-1]; //-1 for actual location

		const int i_crip2 = i_snU[i_loc_rand_xloc-1]; //-1 for actual location	



		

		//========================

		//sub case 1: max of ns >= 2

		//========================

		if(max_i_ns >= 2)

		{

			//-----------

			//find rows that have the same string as the row at floc

			//-----------

			std::string s_ncn;

			double* d_uox_a_row = new double[ncol]; //temp array of a row of uox

			for(int i=0; i<ncol; i++) d_uox_a_row[i] = uox[i_loc_rand_floc-1][i]; //-1 for actual location

			Trans1(d_uox_a_row, ncol, s_ncn);

			std::vector<int> v_uloc; //actual locations where cn = ncn

			which(cn, nrow, s_ncn, v_uloc); 

			const int i_nul = (int)v_uloc.size(); //size of uloc

			

			//------------

			//replace z at row=mloc and column=mxl

			//------------

			for(int i=0; i<i_nml; i++)//row-wise copy. Note: i_nml is the size of v_mloc[]

			{

				for(int j=0; j<i_nxl; j++)//column-wise copy. Note: i_nxl is the size of v_mxl[]

				{

					z[v_mloc[i]-1][v_mxl[j]-1] 

					= pcell[i_loc_rand_xloc-1][v_mxl[j]-1]; //-1 for actual location

				}			

			}			

			//------------

			//replace z at row=uloc and all columns

			//------------

			for(int i=0; i<i_nul; i++)//row-wise copy. Note: i_nul is the size of v_uloc[]

			{

				for(int j=0; j<ncol; j++)//column-wise copy. Note: all columns

				{

					z[v_uloc[i]-1][j] 

					= pcell[i_loc_rand_xloc-1][j]; //-1 for actual location

				}			

			}			

			delete[] d_uox_a_row; 



		}



		//========================

		//sub case 2: max of ns < 2 && crip1>=crip2

		//========================

		if(max_i_ns < 2 && i_crip1 >= i_crip2)

		{

			//-----------

			//find rows that have the same string as the row at floc

			//-----------

			std::string s_ncn2;

			double* d_uox_a_row2 = new double[ncol]; //temp array of a row of uox

			for(int i=0; i<ncol; i++) d_uox_a_row2[i] = pcell[i_loc_rand_xloc-1][i]; //-1 for actual location

			Trans1(d_uox_a_row2, ncol, s_ncn2);

			std::vector<int> v_uloc2; //actual locations where cn = ncn

			which(cn, nrow, s_ncn2, v_uloc2); 

			const int i_nul2 = (int)v_uloc2.size(); //size of uloc

			

			//------------

			//replace z at row=mloc and column=mxl

			//------------

			for(int i=0; i<i_nml; i++)//row-wise copy. Note: i_nml is the size of v_mloc[]

			{

				for(int j=0; j<i_nxl; j++)//column-wise copy. Note: i_nxl is the size of v_mxl[]

				{

					z[v_mloc[i]-1][v_mxl[j]-1] 

					= uox[i_loc_rand_floc-1][v_mxl[j]-1]; //-1 for actual location

				}			

			}			

			//------------

			//replace z at row=uloc and all columns

			//------------

			for(int i=0; i<i_nul2; i++)//row-wise copy. Note: i_nul2 is the size of v_uloc2[]

			{

				for(int j=0; j<ncol; j++)//column-wise copy. Note: all columns

				{

					z[v_uloc2[i]-1][j] 

					= uox[i_loc_rand_floc-1][j]; //-1 for actual location

				}			

			}			

			delete[] d_uox_a_row2; 

			//testout

			//RPrint("===== after sub case 2============ ");

			//RPrint("v_uloc2"); RPrint(v_uloc2);

			//RPrint("i_nul2"); RPrint(i_nul2);

			//RPrint("updated z[][]: "); RPrint(z, nrow, ncol);

			

		}		



		//========================

		//sub case 3: max of ns < 2 && crip1 < crip2

		//========================

		if(max_i_ns < 2 && i_crip1 < i_crip2)

		{

			//-----------

			//find rows that have the same string as the row at floc

			//-----------

			std::string s_ncn3;

			double* d_uox_a_row3 = new double[ncol]; //temp array of a row of uox

			for(int i=0; i<ncol; i++) d_uox_a_row3[i] = uox[i_loc_rand_floc-1][i]; //-1 for actual location

			Trans1(d_uox_a_row3, ncol, s_ncn3);

			std::vector<int> v_uloc3; //actual locations where cn = ncn

			which(cn, nrow, s_ncn3, v_uloc3); 

			const int i_nul3 = (int)v_uloc3.size(); //size of uloc

			

			//------------

			//replace z at row=mloc and column=mxl

			//------------

			for(int i=0; i<i_nml; i++)//row-wise copy. Note: i_nml is the size of v_mloc[]

			{

				for(int j=0; j<i_nxl; j++)//column-wise copy. Note: i_nxl is the size of v_mxl[]

				{

					z[v_mloc[i]-1][v_mxl[j]-1] 

					= pcell[i_loc_rand_xloc-1][v_mxl[j]-1]; //-1 for actual location

				}			

			}			

			//------------

			//replace z at row=uloc and all columns

			//------------

			for(int i=0; i<i_nul3; i++)//row-wise copy. Note: i_nul3 is the size of v_uloc3[]

			{

				for(int j=0; j<ncol; j++)//column-wise copy. Note: all columns

				{

					z[v_uloc3[i]-1][j] 

					= pcell[i_loc_rand_xloc-1][j]; //-1 for actual location

				}			

			}			

			delete[] d_uox_a_row3; 

			

		}		

		

		//-----------------------

		//local deallocation of memory used in Case 2

		//-----------------------

		delete[] dcell; 

		Del_dMatrix(pcell, nrow_uox-1, ncol);

		delete[] d_sdis; 

		delete[] i_socn; 

		delete[] i_snU; 

		delete[] i_xloc;

	} // end of case 2 nf< 2

	

	

	

	//------------------

	//Deallocation

	//------------------

	delete[] d_cn0; 

	Del_dMatrix(d_cand, nrow_uox, ncol);

    delete[] d_fdist;	

	delete[] i_nf; 

	

	delete[] cn_ol;

	

	return; //temporary ending 

}





} //end of namespace


 //Fn===========================================================================

 //Merge_Extension_Bigp_cpp.cc-----------------------------------------------------------------------------

 //Fn===========================================================================

namespace FHDI {
	//#include <cstdlib> //for rand() and srand()
	//#include <time.h>  //for time()

	void Merge_Extension_Bigp_cpp(const int i_reci, double** uox, const int nrow_uox,
		double** mox, const int nrow_mox, int* tnU,
		std::string cn[], int* ol, const int nrow_ol,
		double** z, const int nrow, const int ncol,
		const int i_merge, int** codes, const int i_option_collapsing,
		const bool b_DEBUG)
		//Description=========================================
		// Merge the categorized matrix z 
		//
		// Algorithm: 											
		//   For a given missing row at i_reci                 e.g., {12, NA, 4}           
		//   Step 1: find columns having the observed cells:   e.g., 1st and 3rd columns   
		//   Step 2: over all rows, search other observed at the 1st and 3rd columns 
		//   Step 3: calculate relative distance, sum(|a-b|^2) 
		//   Step 4: among rows that have the shortest distance, randomly select donors 
		//   Step 5: fill the missing cell with the selected donors 
		//
		//
		// original R code: Dr. Im, J. and Dr. Kim, J. 
		// c++ code: 		Yicheng Yang
		// All rights reserved
		// 
		// updated: Feb 24, 2020
		//----------------------------------------------------
		//IN    : int i_reci = location of row with missing cell that has the least number of donors
		//
		//IN    : double uox(nrow_uox, ncol)= sorted unique patterns of observed cells. up to i_count_uox rows are meaningful 
		//IN    : double mox(nrow_mox, ncol)= sorted unique patterns of missing  cells. up to i_count_mox rows are meaningful                           
		//IN    : int tnU[nrow_uox]		= table format of the number of donors
		//IN 	: string cn(nrow)		= vector of string to represent each row of z          
		//IN	: int ol(nrow_ol)		= actual location of rows containing ONLY observed cells 
		//IN    : int i_merge = random donor selection in Merge algorithm in Cell Make
		//                            0= no random seed number setting
		//						      1= random seed number setting 
		//INOUT : double z(nrow, ncol)  = updated category matrix corresponding to original matrix x   
		//====================================================
	{
		//below setting is for debugging random sampling
		const bool b_random = 0; //0: use rand(); 1:deterministic for debugging 

								 //---------------
								 //initialize random number generator
								 //---------------
								 //if (i_merge == 1) std::srand(time(NULL)); //turn on random seed using C++ standard rand() fn 
								 //this will generate purely random numbers and 
								 //will be platform-dependent, i.e., different on Win and Linux 
								 //if (i_merge == 0) std::srand(123);	    //turn on the fixed seed //This is still platform-dependent 
								 //maybe, use Numerical Recipe for platform-independent  

								 //---------------
								 //make a table of cn[full observed cells only]
								 //---------------
								 //std::string cn_ol[nrow_ol];
		std::string *cn_ol = new std::string[nrow_ol];

		for (int i = 0; i<nrow_ol; i++) cn_ol[i] = cn[ol[i] - 1]; //Note: -1 for actual loc
		std::vector<std::string> v_table_cn_ol_row1; //names of the table
		std::vector<int> 		 v_table_cn_ol_row2; //counts of the table
		table_cpp(cn_ol, nrow_ol, v_table_cn_ol_row1, v_table_cn_ol_row2);
		const int i_size_v_table_cn_ol_row2 = (int)v_table_cn_ol_row2.size();

		//----------------
		//get a string of the current row having missing cells, which has the least observed donors
		//----------------
		double* d_cn0 = new double[ncol];
		for (int i = 0; i<ncol; i++) d_cn0[i] = mox[i_reci][i];
		std::string cn0;
		Trans1(d_cn0, ncol, cn0);


		//----------------
		//ACTUAL locations of other missing rows that have the same string as cn0
		//----------------
		std::vector<int> v_mloc;
		which(cn, nrow, cn0, v_mloc);
		const int i_nml = (int)v_mloc.size();
		//testout
		/*
		if(b_DEBUG){
		RPrint("========in Merge============");
		RPrint("ol: "); RPrint(ol, nrow_ol);
		RPrint("cn_ol: "); RPrint(cn_ol, nrow_ol);
		RPrint("nrow_uox: "); RPrint(nrow_uox);
		RPrint("nrow_mox: "); RPrint(nrow_mox);
		RPrint("i_reci: "); RPrint(i_reci);
		RPrint("v_table_cn_ol_row2: "); RPrint(v_table_cn_ol_row2);
		RPrint("d_cn0[]: "); RPrint(d_cn0, ncol);
		RPrint("cn0[]: "); RPrint(&cn0,1 );
		RPrint("v_mloc: "); RPrint(v_mloc);
		RPrint("i_nml: "); RPrint(i_nml);
		}
		*/



		//-----------------
		//Which columns are NOT missing in mox[i_reci][]
		//-----------------
		double* d_mox_row = new double[ncol]; //temporary array
		for (int i = 0; i<ncol; i++) d_mox_row[i] = mox[i_reci][i];
		std::vector<int> v_mxl; //ACTUAL location of non-missing column of mox[i_reci][]
								//whichINV(d_mox_row, ncol, 0.0, v_mxl);
		for (int i = 0; i < i_option_collapsing; i++) {
			//if (codes[i_reci][i] == 0) { TestOut << "Error! Check the correlated_variables function, it is not correct !!!" << endl; }

			if (codes[i_reci][i] != 0) { // This is for case that the number of observed values in mox[i_recv] is smaller than i_option_collapsing
				v_mxl.push_back(codes[i_reci][i]);
			}
		}

		//TestOut<<"In Merge_extension_cpp v_mxl at i_reci = "<< i_reci << endl;
		//for (int i = 0; i < v_mxl.size(); i++) {
		//	TestOut << "v_mxl["<<i<<"]: "<< v_mxl[i] <<endl;
		//}

		const int i_nxl = (int)v_mxl.size(); //number of non-missing cell on this row
		delete[] d_mox_row;

		//testout
		//if (b_DEBUG) {
		//	RPrint("v_mxl: "); RPrint(v_mxl);
		//	RPrint("i_nxl: "); RPrint(i_nxl);
		//}

		//-----------------
		//Find the nearest potential donor cells using "fdis"
		//NOTE: below two matrix and array has nrow_uox rows since it is 
		//related to observed cells uox
		//-----------------
		double ** d_cand = New_dMatrix(nrow_uox, ncol); //NOTE: the column may be flexible for below cases 
		double *  d_fdist = new double[nrow_uox];        //distance between entities 
		Fill_dVector(d_fdist, nrow_uox, 0.0);

		if (i_nxl == 1) //when the current missing row has only ONE observed cell   
		{
			//------------
			//make a copy of all rows of the one column 
			// that corresponds to the column where the observed cell of current missing row
			// is located 		
			//------------
			for (int i = 0; i<nrow_uox; i++)
			{
				d_cand[i][0] = uox[i][v_mxl[0] - 1];
			} //-1 for ACTUAL location 

			  //calculate distance using |a-b|^2
			const double d_mox_mxl = mox[i_reci][v_mxl[0] - 1];
			distance2(d_cand, nrow_uox, i_nxl, d_mox_mxl,
				d_fdist);
			//---
			//NOTE: only the 1st value contains meaningful distance	
			//to avoid error in finding the minimum distance, 
			//in below, minimum searching needs due consideration
			//---

			//testout
			if (b_DEBUG) {
				//RPrint("successful so far2: i_nxl == 1 ");
				//RPrint("d_fdist[]: "); RPrint(d_fdist, nrow_uox);
				//RPrint("d_cand[][]: "); RPrint(d_cand, nrow_uox, ncol);
			}
		}


		if (i_nxl >1) //when current missing row has more than one column that has observed cells 
		{
			//------------
			//make a copy of all rows of all columns that correspond to the observed cells 
			//------------
			for (int i = 0; i<nrow_uox; i++)
			{
				for (int j = 0; j<i_nxl; j++) //note: i_nxl is the length of v_mxl
				{
					d_cand[i][j] = uox[i][v_mxl[j] - 1];
				} //-1 for ACTUAL location 
			}
			//-------------
			//calculate distance = sum(|a-b|^2) per row where mox[i][mxl] is the origin
			//-------------	
			double d_sum_dist = 0.0;
			for (int i = 0; i<nrow_uox; i++)
			{
				d_sum_dist = 0.0; //re-initialize
				for (int j = 0; j<i_nxl; j++)
				{
					double d_mox_temp = mox[i_reci][v_mxl[j] - 1];
					double d_temp1 = d_cand[i][j];
					d_sum_dist += (d_mox_temp - d_temp1)*(d_mox_temp - d_temp1);
				}
				d_fdist[i] = d_sum_dist;
			}
			//testout
			//if (b_DEBUG) {
			//	RPrint("successful so far3: : i_nxl > 1 ");
			//	RPrint("d_fdist[]: "); RPrint(d_fdist, nrow_uox);
			//	RPrint("d_cand[][]: "); RPrint(d_cand, nrow_uox, ncol);
			//}
		}


		//------------
		//find the minimum distance
		//------------
		std::vector<int> v_floc; //ACTUAL location of minimum dist. in fdist
		double d_min_fdist = 0.0;
		if (i_nxl >= 1)
		{
			d_min_fdist = min_FHDI(d_fdist, nrow_uox);
			which(d_fdist, nrow_uox, d_min_fdist, v_floc);
		}
		const int i_size_floc = (int)v_floc.size();
		if (i_size_floc <= 0) { Rprintf("Error! floc size is 0!"); return; }

		//testout
		//if (b_DEBUG) {
		//	RPrint("under the condition of i_nxl: "); RPrint(i_nxl);
		//	RPrint("d_min_fdist: "); RPrint(d_min_fdist);
		//	RPrint("v_floc: "); RPrint(v_floc);
		//}

		//------------
		//select out a table of the location information of the minimum distance cells
		//------------
		int* i_nf = new int[i_size_floc];
		for (int i = 0; i<i_size_floc; i++) i_nf[i] = v_table_cn_ol_row2[v_floc[i] - 1]; //-1 for actual loc
		const int max_nf = max_FHDI(i_nf, i_size_floc);

		//testout
		//if (b_DEBUG) {
		//	RPrint("max_nf: "); RPrint(max_nf);
		//	RPrint("i_nf[]: "); RPrint(i_nf, i_size_floc);
		//}

		//-----------------------------
		//Case 1: more than 2 rows that have the smallest distance
		//-----------------------------
		if (max_nf >= 2)
		{
			//-------------
			//find rows that have max nf
			//-------------
			std::vector<int> v_nf_max;
			which(i_nf, i_size_floc, max_nf, v_nf_max); //Actual locations which have max of nf
			const int i_size_nf_max = (int)v_nf_max.size();

			//-------------
			//locations having the minimum distance between missing and observed cells
			//-------------
			std::vector<int> v_xloc;
			for (int i = 0; i<i_size_nf_max; i++) v_xloc.push_back(v_floc[v_nf_max[i] - 1]); //-1 for actual loc
			const int i_size_xloc = (int)v_xloc.size();

			//-------------
			//random number within [1, i_size_xloc]
			//Note: this is ACTUAL location
			//-------------
			int i_loc_rand_temp0 = 1;

			//window version 
			//if (i_merge == 1) i_loc_rand_temp0 = std::rand() % i_size_xloc + 1; //purely random 

			//R package version 
			if (i_merge == 1) i_loc_rand_temp0 = (int)floor(Rf_runif(1.0, i_size_xloc)); //purely random 


			if (b_random) i_loc_rand_temp0 = 1; //for debugging // 
			const int i_loc_rand_xloc = v_xloc[i_loc_rand_temp0 - 1]; //-1 for actual loc

																	  //-------------
																	  //update z 
																	  //with the randomly selected row of donor  
																	  //-------------
			for (int i = 0; i<i_nml; i++)//row-wise copy. Note: i_nml is the size of v_mloc[]
			{
				for (int j = 0; j<i_nxl; j++)//column-wise copy. Note: i_nxl is the size of v_mxl[]
				{
					z[v_mloc[i] - 1][v_mxl[j] - 1] = uox[i_loc_rand_xloc - 1][v_mxl[j] - 1]; //-1 for actual location
				}
			}
			//testout
			//RPrint("after Case 1");
			//RPrint("i_loc_rand_temp0: "); RPrint(i_loc_rand_temp0);
			//RPrint("i_loc_rand_xloc: ");  RPrint(i_loc_rand_xloc);
			//RPrint("z[][]: "); RPrint(z, nrow, ncol);

		}
		//testout
		//RPrint("successful so far5 ");


		//-----------------------------
		//Case 2: less than 2 rows that have the smallest distance
		//-----------------------------
		if (max_nf<2)
		{
			//-------------
			//find rows that is nf=1
			//-------------
			std::vector<int> v_nf_1;
			which(i_nf, i_size_floc, 1, v_nf_1); //Actual locations which have 1 in nf
			const int i_size_nf_1 = (int)v_nf_1.size();

			//-------------
			//reduce floc to have rows with only nf=1
			//-------------
			std::vector<int> v_floc_1;
			for (int i = 0; i<i_size_nf_1; i++)
			{
				int i_temp = v_nf_1[i] - 1; //-1 for actual location 
				v_floc_1.push_back(v_floc[i_temp]);
			}
			const int i_size_v_floc_1 = (int)v_floc_1.size();
			//testout
			//RPrint("successful so far5-1 ");
			//RPrint("i_size_nf_1: "); RPrint(i_size_nf_1);
			//RPrint("i_size_v_floc_1: "); RPrint(i_size_v_floc_1);

			//-------------
			//random number within [1, i_size_v_floc_1]
			//Note: this is ACTUAL location
			//-------------
			int i_loc_rand_temp = 1;

			//window version 
			//if (i_merge == 1) i_loc_rand_temp = std::rand() % i_size_v_floc_1 + 1; //purely random 

			//R package version 
			if (i_merge == 1) i_loc_rand_temp = (int)floor(Rf_runif(1.0, i_size_v_floc_1)); //purely random 

			if (b_random) i_loc_rand_temp = 1; //for debugging 
			const int i_loc_rand_floc = v_floc_1[i_loc_rand_temp - 1]; //-1 for actual location
																	   //testout
																	   //RPrint("i_loc_rand_temp: "); RPrint(i_loc_rand_temp);
																	   //RPrint("i_loc_rand_floc: "); RPrint(i_loc_rand_floc);

																	   //-------------
																	   //Make a donor row and update pcell without the donor cell 
																	   //with the randomly selected row number  
																	   //-------------
			double* dcell = new double[ncol];
			for (int i = 0; i<ncol; i++) dcell[i] = uox[i_loc_rand_floc - 1][i]; //-1 for actual location
																				 //NOTE: pcell has (nrow_uox -1) rows
			double** pcell = New_dMatrix(nrow_uox - 1, ncol); //by excluding the dcell
			for (int i = 0; i<nrow_uox; i++)//row-wise copy. 
			{
				//---------------
				//below two conditions it to skip the dcell row
				//---------------
				if (i < (i_loc_rand_floc - 1))
				{
					for (int j = 0; j<ncol; j++)//column-wise copy. 
					{
						pcell[i][j] = uox[i][j];
					}
				}
				if (i >(i_loc_rand_floc - 1)) //to skip the dcell row 
				{
					for (int j = 0; j<ncol; j++)//column-wise copy. 
					{
						pcell[i - 1][j] = uox[i][j];
					} //note the -1 (one row shift)  		
				}
			}
			//testout
			//RPrint("successful so far5-2 ");		
			//RPrint("Inside Case 2");
			//RPrint("dcell[]: "); RPrint(dcell, ncol);
			//RPrint("pcell[][]: "); RPrint(pcell, nrow_uox-1, ncol);

			//-----------------------
			//calculate relative distance between pcell and dcell
			//-----------------------
			const int nrp = nrow_uox - 1;  //total number of rows of pcell
			double* d_sdis = new double[nrp];
			Fill_dVector(d_sdis, nrp, 0.0);

			//-------------
			//distance between pcell and dcell
			//(1) when pcell is an array
			//-------------
			double sdis = 0.0;
			if (nrp == 1)
			{
				for (int i = 0; i<ncol; i++)
					sdis += (pcell[0][i] - dcell[i])*(pcell[0][i] - dcell[i]);
			}
			d_sdis[0] = sdis;
			//testout
			//RPrint("successful so far5-3 ");				

			//-------------
			//distance between pcell and dcell
			//(2) when pcell is indeed a Matrix
			//-------------
			if (nrp > 1)
			{
				//-------------
				//calculate distance = sum(|a-b|^2) per row 
				//-------------	
				double d_sum_sdis = 0.0;
				for (int i = 0; i<nrp; i++)
				{
					d_sum_sdis = 0.0; //re-initialize
					for (int j = 0; j<ncol; j++)
					{
						d_sum_sdis += (pcell[i][j] - dcell[j])*(pcell[i][j] - dcell[j]);
					}
					d_sdis[i] = d_sum_sdis; //NEEDS to check!
				}
			}
			//testout
			//RPrint("successful so far5-4 ");						
			//RPrint("nrp: "); RPrint(nrp);
			//RPrint("d_sdis[]: "); RPrint(d_sdis, nrp);

			//-----
			//find the minimum sdis[]
			//-----
			const double d_min_sdis = min_FHDI(d_sdis, nrp);
			std::vector<int> v_sloc; //ACTUAL locations of min sdis 
			which(d_sdis, nrp, d_min_sdis, v_sloc);
			const int i_size_v_sloc = (int)v_sloc.size();
			//testout
			//RPrint("successful so far5-5 ");						
			//RPrint("d_min_sdis: "); RPrint(d_min_sdis);
			//RPrint("v_sloc[]: "); RPrint(v_sloc);

			//-----
			//exclude floc row from the table of cn[ol]
			//-----
			int* i_socn = new int[i_size_v_table_cn_ol_row2 - 1]; //reduced vector of tocn
			for (int i = 0; i<i_size_v_table_cn_ol_row2; i++)
			{
				if (i < (i_loc_rand_floc - 1)) //except for the floc actual location
				{
					i_socn[i] = v_table_cn_ol_row2[i];
				}
				if (i >(i_loc_rand_floc - 1)) //except for the floc actual location
				{
					i_socn[i - 1] = v_table_cn_ol_row2[i]; //Note: one index shift 
				}
			}
			//testout
			//RPrint("successful so far5-6 ");								
			//RPrint("i_socn[]: "); RPrint(i_socn, i_size_v_table_cn_ol_row2-1 );

			//-----
			//exclude floc row from tnU[]
			//------
			int* i_snU = new int[nrow_uox - 1];//reduced array of tnU[]
			for (int i = 0; i<nrow_uox; i++)
			{
				if (i < (i_loc_rand_floc - 1)) //except for the floc actual location
				{
					i_snU[i] = tnU[i];
				}
				if (i >(i_loc_rand_floc - 1)) //except for the floc actual location
				{
					i_snU[i - 1] = tnU[i]; //Note: one index shift 
				}
			}
			//testout
			//RPrint("successful so far5-7 ");								
			//RPrint("i_snU[]: "); RPrint(i_snU, nrow_uox -1);

			//------
			//select out sloc rows from socn
			//------
			int* i_ns = new int[i_size_v_sloc]; //part of socn at sloc
			for (int i = 0; i<i_size_v_sloc; i++) i_ns[i] = i_socn[(int)v_sloc[i] - 1]; //-1 for actual location 
			const int max_i_ns = max_FHDI(i_ns, i_size_v_sloc);
			std::vector<int> v_i_ns; //Actual locations of max ns
			which(i_ns, i_size_v_sloc, max_i_ns, v_i_ns);
			const int i_size_v_i_ns = (int)v_i_ns.size();
			int* i_xloc = new int[i_size_v_i_ns];
			for (int i = 0; i<i_size_v_i_ns; i++)
			{
				i_xloc[i] = v_sloc[(int)v_i_ns[i] - 1]; //-1 for actual location
			}

			//deallocate used array or matrix (2018_0416)
			delete[] i_ns;

			//------
			//get a random integer between [1, length(x_loc)]
			//------
			int i_loc_rand_temp2 = 1;

			//window version 
			//if (i_merge == 1) i_loc_rand_temp2 = std::rand() % i_size_v_i_ns + 1; //purely random 

			//R package version 
			if (i_merge == 1) i_loc_rand_temp2 = (int)floor(Rf_runif(1, i_size_v_i_ns)); //purely random 

			if (b_random) i_loc_rand_temp2 = 1; //for debugging
			const int i_loc_rand_xloc = i_xloc[i_loc_rand_temp2 - 1]; //-1 for actual location

																	  //------
																	  //select out a row at floc from tnU and at xloc from snU
																	  //------
			const int i_crip1 = tnU[i_loc_rand_floc - 1]; //-1 for actual location
			const int i_crip2 = i_snU[i_loc_rand_xloc - 1]; //-1 for actual location	

															//testout
															//RPrint("successful so far6 ");
															//RPrint("i_loc_rand_temp2 : "); RPrint(i_loc_rand_temp2);
															//RPrint("i_loc_rand_xloc : "); RPrint(i_loc_rand_xloc);
															//RPrint("i_xloc[]: "); RPrint(i_xloc, i_size_v_i_ns);
															//RPrint("i_crip1: "); RPrint(i_crip1);
															//RPrint("i_crip2: "); RPrint(i_crip2);

															//========================
															//sub case 1: max of ns >= 2
															//========================
			if (max_i_ns >= 2)
			{
				//-----------
				//find rows that have the same string as the row at floc
				//-----------
				std::string s_ncn;
				double* d_uox_a_row = new double[ncol]; //temp array of a row of uox
				for (int i = 0; i<ncol; i++) d_uox_a_row[i] = uox[i_loc_rand_floc - 1][i]; //-1 for actual location
				Trans1(d_uox_a_row, ncol, s_ncn);
				std::vector<int> v_uloc; //actual locations where cn = ncn
				which(cn, nrow, s_ncn, v_uloc);
				const int i_nul = (int)v_uloc.size(); //size of uloc

													  //------------
													  //replace z at row=mloc and column=mxl
													  //------------
				for (int i = 0; i<i_nml; i++)//row-wise copy. Note: i_nml is the size of v_mloc[]
				{
					for (int j = 0; j<i_nxl; j++)//column-wise copy. Note: i_nxl is the size of v_mxl[]
					{
						z[v_mloc[i] - 1][v_mxl[j] - 1]
							= pcell[i_loc_rand_xloc - 1][v_mxl[j] - 1]; //-1 for actual location
					}
				}
				//------------
				//replace z at row=uloc and all columns
				//------------
				for (int i = 0; i<i_nul; i++)//row-wise copy. Note: i_nul is the size of v_uloc[]
				{
					for (int j = 0; j<ncol; j++)//column-wise copy. Note: all columns
					{
						z[v_uloc[i] - 1][j]
							= pcell[i_loc_rand_xloc - 1][j]; //-1 for actual location
					}
				}
				delete[] d_uox_a_row;
				//testout
				//RPrint("===== after sub case 1============ ");
				//RPrint("v_uloc"); RPrint(v_uloc);
				//RPrint("i_nul"); RPrint(i_nul);
				//RPrint("updated z[][]: "); RPrint(z, nrow, ncol);

			}

			//========================
			//sub case 2: max of ns < 2 && crip1>=crip2
			//========================
			if (max_i_ns < 2 && i_crip1 >= i_crip2)
			{
				//-----------
				//find rows that have the same string as the row at floc
				//-----------
				std::string s_ncn2;
				double* d_uox_a_row2 = new double[ncol]; //temp array of a row of uox
				for (int i = 0; i<ncol; i++) d_uox_a_row2[i] = pcell[i_loc_rand_xloc - 1][i]; //-1 for actual location
				Trans1(d_uox_a_row2, ncol, s_ncn2);
				std::vector<int> v_uloc2; //actual locations where cn = ncn
				which(cn, nrow, s_ncn2, v_uloc2);
				const int i_nul2 = (int)v_uloc2.size(); //size of uloc

														//------------
														//replace z at row=mloc and column=mxl
														//------------
				for (int i = 0; i<i_nml; i++)//row-wise copy. Note: i_nml is the size of v_mloc[]
				{
					for (int j = 0; j<i_nxl; j++)//column-wise copy. Note: i_nxl is the size of v_mxl[]
					{
						z[v_mloc[i] - 1][v_mxl[j] - 1]
							= uox[i_loc_rand_floc - 1][v_mxl[j] - 1]; //-1 for actual location
					}
				}
				//------------
				//replace z at row=uloc and all columns
				//------------
				for (int i = 0; i<i_nul2; i++)//row-wise copy. Note: i_nul2 is the size of v_uloc2[]
				{
					for (int j = 0; j<ncol; j++)//column-wise copy. Note: all columns
					{
						z[v_uloc2[i] - 1][j]
							= uox[i_loc_rand_floc - 1][j]; //-1 for actual location
					}
				}
				delete[] d_uox_a_row2;
				//testout
				//RPrint("===== after sub case 2============ ");
				//RPrint("v_uloc2"); RPrint(v_uloc2);
				//RPrint("i_nul2"); RPrint(i_nul2);
				//RPrint("updated z[][]: "); RPrint(z, nrow, ncol);

			}

			//========================
			//sub case 3: max of ns < 2 && crip1 < crip2
			//========================
			if (max_i_ns < 2 && i_crip1 < i_crip2)
			{
				//-----------
				//find rows that have the same string as the row at floc
				//-----------
				std::string s_ncn3;
				double* d_uox_a_row3 = new double[ncol]; //temp array of a row of uox
				for (int i = 0; i<ncol; i++) d_uox_a_row3[i] = uox[i_loc_rand_floc - 1][i]; //-1 for actual location
				Trans1(d_uox_a_row3, ncol, s_ncn3);
				std::vector<int> v_uloc3; //actual locations where cn = ncn
				which(cn, nrow, s_ncn3, v_uloc3);
				const int i_nul3 = (int)v_uloc3.size(); //size of uloc

														//------------
														//replace z at row=mloc and column=mxl
														//------------
				for (int i = 0; i<i_nml; i++)//row-wise copy. Note: i_nml is the size of v_mloc[]
				{
					for (int j = 0; j<i_nxl; j++)//column-wise copy. Note: i_nxl is the size of v_mxl[]
					{
						z[v_mloc[i] - 1][v_mxl[j] - 1]
							= pcell[i_loc_rand_xloc - 1][v_mxl[j] - 1]; //-1 for actual location
					}
				}
				//------------
				//replace z at row=uloc and all columns
				//------------
				for (int i = 0; i<i_nul3; i++)//row-wise copy. Note: i_nul3 is the size of v_uloc3[]
				{
					for (int j = 0; j<ncol; j++)//column-wise copy. Note: all columns
					{
						z[v_uloc3[i] - 1][j]
							= pcell[i_loc_rand_xloc - 1][j]; //-1 for actual location
					}
				}
				delete[] d_uox_a_row3;
				//testout
				//RPrint("===== after sub case 3============ ");
				//RPrint("v_uloc3"); RPrint(v_uloc3);
				//RPrint("i_nul3"); RPrint(i_nul3);
				//RPrint("updated z[][]: "); RPrint(z, nrow, ncol);

			}
			//testout
			//RPrint("successful so far9 ");

			//-----------------------
			//local deallocation of memory used in Case 2
			//-----------------------
			delete[] dcell;
			Del_dMatrix(pcell, nrow_uox - 1, ncol);
			delete[] d_sdis;
			delete[] i_socn;
			delete[] i_snU;
			delete[] i_xloc;
		} // end of case 2 nf< 2



		  //------------------
		  //Deallocation
		  //------------------
		delete[] d_cn0;
		Del_dMatrix(d_cand, nrow_uox, ncol);
		delete[] d_fdist;
		delete[] i_nf;

		delete[] cn_ol;

		return; //temporary ending 
	}
} //end of namespace

  //Fn===========================================================================

  //KNN.cc-----------------------------------------------------------------------------

  //Fn===========================================================================

namespace FHDI {


	void KNN(const int i_reci, double** uox, const int nrow_uox,
		double** mox, const int nrow_mox, double* d_k,
		std::string cn[], int* ol, const int nrow_ol,
		const int nrow, const int ncol, const int i_merge,
		std::vector<int> &v_nD, List_FHDI &List_nU)
		//Description=========================================
		// Find deficient donors for the recipient who has less than 2 donors by the Euclidean distance
		//
		// Algorithm: 											
		//   For a given missing row at i_reci                 e.g., {12, NA, 4}           
		//   Step 1: compute Euclidean distance from all unique observed patterns to the recipient  
		//   Step 2: Case 0: if the recipient has a donor, then randomly select another one from list
		//   Step 3: Case 1: if the recipient has no donor and the max occurnace of the candidate (who has minimum distance) in the list is >=2, then randomly select another one from list
		//   Step 4: Case 2: if the recipient has no donor and the max occurnace of the candidate (who has minimum distance) in the list is <2, and the size of the list is >=2, 
		//                   then randomly select two donors from list
		//   Step 5: Case 3: if the recipient has no donor and the max occurnace of the candidate (who has minimum distance) in the list is <2, and the size of the list is 1, 
		//                   then select another donor from the list of candicates who has the second minimum distance
		//
		//
		// original R code: Dr. Im, J. and Dr. Kim, J. 
		// c++ code: 		Yicheng Yang. 
		// All rights reserved
		// 
		// updated: Aug 9, 2020
		//----------------------------------------------------
		//IN    : int i_reci = location of row with missing cell that has the least number of donors
		//
		//IN    : double uox(nrow_uox, ncol)= sorted unique patterns of observed cells. up to i_count_uox rows are meaningful 
		//IN    : double mox(nrow_mox, ncol)= sorted unique patterns of missing  cells. up to i_count_mox rows are meaningful                           
		//IN 	: string cn(nrow)		= vector of string to represent each row of z          
		//IN	: int ol(nrow_ol)		= actual location of rows containing ONLY observed cells 
		//INOUT : v_nD(nrow_mox)       = total number of donors of all mox 
		//INOUT : List_nU(nrow_mox)    = List of actual location of donors of all mox in uox
		//====================================================
	{
		//below setting is for debugging random sampling
		//const bool b_random = 0; //0: use rand(); 1:deterministic for debugging 

		//---------------
		//initialize random number generator
		//---------------
		//if (i_merge == 1) std::srand(time(NULL)); //turn on random seed using C++ standard rand() fn 
		//									//this will generate purely random numbers and 
		//									//will be platform-dependent, i.e., different on Win and Linux 
		//if (i_merge == 0) std::srand(123);	//turn on the fixed seed //This is still platform-dependent 
		//									//maybe, use Numerical Recipe for platform-independent  

		//---------------
		//std::string cn_ol[nrow_ol];
		std::string *cn_ol = new std::string[nrow_ol];

		for (int i = 0; i < nrow_ol; i++) cn_ol[i] = cn[ol[i] - 1]; //Note: -1 for actual loc
		std::vector<std::string> v_table_cn_ol_row1; //names of the table
		std::vector<int> 		 v_table_cn_ol_row2; //counts of the table
		table_cpp(cn_ol, nrow_ol, v_table_cn_ol_row1, v_table_cn_ol_row2);

		//----------------
		//get a string of the current row having missing cells, which has the least observed donors
		//----------------
		double* d_cn0 = new double[ncol];
		for (int i = 0; i < ncol; i++) d_cn0[i] = mox[i_reci][i];
		std::string cn0;
		Trans1(d_cn0, ncol, cn0);


		//----------------
		//ACTUAL locations of other missing rows that have the same string as mox[i]
		//----------------
		std::vector<int> v_mloc;
		which(cn, nrow, cn0, v_mloc);
		//const int i_nml = (int)v_mloc.size();
		//testout
		/*
		if(b_DEBUG){
		RPrint("========in Merge============");
		RPrint("ol: "); RPrint(ol, nrow_ol);
		RPrint("cn_ol: "); RPrint(cn_ol, nrow_ol);
		RPrint("nrow_uox: "); RPrint(nrow_uox);
		RPrint("nrow_mox: "); RPrint(nrow_mox);
		RPrint("i_reci: "); RPrint(i_reci);
		RPrint("v_table_cn_ol_row2: "); RPrint(v_table_cn_ol_row2);
		RPrint("d_cn0[]: "); RPrint(d_cn0, ncol);
		RPrint("cn0[]: "); RPrint(&cn0,1 );
		RPrint("v_mloc: "); RPrint(v_mloc);
		RPrint("i_nml: "); RPrint(i_nml);
		}
		*/


		//-----------------
		//Which columns are NOT missing in mox[i_reci][]
		//-----------------
		double* d_mox_row = new double[ncol]; //temporary array
		for (int i = 0; i < ncol; i++) d_mox_row[i] = mox[i_reci][i];


		std::vector<int> v_mxl; //ACTUAL location of non-missing column of mox[i_reci][]
		whichINV(d_mox_row, ncol, 0.0, v_mxl);
		const int i_nxl = (int)v_mxl.size(); //number of non-missing cell on this row
		delete[] d_mox_row;

		//-----------------
		//Find the nearest potential donor cells using "fdis"
		//NOTE: below two matrix and array has nrow_uox rows since it is 
		//related to observed cells uox
		//-----------------
		double ** d_cand = New_dMatrix(nrow_uox, ncol); //NOTE: the column may be flexible for below cases 
		double *  d_fdist = new double[nrow_uox];        //distance between entities 
		Fill_dVector(d_fdist, nrow_uox, 0.0);

		if (i_nxl == 1) //when the current missing row has only ONE observed cell   
		{
			//------------
			//make a copy of all rows of the one column 
			// that corresponds to the column where the observed cell of current missing row
			// is located 		
			//------------
			for (int i = 0; i < nrow_uox; i++)
			{
				d_cand[i][0] = (uox[i][v_mxl[0] - 1]) / (d_k[v_mxl[0] - 1]);
			} //-1 for ACTUAL location 

			  //calculate distance using |a-b|^2
			const double d_mox_mxl = (mox[i_reci][v_mxl[0] - 1]) / (d_k[v_mxl[0] - 1]);
			distance2(d_cand, nrow_uox, i_nxl, d_mox_mxl,
				d_fdist);
			//---
			//NOTE: only the 1st value contains meaningful distance	
			//to avoid error in finding the minimum distance, 
			//in below, minimum searching needs due consideration
			//---

			//testout
			//if(b_DEBUG){
			//RPrint("successful so far2: i_nxl == 1 ");
			//RPrint("d_fdist[]: "); RPrint(d_fdist, nrow_uox);
			//RPrint("d_cand[][]: "); RPrint(d_cand, nrow_uox, ncol);
			//}
		}


		if (i_nxl > 1) //when current missing row has more than one column that has observed cells 
		{
			//------------
			//make a copy of all rows of all columns that correspond to the observed cells 
			//------------
			for (int i = 0; i < nrow_uox; i++)
			{
				for (int j = 0; j < i_nxl; j++) //note: i_nxl is the length of v_mxl
				{
					d_cand[i][j] = (uox[i][v_mxl[j] - 1]) / (d_k[v_mxl[j] - 1]); // Normalized it by k
				} //-1 for ACTUAL location 
			}
			//-------------
			//calculate distance = sum(|a-b|^2) per row where mox[i][mxl] is the origin
			//-------------	
			double d_sum_dist = 0.0;
			for (int i = 0; i < nrow_uox; i++)
			{
				d_sum_dist = 0.0; //re-initialize
				for (int j = 0; j < i_nxl; j++)
				{
					double d_mox_temp = (mox[i_reci][v_mxl[j] - 1]) / (d_k[v_mxl[j] - 1]);// Normalized it by k
					double d_temp1 = d_cand[i][j];
					d_sum_dist += (d_mox_temp - d_temp1)*(d_mox_temp - d_temp1);
				}
				d_fdist[i] = d_sum_dist;
			}
			//testout
			//if(b_DEBUG){
			//RPrint("successful so far3: : i_nxl > 1 ");
			//RPrint("d_fdist[]: "); RPrint(d_fdist, nrow_uox);
			//RPrint("d_cand[][]: "); RPrint(d_cand, nrow_uox, ncol);
			//}
		}

		//-------------
		//set the distance from obatined donors in uox to mox[i_reci] as 1234567 instead of 0s
		//if the distnace is 0, that means this uox is already a donor in the list
		//-------------
		for (int k1 = 0; k1 < nrow_uox; k1++) {
			if (d_fdist[k1] == 0) {
				d_fdist[k1] = 1234567;
			}
		}

		//------------
		//find the minimum distance
		//------------
		std::vector<int> v_floc; //ACTUAL location of donors in uox who has the minimum distance
		double d_min_fdist = 0.0;
		if (i_nxl >= 1)
		{
			d_min_fdist = min_FHDI(d_fdist, nrow_uox);
			which(d_fdist, nrow_uox, d_min_fdist, v_floc);
		}

		const int i_size_floc = (int)v_floc.size();
		if (i_size_floc <= 0) { Rprintf("Error! floc size is 0!"); return; }

		//testout
		//if(b_DEBUG){
		//RPrint("under the condition of i_nxl: "); RPrint(i_nxl);
		//RPrint("d_min_fdist: "); RPrint(d_min_fdist);
		//RPrint("v_floc: "); RPrint(v_floc);
		//}

		//------------
		//select out a table of the location information of the minimum distance cells
		//------------
		int* i_nf = new int[i_size_floc]; // occurance of all donors in uox for mox[i]
		for (int i = 0; i < i_size_floc; i++) i_nf[i] = v_table_cn_ol_row2[v_floc[i] - 1]; //-1 for actual loc
		const int max_nf = max_FHDI(i_nf, i_size_floc); //highest occueance of all donors

														//testout
														//if(b_DEBUG){
														//RPrint("max_nf: "); RPrint(max_nf);
														//RPrint("i_nf[]: "); RPrint(i_nf, i_size_floc);
														//}

														//-------------
														//find rows that have max nf
														//-------------
		std::vector<int> v_nf_max;
		which(i_nf, i_size_floc, max_nf, v_nf_max); //Actual locations which have max of nf
		const int i_size_nf_max = (int)v_nf_max.size();

		//-------------
		//locations having the minimum distance between missing and observed cells
		//-------------
		std::vector<int> v_xloc;// actual locations of donors in uox who has minimum distance and highest occurance
		for (int i = 0; i < i_size_nf_max; i++) v_xloc.push_back(v_floc[v_nf_max[i] - 1]); //-1 for actual loc
		const int i_size_xloc = (int)v_xloc.size();


		// Example:
		// v_floc = {1,3,7,9} -> occur = {3,3,1,1}
		// v_xloc ={1,3} -> occur = {3,3}
		// i_size_xloc = i_size_nf_max =2

		//---------------------------------------
		//Case 0: if mox[i] has one donor and it only needs one more donor from uox who has the smallest distance

		//a) i_size_xloc >=2, need randomly select 1
		//{3,3,2,1} i_size_xloc=2
		//{1,1} i_size_xloc=2

		//b) i_size_xloc == 1, no random selection
		//{3,2,1,1} i_size_xloc=1
		//{2} ..
		//{1} .. 

		//----------------------------------------


		if (v_nD[i_reci] == 1) {

			//-------------
			//random number within [1, i_size_xloc]
			//Note: this is ACTUAL location
			//-------------
			int i_loc_rand_temp0 = 1;
			//if ((i_merge == 1) && (i_size_xloc >= 2)) i_loc_rand_temp0 = std::rand() % i_size_xloc + 1; //purely random 
			if ((i_merge == 1) && (i_size_xloc >= 2)) i_loc_rand_temp0 = (int)floor(Rf_runif(1.0, i_size_xloc)); //purely random 

																												 //if(b_random) i_loc_rand_temp0 = 1 ; //for debugging // 
			const int i_loc_rand_xloc = v_xloc[i_loc_rand_temp0 - 1]; //-1 for actual loc

			v_nD[i_reci] = 1 + max_nf;

			std::vector<double> d_oloc_temp;

			d_oloc_temp.push_back(i_loc_rand_xloc);

			//sort(d_oloc_temp.begin(), d_oloc_temp.end());

			List_nU.put_block_yicheng(i_reci, 1, d_oloc_temp);

		}



		if (v_nD[i_reci] == 0) {


			//-----------------------------
			//Case 1: if mox[i] has no donors and the highest occurance (i.e., max_nf) of donors with the smallest distance is >= 2
			//        then it needs only one more donor from uox who has the smallest distance

			//a) max_nf >= 2
			// {3,3,2,1} i_size_xloc >=2, need randomly select 1

			// {3,2,1,1} i_size_xloc ==1,no random selection
			// {2} ..

			if (max_nf >= 2) {

				//-------------
				//random number within [1, i_size_xloc]
				//Note: this is ACTUAL location
				//-------------
				int i_loc_rand_temp0 = 1;
				//if ((i_merge == 1) && (i_size_xloc >= 2)) i_loc_rand_temp0 = std::rand() % i_size_xloc + 1; //purely random 
				if ((i_merge == 1) && (i_size_xloc >= 2)) i_loc_rand_temp0 = (int)floor(Rf_runif(1.0, i_size_xloc)); //purely random  

				const int i_loc_rand_xloc = v_xloc[i_loc_rand_temp0 - 1]; //-1 for actual loc

				v_nD[i_reci] = max_nf;

				std::vector<double> d_oloc_temp;
				d_oloc_temp.push_back(i_loc_rand_xloc);

				//sort(d_oloc_temp.begin(), d_oloc_temp.end());

				List_nU.put_block_yicheng(i_reci, 1, d_oloc_temp);

			}


			//Case 2: if mox[i] has no donors and the highest occurance (i.e., max_nf) of donors with the smallest distance is < 2 and 
			//        v_floc.size() is >= 2,
			//        then it needs two donors from uox who has the smallest distance

			// {1,1,1,1} need randomly select 2

			// {1,1} no random selection 

			if ((max_nf < 2) && (v_floc.size() >= 2)) {

				//-------------
				//random number within [1, i_size_xloc]
				//Note: this is ACTUAL location
				//-------------
				int i_loc_rand_temp0 = 1;
				int i_loc_rand_temp1 = 2;

				if ((i_merge == 1) && (i_size_xloc > 2)) {
					i_loc_rand_temp0 = 0;// Make sure it will go into the while loop
					i_loc_rand_temp1 = 0;
					while (i_loc_rand_temp0 == i_loc_rand_temp1) {
						//i_loc_rand_temp0 = std::rand() % i_size_xloc + 1;
						//i_loc_rand_temp1 = std::rand() % i_size_xloc + 1;

						i_loc_rand_temp0 = (int)floor(Rf_runif(1.0, i_size_xloc));
						i_loc_rand_temp1 = (int)floor(Rf_runif(1.0, i_size_xloc));
					}
				}

				//if ((i_merge == 1) && (i_size_xloc > 2)) i_loc_rand_temp0 = std::rand() % i_size_xloc + 1; //purely random 
				//if ((i_merge == 1) && (i_size_xloc > 2)) i_loc_rand_temp1 = std::rand() % i_size_xloc + 1; //purely random 

				//if (i_loc_rand_temp0 == i_loc_rand_temp1) { RPrint("Error in KNN of cell make for random selection!!!"); return; }

				if (i_loc_rand_temp0 == i_loc_rand_temp1) {

					Rprintf("Error in KNN of cell make big-n for random selection !!!");

					return;
				}
				//if(b_random) i_loc_rand_temp0 = 1 ; //for debugging // 
				const int i_loc_rand_xloc = v_xloc[i_loc_rand_temp0 - 1]; //-1 for actual loc

				const int i_loc_rand_xloc1 = v_xloc[i_loc_rand_temp1 - 1]; //-1 for actual loc

				v_nD[i_reci] = 2;

				std::vector<double> d_oloc_temp;
				d_oloc_temp.push_back(i_loc_rand_xloc);
				d_oloc_temp.push_back(i_loc_rand_xloc1);

				//sort(d_oloc_temp.begin(), d_oloc_temp.end());

				List_nU.put_block_yicheng(i_reci, 2, d_oloc_temp);

			}

			//Case 3: if mox[i] has no donors and the highest occurance (i.e., max_nf) of donors with the smallest distance is < 2 and 
			//        v_floc.size() is = 1,
			//        then it needs two donors from uox who has the smallest distance

			// {1} no random selection and find in the second minimum set
			if ((max_nf < 2) && (v_floc.size() == 1)) {

				std::vector<int> v_floc_second; //ACTUAL location of donors in uox who has the second minimum distance
				double d_second_min_fdist = 0.0;
				if (i_nxl >= 1)
				{
					d_second_min_fdist = second_min_FHDI(d_fdist, nrow_uox);
					which(d_fdist, nrow_uox, d_second_min_fdist, v_floc_second);
				}

				if (d_second_min_fdist == d_min_fdist) { Rprintf("Error in KNN of cell make for getting the second minimum distance!!!"); return; }

				const int i_size_floc_second = (int)v_floc_second.size();

				int* i_nf_second = new int[i_size_floc_second]; // occurance of all donors in uox for mox[i]
				for (int i = 0; i < i_size_floc_second; i++) i_nf_second[i] = v_table_cn_ol_row2[v_floc_second[i] - 1]; //-1 for actual loc
				const int max_nf_second = max_FHDI(i_nf_second, i_size_floc_second); //highest occueance of all donors

																					 //-------------
																					 //find rows that have max_nf_second
																					 //-------------
				std::vector<int> v_nf_max_second;
				which(i_nf_second, i_size_floc_second, max_nf_second, v_nf_max_second); //Actual locations which have max of nf
				const int i_size_nf_max_second = (int)v_nf_max_second.size();

				//-------------
				//locations having the second minimum distance between missing and observed cells
				//-------------
				std::vector<int> v_xloc_second;// actual locations of donors in uox who has the seond minimum distance and highest occurance
				for (int i = 0; i < i_size_nf_max_second; i++) v_xloc_second.push_back(v_floc_second[v_nf_max_second[i] - 1]); //-1 for actual loc
				const int i_size_xloc_second = (int)v_xloc_second.size();

				v_nD[i_reci] = 1 + max_nf_second;

				int i_loc_rand_temp0 = 1;
				//if ((i_merge == 1) && (i_size_xloc_second >= 2)) i_loc_rand_temp0 = std::rand() % i_size_xloc + 1; //purely random 
				if ((i_merge == 1) && (i_size_xloc_second >= 2)) i_loc_rand_temp0 = (int)floor(Rf_runif(1.0, i_size_xloc));  //purely random 

				const int i_loc_rand_xloc = v_xloc_second[i_loc_rand_temp0 - 1]; //-1 for actual loc

				std::vector<double> d_oloc_temp;
				d_oloc_temp.push_back(v_floc[0]);
				d_oloc_temp.push_back(i_loc_rand_xloc);

				//sort(d_oloc_temp.begin(), d_oloc_temp.end());

				List_nU.put_block_yicheng(i_reci, 2, d_oloc_temp);


				//delete[] d_oloc_temp;

				delete[] i_nf_second;
			}



		}

		//------------------
		//Deallocation
		//------------------
		delete[] d_cn0;
		Del_dMatrix(d_cand, nrow_uox, ncol);
		delete[] d_fdist;
		delete[] i_nf;

		delete[] cn_ol;

		return; //temporary ending 
	}

} //end of namespace

  //Fn===========================================================================

  //KNN_Bigp.cc-----------------------------------------------------------------------------

  //Fn===========================================================================

namespace FHDI {


	void KNN_Bigp(const int i_reci, double** uox, const int nrow_uox,
		double** mox, const int nrow_mox, double* d_k, int** codes, const int i_option_collapsing,
		std::string cn[], int* ol, const int nrow_ol,
		const int nrow, const int ncol, const int i_merge,
		std::vector<int> &v_nD, List_FHDI &List_nU)
		//Description=========================================
		// Find deficient donors for the recipient who has less than 2 donors by the Euclidean distance
		//
		// Algorithm: 											
		//   For a given missing row at i_reci                 e.g., {12, NA, 4}           
		//   Step 1: compute Euclidean distance from all unique observed patterns to the recipient  
		//   Step 2: Case 0: if the recipient has a donor, then randomly select another one from list
		//   Step 3: Case 1: if the recipient has no donor and the max occurnace of the candidate (who has minimum distance) in the list is >=2, then randomly select another one from list
		//   Step 4: Case 2: if the recipient has no donor and the max occurnace of the candidate (who has minimum distance) in the list is <2, and the size of the list is >=2, 
		//                   then randomly select two donors from list
		//   Step 5: Case 3: if the recipient has no donor and the max occurnace of the candidate (who has minimum distance) in the list is <2, and the size of the list is 1, 
		//                   then select another donor from the list of candicates who has the second minimum distance
		//
		//
		// original R code: Dr. Im, J. and Dr. Kim, J. 
		// c++ code: 		Yicheng Yang. 
		// All rights reserved
		// 
		// updated: Aug 11, 2020
		//----------------------------------------------------
		//IN    : int i_reci = location of row with missing cell that has the least number of donors
		//
		//IN    : double uox(nrow_uox, ncol)= sorted unique patterns of observed cells. up to i_count_uox rows are meaningful 
		//IN    : double mox(nrow_mox, ncol)= sorted unique patterns of missing  cells. up to i_count_mox rows are meaningful                           
		//IN 	: string cn(nrow)		= vector of string to represent each row of z          
		//IN	: int ol(nrow_ol)		= actual location of rows containing ONLY observed cells 
		//INOUT : v_nD(nrow_mox)       = total number of donors of all mox 
		//INOUT : List_nU(nrow_mox)    = List of actual location of donors of all mox in uox
		//====================================================
	{
		//below setting is for debugging random sampling
		//const bool b_random = 0; //0: use rand(); 1:deterministic for debugging 

		//---------------
		//initialize random number generator
		//---------------
		//if(i_merge == 1) std::srand(time(NULL)); //turn on random seed using C++ standard rand() fn 
		//                                    //this will generate purely random numbers and 
		//									//will be platform-dependent, i.e., different on Win and Linux 
		//if(i_merge == 0) std::srand(123);	//turn on the fixed seed //This is still platform-dependent 
		//                                    //maybe, use Numerical Recipe for platform-independent  

		//---------------
		//std::string cn_ol[nrow_ol];
		std::string *cn_ol = new std::string[nrow_ol];

		for (int i = 0; i<nrow_ol; i++) cn_ol[i] = cn[ol[i] - 1]; //Note: -1 for actual loc
		std::vector<std::string> v_table_cn_ol_row1; //names of the table
		std::vector<int> 		 v_table_cn_ol_row2; //counts of the table
		table_cpp(cn_ol, nrow_ol, v_table_cn_ol_row1, v_table_cn_ol_row2);

		//----------------
		//get a string of the current row having missing cells, which has the least observed donors
		//----------------
		double* d_cn0 = new double[ncol];
		for (int i = 0; i<ncol; i++) d_cn0[i] = mox[i_reci][i];
		std::string cn0;
		Trans1(d_cn0, ncol, cn0);


		//----------------
		//ACTUAL locations of other missing rows that have the same string as mox[i]
		//----------------
		std::vector<int> v_mloc;
		which(cn, nrow, cn0, v_mloc);
		//const int i_nml = (int)v_mloc.size();
		//testout
		/*
		if(b_DEBUG){
		RPrint("========in Merge============");
		RPrint("ol: "); RPrint(ol, nrow_ol);
		RPrint("cn_ol: "); RPrint(cn_ol, nrow_ol);
		RPrint("nrow_uox: "); RPrint(nrow_uox);
		RPrint("nrow_mox: "); RPrint(nrow_mox);
		RPrint("i_reci: "); RPrint(i_reci);
		RPrint("v_table_cn_ol_row2: "); RPrint(v_table_cn_ol_row2);
		RPrint("d_cn0[]: "); RPrint(d_cn0, ncol);
		RPrint("cn0[]: "); RPrint(&cn0,1 );
		RPrint("v_mloc: "); RPrint(v_mloc);
		RPrint("i_nml: "); RPrint(i_nml);
		}
		*/


		//-----------------
		//Which columns are NOT missing in mox[i_reci][]
		//-----------------
		double* d_mox_row = new double[ncol]; //temporary array
		for (int i = 0; i<ncol; i++) d_mox_row[i] = mox[i_reci][i];


		std::vector<int> v_mxl; //ACTUAL location of non-missing column of mox[i_reci][]
								//whichINV(d_mox_row, ncol, 0.0, v_mxl);

		for (int k = 0; k < i_option_collapsing; k++) {
			//if (codes[i_reci][i] == 0) { TestOut << "Error! Check the correlated_variables function, it is not correct !!!" << endl; }

			if (codes[i_reci][k] != 0) { // This is for case that the number of observed values in mox[i_recv] is smaller than i_option_collapsing
				v_mxl.push_back(codes[i_reci][k]);
			}

		}


		const int i_nxl = (int)v_mxl.size(); //number of non-missing cell on this row
		delete[] d_mox_row;

		//-----------------
		//Find the nearest potential donor cells using "fdis"
		//NOTE: below two matrix and array has nrow_uox rows since it is 
		//related to observed cells uox
		//-----------------
		double ** d_cand = New_dMatrix(nrow_uox, ncol); //NOTE: the column may be flexible for below cases 
		double *  d_fdist = new double[nrow_uox];        //distance between entities 
		Fill_dVector(d_fdist, nrow_uox, 0.0);

		if (i_nxl == 1) //when the current missing row has only ONE observed cell   
		{
			//------------
			//make a copy of all rows of the one column 
			// that corresponds to the column where the observed cell of current missing row
			// is located 		
			//------------
			for (int i = 0; i<nrow_uox; i++)
			{
				d_cand[i][0] = (uox[i][v_mxl[0] - 1]) / (d_k[v_mxl[0] - 1]);
			} //-1 for ACTUAL location 

			  //calculate distance using |a-b|^2
			const double d_mox_mxl = (mox[i_reci][v_mxl[0] - 1]) / (d_k[v_mxl[0] - 1]);
			distance2(d_cand, nrow_uox, i_nxl, d_mox_mxl,
				d_fdist);
			//---
			//NOTE: only the 1st value contains meaningful distance	
			//to avoid error in finding the minimum distance, 
			//in below, minimum searching needs due consideration
			//---

			//testout
			//if(b_DEBUG){
			//RPrint("successful so far2: i_nxl == 1 ");
			//RPrint("d_fdist[]: "); RPrint(d_fdist, nrow_uox);
			//RPrint("d_cand[][]: "); RPrint(d_cand, nrow_uox, ncol);
			//}
		}


		if (i_nxl >1) //when current missing row has more than one column that has observed cells 
		{
			//------------
			//make a copy of all rows of all columns that correspond to the observed cells 
			//------------
			for (int i = 0; i<nrow_uox; i++)
			{
				for (int j = 0; j<i_nxl; j++) //note: i_nxl is the length of v_mxl
				{
					d_cand[i][j] = (uox[i][v_mxl[j] - 1]) / (d_k[v_mxl[j] - 1]);
				} //-1 for ACTUAL location 
			}
			//-------------
			//calculate distance = sum(|a-b|^2) per row where mox[i][mxl] is the origin
			//-------------	
			double d_sum_dist = 0.0;
			for (int i = 0; i<nrow_uox; i++)
			{
				d_sum_dist = 0.0; //re-initialize
				for (int j = 0; j<i_nxl; j++)
				{
					double d_mox_temp = (mox[i_reci][v_mxl[j] - 1]) / (d_k[v_mxl[j] - 1]);
					double d_temp1 = d_cand[i][j];
					d_sum_dist += (d_mox_temp - d_temp1)*(d_mox_temp - d_temp1);
				}
				d_fdist[i] = d_sum_dist;
			}
			//testout
			//if(b_DEBUG){
			//RPrint("successful so far3: : i_nxl > 1 ");
			//RPrint("d_fdist[]: "); RPrint(d_fdist, nrow_uox);
			//RPrint("d_cand[][]: "); RPrint(d_cand, nrow_uox, ncol);
			//}
		}

		//-------------
		//set the distance from obatined donors in uox to mox[i_reci] as 1234567 instead of 0s
		//if the distnace is 0, that means this uox is already a donor in the list
		//-------------
		for (int k1 = 0; k1 < nrow_uox; k1++) {
			if (d_fdist[k1] == 0) {
				d_fdist[k1] = 1234567;
			}
		}

		//------------
		//find the minimum distance
		//------------
		std::vector<int> v_floc; //ACTUAL location of donors in uox who has the minimum distance
		double d_min_fdist = 0.0;
		if (i_nxl >= 1)
		{
			d_min_fdist = min_FHDI(d_fdist, nrow_uox);
			which(d_fdist, nrow_uox, d_min_fdist, v_floc);
		}

		const int i_size_floc = (int)v_floc.size();
		if (i_size_floc <= 0) { Rprintf("Error! floc size is 0!"); return; }


		//testout
		//if(b_DEBUG){
		//RPrint("under the condition of i_nxl: "); RPrint(i_nxl);
		//RPrint("d_min_fdist: "); RPrint(d_min_fdist);
		//RPrint("v_floc: "); RPrint(v_floc);
		//}

		//------------
		//select out a table of the location information of the minimum distance cells
		//------------
		int* i_nf = new int[i_size_floc]; // occurance of all donors in uox for mox[i]
		for (int i = 0; i<i_size_floc; i++) i_nf[i] = v_table_cn_ol_row2[v_floc[i] - 1]; //-1 for actual loc
		const int max_nf = max_FHDI(i_nf, i_size_floc); //highest occueance of all donors

														//testout
														//if(b_DEBUG){
														//RPrint("max_nf: "); RPrint(max_nf);
														//RPrint("i_nf[]: "); RPrint(i_nf, i_size_floc);
														//}

														//-------------
														//find rows that have max nf
														//-------------
		std::vector<int> v_nf_max;
		which(i_nf, i_size_floc, max_nf, v_nf_max); //Actual locations which have max of nf
		const int i_size_nf_max = (int)v_nf_max.size();

		//-------------
		//locations having the minimum distance between missing and observed cells
		//-------------
		std::vector<int> v_xloc;// actual locations of donors in uox who has minimum distance and highest occurance
		for (int i = 0; i < i_size_nf_max; i++) v_xloc.push_back(v_floc[v_nf_max[i] - 1]); //-1 for actual loc
		const int i_size_xloc = (int)v_xloc.size();


		// Example:
		// v_floc = {1,3,7,9} -> occur = {3,3,1,1}
		// v_xloc ={1,3} -> occur = {3,3}
		// i_size_xloc = i_size_nf_max =2

		//---------------------------------------
		//Case 0: if mox[i] has one donor and it only needs one more donor from uox who has the smallest distance

		//a) i_size_xloc >=2, need randomly select 1
		//{3,3,2,1} i_size_xloc=2
		//{1,1} i_size_xloc=2

		//b) i_size_xloc == 1, no random selection
		//{3,2,1,1} i_size_xloc=1
		//{2} ..
		//{1} .. 

		//----------------------------------------

		if (v_nD[i_reci] == 1) {

			//-------------
			//random number within [1, i_size_xloc]
			//Note: this is ACTUAL location
			//-------------
			int i_loc_rand_temp0 = 1;
			//if ((i_merge == 1)&&(i_size_xloc >=2)) i_loc_rand_temp0 = std::rand() % i_size_xloc + 1; //purely random 
			if ((i_merge == 1) && (i_size_xloc >= 2)) i_loc_rand_temp0 = (int)floor(Rf_runif(1.0, i_size_xloc)); //purely random 

																												 //if(b_random) i_loc_rand_temp0 = 1 ; //for debugging // 
			const int i_loc_rand_xloc = v_xloc[i_loc_rand_temp0 - 1]; //-1 for actual loc

			v_nD[i_reci] = 1 + max_nf;


			std::vector<double> d_oloc_temp;

			d_oloc_temp.push_back(i_loc_rand_xloc);

			//sort(d_oloc_temp.begin(), d_oloc_temp.end());

			List_nU.put_block_yicheng(i_reci, 1, d_oloc_temp);


		}



		if (v_nD[i_reci] == 0) {


			//-----------------------------
			//Case 1: if mox[i] has no donors and the highest occurance (i.e., max_nf) of donors with the smallest distance is >= 2
			//        then it needs only one more donor from uox who has the smallest distance

			//a) max_nf >= 2
			// {3,3,2,1} i_size_xloc >=2, need randomly select 1

			// {3,2,1,1} i_size_xloc ==1,no random selection
			// {2} ..

			if (max_nf >= 2) {

				//-------------
				//random number within [1, i_size_xloc]
				//Note: this is ACTUAL location
				//-------------
				int i_loc_rand_temp0 = 1;
				//if ((i_merge == 1) && (i_size_xloc >= 2)) i_loc_rand_temp0 = std::rand() % i_size_xloc + 1; //purely random 
				if ((i_merge == 1) && (i_size_xloc >= 2)) i_loc_rand_temp0 = (int)floor(Rf_runif(1.0, i_size_xloc)); //purely random  

																													 //if(b_random) i_loc_rand_temp0 = 1 ; //for debugging // 
				const int i_loc_rand_xloc = v_xloc[i_loc_rand_temp0 - 1]; //-1 for actual loc

				v_nD[i_reci] = max_nf;

				std::vector<double> d_oloc_temp;
				d_oloc_temp.push_back(i_loc_rand_xloc);

				//sort(d_oloc_temp.begin(), d_oloc_temp.end());

				List_nU.put_block_yicheng(i_reci, 1, d_oloc_temp);


				//delete[] d_oloc_temp;

			}


			//Case 2: if mox[i] has no donors and the highest occurance (i.e., max_nf) of donors with the smallest distance is < 2 and 
			//        v_floc.size() is >= 2,
			//        then it needs two donors from uox who has the smallest distance

			// {1,1,1,1} need randomly select 2

			// {1,1} no random selection 

			if ((max_nf < 2) && (v_floc.size() >= 2)) {

				//-------------
				//random number within [1, i_size_xloc]
				//Note: this is ACTUAL location
				//-------------
				int i_loc_rand_temp0 = 1;
				int i_loc_rand_temp1 = 2;

				if ((i_merge == 1) && (i_size_xloc > 2)) {
					i_loc_rand_temp0 = 0;// Make sure it will go into the while loop
					i_loc_rand_temp1 = 0;
					while (i_loc_rand_temp0 == i_loc_rand_temp1) {
						//i_loc_rand_temp0 = std::rand() % i_size_xloc + 1;
						//i_loc_rand_temp1 = std::rand() % i_size_xloc + 1;

						i_loc_rand_temp0 = (int)floor(Rf_runif(1.0, i_size_xloc));
						i_loc_rand_temp1 = (int)floor(Rf_runif(1.0, i_size_xloc));
					}
				}

				//if ((i_merge == 1) && (i_size_xloc > 2)) i_loc_rand_temp0 = std::rand() % i_size_xloc + 1; //purely random 
				//if ((i_merge == 1) && (i_size_xloc > 2)) i_loc_rand_temp1 = std::rand() % i_size_xloc + 1; //purely random 

				//if (i_loc_rand_temp0 == i_loc_rand_temp1) { RPrint("Error in KNN of cell make for random selection!!!"); return; }

				if (i_loc_rand_temp0 == i_loc_rand_temp1) {

					Rprintf("Error in KNN of cell make big-p for random selection !!!");

					return;
				}
				//if(b_random) i_loc_rand_temp0 = 1 ; //for debugging // 
				const int i_loc_rand_xloc = v_xloc[i_loc_rand_temp0 - 1]; //-1 for actual loc
				const int i_loc_rand_xloc1 = v_xloc[i_loc_rand_temp1 - 1]; //-1 for actual loc

				v_nD[i_reci] = 2;


				std::vector<double> d_oloc_temp;
				d_oloc_temp.push_back(i_loc_rand_xloc);
				d_oloc_temp.push_back(i_loc_rand_xloc1);

				//sort(d_oloc_temp.begin(), d_oloc_temp.end());

				List_nU.put_block_yicheng(i_reci, 2, d_oloc_temp);


				//delete[] d_oloc_temp;
			}

			//Case 3: if mox[i] has no donors and the highest occurance (i.e., max_nf) of donors with the smallest distance is < 2 and 
			//        v_floc.size() is = 1,
			//        then it needs two donors from uox who has the smallest distance

			// {1} no random selection and find in the second minimum set
			if ((max_nf < 2) && (v_floc.size() == 1)) {

				std::vector<int> v_floc_second; //ACTUAL location of donors in uox who has the second minimum distance
				double d_second_min_fdist = 0.0;
				if (i_nxl >= 1)
				{
					d_second_min_fdist = second_min_FHDI(d_fdist, nrow_uox);
					which(d_fdist, nrow_uox, d_second_min_fdist, v_floc_second);
				}

				if (d_second_min_fdist == d_min_fdist) { Rprintf("Error in KNN of cell make for getting the second minimum distance!!!"); return; }
				const int i_size_floc_second = (int)v_floc_second.size();

				int* i_nf_second = new int[i_size_floc_second]; // occurance of all donors in uox for mox[i]
				for (int i = 0; i < i_size_floc_second; i++) i_nf_second[i] = v_table_cn_ol_row2[v_floc_second[i] - 1]; //-1 for actual loc
				const int max_nf_second = max_FHDI(i_nf_second, i_size_floc_second); //highest occueance of all donors

																					 //-------------
																					 //find rows that have max_nf_second
																					 //-------------
				std::vector<int> v_nf_max_second;
				which(i_nf_second, i_size_floc_second, max_nf_second, v_nf_max_second); //Actual locations which have max of nf
				const int i_size_nf_max_second = (int)v_nf_max_second.size();

				//-------------
				//locations having the second minimum distance between missing and observed cells
				//-------------
				std::vector<int> v_xloc_second;// actual locations of donors in uox who has the seond minimum distance and highest occurance
				for (int i = 0; i < i_size_nf_max_second; i++) v_xloc_second.push_back(v_floc_second[v_nf_max_second[i] - 1]); //-1 for actual loc
				const int i_size_xloc_second = (int)v_xloc_second.size();


				v_nD[i_reci] = 1 + max_nf_second;

				int i_loc_rand_temp0 = 1;
				//if ((i_merge == 1) && (i_size_xloc_second >= 2)) i_loc_rand_temp0 = std::rand() % i_size_xloc + 1; //purely random 
				if ((i_merge == 1) && (i_size_xloc_second >= 2)) i_loc_rand_temp0 = (int)floor(Rf_runif(1.0, i_size_xloc));  //purely random 

				const int i_loc_rand_xloc = v_xloc_second[i_loc_rand_temp0 - 1]; //-1 for actual loc


				std::vector<double> d_oloc_temp;
				d_oloc_temp.push_back(v_floc[0]);
				d_oloc_temp.push_back(i_loc_rand_xloc);

				//sort(d_oloc_temp.begin(), d_oloc_temp.end());

				List_nU.put_block_yicheng(i_reci, 2, d_oloc_temp);


				//delete[] d_oloc_temp;

				delete[] i_nf_second;
			}



		}

		//------------------
		//Deallocation
		//------------------
		delete[] d_cn0;
		Del_dMatrix(d_cand, nrow_uox, ncol);
		delete[] d_fdist;
		delete[] i_nf;

		delete[] cn_ol;

		return; //temporary ending 
	}
} //end of namespace



//Fn===========================================================================

//Cell_Make_Extension_cpp.cc-----------------------------------------------------------------------------

//Fn===========================================================================

namespace FHDI{



bool Cell_Make_Extension_cpp(double** x, const int nrow, const int ncol, double* d_k, 

							 int* NonCollapsible_categorical,
							 
							 double** z, 

						 	rbind_FHDI &rbind_uox_CellMake, 

							rbind_FHDI &rbind_mox_CellMake, 

							const int i_merge) 

//Description=========================================

// make cells with the raw data matrix x 

// categorization takes place 

// according to the given number of categories stored in d_k(ncol)

//

// Algorithm:  for categorization

// perc: percentiles used to get quantiles, determined by k

// quan: quantiles if k=4, we quan=(Q1,Q2,Q3) have Q1(=1/4), Q2 (=Median) and Q3(=3/4)

// 

// Note: as of Oct 2016, NA values (missing data) is marked by a long integer at the parent "r" code

//

// original R code: Dr. Im, J. and Dr. Kim, J. 

// c++ code: 		Dr. Cho, I. 

// All rights reserved

// 

// updated: March 28, 2017

//----------------------------------------------------

//IN	: double x(nrow, ncol) 	= {y1, y2, ... } total data containing missing values

//IN	: double d_k(ncol)		= a vector of categories of each column of xalloc

//IN    : int NonCollapsible_categorical(nrol) = {0,0, .., 1,.. 0} 
//				index for non-collapsible categorical variables. 
//				when at least one column has "1" skip cell-collapse procedure
//				this may casue a potential error of lack of enough donor! 
//				(2018, 04 21) 
//											  

//OUT   : double z(nrow, ncol)  = catorized matrix corresponding to original matrix x

//                                initialized with 0.0 

//OUT	: rbind_FHDI rbind_uox_CellMake(ncol); //compact storage of uox, unique observed rows sorted in the ascending order

//OUT	: rbind_FHDI rbind_mox_CellMake(ncol); //compact storage of mox, unique observed rows sorted in the ascending order

//

//IN    : int i_merge = random donor selection in Merge algorithm in Cell Make

//                            0= no random seed number setting

//						      1= random seed number setting 

//====================================================

{

	const int n_max_iteration = nrow*2; //maximum number of iterations 

	//const int n_max_iteration = 2;  //temporary

	//-------------------------------------
	//Determine if there is Non-Collapsible Categorical variable
	//-------------------------------------
	double* d_k_Collapsible = new double[ncol]; //k for collapsible variables only 
	Copy_dVector(d_k, ncol, d_k_Collapsible); 
	
	int i_NonCollapsible_categ_total = 0; 
	for(int i=0; i<ncol; i++)
	{
		i_NonCollapsible_categ_total += NonCollapsible_categorical[i];
		
		if(NonCollapsible_categorical[i] == 0) d_k_Collapsible[i] = d_k[i]; //use user-defined k 
		if(NonCollapsible_categorical[i] == 1) d_k_Collapsible[i] = 1; //will be overwritten by actual total categories
	}

	//testout
	/*RPrint("initial d_k[]"); RPrint(d_k, ncol); 
	RPrint("\n"); 
	RPrint("d_k_Collapsible[]"); RPrint(d_k_Collapsible, ncol); 
	RPrint("\n"); 
	RPrint("NonCollapsible_categorical[]"); RPrint(NonCollapsible_categorical, ncol); 
	RPrint("\n"); 
	*/
	

	//-------------------------------------

	//Categorize raw data

	//-------------------------------------
	//Note: when there is non-collapsible variable, 
	//      its associated d_k
	//      is replaced with actual total categories 
	bool b_success_categorize = categorize_cpp(x, nrow, ncol, d_k, z, 
											   NonCollapsible_categorical);


	if(!b_success_categorize) 
	{
		//early deallocation 
		delete[] d_k_Collapsible; 
		
		return 0;
	}

	//testout
//   	RPrint("After initial categorize()  \n"); 

	//RPrint("d_k: \n"); RPrint(d_k,  ncol);

	//RPrint("z: \n"); RPrint(z, nrow, ncol);


	//----------
	//clear category matrix for possible garbage
	//Note: z has only positive integer as category #
	//----------
	/*for(int i=0; i<nrow; i++)
	{
		for(int j=0; j<ncol; j++)
		{
			if(fabs_FHDI(z[i][j] < 1e-3)) z[i][j] = 0.0; 
		}
	}
	*/

	
	//-------------------------------------

	//make a copy of z

	//-------------------------------------

	double** zbase = New_dMatrix(nrow, ncol);

	Copy_dMatrix(z, nrow, ncol, zbase);

		

	//-------------------------------------

	//sort in the order of high missing rate

	//at the end, i_orn has the "actual" column numbers from highest missing rate

	//            to the lowest missing rate

	//-------------------------------------

	int* i_orn = new int[ncol]; 	 	Fill_iVector(i_orn, ncol, 0);

	int* i_orn_temp = new int[ncol]; 	Fill_iVector(i_orn_temp, ncol, 0);

	int* i_orn_temp2 = new int[ncol]; 	Fill_iVector(i_orn_temp2, ncol, 0);



	int i_temp = 0; 

	for(int i_col=0; i_col<ncol; i_col++)

	{

		i_temp = 0;

		for(int i_row=0; i_row<nrow; i_row++)

		{

			if(fabs_FHDI(z[i_row][i_col]) < 1e-5) //count only  "0"

			{ i_temp++; }

		}

		i_orn_temp[i_col] = i_temp; //store how many "0" in this column

	}

	Copy_iVector(i_orn_temp, ncol, i_orn_temp2); //store before sorting 

	//std::sort(&i_orn_temp[0], &i_orn_temp[ncol-1]); //this works well, but not recommended

	std::sort(i_orn_temp, i_orn_temp+ncol);  	

	

	for(int i=0; i<ncol; i++)

	{

		i_temp = i_orn_temp[ncol-1-i]; //reversed searching since the "sort" occurred in ascending order

		for(int j=0; j<ncol; j++)

		{

			if(i_temp == i_orn_temp2[j])

			{

				i_orn[i] = j+1; //store column number (actual number, 1, 2, ...) 

				i_orn_temp2[j] = -1; //not to be found again 

				break;

			}

		}

	}

	

	//-------------------------------------

	//create concatenated vector of z

	//-------------------------------------

	//Note: after Zmat..() all of the below variables are updated 

	//-------------------------------------

	//std::string cn[nrow]; //declaration of concatenated string vector of z

	std::string *cn = new std::string[nrow]; //declaration of concatenated string vector of z

	int* ml = new int[nrow];

	int* ol = new int[nrow];

	double** uox = New_dMatrix(nrow, ncol);

	double** mox = New_dMatrix(nrow, ncol);

	int i_count_ol;

	int i_count_ml; 

	int i_count_uox;

	int i_count_mox; 



	std::vector<int> v_nD; 

	List_FHDI List_nU(nrow); //default for the size of nrow, but will be updated in the main loop

	int* tnU = new int[nrow]; Fill_iVector(tnU, nrow, 0); //this default size will be udpated in the main loop

	int i_cellmake = 1; // actiavte the b_success_nDAU because cell make may not have enough donors

	//============================================

	//============================================

	//Main Loop to update z by merging algorithm

	//============================================

	//============================================

	int i_loop = 0; 

	for(i_loop = 0; i_loop<n_max_iteration; i_loop++)

	{

		//testout

		//RPrint("==============================");

		//RPrint("Main loop of Cell_Make.. i+1: "); RPrint(i_loop+1);

		

		bool b_DEBUG_Zmat = false; 

		if(i_loop == -5) b_DEBUG_Zmat = true;


		//----------
		//clear category matrix for possible garbage
		//Note: z has only positive integer as category #
		//----------
		/*for(int i=0; i<nrow; i++)
		{
			for(int j=0; j<ncol; j++)
			{
				if(fabs_FHDI(z[i][j] < 1e-3)) z[i][j] = 0.0; 
			}
		}
		*/
		

		//-----------

		//var; ref of Drs Im, Kim, and Fuller

		//ml = A_M, actual numbers of rows containing missing units

		//ol = A_R, actual numbers of rows having the fully observed units 

		//-----------

		//uox = sorted unique categorized patterns: e.g., a1b, a2c, c1f, d44, d45, etc.

		//mox = sorted unique categorized patterns: e.g., a00, a01, b10, c00, d01, etc.  

		//-----------

		Zmat_Extension_cpp(z, nrow, ncol, cn, 

								ml, ol, i_count_ol, i_count_ml,  

								uox, mox, i_count_uox, i_count_mox,

								b_DEBUG_Zmat);

		

		if(i_count_ml <= 0 || i_count_ol <= 0)

		{ 
			Rprintf("ERROR! i_count_ml or _ol is zero! Change k, check data quality, further break down categorical variables, or so. It may help \n"); 

			//early deallocaiton -----------------
			delete[] d_k_Collapsible; 
			delete[] cn; 
			delete[] ml;
			delete[] ol;
			delete[] tnU;
			Del_dMatrix(zbase, nrow, ncol);
			Del_dMatrix(uox, nrow, ncol);
			Del_dMatrix(mox, nrow, ncol);
			delete[] i_orn;
			delete[] i_orn_temp;
			delete[] i_orn_temp2;
			
			return 0;
		}


		//----------
		//clear category matrix for possible garbage
		//Note: z has only positive integer as category #
		//----------
		/*for(int i=0; i<nrow; i++)
		{
			for(int j=0; j<ncol; j++)
			{
				if(fabs_FHDI(z[i][j] < 1e-3)) z[i][j] = 0.0; 
			}
		}
		*/


	

		//------------------------------------------

		//generate number of donors nD[]

		// List of observed cells serving as donors List_nU: row numbers of donors per each missing row

		// Table of nU, tnU: total number of all donors for each missing row 

		//------------------------------------------

		//re-initialize tnU and List_nU and v_nD

		List_nU.initialize(i_count_mox);

		v_nD = std::vector<int>(); 

	    tnU = NULL; tnU = new int[i_count_uox]; Fill_iVector(tnU, i_count_uox, 0);

		

		bool b_DEBUG_nDAU = 0; 

		//if(i_loop>9) b_DEBUG_nDAU = 1;

		

		bool b_success_nDAU = nDAU_cpp(uox, mox, i_count_uox, i_count_mox, ncol,

				 cn, ol, i_count_ol, i_cellmake,

				 v_nD, List_nU, tnU, b_DEBUG_nDAU); 	

		//testout

		//RPrint("nDAU_... has been done");

		if(!b_success_nDAU)
		{			
			Rprintf("Error! nDAU Failed! Change k, check data quality, further break down categorical variables, or so. It may help \n");

			//early deallocaiton -----------------
			delete[] d_k_Collapsible; 
			delete[] cn; 
			delete[] ml;
			delete[] ol;
			delete[] tnU;
			Del_dMatrix(zbase, nrow, ncol);
			Del_dMatrix(uox, nrow, ncol);
			Del_dMatrix(mox, nrow, ncol);
			delete[] i_orn;
			delete[] i_orn_temp;
			delete[] i_orn_temp2;
			
			return 0; //abnormal ending 								
		}


		//================================
		//When there are at least one Non-Collapsible Categorical Variable exists
		//Skip Cell-Collapse procedure 
		//as of 2018, 04 21
		//INACTIVATED 2018, 04 25
		//================================
		/*
		if(i_NonCollapsible_categ_total >= 1)
		{
			Rprintf("There is at least one Non-Collapsible categorical variable! \n");
			Rprintf("So, the automatic cell-collapse won't take place. \n");
			Rprintf("This may affect the following FHDI/FEFI imputation's convergence. \n");
			Rprintf("If converged imputation is of top interest, (categorical=NULL) may help. \n");
			break;
			
		}
		*/

		
		
		
		
		//================================

		//Check whether there exist enough fully observed units for current 

		//"k" categories 

		//If not, reduce k to (k-1) categories at a specific position

		//================================

		int i_nD_sum = 0; //summation of all nD[] 

		for(unsigned i=0; i<v_nD.size(); i++) {if(v_nD[i] < 2) i_nD_sum += v_nD[i]; }

		//when too small number of donors

		if(i_count_uox == 2 && i_nD_sum >0)

		{

			//testout
			Rprintf(" Special case for small donors!  \n");

			//-----

			//max of k[ncol]: maximum category number

			//-----

			//double max_k = 0.0; for(int i=0; i<ncol; i++) {if(max_k <k[i]) max_k = k[i];}

			//double max_k = max_FHDI(d_k, ncol);
			double max_k = 0.0;
			double max_k_Collapsible = 0.0; //max of k among only collapsible variables
			//-------------------
			//when all variables are collapsible 
			//-------------------
			if(i_NonCollapsible_categ_total == 0)
			{
				max_k = max_FHDI(d_k, ncol);
			}
			//---------
			//when there is non-collapsible categorical variable
			//---------
			if(i_NonCollapsible_categ_total >= 1)
			{
				max_k_Collapsible = max_FHDI(d_k_Collapsible, ncol);
			}
			
			//testout
			/*RPrint("within Special case \n"); 
			RPrint("i_NonCollapsible_categ_total: "); RPrint(i_NonCollapsible_categ_total); 
			RPrint("max_k: "); RPrint(max_k); 
			RPrint("d_k[]: "); RPrint(d_k, ncol); 
			RPrint("max_k_Collapsible: "); RPrint(max_k_Collapsible); 
			RPrint("d_k_Collapsible[]: "); RPrint(d_k_Collapsible, ncol); 
			*/
			
			std::vector<int> v_maxk; 

			//which(d_k, ncol, max_k, v_maxk); //ACTUAL locations of columns that have the max category k

			//-------------------
			//when all variables are collapsible 
			//-------------------
			if(i_NonCollapsible_categ_total == 0)
			{
				which(d_k, ncol, max_k, v_maxk); //ACTUAL locations of columns that have the max category k
			}
			//---------
			//when there is non-collapsible categorical variable
			//---------
			if(i_NonCollapsible_categ_total >= 1)
			{
				which(d_k_Collapsible, ncol, max_k_Collapsible, v_maxk); //ACTUAL locations of columns that have the max category k
			}

			
			//-----

			//get some of orn of which location is the same as the maxk 

			//-----

			int n_orm = (int)v_maxk.size(); 

			int* i_orm = new int[n_orm]; 

			for(int j=0; j<n_orm; j++) i_orm[j] = i_orn[v_maxk[j]-1]; //Note: actual loc of column

			

			//-----

			//get the first column that has the min(i_orm)

			//-----

			int min_orm = min_FHDI(i_orm, n_orm); 

			//for(int j=0; j<n_orm; j++) {if(min_orm>i_orm[j]) min_orm = i_orm[j];} 

			std::vector<int> v_orm; 

			which(i_orm, n_orm, min_orm, v_orm); //ACTUAL locations of columns that have the min orm

			int i_mc = v_orm[0]; //the first cell that has the min orn

			
			//testout
			//RPrint("i_mc: "); RPrint(i_mc); 

			//------

			//re-categorize with the reduced category (k-1)

			//------

			double* d_x_temp = new double[nrow]; //a column corresponding to min_orm

			double* d_z_temp = new double[nrow]; //new column with categorized values

			for(int i=0; i<nrow; i++) d_x_temp[i] = x[i][i_mc-1]; //Note: actual loc in i_mc

			double d_k_one_dummy = d_k[i_mc -1];
			
			//b_success_categorize = categorize_cpp(d_x_temp, nrow, d_k[i_mc -1], d_z_temp); 
			
			int NonCollapsible_categorical_1 = NonCollapsible_categorical[i_mc-1];
			b_success_categorize = categorize_cpp(d_x_temp, nrow, d_k_one_dummy, d_z_temp,
			                                      NonCollapsible_categorical_1); 

			//testout
			//RPrint("within Special case, after single column categorize() \n"); 
			//RPrint("NonCollapsible_categorical_1: "); RPrint(NonCollapsible_categorical_1); 
			//RPrint("d_k_one_dummy: "); RPrint(d_k_one_dummy); 
												  
			if(!b_success_categorize) 
			{
				//early deallocaiton -----------------
				delete[] d_k_Collapsible; 
				delete[] cn; 
				delete[] ml;
				delete[] ol;
				delete[] tnU;
				Del_dMatrix(zbase, nrow, ncol);
				Del_dMatrix(uox, nrow, ncol);
				Del_dMatrix(mox, nrow, ncol);
				delete[] i_orn;
				delete[] i_orn_temp;
				delete[] i_orn_temp2;
				
				delete[] i_orm; 
				delete[] d_x_temp; 
				delete[] d_z_temp;
				
				return 0; 
			}

			//if this column is collapsible 
			if(NonCollapsible_categorical[i_mc-1] == 0)
			{
				d_k[i_mc -1] = d_k_one_dummy ; //this k value may have been updated for automatic categorical var.
				d_k_Collapsible[i_mc-1] = d_k_one_dummy; 
			}
			//----------
			//clear category matrix for possible garbage
			//Note: d_z_temp has only positive integer as category #
			//----------
			for(int i=0; i<nrow; i++)
			{ if(fabs_FHDI(d_z_temp[i] < 1e-3)) d_z_temp[i] = 0.0; }
			
			
			
			//-----

			//update zbase's one column with the reduced category

			//-----
			//if this column is collapsible 
			if(NonCollapsible_categorical[i_mc-1] == 0)
			{
				for(int i=0; i<nrow; i++) zbase[i][i_mc-1] = d_z_temp[i]; //Note: actual loc in i_mc
			}

			//testout
			//RPrint("In special case, i_mc : "); RPrint(i_mc); 
			//RPrint("d_z_temp[] : "); RPrint(d_z_temp, nrow); 
			
			//-----
			//check too small category number error (April 2018)
			//-----
			//if this column is collapsible 
			if(NonCollapsible_categorical[i_mc-1] == 0)
			{			
				if( fabs_FHDI(d_k[i_mc-1] - 1) < 1.0)
				{
					{
						Rprintf("Error! There is not enough observed units or categories. Change k or break down category; it may help  \n "); 
						return 0;
					}
				}
			}
			
			//-----
			//reduce the previous category number
			//for the ease of category condensation
			//-----
			//if this column is collapsible 
			if(NonCollapsible_categorical[i_mc-1] == 0)
			{			
				d_k[i_mc-1] = d_k[i_mc-1] - 1;  
				d_k_Collapsible[i_mc-1] = d_k_Collapsible[i_mc-1] - 1; 
			}

			

			//------

			//check Abort condition by looking at min of k

			//------

			int min_k_new = min_FHDI(d_k, ncol); 

			//for(int j=0; j<ncol; j++) {if(min_k_new>k[j]) min_k_new = k[j];}

			if(min_k_new < 2) 

			{Rprintf("There is not enough observed units in the original data. Thus, automatic cell-collapse has been done!   \n"); 
			 break;}

			//MUST ACTIVATE BREAK after adding a LOOP !!!!!

			
			//----------
			//clear category matrix for possible garbage
			//Note: zbase has only positive integer as category #
			//----------
			for(int i=0; i<nrow; i++)
			{
				for(int j=0; j<ncol; j++)
				{
					if(fabs_FHDI(zbase[i][j] < 1e-3)) zbase[i][j] = 0.0; 
				}
			}

			

			//-----

			//update with new reduced data

			//-----
			

			Copy_dMatrix(zbase, nrow, ncol, z);

			

			//-----

			//re-initialize before calling Zmat_...()

			//-----

			Fill_iVector(ml, nrow, 0);

			Fill_iVector(ol, nrow, 0);

			Fill_dMatrix(uox, nrow, ncol, 0.0);

			Fill_dMatrix(mox, nrow, ncol, 0.0);

			i_count_ol = 0;

			i_count_ml = 0; 

			i_count_uox = 0;

			i_count_mox = 0;

			v_nD = std::vector<int>(); 

			//----------
			//clear category matrix for possible garbage
			//Note: z has only positive integer as category #
			//----------
			/*for(int i=0; i<nrow; i++)
			{
				for(int j=0; j<ncol; j++)
				{
					if(fabs_FHDI(z[i][j] < 1e-3)) z[i][j] = 0.0; 
				}
			}*/
			

			Zmat_Extension_cpp(z, nrow, ncol, cn, 

								ml, ol, i_count_ol, i_count_ml,  

								uox, mox, i_count_uox, i_count_mox,

								b_DEBUG_Zmat);

		

			if(i_count_ml <= 0 || i_count_ol <= 0)
			{ 
				Rprintf("ERROR! i_count_ml or _ol is zero!   \n");

				Rprintf("Change k, further break down categorical variables, or check data quality \n");		

				//early deallocaiton -----------------
				delete[] d_k_Collapsible; 
				delete[] cn; 
				delete[] ml;
				delete[] ol;
				delete[] tnU;
				Del_dMatrix(zbase, nrow, ncol);
				Del_dMatrix(uox, nrow, ncol);
				Del_dMatrix(mox, nrow, ncol);
				delete[] i_orn;
				delete[] i_orn_temp;
				delete[] i_orn_temp2;
				
				delete[] i_orm; 
				delete[] d_x_temp; 
				delete[] d_z_temp;
				
				return 0;
			}

			

			//------------------------------------------

			//generate number of donors nD[]

			// List of observed cells serving as donors List_nU

			// Table of nU tnU

			//NOTE: for large data with many variables, 

			//      The case of no possible donors may arise 

			//      If so, reduction of k begins, say with (k-1)

			//------------------------------------------

			//re-initialize tnU and List_nU and v_nD

			List_nU.initialize(i_count_mox);

			v_nD = std::vector<int>(); 

			tnU = NULL; tnU = new int[i_count_uox]; Fill_iVector(tnU, i_count_uox, 0);

			

			b_success_nDAU =  nDAU_cpp(uox, mox, i_count_uox, i_count_mox, ncol,

					 cn, ol, i_count_ol, i_cellmake,

					 v_nD, List_nU, tnU, b_DEBUG_nDAU); 			

			if(!b_success_nDAU)
			{			
				Rprintf("Error! nDAU Failed! Change k, check data quality, further break down categorical variables, or so. It may help \n");

				//early deallocaiton -----------------
				delete[] d_k_Collapsible; 
				delete[] cn; 
				delete[] ml;
				delete[] ol;
				delete[] tnU;
				Del_dMatrix(zbase, nrow, ncol);
				Del_dMatrix(uox, nrow, ncol);
				Del_dMatrix(mox, nrow, ncol);
				delete[] i_orn;
				delete[] i_orn_temp;
				delete[] i_orn_temp2;
				
				delete[] i_orm; 
				delete[] d_x_temp; 
				delete[] d_z_temp;
				
				return 0; //abnormal ending 								
			}
			

			//-----

			//local deallocation

			//-----

			v_maxk = std::vector<int>();

			v_orm  = std::vector<int>();

			delete[] i_orm; 

			delete[] d_x_temp; 

			delete[] d_z_temp; 

		} //end of Special case of too small donors 



		//-----

		//find the first location having min of nD[], i.e. minimum donors

		//-----

		int i_min_nD = min_FHDI(v_nD); 

		int i_reci = 0;

		for(int i=0; i<(int)v_nD.size();i++) 

		{if(v_nD[i] == i_min_nD){ i_reci = i; break;}} 

		

		

		

		



		//===========================

		//===========================

		//Merge z 

		//===========================

		//===========================

		//testout 

		bool b_DEBUG_Merge = false; 

		if(i_loop ==-3) b_DEBUG_Merge = true; 

		
		//----------
		//clear category matrix for possible garbage
		//Note: z has only positive integer as category #
		//----------
		/*for(int i=0; i<nrow; i++)
		{
			for(int j=0; j<ncol; j++)
			{
				if(fabs_FHDI(z[i][j] < 1e-3)) z[i][j] = 0.0; 
			}
		}*/
		//------------------
		//when there is at least one non-collapsible categorical variable
		//skip the merge procedure 
		//2018, 0426
		//------------------
		if(i_NonCollapsible_categ_total>=1 && v_nD[i_reci] < 2)
		{
			RPrint("The current data set does not have enough donors while there is at least one non-collapsible categorical variable! \n");
			RPrint("Thus, auto merging procedure won't take place! \n"); 
			break; 
		}
		
		if(v_nD[i_reci] < 2) //if donors are less than 2, do MERGE

		{

			FHDI::Merge_Extension_cpp(i_reci, uox, i_count_uox, 

							mox, i_count_mox, tnU, 

							cn,  ol, i_count_ol,

							z, nrow, ncol, 

							i_merge,

							b_DEBUG_Merge);

		}

		if(v_nD[i_reci] >=2) //if more than 2 donors exist, exit

		{ break; } //finish main loop 

		

		

		

		//----

		//unconverged ending

		//-----

		if(i_loop == n_max_iteration-1) 

		{ Rprintf(" reached n_max_iteration after step ");

			Rprintf("%d ", n_max_iteration);
			
			Rprintf(" Change k, check data quality, further break down categorical variables, or so. It may help ");

			//early deallocaiton -----------------
			delete[] d_k_Collapsible; 
			delete[] cn; 
			delete[] ml;
			delete[] ol;
			delete[] tnU;
			Del_dMatrix(zbase, nrow, ncol);
			Del_dMatrix(uox, nrow, ncol);
			Del_dMatrix(mox, nrow, ncol);
			delete[] i_orn;
			delete[] i_orn_temp;
			delete[] i_orn_temp2;
				

			return 0; 

		}

	} //end of main loop

	//testout

	Rprintf("converged in Cell_Make after iterations: "); Rprintf("%d ", i_loop+1);

	


	//----------
	//clear uox and mox matrix for possible garbage
	//Note: Must have only positive integer as category #
	//----------
	for(int i=0; i<i_count_uox; i++)
	{
		for(int j=0; j<ncol; j++)
		{
			if(fabs_FHDI(uox[i][j] < 1e-3)) uox[i][j] = 0.0; 
		}
	}
	for(int i=0; i<i_count_mox; i++)
	{
		for(int j=0; j<ncol; j++)
		{
			if(fabs_FHDI(mox[i][j] < 1e-3)) mox[i][j] = 0.0; 
		}
	}




	//--------------

	//prepare separate output of Cell Make

	//--------------

	double* d_temp_um = new double[ncol]; 

	for(int i=0; i<i_count_uox; i++)

	{

		for(int j=0; j<ncol; j++) d_temp_um[j] = uox[i][j]; 

		

		rbind_uox_CellMake.append_block(d_temp_um); 

	}

	for(int i=0; i<i_count_mox; i++)

	{

		for(int j=0; j<ncol; j++) d_temp_um[j] = mox[i][j]; 

		

		rbind_mox_CellMake.append_block(d_temp_um); 

	}	

	delete[] d_temp_um; 

	

	

	//testout

	//RPrint(" ========= Cell_Make_Extension.. has successfully finished!");

	Rprintf(" ========= FHDI_CellMake has successfully finished!\n");

	

	//-------------------------------------

	//Deallocation

	//-------------------------------------
	delete[] d_k_Collapsible; 
	
	delete[] cn; 

	delete[] i_orn;

	delete[] i_orn_temp;

	delete[] i_orn_temp2;

	delete[] ml;

	delete[] ol;

	delete[] tnU;



	Del_dMatrix(zbase, nrow, ncol);

	Del_dMatrix(uox, nrow, ncol);

	Del_dMatrix(mox, nrow, ncol);

	

	

	return 1;

}



} //end of namespace

//Fn===========================================================================

//Cell_Make_Extension_Bigp_cpp.cc-----------------------------------------------------------------------------

//Fn===========================================================================

namespace FHDI {
	//#include "Merge_Extension_cpp_MPI.cc"
	//#include "Merge_Extension_Bigp_cpp_MPI.cc"
	//#include "Ranking_m.cc"
	bool Cell_Make_Extension_Bigp_cpp(double** x, int** r, const int nrow, const int ncol, double* d_k,

		int* NonCollapsible_categorical,

		double** z,

		int** codes,

		rbind_FHDI &rbind_uox_CellMake,

		rbind_FHDI &rbind_mox_CellMake,

		const int i_merge, const int i_option_collapsing, const int i_option_SIS_type, int top)

		//Description=========================================

		// make cells with the raw data matrix x 

		// categorization takes place 

		// according to the given number of categories stored in d_k(ncol)

		//

		// Algorithm:  for categorization

		// perc: percentiles used to get quantiles, determined by k

		// quan: quantiles if k=4, we quan=(Q1,Q2,Q3) have Q1(=1/4), Q2 (=Median) and Q3(=3/4)

		// 

		// Note: as of Oct 2016, NA values (missing data) is marked by a long integer at the parent "r" code

		//

		// original R code: Dr. Im, J. and Dr. Kim, J. 

		// c++ code: 		Yicheng Yang

		// All rights reserved

		// 

		// updated: Feb 23, 2020

		//----------------------------------------------------

		//IN	: double x(nrow, ncol) 	= {y1, y2, ... } total data containing missing values

		//IN	: double d_k(ncol)		= a vector of categories of each column of xalloc

		//IN    : int NonCollapsible_categorical(nrol) = {0,0, .., 1,.. 0} 
		//				index for non-collapsible categorical variables. 
		//				when at least one column has "1" skip cell-collapse procedure
		//				this may casue a potential error of lack of enough donor! 
		//				(2018, 04 21) 
		//											  

		//OUT   : double z(nrow, ncol)  = catorized matrix corresponding to original matrix x

		//                                initialized with 0.0 

		//OUT	: rbind_FHDI rbind_uox_CellMake(ncol); //compact storage of uox, unique observed rows sorted in the ascending order

		//OUT	: rbind_FHDI rbind_mox_CellMake(ncol); //compact storage of mox, unique observed rows sorted in the ascending order

		//

		//IN    : int i_merge = random donor selection in Merge algorithm in Cell Make

		//                            0= no random seed number setting

		//						      1= random seed number setting 

		//IN    : int i_option_collapsing = choice of big-p algorithm 

		//                            0= no big-p algorithms

		//                           !0= perform big-p algorithms

		//IN    : int i_option_SIS_type = 1: SIS with intersection 

		//                           2: SIS with union 

		//                           3: SIS with global ranking 

		//OUT   : int codes(nrow, i_option_collapsing); // storage to record most correlated variables of mox
		//====================================================

	{

		const int n_max_iteration = nrow * 2; //maximum number of iterations 

											  //const int n_max_iteration = 2;  //temporary

											  //-------------------------------------
											  //Determine if there is Non-Collapsible Categorical variable
											  //-------------------------------------
		double* d_k_Collapsible = new double[ncol]; //k for collapsible variables only 
		Copy_dVector(d_k, ncol, d_k_Collapsible);

		int i_NonCollapsible_categ_total = 0;
		for (int i = 0; i<ncol; i++)
		{
			i_NonCollapsible_categ_total += NonCollapsible_categorical[i];

			if (NonCollapsible_categorical[i] == 0) d_k_Collapsible[i] = d_k[i]; //use user-defined k 
			if (NonCollapsible_categorical[i] == 1) d_k_Collapsible[i] = 1; //will be overwritten by actual total categories
		}

		//testout
		/*RPrint("initial d_k[]"); RPrint(d_k, ncol);
		RPrint("\n");
		RPrint("d_k_Collapsible[]"); RPrint(d_k_Collapsible, ncol);
		RPrint("\n");
		RPrint("NonCollapsible_categorical[]"); RPrint(NonCollapsible_categorical, ncol);
		RPrint("\n");
		*/


		//-------------------------------------

		//Categorize raw data

		//-------------------------------------
		//Note: when there is non-collapsible variable, 
		//      its associated d_k
		//      is replaced with actual total categories 
		bool b_success_categorize = categorize_cpp(x, nrow, ncol, d_k, z,
			NonCollapsible_categorical);

		//TestOut << " z matrix from categorize" << endl;
		//RPrint(z, nrow, ncol, TestOut);

		if (!b_success_categorize)
		{
			//early deallocation 
			delete[] d_k_Collapsible;

			return 0;
		}

		//testout
		//   	RPrint("After initial categorize()  \n"); 

		//RPrint("d_k: \n"); RPrint(d_k,  ncol);

		//RPrint("z: \n"); RPrint(z, nrow, ncol);


		//----------
		//clear category matrix for possible garbage
		//Note: z has only positive integer as category #
		//----------
		/*for(int i=0; i<nrow; i++)
		{
		for(int j=0; j<ncol; j++)
		{
		if(fabs_FHDI(z[i][j] < 1e-3)) z[i][j] = 0.0;
		}
		}
		*/


		//-------------------------------------

		//make a copy of z

		//-------------------------------------

		double** zbase = New_dMatrix(nrow, ncol);

		Copy_dMatrix(z, nrow, ncol, zbase);



		//-------------------------------------

		//sort in the order of high missing rate

		//at the end, i_orn has the "actual" column numbers from highest missing rate

		//            to the lowest missing rate

		//-------------------------------------

		int* i_orn = new int[ncol]; 	 	Fill_iVector(i_orn, ncol, 0);

		int* i_orn_temp = new int[ncol]; 	Fill_iVector(i_orn_temp, ncol, 0);

		int* i_orn_temp2 = new int[ncol]; 	Fill_iVector(i_orn_temp2, ncol, 0);



		int i_temp = 0;

		for (int i_col = 0; i_col<ncol; i_col++)

		{

			i_temp = 0;

			for (int i_row = 0; i_row<nrow; i_row++)

			{

				if (fabs_FHDI(z[i_row][i_col]) < 1e-5) //count only  "0"

				{
					i_temp++;
				}

			}

			i_orn_temp[i_col] = i_temp; //store how many "0" in this column

		}

		Copy_iVector(i_orn_temp, ncol, i_orn_temp2); //store before sorting 

													 //std::sort(&i_orn_temp[0], &i_orn_temp[ncol-1]); //this works well, but not recommended

		std::sort(i_orn_temp, i_orn_temp + ncol);



		for (int i = 0; i<ncol; i++)

		{

			i_temp = i_orn_temp[ncol - 1 - i]; //reversed searching since the "sort" occurred in ascending order

			for (int j = 0; j<ncol; j++)

			{

				if (i_temp == i_orn_temp2[j])

				{

					i_orn[i] = j + 1; //store column number (actual number, 1, 2, ...) 

					i_orn_temp2[j] = -1; //not to be found again 

					break;

				}

			}

		}



		//-------------------------------------

		//create concatenated vector of z

		//-------------------------------------

		//Note: after Zmat..() all of the below variables are updated 

		//-------------------------------------

		//std::string cn[nrow]; //declaration of concatenated string vector of z

		std::string *cn = new std::string[nrow]; //declaration of concatenated string vector of z

		int* ml = new int[nrow];

		int* ol = new int[nrow];

		double** uox = New_dMatrix(nrow, ncol);

		double** mox = New_dMatrix(nrow, ncol);

		int i_count_ol;

		int i_count_ml;

		int i_count_uox;

		int i_count_mox;



		std::vector<int> v_nD;

		List_FHDI List_nU(nrow); //default for the size of nrow, but will be updated in the main loop

		int* tnU = new int[nrow]; Fill_iVector(tnU, nrow, 0); //this default size will be udpated in the main loop

		int i_cellmake = 1; // actiavte the b_success_nDAU because cell make may not have enough donors

		//============================================

		//============================================

		//Main Loop to update z by merging algorithm

		//============================================

		//============================================

		//--------------------------------------
		//Compute correlation ranking matrix 
		//-----------------------------------------
		// Ranking of correlation of each variable in descending order. Note it excludes itself from ranking

		//double** correlation_yicheng = New_dMatrix(ncol, ncol);
		//int** correlation_ranking = New_iMatrix(ncol, (ncol - 1));
		//FHDI::Ranking_m(nrow, ncol, x, r, correlation_yicheng, correlation_ranking);

		//----------------
		//Prepare fully observed y matrix
		//---------------------
		std::vector<int> ol_temp;
		int d_temp = 0;
		for (int i_row = 0; i_row < nrow; i_row++)
		{
			d_temp = 1.0;
			for (int i_col = 0; i_col < ncol; i_col++)
			{
				if (r[i_row][i_col] == 0) { d_temp = 0.0; break; } //found zero, i.e. missing cell
			}

			if (fabs(d_temp) > 1e-15) //this row has no missing cells
			{
				ol_temp.push_back(i_row);
			} //actual number of the row having no missing cells
		}
		int nrow_ol = ol_temp.size();

		double** ol_matrix = New_dMatrix(nrow_ol, ncol);

		for (int i = 0;i < nrow_ol;i++) {
			for (int j = 0; j < ncol; j++) {
				ol_matrix[i][j] = x[ol_temp[i]][j];
			}
		}


		//int top = 100; // Top "top" rankings exclude itself. Note that  i_option_collapsing < top <= ncol-1
		if ((ncol - 1) < top) {
			top = ncol - 1;
		}

		int** correlation_ranking_top = New_iMatrix(ncol, top);

		FHDI::Ranking_top(nrow_ol, ncol, top, ol_matrix, correlation_ranking_top);

		//----------------------------------------

		int i_loop = 0;

		for (i_loop = 0; i_loop<n_max_iteration; i_loop++)

		{

			//testout

			//RPrint("==============================");

			//RPrint("Main loop of Cell_Make.. i+1: "); RPrint(i_loop+1);
			//cout << "Main loop of Cell_Make at iteration " << i_loop + 1 << endl;


			bool b_DEBUG_Zmat = false;

			if (i_loop == -5) b_DEBUG_Zmat = true;


			//----------
			//clear category matrix for possible garbage
			//Note: z has only positive integer as category #
			//----------
			/*for(int i=0; i<nrow; i++)
			{
			for(int j=0; j<ncol; j++)
			{
			if(fabs_FHDI(z[i][j] < 1e-3)) z[i][j] = 0.0;
			}
			}
			*/


			//-----------

			//var; ref of Drs Im, Kim, and Fuller

			//ml = A_M, actual numbers of rows containing missing units

			//ol = A_R, actual numbers of rows having the fully observed units 

			//-----------

			//uox = sorted unique categorized patterns: e.g., a1b, a2c, c1f, d44, d45, etc.

			//mox = sorted unique categorized patterns: e.g., a00, a01, b10, c00, d01, etc.  

			//-----------

			Zmat_Extension_cpp(z, nrow, ncol, cn,

				ml, ol, i_count_ol, i_count_ml,

				uox, mox, i_count_uox, i_count_mox,

				b_DEBUG_Zmat);



			if (i_count_ml <= 0 || i_count_ol <= 0)

			{
				Rprintf("ERROR! i_count_ml or _ol is zero! Change k, check data quality, further break down categorical variables, or so. It may help \n");

				//early deallocaiton -----------------
				delete[] d_k_Collapsible;
				delete[] cn;
				delete[] ml;
				delete[] ol;
				delete[] tnU;
				Del_dMatrix(zbase, nrow, ncol);
				Del_dMatrix(uox, nrow, ncol);
				Del_dMatrix(mox, nrow, ncol);
				delete[] i_orn;
				delete[] i_orn_temp;
				delete[] i_orn_temp2;

				return 0;
			}


			//----------
			//clear category matrix for possible garbage
			//Note: z has only positive integer as category #
			//----------
			/*for(int i=0; i<nrow; i++)
			{
			for(int j=0; j<ncol; j++)
			{
			if(fabs_FHDI(z[i][j] < 1e-3)) z[i][j] = 0.0;
			}
			}
			*/




			//------------------------------------------

			//generate number of donors nD[]

			// List of observed cells serving as donors List_nU: row numbers of donors per each missing row

			// Table of nU, tnU: total number of all donors for each missing row 

			//------------------------------------------

			//re-initialize tnU and List_nU and v_nD

			List_nU.initialize(i_count_mox);

			v_nD = std::vector<int>(); // Note that size of v_nD is i_count_mox

			tnU = NULL; tnU = new int[i_count_uox]; Fill_iVector(tnU, i_count_uox, 0);



			bool b_DEBUG_nDAU = 0;

			//if(i_loop>9) b_DEBUG_nDAU = 1;

			//!!!!! Initialize codes before nDAU every loop
			for (int k1 = 0; k1 < nrow; k1++) {
				for (int k2 = 0; k2 < i_option_collapsing; k2++) {
					codes[k1][k2] = 0;
				}
			}


			//TestOut << "nDAU_Bigp_cpp at i_loop " << i_loop << endl;
			bool b_success_nDAU = nDAU_Bigp_cpp(uox, mox, i_count_uox, i_count_mox, ncol, i_option_collapsing, i_option_SIS_type, 

				cn, ol, i_count_ol, top, i_cellmake,

				v_nD, List_nU, tnU, codes, correlation_ranking_top, ol_matrix, b_DEBUG_nDAU);
			//testout


			//RPrint("nDAU_... has been done");

			if (!b_success_nDAU)
			{
				Rprintf("Error! nDAU Failed! Change k, check data quality, further break down categorical variables, or so. It may help \n");

				//early deallocaiton -----------------
				delete[] d_k_Collapsible;
				delete[] cn;
				delete[] ml;
				delete[] ol;
				delete[] tnU;
				Del_dMatrix(zbase, nrow, ncol);
				Del_dMatrix(uox, nrow, ncol);
				Del_dMatrix(mox, nrow, ncol);
				delete[] i_orn;
				delete[] i_orn_temp;
				delete[] i_orn_temp2;

				return 0; //abnormal ending 								
			}

			//TestOut << "Yicheng-Codes at i_loop = " << i_loop << " where i_count_mox = " << i_count_mox << endl;

			//for (int kk2 = 0; kk2 < nrow; kk2++) {
			//	for (int kk3 = 0; kk3 < i_option_collapsing; kk3++) {
			//		TestOut << setw(20) << codes[kk2][kk3];
			//	}
			//	TestOut << endl;
			//}

			//================================
			//When there are at least one Non-Collapsible Categorical Variable exists
			//Skip Cell-Collapse procedure 
			//as of 2018, 04 21
			//INACTIVATED 2018, 04 25
			//================================
			/*
			if(i_NonCollapsible_categ_total >= 1)
			{
			Rprintf("There is at least one Non-Collapsible categorical variable! \n");
			Rprintf("So, the automatic cell-collapse won't take place. \n");
			Rprintf("This may affect the following FHDI/FEFI imputation's convergence. \n");
			Rprintf("If converged imputation is of top interest, (categorical=NULL) may help. \n");
			break;

			}
			*/





			//================================

			//Check whether there exist enough fully observed units for current 

			//"k" categories 

			//If not, reduce k to (k-1) categories at a specific position

			//================================

			int i_nD_sum = 0; //summation of all nD[] 

			for (unsigned i = 0; i<v_nD.size(); i++) { if (v_nD[i] < 2) i_nD_sum += v_nD[i]; }

			//when too small number of donors

			if (i_count_uox == 2 && i_nD_sum >0)

			{

				//testout
				Rprintf(" Special case for small donors!  \n");

				//-----

				//max of k[ncol]: maximum category number

				//-----

				//double max_k = 0.0; for(int i=0; i<ncol; i++) {if(max_k <k[i]) max_k = k[i];}

				//double max_k = max_FHDI(d_k, ncol);
				double max_k = 0.0;
				double max_k_Collapsible = 0.0; //max of k among only collapsible variables
												//-------------------
												//when all variables are collapsible 
												//-------------------
				if (i_NonCollapsible_categ_total == 0)
				{
					max_k = max_FHDI(d_k, ncol);
				}
				//---------
				//when there is non-collapsible categorical variable
				//---------
				if (i_NonCollapsible_categ_total >= 1)
				{
					max_k_Collapsible = max_FHDI(d_k_Collapsible, ncol);
				}

				//testout
				/*RPrint("within Special case \n");
				RPrint("i_NonCollapsible_categ_total: "); RPrint(i_NonCollapsible_categ_total);
				RPrint("max_k: "); RPrint(max_k);
				RPrint("d_k[]: "); RPrint(d_k, ncol);
				RPrint("max_k_Collapsible: "); RPrint(max_k_Collapsible);
				RPrint("d_k_Collapsible[]: "); RPrint(d_k_Collapsible, ncol);
				*/

				std::vector<int> v_maxk;

				//which(d_k, ncol, max_k, v_maxk); //ACTUAL locations of columns that have the max category k

				//-------------------
				//when all variables are collapsible 
				//-------------------
				if (i_NonCollapsible_categ_total == 0)
				{
					which(d_k, ncol, max_k, v_maxk); //ACTUAL locations of columns that have the max category k
				}
				//---------
				//when there is non-collapsible categorical variable
				//---------
				if (i_NonCollapsible_categ_total >= 1)
				{
					which(d_k_Collapsible, ncol, max_k_Collapsible, v_maxk); //ACTUAL locations of columns that have the max category k
				}


				//-----

				//get some of orn of which location is the same as the maxk 

				//-----

				int n_orm = (int)v_maxk.size();

				int* i_orm = new int[n_orm];

				for (int j = 0; j<n_orm; j++) i_orm[j] = i_orn[v_maxk[j] - 1]; //Note: actual loc of column



																			   //-----

																			   //get the first column that has the min(i_orm)

																			   //-----

				int min_orm = min_FHDI(i_orm, n_orm);

				//for(int j=0; j<n_orm; j++) {if(min_orm>i_orm[j]) min_orm = i_orm[j];} 

				std::vector<int> v_orm;

				which(i_orm, n_orm, min_orm, v_orm); //ACTUAL locations of columns that have the min orm

				int i_mc = v_orm[0]; //the first cell that has the min orn


									 //testout
									 //RPrint("i_mc: "); RPrint(i_mc); 

									 //------

									 //re-categorize with the reduced category (k-1)

									 //------

				double* d_x_temp = new double[nrow]; //a column corresponding to min_orm

				double* d_z_temp = new double[nrow]; //new column with categorized values

				for (int i = 0; i<nrow; i++) d_x_temp[i] = x[i][i_mc - 1]; //Note: actual loc in i_mc

				double d_k_one_dummy = d_k[i_mc - 1];

				//b_success_categorize = categorize_cpp(d_x_temp, nrow, d_k[i_mc -1], d_z_temp); 

				int NonCollapsible_categorical_1 = NonCollapsible_categorical[i_mc - 1];
				b_success_categorize = categorize_cpp(d_x_temp, nrow, d_k_one_dummy, d_z_temp,
					NonCollapsible_categorical_1);

				//testout
				//RPrint("within Special case, after single column categorize() \n"); 
				//RPrint("NonCollapsible_categorical_1: "); RPrint(NonCollapsible_categorical_1); 
				//RPrint("d_k_one_dummy: "); RPrint(d_k_one_dummy); 

				if (!b_success_categorize)
				{
					//early deallocaiton -----------------
					delete[] d_k_Collapsible;
					delete[] cn;
					delete[] ml;
					delete[] ol;
					delete[] tnU;
					Del_dMatrix(zbase, nrow, ncol);
					Del_dMatrix(uox, nrow, ncol);
					Del_dMatrix(mox, nrow, ncol);
					delete[] i_orn;
					delete[] i_orn_temp;
					delete[] i_orn_temp2;

					delete[] i_orm;
					delete[] d_x_temp;
					delete[] d_z_temp;

					return 0;
				}

				//if this column is collapsible 
				if (NonCollapsible_categorical[i_mc - 1] == 0)
				{
					d_k[i_mc - 1] = d_k_one_dummy; //this k value may have been updated for automatic categorical var.
					d_k_Collapsible[i_mc - 1] = d_k_one_dummy;
				}
				//----------
				//clear category matrix for possible garbage
				//Note: d_z_temp has only positive integer as category #
				//----------
				for (int i = 0; i<nrow; i++)
				{
					if (fabs_FHDI(d_z_temp[i] < 1e-3)) d_z_temp[i] = 0.0;
				}



				//-----

				//update zbase's one column with the reduced category

				//-----
				//if this column is collapsible 
				if (NonCollapsible_categorical[i_mc - 1] == 0)
				{
					for (int i = 0; i<nrow; i++) zbase[i][i_mc - 1] = d_z_temp[i]; //Note: actual loc in i_mc
				}

				//testout
				//RPrint("In special case, i_mc : "); RPrint(i_mc); 
				//RPrint("d_z_temp[] : "); RPrint(d_z_temp, nrow); 

				//-----
				//check too small category number error (April 2018)
				//-----
				//if this column is collapsible 
				if (NonCollapsible_categorical[i_mc - 1] == 0)
				{
					if (fabs_FHDI(d_k[i_mc - 1] - 1) < 1.0)
					{
						{
							Rprintf("Error! There is not enough observed units or categories. Change k or break down category; it may help  \n ");
							return 0;
						}
					}
				}

				//-----
				//reduce the previous category number
				//for the ease of category condensation
				//-----
				//if this column is collapsible 
				if (NonCollapsible_categorical[i_mc - 1] == 0)
				{
					d_k[i_mc - 1] = d_k[i_mc - 1] - 1;
					d_k_Collapsible[i_mc - 1] = d_k_Collapsible[i_mc - 1] - 1;
				}



				//------

				//check Abort condition by looking at min of k

				//------

				int min_k_new = min_FHDI(d_k, ncol);

				//for(int j=0; j<ncol; j++) {if(min_k_new>k[j]) min_k_new = k[j];}

				if (min_k_new < 2)

				{
					Rprintf("There is not enough observed units in the original data. Thus, automatic cell-collapse has been done!   \n");
					break;
				}

				//MUST ACTIVATE BREAK after adding a LOOP !!!!!


				//----------
				//clear category matrix for possible garbage
				//Note: zbase has only positive integer as category #
				//----------
				for (int i = 0; i<nrow; i++)
				{
					for (int j = 0; j<ncol; j++)
					{
						if (fabs_FHDI(zbase[i][j] < 1e-3)) zbase[i][j] = 0.0;
					}
				}



				//-----

				//update with new reduced data

				//-----


				Copy_dMatrix(zbase, nrow, ncol, z);



				//-----

				//re-initialize before calling Zmat_...()

				//-----

				Fill_iVector(ml, nrow, 0);

				Fill_iVector(ol, nrow, 0);

				Fill_dMatrix(uox, nrow, ncol, 0.0);

				Fill_dMatrix(mox, nrow, ncol, 0.0);

				i_count_ol = 0;

				i_count_ml = 0;

				i_count_uox = 0;

				i_count_mox = 0;

				v_nD = std::vector<int>();

				//----------
				//clear category matrix for possible garbage
				//Note: z has only positive integer as category #
				//----------
				/*for(int i=0; i<nrow; i++)
				{
				for(int j=0; j<ncol; j++)
				{
				if(fabs_FHDI(z[i][j] < 1e-3)) z[i][j] = 0.0;
				}
				}*/


				Zmat_Extension_cpp(z, nrow, ncol, cn,

					ml, ol, i_count_ol, i_count_ml,

					uox, mox, i_count_uox, i_count_mox,

					b_DEBUG_Zmat);



				if (i_count_ml <= 0 || i_count_ol <= 0)
				{
					Rprintf("ERROR! i_count_ml or _ol is zero!   \n");

					Rprintf("Change k, further break down categorical variables, or check data quality \n");

					//early deallocaiton -----------------
					delete[] d_k_Collapsible;
					delete[] cn;
					delete[] ml;
					delete[] ol;
					delete[] tnU;
					Del_dMatrix(zbase, nrow, ncol);
					Del_dMatrix(uox, nrow, ncol);
					Del_dMatrix(mox, nrow, ncol);
					delete[] i_orn;
					delete[] i_orn_temp;
					delete[] i_orn_temp2;

					delete[] i_orm;
					delete[] d_x_temp;
					delete[] d_z_temp;

					return 0;
				}



				//------------------------------------------

				//generate number of donors nD[]

				// List of observed cells serving as donors List_nU

				// Table of nU tnU

				//NOTE: for large data with many variables, 

				//      The case of no possible donors may arise 

				//      If so, reduction of k begins, say with (k-1)

				//------------------------------------------

				//re-initialize tnU and List_nU and v_nD

				List_nU.initialize(i_count_mox);

				v_nD = std::vector<int>();

				tnU = NULL; tnU = new int[i_count_uox]; Fill_iVector(tnU, i_count_uox, 0);



				bool b_success_nDAU = nDAU_Bigp_cpp(uox, mox, i_count_uox, i_count_mox, ncol, i_option_collapsing, i_option_SIS_type,

					cn, ol, i_count_ol, top, i_cellmake,

					v_nD, List_nU, tnU, codes, correlation_ranking_top, ol_matrix, b_DEBUG_nDAU);



				if (!b_success_nDAU)
				{
					Rprintf("Error! nDAU Failed! Change k, check data quality, further break down categorical variables, or so. It may help \n");

					//early deallocaiton -----------------
					delete[] d_k_Collapsible;
					delete[] cn;
					delete[] ml;
					delete[] ol;
					delete[] tnU;
					Del_dMatrix(zbase, nrow, ncol);
					Del_dMatrix(uox, nrow, ncol);
					Del_dMatrix(mox, nrow, ncol);
					delete[] i_orn;
					delete[] i_orn_temp;
					delete[] i_orn_temp2;

					delete[] i_orm;
					delete[] d_x_temp;
					delete[] d_z_temp;

					return 0; //abnormal ending 								
				}



				//-----

				//local deallocation

				//-----

				v_maxk = std::vector<int>();

				v_orm = std::vector<int>();

				delete[] i_orm;

				delete[] d_x_temp;

				delete[] d_z_temp;

			} //end of Special case of too small donors 



			  //-----

			  //find the first location having min of nD[], i.e. minimum donors

			  //-----


			int i_min_nD = min_FHDI(v_nD);

			int i_reci = 0;

			for (int i = 0; i<(int)v_nD.size();i++)

			{
				if (v_nD[i] == i_min_nD) { i_reci = i; break; }
			}











			//===========================

			//===========================

			//Merge z 

			//===========================

			//===========================

			//testout 

			bool b_DEBUG_Merge = false;

			if (i_loop == -3) b_DEBUG_Merge = true;


			//----------
			//clear category matrix for possible garbage
			//Note: z has only positive integer as category #
			//----------
			/*for(int i=0; i<nrow; i++)
			{
			for(int j=0; j<ncol; j++)
			{
			if(fabs_FHDI(z[i][j] < 1e-3)) z[i][j] = 0.0;
			}
			}*/
			//------------------
			//when there is at least one non-collapsible categorical variable
			//skip the merge procedure 
			//2018, 0426
			//------------------
			if (i_NonCollapsible_categ_total >= 1 && v_nD[i_reci] < 2)
			{
				Rprintf("The current data set does not have enough donors while there is at least one non-collapsible categorical variable! \n");
				Rprintf("Thus, auto merging procedure won't take place! \n");
				break;
			}

			//TestOut << "Merge_Extension_Bigp_cpp at i_loop " << i_loop << endl;
			if (v_nD[i_reci] < 2) //if donors are less than 2, do MERGE

			{

				FHDI::Merge_Extension_Bigp_cpp(i_reci, uox, i_count_uox,

					mox, i_count_mox, tnU,

					cn, ol, i_count_ol,

					z, nrow, ncol,

					i_merge, codes, i_option_collapsing,

					b_DEBUG_Merge);

			}


			if (v_nD[i_reci] >= 2) //if more than 2 donors exist, exit

			{
				break;
			} //finish main loop 







			  //----

			  //unconverged ending

			  //-----

			if (i_loop == n_max_iteration - 1)

			{
				Rprintf(" reached n_max_iteration after step ");

				Rprintf("%d ", n_max_iteration);

				Rprintf(" Change k, check data quality, further break down categorical variables, or so. It may help ");

				//early deallocaiton -----------------
				delete[] d_k_Collapsible;
				delete[] cn;
				delete[] ml;
				delete[] ol;
				delete[] tnU;
				Del_dMatrix(zbase, nrow, ncol);
				Del_dMatrix(uox, nrow, ncol);
				Del_dMatrix(mox, nrow, ncol);
				delete[] i_orn;
				delete[] i_orn_temp;
				delete[] i_orn_temp2;


				return 0;

			}

		} //end of main loop

		  //testout


		Rprintf("converged in Cell_Make after iterations: "); Rprintf("%d ", i_loop + 1);
		//RPrint("converged in Cell_Make after iterations: ", TestOut); 
		//RPrint("%d ", i_loop + 1, TestOut);




		//----------
		//clear uox and mox matrix for possible garbage
		//Note: Must have only positive integer as category #
		//----------
		for (int i = 0; i<i_count_uox; i++)
		{
			for (int j = 0; j<ncol; j++)
			{
				if (fabs_FHDI(uox[i][j] < 1e-3)) uox[i][j] = 0.0;
			}
		}
		for (int i = 0; i<i_count_mox; i++)
		{
			for (int j = 0; j<ncol; j++)
			{
				if (fabs_FHDI(mox[i][j] < 1e-3)) mox[i][j] = 0.0;
			}
		}




		//--------------

		//prepare separate output of Cell Make

		//--------------

		double* d_temp_um = new double[ncol];

		for (int i = 0; i<i_count_uox; i++)

		{

			for (int j = 0; j<ncol; j++) d_temp_um[j] = uox[i][j];



			rbind_uox_CellMake.append_block(d_temp_um);

		}

		for (int i = 0; i<i_count_mox; i++)

		{

			for (int j = 0; j<ncol; j++) d_temp_um[j] = mox[i][j];



			rbind_mox_CellMake.append_block(d_temp_um);

		}


		delete[] d_temp_um;





		//testout

		//RPrint(" ========= Cell_Make_Extension.. has successfully finished!");

		Rprintf(" ========= FHDI_CellMake_Bigp has successfully finished! \n");



		//-------------------------------------

		//Deallocation

		//-------------------------------------
		delete[] d_k_Collapsible;

		delete[] cn;

		delete[] i_orn;

		delete[] i_orn_temp;

		delete[] i_orn_temp2;

		delete[] ml;

		delete[] ol;

		delete[] tnU;



		Del_dMatrix(zbase, nrow, ncol);

		Del_dMatrix(uox, nrow, ncol);

		Del_dMatrix(mox, nrow, ncol);


		Del_dMatrix(ol_matrix, nrow_ol, ncol);

		Del_iMatrix(correlation_ranking_top, ncol, top);

		//Del_dMatrix(correlation_yicheng, ncol, ncol);

		//Del_iMatrix(correlation_ranking, ncol, (ncol - 1));




		return 1;

	}

} //end of namespace

  //Fn===========================================================================

  //Cell_Make_Neighbor_cpp.cc-----------------------------------------------------------------------------

  //Fn===========================================================================

namespace FHDI {


	bool Cell_Make_Neighbor_cpp(double** x, const int nrow, const int ncol, double* d_k,

		int* NonCollapsible_categorical,

		double** z,

		rbind_FHDI &rbind_uox_CellMake,

		rbind_FHDI &rbind_mox_CellMake,

		List_FHDI &List_nU,

		const int i_merge)

		//Description=========================================

		// make cells with the raw data matrix x 

		// categorization takes place 

		// according to the given number of categories stored in d_k(ncol)

		//

		// Algorithm:  for categorization

		// perc: percentiles used to get quantiles, determined by k

		// quan: quantiles if k=4, we quan=(Q1,Q2,Q3) have Q1(=1/4), Q2 (=Median) and Q3(=3/4)

		// 

		// Note: as of Oct 2016, NA values (missing data) is marked by a long integer at the parent "r" code

		//

		// original R code: Dr. Im, J. and Dr. Kim, J. 

		// c++ code: 		Dr. Cho, I. and Yicheng Yang

		// All rights reserved

		// 

		// updated: Aug 9, 2020

		//----------------------------------------------------

		//IN	: double x(nrow, ncol) 	= {y1, y2, ... } total data containing missing values

		//IN	: double d_k(ncol)		= a vector of categories of each column of xalloc

		//IN    : int NonCollapsible_categorical(nrol) = {0,0, .., 1,.. 0} 
		//				index for non-collapsible categorical variables. 
		//				when at least one column has "1" skip cell-collapse procedure
		//				this may casue a potential error of lack of enough donor! 
		//				(2018, 04 21) 
		//											  

		//OUT   : double z(nrow, ncol)  = catorized matrix corresponding to original matrix x

		//                                initialized with 0.0 

		//OUT	: rbind_FHDI rbind_uox_CellMake(ncol); //compact storage of uox, unique observed rows sorted in the ascending order

		//OUT	: rbind_FHDI rbind_mox_CellMake(ncol); //compact storage of mox, unique observed rows sorted in the ascending order

		//

		//IN    : int i_merge = random donor selection in Merge algorithm in Cell Make

		//                            0= no random seed number setting

		//						      1= random seed number setting 

		//====================================================

	{


		//-------------------------------------
		//Determine if there is Non-Collapsible Categorical variable
		//-------------------------------------
		double* d_k_Collapsible = new double[ncol]; //k for collapsible variables only 
		Copy_dVector(d_k, ncol, d_k_Collapsible);

		int i_NonCollapsible_categ_total = 0;
		for (int i = 0; i<ncol; i++)
		{
			i_NonCollapsible_categ_total += NonCollapsible_categorical[i];

			if (NonCollapsible_categorical[i] == 0) d_k_Collapsible[i] = d_k[i]; //use user-defined k 
			if (NonCollapsible_categorical[i] == 1) d_k_Collapsible[i] = 1; //will be overwritten by actual total categories
		}

		//testout
		/*RPrint("initial d_k[]"); RPrint(d_k, ncol);
		RPrint("\n");
		RPrint("d_k_Collapsible[]"); RPrint(d_k_Collapsible, ncol);
		RPrint("\n");
		RPrint("NonCollapsible_categorical[]"); RPrint(NonCollapsible_categorical, ncol);
		RPrint("\n");
		*/


		//-------------------------------------

		//Categorize raw data

		//-------------------------------------
		//Note: when there is non-collapsible variable, 
		//      its associated d_k
		//      is replaced with actual total categories 
		bool b_success_categorize = categorize_cpp(x, nrow, ncol, d_k, z,
			NonCollapsible_categorical);

		//TestOut << " z matrix from categorize" << endl;
		//RPrint(z, nrow, ncol, TestOut);

		if (!b_success_categorize)
		{
			//early deallocation 
			delete[] d_k_Collapsible;

			return 0;
		}

		//testout
		//   	RPrint("After initial categorize()  \n"); 

		//RPrint("d_k: \n"); RPrint(d_k,  ncol);

		//RPrint("z: \n"); RPrint(z, nrow, ncol);


		//----------
		//clear category matrix for possible garbage
		//Note: z has only positive integer as category #
		//----------
		/*for(int i=0; i<nrow; i++)
		{
		for(int j=0; j<ncol; j++)
		{
		if(fabs_FHDI(z[i][j] < 1e-3)) z[i][j] = 0.0;
		}
		}
		*/


		//-------------------------------------

		//make a copy of z

		//-------------------------------------

		//double** zbase = New_dMatrix(nrow, ncol);

		//Copy_dMatrix(z, nrow, ncol, zbase);



		//-------------------------------------

		//sort in the order of high missing rate

		//at the end, i_orn has the "actual" column numbers from highest missing rate

		//            to the lowest missing rate

		//-------------------------------------

		int* i_orn = new int[ncol]; 	 	Fill_iVector(i_orn, ncol, 0);

		int* i_orn_temp = new int[ncol]; 	Fill_iVector(i_orn_temp, ncol, 0);

		int* i_orn_temp2 = new int[ncol]; 	Fill_iVector(i_orn_temp2, ncol, 0);



		int i_temp = 0;

		for (int i_col = 0; i_col<ncol; i_col++)

		{

			i_temp = 0;

			for (int i_row = 0; i_row<nrow; i_row++)

			{

				if (fabs_FHDI(z[i_row][i_col]) < 1e-5) //count only  "0"

				{
					i_temp++;
				}

			}

			i_orn_temp[i_col] = i_temp; //store how many "0" in this column

		}

		Copy_iVector(i_orn_temp, ncol, i_orn_temp2); //store before sorting 

													 //std::sort(&i_orn_temp[0], &i_orn_temp[ncol-1]); //this works well, but not recommended

		std::sort(i_orn_temp, i_orn_temp + ncol);



		for (int i = 0; i<ncol; i++)

		{

			i_temp = i_orn_temp[ncol - 1 - i]; //reversed searching since the "sort" occurred in ascending order

			for (int j = 0; j<ncol; j++)

			{

				if (i_temp == i_orn_temp2[j])

				{

					i_orn[i] = j + 1; //store column number (actual number, 1, 2, ...) 

					i_orn_temp2[j] = -1; //not to be found again 

					break;

				}

			}

		}



		//-------------------------------------

		//create concatenated vector of z

		//-------------------------------------

		//Note: after Zmat..() all of the below variables are updated 

		//-------------------------------------

		//std::string cn[nrow]; //declaration of concatenated string vector of z

		std::string *cn = new std::string[nrow]; //declaration of concatenated string vector of z

		int* ml = new int[nrow];

		int* ol = new int[nrow];

		double** uox = New_dMatrix(nrow, ncol);

		double** mox = New_dMatrix(nrow, ncol);

		int i_count_ol;

		int i_count_ml;

		int i_count_uox;

		int i_count_mox;



		std::vector<int> v_nD;

		//List_FHDI List_nU(nrow); //default for the size of nrow, but will be updated in the main loop

		int* tnU = new int[nrow]; Fill_iVector(tnU, nrow, 0); //this default size will be udpated in the main loop

		int i_cellmake = 2; // Inactiavte the b_success_nDAU because cell make with KNN always have enough donors

							//============================================

							//============================================

							//Main part to update z by K-nearest-neighbor

							//============================================

							//============================================

		bool b_DEBUG_Zmat = false;

		//if (i_loop == -5) b_DEBUG_Zmat = true;

		Zmat_Extension_cpp(z, nrow, ncol, cn,

			ml, ol, i_count_ol, i_count_ml,

			uox, mox, i_count_uox, i_count_mox,

			b_DEBUG_Zmat);


		if (i_count_ml <= 0 || i_count_ol <= 0)

		{
			Rprintf("ERROR! i_count_ml or _ol is zero! Change k, check data quality, further break down categorical variables, or so. It may help \n");

			//early deallocaiton -----------------
			delete[] d_k_Collapsible;
			delete[] cn;
			delete[] ml;
			delete[] ol;
			delete[] tnU;
			//Del_dMatrix(zbase, nrow, ncol);
			Del_dMatrix(uox, nrow, ncol);
			Del_dMatrix(mox, nrow, ncol);
			delete[] i_orn;
			delete[] i_orn_temp;
			delete[] i_orn_temp2;

			return 0;
		}


		//------------------------------------------

		//generate number of donors nD[]

		// List of observed cells serving as donors List_nU: row numbers of donors per each missing row

		// Table of nU, tnU: total number of all donors for each missing row 

		//------------------------------------------

		//re-initialize tnU and List_nU and v_nD

		List_nU.initialize(i_count_mox);

		v_nD = std::vector<int>();

		tnU = NULL; tnU = new int[i_count_uox]; Fill_iVector(tnU, i_count_uox, 0);



		bool b_DEBUG_nDAU = 0;

		//if(i_loop>9) b_DEBUG_nDAU = 1;



		bool b_success_nDAU = nDAU_cpp(uox, mox, i_count_uox, i_count_mox, ncol,

			cn, ol, i_count_ol, i_cellmake,

			v_nD, List_nU, tnU, b_DEBUG_nDAU);

		//testout

		//RPrint("nDAU_... has been done");

		if (!b_success_nDAU)
		{
			Rprintf("Error! nDAU Failed! Change k, check data quality, further break down categorical variables, or so. It may help \n");

			//early deallocaiton -----------------
			delete[] d_k_Collapsible;
			delete[] cn;
			delete[] ml;
			delete[] ol;
			delete[] tnU;
			//Del_dMatrix(zbase, nrow, ncol);
			Del_dMatrix(uox, nrow, ncol);
			Del_dMatrix(mox, nrow, ncol);
			delete[] i_orn;
			delete[] i_orn_temp;
			delete[] i_orn_temp2;

			return 0; //abnormal ending 	

			//exit(0);
		}

		//-------------------------------------------------------------
		// Updated on Aug 14, 2020. This part enables 
		// cell make with KNN to handle categorical data or hybrid data
		//--------------------------------------------------------------
		int i_min_nD = min_FHDI(v_nD);

		if (i_min_nD >= 2) //if more than 2 donors exist, exit

		{

			Rprintf("Note that the current data already has at least two donors for all recipients originally such that KNN won't take place! \n");

			return 1;

		} //finish main loop 

		if (i_NonCollapsible_categ_total >= 1 && i_min_nD < 2)
		{
			Rprintf("The current data set does not have enough donors while there is at least one non-collapsible categorical variable! \n");

			Rprintf("Thus, KNN won't take place! \n");

			return 0; 

			//exit(0);
		}

		//----------------------------------------------------------

		for (int i_loop = 0; i_loop < i_count_mox; i_loop++) {

			if (v_nD[i_loop] < 2) {

				KNN(i_loop, uox, i_count_uox,
					mox, i_count_mox, d_k,
					cn, ol, i_count_ol,
					nrow, ncol, i_merge,
					v_nD, List_nU);

			}
		}



		//Important!!! Note that List_nU must include actual locations of donors in uox in ascending orders
		//Or there will be mismatch problem in FHDI_Neighbor to compute fractional weights

		std::vector<int> List_temp;

		for (int j2 = 0; j2 < i_count_mox;j2++) {

			List_temp.clear();

			List_nU.get_block_yicheng(j2, List_temp);

			sort(List_temp.begin(), List_temp.end());

			//cout<<"List_temp at "<<j2<<endl;
			//for (int j3 = 0; j3 < List_temp.size();j3++) {
			//	cout<<"List_temp["<<j3<<"]: "<< List_temp[j3]<<endl;
			//}

			List_nU.put_block(j2, List_temp);

		}

		//int v_nD_size = v_nD.size();

		//for (int kk = 0; kk < v_nD_size; kk++) {
		//	if (v_nD[kk] < 2) Rprintf("ERROR! The dataset after k-nearest-neighbor still does not gurantee at least two donors for all unique missing patterns");
		//}

		int i_min_nD2 = min_FHDI(v_nD);

		if (i_min_nD2 < 2) {

			Rprintf("ERROR! The dataset after k-nearest-neighbor still does not gurantee at least two donors for all unique missing patterns");

			return 0; //abnormal ending 

		}
		//----------
		//clear uox and mox matrix for possible garbage
		//Note: Must have only positive integer as category #
		//----------
		for (int i = 0; i<i_count_uox; i++)
		{
			for (int j = 0; j<ncol; j++)
			{
				if (fabs_FHDI(uox[i][j] < 1e-3)) uox[i][j] = 0.0;
			}
		}
		for (int i = 0; i<i_count_mox; i++)
		{
			for (int j = 0; j<ncol; j++)
			{
				if (fabs_FHDI(mox[i][j] < 1e-3)) mox[i][j] = 0.0;
			}
		}




		//--------------

		//prepare separate output of Cell Make

		//--------------

		double* d_temp_um = new double[ncol];

		for (int i = 0; i<i_count_uox; i++)

		{

			for (int j = 0; j<ncol; j++) d_temp_um[j] = uox[i][j];



			rbind_uox_CellMake.append_block(d_temp_um);

		}

		for (int i = 0; i<i_count_mox; i++)

		{

			for (int j = 0; j<ncol; j++) d_temp_um[j] = mox[i][j];



			rbind_mox_CellMake.append_block(d_temp_um);

		}

		delete[] d_temp_um;





		//testout

		//RPrint(" ========= Cell_Make_Extension.. has successfully finished!");

		Rprintf(" ========= FHDI_CellMake with KNN has successfully finished! \n");



		//-------------------------------------

		//Deallocation

		//-------------------------------------
		delete[] d_k_Collapsible;

		delete[] cn;

		delete[] i_orn;

		delete[] i_orn_temp;

		delete[] i_orn_temp2;

		delete[] ml;

		delete[] ol;

		delete[] tnU;



		//Del_dMatrix(zbase, nrow, ncol);

		Del_dMatrix(uox, nrow, ncol);

		Del_dMatrix(mox, nrow, ncol);





		return 1;

	}

} //end of namespace

  //Fn===========================================================================

  //Cell_Make_Neighbor_Bigp_cpp.cc-----------------------------------------------------------------------------

  //Fn===========================================================================

namespace FHDI {

	bool Cell_Make_Neighbor_Bigp_cpp(double** x, int** r, const int nrow, const int ncol, double* d_k,

		int* NonCollapsible_categorical,

		double** z,

		int** codes,

		rbind_FHDI &rbind_uox_CellMake,

		rbind_FHDI &rbind_mox_CellMake,

		List_FHDI &List_nU,

		const int i_merge, const int i_option_collapsing, const int i_option_SIS_type, int top)

		//Description=========================================

		// make cells with the raw data matrix x 

		// categorization takes place 

		// according to the given number of categories stored in d_k(ncol)

		//

		// Algorithm:  for categorization

		// perc: percentiles used to get quantiles, determined by k

		// quan: quantiles if k=4, we quan=(Q1,Q2,Q3) have Q1(=1/4), Q2 (=Median) and Q3(=3/4)

		// 

		// Note: as of Oct 2016, NA values (missing data) is marked by a long integer at the parent "r" code

		//

		// original R code: Dr. Im, J. and Dr. Kim, J. 

		// c++ code: 		Dr. Cho, I. and Yicheng Yang

		// All rights reserved

		// 

		// updated: Aug 11, 2020

		//----------------------------------------------------

		//IN	: double x(nrow, ncol) 	= {y1, y2, ... } total data containing missing values

		//IN	: double d_k(ncol)		= a vector of categories of each column of xalloc

		//IN    : int NonCollapsible_categorical(nrol) = {0,0, .., 1,.. 0} 
		//				index for non-collapsible categorical variables. 
		//				when at least one column has "1" skip cell-collapse procedure
		//				this may casue a potential error of lack of enough donor! 
		//				(2018, 04 21) 
		//											  

		//OUT   : double z(nrow, ncol)  = catorized matrix corresponding to original matrix x

		//                                initialized with 0.0 

		//OUT	: rbind_FHDI rbind_uox_CellMake(ncol); //compact storage of uox, unique observed rows sorted in the ascending order

		//OUT	: rbind_FHDI rbind_mox_CellMake(ncol); //compact storage of mox, unique observed rows sorted in the ascending order

		//

		//IN    : int i_merge = random donor selection in Merge algorithm in Cell Make

		//                            0= no random seed number setting

		//						      1= random seed number setting 

		//====================================================

	{



		//-------------------------------------
		//Determine if there is Non-Collapsible Categorical variable
		//-------------------------------------
		double* d_k_Collapsible = new double[ncol]; //k for collapsible variables only 
		Copy_dVector(d_k, ncol, d_k_Collapsible);

		int i_NonCollapsible_categ_total = 0;
		for (int i = 0; i<ncol; i++)
		{
			i_NonCollapsible_categ_total += NonCollapsible_categorical[i];

			if (NonCollapsible_categorical[i] == 0) d_k_Collapsible[i] = d_k[i]; //use user-defined k 
			if (NonCollapsible_categorical[i] == 1) d_k_Collapsible[i] = 1; //will be overwritten by actual total categories
		}

		//testout
		/*RPrint("initial d_k[]"); RPrint(d_k, ncol);
		RPrint("\n");
		RPrint("d_k_Collapsible[]"); RPrint(d_k_Collapsible, ncol);
		RPrint("\n");
		RPrint("NonCollapsible_categorical[]"); RPrint(NonCollapsible_categorical, ncol);
		RPrint("\n");
		*/


		//-------------------------------------

		//Categorize raw data

		//-------------------------------------
		//Note: when there is non-collapsible variable, 
		//      its associated d_k
		//      is replaced with actual total categories 
		bool b_success_categorize = categorize_cpp(x, nrow, ncol, d_k, z,
			NonCollapsible_categorical);

		//TestOut << " z matrix from categorize" << endl;
		//RPrint(z, nrow, ncol, TestOut);

		if (!b_success_categorize)
		{
			//early deallocation 
			delete[] d_k_Collapsible;

			return 0;
		}

		//testout
		//   	RPrint("After initial categorize()  \n"); 

		//RPrint("d_k: \n"); RPrint(d_k,  ncol);

		//RPrint("z: \n"); RPrint(z, nrow, ncol);


		//----------
		//clear category matrix for possible garbage
		//Note: z has only positive integer as category #
		//----------
		/*for(int i=0; i<nrow; i++)
		{
		for(int j=0; j<ncol; j++)
		{
		if(fabs_FHDI(z[i][j] < 1e-3)) z[i][j] = 0.0;
		}
		}
		*/


		//-------------------------------------

		//make a copy of z

		//-------------------------------------

		double** zbase = New_dMatrix(nrow, ncol);

		Copy_dMatrix(z, nrow, ncol, zbase);



		//-------------------------------------

		//sort in the order of high missing rate

		//at the end, i_orn has the "actual" column numbers from highest missing rate

		//            to the lowest missing rate

		//-------------------------------------

		int* i_orn = new int[ncol]; 	 	Fill_iVector(i_orn, ncol, 0);

		int* i_orn_temp = new int[ncol]; 	Fill_iVector(i_orn_temp, ncol, 0);

		int* i_orn_temp2 = new int[ncol]; 	Fill_iVector(i_orn_temp2, ncol, 0);



		int i_temp = 0;

		for (int i_col = 0; i_col<ncol; i_col++)

		{

			i_temp = 0;

			for (int i_row = 0; i_row<nrow; i_row++)

			{

				if (fabs_FHDI(z[i_row][i_col]) < 1e-5) //count only  "0"

				{
					i_temp++;
				}

			}

			i_orn_temp[i_col] = i_temp; //store how many "0" in this column

		}

		Copy_iVector(i_orn_temp, ncol, i_orn_temp2); //store before sorting 

													 //std::sort(&i_orn_temp[0], &i_orn_temp[ncol-1]); //this works well, but not recommended

		std::sort(i_orn_temp, i_orn_temp + ncol);



		for (int i = 0; i<ncol; i++)

		{

			i_temp = i_orn_temp[ncol - 1 - i]; //reversed searching since the "sort" occurred in ascending order

			for (int j = 0; j<ncol; j++)

			{

				if (i_temp == i_orn_temp2[j])

				{

					i_orn[i] = j + 1; //store column number (actual number, 1, 2, ...) 

					i_orn_temp2[j] = -1; //not to be found again 

					break;

				}

			}

		}



		//-------------------------------------

		//create concatenated vector of z

		//-------------------------------------

		//Note: after Zmat..() all of the below variables are updated 

		//-------------------------------------

		//std::string cn[nrow]; //declaration of concatenated string vector of z

		std::string *cn = new std::string[nrow]; //declaration of concatenated string vector of z

		int* ml = new int[nrow];

		int* ol = new int[nrow];

		double** uox = New_dMatrix(nrow, ncol);

		double** mox = New_dMatrix(nrow, ncol);

		int i_count_ol;

		int i_count_ml;

		int i_count_uox;

		int i_count_mox;



		std::vector<int> v_nD;

		//List_FHDI List_nU(nrow); //default for the size of nrow, but will be updated in the main loop

		int* tnU = new int[nrow]; Fill_iVector(tnU, nrow, 0); //this default size will be udpated in the main loop

		int i_cellmake = 2; // Inactiavte the b_success_nDAU because cell make with KNN always have enough donors

		//============================================

		//============================================

		//Main part to update z by K-nearest-neighbor

		//============================================

		//============================================

		//--------------------------------------
		//Compute correlation ranking matrix 
		//-----------------------------------------
		// Ranking of correlation of each variable in descending order. Note it excludes itself from ranking

		//double** correlation_yicheng = New_dMatrix(ncol, ncol);
		//int** correlation_ranking = New_iMatrix(ncol, (ncol - 1));

		//FHDI::Ranking_m(nrow, ncol, x, r, correlation_yicheng, correlation_ranking, TestOut);

		//----------------
	    //Prepare fully observed y matrix
	    //---------------------
		std::vector<int> ol_temp;
		int d_temp = 0;
		for (int i_row = 0; i_row < nrow; i_row++)
		{
			d_temp = 1.0;
			for (int i_col = 0; i_col < ncol; i_col++)
			{
				if (r[i_row][i_col] == 0) { d_temp = 0.0; break; } //found zero, i.e. missing cell
			}

			if (fabs(d_temp) > 1e-15) //this row has no missing cells
			{
				ol_temp.push_back(i_row);
			} //actual number of the row having no missing cells
		}
		int nrow_ol = ol_temp.size();

		double** ol_matrix = New_dMatrix(nrow_ol, ncol);

		for (int i = 0;i < nrow_ol;i++) {
			for (int j = 0; j < ncol; j++) {
				ol_matrix[i][j] = x[ol_temp[i]][j];
			}
		}



		//int top = 100; // Top "top" rankings exclude itself. Note that  i_option_collapsing < top <= ncol-1
		if ((ncol - 1) < top) {
			top = ncol - 1;
		}

		int** correlation_ranking_top = New_iMatrix(ncol, top);

		FHDI::Ranking_top(nrow_ol, ncol, top, ol_matrix, correlation_ranking_top);


		//TestOut << "correlation matrix: " << endl;
		//for (int i = 0; i < ncol; i++) {
		//	for (int j = 0; j < ncol; j++) {
		//		TestOut << setw(20) << correlation_yicheng[i][j];
		//	}
		//	TestOut << endl;
		//}

		bool b_DEBUG_Zmat = false;

		//if (i_loop == -5) b_DEBUG_Zmat = true;

		Zmat_Extension_cpp(z, nrow, ncol, cn,

			ml, ol, i_count_ol, i_count_ml,

			uox, mox, i_count_uox, i_count_mox,

			b_DEBUG_Zmat);


		if (i_count_ml <= 0 || i_count_ol <= 0 || (nrow_ol != i_count_ol))

		{
			Rprintf("ERROR! i_count_ml or _ol is zero! Change k, check data quality, further break down categorical variables, or so. It may help \n");

			//early deallocaiton -----------------
			delete[] d_k_Collapsible;
			delete[] cn;
			delete[] ml;
			delete[] ol;
			delete[] tnU;
			Del_dMatrix(zbase, nrow, ncol);
			Del_dMatrix(uox, nrow, ncol);
			Del_dMatrix(mox, nrow, ncol);
			delete[] i_orn;
			delete[] i_orn_temp;
			delete[] i_orn_temp2;

			return 0;
		}


		//------------------------------------------

		//generate number of donors nD[]

		// List of observed cells serving as donors List_nU: row numbers of donors per each missing row

		// Table of nU, tnU: total number of all donors for each missing row 

		//------------------------------------------

		//re-initialize tnU and List_nU and v_nD

		List_nU.initialize(i_count_mox);

		v_nD = std::vector<int>();

		tnU = NULL; tnU = new int[i_count_uox]; Fill_iVector(tnU, i_count_uox, 0);



		bool b_DEBUG_nDAU = 0;

		//if(i_loop>9) b_DEBUG_nDAU = 1;

		//!!!!! Initialize codes before nDAU every loop
		for (int k1 = 0; k1 < nrow; k1++) {
			for (int k2 = 0; k2 < i_option_collapsing; k2++) {
				codes[k1][k2] = 0;
			}
		}

		bool b_success_nDAU = nDAU_Bigp_cpp(uox, mox, i_count_uox, i_count_mox, ncol, i_option_collapsing, i_option_SIS_type,

			cn, ol, i_count_ol, top, i_cellmake,

			v_nD, List_nU, tnU, codes, correlation_ranking_top, ol_matrix, b_DEBUG_nDAU);

		//testout

		//RPrint("nDAU_... has been done");

		if (!b_success_nDAU)
		{
			Rprintf("Error! nDAU Failed! Change k, check data quality, further break down categorical variables, or so. It may help \n");

			//early deallocaiton -----------------
			delete[] d_k_Collapsible;
			delete[] cn;
			delete[] ml;
			delete[] ol;
			delete[] tnU;
			Del_dMatrix(zbase, nrow, ncol);
			Del_dMatrix(uox, nrow, ncol);
			Del_dMatrix(mox, nrow, ncol);
			delete[] i_orn;
			delete[] i_orn_temp;
			delete[] i_orn_temp2;

			return 0; //abnormal ending 

			//exit(0);
		}


		//-------------------------------------------------------------
		// Updated on Aug 14, 2020. This part enables 
		// cell make with KNN to handle categorical data or hybrid data
		//--------------------------------------------------------------
		int i_min_nD = min_FHDI(v_nD);

		if (i_min_nD >= 2) //if more than 2 donors exist, exit

		{

			Rprintf("Note that the current data already has at least two donors for all recipients originally such that KNN won't take place! \n");

			return 1;

		} //finish main loop 

		if (i_NonCollapsible_categ_total >= 1 && i_min_nD < 2)
		{
			Rprintf("The current data set does not have enough donors while there is at least one non-collapsible categorical variable! \n");

			Rprintf("Thus, KNN won't take place! \n");

			return 0;

			//exit(0);
		}

		//----------------------------------------------------------


		for (int i_loop = 0; i_loop < i_count_mox; i_loop++) {

			if (v_nD[i_loop] < 2) {

				KNN_Bigp(i_loop, uox, i_count_uox,
					mox, i_count_mox, d_k, codes, i_option_collapsing,
					cn, ol, i_count_ol,
					nrow, ncol, i_merge,
					v_nD, List_nU);

			}
		}


		//Important!!! Note that List_nU must include actual locations of donors in uox in ascending orders
		//Or there will be mismatch problem in FHDI_Neighbor to compute fractional weights

		std::vector<int> List_temp;

		for (int j2 = 0; j2 < i_count_mox;j2++) {

			List_temp.clear();

			List_nU.get_block_yicheng(j2, List_temp);

			sort(List_temp.begin(), List_temp.end());

			//cout<<"List_temp at "<<j2<<endl;
			//for (int j3 = 0; j3 < List_temp.size();j3++) {
			//	cout<<"List_temp["<<j3<<"]: "<< List_temp[j3]<<endl;
			//}

			List_nU.put_block(j2, List_temp);

		}
		//int i_min_nD = min_FHDI(v_nD);

		//int v_nD_size = v_nD.size();

		//for (int kk = 0; kk< v_nD_size;kk++) {

		//	if (v_nD[kk] < 2) Rprintf("ERROR! The dataset after k-nearest-neighbor still does not gurantee at least two donors for all unique missing patterns");

		//}

		int i_min_nD2 = min_FHDI(v_nD);

		if (i_min_nD2 < 2) {

			Rprintf("ERROR! The dataset after k-nearest-neighbor still does not gurantee at least two donors for all unique missing patterns");

			return 0; //abnormal ending 

		}

		//----------
		//clear uox and mox matrix for possible garbage
		//Note: Must have only positive integer as category #
		//----------
		for (int i = 0; i<i_count_uox; i++)
		{
			for (int j = 0; j<ncol; j++)
			{
				if (fabs_FHDI(uox[i][j] < 1e-3)) uox[i][j] = 0.0;
			}
		}
		for (int i = 0; i<i_count_mox; i++)
		{
			for (int j = 0; j<ncol; j++)
			{
				if (fabs_FHDI(mox[i][j] < 1e-3)) mox[i][j] = 0.0;
			}
		}




		//--------------

		//prepare separate output of Cell Make

		//--------------

		double* d_temp_um = new double[ncol];

		for (int i = 0; i<i_count_uox; i++)

		{

			for (int j = 0; j<ncol; j++) d_temp_um[j] = uox[i][j];



			rbind_uox_CellMake.append_block(d_temp_um);

		}

		for (int i = 0; i<i_count_mox; i++)

		{

			for (int j = 0; j<ncol; j++) d_temp_um[j] = mox[i][j];



			rbind_mox_CellMake.append_block(d_temp_um);

		}

		delete[] d_temp_um;





		//testout

		//RPrint(" ========= Cell_Make_Extension.. has successfully finished!");

		Rprintf(" ========= FHDI_CellMake_Bigp with KNN has successfully finished! \n");


		//-------------------------------------

		//Deallocation

		//-------------------------------------
		delete[] d_k_Collapsible;

		delete[] cn;

		delete[] i_orn;

		delete[] i_orn_temp;

		delete[] i_orn_temp2;

		delete[] ml;

		delete[] ol;

		delete[] tnU;



		Del_dMatrix(zbase, nrow, ncol);

		Del_dMatrix(uox, nrow, ncol);

		Del_dMatrix(mox, nrow, ncol);


		Del_dMatrix(ol_matrix, nrow_ol, ncol);

		Del_iMatrix(correlation_ranking_top, ncol, top);

		//Del_dMatrix(correlation_yicheng, ncol, ncol);

		//Del_iMatrix(correlation_ranking, ncol, (ncol - 1));

		return 1;

	}

} //end of namespace



//Fn===========================================================================

//AGMAT_Extension_cpp.cc-----------------------------------------------------------------------------

//Fn===========================================================================

namespace FHDI

{

bool AGMAT_Extension_cpp(double** mox, const int nrow_mox, 

						 double** uox, const int nrow_uox, 

						 const int ncol, int* id, 

						 std::vector<std::string> v_table_tmvec_row1,

						 std::vector<int> v_table_tmvec_row2,

                         std::string cn[], const int nrow, 

						 rbind_FHDI &rst_final)

//Description=========================================

// Augment missing cells in mox using the observed values of uox

//

// Algorithm:  All possible donors will be used to fill in the missing cell 

//             but, if there is no matched donors in uox, this algorithm may fail

//             as of Oct 2016

// for each missing pattern, find all the possible donors

// e.g., 

// (1) a missing row   = 000

// 	   agmat           = all observed rows

// (2) a missing row   = a01

//     agmat           = ac1, af1, a11, ..., az1. 

//

//

// original R code: Dr. Im, J. and Dr. Kim, J. 

// c++ code: 		Dr. Cho, I. 

// All rights reserved

// 

// updated: Oct 26, 2016

//----------------------------------------------------

//IN    : double mox(nrow_mox, ncol)= sorted unique patterns of missing  cells. up to i_count_mox rows are meaningful                           

//IN    : double uox(nrow_uox, ncol)= sorted unique patterns of observed cells. up to i_count_uox rows are meaningful 

//IN    : int id(nrow) = index of row. Default is ACTUAL row number

//IN	: vector<string> v_table_tmvec_row1  = name table of condensed missing patterns

//IN	: vector<int> v_table_tmvec_row2  = counts table of condensed missing patterns

//IN  	: string cn(nrow)		= vector of string to represent each row of z     

//OUT   : rbind_FHDI rst_final(??, ncol) = updated observed rows to be used later 

//                                          number of rows will be determined by this code   

//====================================================

{

	const int nr1 = nrow_mox;

	const int nr2 = nrow_uox;

	

	//-------------------

	//initialize rst, the matrix for storage for augmented observations

	//-------------------

	rbind_FHDI rst(ncol+1); //1+ncol = (row id)+(observed rows used for imputation later) 



	//--------------------

	//Main Loop for all missing rows

	//--------------------

	int* i_temp_x = new int[ncol];

	int i_sum_x = 0;

	std::vector<int> v_cn_z_i; 

	int* zid = NULL;

	int i_size_zid=0; 

	int i_loc=0;	

	int* i_srst = new int[nr2];

	std::vector<int> loc_srst_nl; 

	

	//-------------

	//LOOP for all missing rows

	//-------------

	for(int i=0; i<nr1; i++)

	{

		//get current row of missing cell 

		for(int j=0; j<ncol; j++) i_temp_x[j] =  (int)mox[i][j]; 

		i_sum_x = sum_FHDI(i_temp_x, ncol);



		std::string s_temp = v_table_tmvec_row1[i]; //name of ith row

		

		v_cn_z_i.clear(); //re-initialize 

		which(cn, nrow, s_temp, v_cn_z_i); //Note: Actual location is returned

		int i_size_v_cn_z_i = (int)v_cn_z_i.size(); //number of locations in cn having s_temp

		

		//----------------------

		//Condition 1: this row's cells are all missing

		//  this case, all observed rows are used for imputation 

		//-----------------------

		if(i_sum_x == 0)

		{

			//-----------------

			//make "zid" which means 

			//the row location of current missing row repeated by number of all observed rows

			//-----------------

			zid = NULL; //re-initialize; 

			i_size_zid = i_size_v_cn_z_i*nr2;

			zid = new int[i_size_zid]; 

			

			for(int j=0; j<i_size_v_cn_z_i; j++)

			{

				for(int k=0; k<nr2; k++) //repeated copy of the id number 

				{

					//NOTE: zid contains ACTUAL id number 

					zid[j*nr2 + k] = id[v_cn_z_i[j]-1]; //-1 for actual location

				}

			}

			

			//-------------------

			//make a matrix that consists of zid & repeated uox for all missing rows

			//-------------------

			double** rst_temp = New_dMatrix(i_size_zid, ncol+1);

			 

			for(int j=0; j<i_size_v_cn_z_i; j++) //all rows that have the current missing pattern

			{

				for(int k=0; k<nr2; k++) //repeated copy  

				{

					i_loc = j*nr2 + k; //serial number of the entire rows of the matrix

					

					//first column is zid[]

					rst_temp[i_loc][0] = zid[i_loc];

				

					//from the second through the end columns are occupied with uox

					for(int i_col=0; i_col<ncol; i_col++)

					{

						rst_temp[i_loc][i_col+1] = uox[k][i_col];

					}

				}

			}

			//---

			//Append the entire matrix to rst

			//---

			rst.bind_blocks(i_size_zid, ncol+1, rst_temp);

			

		

			//---------

			//local deallocation

			//---------

			Del_dMatrix(rst_temp, i_size_zid, ncol+1);

		}

		

		//----------------------

		//Condition 2: some cells of current row are missing

		//-----------------------

		int nl = 0;

		if(i_sum_x > 0)

		{

			//------

			//number of observed cells on this row

			//------

			nl = 0; 

			for(int j=0; j<ncol; j++) 

			{

				if(mox[i][j]>0) nl++; 

			}

			

			//-------

			//indicator matrix that matches the donors

			//srst: row-wise sum of the indicator matrix 

			//algorithm: 

			// current row   = a01 

			// observed rows = a11, ab1, af1, ... will be selected and stored  

			//-------

			loc_srst_nl.clear(); //re-initialize

			Fill_iVector(i_srst, nr2, 0); //re-initialize 

				

			for(int j=0; j<nr2; j++)

			{

				int i_sum_crst = 0; 

				for(int k=0; k<ncol; k++)

				{

					//Note: in below check, mox is fixed at ith row 

					if(fabs_FHDI(mox[i][k] - uox[j][k])<1e-3) //part of missing cell = obserbed cell 

					{

						i_sum_crst++; // increment if a cell of missing row = obs. cell 

					}

				}

				//---

				//store how many cells of missing row match those of observed row

				//---

				i_srst[j] = i_sum_crst; 

				if(i_sum_crst==nl) loc_srst_nl.push_back(j+1); //Actual location 				

			}

			

			//-----

			//total matching rows

			//-----

			const int i_size_loc_srst_nl = (int)loc_srst_nl.size(); 

			if(i_size_loc_srst_nl == 0) //error case

			{Rprintf("Error! there is no matched cell! \n"); return 0;}

			

			if(i_size_loc_srst_nl > 0) 

			{

				//-----------------

				//make "zid" which means 

				//the row location of current missing row repeated by number of observed rows

				//-----------------

				zid = NULL; //re-initialize; 

				i_size_zid = i_size_v_cn_z_i*i_size_loc_srst_nl;

				zid = new int[i_size_zid]; 

			

				for(int j=0; j<i_size_v_cn_z_i; j++)

				{

					for(int k=0; k<i_size_loc_srst_nl; k++) //repeated copy of the id number 

					{

						//NOTE: zid contains ACTUAL id number 

						zid[j*i_size_loc_srst_nl + k] = id[v_cn_z_i[j]-1]; //-1 for actual location

					}

				}

			

				//-------------------

				//make a matrix that consists of zid & repeated uox for all missing rows

				//-------------------

				double** rst_temp2 = New_dMatrix(i_size_zid, ncol+1);

				 

				for(int j=0; j<i_size_v_cn_z_i; j++)

				{

					for(int k=0; k<i_size_loc_srst_nl; k++) //repeated copy of the id number 

					{

						i_loc = j*i_size_loc_srst_nl + k; //serial number of the entire rows of the matrix

						

						//first column is zid[]

						rst_temp2[i_loc][0] = zid[i_loc];

					

						//from the second through the end columns are occupied with uox

						for(int i_col=0; i_col<ncol; i_col++)

						{

							//rst_temp2[i_loc][i_col+1] = uox[k][i_col]; //cf. condition 1 form

							rst_temp2[i_loc][i_col+1] = uox[loc_srst_nl[k]-1][i_col]; //-1 for actual location

							

						}

					}

				}

				//---

				//Append the entire matrix to rst

				//---

				rst.bind_blocks(i_size_zid, ncol+1, rst_temp2);

				

				

				//---------

				//local deallocation

				//---------

				Del_dMatrix(rst_temp2, i_size_zid, ncol+1);				

			}

		}



	} //end of LOOP for all missing rows

	

	//----------------

	//re-order rst in terms of id (the first column)

	//----------------

	const int n_row_rst = rst.size_row(); 

	int* i_rst_id = new int[n_row_rst];

	for(int i=0; i<n_row_rst; i++) i_rst_id[i] = (int)rst(i,0);

	order_FHDI(i_rst_id, n_row_rst); //returned with the order of rows in ascending magnitude

   

	//--------------------

	//remove the first column with id

	//store the rst into the final storage

	//--------------------

	//rbind_FHDI rst_final(ncol); //Note: without the first column of id

	double* d_row_rst 		= new double[ncol+1]; 

	double* d_row_rst_short = new double[ncol];

	for(int i=0; i<n_row_rst; i++)

	{

		rst.get_block(i_rst_id[i]-1, d_row_rst); //get a row// -1 for actual loc

		for(int k=0; k<ncol; k++) d_row_rst_short[k] = d_row_rst[k+1]; //without id  

		rst_final.append_block(d_row_rst_short);	//append a new row to the final storage 

	}	

	

	//-------

	//local deallocation

	//-------

	delete[] i_temp_x;

	delete[] zid;	

	delete[] i_srst;

	delete[] i_rst_id;

	delete[] d_row_rst;

	delete[] d_row_rst_short;	

	

	return 1;

}



} //end of namespace


  //Fn===========================================================================

  //AGMAT_Extension_Bigp_cpp.cc-----------------------------------------------------------------------------

  //Fn===========================================================================

namespace FHDI
{
	bool AGMAT_Extension_Bigp_cpp(double** mox, const int nrow_mox,
		double** uox, const int nrow_uox,
		const int ncol, int* id,
		std::vector<std::string> v_table_tmvec_row1,
		std::vector<int> v_table_tmvec_row2,
		std::string cn[], const int nrow, const int i_option_collapsing, int** codes,
		rbind_FHDI &rst_final)
		//Description=========================================
		// Augment missing cells in mox using the observed values of uox
		//
		// Algorithm:  All possible donors will be used to fill in the missing cell 
		//             but, if there is no matched donors in uox, this algorithm may fail
		//             as of Oct 2016
		// for each missing pattern, find all the possible donors
		// e.g., 
		// (1) a missing row   = 000
		// 	   agmat           = all observed rows
		// (2) a missing row   = a01
		//     agmat           = ac1, af1, a11, ..., az1. 
		//
		//
		// original R code: Dr. Im, J. and Dr. Kim, J. 
		// c++ code: 		Dr. Cho, I. and Yicheng Yang
		// All rights reserved
		// 
		// updated: Feb 26, 2020
		//----------------------------------------------------
		//IN    : double mox(nrow_mox, ncol)= sorted unique patterns of missing  cells. up to i_count_mox rows are meaningful                           
		//IN    : double uox(nrow_uox, ncol)= sorted unique patterns of observed cells. up to i_count_uox rows are meaningful 
		//IN    : int id(nrow) = index of row. Default is ACTUAL row number
		//IN    : int i_option_collapsing = choice of big-p algorithm 
		//                               0= no big-p algorithms
		//                              !0= perform big-p algorithms
		//IN   :  int codes(nrow, i_option_collapsing); // storage to record most correlated variables of mox
		//IN	: vector<string> v_table_tmvec_row1  = name table of condensed missing patterns
		//IN	: vector<int> v_table_tmvec_row2  = counts table of condensed missing patterns
		//IN  	: string cn(nrow)		= vector of string to represent each row of z     
		//OUT   : rbind_FHDI rst_final(??, ncol) = updated observed rows to be used later 
		//                                          number of rows will be determined by this code   
		//====================================================
	{
		/*TestOut << "Codes in AGMAT" << endl;

		for (int kk2 = 0; kk2 < nrow; kk2++) {
		for (int kk3 = 0; kk3 < i_option_collapsing; kk3++) {
		TestOut << setw(20) << codes[kk2][kk3];
		}
		TestOut << endl;
		}
		TestOut << "mox in AGMAT" << endl;

		for (int kk2 = 0; kk2 < nrow_mox; kk2++) {
		for (int kk3 = 0; kk3 < ncol; kk3++) {
		TestOut << setw(20) << mox[kk2][kk3];
		}
		TestOut << endl;
		}
		TestOut << "uox in AGMAT" << endl;

		for (int kk2 = 0; kk2 < nrow_uox; kk2++) {
		for (int kk3 = 0; kk3 < ncol; kk3++) {
		TestOut << setw(20) << uox[kk2][kk3];
		}
		TestOut << endl;
		}*/

		const int nr1 = nrow_mox;
		const int nr2 = nrow_uox;

		//-------------------
		//initialize rst, the matrix for storage for augmented observations
		//-------------------
		rbind_FHDI rst(ncol + 1); //1+ncol = (row id)+(observed rows used for imputation later) 

								  //--------------------
								  //Main Loop for all missing rows
								  //--------------------
		int* i_temp_x = new int[ncol];
		int i_sum_x = 0;
		std::vector<int> v_cn_z_i;
		int* zid = NULL;
		int i_size_zid = 0;
		int i_loc = 0;
		int* i_srst = new int[nr2];
		std::vector<int> loc_srst_nl;
		std::vector<int> v_mxl; // hold the most correlated variables of mox[i]

								//-------------
								//LOOP for all missing rows
								//-------------

		for (int i = 0; i<nr1; i++)
		{
			//get current row of missing cell 
			for (int j = 0; j<ncol; j++) i_temp_x[j] = (int)mox[i][j];
			i_sum_x = sum_FHDI(i_temp_x, ncol);

			std::string s_temp = v_table_tmvec_row1[i]; //name of ith row

			v_cn_z_i.clear(); //re-initialize 
			which(cn, nrow, s_temp, v_cn_z_i); //Note: Actual location is returned
			int i_size_v_cn_z_i = (int)v_cn_z_i.size(); //number of locations in cn having s_temp

														//----------------------
														//Condition 1: this row's cells are all missing
														//  this case, all observed rows are used for imputation 
														//-----------------------
			if (i_sum_x == 0)
			{
				//-----------------
				//make "zid" which means 
				//the row location of current missing row repeated by number of all observed rows
				//-----------------
				zid = NULL; //re-initialize; 
				i_size_zid = i_size_v_cn_z_i*nr2;
				zid = new int[i_size_zid];

				for (int j = 0; j<i_size_v_cn_z_i; j++)
				{
					for (int k = 0; k<nr2; k++) //repeated copy of the id number 
					{
						//NOTE: zid contains ACTUAL id number 
						zid[j*nr2 + k] = id[v_cn_z_i[j] - 1]; //-1 for actual location
					}
				}

				//-------------------
				//make a matrix that consists of zid & repeated uox for all missing rows
				//-------------------
				double** rst_temp = New_dMatrix(i_size_zid, ncol + 1);

				for (int j = 0; j<i_size_v_cn_z_i; j++) //all rows that have the current missing pattern
				{
					for (int k = 0; k<nr2; k++) //repeated copy  
					{
						i_loc = j*nr2 + k; //serial number of the entire rows of the matrix

										   //first column is zid[]
						rst_temp[i_loc][0] = zid[i_loc];

						//from the second through the end columns are occupied with uox
						for (int i_col = 0; i_col<ncol; i_col++)
						{
							rst_temp[i_loc][i_col + 1] = uox[k][i_col];
						}
					}
				}
				//---
				//Append the entire matrix to rst
				//---
				rst.bind_blocks(i_size_zid, ncol + 1, rst_temp);

				//testout
				/*
				RPrint(" ==Condition1. all cells on a row are missing. i: "); RPrint(i);
				RPrint("zid:"); RPrint(zid, i_size_zid);
				RPrint("rst:");
				rst.print_rbind_FHDI();
				*/

				//---------
				//local deallocation
				//---------
				Del_dMatrix(rst_temp, i_size_zid, ncol + 1);
			}

			//----------------------
			//Condition 2: some cells of current row are missing
			//-----------------------
			int nl = 0;
			if (i_sum_x > 0)
			{
				//------
				//number of observed cells on this row
				//------
				nl = 0;
				for (int j = 0; j<ncol; j++)
				{
					if (mox[i][j]>0) nl++;
				}

				if (nl > i_option_collapsing) {
					nl = i_option_collapsing;
				}
				//-------
				//indicator matrix that matches the donors
				//srst: row-wise sum of the indicator matrix 
				//algorithm: 
				// current row   = a01 
				// observed rows = a11, ab1, af1, ... will be selected and stored  
				//-------
				loc_srst_nl.clear(); //re-initialize
				Fill_iVector(i_srst, nr2, 0); //re-initialize 
				v_mxl.clear();

				//inherents the most correlated variables of mox[i]
				for (int k = 0; k < i_option_collapsing; k++) {
					if (codes[i][k] != 0) {
						v_mxl.push_back(codes[i][k]);
						//TestOut << "code[" << i << "]: " << codes[i][k] << endl;
					}
				}

				int v_mxl_size = v_mxl.size();


				for (int j = 0; j<nr2; j++)
				{
					int i_sum_crst = 0;
					for (int k = 0; k<v_mxl_size; k++)
					{
						//Note: in below check, mox is fixed at ith row 
						if (fabs(mox[i][v_mxl[k] - 1] - uox[j][v_mxl[k] - 1])<1e-3) // Note v_mxl records the actual locations !!!
						{
							i_sum_crst++; // increment if a cell of missing row = obs. cell 
						}
					}
					//---
					//store how many cells of missing row match those of observed row
					//---
					i_srst[j] = i_sum_crst;
					if (i_sum_crst == nl) loc_srst_nl.push_back(j + 1); //Actual location 				
				}
				//testout
				/*
				RPrint(" ==Condition2. some cells on a row are missing. i: "); RPrint(i);
				RPrint("nl: "); RPrint(nl);
				RPrint("srst: "); RPrint(i_srst, nr2);
				RPrint("loc_srst_nl: "); RPrint(loc_srst_nl);
				*/

				//-----
				//total matching rows
				//-----
				const int i_size_loc_srst_nl = (int)loc_srst_nl.size();

				if (i_size_loc_srst_nl == 0) //error case

				{
					Rprintf("Error! there is no matched cell! \n"); return 0;
				}

				if (i_size_loc_srst_nl > 0)
				{
					//-----------------
					//make "zid" which means 
					//the row location of current missing row repeated by number of observed rows
					//-----------------
					zid = NULL; //re-initialize; 
					i_size_zid = i_size_v_cn_z_i*i_size_loc_srst_nl;
					zid = new int[i_size_zid];

					for (int j = 0; j<i_size_v_cn_z_i; j++)
					{
						for (int k = 0; k<i_size_loc_srst_nl; k++) //repeated copy of the id number 
						{
							//NOTE: zid contains ACTUAL id number 
							zid[j*i_size_loc_srst_nl + k] = id[v_cn_z_i[j] - 1]; //-1 for actual location
						}
					}

					//-------------------
					//make a matrix that consists of zid & repeated uox for all missing rows
					//-------------------
					double** rst_temp2 = New_dMatrix(i_size_zid, ncol + 1);

					for (int j = 0; j<i_size_v_cn_z_i; j++)
					{
						for (int k = 0; k<i_size_loc_srst_nl; k++) //repeated copy of the id number 
						{
							i_loc = j*i_size_loc_srst_nl + k; //serial number of the entire rows of the matrix

															  //first column is zid[]
							rst_temp2[i_loc][0] = zid[i_loc];

							//from the second through the end columns are occupied with uox
							for (int i_col = 0; i_col<ncol; i_col++)
							{
								//rst_temp2[i_loc][i_col+1] = uox[k][i_col]; //cf. condition 1 form
								rst_temp2[i_loc][i_col + 1] = uox[loc_srst_nl[k] - 1][i_col]; //-1 for actual location

							}
						}
					}
					//---
					//Append the entire matrix to rst
					//---
					rst.bind_blocks(i_size_zid, ncol + 1, rst_temp2);

					//testout
					/*
					RPrint("zid:"); RPrint(zid, i_size_zid);
					RPrint("rst:");
					rst.print_rbind_FHDI();
					*/

					//---------
					//local deallocation
					//---------
					Del_dMatrix(rst_temp2, i_size_zid, ncol + 1);
				}
			}

		} //end of LOOP for all missing rows

		  //----------------
		  //re-order rst in terms of id (the first column)
		  //----------------
		const int n_row_rst = rst.size_row();
		int* i_rst_id = new int[n_row_rst];
		for (int i = 0; i<n_row_rst; i++) i_rst_id[i] = (int)rst(i, 0);
		order_FHDI(i_rst_id, n_row_rst); //returned with the order of rows in ascending magnitude
										 //testout
										 //RPrint("n_row_rst :"); RPrint(n_row_rst);
										 //RPrint("i_rst_id :"); RPrint(i_rst_id, n_row_rst);


										 //--------------------
										 //remove the first column with id
										 //store the rst into the final storage
										 //--------------------
										 //rbind_FHDI rst_final(ncol); //Note: without the first column of id
		double* d_row_rst = new double[ncol + 1];
		double* d_row_rst_short = new double[ncol];
		for (int i = 0; i<n_row_rst; i++)
		{
			rst.get_block(i_rst_id[i] - 1, d_row_rst); //get a row// -1 for actual loc
			for (int k = 0; k<ncol; k++) d_row_rst_short[k] = d_row_rst[k + 1]; //without id  
			rst_final.append_block(d_row_rst_short);	//append a new row to the final storage 
		}

		//testout
		//RPrint("End of AUGMAT =========="); 
		//RPrint("rst_final:"); rst_final.print_rbind_FHDI(); 

		//-------
		//local deallocation
		//-------
		delete[] i_temp_x;
		delete[] zid;
		delete[] i_srst;
		delete[] i_rst_id;
		delete[] d_row_rst;
		delete[] d_row_rst_short;

		return 1;
	}

} //end of namespace

  //Fn===========================================================================

  //AGMAT_Neighbor_cpp.cc-----------------------------------------------------------------------------

  //Fn===========================================================================

namespace FHDI
{
	bool AGMAT_Neighbor_cpp(double** mox, const int nrow_mox,
		double** uox, const int nrow_uox,
		const int ncol, int* id,
		std::vector<std::string> v_table_tmvec_row1,
		std::vector<int> v_table_tmvec_row2,
		std::string cn[], const int nrow, List_FHDI &List_nU,
		rbind_FHDI &rst_final)
		//Description=========================================
		// Augment missing cells in mox using the observed values of uox
		//
		// Algorithm:  All possible donors will be used to fill in the missing cell 
		//             but, if there is no matched donors in uox, this algorithm may fail
		//             as of Oct 2016
		// for each missing pattern, find all the possible donors
		// e.g., 
		// (1) a missing row   = 000
		// 	   agmat           = all observed rows
		// (2) a missing row   = a01
		//     agmat           = ac1, af1, a11, ..., az1. 
		//
		//
		// original R code: Dr. Im, J. and Dr. Kim, J. 
		// c++ code: 		Dr. Cho, I. and Yicheng Yang
		// All rights reserved
		// 
		// updated: Aug 11, 2020
		//----------------------------------------------------
		//IN    : double mox(nrow_mox, ncol)= sorted unique patterns of missing  cells. up to i_count_mox rows are meaningful                           
		//IN    : double uox(nrow_uox, ncol)= sorted unique patterns of observed cells. up to i_count_uox rows are meaningful 
		//IN    : int id(nrow) = index of row. Default is ACTUAL row number
		//IN	: vector<string> v_table_tmvec_row1  = name table of condensed missing patterns
		//IN	: vector<int> v_table_tmvec_row2  = counts table of condensed missing patterns
		//IN  	: string cn(nrow)		= vector of string to represent each row of z     
		//OUT   : rbind_FHDI rst_final(??, ncol) = updated observed rows to be used later 
		//                                          number of rows will be determined by this code   
		//====================================================
	{

		const int nr1 = nrow_mox;
		const int nr2 = nrow_uox;

		//-------------------
		//initialize rst, the matrix for storage for augmented observations
		//-------------------
		rbind_FHDI rst(ncol + 1); //1+ncol = (row id)+(observed rows used for imputation later) 

								  //--------------------
								  //Main Loop for all missing rows
								  //--------------------
		int* i_temp_x = new int[ncol];
		int i_sum_x = 0;
		std::vector<int> v_cn_z_i;
		int* zid = NULL;
		int i_size_zid = 0;
		int i_loc = 0;
		//int* i_srst = new int[nr2]; // not used in KNN
		std::vector<int> loc_srst_nl;

		//-------------
		//LOOP for all missing rows
		//-------------
		for (int i = 0; i<nr1; i++)
		{
			//get current row of missing cell 
			for (int j = 0; j<ncol; j++) i_temp_x[j] = (int)mox[i][j];
			i_sum_x = sum_FHDI(i_temp_x, ncol);

			std::string s_temp = v_table_tmvec_row1[i]; //name of ith row

			v_cn_z_i.clear(); //re-initialize 
			which(cn, nrow, s_temp, v_cn_z_i); //Note: Actual location is returned
			int i_size_v_cn_z_i = (int)v_cn_z_i.size(); //number of locations in cn having s_temp

														//----------------------
														//Condition 1: this row's cells are all missing
														//  this case, all observed rows are used for imputation 
														//-----------------------
			if (i_sum_x == 0)
			{
				//-----------------
				//make "zid" which means 
				//the row location of current missing row repeated by number of all observed rows
				//-----------------
				zid = NULL; //re-initialize; 
				i_size_zid = i_size_v_cn_z_i*nr2;
				zid = new int[i_size_zid];

				for (int j = 0; j<i_size_v_cn_z_i; j++)
				{
					for (int k = 0; k<nr2; k++) //repeated copy of the id number 
					{
						//NOTE: zid contains ACTUAL id number 
						zid[j*nr2 + k] = id[v_cn_z_i[j] - 1]; //-1 for actual location
					}
				}

				//-------------------
				//make a matrix that consists of zid & repeated uox for all missing rows
				//-------------------
				double** rst_temp = New_dMatrix(i_size_zid, ncol + 1);

				for (int j = 0; j<i_size_v_cn_z_i; j++) //all rows that have the current missing pattern
				{
					for (int k = 0; k<nr2; k++) //repeated copy  
					{
						i_loc = j*nr2 + k; //serial number of the entire rows of the matrix

										   //first column is zid[]
						rst_temp[i_loc][0] = zid[i_loc];

						//from the second through the end columns are occupied with uox
						for (int i_col = 0; i_col<ncol; i_col++)
						{
							rst_temp[i_loc][i_col + 1] = uox[k][i_col];
						}
					}
				}
				//---
				//Append the entire matrix to rst
				//---
				rst.bind_blocks(i_size_zid, ncol + 1, rst_temp);

				//testout
				/*
				RPrint(" ==Condition1. all cells on a row are missing. i: "); RPrint(i);
				RPrint("zid:"); RPrint(zid, i_size_zid);
				RPrint("rst:");
				rst.print_rbind_FHDI();
				*/

				//---------
				//local deallocation
				//---------
				Del_dMatrix(rst_temp, i_size_zid, ncol + 1);
			}

			//----------------------
			//Condition 2: some cells of current row are missing
			//-----------------------
			//int nl = 0;
			if (i_sum_x > 0)
			{
				//------
				//number of observed cells on this row
				//------
				//nl = 0; 
				//for(int j=0; j<ncol; j++) 
				//{
				//	if(mox[i][j]>0) nl++; 
				//}
				//
				////-------
				////indicator matrix that matches the donors
				////srst: row-wise sum of the indicator matrix 
				////algorithm: 
				//// current row   = a01 
				//// observed rows = a11, ab1, af1, ... will be selected and stored  
				////-------
				//loc_srst_nl.clear(); //re-initialize
				//Fill_iVector(i_srst, nr2, 0); //re-initialize 
				//	
				//for(int j=0; j<nr2; j++)
				//{
				//	int i_sum_crst = 0; 
				//	for(int k=0; k<ncol; k++)
				//	{
				//		//Note: in below check, mox is fixed at ith row 
				//		if(fabs(mox[i][k] - uox[j][k])<1e-3) //part of missing cell = obserbed cell 
				//		{
				//			i_sum_crst++; // increment if a cell of missing row = obs. cell 
				//		}
				//	}
				//	//---
				//	//store how many cells of missing row match those of observed row
				//	//---
				//	i_srst[j] = i_sum_crst; 
				//	if(i_sum_crst==nl) loc_srst_nl.push_back(j+1); //Actual location 				
				//}
				//testout
				/*
				RPrint(" ==Condition2. some cells on a row are missing. i: "); RPrint(i);
				RPrint("nl: "); RPrint(nl);
				RPrint("srst: "); RPrint(i_srst, nr2);
				RPrint("loc_srst_nl: "); RPrint(loc_srst_nl);
				*/

				loc_srst_nl.clear(); //re-initialize

				List_nU.get_block_yicheng(i, loc_srst_nl);

				//-----
				//total matching rows
				//-----
				const int i_size_loc_srst_nl = (int)loc_srst_nl.size();
				if (i_size_loc_srst_nl == 0) //error case
				{
					Rprintf("Error! there is no matched cell! \n"); return 0;
				}

				if (i_size_loc_srst_nl > 0)
				{
					//-----------------
					//make "zid" which means 
					//the row location of current missing row repeated by number of observed rows
					//-----------------
					zid = NULL; //re-initialize; 
					i_size_zid = i_size_v_cn_z_i*i_size_loc_srst_nl;
					zid = new int[i_size_zid];

					for (int j = 0; j<i_size_v_cn_z_i; j++)
					{
						for (int k = 0; k<i_size_loc_srst_nl; k++) //repeated copy of the id number 
						{
							//NOTE: zid contains ACTUAL id number 
							zid[j*i_size_loc_srst_nl + k] = id[v_cn_z_i[j] - 1]; //-1 for actual location
						}
					}

					//-------------------
					//make a matrix that consists of zid & repeated uox for all missing rows
					//-------------------
					double** rst_temp2 = New_dMatrix(i_size_zid, ncol + 1);

					for (int j = 0; j<i_size_v_cn_z_i; j++)
					{
						for (int k = 0; k<i_size_loc_srst_nl; k++) //repeated copy of the id number 
						{
							i_loc = j*i_size_loc_srst_nl + k; //serial number of the entire rows of the matrix

															  //first column is zid[]
							rst_temp2[i_loc][0] = zid[i_loc];

							//from the second through the end columns are occupied with uox
							for (int i_col = 0; i_col<ncol; i_col++)
							{
								//rst_temp2[i_loc][i_col+1] = uox[k][i_col]; //cf. condition 1 form
								rst_temp2[i_loc][i_col + 1] = uox[loc_srst_nl[k] - 1][i_col]; //-1 for actual location

							}
						}
					}
					//---
					//Append the entire matrix to rst
					//---
					rst.bind_blocks(i_size_zid, ncol + 1, rst_temp2);

					//testout
					/*
					RPrint("zid:"); RPrint(zid, i_size_zid);
					RPrint("rst:");
					rst.print_rbind_FHDI();
					*/

					//---------
					//local deallocation
					//---------
					Del_dMatrix(rst_temp2, i_size_zid, ncol + 1);
				}
			}

		} //end of LOOP for all missing rows

		  //----------------
		  //re-order rst in terms of id (the first column)
		  //----------------
		const int n_row_rst = rst.size_row();
		int* i_rst_id = new int[n_row_rst];
		for (int i = 0; i<n_row_rst; i++) i_rst_id[i] = (int)rst(i, 0);
		order_FHDI(i_rst_id, n_row_rst); //returned with the order of rows in ascending magnitude
										 //testout
										 //RPrint("n_row_rst :"); RPrint(n_row_rst);
										 //RPrint("i_rst_id :"); RPrint(i_rst_id, n_row_rst);


										 //--------------------
										 //remove the first column with id
										 //store the rst into the final storage
										 //--------------------
										 //rbind_FHDI rst_final(ncol); //Note: without the first column of id
		double* d_row_rst = new double[ncol + 1];
		double* d_row_rst_short = new double[ncol];
		for (int i = 0; i<n_row_rst; i++)
		{
			rst.get_block(i_rst_id[i] - 1, d_row_rst); //get a row// -1 for actual loc
			for (int k = 0; k<ncol; k++) d_row_rst_short[k] = d_row_rst[k + 1]; //without id  
			rst_final.append_block(d_row_rst_short);	//append a new row to the final storage 
		}

		//testout
		//RPrint("End of AUGMAT =========="); 
		//RPrint("rst_final:"); rst_final.print_rbind_FHDI(); 

		//-------
		//local deallocation
		//-------
		delete[] i_temp_x;
		delete[] zid;
		//delete[] i_srst;
		delete[] i_rst_id;
		delete[] d_row_rst;
		delete[] d_row_rst_short;

		return 1;
	}

} //end of namespace





//Fn===========================================================================

//Cal_W_Extension_cpp.cc-----------------------------------------------------------------------------

//Fn===========================================================================

namespace FHDI

{

bool Cal_W_Extension_cpp(double** mox, const int nrow_mox, 

						 double** uox, const int nrow_uox, 

						 const int ncol, int* id, 

						 std::vector<std::string> v_table_tmvec_row1,

						 std::vector<int> v_table_tmvec_row2,

						 std::vector<double> jp_prob,

						 double** d_mx, const int i_size_ml, 

						 double* w, std::string cn[], const int nrow, 

						 std::vector<double> &v_rst_final)

//Description=========================================

// update weight and joint probability

//

// Algorithm:  All possible donors will be used to fill in the missing cell 

//             but, if there is no matched donors in uox, this algorithm may fail

//             as of Oct 2016

//

// original R code: Dr. Im, J. and Dr. Kim, J. 

// c++ code: 		Dr. Cho, I. 

// All rights reserved

// 

// updated: Oct 26, 2016

//----------------------------------------------------

//IN    : double mox(nrow_mox, ncol)= sorted unique patterns of missing  cells. up to i_count_mox rows are meaningful                           

//IN    : double uox(nrow_uox, ncol)= sorted unique patterns of observed cells. up to i_count_uox rows are meaningful 

//IN    : int id(nrow) = index of row. Default is ACTUAL row number

//IN	: vector<string> v_table_tmvec_row1  = name of table of condensed missing patterns

//IN	: vector<int> v_table_tmvec_row2  = counts of table of condensed missing patterns

//IN   	: vector<double> jp_prob 	= weighted joint probability of all condensed observed DONORS

//IN    : double d_mx(i_size_ml, ncol) = copy of all the missing cells 

//IN    : double w[ml] 				= weights corresponding to missing rows

//IN  	: string cn(nrow)			= vector of string to represent each row of z     

//OUT   : std::vector<double> &v_rst_final  = new weights 

//====================================================

{

	const int nr1 = nrow_mox;

	const int nr2 = nrow_uox;

	const int nx  = i_size_ml; //rows of mx

	

	//-------------------

	//sum of joint probability

	//-------------------

	double sum_jp = 0.0; 

	const int i_size_jp = (int)jp_prob.size(); 

	for(int i=0; i<i_size_jp; i++) sum_jp += jp_prob[i]; 

	

	//-------------------

	//initialize rst, the matrix for storage for augmented observations

	//-------------------

	rbind_FHDI rst(2); //number of columns 



	//--------------------

	//Main Loop for all missing rows

	//--------------------

	int* i_temp_x = new int[ncol];

	int i_sum_x = 0;

	std::vector<int> v_cn_z_i; 

	int* zid = NULL;

	int i_size_zid=0; 

	int i_loc=0;	

	int* i_srst = new int[nx];		//for Condition 1&2

	int* i_srst1 = new int[nr2]; 	//for Condition 2

	std::vector<int> loc_srst_ncol; 

	std::vector<int> loc_srst_ncol1; 

	

	double* w_srst_ncol = NULL;

    double* jp_zi 		= NULL;	

	

	//----------------

	//LOOP for all missing rows

	//----------------

	for(int i=0; i<nr1; i++)

	{

		//---------------------

		// generate sum of rows that indicate the matched rows of mx and mox

		//---------------------

		//indicator matrix that matches the donors

		//srst: row-wise sum of the indicator matrix 

		//-------

		loc_srst_ncol.clear(); //re-initialize

		Fill_iVector(i_srst, nx, 0); //re-initialize 

				

		for(int j=0; j<nx; j++)  //Loop for i_size_ml, all the missing rows

		{

			int i_sum_crst = 0; 

			for(int k=0; k<ncol; k++)

			{

				//Note: in below check, mox is fixed at ith row 

				if(fabs_FHDI(mox[i][k] - d_mx[j][k])<1e-3) //part of missing cell = obserbed cell 

				{

					i_sum_crst++; // increment if a cell of missing row = obs. cell 

				}

			}

			//---

			//store how many cells of the current missing row match those of all missing rows

			//---

			i_srst[j] = i_sum_crst; 

			

			//---

			//store numbers of missing rows that exactly match the current missing row

			//i.e., target rows to be imputed later 

			//---

			if(i_sum_crst==ncol) loc_srst_ncol.push_back(j+1); //Actual location 				

		}

		//-----

		//how many missing rows have the same missing pattern as the current missing row

		//-----

		const int i_size_loc_srst_ncol = (int)loc_srst_ncol.size(); 

		

		//---------------------------------

		//get current row of missing cell 

		//---------------------------------

		for(int j=0; j<ncol; j++) i_temp_x[j] =  mox[i][j]; 

		i_sum_x = sum_FHDI(i_temp_x, ncol); //how many non-zeros in current missing row



		std::string s_temp = v_table_tmvec_row1[i]; //string name of ith missing row

		

		v_cn_z_i.clear(); //re-initialize 

		which(cn, nrow, s_temp, v_cn_z_i); //Note: Actual location is returned

		int i_size_v_cn_z_i = (int)v_cn_z_i.size(); //number of locations in cn having s_temp

		

		//----------------------

		//----------------------

		//Condition 1: this row's cells are all missing

		//----------------------

		//----------------------

		if(i_sum_x == 0)

		{

			//-----------------

			//make "zid" which means 

			//the row location of current missing row repeated by number of observed rows

			//-----------------

			zid = NULL; //re-initialize; 

            

			//-----

			// "i_size_v_cn_z_i" means all the missing rows that have the same pattern as the current missing row

			// so, below "i_size_zid" means that all the observed rows (nr2) will fill the missing rows 

			//-----

			i_size_zid = i_size_v_cn_z_i*nr2; 

			zid = new int[i_size_zid]; 

			

			for(int j=0; j<i_size_v_cn_z_i; j++)

			{

				for(int k=0; k<nr2; k++) //nr2 times repeated copy with the id number 

				{

					//NOTE: zid contains ACTUAL id number 

					//Meaning id's of the missing rows that have the identical pattern as the current missing rows 

					zid[j*nr2 + k] = id[v_cn_z_i[j]-1]; //-1 for actual location

				}

			}

			

			//-------------------

			//get ready w[] at srst = ncol

			//-------------------

			w_srst_ncol = NULL; //re-initialize

			w_srst_ncol = new double[i_size_loc_srst_ncol*nr2];

			for(int j=0; j<i_size_loc_srst_ncol; j++)  //repeat each entity by nr2 times

			{ 

				for(int k=0; k<nr2; k++)

				{

					w_srst_ncol[j*nr2+k] = w[loc_srst_ncol[j]-1]; //-1 for actual location

				}

			} 

			

			//-------------------

			//get ready second column

			//-------------------

			const int z_i_now = v_table_tmvec_row2[i];

			jp_zi = NULL; //re-initialize 

			jp_zi = new double[i_size_jp * z_i_now];

			for(int j=0; j<z_i_now; j++)  //repeat entire jp..[] by z_i_now times 

			{

				for(int k=0; k<i_size_jp; k++) 

					jp_zi[j*i_size_jp + k] = jp_prob[k]/sum_jp; 

			}

			

			//-------------------

			//make a matrix that consists of zid & repeated weights for all missing rows

			//-------------------

			double** rst_temp = New_dMatrix(i_size_zid, 2);

			 

			for(int j=0; j<i_size_v_cn_z_i; j++)

			{

				for(int k=0; k<nr2; k++) //repeated copy of the id number 

				{

					i_loc = j*nr2 + k; //serial number of the entire rows of the matrix

					

					//first column is zid[]

					rst_temp[i_loc][0] = zid[i_loc];

					

					//second column joint prob * weight 

					rst_temp[i_loc][1] = jp_zi[i_loc]*w_srst_ncol[i_loc];

				}

			}

			//---

			//Append the entire matrix to rst

			//---

			rst.bind_blocks(i_size_zid, 2, rst_temp);

		

			//---------

			//local deallocation

			//---------

			Del_dMatrix(rst_temp, i_size_zid, 2);

		}

		

		//----------------------

		//----------------------

		//Condition 2: some cells of current row are not missing

		//----------------------

		//----------------------

		int nl = 0;

		if(i_sum_x > 0)

		{

			//------

			//number of observed cells on this row

			//------

			nl = 0; 

			for(int j=0; j<ncol; j++) 

			{

				if(mox[i][j]>0) nl++; //number of the observed 

			}

			

			//-------

			//indicator matrix that matches the donors

			//srst: row-wise sum of the indicator matrix 

			//-------

			loc_srst_ncol1.clear(); //re-initialize //Note: this is different from loc_srst_ncol

			Fill_iVector(i_srst1, nr2, 0); //re-initialize 

				

			for(int j=0; j<nr2; j++)

			{

				int i_sum_crst = 0; 

				for(int k=0; k<ncol; k++)

				{

					//Note: in below check, mox is fixed at ith row 

					if(fabs_FHDI(mox[i][k] - uox[j][k])<1e-3) //part of missing cell = obserbed cell 

					{

						i_sum_crst++; // increment if a cell of the current missing row = obs. cell 

					}

				}

				//---

				//store how many cells of the current missing row match those of the observed row

				//---

				i_srst1[j] = i_sum_crst; 

				

				//---

				//store row number of the observed that matches the current missing row

				//---

				if(i_sum_crst==nl) loc_srst_ncol1.push_back(j+1); //Actual location 				

			}

			

			//-----

			//total number of the observed rows that matches the current missing row

			//-----

			const int i_size_loc_srst_ncol1 = (int)loc_srst_ncol1.size(); 

			if(i_size_loc_srst_ncol1 == 0) //error case

			{
				Rprintf("Error! there is no matched cell! \n"); 
				
				//deallocation before early return
				delete[] i_temp_x; 
				delete[] zid;
				delete[] i_srst;
				delete[] i_srst1;
				delete[] w_srst_ncol;
				delete[] jp_zi; 
				
				return 0;
			}

			

			if(i_size_loc_srst_ncol1 > 0) 

			{

				//-----------------

				//make "zid" which means 

				//the row location of current missing row repeated by number of observed rows

				//-----------------

				zid = NULL; //re-initialize; 

				i_size_zid = i_size_v_cn_z_i * i_size_loc_srst_ncol1; //Note: .._ncol1 is used NOT .._ncol

				zid = new int[i_size_zid]; 

			

				for(int j=0; j<i_size_v_cn_z_i; j++)

				{

					for(int k=0; k<i_size_loc_srst_ncol1; k++) //repeated copy of the id number 

					{

						//NOTE: zid contains ACTUAL id number 

						zid[j*i_size_loc_srst_ncol1 + k] = id[v_cn_z_i[j]-1]; //-1 for actual location

					}

				}

				

				//-------------------

				//get ready w[] at srst = ncol

				//-------------------

				w_srst_ncol = NULL; //re-initialize

				w_srst_ncol = new double[i_size_loc_srst_ncol * i_size_loc_srst_ncol1];

				for(int j=0; j<i_size_loc_srst_ncol; j++)  //repeat each entity 

				{ 

					for(int k=0; k<i_size_loc_srst_ncol1; k++)

					{

						//------

						//Note: the weights are pulled out from loc_srst_loc NOT .._loc1 

						//      below "loc_srst_ncol" means the target rows to be imputed later

						//------

						w_srst_ncol[j*i_size_loc_srst_ncol1 + k] = w[loc_srst_ncol[j]-1]; //-1 for actual location

					}

				} 

				

				//-------------------

				//get ready second column

				//below "loc_srst_ncol1" contains the obs. row numbers that will serve as donor

				//-------------------

				double sum_jp_loc = 0.0; //sum of jp only at location where srst = ncol

				for(int j=0; j<i_size_loc_srst_ncol1; j++) 

					sum_jp_loc += jp_prob[loc_srst_ncol1[j]-1]; 

				

				const int z_i_now = v_table_tmvec_row2[i];

				jp_zi = new double[i_size_loc_srst_ncol1 * z_i_now];

				for(int j=0; j<z_i_now; j++)  //repeat entire jp..[] by z_i_now times 

				{

					for(int k=0; k<i_size_loc_srst_ncol1; k++) 

						jp_zi[j*i_size_loc_srst_ncol1 + k] 

					           = jp_prob[loc_srst_ncol1[k]-1]/sum_jp_loc; 

				}

				

			

				//-------------------

				//make a matrix that consists of zid & repeated uox for all missing rows

				//-------------------

				double** rst_temp2 = New_dMatrix(i_size_zid, 2);

				 

				for(int j=0; j<i_size_v_cn_z_i; j++)

				{

					for(int k=0; k<i_size_loc_srst_ncol1; k++) //repeated copy of the id number 

					{

						i_loc = j*i_size_loc_srst_ncol1 + k; //serial number of the entire rows of the matrix

						

						//first column is zid[]

						rst_temp2[i_loc][0] = zid[i_loc];

					

						//second column. joint prob * weight 

						rst_temp2[i_loc][1] = jp_zi[i_loc]*w_srst_ncol[i_loc]; //-1 for actual location

					}

				}

				//---

				//Append the entire matrix to rst

				//---

				rst.bind_blocks(i_size_zid, 2, rst_temp2);

			

				//---------

				//local deallocation

				//---------

				Del_dMatrix(rst_temp2, i_size_zid, ncol+1);				

			}

		}



	} //end of LOOP for all missing rows

	

	//----------------

	//re-order rst in terms of id (the first column)

	//----------------

	const int n_row_rst = rst.size_row(); 

	int* i_rst_id = new int[n_row_rst];

	for(int i=0; i<n_row_rst; i++) i_rst_id[i] = (int)rst(i,0);

	order_FHDI(i_rst_id, n_row_rst); //returned with the order of rows in ascending magnitude

    

	//--------------------

	//remove the first column with id

	//store the rst into the final storage

	//--------------------

	double* d_row_rst 		= new double[2]; 

	double  d_row_rst_short = 0.0;

	for(int i=0; i<n_row_rst; i++)

	{

		rst.get_block(i_rst_id[i]-1, d_row_rst); //get a row// -1 for actual loc

		d_row_rst_short = d_row_rst[1]; //without id  

		v_rst_final.push_back(d_row_rst_short);	//append a new row to the final storage 

	}	

	

	//-------

	//local deallocation

	//-------

	delete[] i_temp_x;

	delete[] zid;	

	delete[] i_srst;

	delete[] i_srst1;

	delete[] w_srst_ncol;

	delete[] jp_zi;

	delete[] i_rst_id;

	delete[] d_row_rst;

	

	return 1;

}



} //end of namespace

  //Fn===========================================================================

  //Cal_W_Extension_Bigp_cpp.cc-----------------------------------------------------------------------------

  //Fn===========================================================================

namespace FHDI
{
	bool Cal_W_Extension_Bigp_cpp(double** mox, const int nrow_mox,
		double** uox, const int nrow_uox, const int i_option_collapsing,
		const int ncol, int* id, int** codes,
		std::vector<std::string> v_table_tmvec_row1,
		std::vector<int> v_table_tmvec_row2,
		std::vector<double> jp_prob,
		double** d_mx, const int i_size_ml,
		double* w, std::string cn[], const int nrow,
		std::vector<double> &v_rst_final)
		//Description=========================================
		// update weight and joint probability
		//
		// Algorithm:  All possible donors will be used to fill in the missing cell 
		//             but, if there is no matched donors in uox, this algorithm may fail
		//             as of Oct 2016
		//
		// original R code: Dr. Im, J. and Dr. Kim, J. 
		// c++ code: 		Dr. Cho, I and Yicheng Yang. 
		// All rights reserved
		// 
		// updated: Feb 28, 2020
		//----------------------------------------------------
		//IN    : double mox(nrow_mox, ncol)= sorted unique patterns of missing  cells. up to i_count_mox rows are meaningful                           
		//IN    : double uox(nrow_uox, ncol)= sorted unique patterns of observed cells. up to i_count_uox rows are meaningful 
		//IN    : int id(nrow) = index of row. Default is ACTUAL row number
		//IN	: vector<string> v_table_tmvec_row1  = name of table of condensed missing patterns
		//IN	: vector<int> v_table_tmvec_row2  = counts of table of condensed missing patterns
		//IN   	: vector<double> jp_prob 	= weighted joint probability of all condensed observed DONORS
		//IN    : double d_mx(i_size_ml, ncol) = copy of all the missing cells 
		//IN    : double w[ml] 				= weights corresponding to missing rows
		//IN  	: string cn(nrow)			= vector of string to represent each row of z   
		//IN    : int i_option_collapsing = choice of big-p algorithm 
		//                               0= no big-p algorithms
		//                              !0= perform big-p algorithms
		//IN   :  int codes(nrow, i_option_collapsing); // storage to record most correlated variables of mox
		//OUT   : std::vector<double> &v_rst_final  = new weights 
		//====================================================
	{
		const int nr1 = nrow_mox;
		const int nr2 = nrow_uox;
		const int nx = i_size_ml; //rows of mx

								  //-------------------
								  //sum of joint probability
								  //-------------------
		double sum_jp = 0.0;
		const int i_size_jp = (int)jp_prob.size();
		for (int i = 0; i < i_size_jp; i++) sum_jp += jp_prob[i];

		//-------------------
		//initialize rst, the matrix for storage for augmented observations
		//-------------------
		rbind_FHDI rst(2); //number of columns 

						   //--------------------
						   //Main Loop for all missing rows
						   //--------------------
		int* i_temp_x = new int[ncol];
		int i_sum_x = 0;
		std::vector<int> v_cn_z_i;
		int* zid = NULL;
		int i_size_zid = 0;
		int i_loc = 0;
		int* i_srst = new int[nx];		//for Condition 1&2
		int* i_srst1 = new int[nr2]; 	//for Condition 2
		std::vector<int> loc_srst_ncol;
		std::vector<int> loc_srst_ncol1;
		std::vector<int> v_mxl; // hold the most correlated variables of mox[i]

		double* w_srst_ncol = NULL;
		double* jp_zi = NULL;

		//----------------
		//LOOP for all missing rows
		//----------------
		for (int i = 0; i < nr1; i++)
		{
			//---------------------
			// generate sum of rows that indicate the matched rows of mx and mox
			//---------------------
			//indicator matrix that matches the donors
			//srst: row-wise sum of the indicator matrix 
			//-------
			loc_srst_ncol.clear(); //re-initialize
			Fill_iVector(i_srst, nx, 0); //re-initialize 

			for (int j = 0; j < nx; j++)  //Loop for i_size_ml, all the missing rows
			{
				int i_sum_crst = 0;
				for (int k = 0; k < ncol; k++)
				{
					//Note: in below check, mox is fixed at ith row 
					if (fabs_FHDI(mox[i][k] - d_mx[j][k]) < 1e-3) //part of missing cell = obserbed cell 
					{
						i_sum_crst++; // increment if a cell of missing row = obs. cell 
					}
				}
				//---
				//store how many cells of the current missing row match those of all missing rows
				//---
				i_srst[j] = i_sum_crst;

				//---
				//store numbers of missing rows that exactly match the current missing row
				//i.e., target rows to be imputed later 
				//---
				if (i_sum_crst == ncol) loc_srst_ncol.push_back(j + 1); //Actual location 				
			}
			//-----
			//how many missing rows have the same missing pattern as the current missing row
			//-----
			const int i_size_loc_srst_ncol = (int)loc_srst_ncol.size();

			//---------------------------------
			//get current row of missing cell 
			//---------------------------------
			for (int j = 0; j < ncol; j++) i_temp_x[j] = mox[i][j];
			i_sum_x = sum_FHDI(i_temp_x, ncol); //how many non-zeros in current missing row

			std::string s_temp = v_table_tmvec_row1[i]; //string name of ith missing row

			v_cn_z_i.clear(); //re-initialize 
			which(cn, nrow, s_temp, v_cn_z_i); //Note: Actual location is returned
			int i_size_v_cn_z_i = (int)v_cn_z_i.size(); //number of locations in cn having s_temp

														//----------------------
														//----------------------
														//Condition 1: this row's cells are all missing
														//----------------------
														//----------------------
			if (i_sum_x == 0)
			{
				//-----------------
				//make "zid" which means 
				//the row location of current missing row repeated by number of observed rows
				//-----------------
				zid = NULL; //re-initialize; 

							//-----
							// "i_size_v_cn_z_i" means all the missing rows that have the same pattern as the current missing row
							// so, below "i_size_zid" means that all the observed rows (nr2) will fill the missing rows 
							//-----
				i_size_zid = i_size_v_cn_z_i*nr2;
				zid = new int[i_size_zid];

				for (int j = 0; j < i_size_v_cn_z_i; j++)
				{
					for (int k = 0; k < nr2; k++) //nr2 times repeated copy with the id number 
					{
						//NOTE: zid contains ACTUAL id number 
						//Meaning id's of the missing rows that have the identical pattern as the current missing rows 
						zid[j*nr2 + k] = id[v_cn_z_i[j] - 1]; //-1 for actual location
					}
				}

				//-------------------
				//get ready w[] at srst = ncol
				//-------------------
				w_srst_ncol = NULL; //re-initialize
				w_srst_ncol = new double[i_size_loc_srst_ncol*nr2];
				for (int j = 0; j < i_size_loc_srst_ncol; j++)  //repeat each entity by nr2 times
				{
					for (int k = 0; k < nr2; k++)
					{
						w_srst_ncol[j*nr2 + k] = w[loc_srst_ncol[j] - 1]; //-1 for actual location
					}
				}

				//-------------------
				//get ready second column
				//-------------------
				const int z_i_now = v_table_tmvec_row2[i];
				jp_zi = NULL; //re-initialize 
				jp_zi = new double[i_size_jp * z_i_now];
				for (int j = 0; j < z_i_now; j++)  //repeat entire jp..[] by z_i_now times 
				{
					for (int k = 0; k < i_size_jp; k++)
						jp_zi[j*i_size_jp + k] = jp_prob[k] / sum_jp;
				}

				//-------------------
				//make a matrix that consists of zid & repeated weights for all missing rows
				//-------------------
				double** rst_temp = New_dMatrix(i_size_zid, 2);

				for (int j = 0; j < i_size_v_cn_z_i; j++)
				{
					for (int k = 0; k < nr2; k++) //repeated copy of the id number 
					{
						i_loc = j*nr2 + k; //serial number of the entire rows of the matrix

										   //first column is zid[]
						rst_temp[i_loc][0] = zid[i_loc];

						//second column joint prob * weight 
						rst_temp[i_loc][1] = jp_zi[i_loc] * w_srst_ncol[i_loc];
					}
				}
				//---
				//Append the entire matrix to rst
				//---
				rst.bind_blocks(i_size_zid, 2, rst_temp);

				//testout
				/*
				RPrint(" == in Cal_W Condition 1 ==== i: "); RPrint(i);
				RPrint("zid:"); RPrint(zid, i_size_zid);
				RPrint("rst:");
				rst.print_rbind_FHDI();
				*/

				//---------
				//local deallocation
				//---------
				Del_dMatrix(rst_temp, i_size_zid, 2);
			}

			//----------------------
			//----------------------
			//Condition 2: some cells of current row are not missing
			//----------------------
			//----------------------
			int nl = 0;
			if (i_sum_x > 0)
			{
				//------
				//number of observed cells on this row
				//------
				nl = 0;
				for (int j = 0; j < ncol; j++)
				{
					if (mox[i][j] > 0) nl++; //number of the observed 
				}

				if (nl > i_option_collapsing) {
					nl = i_option_collapsing;
				}

				//-------
				//indicator matrix that matches the donors
				//srst: row-wise sum of the indicator matrix 
				//-------
				loc_srst_ncol1.clear(); //re-initialize //Note: this is different from loc_srst_ncol
				Fill_iVector(i_srst1, nr2, 0); //re-initialize 
				v_mxl.clear();

				//inherents the most correlated variables of mox[i]
				for (int k = 0; k < i_option_collapsing; k++) {
					if (codes[i][k] != 0) {
						v_mxl.push_back(codes[i][k]);
					}
				}

				int v_mxl_size = v_mxl.size();

				for (int j = 0; j < nr2; j++)
				{
					int i_sum_crst = 0;
					for (int k = 0; k < v_mxl_size; k++)
					{
						//Note: in below check, mox is fixed at ith row 
						if (fabs_FHDI(mox[i][v_mxl[k] - 1] - uox[j][v_mxl[k] - 1]) < 1e-3) // Note v_mxl records the actual locations !!!
						{
							i_sum_crst++; // increment if a cell of the current missing row = obs. cell 
						}
					}
					//---
					//store how many cells of the current missing row match those of the observed row
					//---
					i_srst1[j] = i_sum_crst;

					//---
					//store row number of the observed that matches the current missing row
					//---
					if (i_sum_crst == nl) loc_srst_ncol1.push_back(j + 1); //Actual location 				
				}
				//testout
				/*
				RPrint(" == in Cal_W Condition2. ====== i: "); RPrint(i);
				RPrint("nl: "); RPrint(nl);
				RPrint("srst: "); RPrint(i_srst, nr2);
				RPrint("loc_srst_ncol: "); RPrint(loc_srst_ncol);
				*/

				//-----
				//total number of the observed rows that matches the current missing row
				//-----
				const int i_size_loc_srst_ncol1 = (int)loc_srst_ncol1.size();
				if (i_size_loc_srst_ncol1 == 0) //error case
				{
					Rprintf("Error! there is no matched cell! \n");

					//deallocation before early return
					delete[] i_temp_x;
					delete[] zid;
					delete[] i_srst;
					delete[] i_srst1;
					delete[] w_srst_ncol;
					delete[] jp_zi;

					return 0;
				}

				if (i_size_loc_srst_ncol1 > 0)
				{
					//-----------------
					//make "zid" which means 
					//the row location of current missing row repeated by number of observed rows
					//-----------------
					zid = NULL; //re-initialize; 
					i_size_zid = i_size_v_cn_z_i * i_size_loc_srst_ncol1; //Note: .._ncol1 is used NOT .._ncol
					zid = new int[i_size_zid];

					for (int j = 0; j < i_size_v_cn_z_i; j++)
					{
						for (int k = 0; k < i_size_loc_srst_ncol1; k++) //repeated copy of the id number 
						{
							//NOTE: zid contains ACTUAL id number 
							zid[j*i_size_loc_srst_ncol1 + k] = id[v_cn_z_i[j] - 1]; //-1 for actual location
						}
					}

					//-------------------
					//get ready w[] at srst = ncol
					//-------------------
					w_srst_ncol = NULL; //re-initialize
					w_srst_ncol = new double[i_size_loc_srst_ncol * i_size_loc_srst_ncol1];
					for (int j = 0; j < i_size_loc_srst_ncol; j++)  //repeat each entity 
					{
						for (int k = 0; k < i_size_loc_srst_ncol1; k++)
						{
							//------
							//Note: the weights are pulled out from loc_srst_loc NOT .._loc1 
							//      below "loc_srst_ncol" means the target rows to be imputed later
							//------
							w_srst_ncol[j*i_size_loc_srst_ncol1 + k] = w[loc_srst_ncol[j] - 1]; //-1 for actual location
						}
					}

					//-------------------
					//get ready second column
					//below "loc_srst_ncol1" contains the obs. row numbers that will serve as donor
					//-------------------
					double sum_jp_loc = 0.0; //sum of jp only at location where srst = ncol
					for (int j = 0; j < i_size_loc_srst_ncol1; j++)
						sum_jp_loc += jp_prob[loc_srst_ncol1[j] - 1];

					const int z_i_now = v_table_tmvec_row2[i];
					jp_zi = new double[i_size_loc_srst_ncol1 * z_i_now];
					for (int j = 0; j < z_i_now; j++)  //repeat entire jp..[] by z_i_now times 
					{
						for (int k = 0; k < i_size_loc_srst_ncol1; k++)
							jp_zi[j*i_size_loc_srst_ncol1 + k]
							= jp_prob[loc_srst_ncol1[k] - 1] / sum_jp_loc;
					}


					//-------------------
					//make a matrix that consists of zid & repeated uox for all missing rows
					//-------------------
					double** rst_temp2 = New_dMatrix(i_size_zid, 2);

					for (int j = 0; j < i_size_v_cn_z_i; j++)
					{
						for (int k = 0; k < i_size_loc_srst_ncol1; k++) //repeated copy of the id number 
						{
							i_loc = j*i_size_loc_srst_ncol1 + k; //serial number of the entire rows of the matrix

																 //first column is zid[]
							rst_temp2[i_loc][0] = zid[i_loc];

							//second column. joint prob * weight 
							rst_temp2[i_loc][1] = jp_zi[i_loc] * w_srst_ncol[i_loc]; //-1 for actual location
						}
					}
					//---
					//Append the entire matrix to rst
					//---
					rst.bind_blocks(i_size_zid, 2, rst_temp2);

					//testout
					/*
					RPrint("zid:"); RPrint(zid, i_size_zid);
					RPrint("rst:");
					rst.print_rbind_FHDI();
					*/

					//---------
					//local deallocation
					//---------
					Del_dMatrix(rst_temp2, i_size_zid, ncol + 1);
				}
			}

		} //end of LOOP for all missing rows

		  //----------------
		  //re-order rst in terms of id (the first column)
		  //----------------
		const int n_row_rst = rst.size_row();
		int* i_rst_id = new int[n_row_rst];
		for (int i = 0; i < n_row_rst; i++) i_rst_id[i] = (int)rst(i, 0);
		order_FHDI(i_rst_id, n_row_rst); //returned with the order of rows in ascending magnitude
										 //testout
										 //RPrint("n_row_rst :"); RPrint(n_row_rst);
										 //RPrint("i_rst_id :"); RPrint(i_rst_id, n_row_rst);


										 //--------------------
										 //remove the first column with id
										 //store the rst into the final storage
										 //--------------------
		double* d_row_rst = new double[2];
		double  d_row_rst_short = 0.0;
		for (int i = 0; i < n_row_rst; i++)
		{
			rst.get_block(i_rst_id[i] - 1, d_row_rst); //get a row// -1 for actual loc
			d_row_rst_short = d_row_rst[1]; //without id  
			v_rst_final.push_back(d_row_rst_short);	//append a new row to the final storage 
		}

		//testout
		//RPrint("End of Cal_W =========="); 
		//RPrint("v_rst_final:"); RPrint(v_rst_final); 


		//-------
		//local deallocation
		//-------
		delete[] i_temp_x;
		delete[] zid;
		delete[] i_srst;
		delete[] i_srst1;
		delete[] w_srst_ncol;
		delete[] jp_zi;
		delete[] i_rst_id;
		delete[] d_row_rst;

		return 1;

	}

} //end of namespace

  //Fn===========================================================================

  //Cal_W_Neighbor_cpp.cc-----------------------------------------------------------------------------

  //Fn===========================================================================

namespace FHDI
{
	bool Cal_W_Neighbor_cpp(double** mox, const int nrow_mox,
		double** uox, const int nrow_uox,
		const int ncol, int* id, List_FHDI &List_nU,
		std::vector<std::string> v_table_tmvec_row1,
		std::vector<int> v_table_tmvec_row2,
		std::vector<double> jp_prob,
		double** d_mx, const int i_size_ml,
		double* w, std::string cn[], const int nrow,
		std::vector<double> &v_rst_final)
		//Description=========================================
		// update weight and joint probability
		//
		// Algorithm:  All possible donors will be used to fill in the missing cell 
		//             but, if there is no matched donors in uox, this algorithm may fail
		//             as of Oct 2016
		//
		// original R code: Dr. Im, J. and Dr. Kim, J. 
		// c++ code: 		Dr. Cho, I. 
		// All rights reserved
		// 
		// updated: Oct 26, 2016
		//----------------------------------------------------
		//IN    : double mox(nrow_mox, ncol)= sorted unique patterns of missing  cells. up to i_count_mox rows are meaningful                           
		//IN    : double uox(nrow_uox, ncol)= sorted unique patterns of observed cells. up to i_count_uox rows are meaningful 
		//IN    : int id(nrow) = index of row. Default is ACTUAL row number
		//IN	: vector<string> v_table_tmvec_row1  = name of table of condensed missing patterns
		//IN	: vector<int> v_table_tmvec_row2  = counts of table of condensed missing patterns
		//IN   	: vector<double> jp_prob 	= weighted joint probability of all condensed observed DONORS
		//IN    : double d_mx(i_size_ml, ncol) = copy of all the missing cells 
		//IN    : double w[ml] 				= weights corresponding to missing rows
		//IN  	: string cn(nrow)			= vector of string to represent each row of z     
		//OUT   : std::vector<double> &v_rst_final  = new weights 
		//====================================================
	{
		const int nr1 = nrow_mox;
		const int nr2 = nrow_uox;
		const int nx = i_size_ml; //rows of mx

								  //-------------------
								  //sum of joint probability
								  //-------------------
		double sum_jp = 0.0;
		const int i_size_jp = (int)jp_prob.size();
		for (int i = 0; i<i_size_jp; i++) sum_jp += jp_prob[i];

		//-------------------
		//initialize rst, the matrix for storage for augmented observations
		//-------------------
		rbind_FHDI rst(2); //number of columns 

						   //--------------------
						   //Main Loop for all missing rows
						   //--------------------
		int* i_temp_x = new int[ncol];
		int i_sum_x = 0;
		std::vector<int> v_cn_z_i; // actual location of mox[i] in z
		int* zid = NULL;
		int i_size_zid = 0;
		int i_loc = 0;
		int* i_srst = new int[nx];		//for Condition 1&2
		int* i_srst1 = new int[nr2]; 	//for Condition 2
		std::vector<int> loc_srst_ncol; // actual locations of mox[i] in ml
		std::vector<int> loc_srst_ncol1; // actual locations of donors of mox[i] in uox

		double* w_srst_ncol = NULL;
		double* jp_zi = NULL;

		//----------------
		//LOOP for all missing rows
		//----------------
		for (int i = 0; i<nr1; i++)
		{
			//---------------------
			// generate sum of rows that indicate the matched rows of mx and mox
			//---------------------
			//indicator matrix that matches the donors
			//srst: row-wise sum of the indicator matrix 
			//-------
			loc_srst_ncol.clear(); //re-initialize
			Fill_iVector(i_srst, nx, 0); //re-initialize 

			for (int j = 0; j<nx; j++)  //Loop for i_size_ml, all the missing rows
			{
				int i_sum_crst = 0;
				for (int k = 0; k<ncol; k++)
				{
					//Note: in below check, mox is fixed at ith row 
					if (fabs(mox[i][k] - d_mx[j][k])<1e-3) //part of missing cell = obserbed cell 
					{
						i_sum_crst++; // increment if a cell of missing row = obs. cell 
					}
				}
				//---
				//store how many cells of the current missing row match those of all missing rows
				//---
				i_srst[j] = i_sum_crst;

				//---
				//store numbers of missing rows that exactly match the current missing row
				//i.e., target rows to be imputed later 
				//---
				if (i_sum_crst == ncol) loc_srst_ncol.push_back(j + 1); //Actual location 				
			}
			//-----
			//how many missing rows have the same missing pattern as the current missing row
			//-----
			const int i_size_loc_srst_ncol = (int)loc_srst_ncol.size();

			//---------------------------------
			//get current row of missing cell 
			//---------------------------------
			for (int j = 0; j<ncol; j++) i_temp_x[j] = mox[i][j];
			i_sum_x = sum_FHDI(i_temp_x, ncol); //how many non-zeros in current missing row

			std::string s_temp = v_table_tmvec_row1[i]; //string name of ith missing row

			v_cn_z_i.clear(); //re-initialize 
			which(cn, nrow, s_temp, v_cn_z_i); //Note: Actual location of mox[i] in z is returned
			int i_size_v_cn_z_i = (int)v_cn_z_i.size(); //number of locations in cn having s_temp

														//----------------------
														//----------------------
														//Condition 1: this row's cells are all missing
														//----------------------
														//----------------------
			if (i_sum_x == 0)
			{
				//-----------------
				//make "zid" which means 
				//the row location of current missing row repeated by number of observed rows
				//-----------------
				zid = NULL; //re-initialize; 

							//-----
							// "i_size_v_cn_z_i" means all the missing rows that have the same pattern as the current missing row
							// so, below "i_size_zid" means that all the observed rows (nr2) will fill the missing rows 
							//-----
				i_size_zid = i_size_v_cn_z_i*nr2;
				zid = new int[i_size_zid];

				for (int j = 0; j<i_size_v_cn_z_i; j++)
				{
					for (int k = 0; k<nr2; k++) //nr2 times repeated copy with the id number 
					{
						//NOTE: zid contains ACTUAL id number 
						//Meaning id's of the missing rows that have the identical pattern as the current missing rows 
						zid[j*nr2 + k] = id[v_cn_z_i[j] - 1]; //-1 for actual location
					}
				}

				//-------------------
				//get ready w[] at srst = ncol
				//-------------------
				w_srst_ncol = NULL; //re-initialize
				w_srst_ncol = new double[i_size_loc_srst_ncol*nr2];
				for (int j = 0; j<i_size_loc_srst_ncol; j++)  //repeat each entity by nr2 times
				{
					for (int k = 0; k<nr2; k++)
					{
						w_srst_ncol[j*nr2 + k] = w[loc_srst_ncol[j] - 1]; //-1 for actual location
					}
				}

				//-------------------
				//get ready second column
				//-------------------
				const int z_i_now = v_table_tmvec_row2[i];
				jp_zi = NULL; //re-initialize 
				jp_zi = new double[i_size_jp * z_i_now];
				for (int j = 0; j<z_i_now; j++)  //repeat entire jp..[] by z_i_now times 
				{
					for (int k = 0; k<i_size_jp; k++)
						jp_zi[j*i_size_jp + k] = jp_prob[k] / sum_jp;
				}

				//-------------------
				//make a matrix that consists of zid & repeated weights for all missing rows
				//-------------------
				double** rst_temp = New_dMatrix(i_size_zid, 2);

				for (int j = 0; j<i_size_v_cn_z_i; j++)
				{
					for (int k = 0; k<nr2; k++) //repeated copy of the id number 
					{
						i_loc = j*nr2 + k; //serial number of the entire rows of the matrix

										   //first column is zid[]
						rst_temp[i_loc][0] = zid[i_loc];

						//second column joint prob * weight 
						rst_temp[i_loc][1] = jp_zi[i_loc] * w_srst_ncol[i_loc];
					}
				}
				//---
				//Append the entire matrix to rst
				//---
				rst.bind_blocks(i_size_zid, 2, rst_temp);

				//testout
				/*
				RPrint(" == in Cal_W Condition 1 ==== i: "); RPrint(i);
				RPrint("zid:"); RPrint(zid, i_size_zid);
				RPrint("rst:");
				rst.print_rbind_FHDI();
				*/

				//---------
				//local deallocation
				//---------
				Del_dMatrix(rst_temp, i_size_zid, 2);
			}

			//----------------------
			//----------------------
			//Condition 2: some cells of current row are not missing
			//----------------------
			//----------------------
			//int nl = 0;
			if (i_sum_x > 0)
			{
				////------
				////number of observed cells on this row
				////------
				//nl = 0; 
				//for(int j=0; j<ncol; j++) 
				//{
				//	if(mox[i][j]>0) nl++; //number of the observed 
				//}
				//
				////-------
				////indicator matrix that matches the donors
				////srst: row-wise sum of the indicator matrix 
				////-------
				//loc_srst_ncol1.clear(); //re-initialize //Note: this is different from loc_srst_ncol
				//Fill_iVector(i_srst1, nr2, 0); //re-initialize 
				//	
				//for(int j=0; j<nr2; j++)
				//{
				//	int i_sum_crst = 0; 
				//	for(int k=0; k<ncol; k++)
				//	{
				//		//Note: in below check, mox is fixed at ith row 
				//		if(fabs(mox[i][k] - uox[j][k])<1e-3) //part of missing cell = obserbed cell 
				//		{
				//			i_sum_crst++; // increment if a cell of the current missing row = obs. cell 
				//		}
				//	}
				//	//---
				//	//store how many cells of the current missing row match those of the observed row
				//	//---
				//	i_srst1[j] = i_sum_crst; 
				//	
				//	//---
				//	//store row number of the observed that matches the current missing row
				//	//---
				//	if(i_sum_crst==nl) loc_srst_ncol1.push_back(j+1); //Actual location 				
				//}
				//testout
				/*
				RPrint(" == in Cal_W Condition2. ====== i: "); RPrint(i);
				RPrint("nl: "); RPrint(nl);
				RPrint("srst: "); RPrint(i_srst, nr2);
				RPrint("loc_srst_ncol: "); RPrint(loc_srst_ncol);
				*/
				loc_srst_ncol1.clear(); //re-initialize //Note: this is different from loc_srst_ncol
				Fill_iVector(i_srst1, nr2, 0); //re-initialize 

				List_nU.get_block_yicheng(i, loc_srst_ncol1);
				//-----
				//total number of the observed rows that matches the current missing row
				//-----
				const int i_size_loc_srst_ncol1 = (int)loc_srst_ncol1.size();
				if (i_size_loc_srst_ncol1 == 0) //error case

				{
					Rprintf("Error! there is no matched cell! \n");

					//deallocation before early return
					delete[] i_temp_x;
					delete[] zid;
					delete[] i_srst;
					delete[] i_srst1;
					delete[] w_srst_ncol;
					delete[] jp_zi;

					return 0;
				}


				if (i_size_loc_srst_ncol1 > 0)
				{
					//-----------------
					//make "zid" which means 
					//the row location of current missing row repeated by number of observed rows
					//-----------------
					zid = NULL; //re-initialize; 
					i_size_zid = i_size_v_cn_z_i * i_size_loc_srst_ncol1; //Note: .._ncol1 is used NOT .._ncol
					zid = new int[i_size_zid];

					for (int j = 0; j<i_size_v_cn_z_i; j++)
					{
						for (int k = 0; k<i_size_loc_srst_ncol1; k++) //repeated copy of the id number 
						{
							//NOTE: zid contains ACTUAL id number 
							zid[j*i_size_loc_srst_ncol1 + k] = id[v_cn_z_i[j] - 1]; //-1 for actual location
						}
					}

					//-------------------
					//get ready w[] at srst = ncol
					//-------------------
					w_srst_ncol = NULL; //re-initialize
					w_srst_ncol = new double[i_size_loc_srst_ncol * i_size_loc_srst_ncol1];
					for (int j = 0; j<i_size_loc_srst_ncol; j++)  //repeat each entity 
					{
						for (int k = 0; k<i_size_loc_srst_ncol1; k++)
						{
							//------
							//Note: the weights are pulled out from loc_srst_loc NOT .._loc1 
							//      below "loc_srst_ncol" means the target rows to be imputed later
							//------
							w_srst_ncol[j*i_size_loc_srst_ncol1 + k] = w[loc_srst_ncol[j] - 1]; //-1 for actual location
						}
					}

					//-------------------
					//get ready second column
					//below "loc_srst_ncol1" contains the obs. row numbers that will serve as donor
					//-------------------
					double sum_jp_loc = 0.0; //sum of jp only at location where srst = ncol
					for (int j = 0; j<i_size_loc_srst_ncol1; j++)
						sum_jp_loc += jp_prob[loc_srst_ncol1[j] - 1];

					const int z_i_now = v_table_tmvec_row2[i];
					jp_zi = new double[i_size_loc_srst_ncol1 * z_i_now];
					for (int j = 0; j<z_i_now; j++)  //repeat entire jp..[] by z_i_now times 
					{
						for (int k = 0; k<i_size_loc_srst_ncol1; k++)
							jp_zi[j*i_size_loc_srst_ncol1 + k]
							= jp_prob[loc_srst_ncol1[k] - 1] / sum_jp_loc;
					}


					//-------------------
					//make a matrix that consists of zid & repeated uox for all missing rows
					//-------------------
					double** rst_temp2 = New_dMatrix(i_size_zid, 2);

					for (int j = 0; j<i_size_v_cn_z_i; j++)
					{
						for (int k = 0; k<i_size_loc_srst_ncol1; k++) //repeated copy of the id number 
						{
							i_loc = j*i_size_loc_srst_ncol1 + k; //serial number of the entire rows of the matrix

																 //first column is zid[]
							rst_temp2[i_loc][0] = zid[i_loc];

							//second column. joint prob * weight 
							rst_temp2[i_loc][1] = jp_zi[i_loc] * w_srst_ncol[i_loc]; //-1 for actual location
						}
					}
					//---
					//Append the entire matrix to rst
					//---
					rst.bind_blocks(i_size_zid, 2, rst_temp2);

					//testout
					/*
					RPrint("zid:"); RPrint(zid, i_size_zid);
					RPrint("rst:");
					rst.print_rbind_FHDI();
					*/

					//---------
					//local deallocation
					//---------
					Del_dMatrix(rst_temp2, i_size_zid, ncol + 1);
				}
			}

		} //end of LOOP for all missing rows

		  //----------------
		  //re-order rst in terms of id (the first column)
		  //----------------
		const int n_row_rst = rst.size_row();
		int* i_rst_id = new int[n_row_rst];
		for (int i = 0; i<n_row_rst; i++) i_rst_id[i] = (int)rst(i, 0);
		order_FHDI(i_rst_id, n_row_rst); //returned with the order of rows in ascending magnitude
										 //testout
										 //RPrint("n_row_rst :"); RPrint(n_row_rst);
										 //RPrint("i_rst_id :"); RPrint(i_rst_id, n_row_rst);


										 //--------------------
										 //remove the first column with id
										 //store the rst into the final storage
										 //--------------------
		double* d_row_rst = new double[2];
		double  d_row_rst_short = 0.0;
		for (int i = 0; i<n_row_rst; i++)
		{
			rst.get_block(i_rst_id[i] - 1, d_row_rst); //get a row// -1 for actual loc
			d_row_rst_short = d_row_rst[1]; //without id  
			v_rst_final.push_back(d_row_rst_short);	//append a new row to the final storage 
		}

		//testout
		//RPrint("End of Cal_W =========="); 
		//RPrint("v_rst_final:"); RPrint(v_rst_final); 


		//-------
		//local deallocation
		//-------
		delete[] i_temp_x;
		delete[] zid;
		delete[] i_srst;
		delete[] i_srst1;
		delete[] w_srst_ncol;
		delete[] jp_zi;
		delete[] i_rst_id;
		delete[] d_row_rst;

		return 1;
	}

} //end of namespace


//Fn===========================================================================

//Cell_Prob_Extension_cpp.cc-----------------------------------------------------------------------------

//Fn===========================================================================

namespace FHDI{



bool Cell_Prob_Extension_cpp(double** z, const int nrow, const int ncol,

							 std::vector<double> &jp_prob_return,

							 std::vector<std::string> &jp_name_return, 

							 double* w, int* id)



//Description=========================================

// make joint probability of cells with the categorized matrix z 

// where 0 means missing data

//

//

// original R code: Dr. Im, J. and Dr. Kim, J. 

// c++ code: 		Dr. Cho, I. 

// All rights reserved

// 

// updated: March 28, 2017

//----------------------------------------------------

//IN    : double z(nrow, ncol)  = catorized matrix corresponding to original matrix x

//                                initialized with 0.0 

//IN    : double w(nrow) = weights for rows (default = 1.0)

//IN    : int    id(nrow) = row id (default = sequential row numbers)

//

//OUT   : std::vector<double>      jp_prob_return  = final updated joint probability 

//OUT   : std::vector<std::string> jp_name_return  = name of final updated joint probability 

//====================================================

{

	//-------------

	//maximum number of iterations for updating weights

	//-------------

	const int n_maximum_iteration = nrow*100; //set by user!

	



	//--------------

	//locations of missing cells (ml) and observed cells (ol)

	//Note: unlike in Cell_Make..(), std vector is used here

	//--------------

	std::vector<int> ol; //Actual row number 

	std::vector<int> ml; //Actual row number 



	double d_temp=0.0; 

	for(int i_row=0; i_row<nrow; i_row++)

	{

		d_temp=1.0; 

		for(int i_col=0; i_col<ncol; i_col++)

		{

			if(z[i_row][i_col] == 0) {d_temp=0.0; break;} //found zero, i.e. missing cell

		}

		

		if(fabs_FHDI(d_temp) > 1e-15 ) //this row has no missing cells

		{ol.push_back(i_row + 1);} //actual number of the row having no missing cells

		

		if(fabs_FHDI(d_temp) < 1e-15) //this row has AT LEAST one missing cells

		{ml.push_back(i_row + 1);}  //actual number of the row having missing cells

	}

	const int i_size_ol = (int)ol.size(); 

	const int i_size_ml = (int)ml.size(); 

	if(i_size_ol ==0) {Rprintf("Error! no observed unit in Cell_Prob. \n"); return 0; }

	if(i_size_ml ==0) {Rprintf("Error! no missing  unit in Cell_Prob. \n"); return 0; }



	//----------------

	//weights corresponding to missing rows. Will be used for Cal_W..() later

	//select out weights at missing rows

	//----------------

	double* w_ml = new double[i_size_ml];

	for(int i=0; i<i_size_ml; i++) w_ml[i]  = w[ml[i] - 1] ; //-1 for actual loc

	

	//--------------

	//Rows having only observed data (categorized) 

	//Rows having AT LEAST one missing data (categorized)

	//--------------

	double** d_ox = New_dMatrix(i_size_ol, ncol);

	double** d_mx = New_dMatrix(i_size_ml, ncol);

	for(int i=0; i<i_size_ol; i++) 

	{

		for(int j=0; j<ncol; j++) 

		{

			d_ox[i][j] = z[ol[i]-1][j]; //-1 for Actual loc

		}

	}

	for(int i=0; i<i_size_ml; i++) 

	{

		for(int j=0; j<ncol; j++) 

		{

			d_mx[i][j] = z[ml[i]-1][j];	//-1 for Actual loc

		}

	}

	

	//--------------

	//transform z into condensed string format

	//--------------

	//std::string cn[nrow]; //declaration of concatenated vector of z

	//std::string cn0[nrow]; //backup of cn

	std::string *cn = new std::string[nrow]; //declaration of concatenated vector of z

	std::string *cn0 = new std::string[nrow]; //backup of cn	

	Trans(z, nrow, ncol, cn);

	for(int i=0; i<nrow; i++) cn0[i] = cn[i]; 

	

	//---------------

	//Rows of Condensed Strings with Observed cells

	//                          with AT LEAT One Missing  cell

	//---------------

	//std::string s_ocn[i_size_ol];

	//std::string s_mcn[i_size_ml];

	std::string *s_ocn = new std::string[i_size_ol];

	std::string *s_mcn = new std::string[i_size_ml];	

	for(int i=0; i<i_size_ol; i++) s_ocn[i] = cn[ol[i]-1]; //-1 for actual row

	for(int i=0; i<i_size_ml; i++) s_mcn[i] = cn[ml[i]-1]; //-1 for actual row



	//---------------------

	//---------------------

	//make UNIQUE patterns of z by cn

	//i.e., uox and mox

	//---------------------

	//step. Sort the "cn"

	//---------------------

	//std::string s_ocn_temp[i_size_ol]; //string vector of observed patterns only

	//std::string s_mcn_temp[i_size_ml]; //string vector of missing patterns only

	std::string *s_ocn_temp = new std::string[i_size_ol]; //string vector of observed patterns only

	std::string *s_mcn_temp = new std::string[i_size_ml]; //string vector of missing patterns only	

	for(int i=0; i<i_size_ol; i++) {s_ocn_temp[i] = s_ocn[i];} 

	for(int i=0; i<i_size_ml; i++) {s_mcn_temp[i] = s_mcn[i];} 

		

	std::sort(s_ocn_temp, s_ocn_temp+i_size_ol); //knowing that s_ocn_temp[] has i_size_ol entities

	std::sort(s_mcn_temp, s_mcn_temp+i_size_ml); //knowing that s_mcn_temp[] has i_size_ml entities

	

	//------------

	//memorize observed patterns 

	//------------

	double** uox = New_dMatrix(nrow, ncol);

	double** mox = New_dMatrix(nrow, ncol);

	

	int i_count_uox = 0; //total number of unique uox 

	std::string s_temp ; 

	for(int i=0; i<i_size_ol; i++)

	{

		s_temp = s_ocn_temp[i]; //get a string 

		for(int j=0; j<nrow; j++) //search all rows 

		{

			//----

			//below condition is needed for finding UNIQUE pattern

			//----

			//if(j==0 && s_temp == cn[j]) 

			//if(i==0 && s_temp == cn[j]) //with first string, find the same string in cn 

			if(i==0 && s_temp.compare(cn[j]) == 0) //0: equal string

			{

				for(int k=0; k<ncol; k++) 

				{uox[i_count_uox][k] = z[j][k]; } //store the found observed pattern

				i_count_uox++; 

				break; 

			}

			//if(j>0 && s_temp == cn[j] && s_temp != cn[j-1])

			//if(i>0 && s_temp == cn[j] && s_temp != s_ocn_temp[i-1]) //find UNIQUE matching 

			if(i>0 && s_temp.compare(cn[j]) == 0 && s_temp.compare(s_ocn_temp[i-1]) != 0) 

			{

				for(int k=0; k<ncol; k++) 

				{uox[i_count_uox][k] = z[j][k]; } //store the found observed pattern				

				i_count_uox++; 

				break; 

			}

		}

	}

	//Now, i_count_uox means the total number of unique observed patterns



	//------------

	//memorize missing patterns 

	//------------

	int i_count_mox = 0; //total number of unique mox 

	 

	for(int i=0; i<i_size_ml; i++)

	{

		s_temp = s_mcn_temp[i]; //get a string 

		for(int j=0; j<nrow; j++) //search all rows 

		{

			//----

			//below condition is needed for finding unique pattern

			//----

			//if(j==0 && s_temp == cn[j]) 

			//if(i==0 && s_temp == cn[j]) //with first string, find matching string in cn

			if(i==0 && s_temp.compare(cn[j]) == 0 ) //0: equal string 

			{

				for(int k=0; k<ncol; k++) 

				{mox[i_count_mox][k] = z[j][k]; } //store the found missing pattern

				i_count_mox++; 

				break; 

			}

			//if(j>0 && s_temp == cn[j] && s_temp != cn[j-1])

			//if(i>0 && s_temp == cn[j] && s_temp != s_mcn_temp[i-1]) //find UNIQUE matching string

			if(i>0 && s_temp.compare(cn[j]) == 0 && s_temp.compare(s_mcn_temp[i-1]) != 0) //0: equal

			{

				for(int k=0; k<ncol; k++) 

				{mox[i_count_mox][k] = z[j][k]; } //store the found missing pattern				

				i_count_mox++; 

				break;

			}

		}

	}

	//Now, i_count_mox means the total number of unique missing patterns

	

	//----------------

	//additional check for unique observed and missing patterns

	//----------------

	//observed patterns//

	d_temp = 0.0; 

	double** uox_final = New_dMatrix(nrow, ncol); 

	for(int j=0; j<ncol; j++) {uox_final[0][j] = uox[0][j]; } //first row initialization

	int i_count_uox_final = 1; //starting from the second row



	for(int i=1; i<i_count_uox; i++) //starting from the second one

	{

		d_temp = 0.0; //initialize 

		for(int j=0; j<ncol; j++) {d_temp += fabs_FHDI(uox[i][j] - uox[i-1][j]) ;} //difference of adjacent rows

		

		if(d_temp > 1e-3) //adjacent rows are NOT the same each other

		{

			for(int j=0; j<ncol; j++) {uox_final[i_count_uox_final][j] = uox[i][j];} 

			i_count_uox_final++; 

		}

	}

	i_count_uox = i_count_uox_final; //replace with the accurate value

	//store the final matrix 

	for(int i=0; i<i_count_uox; i++) 

	{ 

		for(int j =0; j<ncol; j++) 

			uox[i][j] = uox_final[i][j]; 

	}

	Del_dMatrix(uox_final, nrow, ncol);

	

	//--------------------------

	//missing patterns//

	//--------------------------

	double** mox_final = New_dMatrix(nrow, ncol); 

	for(int j=0; j<ncol; j++) {mox_final[0][j] = mox[0][j]; } //first row initialization

	int i_count_mox_final = 1; //starting from the second row



	for(int i=1; i<i_count_mox; i++) //starting from the second one

	{

		d_temp = 0.0; //initialize

		for(int j=0; j<ncol; j++) {d_temp += fabs_FHDI(mox[i][j] - mox[i-1][j]) ;} //difference of adjacent rows

		

		if(d_temp > 1e-3) //adjacent rows are NOT the same each other

		{

			for(int j=0; j<ncol; j++) {mox_final[i_count_mox_final][j] = mox[i][j];} 

			i_count_mox_final++; 

		}

	}

	i_count_mox = i_count_mox_final; //replace with the accurate value



	//store the final matrix	

	for(int i=0; i<i_count_mox; i++) 

	{ 

		for(int j =0; j<ncol; j++) 

			mox[i][j] = mox_final[i][j]; 

	}

	Del_dMatrix(mox_final, nrow, ncol);

	

    //!!!!! now uox and mox have the UNIQUE observed and missing patterns

	//!!!!! i_count_mox and _uox have the final number of meaningful rows of mox and uox, respectively

	

	//------------------

	//sort mcn and make a table

	//------------------

	for(int i =0; i<i_size_ml; i++) s_mcn_temp[i] = s_mcn[i]; 

	std::sort(s_mcn_temp, s_mcn_temp+i_size_ml);

	

	std::vector<std::string> v_table_tmvec_row1; //names of the table

	std::vector<int> 		 v_table_tmvec_row2; //counts of the table

	table_cpp(s_mcn_temp, i_size_ml, v_table_tmvec_row1, v_table_tmvec_row2);

	

	//-------------------

	//Augment observed cells for missing patterns

	//algorithm: 

	// for each missing pattern, find all the possible donors

	// e.g., 

	// (1) a missing row   = 000

	// 	   agmat           = all observed rows

	// (2) a missing row   = a01

	//     agmat           = ac1, af1, a11, ..., az1. 

	//-------------------

	rbind_FHDI agmat(ncol); //Note: without the first column of id

	bool b_success_AGMAT = AGMAT_Extension_cpp(mox, i_count_mox, 

						uox, i_count_uox, 

						ncol, id, 

						v_table_tmvec_row1,

						v_table_tmvec_row2,

                        cn, nrow, 

						agmat); 

	if(!b_success_AGMAT)
	{			
		Rprintf("Error! AGMAT Failed! Change k, check data quality, further break down categorical variables, or so. It may help \n");

		//deallocation before early return
		delete[] w_ml; 
		Del_dMatrix(d_ox, i_size_ol, ncol);
		Del_dMatrix(d_mx, i_size_ml, ncol);			
		delete[] cn; 
		delete[] cn0;
		delete[] s_ocn; 
		delete[] s_mcn; 
		delete[] s_ocn_temp; 
		delete[] s_mcn_temp; 
		Del_dMatrix(uox, nrow, ncol);	
		Del_dMatrix(mox, nrow, ncol);		

		return 0; //abnormal ending 								
	}
						
	const int n_row_agmat = agmat.size_row(); //get the number of rows 

	

	//---------

	//Translate 1. existing ox (rows with full observations) & 2. agmat

	//without appending the augmented rows onto the previous ox, i.e. the existing rows with observations

	//---------

	const int i_total_ox_agmat =  i_size_ol + n_row_agmat;

	double* d_fmat1 = new double[ncol]; //one row of observations

	std::string s_fcd1; //one row of translated string 

	//std::string s_fcd[i_total_ox_agmat]; //total rows of translated ox and augmat

	std::string *s_fcd = new std::string[i_total_ox_agmat]; //total rows of translated ox and augmat

	

	for(int i=0; i<i_total_ox_agmat; i++)

	{

		if(i<i_size_ol) //up to all ox rows 

		{

			for(int j=0; j<ncol; j++) d_fmat1[j] = d_ox[i][j];

			Trans1(d_fmat1, ncol, s_fcd1);

			s_fcd[i] = s_fcd1; 

		}

		if(i>=i_size_ol) // all augmat rows 

		{

			Fill_dVector(d_fmat1, ncol, 0.0); //re-initialize

			agmat.get_block( i-i_size_ol, d_fmat1); //row number of agmat = 0...n_row_agmat-1

			Trans1(d_fmat1, ncol, s_fcd1);

			s_fcd[i] = s_fcd1; 

		}		

	}

	

	//-------------

	//sampling weight of the observed unit

	//-------------

	double* w1 = new double[i_size_ol];

	for(int i=0; i<i_size_ol; i++) w1[i] = w[ol[i] - 1]; //-1 for actual location

	

	

	//----------------

	//calculate weighted joint probability

	//Note: Initial jp is calculated only

	//with all the OBSERVED condensed strings in s_ocn

	//NOT with Augmented data matix 

	//-----------------

	std::vector<std::string> jp_name; 

	std::vector<double>		 jp_prob;

	wpct_FHDI(s_ocn, i_size_ol, w1, jp_name, jp_prob); 

	const int i_size_jp_prob = (int)jp_prob.size(); 

               

	

	//===================================

	//===================================

	//Cal_W(): update new weights and the joint probability of cells

	//===================================

	//===================================

	std::vector<double> 		w20; //new storage for updated weights  

	std::vector<double>		 	jp_prob_0; //probability backup in the loop

	std::vector<std::string> 	jp_name_new; 

	std::vector<double>		 	jp_prob_new;	

	for(int j=0; j<i_size_jp_prob; j++) 

	{

		jp_name_new.push_back(jp_name[j]); //initialize with jp_prob

		jp_prob_new.push_back(jp_prob[j]); //initialize with jp_prob 

	}

	

	//MAIN ITERATION for Updating weights =======================

	for(int i_loop=0; i_loop<n_maximum_iteration; i_loop++)

	{

		//------------

		//intialize with the updated joint probability

		//------------

		jp_prob_0.clear(); //re-initialize 

		for(int j=0; j<i_size_jp_prob; j++) 

		{

			jp_prob_0.push_back(jp_prob_new[j]); //initialize with jp_prob_new 

		}

	



		//---------------------------------------

		//update weights, w20[] 

		//Note: prob must be the newest one! i.e. jp_prob_new

		//---------------------------------------

		w20.clear();  //re-initialize 

		bool b_success_Cal_W = Cal_W_Extension_cpp(mox, i_count_mox, 

						uox, i_count_uox, 

						ncol, id, 

						v_table_tmvec_row1,

						v_table_tmvec_row2,

						jp_prob_new,

						d_mx, i_size_ml, 

						w_ml, cn0, nrow, 

						w20);

		if(!b_success_Cal_W)
		{

			Rprintf("Error! Cal_W has failed! \n");
			
			//deallocation before early return
			delete[] w_ml; 
			Del_dMatrix(d_ox, i_size_ol, ncol);
			Del_dMatrix(d_mx, i_size_ml, ncol);			
			delete[] cn; 
			delete[] cn0;
			delete[] s_ocn; 
			delete[] s_mcn; 
			delete[] s_ocn_temp; 
			delete[] s_mcn_temp; 
			Del_dMatrix(uox, nrow, ncol);	
			Del_dMatrix(mox, nrow, ncol);		
			//up to here, memory allocated before AGMAT...			
			delete[] d_fmat1; 
			delete[] s_fcd;
			delete[] w1;
			
			return 0;
		}
		

		//-----------

		//combine new weights

		//-----------

		double* w12 = new double[i_total_ox_agmat]; 

		for(int j=0; j<i_total_ox_agmat; j++)

		{

			if(j<i_size_ol) //weights of existing w1

			{ w12[j] = w1[j];}

			if(j>=i_size_ol) //updated weights 

			{ w12[j] = w20[j-i_size_ol]; }

		}

	

		//-----------

		//new joint probability

		//Note: Unlike the initial jp, 

		//new augmented matrix along with the updated weight vector are used for the jp update 

		//-----------

		jp_name_new.clear(); //re-initialize

		jp_prob_new.clear(); //re-initialize

		wpct_FHDI(s_fcd, i_total_ox_agmat, w12, jp_name_new, jp_prob_new); 

		

		//------------

		//calculate difference in the joint probability

		//------------

		double dif = 0.0; 

		for(int j=0; j<i_size_jp_prob; j++) 

			dif += (jp_prob_0[j] - jp_prob_new[j])*(jp_prob_0[j] - jp_prob_new[j]);

		

		

		if(dif < 1e-6) 

		{

			//Rprintf(" Cell_Prob... finished after iterations : "<< i_loop+1);

			break; 

		}

	

		//------------

		//check max iterations

		//------------

		if(i_loop == n_maximum_iteration-1)

		{

			Rprintf("CAUTION!! max iteration reached in Cell_Prob. \n");
			
			//deallocation before early return
			delete[] w_ml; 
			Del_dMatrix(d_ox, i_size_ol, ncol);
			Del_dMatrix(d_mx, i_size_ml, ncol);			
			delete[] cn; 
			delete[] cn0;
			delete[] s_ocn; 
			delete[] s_mcn; 
			delete[] s_ocn_temp; 
			delete[] s_mcn_temp; 
			Del_dMatrix(uox, nrow, ncol);	
			Del_dMatrix(mox, nrow, ncol);		
			//up to here, memory allocated before AGMAT...			
			delete[] d_fmat1; 
			delete[] s_fcd;
			delete[] w1;
			//below is for local 
			delete[] w12; 
			
			return 0;
		}

		

		//------------

		//local deallocation

		//------------

		delete[] w12; 

	}

	

	

	//---------------------------

	//prep return 

	//the latest joint probability

	//---------------------------

	jp_prob_return.clear(); 

	jp_name_return.clear();

	for(int j=0; j<i_size_jp_prob; j++) 

	{

		jp_prob_return.push_back(jp_prob_new[j]); //return with jp_prob_new

		jp_name_return.push_back(jp_name_new[j]); //return with jp_name_new		

	}


	//Rprintf("========= Cell_Prob_Extension.. has successfully finished! \n");


	//-------------

	//deallocation

	//-------------

	delete[] w_ml; 

	Del_dMatrix(d_ox, i_size_ol, ncol);

	Del_dMatrix(d_mx, i_size_ml, ncol);
	
	delete[] cn; 

	delete[] cn0;

	delete[] s_ocn; 

	delete[] s_mcn; 

	delete[] s_ocn_temp; 

	delete[] s_mcn_temp; 

	Del_dMatrix(uox, nrow, ncol);	

	Del_dMatrix(mox, nrow, ncol);
	
	//up to here, memory allocated before AGMAT...
	
	delete[] d_fmat1; 

	delete[] s_fcd;

	delete[] w1;
	

	return 1;

	

}



} //end of namespace


  //Fn===========================================================================

  //Cell_Prob_Extension_Bigp_cpp.cc-----------------------------------------------------------------------------

  //Fn===========================================================================

namespace FHDI {


	bool Cell_Prob_Extension_Bigp_cpp(double** z, const int nrow, const int ncol, const int i_option_collapsing,
		std::vector<double> &jp_prob_return,
		std::vector<std::string> &jp_name_return,
		double* w, int* id, int** codes)

		//Description=========================================
		// make joint probability of cells with the categorized matrix z 
		// where 0 means missing data
		//
		//
		// original R code: Dr. Im, J. and Dr. Kim, J. 
		// c++ code: 		Dr. Cho, I. and Yicheng Yang
		// All rights reserved
		// 
		// updated: Feb 28, 2020
		//----------------------------------------------------
		//IN    : double z(nrow, ncol)  = catorized matrix corresponding to original matrix x
		//                                initialized with 0.0 
		//IN    : double w(nrow) = weights for rows (default = 1.0)
		//IN    : int    id(nrow) = row id (default = sequential row numbers)
		//IN    : int i_option_collapsing = choice of big-p algorithm 
		//                               0= no big-p algorithms
		//                              !0= perform big-p algorithms
		//IN    : int codes(nrow, i_option_collapsing); // storage to record most correlated variables of mox
		//OUT   : std::vector<double>      jp_prob_return  = final updated joint probability 
		//OUT   : std::vector<std::string> jp_name_return  = name of final updated joint probability 
		//====================================================
	{
		//testout
		//TestOut << "==========Begin Cell Prob...===========" << endl;

		//-------------
		//maximum number of iterations for updating weights
		//-------------
		const int n_maximum_iteration = nrow * 100; //set by user!


													//--------------
													//locations of missing cells (ml) and observed cells (ol)
													//Note: unlike in Cell_Make..(), std vector is used here
													//--------------
		std::vector<int> ol; //Actual row number 
		std::vector<int> ml; //Actual row number 

		double d_temp = 0.0;
		for (int i_row = 0; i_row<nrow; i_row++)
		{
			d_temp = 1.0;
			for (int i_col = 0; i_col<ncol; i_col++)
			{
				if (z[i_row][i_col] == 0) { d_temp = 0.0; break; } //found zero, i.e. missing cell
			}

			if (fabs(d_temp) > 1e-15) //this row has no missing cells
			{
				ol.push_back(i_row + 1);
			} //actual number of the row having no missing cells

			if (fabs(d_temp) < 1e-15) //this row has AT LEAST one missing cells
			{
				ml.push_back(i_row + 1);
			}  //actual number of the row having missing cells
		}
		const int i_size_ol = (int)ol.size();
		const int i_size_ml = (int)ml.size();

		if (i_size_ol == 0) { Rprintf("Error! no observed unit in Cell_Prob. \n"); return 0; }

		if (i_size_ml == 0) { Rprintf("Error! no missing  unit in Cell_Prob. \n"); return 0; }

		//----------------
		//weights corresponding to missing rows. Will be used for Cal_W..() later
		//select out weights at missing rows
		//----------------
		double* w_ml = new double[i_size_ml];
		for (int i = 0; i<i_size_ml; i++) w_ml[i] = w[ml[i] - 1]; //-1 for actual loc

																  //--------------
																  //Rows having only observed data (categorized) 
																  //Rows having AT LEAST one missing data (categorized)
																  //--------------
		double** d_ox = New_dMatrix(i_size_ol, ncol);
		double** d_mx = New_dMatrix(i_size_ml, ncol);
		for (int i = 0; i<i_size_ol; i++)
		{
			for (int j = 0; j<ncol; j++)
			{
				d_ox[i][j] = z[ol[i] - 1][j]; //-1 for Actual loc
			}
		}
		for (int i = 0; i<i_size_ml; i++)
		{
			for (int j = 0; j<ncol; j++)
			{
				d_mx[i][j] = z[ml[i] - 1][j];	//-1 for Actual loc
			}
		}

		//--------------
		//transform z into condensed string format
		//--------------
		//std::string cn[nrow]; //declaration of concatenated vector of z
		//std::string cn0[nrow]; //backup of cn
		std::string *cn = new std::string[nrow]; //declaration of concatenated vector of z
		std::string *cn0 = new std::string[nrow]; //backup of cn	
		Trans(z, nrow, ncol, cn);
		for (int i = 0; i<nrow; i++) cn0[i] = cn[i];

		//---------------
		//Rows of Condensed Strings with Observed cells
		//                          with AT LEAT One Missing  cell
		//---------------
		//std::string s_ocn[i_size_ol];
		//std::string s_mcn[i_size_ml];
		std::string *s_ocn = new std::string[i_size_ol];
		std::string *s_mcn = new std::string[i_size_ml];
		for (int i = 0; i<i_size_ol; i++) s_ocn[i] = cn[ol[i] - 1]; //-1 for actual row
		for (int i = 0; i<i_size_ml; i++) s_mcn[i] = cn[ml[i] - 1]; //-1 for actual row

																	//---------------------
																	//---------------------
																	//make UNIQUE patterns of z by cn
																	//i.e., uox and mox
																	//---------------------
																	//step. Sort the "cn"
																	//---------------------
																	//std::string s_ocn_temp[i_size_ol]; //string vector of observed patterns only
																	//std::string s_mcn_temp[i_size_ml]; //string vector of missing patterns only

		std::string *s_ocn_temp = new std::string[i_size_ol]; //string vector of observed patterns only
		std::string *s_mcn_temp = new std::string[i_size_ml]; //string vector of missing patterns only	
		for (int i = 0; i<i_size_ol; i++) { s_ocn_temp[i] = s_ocn[i]; }
		for (int i = 0; i<i_size_ml; i++) { s_mcn_temp[i] = s_mcn[i]; }

		std::sort(s_ocn_temp, s_ocn_temp + i_size_ol); //knowing that s_ocn_temp[] has i_size_ol entities
		std::sort(s_mcn_temp, s_mcn_temp + i_size_ml); //knowing that s_mcn_temp[] has i_size_ml entities

													   //------------
													   //memorize observed patterns 
													   //------------
		double** uox = New_dMatrix(nrow, ncol);
		double** mox = New_dMatrix(nrow, ncol);

		int i_count_uox = 0; //total number of unique uox 
		std::string s_temp;
		for (int i = 0; i<i_size_ol; i++)
		{
			s_temp = s_ocn_temp[i]; //get a string 
			for (int j = 0; j<nrow; j++) //search all rows 
			{
				//----
				//below condition is needed for finding UNIQUE pattern
				//----
				//if(j==0 && s_temp == cn[j]) 
				//if(i==0 && s_temp == cn[j]) //with first string, find the same string in cn 
				if (i == 0 && s_temp.compare(cn[j]) == 0) //0: equal string
				{
					for (int k = 0; k<ncol; k++)
					{
						uox[i_count_uox][k] = z[j][k];
					} //store the found observed pattern
					i_count_uox++;
					break;
				}
				//if(j>0 && s_temp == cn[j] && s_temp != cn[j-1])
				//if(i>0 && s_temp == cn[j] && s_temp != s_ocn_temp[i-1]) //find UNIQUE matching 
				if (i>0 && s_temp.compare(cn[j]) == 0 && s_temp.compare(s_ocn_temp[i - 1]) != 0)
				{
					for (int k = 0; k<ncol; k++)
					{
						uox[i_count_uox][k] = z[j][k];
					} //store the found observed pattern				
					i_count_uox++;
					break;
				}
			}
		}
		//Now, i_count_uox means the total number of unique observed patterns

		//------------
		//memorize missing patterns 
		//------------
		int i_count_mox = 0; //total number of unique mox 

		for (int i = 0; i<i_size_ml; i++)
		{
			s_temp = s_mcn_temp[i]; //get a string 
			for (int j = 0; j<nrow; j++) //search all rows 
			{
				//----
				//below condition is needed for finding unique pattern
				//----
				//if(j==0 && s_temp == cn[j]) 
				//if(i==0 && s_temp == cn[j]) //with first string, find matching string in cn
				if (i == 0 && s_temp.compare(cn[j]) == 0) //0: equal string 
				{
					for (int k = 0; k<ncol; k++)
					{
						mox[i_count_mox][k] = z[j][k];
					} //store the found missing pattern
					i_count_mox++;
					break;
				}
				//if(j>0 && s_temp == cn[j] && s_temp != cn[j-1])
				//if(i>0 && s_temp == cn[j] && s_temp != s_mcn_temp[i-1]) //find UNIQUE matching string
				if (i>0 && s_temp.compare(cn[j]) == 0 && s_temp.compare(s_mcn_temp[i - 1]) != 0) //0: equal
				{
					for (int k = 0; k<ncol; k++)
					{
						mox[i_count_mox][k] = z[j][k];
					} //store the found missing pattern				
					i_count_mox++;
					break;
				}
			}
		}
		//Now, i_count_mox means the total number of unique missing patterns

		//----------------
		//additional check for unique observed and missing patterns
		//----------------
		//observed patterns//
		d_temp = 0.0;
		double** uox_final = New_dMatrix(nrow, ncol);
		for (int j = 0; j<ncol; j++) { uox_final[0][j] = uox[0][j]; } //first row initialization
		int i_count_uox_final = 1; //starting from the second row

		for (int i = 1; i<i_count_uox; i++) //starting from the second one
		{
			d_temp = 0.0; //initialize 
			for (int j = 0; j<ncol; j++) { d_temp += fabs(uox[i][j] - uox[i - 1][j]); } //difference of adjacent rows

			if (d_temp > 1e-3) //adjacent rows are NOT the same each other
			{
				for (int j = 0; j<ncol; j++) { uox_final[i_count_uox_final][j] = uox[i][j]; }
				i_count_uox_final++;
			}
		}
		i_count_uox = i_count_uox_final; //replace with the accurate value
										 //store the final matrix 
		for (int i = 0; i<i_count_uox; i++)
		{
			for (int j = 0; j<ncol; j++)
				uox[i][j] = uox_final[i][j];
		}
		Del_dMatrix(uox_final, nrow, ncol);

		//--------------------------
		//missing patterns//
		//--------------------------
		double** mox_final = New_dMatrix(nrow, ncol);
		for (int j = 0; j<ncol; j++) { mox_final[0][j] = mox[0][j]; } //first row initialization
		int i_count_mox_final = 1; //starting from the second row

		for (int i = 1; i<i_count_mox; i++) //starting from the second one
		{
			d_temp = 0.0; //initialize
			for (int j = 0; j<ncol; j++) { d_temp += fabs(mox[i][j] - mox[i - 1][j]); } //difference of adjacent rows

			if (d_temp > 1e-3) //adjacent rows are NOT the same each other
			{
				for (int j = 0; j<ncol; j++) { mox_final[i_count_mox_final][j] = mox[i][j]; }
				i_count_mox_final++;
			}
		}
		i_count_mox = i_count_mox_final; //replace with the accurate value

										 //store the final matrix	
		for (int i = 0; i<i_count_mox; i++)
		{
			for (int j = 0; j<ncol; j++)
				mox[i][j] = mox_final[i][j];
		}
		Del_dMatrix(mox_final, nrow, ncol);

		//!!!!! now uox and mox have the UNIQUE observed and missing patterns
		//!!!!! i_count_mox and _uox have the final number of meaningful rows of mox and uox, respectively

		//------------------
		//sort mcn and make a table
		//------------------
		for (int i = 0; i<i_size_ml; i++) s_mcn_temp[i] = s_mcn[i];
		std::sort(s_mcn_temp, s_mcn_temp + i_size_ml);

		std::vector<std::string> v_table_tmvec_row1; //names of the table
		std::vector<int> 		 v_table_tmvec_row2; //counts of the table
		table_cpp(s_mcn_temp, i_size_ml, v_table_tmvec_row1, v_table_tmvec_row2);


		//testout
		//RPrint("============ Cell_Prob before AGMAT==============", TestOut);
		/*
		RPrint("z :"); RPrint(z, nrow, ncol);
		RPrint("ol:"); RPrint(ol);
		RPrint("ml:"); RPrint(ml);
		RPrint("ox:"); RPrint(d_ox, i_size_ol, ncol);
		RPrint("mx:"); RPrint(d_mx, i_size_ml, ncol);
		RPrint("ocn:"); RPrint(s_ocn, i_size_ol);
		RPrint("mcn:"); RPrint(s_mcn, i_size_ml);
		RPrint("uox:"); RPrint(uox, i_count_uox, ncol);
		RPrint("mox:"); RPrint(mox, i_count_mox, ncol);
		RPrint("Table of mcn. counts: "); RPrint(v_table_tmvec_row2);
		*/

		//-------------------
		//Augment observed cells for missing patterns
		//algorithm: 
		// for each missing pattern, find all the possible donors
		// e.g., 
		// (1) a missing row   = 000
		// 	   agmat           = all observed rows
		// (2) a missing row   = a01
		//     agmat           = ac1, af1, a11, ..., az1. 
		//-------------------
		rbind_FHDI agmat(ncol); //Note: without the first column of id


		bool b_success_AGMAT = AGMAT_Extension_Bigp_cpp(mox, i_count_mox,
			uox, i_count_uox,
			ncol, id,
			v_table_tmvec_row1,
			v_table_tmvec_row2,
			cn, nrow, i_option_collapsing, codes,
			agmat);

		if (!b_success_AGMAT)
		{
			Rprintf("Error! AGMAT Failed! Change k, check data quality, further break down categorical variables, or so. It may help \n");

			//deallocation before early return
			delete[] w_ml;
			Del_dMatrix(d_ox, i_size_ol, ncol);
			Del_dMatrix(d_mx, i_size_ml, ncol);
			delete[] cn;
			delete[] cn0;
			delete[] s_ocn;
			delete[] s_mcn;
			delete[] s_ocn_temp;
			delete[] s_mcn_temp;
			Del_dMatrix(uox, nrow, ncol);
			Del_dMatrix(mox, nrow, ncol);

			return 0; //abnormal ending 								
		}

		const int n_row_agmat = agmat.size_row(); //get the number of rows 

												  //---------
												  //Translate 1. existing ox (rows with full observations) & 2. agmat
												  //without appending the augmented rows onto the previous ox, i.e. the existing rows with observations
												  //---------
		const int i_total_ox_agmat = i_size_ol + n_row_agmat;
		double* d_fmat1 = new double[ncol]; //one row of observations
		std::string s_fcd1; //one row of translated string 
							//std::string s_fcd[i_total_ox_agmat]; //total rows of translated ox and augmat
		std::string *s_fcd = new std::string[i_total_ox_agmat]; //total rows of translated ox and augmat

		for (int i = 0; i<i_total_ox_agmat; i++)
		{
			if (i<i_size_ol) //up to all ox rows 
			{
				for (int j = 0; j<ncol; j++) d_fmat1[j] = d_ox[i][j];
				Trans1(d_fmat1, ncol, s_fcd1);
				s_fcd[i] = s_fcd1;
			}
			if (i >= i_size_ol) // all augmat rows 
			{
				Fill_dVector(d_fmat1, ncol, 0.0); //re-initialize
				agmat.get_block(i - i_size_ol, d_fmat1); //row number of agmat = 0...n_row_agmat-1
				Trans1(d_fmat1, ncol, s_fcd1);
				s_fcd[i] = s_fcd1;
			}
		}

		//-------------
		//sampling weight of the observed unit
		//-------------
		double* w1 = new double[i_size_ol];
		for (int i = 0; i<i_size_ol; i++) w1[i] = w[ol[i] - 1]; //-1 for actual location

																//testout
																/*
																RPrint("After AUGMAT ==========");
																RPrint("agmat:"); agmat.print_rbind_FHDI();
																RPrint("s_fcd:"); RPrint(s_fcd, i_total_ox_agmat);
																RPrint("w1:"); RPrint(w1, i_size_ol);
																*/

																//----------------
																//calculate weighted joint probability
																//Note: Initial jp is calculated only
																//with all the OBSERVED condensed strings in s_ocn
																//NOT with Augmented data matix 
																//-----------------
		std::vector<std::string> jp_name;
		std::vector<double>		 jp_prob;
		wpct_FHDI(s_ocn, i_size_ol, w1, jp_name, jp_prob);
		const int i_size_jp_prob = (int)jp_prob.size();

		//testout
		//RPrint("After wpct initial ==========", TestOut);
		/*
		RPrint("After wpct initial ==========");
		std::string s_temp_jp[i_size_jp_prob];
		for(int i=0; i<i_size_jp_prob; i++) s_temp_jp[i] = jp_name[i];
		RPrint("jp_name:"); RPrint(s_temp_jp, i_size_jp_prob);
		RPrint("jp_prob:"); RPrint(jp_prob);
		*/

		//===================================
		//===================================
		//Cal_W(): update new weights and the joint probability of cells
		//===================================
		//===================================
		std::vector<double> 		w20; //new storage for updated weights  
		std::vector<double>		 	jp_prob_0; //probability backup in the loop
		std::vector<std::string> 	jp_name_new;
		std::vector<double>		 	jp_prob_new;
		for (int j = 0; j<i_size_jp_prob; j++)
		{
			jp_name_new.push_back(jp_name[j]); //initialize with jp_prob
			jp_prob_new.push_back(jp_prob[j]); //initialize with jp_prob 
		}

		//MAIN ITERATION for Updating weights =======================
		for (int i_loop = 0; i_loop<n_maximum_iteration; i_loop++)
		{
			//------------
			//intialize with the updated joint probability
			//------------
			jp_prob_0.clear(); //re-initialize 

			for (int j = 0; j<i_size_jp_prob; j++)
			{
				jp_prob_0.push_back(jp_prob_new[j]); //initialize with jp_prob_new 
			}


			//---------------------------------------
			//update weights, w20[] 
			//Note: prob must be the newest one! i.e. jp_prob_new
			//---------------------------------------
			w20.clear();  //re-initialize 


			bool b_success_Cal_W = Cal_W_Extension_Bigp_cpp(mox, i_count_mox,
				uox, i_count_uox, i_option_collapsing,
				ncol, id, codes,
				v_table_tmvec_row1,
				v_table_tmvec_row2,
				jp_prob_new,
				d_mx, i_size_ml,
				w_ml, cn0, nrow,
				w20);


			if (!b_success_Cal_W)
			{

				Rprintf("Error! Cal_W has failed! \n");

				//deallocation before early return
				delete[] w_ml;
				Del_dMatrix(d_ox, i_size_ol, ncol);
				Del_dMatrix(d_mx, i_size_ml, ncol);
				delete[] cn;
				delete[] cn0;
				delete[] s_ocn;
				delete[] s_mcn;
				delete[] s_ocn_temp;
				delete[] s_mcn_temp;
				Del_dMatrix(uox, nrow, ncol);
				Del_dMatrix(mox, nrow, ncol);
				//up to here, memory allocated before AGMAT...			
				delete[] d_fmat1;
				delete[] s_fcd;
				delete[] w1;

				return 0;
			}


			//-----------
			//combine new weights
			//-----------
			double* w12 = new double[i_total_ox_agmat];
			for (int j = 0; j<i_total_ox_agmat; j++)
			{
				if (j<i_size_ol) //weights of existing w1
				{
					w12[j] = w1[j];
				}
				if (j >= i_size_ol) //updated weights 
				{
					w12[j] = w20[j - i_size_ol];
				}
			}

			//-----------
			//new joint probability
			//Note: Unlike the initial jp, 
			//new augmented matrix along with the updated weight vector are used for the jp update 
			//-----------
			jp_name_new.clear(); //re-initialize
			jp_prob_new.clear(); //re-initialize
			wpct_FHDI(s_fcd, i_total_ox_agmat, w12, jp_name_new, jp_prob_new);

			//------------
			//calculate difference in the joint probability
			//------------
			double dif = 0.0;
			for (int j = 0; j<i_size_jp_prob; j++)
				dif += (jp_prob_0[j] - jp_prob_new[j])*(jp_prob_0[j] - jp_prob_new[j]);

			//testout
			//RPrint(" in Cal_W ----------- i_loop: "); RPrint(i_loop);
			//RPrint(" dif: "); RPrint(dif);
			//RPrint(" jp_prob_0: "); RPrint(jp_prob_0);
			//RPrint(" jp_prob_new: "); RPrint(jp_prob_new);

			if (dif < 1e-6)
			{
				//TestOut << " Cell_Prob... finished after iterations : " << i_loop + 1 << endl;
				break;
			}

			//------------
			//check max iterations
			//------------
			if (i_loop == n_maximum_iteration - 1)

			{

				Rprintf("CAUTION!! max iteration reached in Cell_Prob. \n");

				//deallocation before early return
				delete[] w_ml;
				Del_dMatrix(d_ox, i_size_ol, ncol);
				Del_dMatrix(d_mx, i_size_ml, ncol);
				delete[] cn;
				delete[] cn0;
				delete[] s_ocn;
				delete[] s_mcn;
				delete[] s_ocn_temp;
				delete[] s_mcn_temp;
				Del_dMatrix(uox, nrow, ncol);
				Del_dMatrix(mox, nrow, ncol);
				//up to here, memory allocated before AGMAT...			
				delete[] d_fmat1;
				delete[] s_fcd;
				delete[] w1;
				//below is for local 
				delete[] w12;

				return 0;
			}

			//------------
			//local deallocation
			//------------
			delete[] w12;
		}


		//---------------------------
		//prep return 
		//the latest joint probability
		//---------------------------
		jp_prob_return.clear();
		jp_name_return.clear();
		for (int j = 0; j<i_size_jp_prob; j++)
		{
			jp_prob_return.push_back(jp_prob_new[j]); //return with jp_prob_new
			jp_name_return.push_back(jp_name_new[j]); //return with jp_name_new		
		}

		//testout
		//RPrint(" ========= Cell_Prob_Extension.. has successfully finished!", TestOut);
		//Rprintf("========= Cell_Prob_Extension_Bigp.. has successfully finished! \n");

		//-------------
		//deallocation
		//-------------
		//delete[] w;
		delete[] w_ml;
		delete[] w1;
		//delete[] id; 
		delete[] d_fmat1;
		Del_dMatrix(d_ox, i_size_ol, ncol);
		Del_dMatrix(d_mx, i_size_ml, ncol);
		Del_dMatrix(mox, nrow, ncol);
		Del_dMatrix(uox, nrow, ncol);

		delete[] s_ocn_temp;

		delete[] s_mcn_temp;

		delete[] cn;
		delete[] cn0;
		delete[] s_ocn;
		delete[] s_mcn;
		delete[] s_fcd;

		return 1;

	}

} //end of namespace


  //Fn===========================================================================

  //Cell_Prob_Neighbor_cpp.cc-----------------------------------------------------------------------------

  //Fn===========================================================================

namespace FHDI {


	bool Cell_Prob_Neighbor_cpp(double** z, const int nrow, const int ncol, List_FHDI &List_nU,
		std::vector<double> &jp_prob_return,
		std::vector<std::string> &jp_name_return,
		double* w, int* id)

		//Description=========================================
		// make joint probability of cells with the categorized matrix z 
		// where 0 means missing data
		//
		//
		// original R code: Dr. Im, J. and Dr. Kim, J. 
		// c++ code: 		Dr. Cho, I. and Yicheng Yang
		// All rights reserved
		// 
		// updated: Aug 11, 2020
		//----------------------------------------------------
		//IN    : double z(nrow, ncol)  = catorized matrix corresponding to original matrix x
		//                                initialized with 0.0 
		//IN    : double w(nrow) = weights for rows (default = 1.0)
		//IN    : int    id(nrow) = row id (default = sequential row numbers)
		//
		//OUT   : std::vector<double>      jp_prob_return  = final updated joint probability 
		//OUT   : std::vector<std::string> jp_name_return  = name of final updated joint probability 
		//====================================================
	{
		//testout

		//-------------
		//maximum number of iterations for updating weights
		//-------------
		const int n_maximum_iteration = nrow * 100; //set by user!


													//--------------
													//locations of missing cells (ml) and observed cells (ol)
													//Note: unlike in Cell_Make..(), std vector is used here
													//--------------
		std::vector<int> ol; //Actual row number 
		std::vector<int> ml; //Actual row number 

		double d_temp = 0.0;
		for (int i_row = 0; i_row<nrow; i_row++)
		{
			d_temp = 1.0;
			for (int i_col = 0; i_col<ncol; i_col++)
			{
				if (z[i_row][i_col] == 0) { d_temp = 0.0; break; } //found zero, i.e. missing cell
			}

			if (fabs(d_temp) > 1e-15) //this row has no missing cells
			{
				ol.push_back(i_row + 1);
			} //actual number of the row having no missing cells

			if (fabs(d_temp) < 1e-15) //this row has AT LEAST one missing cells
			{
				ml.push_back(i_row + 1);
			}  //actual number of the row having missing cells
		}
		const int i_size_ol = (int)ol.size();
		const int i_size_ml = (int)ml.size();

		if (i_size_ol == 0) { Rprintf("Error! no observed unit in Cell_Prob. \n"); return 0; }

		if (i_size_ml == 0) { Rprintf("Error! no missing  unit in Cell_Prob. \n"); return 0; }

		//----------------
		//weights corresponding to missing rows. Will be used for Cal_W..() later
		//select out weights at missing rows
		//----------------
		double* w_ml = new double[i_size_ml];
		for (int i = 0; i<i_size_ml; i++) w_ml[i] = w[ml[i] - 1]; //-1 for actual loc

																  //--------------
																  //Rows having only observed data (categorized) 
																  //Rows having AT LEAST one missing data (categorized)
																  //--------------
		double** d_ox = New_dMatrix(i_size_ol, ncol);
		double** d_mx = New_dMatrix(i_size_ml, ncol);
		for (int i = 0; i<i_size_ol; i++)
		{
			for (int j = 0; j<ncol; j++)
			{
				d_ox[i][j] = z[ol[i] - 1][j]; //-1 for Actual loc
			}
		}
		for (int i = 0; i<i_size_ml; i++)
		{
			for (int j = 0; j<ncol; j++)
			{
				d_mx[i][j] = z[ml[i] - 1][j];	//-1 for Actual loc
			}
		}

		//--------------
		//transform z into condensed string format
		//--------------
		//std::string cn[nrow]; //declaration of concatenated vector of z
		//std::string cn0[nrow]; //backup of cn
		std::string *cn = new std::string[nrow]; //declaration of concatenated vector of z
		std::string *cn0 = new std::string[nrow]; //backup of cn	
		Trans(z, nrow, ncol, cn);
		for (int i = 0; i<nrow; i++) cn0[i] = cn[i];

		//---------------
		//Rows of Condensed Strings with Observed cells
		//                          with AT LEAT One Missing  cell
		//---------------
		//std::string s_ocn[i_size_ol];
		//std::string s_mcn[i_size_ml];
		std::string *s_ocn = new std::string[i_size_ol];
		std::string *s_mcn = new std::string[i_size_ml];
		for (int i = 0; i<i_size_ol; i++) s_ocn[i] = cn[ol[i] - 1]; //-1 for actual row
		for (int i = 0; i<i_size_ml; i++) s_mcn[i] = cn[ml[i] - 1]; //-1 for actual row

																	//---------------------
																	//---------------------
																	//make UNIQUE patterns of z by cn
																	//i.e., uox and mox
																	//---------------------
																	//step. Sort the "cn"
																	//---------------------
																	//std::string s_ocn_temp[i_size_ol]; //string vector of observed patterns only
																	//std::string s_mcn_temp[i_size_ml]; //string vector of missing patterns only
		std::string *s_ocn_temp = new std::string[i_size_ol]; //string vector of observed patterns only
		std::string *s_mcn_temp = new std::string[i_size_ml]; //string vector of missing patterns only	
		for (int i = 0; i<i_size_ol; i++) { s_ocn_temp[i] = s_ocn[i]; }
		for (int i = 0; i<i_size_ml; i++) { s_mcn_temp[i] = s_mcn[i]; }

		std::sort(s_ocn_temp, s_ocn_temp + i_size_ol); //knowing that s_ocn_temp[] has i_size_ol entities
		std::sort(s_mcn_temp, s_mcn_temp + i_size_ml); //knowing that s_mcn_temp[] has i_size_ml entities

													   //------------
													   //memorize observed patterns 
													   //------------
		double** uox = New_dMatrix(nrow, ncol);
		double** mox = New_dMatrix(nrow, ncol);

		int i_count_uox = 0; //total number of unique uox 
		std::string s_temp;
		for (int i = 0; i<i_size_ol; i++)
		{
			s_temp = s_ocn_temp[i]; //get a string 
			for (int j = 0; j<nrow; j++) //search all rows 
			{
				//----
				//below condition is needed for finding UNIQUE pattern
				//----
				//if(j==0 && s_temp == cn[j]) 
				//if(i==0 && s_temp == cn[j]) //with first string, find the same string in cn 
				if (i == 0 && s_temp.compare(cn[j]) == 0) //0: equal string
				{
					for (int k = 0; k<ncol; k++)
					{
						uox[i_count_uox][k] = z[j][k];
					} //store the found observed pattern
					i_count_uox++;
					break;
				}
				//if(j>0 && s_temp == cn[j] && s_temp != cn[j-1])
				//if(i>0 && s_temp == cn[j] && s_temp != s_ocn_temp[i-1]) //find UNIQUE matching 
				if (i>0 && s_temp.compare(cn[j]) == 0 && s_temp.compare(s_ocn_temp[i - 1]) != 0)
				{
					for (int k = 0; k<ncol; k++)
					{
						uox[i_count_uox][k] = z[j][k];
					} //store the found observed pattern				
					i_count_uox++;
					break;
				}
			}
		}
		//Now, i_count_uox means the total number of unique observed patterns

		//------------
		//memorize missing patterns 
		//------------
		int i_count_mox = 0; //total number of unique mox 

		for (int i = 0; i<i_size_ml; i++)
		{
			s_temp = s_mcn_temp[i]; //get a string 
			for (int j = 0; j<nrow; j++) //search all rows 
			{
				//----
				//below condition is needed for finding unique pattern
				//----
				//if(j==0 && s_temp == cn[j]) 
				//if(i==0 && s_temp == cn[j]) //with first string, find matching string in cn
				if (i == 0 && s_temp.compare(cn[j]) == 0) //0: equal string 
				{
					for (int k = 0; k<ncol; k++)
					{
						mox[i_count_mox][k] = z[j][k];
					} //store the found missing pattern
					i_count_mox++;
					break;
				}
				//if(j>0 && s_temp == cn[j] && s_temp != cn[j-1])
				//if(i>0 && s_temp == cn[j] && s_temp != s_mcn_temp[i-1]) //find UNIQUE matching string
				if (i>0 && s_temp.compare(cn[j]) == 0 && s_temp.compare(s_mcn_temp[i - 1]) != 0) //0: equal
				{
					for (int k = 0; k<ncol; k++)
					{
						mox[i_count_mox][k] = z[j][k];
					} //store the found missing pattern				
					i_count_mox++;
					break;
				}
			}
		}
		//Now, i_count_mox means the total number of unique missing patterns

		//----------------
		//additional check for unique observed and missing patterns
		//----------------
		//observed patterns//
		d_temp = 0.0;
		double** uox_final = New_dMatrix(nrow, ncol);
		for (int j = 0; j<ncol; j++) { uox_final[0][j] = uox[0][j]; } //first row initialization
		int i_count_uox_final = 1; //starting from the second row

		for (int i = 1; i<i_count_uox; i++) //starting from the second one
		{
			d_temp = 0.0; //initialize 
			for (int j = 0; j<ncol; j++) { d_temp += fabs(uox[i][j] - uox[i - 1][j]); } //difference of adjacent rows

			if (d_temp > 1e-3) //adjacent rows are NOT the same each other
			{
				for (int j = 0; j<ncol; j++) { uox_final[i_count_uox_final][j] = uox[i][j]; }
				i_count_uox_final++;
			}
		}
		i_count_uox = i_count_uox_final; //replace with the accurate value
										 //store the final matrix 
		for (int i = 0; i<i_count_uox; i++)
		{
			for (int j = 0; j<ncol; j++)
				uox[i][j] = uox_final[i][j];
		}
		Del_dMatrix(uox_final, nrow, ncol);

		//--------------------------
		//missing patterns//
		//--------------------------
		double** mox_final = New_dMatrix(nrow, ncol);
		for (int j = 0; j<ncol; j++) { mox_final[0][j] = mox[0][j]; } //first row initialization
		int i_count_mox_final = 1; //starting from the second row

		for (int i = 1; i<i_count_mox; i++) //starting from the second one
		{
			d_temp = 0.0; //initialize
			for (int j = 0; j<ncol; j++) { d_temp += fabs(mox[i][j] - mox[i - 1][j]); } //difference of adjacent rows

			if (d_temp > 1e-3) //adjacent rows are NOT the same each other
			{
				for (int j = 0; j<ncol; j++) { mox_final[i_count_mox_final][j] = mox[i][j]; }
				i_count_mox_final++;
			}
		}
		i_count_mox = i_count_mox_final; //replace with the accurate value

										 //store the final matrix	
		for (int i = 0; i<i_count_mox; i++)
		{
			for (int j = 0; j<ncol; j++)
				mox[i][j] = mox_final[i][j];
		}
		Del_dMatrix(mox_final, nrow, ncol);

		//!!!!! now uox and mox have the UNIQUE observed and missing patterns
		//!!!!! i_count_mox and _uox have the final number of meaningful rows of mox and uox, respectively

		//------------------
		//sort mcn and make a table
		//------------------
		for (int i = 0; i<i_size_ml; i++) s_mcn_temp[i] = s_mcn[i];
		std::sort(s_mcn_temp, s_mcn_temp + i_size_ml);

		std::vector<std::string> v_table_tmvec_row1; //names of the table
		std::vector<int> 		 v_table_tmvec_row2; //counts of the table
		table_cpp(s_mcn_temp, i_size_ml, v_table_tmvec_row1, v_table_tmvec_row2);

		//testout
		//RPrint("============ Cell_Prob before AGMAT==============", TestOut);
		/*
		RPrint("z :"); RPrint(z, nrow, ncol);
		RPrint("ol:"); RPrint(ol);
		RPrint("ml:"); RPrint(ml);
		RPrint("ox:"); RPrint(d_ox, i_size_ol, ncol);
		RPrint("mx:"); RPrint(d_mx, i_size_ml, ncol);
		RPrint("ocn:"); RPrint(s_ocn, i_size_ol);
		RPrint("mcn:"); RPrint(s_mcn, i_size_ml);
		RPrint("uox:"); RPrint(uox, i_count_uox, ncol);
		RPrint("mox:"); RPrint(mox, i_count_mox, ncol);
		RPrint("Table of mcn. counts: "); RPrint(v_table_tmvec_row2);
		*/

		//-------------------
		//Augment observed cells for missing patterns
		//algorithm: 
		// for each missing pattern, find all the possible donors
		// e.g., 
		// (1) a missing row   = 000
		// 	   agmat           = all observed rows
		// (2) a missing row   = a01
		//     agmat           = ac1, af1, a11, ..., az1. 
		//-------------------
		rbind_FHDI agmat(ncol); //Note: without the first column of id
		bool b_success_AGMAT_neighbor = AGMAT_Neighbor_cpp(mox, i_count_mox,
			uox, i_count_uox,
			ncol, id,
			v_table_tmvec_row1,
			v_table_tmvec_row2,
			cn, nrow, List_nU,
			agmat);

		if (!b_success_AGMAT_neighbor)
		{
			Rprintf("Error! AGMAT KNN Failed! Change k, check data quality, further break down categorical variables, or so. It may help \n");

			//deallocation before early return
			delete[] w_ml;
			Del_dMatrix(d_ox, i_size_ol, ncol);
			Del_dMatrix(d_mx, i_size_ml, ncol);
			delete[] cn;
			delete[] cn0;
			delete[] s_ocn;
			delete[] s_mcn;
			delete[] s_ocn_temp;
			delete[] s_mcn_temp;
			Del_dMatrix(uox, nrow, ncol);
			Del_dMatrix(mox, nrow, ncol);

			return 0; //abnormal ending 								
		}

		const int n_row_agmat = agmat.size_row(); //get the number of rows 

												  //---------
												  //Translate 1. existing ox (rows with full observations) & 2. agmat
												  //without appending the augmented rows onto the previous ox, i.e. the existing rows with observations
												  //---------
		const int i_total_ox_agmat = i_size_ol + n_row_agmat;
		double* d_fmat1 = new double[ncol]; //one row of observations
		std::string s_fcd1; //one row of translated string 
							//std::string s_fcd[i_total_ox_agmat]; //total rows of translated ox and augmat
		std::string *s_fcd = new std::string[i_total_ox_agmat]; //total rows of translated ox and augmat

		for (int i = 0; i<i_total_ox_agmat; i++)
		{
			if (i<i_size_ol) //up to all ox rows 
			{
				for (int j = 0; j<ncol; j++) d_fmat1[j] = d_ox[i][j];
				Trans1(d_fmat1, ncol, s_fcd1);
				s_fcd[i] = s_fcd1;
			}
			if (i >= i_size_ol) // all augmat rows 
			{
				Fill_dVector(d_fmat1, ncol, 0.0); //re-initialize
				agmat.get_block(i - i_size_ol, d_fmat1); //row number of agmat = 0...n_row_agmat-1
				Trans1(d_fmat1, ncol, s_fcd1);
				s_fcd[i] = s_fcd1;
			}
		}

		//-------------
		//sampling weight of the observed unit
		//-------------
		double* w1 = new double[i_size_ol];
		for (int i = 0; i<i_size_ol; i++) w1[i] = w[ol[i] - 1]; //-1 for actual location

																//testout
																/*
																RPrint("After AUGMAT ==========");
																RPrint("agmat:"); agmat.print_rbind_FHDI();
																RPrint("s_fcd:"); RPrint(s_fcd, i_total_ox_agmat);
																RPrint("w1:"); RPrint(w1, i_size_ol);
																*/

																//----------------
																//calculate weighted joint probability
																//Note: Initial jp is calculated only
																//with all the OBSERVED condensed strings in s_ocn
																//NOT with Augmented data matix 
																//-----------------
		std::vector<std::string> jp_name;
		std::vector<double>		 jp_prob;
		wpct_FHDI(s_ocn, i_size_ol, w1, jp_name, jp_prob);
		const int i_size_jp_prob = (int)jp_prob.size();

		//testout
		//RPrint("After wpct initial ==========", TestOut);
		/*
		RPrint("After wpct initial ==========");
		std::string s_temp_jp[i_size_jp_prob];
		for(int i=0; i<i_size_jp_prob; i++) s_temp_jp[i] = jp_name[i];
		RPrint("jp_name:"); RPrint(s_temp_jp, i_size_jp_prob);
		RPrint("jp_prob:"); RPrint(jp_prob);
		*/

		//===================================
		//===================================
		//Cal_W(): update new weights and the joint probability of cells
		//===================================
		//===================================
		std::vector<double> 		w20; //new storage for updated weights  
		std::vector<double>		 	jp_prob_0; //probability backup in the loop
		std::vector<std::string> 	jp_name_new;
		std::vector<double>		 	jp_prob_new;
		for (int j = 0; j<i_size_jp_prob; j++)
		{
			jp_name_new.push_back(jp_name[j]); //initialize with jp_prob
			jp_prob_new.push_back(jp_prob[j]); //initialize with jp_prob 
		}

		//MAIN ITERATION for Updating weights =======================
		for (int i_loop = 0; i_loop<n_maximum_iteration; i_loop++)
		{
			//------------
			//intialize with the updated joint probability
			//------------
			jp_prob_0.clear(); //re-initialize 
			for (int j = 0; j<i_size_jp_prob; j++)
			{
				jp_prob_0.push_back(jp_prob_new[j]); //initialize with jp_prob_new 
			}


			//---------------------------------------
			//update weights, w20[] 
			//Note: prob must be the newest one! i.e. jp_prob_new
			//---------------------------------------
			w20.clear();  //re-initialize 
			bool b_success_Cal_W_neighbor = Cal_W_Neighbor_cpp(mox, i_count_mox,
				uox, i_count_uox,
				ncol, id, List_nU,
				v_table_tmvec_row1,
				v_table_tmvec_row2,
				jp_prob_new,
				d_mx, i_size_ml,
				w_ml, cn0, nrow,
				w20);


			if (!b_success_Cal_W_neighbor)
			{

				Rprintf("Error! Cal_W KNN has failed! \n");

				//deallocation before early return
				delete[] w_ml;
				Del_dMatrix(d_ox, i_size_ol, ncol);
				Del_dMatrix(d_mx, i_size_ml, ncol);
				delete[] cn;
				delete[] cn0;
				delete[] s_ocn;
				delete[] s_mcn;
				delete[] s_ocn_temp;
				delete[] s_mcn_temp;
				Del_dMatrix(uox, nrow, ncol);
				Del_dMatrix(mox, nrow, ncol);
				//up to here, memory allocated before AGMAT...			
				delete[] d_fmat1;
				delete[] s_fcd;
				delete[] w1;

				return 0;
			}


			//-----------
			//combine new weights
			//-----------
			double* w12 = new double[i_total_ox_agmat];
			for (int j = 0; j<i_total_ox_agmat; j++)
			{
				if (j<i_size_ol) //weights of existing w1
				{
					w12[j] = w1[j];
				}
				if (j >= i_size_ol) //updated weights 
				{
					w12[j] = w20[j - i_size_ol];
				}
			}

			//-----------
			//new joint probability
			//Note: Unlike the initial jp, 
			//new augmented matrix along with the updated weight vector are used for the jp update 
			//-----------
			jp_name_new.clear(); //re-initialize
			jp_prob_new.clear(); //re-initialize
			wpct_FHDI(s_fcd, i_total_ox_agmat, w12, jp_name_new, jp_prob_new);

			//------------
			//calculate difference in the joint probability
			//------------
			double dif = 0.0;
			for (int j = 0; j<i_size_jp_prob; j++)
				dif += (jp_prob_0[j] - jp_prob_new[j])*(jp_prob_0[j] - jp_prob_new[j]);

			//testout
			//RPrint(" in Cal_W ----------- i_loop: "); RPrint(i_loop);
			//RPrint(" dif: "); RPrint(dif);
			//RPrint(" jp_prob_0: "); RPrint(jp_prob_0);
			//RPrint(" jp_prob_new: "); RPrint(jp_prob_new);

			if (dif < 1e-6)
			{
				//TestOut<<" Cell_Prob with KNN... finished after iterations : "<< i_loop+1<<endl;
				break;
			}

			//------------
			//check max iterations
			//------------
			if (i_loop == n_maximum_iteration - 1)
			{
				Rprintf("CAUTION!! max iteration reached in Cell_Prob KNN. \n");

				//deallocation before early return
				delete[] w_ml;
				Del_dMatrix(d_ox, i_size_ol, ncol);
				Del_dMatrix(d_mx, i_size_ml, ncol);
				delete[] cn;
				delete[] cn0;
				delete[] s_ocn;
				delete[] s_mcn;
				delete[] s_ocn_temp;
				delete[] s_mcn_temp;
				Del_dMatrix(uox, nrow, ncol);
				Del_dMatrix(mox, nrow, ncol);
				//up to here, memory allocated before AGMAT...			
				delete[] d_fmat1;
				delete[] s_fcd;
				delete[] w1;
				//below is for local 
				delete[] w12;

				return 0;
			}

			//------------
			//local deallocation
			//------------
			delete[] w12;
		}


		//---------------------------
		//prep return 
		//the latest joint probability
		//---------------------------
		jp_prob_return.clear();
		jp_name_return.clear();
		for (int j = 0; j<i_size_jp_prob; j++)
		{
			jp_prob_return.push_back(jp_prob_new[j]); //return with jp_prob_new
			jp_name_return.push_back(jp_name_new[j]); //return with jp_name_new		
		}

		//testout
		//RPrint(" ========= Cell_Prob_Extension.. has successfully finished!", TestOut);
		//Rprintf("========= Cell_Prob_Extension_KNN.. has successfully finished! \n");
		//-------------
		//deallocation
		//-------------
		//delete[] w;
		delete[] w_ml;
		delete[] w1;
		//delete[] id; 
		delete[] d_fmat1;
		Del_dMatrix(d_ox, i_size_ol, ncol);
		Del_dMatrix(d_mx, i_size_ml, ncol);
		Del_dMatrix(mox, nrow, ncol);
		Del_dMatrix(uox, nrow, ncol);

		delete[] cn;
		delete[] cn0;
		delete[] s_ocn;
		delete[] s_mcn;
		delete[] s_fcd;

		delete[] s_ocn_temp;
		delete[] s_mcn_temp;

		return 1;

	}

} //end of namespace


//Fn===========================================================================

//Fully_Efficient_Fractional_Imputation.cc-----------------------------------------------------------------------------

//Fn===========================================================================

namespace FHDI

{



void Fully_Efficient_Fractional_Imputation(const int ng, const int mg, 

		std::vector<int> v_obsg, std::vector<int> v_mxl, 

		double** y, double** z, const int nrow, const int ncol,

		std::vector<int> v_cn_z_i, double* fwij, const int i_size_v_cn_obsg_ncp,

		double* w, int* id,

		double** fmat)

//Description----------------------

//perform FEFI 

//  Algorithm: impute by using all possible donors

//  final outcome is "fmat" in which each column means that

//  col1: id

//  col2: fid, i.e., id of imputed value

//  col3: sampling weight

//  col4: fractional weights 

//  col5: imputed original data (matrix with column of ncol) 

//  col6: imputed category data (matrix with column of ncol)

//  col7: 1:ng 

//  col8: = col2  (for consistency with FHDI results)

//  col9: = col3  (for consistency with FHDI results)

//

//

// original R code: Dr. Im, J. and Dr. Kim, J. 

// c++ code: 		Dr. Cho, I. 

// All rights reserved

// 

// updated: Nov 14, 2016

//

//IN   : int ng = number of observed donors (=i_size_v_obsg)

//IN   : int mg = Actual row locations that has the same as current missing pattern 

//IN   : std::vector<int> v_obsg = observed donors ordered by half-ascending and -descending manner

//IN   : std::vector<int> v_mxl = a missing row's columns having observed cells  

//IN   : double y(nrow, ncol)= original data matrix with missing cells 

//IN   : double z(nrow, ncol)= categorized matrix of y. 0 for missing cell

//IN   : std::vector<int> v_cn_z_i = actual locations of current missing row in cn

//IN   : double* fwij[i_size_v_cn_obsg_ncp];

//IN   : double* w[nrow]

//IN   : int* id[nrow]

//

//OUT  : double** fmat(ng*mg, 7+2*ncol) see details in the above 

//----------------------

{

	double* wij = new double[ng*mg]; //donors & locations 

	

	//-----------

	//---local sizes

	//-----------

	const int i_size_v_obsg = (int)v_obsg.size(); 

	const int i_size_v_cn_z_i = (int)v_cn_z_i.size(); 

	

	std::vector<int> v_obsg_times_mg; 

	for(int j=0; j<mg; j++)

		for(int k=0; k<i_size_v_obsg; k++)

		{   v_obsg_times_mg.push_back(v_obsg[k]);   }



	const int i_size_v_obsg_times_mg = (int)v_obsg_times_mg.size(); 



	//----------------------------

	//impute missing cells from original data matrix 

	//----------------------------

	double** d_iy = New_dMatrix(i_size_v_obsg_times_mg, ncol); //imputed original matrix

	double** d_cmat = New_dMatrix(i_size_v_obsg_times_mg, ncol); //imputed category matrix



	const int i_size_v_mxl = (int)v_mxl.size(); 

	int i_mxl = 0; //default of non-missing cell of this row  

	if(i_size_v_mxl >= 1) i_mxl = i_size_v_mxl;  

	

	//default imputed cell is the observed original cells 

	for(int j=0; j<i_size_v_obsg_times_mg; j++)

	{   

		for(int k=0; k<ncol; k++)

		{

			d_iy[j][k] = y[v_obsg_times_mg[j] - 1][k];  //-1 for actual size  

			d_cmat[j][k] = z[v_obsg_times_mg[j] - 1][k];  //-1 for actual loc		

		}

	}  

	

	//impute missing cells using donors  

	if(i_mxl >= 1)

	{

		for(int k=0; k<i_mxl; k++) //column-wise copy

		{   

			int i_temp_row_iy=0; //sequential index in each row 

			for(int j=0; j<i_size_v_cn_z_i; j++) //Note: i_size_v_cn_z_i = length(loc2)

			{

				//Note v_cn_z_i = loc2 //

				double d_temp_iy = y[v_cn_z_i[j] - 1][v_mxl[k]-1];  //-1 for actual 

				

				for( int j2=0; j2< ng; j2++) //repeat 

					d_iy[i_temp_row_iy++][v_mxl[k]-1] = d_temp_iy;   						

			}



		}  

	}

	

	

	//------------------------------

	//------------------------------

	//make return matrix fmat[][]

	//------------------------------

	//------------------------------

	for(int j=0; j<mg; j++)

	{

		for(int k=0; k<ng; k++) wij[ng*j + k] = fwij[k]; 

	}

	

	//column 1: id[]// note: v_cn_z_i means loc2 

	for(int j=0; j<i_size_v_cn_z_i; j++)

	{ 

		int i_temp_fmat_col1 = id[v_cn_z_i[j]-1]; //-1 for actual location 

		

		for(int k=0; k<ng; k++) //repeat each id ng times

		{

			fmat[ng*j + k][0] = i_temp_fmat_col1; 

		}

	}		

	

	//column 2: 1:ng repeated mg times // note: v_cn_z_i = loc2 

	for(int j=0; j<mg; j++)

	{ 				

		for(int k=0; k<ng; k++) 

		{

			fmat[ng*j + k][1] = k+1; //store actual number  

		}

	}

				

	//column 3: w[]// note: v_cn_z_i means loc2 

	for(int j=0; j<i_size_v_cn_z_i; j++)

	{ 

		double d_temp_fmat_col3 = w[v_cn_z_i[j]-1]; //-1 for actual location 

		

		for(int k=0; k<ng; k++) 

		{

			fmat[ng*j + k][2] = d_temp_fmat_col3; 

		}

	}

	

	//column 4: wij[]// note: v_cn_z_i means loc2 

	for(int j=0; j<i_size_v_cn_z_i; j++)

	{ 

		for(int k=0; k<ng; k++) 

		{

			fmat[ng*j + k][3] = wij[ng*j + k]; 

		}

	}				

	

	//column set 5: [4, (4+ncol-1)]: d_iy[][ncol]

	int i_begin = 4; //starting point of current column set 

	for(int j=0; j<i_size_v_cn_z_i; j++)

	{ 

		for(int k=0; k<ng; k++) 

		{

			for(int i_col_iy=0; i_col_iy<ncol; i_col_iy++)

			{  fmat[ng*j + k][i_begin+i_col_iy] 

					   = d_iy[ng*j + k][i_col_iy];      }

			 

		}

	}				

	

	//column set 6: [4+ncol, (4+2*ncol-1)]: d_cmat[][ncol]

	i_begin = 4+ncol; //starting point of current column set 

	for(int j=0; j<i_size_v_cn_z_i; j++)

	{ 

		for(int k=0; k<ng; k++) 

		{

			for(int i_col_iy=0; i_col_iy<ncol; i_col_iy++)

			{  fmat[ng*j + k][i_begin+i_col_iy] 

					   = d_cmat[ng*j + k][i_col_iy];      }

			 

		}

	}

	

	//column set 7: at 4+2*ncol. obsg[]

	i_begin = 4+2*ncol; //starting point of current column set 

	for(int j=0; j<i_size_v_cn_z_i; j++) //Note: = mg 

	{ 

		for(int k=0; k<ng; k++) 

		{

			fmat[ng*j + k][i_begin] = v_obsg[k];      

		}

	}			

	

	//column set 8: at 4+2*ncol+1. 1:ng repeated by mg times 

	i_begin = 4+2*ncol+1; //starting point of current column set 

	for(int j=0; j<mg; j++) // 

	{ 

		for(int k=0; k<ng; k++) 

		{

			fmat[ng*j + k][i_begin] = k+1;      

		}

	}				

	

	//column set 9: at 4+2*ncol+2. fwij repeated by mg times 

	i_begin = 4+2*ncol+2; //starting point of current column set 

	for(int j=0; j<mg; j++) // 

	{ 

		for(int k=0; k<ng; k++) 

		{

			fmat[ng*j + k][i_begin] = fwij[k];      

		}

	}			

	

	//------------

	//Deallocation 

	//------------

	Del_dMatrix(d_iy, i_size_v_obsg_times_mg, ncol);

	Del_dMatrix(d_cmat, i_size_v_obsg_times_mg, ncol);

	

	return; 

}



} //end of namespace



//Fn===========================================================================

//Fractional_Hot_Deck_Imputation.cc-----------------------------------------------------------------------------

//Fn===========================================================================

namespace FHDI

{



void Fractional_Hot_Deck_Imputation(const int i, 

				const int ng, List_FHDI &List_ocsg, const int ncol,

				double** mox, double** y, const int nrow, const int i_M, 

				const int mg, double** z, const int i_mxl, 

				std::vector<int> v_cn_z_i, std::vector<int> v_mxl,

				std::vector<int> v_obsg,

				double* fwij, const int i_size_v_cn_obsg_ncp,

				double* d_obsp, int* i_obsn, 

				const double d_myran, 

				double* w, int* id,

				double** fmat_FHDI)

//Description----------------------

//perform FHDI 

//  Algorithm: impute by using "M" possible donors

//  final outcome is "fmat" in which each column means that

//  col1: id

//  col2: fid, i.e., id of imputed value

//  col3: sampling weight

//  col4: fractional weights 

//  col5: imputed original data (matrix with column of ncol) 

//  col6: imputed category data (matrix with column of ncol)

//  col7: 1:ng 

//  col8: = col2  (for consistency with FHDI results)

//  col9: = col3  (for consistency with FHDI results)

//

//

// original R code: Dr. Im, J. and Dr. Kim, J. 

// c++ code: 		Dr. Cho, I. 

// All rights reserved

// 

// updated: March 30, 2017

//

//IN   : int i = the current row 

//IN   : int ng = number of observed donors (=i_size_v_obsg)

//IN   : List_FHDI List_ocsg(nrm); //order records used for variance estimation

//IN   : double** mox(nrow_mox, ncol)  = rows of missing cells 

//IN   : double y(nrow, ncol)= original data matrix with missing cells 

//IN   : int i_M = number of possible donors

//IN   : int mg = Actual row locations that has the same as current missing pattern 

//IN   : double z(nrow, ncol)= categorized matrix of y. 0 for missing cell

//IN   : int i_mxl = number of non-missing column of current row

//IN   : std::vector<int> v_cn_z_i = actual locations of current missing row in cn

//IN   : std::vector<int> v_mxl = a missing row's columns having observed cells  

//IN   : std::vector<int> v_obsg = observed donors ordered by half-ascending and -descending manner

//IN   : double* fwij[i_size_v_cn_obsg_ncp];

//IN   : double* d_obsp = new double[i_size_v_cn_obsg_ncp]; //joint prob of selected donors

//IN   : int*    i_obsn = new int[i_size_v_cn_obsg_ncp]; //counts of the selected donors

//IN   : double d_myran   = random number generated from the uniform distribution

//                          

//IN   : double* w[nrow]

//IN   : int* id[nrow]

//

//OUT  : double** fmat_FHDI(min(ng, i_M) * mg, 7+2*ncol) see details in the above 

//----------------------

{

	//testout

	//RPrint("============== in FHDI  FHDI ==============");

	

	//------------

	//initial data setting

	//------------

	const int i_size_v_obsg = ng; 

	const int i_size_v_cn_z_i = (int)v_cn_z_i.size(); //same as mg and loc2

	

	

	//---------

	//get ocsg, observed donors

	//---------

	double* d_temp_lloc = new double[i_size_v_obsg]; 

	List_ocsg.get_block(i, d_temp_lloc); //get ith stored row from storage 

	//testout

	//RPrint("lloc :"); //RPrint(d_temp_lloc, i_size_v_obsg);



	//--------

	//missing column of current row

	//--------

	std::vector<int> v_rloc; //actual locations of missing

	v_rloc.clear(); //important since this is inside loop

	

	for(int k=0; k<ncol; k++) 

	{ if(fabs_FHDI(mox[i][k]) < 1e-15) v_rloc.push_back(k+1); }//+1 for actual location

	const int i_size_v_rloc = (int)v_rloc.size(); 

	//testout

	//RPrint("v_rloc :"); //RPrint(v_rloc);

			

	//-------

	//get donors at missing column locations

	//-------

	double** dy = New_dMatrix(i_size_v_obsg, i_size_v_rloc);

	for(int j=0; j<i_size_v_rloc; j++)

	{

		for(int k=0; k<i_size_v_obsg; k++)

		{

			dy[k][j] = y[(int)d_temp_lloc[k]-1][v_rloc[j]-1]; //-1 for actual location 

		}

	}

	//testout

	//RPrint("dy :"); //RPrint(dy, i_size_v_obsg, i_size_v_rloc);

			

	//------------

	//covariance matrix of dy[][]

	//Note: column-wise calculation for the covariance 

	//------------

	double** VM_dy = New_dMatrix(i_size_v_rloc, i_size_v_rloc);

	cov_FHDI(dy, i_size_v_obsg, i_size_v_rloc, VM_dy);

	//testout

	//RPrint("VM_dy :"); //RPrint(VM_dy, i_size_v_rloc, i_size_v_rloc);

			

	//----

	//(a) determine MM depending upon donors and i_M

	//(b) declare memories for FHDI parts

	//----

	int MM = 0; 

	if(i_size_v_obsg <= i_M) MM=i_size_v_obsg; 

	if(i_size_v_obsg >  i_M) MM=i_M; 



	int* i_SN = new int[MM*mg];

	double** d_iy   = New_dMatrix(MM*mg, ncol); //original matrix

	double** d_cmat = New_dMatrix(MM*mg, ncol); //categorized matrix 

	double* wij = new double[ng*mg]; //donors & locations 

	double* d_cs = new double[ng]; 

	double* d_cs_temp = new double[ng]; 

	double* d_Li = new double[ng]; 

	double* d_Ui = new double[ng]; 

	double* d_fefim = new double[i_size_v_rloc];

	int* i_rmg = new int[mg*MM];

	int* i_rM  = new int[mg*MM];

	double* d_SR = new double[mg*MM];

	int* i_obSN = new int[mg*MM];

	//double** fmat_FHDI = New_dMatrix(mg*MM, 7+2*ncol); //7columns and two blocks of ncol 

			

	//------------------------

	//FHDI Case 1: when donors <= M

	//             use all possible donors

	//------------------------

	//int MM=0; 

	if(i_size_v_obsg <= i_M)

	{

		//----

		//index of all donors 

		//----

		for(int j=0; j<mg; j++)

		{

			for(int k=0; k<MM; k++)

				i_SN[j*MM+k] = k+1; //+1 for actual location 

		}

		

		//--------

		//get ready donors from original data

		//--------

		for(int j=0; j<MM*mg; j++)

		{

			for(int k=0; k<ncol; k++)

			{ 

				d_iy[j][k]   = y[v_obsg[i_SN[j]-1]-1][k];  

				d_cmat[j][k] = z[v_obsg[i_SN[j]-1]-1][k];  //-1 for actual loc

			} //-1 for actual location 

		}

		//--------

		//non-missing column consideration of current row

		//--------

		if(i_mxl >= 1)

		{

			for(int k=0; k<i_mxl; k++) //column-wise copy

			{   

				int i_temp_row_iy=0; //sequential index in each row 

				for(int j=0; j<i_size_v_cn_z_i; j++) //Note: i_size_v_cn_z_i = length(loc2)

				{

					//Note v_cn_z_i = loc2 //

					double d_temp_iy = y[v_cn_z_i[j] - 1][v_mxl[k]-1];  //-1 for actual 

		

					for( int j2=0; j2< MM; j2++) //repeat 

						d_iy[i_temp_row_iy++][v_mxl[k]-1] = d_temp_iy;   						

				}



			}  					

		}

		

		//------------

		//weights 

		//------------

		for(int j=0; j<mg; j++)

		{

			for(int k=0; k<ng; k++) wij[ng*j + k] = fwij[k]; 

		}

		



	}

	

	//------------------------

	//FHDI Case 2: when donors > M

	//   select donors with probability proportional to size sampling 

	//------------------------

	if(i_size_v_obsg > i_M)

	{

		//-------------

		//cumulative sum of joint probability with pps 

		//-------------

		for(int j=0; j<ng; j++) 

		{    

			d_cs_temp[j] = d_obsp[j]; //default  

			if(i_obsn[j] !=0) d_cs_temp[j] = d_obsp[j]*MM/i_obsn[j];

		} 

		cumsum_FHDI(d_cs_temp, ng, d_cs); 

		

		//----------

		//get ready Li and Ui

		//----------

		d_Li[0] = 0.0; 

		for(int j=1; j<ng; j++)

		{

			d_Li[j] = d_cs[j-1]; //exclude the last entity 

		}

		Copy_dVector(d_cs, ng, d_Ui); 



		

		//-----------------

		//sum(dy*fwij)

		//-----------------

		double d_sum_fefim=0.0;

		for(int j=0; j<i_size_v_rloc; j++)

		{

			d_sum_fefim = 0.0; 

			//---row-wise sum---//

			for(int k=0; k<i_size_v_obsg; k++)

			{

				d_sum_fefim += dy[k][j]*fwij[k]; 

			}

			d_fefim[j] = d_sum_fefim; 

		}

		//testout

		//RPrint("fefim :"); //RPrint(d_fefim, i_size_v_rloc);

		

		//-----------------

		//random location using uniform distribution   

		//using Numerical Recipes of Press et al 2007. 

		//-----------------

		//double d_Rg_myran = myran.doub();

		//Rprintf("d_Rg using myran():"<<d_Rg_myran);

		

		//simple version using standard rand() for CRAN compatibility, March 30, 2017

		double d_Rg = d_myran;


		//double d_Rg = 0.0633672; //for debugging !!!

		

		//below codes is not recommended by Press et al. 2007. 

		//below is only available for c++ compiler after 2011 

		//std::default_random_engine generator; 

		//std::uniform_real_distribution<double> distribution(0.0, 1.0); 

		//d_Rg = distribution(generator);

		

		for(int j=0; j<mg; j++) 

		{	for(int k=0; k<MM; k++) i_rmg[j*MM+k]=j+1;  }

	

		for(int j=0; j<mg; j++) 

		{	for(int k=0; k<MM; k++)  i_rM[j*MM+k]=k+1;  }

		

		for(int j=0; j<mg*MM; j++)

		{

			d_SR[j] = (d_Rg+(i_rmg[j]-1))/mg + (i_rM[j]-1); 

		}



		//----------

		//set of SR < Ui

		//----------

		int i_SR_Ui = 0;  

		for(int k=0; k<mg*MM; k++)

		{

			i_SR_Ui = 0; //minimum location where SR <= Ui

			for(int j=0; j<ng; j++) 

			{  if(d_SR[k] <= d_Ui[j]) {i_SR_Ui = j+1; break;} }

			

			i_SN[k] = i_SR_Ui; //Actual location stored 

		}

		//-------------

		// select out M donors from half-asc and -desc observation

		//-------------

		for(int j=0; j<mg*MM; j++)

		{ i_obSN[j] = v_obsg[i_SN[j]-1]; } //-1 for actual location 



		//----------------------------

		//impute missing cells from original data matrix 

		//----------------------------

		for(int j=0; j<mg*MM; j++)

		{   

			for(int k=0; k<ncol; k++)

			{

				d_iy[j][k]   = y[i_obSN[j]-1][k];  //-1 for actual size  

				d_cmat[j][k] = z[i_obSN[j]-1][k];  //-1 for actual loc		

			}

		}



		//impute missing cells using donors  

		if(i_mxl >= 1)

		{

			for(int k=0; k<i_mxl; k++) //column-wise copy

			{   

				int i_temp_row_iy=0; //sequential index in each row 

				for(int j=0; j<mg; j++) //cf. i_size_v_cn_z_i = length(loc2)

				{

					//Note v_cn_z_i = loc2 //

					double d_temp_iy = y[v_cn_z_i[j] - 1][v_mxl[k]-1];  //-1 for actual 

		

					for( int j2=0; j2< MM; j2++) //repeat 

						d_iy[i_temp_row_iy++][v_mxl[k]-1] = d_temp_iy;   						

				}

			}  

		}

		

		//------------

		//diy : not used. So not implemented as of Nov 15, 2016

		//------------

		

		//------------

		//wij

		//------------

		for(int j=0; j<mg*MM; j++) wij[j] = 1.0/MM; 

	}			

	

	//------------------------------

	//------------------------------

	//make return matrix fmat_FHDI[][]

	// Note: row number differs from fmat[][]

	//       must be appended to previous fmat[][] 

	//------------------------------

	//------------------------------

	//column 1: id[]// note: v_cn_z_i means loc2 

	for(int j=0; j<i_size_v_cn_z_i; j++)

	{ 

		int i_temp_fmat_col1 = id[v_cn_z_i[j]-1]; //-1 for actual location 

		

		for(int k=0; k<MM; k++) //repeat each id MM times

		{

			fmat_FHDI[MM*j + k][0] = i_temp_fmat_col1; 

		}

	}		

	

	//column 2: 1:MM repeated mg times // note: v_cn_z_i = loc2 

	for(int j=0; j<mg; j++)

	{ 				

		for(int k=0; k<MM; k++) 

		{

			fmat_FHDI[MM*j + k][1] = k+1; //store actual number  

		}

	}

				

	//column 3: w[]// note: v_cn_z_i means loc2 

	for(int j=0; j<i_size_v_cn_z_i; j++)

	{ 

		double d_temp_fmat_col3 = w[v_cn_z_i[j]-1]; //-1 for actual location 

		

		for(int k=0; k<MM; k++) 

		{

			fmat_FHDI[MM*j + k][2] = d_temp_fmat_col3; 

		}

	}

	

	//column 4: wij[]// note: v_cn_z_i means loc2 

	for(int j=0; j<i_size_v_cn_z_i; j++)

	{ 

		for(int k=0; k<MM; k++) 

		{

			fmat_FHDI[MM*j + k][3] = wij[MM*j + k]; 

		}

	}				

	

	//column set 5: [4, (4+ncol-1)]: d_iy[][ncol]

	int i_begin = 4; //starting point of current column set 

	for(int j=0; j<i_size_v_cn_z_i; j++)

	{ 

		for(int k=0; k<MM; k++) 

		{

			for(int i_col_iy=0; i_col_iy<ncol; i_col_iy++)

			{  fmat_FHDI[MM*j + k][i_begin+i_col_iy] 

					   = d_iy[MM*j + k][i_col_iy];      }

			 

		}

	}				

	

	//column set 6: [4+ncol, (4+2*ncol-1)]: d_cmat[][ncol]

	i_begin = 4+ncol; //starting point of current column set 

	for(int j=0; j<i_size_v_cn_z_i; j++)

	{ 

		for(int k=0; k<MM; k++) 

		{

			for(int i_col_iy=0; i_col_iy<ncol; i_col_iy++)

			{  fmat_FHDI[MM*j + k][i_begin+i_col_iy] 

					   = d_cmat[MM*j + k][i_col_iy];      }

			 

		}

	}

	

	//column set 7: at 4+2*ncol. obsg[]

	i_begin = 4+2*ncol; //starting point of current column set 

	for(int j=0; j<i_size_v_cn_z_i; j++) //Note: = mg 

	{ 

		for(int k=0; k<MM; k++) 

		{

			fmat_FHDI[MM*j + k][i_begin] 

					   = v_obsg[i_SN[MM*j+k]-1];      

		}

	}			

	

	//column set 8: at 4+2*ncol+1. 1:ng repeated by mg times 

	i_begin = 4+2*ncol+1; //starting point of current column set 

	for(int j=0; j<mg; j++) // 

	{ 

		for(int k=0; k<MM; k++) 

		{

			fmat_FHDI[MM*j + k][i_begin] = i_SN[j*MM + k];      

		}

	}				

	

	//column set 9: at 4+2*ncol+2. fwij repeated by mg times 

	i_begin = 4+2*ncol+2; //starting point of current column set 

	for(int j=0; j<mg; j++) // 

	{ 

		for(int k=0; k<MM; k++) 

		{

			fmat_FHDI[MM*j + k][i_begin] = fwij[i_SN[j*MM+k]-1];      

		}

	}			

		

	

	

	//-------------------

	//local deallocation

	//-------------------

	delete[] d_temp_lloc; 

	Del_dMatrix(dy, i_size_v_obsg, i_size_v_rloc);	

	Del_dMatrix(VM_dy, i_size_v_rloc, i_size_v_rloc);	



	//----------

	//local deallocation for FHDI parts

	//----------

	delete[] d_cs;

	delete[] d_cs_temp;	

	delete[] d_Li;

	delete[] d_Ui;	

	delete[] d_fefim; 

	delete[] i_rmg; 

	delete[] i_rM; 

	delete[] d_SR; 

	delete[] i_SN; 

	delete[] i_obSN; 

	Del_dMatrix(d_iy,   mg*MM, ncol);

	Del_dMatrix(d_cmat, mg*MM, ncol);

	delete[] wij; 

	//Del_dMatrix(fmat_FHDI, mg*MM, 7+2*ncol);			

		



	return; 

}



} //end of namespace





//Fn===========================================================================

//Results_Fully_Efficient_Fractional_Imputation.cc-----------------------------------------------------------------------------

//Fn===========================================================================

namespace FHDI

{



void Results_Fully_Efficient_Fractional_Imputation(const int i_size_ol, 

		const int ncol,  const int nrow,

		int* id_ol, double* w_ol, double** d_oy, double** d_ox, 

		rbind_FHDI &rbind_imat_FEFI, int** r,

		

		rbind_FHDI &rbind_ipmat_FEFI, 

		rbind_FHDI &rbind_Resp_FEFI, 

		rbind_FHDI &rbind_irmat_FEFI)

//Description----------------------

// prepare output results of  FEFI 

//

//ipmat  = final imputation results

//     	col1: ID 	= unit index

//		col2: FID 	= ID of fractionally imputed value

// 		col3: WGT 	= weight 

//		col4: FWGT	= Frational weight

//		col5: Variables 

//		col6: Responses

//irmat  = imputation results related to the categorized matrix 

//     	col1: ID 	= unit index

//		col2: FID 	= ID of fractionally imputed value

//		col3: OID	= original rank of the imputed value

//		col4: ORDER = SN(selected donor)

//		col5: FEFIW	= Fefi weights 

//		col6: CELL	= cells //

//

// original R code: Dr. Im, J. and Dr. Kim, J. 

// c++ code: 		Dr. Cho, I. 

// All rights reserved

// 

// updated: Nov 21, 2016

//

//IN   : int i_size_ol = rows with observed cells 

//IN   : int ncol = number of column 

//IN   : int* id_ol [i_size_ol]; //same as oid

//IN   : double* w_ol [i_size_ol]; //same as ow

//IN   : double** d_oy (i_size_ol, ncol);  //observed oritianl matrix 

//IN   : double** d_ox (i_size_ol, ncol);  //observed categorized matrix 



//IN   : rbind_imat_FEFI (column =7+2*ncol) = accumulated result matrix of FEFI 

//IN   : int    r(nrow, ncol) = matrix of missing (0)/observed (1) index  

//OUT  : rbind_FHDI  rbind_ipmat_FEFI(4+ncol); //column size is 4+ncol

//OUT  : rbind_FHDI  rbind_Resp_FEFI(ncol+1);  //separate response matrix. Note: in R version it is 

//                                               attached to ipmat  

//OUT  : rbind_FHDI  rbind_irmat_FEFI(5+ncol); //column size is 5+ncol

//----------------------

{

	

	double** d_ipmat0 = New_dMatrix(i_size_ol, 4+ncol);  

	for(int i=0; i<i_size_ol; i++)

	{

		//col1: oid

		d_ipmat0[i][0] = id_ol[i]; 

		//col2: 1

		d_ipmat0[i][1] = 1;

		//col3: ow

		d_ipmat0[i][2] = w_ol[i]; 		

		//col4: 1

		d_ipmat0[i][3] = 1;

		//col5 set: oy

		for(int j=0; j<ncol; j++) d_ipmat0[i][4+j] = d_oy[i][j]; 				

	}

	

	//-----

	//FEFI results output first NOTE: FHDI results should be considered separately!!!

	//-----

	const int i_row_imat_FEFI = rbind_imat_FEFI.size_row(); //total rows of accumulated matrix 

	double** d_ipmat1_FEFI = New_dMatrix(i_row_imat_FEFI, 4+ncol); 

	for(int i=0; i<4+ncol; i++)

	{

		for(int j=0; j< i_row_imat_FEFI; j++)

			d_ipmat1_FEFI[j][i] = rbind_imat_FEFI(j, i);

	}

	

	//---

	//final return matrix of impat of FEFI

	//---

	rbind_ipmat_FEFI.bind_blocks(i_size_ol, 4+ncol, d_ipmat0); //new addition with ipmat0

	rbind_ipmat_FEFI.bind_blocks(i_row_imat_FEFI, 4+ncol, d_ipmat1_FEFI); //append ipmat1 of FEFI

	

	//---

	//re-order ipmat of FEFI with respect to oid, the first column

	//---

	const int i_row_ipmat_FEFI = (int)rbind_ipmat_FEFI.size_row(); 

	int* i_order_ipmat_FEFI = new int[i_row_ipmat_FEFI];

	for(int i=0; i<i_row_ipmat_FEFI; i++) i_order_ipmat_FEFI[i] = rbind_ipmat_FEFI(i,0);

	order_FHDI(i_order_ipmat_FEFI, i_row_ipmat_FEFI);

	//backup before re-order

	double** ipmat_FEFI_backup = New_dMatrix(i_row_ipmat_FEFI, 4+ncol); //ordered ipmat FEFI

	for(int i=0; i<i_row_ipmat_FEFI; i++)

	{

		int i_row_current = i_order_ipmat_FEFI[i]-1; //-1 is for ACTUAL location

		for(int j=0; j<4+ncol; j++) ipmat_FEFI_backup[i][j] = rbind_ipmat_FEFI(i_row_current, j);

	}

	//----

	//re-initialize and store the ordered matrix 

	//----

	rbind_ipmat_FEFI.initialize(4+ncol); //Note: not imat but ipmat!!!

	rbind_ipmat_FEFI.bind_blocks(i_row_ipmat_FEFI, 4+ncol, ipmat_FEFI_backup);	

	

	

	//----

	//make table of id (1st column) of ipmat

	//----

	double* d_first_column_ipmat = new double[i_row_ipmat_FEFI];

	std::vector<double> v_table_name_1stcol_ipmat; 

	std::vector<int>    v_table_count_1stcol_ipmat; 

	

	for(int i=0; i<i_row_ipmat_FEFI; i++) d_first_column_ipmat[i] = rbind_ipmat_FEFI(i,0);

	

	table_cpp(d_first_column_ipmat, i_row_ipmat_FEFI, 

		      v_table_name_1stcol_ipmat, v_table_count_1stcol_ipmat);

	//const int i_size_table_ipmat = (int)v_table_count_1stcol_ipmat.size(); 

	

	//----

	//make Resp matrix 

	//----

	double** d_Resp_FEFI = New_dMatrix(i_row_ipmat_FEFI, ncol+1); 

	for(int i=0; i<ncol; i++)

	{

		int i_temp = 0; 

		for(int k=0; k<nrow; k++) 

		{

			int i_repeat = v_table_count_1stcol_ipmat[k]; 

			for(int j=0; j<i_repeat; j++)	

			{

				d_Resp_FEFI[i_temp][i] = r[k][i]; 

				i_temp++; 

			}

		}

	}

	//-----

	//last column of Resp is prod of response: 0=at least one missing 

	//-----

	for(int i=0; i<i_row_ipmat_FEFI; i++)

	{

		int i_temp =1; 

		for(int j=0; j<ncol; j++) i_temp = i_temp * d_Resp_FEFI[i][j]; 

		//0= at least one missing; 1=all observed 

		d_Resp_FEFI[i][ncol] = i_temp; //last column at ncol+1 

	}

	rbind_Resp_FEFI.bind_blocks(i_row_ipmat_FEFI, ncol+1, d_Resp_FEFI);	//prep return 

	//---------

	//Note: in R serial version, ipmat and Resp are attached column-wise, but 

	//in c++ version, it is kept separate for better memory usage

	//Nov 21, 2016

	//---------



	//-------------------------

	//-------------------------

	//make irmat of FEFI

	//-------------------------

	//-------------------------

	const int nci_FEFI = 7+2*ncol; 



	double** d_irmat0 = New_dMatrix(i_size_ol, 5+ncol);  

	for(int i=0; i<i_size_ol; i++)

	{

		//col1: oid

		d_irmat0[i][0] = id_ol[i]; 

		//col2: 1

		d_irmat0[i][1] = 1;

		//col3: oid

		d_irmat0[i][2] = id_ol[i]; 		

		//col4: 1

		d_irmat0[i][3] = 1;

		//col5: 1

		d_irmat0[i][4] = 1;

		//col6 set: ox

		for(int j=0; j<ncol; j++) d_irmat0[i][5+j] = d_ox[i][j]; 				

	}

			

	//----

	//extract columns 1, 2, (nci-2):nci, -(1:ncol of ipmat0, (nci-2):nci)

	//----

	int* i_col_irmat1 = new int[5+ncol]; //columns to be extracted from imat_FEFI

	i_col_irmat1[0] = 1; //ACTUAL col id

	i_col_irmat1[1] = 2; //ACTUAL col id

	i_col_irmat1[2] = nci_FEFI-2; //ACTUAL col id

	i_col_irmat1[3] = nci_FEFI-1; //ACTUAL col id

	i_col_irmat1[4] = nci_FEFI  ; //ACTUAL col id

    //exclude 1:4+ncol, i.e., columns of ipmat0

	for(int i=0; i<ncol; i++) i_col_irmat1[5+i] = i+(4+ncol+1); //ACTUAL id 

    

	

	double** d_irmat1_FEFI = New_dMatrix(i_row_imat_FEFI, 5+ncol); 

	for(int i=0; i<(5+ncol); i++)

	{

		int i_temp_irmat1 = i_col_irmat1[i] - 1 ; //-1 actual column id

		for(int j=0; j< i_row_imat_FEFI; j++)

			d_irmat1_FEFI[j][i] = rbind_imat_FEFI(j, i_temp_irmat1);

	}

	

	//---

	//final return matrix of irmat of FEFI

	//---

	//rbind_FHDI  rbind_irmat_FEFI(5+ncol); //column size is 5+ncol //defined outside 

	rbind_irmat_FEFI.bind_blocks(i_size_ol, 5+ncol, d_irmat0); //new addition with irmat0

	rbind_irmat_FEFI.bind_blocks(i_row_imat_FEFI, 5+ncol, d_irmat1_FEFI); //append irmat1 of FEFI

	

	//---

	//re-order irmat of FEFI with respect to oid, the first column

	//---

	const int i_row_irmat_FEFI = (int)rbind_irmat_FEFI.size_row(); 

	int* i_order_irmat_FEFI = new int[i_row_irmat_FEFI];

	for(int i=0; i<i_row_irmat_FEFI; i++) i_order_irmat_FEFI[i] = rbind_irmat_FEFI(i,0);

	order_FHDI(i_order_irmat_FEFI, i_row_irmat_FEFI);

	//backup before re-order

	double** irmat_FEFI_backup = New_dMatrix(i_row_irmat_FEFI, 5+ncol); //ordered irmat FEFI

	for(int i=0; i<i_row_irmat_FEFI; i++)

	{

		int i_row_current = i_order_irmat_FEFI[i]-1; //-1 is for ACTUAL location

		for(int j=0; j<5+ncol; j++) irmat_FEFI_backup[i][j] = rbind_irmat_FEFI(i_row_current, j);

	}

	//----

	//re-initialize and store the ordered matrix 

	//----

	rbind_irmat_FEFI.initialize(5+ncol); //Note: not imat but irmat!!!

	rbind_irmat_FEFI.bind_blocks(i_row_irmat_FEFI, 5+ncol, irmat_FEFI_backup);	

	

	//-------------------

	//Deallocation 

	//-------------------

	Del_dMatrix(d_ipmat0, i_size_ol, 4+ncol); //

	Del_dMatrix(d_ipmat1_FEFI, rbind_imat_FEFI.size_row(), 4+ncol); //

	delete[] i_order_ipmat_FEFI;//

	Del_dMatrix(ipmat_FEFI_backup, i_row_ipmat_FEFI, 4+ncol);//



	delete[] d_first_column_ipmat; //

	Del_dMatrix(d_Resp_FEFI, i_row_ipmat_FEFI, ncol+1);//

	

	Del_dMatrix(d_irmat0, i_size_ol, 5+ncol); 	//

	delete[] i_col_irmat1;//



	Del_dMatrix(d_irmat1_FEFI, rbind_imat_FEFI.size_row(), 5+ncol);//

	delete[] i_order_irmat_FEFI; //

	Del_dMatrix(irmat_FEFI_backup, i_row_irmat_FEFI, 5+ncol); //

	

	

	return; 	

	

}

} //end of namespace





//Fn===========================================================================

//Results_Fractional_Hot_Deck_Imputation.cc-----------------------------------------------------------------------------

//Fn===========================================================================

namespace FHDI

{



void Results_Fractional_Hot_Deck_Imputation(const int i_size_ol, 

		const int ncol,  const int nrow,

		int* id_ol, double* w_ol, double** d_oy, double** d_ox, 

		rbind_FHDI &rbind_imat_FHDI, int** r,

		

		rbind_FHDI &rbind_ipmat_FHDI, 

		rbind_FHDI &rbind_Resp_FHDI, 

		rbind_FHDI &rbind_irmat_FHDI)

//Description----------------------

// prepare output results of  FHDI 

//

//ipmat  = final imputation results

//     	col1: ID 	= unit index

//		col2: FID 	= ID of fractionally imputed value

// 		col3: WGT 	= weight 

//		col4: FWGT	= Frational weight

//		col5: Variables 

//		col6: Responses

//irmat  = imputation results related to the categorized matrix 

//     	col1: ID 	= unit index

//		col2: FID 	= ID of fractionally imputed value

//		col3: OID	= original rank of the imputed value

//		col4: ORDER = SN(selected donor)

//		col5: FEFIW	= Fefi weights 

//		col6: CELL	= cells //

//

// original R code: Dr. Im, J. and Dr. Kim, J. 

// c++ code: 		Dr. Cho, I. 

// All rights reserved

// 

// updated: Nov 21, 2016

//

//IN   : int i_size_ol = rows with observed cells 

//IN   : int ncol = number of column 

//IN   : int* id_ol [i_size_ol]; //same as oid

//IN   : double* w_ol [i_size_ol]; //same as ow

//IN   : double** d_oy (i_size_ol, ncol);  //observed oritianl matrix 

//IN   : double** d_ox (i_size_ol, ncol);  //observed categorized matrix 



//IN   : rbind_imat_FHDI (column =7+2*ncol) = accumulated result matrix of FHDI 

//IN   : int    r(nrow, ncol) = matrix of missing (0)/observed (1) index  

//OUT  : rbind_FHDI  rbind_ipmat_FHDI(4+ncol); //column size is 4+ncol

//OUT  : rbind_FHDI  rbind_Resp_FHDI(ncol+1);  //separate response matrix. Note: in R version it is 

//                                               attached to ipmat  

//OUT  : rbind_FHDI  rbind_irmat_FHDI(5+ncol); //column size is 5+ncol

//----------------------

{

	

	double** d_ipmat0 = New_dMatrix(i_size_ol, 4+ncol);  

	for(int i=0; i<i_size_ol; i++)

	{

		//col1: oid

		d_ipmat0[i][0] = id_ol[i]; 

		//col2: 1

		d_ipmat0[i][1] = 1;

		//col3: ow

		d_ipmat0[i][2] = w_ol[i]; 		

		//col4: 1

		d_ipmat0[i][3] = 1;

		//col5 set: oy

		for(int j=0; j<ncol; j++) d_ipmat0[i][4+j] = d_oy[i][j]; 				

	}

	//-----

	//FHDI results output first NOTE: FHDI results should be considered separately!!!

	//-----

	const int i_row_imat_FHDI = rbind_imat_FHDI.size_row(); //total rows of accumulated matrix 

	double** d_ipmat1_FHDI = New_dMatrix(i_row_imat_FHDI, 4+ncol); 

	for(int i=0; i<4+ncol; i++)

	{

		for(int j=0; j< i_row_imat_FHDI; j++)

			d_ipmat1_FHDI[j][i] = rbind_imat_FHDI(j, i);

	}

	//---

	//final return matrix of impat of FHDI

	//---

	//rbind_FHDI  rbind_ipmat_FHDI(4+ncol); //column size is 4+ncol //defined outside 

	rbind_ipmat_FHDI.bind_blocks(i_size_ol, 4+ncol, d_ipmat0); //new addition with ipmat0

	rbind_ipmat_FHDI.bind_blocks(i_row_imat_FHDI, 4+ncol, d_ipmat1_FHDI); //append ipmat1 of FHDI

	

	//---

	//re-order ipmat of FHDI with respect to oid, the first column

	//---

	const int i_row_ipmat_FHDI = (int)rbind_ipmat_FHDI.size_row(); 

	int* i_order_ipmat_FHDI = new int[i_row_ipmat_FHDI];

	for(int i=0; i<i_row_ipmat_FHDI; i++) i_order_ipmat_FHDI[i] = rbind_ipmat_FHDI(i,0);

	order_FHDI(i_order_ipmat_FHDI, i_row_ipmat_FHDI);

	//backup before re-order

	double** ipmat_FHDI_backup = New_dMatrix(i_row_ipmat_FHDI, 4+ncol); //ordered ipmat FHDI

	for(int i=0; i<i_row_ipmat_FHDI; i++)

	{

		int i_row_current = i_order_ipmat_FHDI[i]-1; //-1 is for ACTUAL location

		for(int j=0; j<4+ncol; j++) ipmat_FHDI_backup[i][j] = rbind_ipmat_FHDI(i_row_current, j);

	}

	//----

	//re-initialize and store the ordered matrix 

	//----

	rbind_ipmat_FHDI.initialize(4+ncol); //Note: not imat but ipmat!!!

	rbind_ipmat_FHDI.bind_blocks(i_row_ipmat_FHDI, 4+ncol, ipmat_FHDI_backup);	

	

	//----

	//make table of id (1st column) of ipmat

	//----

	double* d_first_column_ipmat = new double[i_row_ipmat_FHDI];

	std::vector<double> v_table_name_1stcol_ipmat; 

	std::vector<int>    v_table_count_1stcol_ipmat; 

	

	for(int i=0; i<i_row_ipmat_FHDI; i++) d_first_column_ipmat[i] = rbind_ipmat_FHDI(i,0);

	

	table_cpp(d_first_column_ipmat, i_row_ipmat_FHDI, 

		      v_table_name_1stcol_ipmat, v_table_count_1stcol_ipmat);

	

	//----

	//make Resp matrix 

	//----

	double** d_Resp_FHDI = New_dMatrix(i_row_ipmat_FHDI, ncol+1); 

	for(int i=0; i<ncol; i++)

	{

		int i_temp = 0; 

		for(int k=0; k<nrow; k++) 

		{

			int i_repeat = v_table_count_1stcol_ipmat[k]; 

			for(int j=0; j<i_repeat; j++)	

			{

				d_Resp_FHDI[i_temp][i] = r[k][i]; 

				i_temp++; 

			}

		}

	}

	//-----

	//last column of Resp is prod of response: 0=at least one missing 

	//-----

	for(int i=0; i<i_row_ipmat_FHDI; i++)

	{

		int i_temp =1; 

		for(int j=0; j<ncol; j++) i_temp = i_temp * d_Resp_FHDI[i][j]; 

		//0= at least one missing; 1=all observed 

		d_Resp_FHDI[i][ncol] = i_temp; //last column at ncol+1 

	}

	rbind_Resp_FHDI.bind_blocks(i_row_ipmat_FHDI, ncol+1, d_Resp_FHDI);	//prep return 

	//---------

	//Note: in R serial version, ipmat and Resp are attached column-wise, but 

	//in c++ version, it is kept separate for better memory usage

	//Nov 21, 2016

	//---------



	//-------------------------

	//-------------------------

	//make irmat of FHDI

	//-------------------------

	//-------------------------

	const int nci_FHDI = 7+2*ncol; 



	double** d_irmat0 = New_dMatrix(i_size_ol, 5+ncol);  

	for(int i=0; i<i_size_ol; i++)

	{

		//col1: oid

		d_irmat0[i][0] = id_ol[i]; 

		//col2: 1

		d_irmat0[i][1] = 1;

		//col3: oid

		d_irmat0[i][2] = id_ol[i]; 		

		//col4: 1

		d_irmat0[i][3] = 1;

		//col5: 1

		d_irmat0[i][4] = 1;

		//col6 set: ox

		for(int j=0; j<ncol; j++) d_irmat0[i][5+j] = d_ox[i][j]; 				

	}

			

	//----

	//extract columns 1, 2, (nci-2):nci, -(1:ncol of ipmat0, (nci-2):nci)

	//----

	int* i_col_irmat1 = new int[5+ncol]; //columns to be extracted from imat_FHDI

	i_col_irmat1[0] = 1; //ACTUAL col id

	i_col_irmat1[1] = 2; //ACTUAL col id

	i_col_irmat1[2] = nci_FHDI-2; //ACTUAL col id

	i_col_irmat1[3] = nci_FHDI-1; //ACTUAL col id

	i_col_irmat1[4] = nci_FHDI  ; //ACTUAL col id

    //exclude 1:4+ncol, i.e., columns of ipmat0

	for(int i=0; i<ncol; i++) i_col_irmat1[5+i] = i+(4+ncol+1); //ACTUAL id 

    

	

	double** d_irmat1_FHDI = New_dMatrix(i_row_imat_FHDI, 5+ncol); 

	for(int i=0; i<(5+ncol); i++)

	{

		int i_temp_irmat1 = i_col_irmat1[i] - 1 ; //-1 actual column id

		for(int j=0; j< i_row_imat_FHDI; j++)

			d_irmat1_FHDI[j][i] = rbind_imat_FHDI(j, i_temp_irmat1);

	}

	

	//---

	//final return matrix of irmat of FHDI

	//---

	rbind_irmat_FHDI.bind_blocks(i_size_ol, 5+ncol, d_irmat0); //new addition with irmat0

	rbind_irmat_FHDI.bind_blocks(i_row_imat_FHDI, 5+ncol, d_irmat1_FHDI); //append irmat1 of FHDI

	

	

	//---

	//re-order irmat of FHDI with respect to oid, the first column

	//---

	const int i_row_irmat_FHDI = (int)rbind_irmat_FHDI.size_row(); 

	int* i_order_irmat_FHDI = new int[i_row_irmat_FHDI];

	for(int i=0; i<i_row_irmat_FHDI; i++) i_order_irmat_FHDI[i] = rbind_irmat_FHDI(i,0);

	order_FHDI(i_order_irmat_FHDI, i_row_irmat_FHDI);

	//backup before re-order

	double** irmat_FHDI_backup = New_dMatrix(i_row_irmat_FHDI, 5+ncol); //ordered irmat FHDI

	for(int i=0; i<i_row_irmat_FHDI; i++)

	{

		int i_row_current = i_order_irmat_FHDI[i]-1; //-1 is for ACTUAL location

		for(int j=0; j<5+ncol; j++) irmat_FHDI_backup[i][j] = rbind_irmat_FHDI(i_row_current, j);

	}

	//----

	//re-initialize and store the ordered matrix 

	//----

	rbind_irmat_FHDI.initialize(5+ncol); //Note: not imat but irmat!!!

	rbind_irmat_FHDI.bind_blocks(i_row_irmat_FHDI, 5+ncol, irmat_FHDI_backup);	

	

	//-------------------

	//Deallocation 

	//-------------------

	Del_dMatrix(d_ipmat0, i_size_ol, 4+ncol); //

	Del_dMatrix(d_ipmat1_FHDI, rbind_imat_FHDI.size_row(), 4+ncol); //

	delete[] i_order_ipmat_FHDI;//

	Del_dMatrix(ipmat_FHDI_backup, i_row_ipmat_FHDI, 4+ncol);//



	delete[] d_first_column_ipmat; //

	Del_dMatrix(d_Resp_FHDI, i_row_ipmat_FHDI, ncol+1);//

	

	Del_dMatrix(d_irmat0, i_size_ol, 5+ncol); 	//

	delete[] i_col_irmat1;//



	Del_dMatrix(d_irmat1_FHDI, rbind_imat_FHDI.size_row(), 5+ncol);//

	delete[] i_order_irmat_FHDI; //

	Del_dMatrix(irmat_FHDI_backup, i_row_irmat_FHDI, 5+ncol); //

	

	return; 	

}

} //end of namespace





//Fn===========================================================================

//FHDI_Extension_cpp.cc-----------------------------------------------------------------------------

//Fn===========================================================================

namespace FHDI{



void yorder(double** y, const int nrow, const int ncol, 

            double* mox_1, 

			std::vector<int> v_loc, int* i_ym_return);

			

bool FHDI_Extension_cpp(double** y, double** z, int** r, 

						const int nrow, const int ncol, 

						std::vector<std::string> jp_name, 

						std::vector<double> 	 jp_prob, 

						std::string s_M, const int i_M, double* w, int* id, 



						rbind_FHDI &rbind_ipmat_FEFI, 

						rbind_FHDI &rbind_Resp_FEFI, 

						rbind_FHDI &rbind_irmat_FEFI,

						

						rbind_FHDI &rbind_ipmat_FHDI, 

						rbind_FHDI &rbind_Resp_FHDI, 

						rbind_FHDI &rbind_irmat_FHDI,

						

						rbind_FHDI &rbind_uox, 

						rbind_FHDI &rbind_mox, 

						List_FHDI  &List_ord,

						List_FHDI  &List_ocsg)

//Description=========================================

// perform

// Fully Efficient Fractional Imputation OR

// Fractional Hot Deck Imputation

// 

// Algorithm: FEFI of Dr Jae Kwang. Kim and FHDI of Dr Jong Ho. Im

//

// original R code: Dr. Im, J. and Dr. Kim, J. 

// c++ code: 		Dr. Cho, In-Ho 

// All rights reserved

// 

// updated: March 28, 2017

//----------------------------------------------------

//IN    : double y(nrow, ncol)= original data matrix with missing cells 

//IN    : double z(nrow, ncol)= categorized matrix of y. 0 for missing cell

//IN    : int    r(nrow, ncol) = index matrix of missing unit (0)/observed unit (1)  

//IN	: vector<string> jp_name  = name of table of joint probability

//IN	: vector<double> jp_prob  = joint probability 

//IN  	: string s_M = "FEFI" fully efficient fractional imputation 

// 					   "FHDI" Fractional Hot Deck Imputation  

//IN    : int i_M = number of donors used for FHDI

//OUT   : rbind_FHDI  rbind_ipmat_FEFI(4+ncol); //column size is 4+ncol (i.e., for R: ID, FID, WGT, FWGT, Variables)

//OUT   : rbind_FHDI  rbind_Resp_FEFI(ncol+1);  //separate response matrix  (i.e. for R: unit responses and Resp0)

//OUT   : rbind_FHDI  rbind_irmat_FEFI(5+ncol); //column size is 5+ncol (i.e. for R:ID, FID, OID, ORDER, FEFIW, CELL )

//OUT   : rbind_FHDI  rbind_ipmat_FHDI(4+ncol); //column size is 4+ncol

//OUT   : rbind_FHDI  rbind_Resp_FHDI(ncol+1);  //separate response matrix  

//OUT   : rbind_FHDI  rbind_irmat_FHDI(5+ncol); //column size is 5+ncol

//OUT   : rbind_FHDI  rbind_uox (ncol) //observed unique categorized matrix

//OUT   : rbind_FHDI  rbind_mox (ncol) //missing  unique categorized matrix

//OUT   : List_FHDI   List_ord(nrow) //but meaningful up to i_count_mox rows

//OUT   : List_FHDI   List_ocsg(nrow)//but meaningful up to i_count_mox rows

//====================================================

{

	//-----------------

	//random location using uniform distribution   

	//using Numerical Recipes of Press et al 2007. 

	//-----------------

	//Ran_FHDI myran(1); 	//not used for CRAN Compatibility

	//std::srand(123); //window version 
	
	//set.seed(123);// R package version, this should be done at R main 
	
	double d_myran = 0.0; 

	

	

	//-------------

	//column-wise sum of r matrix

	//-------------

	int* i_rn = new int[ncol];

	int i_temp = 0; 

	for(int i=0; i<ncol; i++) 

	{

		i_temp = 0; 

		for(int j=0; j<nrow; j++) i_temp += r[j][i]; 

		i_rn[i] = i_temp; 

	}		

	

	//-------------

	//sample weight (default is 1)

	//id array (default is row number)

	//-------------
	/*

	double* w = new double[nrow];

	int* id   = new int[nrow];

	for(int i=0; i<nrow; i++) 

	{

		w[i] = 1.0; 

		id[i] = i+1; //ACTUAL id

	}
	*/



	//--------------

	//locations of missing cells (ml) and observed cells (ol)

	//Note: unlike in Cell_Make..(), std vector is used here

	//--------------

	std::vector<int> ol; //Actual row number 

	std::vector<int> ml; //Actual row number 



	double d_temp=0.0; 

	for(int i_row=0; i_row<nrow; i_row++)

	{

		d_temp=1.0; 

		for(int i_col=0; i_col<ncol; i_col++)

		{

			if(z[i_row][i_col] == 0) {d_temp=0.0; break;} //found zero, i.e. missing cell

		}

		

		if(fabs_FHDI(d_temp) > 1e-15 ) //this row has no missing cells

		{ol.push_back(i_row + 1);} //actual number of the row having no missing cells

		

		if(fabs_FHDI(d_temp) < 1e-15) //this row has AT LEAST one missing cells

		{ml.push_back(i_row + 1);}  //actual number of the row having missing cells

	}

	const int i_size_ol = (int)ol.size(); 

	const int i_size_ml = (int)ml.size(); 

	if(i_size_ol ==0) {Rprintf("Error! no observed unit in FHDI_Extension. \n"); return 0; }

	if(i_size_ml ==0) {Rprintf("Error! no missing  unit in FHDI_Extension. \n"); return 0; }



	//--------------

	//Rows of observed RAW data 

	//Rows of missing  RAW data

	//--------------

	double** d_oy = New_dMatrix(i_size_ol, ncol);

	double** d_my = New_dMatrix(i_size_ml, ncol);

	for(int i=0; i<i_size_ol; i++) 

	{

		for(int j=0; j<ncol; j++) 

		{

			d_oy[i][j] = y[ol[i]-1][j]; //-1 for Actual loc

		}

	}

	for(int i=0; i<i_size_ml; i++) 

	{

		for(int j=0; j<ncol; j++) 

		{

			d_my[i][j] = y[ml[i]-1][j];	//-1 for Actual loc

		}

	}

	

	//--------------

	//Rows of observed data 

	//Rows of missing data

	//--------------

	double** d_ox = New_dMatrix(i_size_ol, ncol);

	double** d_mx = New_dMatrix(i_size_ml, ncol);

	for(int i=0; i<i_size_ol; i++) 

	{

		for(int j=0; j<ncol; j++) 

		{

			d_ox[i][j] = z[ol[i]-1][j]; //-1 for Actual loc

		}

	}

	for(int i=0; i<i_size_ml; i++) 

	{

		for(int j=0; j<ncol; j++) 

		{

			d_mx[i][j] = z[ml[i]-1][j];	//-1 for Actual loc

		}

	}

	

	//----------------

	//weights corresponding to missing/observed rows. 

	//select out weights at missing rows and observed rows

	//----------------

	double* w_ml = new double[i_size_ml]; //same as mw

	double* w_ol = new double[i_size_ol]; //same as ow

	for(int i=0; i<i_size_ml; i++) w_ml[i]  = w[ml[i] - 1] ; //-1 for actual loc

	for(int i=0; i<i_size_ol; i++) w_ol[i]  = w[ol[i] - 1] ; //-1 for actual loc

	

	//----------------

	//index corresponding to missing/observed rows. 

	//select out weights at missing rows and observed rows

	//----------------

	int* id_ml = new int[i_size_ml]; //same as mid

	int* id_ol = new int[i_size_ol]; //same as oid

	for(int i=0; i<i_size_ml; i++) id_ml[i]  = id[ml[i] - 1] ; //-1 for actual loc

	for(int i=0; i<i_size_ol; i++) id_ol[i]  = id[ol[i] - 1] ; //-1 for actual loc



	//-------------------------------

	//Step 1: generate uox and mox

	//-------------------------------

	//make UNIQUE patterns of z by cn

	//--------------

	//transform z into condensed string format

	//--------------

	//std::string cn[nrow]; //declaration of concatenated vector of z

	std::string *cn = new std::string[nrow]; //declaration of concatenated vector of z

	Trans(z, nrow, ncol, cn);

	

	//---------------

	//Rows of Condensed Strings with Observed cells

	//                          with Missing  cells

	//---------------

	//std::string s_ocn[i_size_ol];

	//std::string s_mcn[i_size_ml];

	std::string *s_ocn = new std::string[i_size_ol];

	std::string *s_mcn = new std::string[i_size_ml];	

	for(int i=0; i<i_size_ol; i++) s_ocn[i] = cn[ol[i]-1]; //-1 for actual row

	for(int i=0; i<i_size_ml; i++) s_mcn[i] = cn[ml[i]-1]; //-1 for actual row

	

	//std::string s_ocn_temp[i_size_ol]; //string vector of observed patterns only

	//std::string s_mcn_temp[i_size_ml]; //string vector of missing patterns only

	std::string *s_ocn_temp = new std::string[i_size_ol]; //string vector of observed patterns only

	std::string *s_mcn_temp = new std::string[i_size_ml]; //string vector of missing patterns only	

	for(int i=0; i<i_size_ol; i++) {s_ocn_temp[i] = s_ocn[i];} 

	for(int i=0; i<i_size_ml; i++) {s_mcn_temp[i] = s_mcn[i];} 

    //sort 		

	std::sort(s_ocn_temp, s_ocn_temp+i_size_ol); //knowing that s_ocn_temp[] has i_size_ol entities

	std::sort(s_mcn_temp, s_mcn_temp+i_size_ml); //knowing that s_mcn_temp[] has i_size_ml entities

	

	//------------

	//memorize observed patterns. Only unique patterns are stored  

	//------------

	double** uox = New_dMatrix(nrow, ncol);

	double** mox = New_dMatrix(nrow, ncol);

	

	int i_count_uox = 0; //total number of unique uox 

	std::string s_temp ; 

	for(int i=0; i<i_size_ol; i++)

	{

		s_temp = s_ocn_temp[i]; //get a string 

		for(int j=0; j<nrow; j++) //search all rows 

		{

			//----

			//below condition is needed for finding UNIQUE pattern

			//----

			//if(j==0 && s_temp == cn[j]) 

			//if(i==0 && s_temp == cn[j]) //with first string, find the same string in cn 

			if(i==0 && s_temp.compare(cn[j]) == 0) //0: equal string

			{

				for(int k=0; k<ncol; k++) 

				{uox[i_count_uox][k] = z[j][k]; } //store the found observed pattern

				i_count_uox++; 

				break; 

			}

			//if(j>0 && s_temp == cn[j] && s_temp != cn[j-1])

			//if(i>0 && s_temp == cn[j] && s_temp != s_ocn_temp[i-1]) //find UNIQUE matching 

			if(i>0 && s_temp.compare(cn[j]) == 0 && s_temp.compare(s_ocn_temp[i-1]) != 0) 

			{

				for(int k=0; k<ncol; k++) 

				{uox[i_count_uox][k] = z[j][k]; } //store the found observed pattern				

				i_count_uox++; 

				break; 

			}

		}

	}

	//Now, i_count_uox means the total number of unique observed patterns



	//------------

	//memorize missing patterns 

	//------------

	int i_count_mox = 0; //total number of unique mox 

	 

	for(int i=0; i<i_size_ml; i++)

	{

		s_temp = s_mcn_temp[i]; //get a string 

		for(int j=0; j<nrow; j++) //search all rows 

		{

			//----

			//below condition is needed for finding unique pattern

			//----

			//if(j==0 && s_temp == cn[j]) 

			//if(i==0 && s_temp == cn[j]) //with first string, find matching string in cn

			if(i==0 && s_temp.compare(cn[j]) == 0 ) //0: equal string 

			{

				for(int k=0; k<ncol; k++) 

				{mox[i_count_mox][k] = z[j][k]; } //store the found missing pattern

				i_count_mox++; 

				break; 

			}

			//if(j>0 && s_temp == cn[j] && s_temp != cn[j-1])

			//if(i>0 && s_temp == cn[j] && s_temp != s_mcn_temp[i-1]) //find UNIQUE matching string

			if(i>0 && s_temp.compare(cn[j]) == 0 && s_temp.compare(s_mcn_temp[i-1]) != 0) //0: equal

			{

				for(int k=0; k<ncol; k++) 

				{mox[i_count_mox][k] = z[j][k]; } //store the found missing pattern				

				i_count_mox++; 

				break;

			}

		}

	}

	//Now, i_count_mox means the total number of unique missing patterns

	

	//----------------

	//additional check for unique observed and missing patterns

	//----------------

	//observed patterns//

	d_temp = 0.0; 

	double** uox_final = New_dMatrix(nrow, ncol); 

	for(int j=0; j<ncol; j++) {uox_final[0][j] = uox[0][j]; } //first row initialization

	int i_count_uox_final = 1; //starting from the second row



	for(int i=1; i<i_count_uox; i++) //starting from the second one

	{

		d_temp = 0.0; //initialize 

		for(int j=0; j<ncol; j++) {d_temp += fabs_FHDI(uox[i][j] - uox[i-1][j]) ;} //difference of adjacent rows

		

		if(d_temp > 1e-3) //adjacent rows are NOT the same each other

		{

			for(int j=0; j<ncol; j++) {uox_final[i_count_uox_final][j] = uox[i][j];} 

			i_count_uox_final++; 

		}

	}

	i_count_uox = i_count_uox_final; //replace with the accurate value

	//store the final matrix 

	for(int i=0; i<i_count_uox; i++) 

	{ 

		for(int j =0; j<ncol; j++) 

			uox[i][j] = uox_final[i][j]; 

	}

	Del_dMatrix(uox_final, nrow, ncol);

	

	//--------------------------

	//missing patterns//

	//--------------------------

	double** mox_final = New_dMatrix(nrow, ncol); 

	for(int j=0; j<ncol; j++) {mox_final[0][j] = mox[0][j]; } //first row initialization

	int i_count_mox_final = 1; //starting from the second row



	for(int i=1; i<i_count_mox; i++) //starting from the second one

	{

		d_temp = 0.0; //initialize

		for(int j=0; j<ncol; j++) {d_temp += fabs_FHDI(mox[i][j] - mox[i-1][j]) ;} //difference of adjacent rows

		

		if(d_temp > 1e-3) //adjacent rows are NOT the same each other

		{

			for(int j=0; j<ncol; j++) {mox_final[i_count_mox_final][j] = mox[i][j];} 

			i_count_mox_final++; 

		}

	}

	i_count_mox = i_count_mox_final; //replace with the accurate value



	//store the final matrix	

	for(int i=0; i<i_count_mox; i++) 

	{ 

		for(int j =0; j<ncol; j++) 

			mox[i][j] = mox_final[i][j]; 

	}

	Del_dMatrix(mox_final, nrow, ncol);

	

    //!!!!! now uox and mox have the UNIQUE observed and missing patterns

	//!!!!! i_count_mox and _uox have the final number of meaningful rows of mox and uox, respectively

	const int nrm = i_count_mox; 

	const int nru = i_count_uox; 

	

	//-------------------------------------------

	//-------------------------------------------

	//Step 2: Impute missing cells

	//        using all possible donors per missing pattern

	//-------------------------------------------

	//-------------------------------------------

	int* i_temp_x = new int[ncol];

	int i_sum_x = 0;

	

	std::vector<int> v_mxl ; //a row's columns having observed cells  

	rbind_FHDI rbind_icell(ncol); //all possible donor cells 

	

	std::vector<int> v_cn_z_i; 

	//int* zid = NULL;

	//int i_size_zid=0; 

	//int i_loc=0;	

	int* i_srst = new int[nru];

	std::vector<int> loc_srst_nl; 

	double* d_temp_cn = new double[ncol]; 



	//List_FHDI List_ord(nrm); //order records used for variance estimation

	//List_FHDI List_ocsg(nrm); //order records used for variance estimation

	

	//--------------------------------

	//--------------------------------

	//--------------------------------

	//Main Loop for FEFI and FHDI

	//--------------------------------

	//--------------------------------

	//--------------------------------

	rbind_FHDI rbind_imat_FEFI(7+2*ncol); //large storage that will accumulate fmat from FEFI  

	rbind_FHDI rbind_imat_FHDI(7+2*ncol); //large storage that will accumulate fmat from FHDI

	

	

	for(int i=0; i<nrm; i++)

	{

		//get current row of missing cell 

		for(int j=0; j<ncol; j++) i_temp_x[j] =  (int)mox[i][j]; 

		i_sum_x = sum_FHDI(i_temp_x, ncol);





		//-------

		//re-initialization for this missing row 

		//-------

		rbind_icell.initialize(ncol); 

		

		//----------------------

		//Condition 1: this row's cells are all missing

		//-----------------------

		if(i_sum_x == 0)

		{

			v_mxl.clear(); //no missing cells  

			rbind_icell.bind_blocks(i_count_uox, ncol, uox); //fine due to row-based copy

		}			

		

		//----------------------

		//Condition 2: this row's cells are partly missing

		//-----------------------

		int nl = 0; 

		if(i_sum_x > 0)

		{

			//------

			//number of observed cells on this row

			//------

			nl = 0; 

			v_mxl.clear(); 

			for(int j=0; j<ncol; j++) 

			{

				if(mox[i][j]>0) 

				{

					nl++; 

					v_mxl.push_back(j+1); //Actual non-missing cell location 

				}

			}

			

			

			//-------

			//indicator matrix that matches the donors

			//srst: row-wise sum of the indicator matrix 

			//-------

			loc_srst_nl.clear(); //re-initialize

			Fill_iVector(i_srst, nru, 0); //re-initialize 

				

			for(int j=0; j<nru; j++)

			{

				int i_sum_crst = 0; 

				for(int k=0; k<ncol; k++)

				{

					//Note: in below check, mox is fixed at ith row 

					if(fabs_FHDI(mox[i][k] - uox[j][k])<1e-3) //part of missing cell = obserbed cell 

					{

						i_sum_crst++; // increment if a cell of missing row = obs. cell 

					}

				}

				//---

				//store how many cells of missing row match those of observed row

				//---

				i_srst[j] = i_sum_crst; 

				if(i_sum_crst==nl) loc_srst_nl.push_back(j+1); //Actual location 				

			}

			

			//-----

			//total matching rows

			//-----

			const int i_size_loc_srst_nl = (int)loc_srst_nl.size(); 

			if(i_size_loc_srst_nl == 0) //error case

			{Rprintf("Error! there is no matched cell! \n"); return 0;}

			

			if(i_size_loc_srst_nl > 0) 

			{

				double* d_temp_srst = new double[ncol];

				for(int j=0; j<i_size_loc_srst_nl; j++)

				{

					for(int k=0; k<ncol; k++) 

					{	d_temp_srst[k] = uox[loc_srst_nl[j]-1][k]; }//-1 for actual loc

					

					rbind_icell.append_block(d_temp_srst); //ncol is the same 

				}

				delete[] d_temp_srst; 

			}

		}



		//----------------------------

		//step 3: Assign donors to missing cell

		//----------------------------

		const int nic = rbind_icell.size_row(); //get the number of total rows

		//std::string s_icn[nic];

		std::string * s_icn = new std::string[nic];

		double* d_temp_icell = new double[ncol];

        std::string s_icn_temp; 

		for(int j=0; j<nic; j++)

		{

			for(int k=0; k<ncol; k++) d_temp_icell[k] = rbind_icell(j,k); //get jth row

			Trans1(d_temp_icell, ncol, s_icn_temp); //transform one row

			s_icn[j] = s_icn_temp; 

		}

		delete[] d_temp_icell;

		

		//------------------

		//search locations where icn = name of jp_name

		//------------------

		const int i_size_jp_name = (int)jp_name.size(); 

		std::vector<double>      v_cp;  //selected joint probability 

		std::vector<std::string> v_ncp; //names of the selected joint probability

		v_cp.clear(); 

		v_ncp.clear(); 

		

		for(int j=0; j<nic; j++)

		{

			s_temp = s_icn[j]; //one donor 

			for(int k=0; k<i_size_jp_name; k++) //search all names of jp

			{

				if(s_temp.compare(jp_name[k]) == 0) //0 means the same string

				{

					v_cp.push_back(jp_prob[k]); //store the joint probability 

					v_ncp.push_back(jp_name[k]); //store name

					break; //stop searching after finding the first match 

				}

			}

		}

		const int i_size_v_cp = (int)v_cp.size();

		double d_sum_v_cp = 0.0; 

		for(int j=0; j<i_size_v_cp; j++) d_sum_v_cp += v_cp[j]; 

		if(d_sum_v_cp != 0) 

		{

			for(int j=0; j<i_size_v_cp; j++) v_cp[j] = v_cp[j]/d_sum_v_cp; 

		}



		//-----------

		//transform current missing row to string for step 3

		//-----------

		for(int j=0; j<ncol; j++) d_temp_cn[j] = mox[i][j]; 

		Trans1(d_temp_cn, ncol, s_temp);

		v_cn_z_i.clear(); //re-initialize 

		which(cn, nrow, s_temp, v_cn_z_i); //Note: Actual location is returned

		int i_size_v_cn_z_i = (int)v_cn_z_i.size(); //number of locations in cn having s_temp

		const int mg = i_size_v_cn_z_i; //Note: loc2 =  i_size_v_cn_z_i

		

		

		//---------------------

		//select out all cells that have s_icn[1:nic] from cn

		//---------------------

		std::vector<int> v_obsg0; v_obsg0.clear(); 

		for(int j=0; j<nic; j++)

		{

			s_temp = s_icn[j]; 

			for(int k=0; k<nrow; k++)

			{

				if(s_temp.compare(cn[k]) == 0) //0=equal string

				{ v_obsg0.push_back(k+1); }//ACTUAL location stored. No exit 

			}

		}

		const int i_size_v_obsg0 = (int)v_obsg0.size();

		

		//---------------

		//sort the found cells

		//---------------

		int* i_obsg0_sorted = new int[i_size_v_obsg0]; 

		for(int k=0; k<i_size_v_obsg0; k++) i_obsg0_sorted[k] = v_obsg0[k];

		std::sort(i_obsg0_sorted, i_obsg0_sorted+i_size_v_obsg0); 

		for(int k=0; k<i_size_v_obsg0; k++) v_obsg0[k] = i_obsg0_sorted[k];

		delete[] i_obsg0_sorted;

		



		//------------------

		//half-ascending and -descending ordering 

		//------------------

		std::vector<int> v_obsg; //half-asc and desc obsg0

		

		int* i_ym_return = new int[i_size_v_obsg0]; //half-asc and desc 

		yorder(y, nrow, ncol, 

               d_temp_cn,

			   v_obsg0, i_ym_return);



	    v_obsg.clear(); 

		for(int j=0; j<i_size_v_obsg0; j++) v_obsg.push_back(i_ym_return[j]);

		//below is temporary for wrong yorder()

		//for(int j=0; j<i_size_v_obsg0; j++) v_obsg.push_back(v_obsg0[j]); 

		

		const int i_size_v_obsg = (int)v_obsg.size(); 

		delete[] i_ym_return; 

		

		

		//testout

		//RPrint("============= after yorder() ================");

		//RPrint("v_obsg0 :"); RPrint(v_obsg0);

		//RPrint("v_obsg :");  RPrint(v_obsg);

		

		//-------------

		// find positions of matches between obsg in obsg0

		// then Store them into List 

		//-------------

		std::vector<int> v_rbsg; v_rbsg.clear(); 

		match_FHDI(v_obsg, v_obsg0, v_rbsg); //get loc stored in v_rbsg

		const int i_size_v_rbsg = (int)v_rbsg.size(); 

		//store rbsg

		double* d_temp_rbsg = new double[i_size_v_rbsg]; 

		for(int k=0; k<i_size_v_rbsg; k++) d_temp_rbsg[k] = v_rbsg[k]; 

		List_ord.put_block(i, i_size_v_rbsg, d_temp_rbsg); //put into storage as ith row

		delete[] d_temp_rbsg; 

		//store obsg

		double* d_temp_obsg = new double[i_size_v_obsg]; 

		for(int k=0; k<i_size_v_obsg; k++) d_temp_obsg[k] = v_obsg[k]; 

		List_ocsg.put_block(i, i_size_v_obsg, d_temp_obsg); //put into storage as ith row

		delete[] d_temp_obsg; 		

		

		const int ng = i_size_v_obsg;

		//------------------------------

		//------------------------------

		//Compute Fractional Weights (fwij)

		//Fractional weights for FEFI representing sampling w

		//------------------------------

		//------------------------------

		//std::string cn_obsg[i_size_v_obsg]; //cn at locations of obsg

		std::string * cn_obsg = new std::string[i_size_v_obsg]; //cn at locations of obsg

		for(int k=0; k<i_size_v_obsg; k++) 

		{	cn_obsg[k] = cn[v_obsg[k]-1];   }//-1 for actual location 

		

		

		//-----

		//make a table of cn at obsg locations

		//------

		std::vector<std::string> v_table_name_cn_obsg;  

		std::vector<int>         v_table_count_cn_obsg;

		

		table_cpp(cn_obsg, i_size_v_obsg, 

		          v_table_name_cn_obsg, v_table_count_cn_obsg); 

		//const int i_size_table_cn_obsg = (int)v_table_count_cn_obsg.size(); 

		

		//------

		//get joint probability of the selected donor 

		//------

		std::vector<int> v_cn_obsg_ncp; //positions of cn_obsg in ncp 

		match_FHDI(cn_obsg, i_size_v_obsg, v_ncp,  

		           v_cn_obsg_ncp); //Note: Actual locations are returned 

		const int i_size_v_cn_obsg_ncp = (int)v_cn_obsg_ncp.size(); 

		

		//------

		//calculate fractional weights

		//------

		double* fwij = new double[i_size_v_cn_obsg_ncp]; //fractional weights 

		double* d_obsp = new double[i_size_v_cn_obsg_ncp]; //joint prob of selected donors

		int*    i_obsn = new int[i_size_v_cn_obsg_ncp]; //counts of the selected donors

		for(int k=0; k<i_size_v_cn_obsg_ncp; k++)

		{

			d_obsp[k] = v_cp[v_cn_obsg_ncp[k]-1];// -1 for actual location 

			i_obsn[k] = v_table_count_cn_obsg[v_cn_obsg_ncp[k]-1];// -1 for actual location  

		

			fwij[k] = 1.0; //default for error case  

			if(i_obsn[k] != 0) fwij[k] = d_obsp[k]/i_obsn[k]; 

			if(i_obsn[k] == 0) Rprintf("Error! zero count in obsn!"); 

		}

		//testout

		//RPrint("fwij[] :"); RPrint(fwij, i_size_v_cn_obsg_ncp);



		

		//----------------------

		//FEFI Imputation

		//  Algorithm: impute by using all possible donors

		//  final outcome is "fmat" in which each column means that

		//  col1: id

		//  col2: fid, i.e., id of imputed value

		//  col3: sampling weight

		//  col4: fractional weights 

		//  col5: imputed original data (matrix with column of ncol) 

		//  col6: imputed category data (matrix with column of ncol)

		//  col7: 1:ng 

		//  col8: = col2  (for consistency with FHDI results)

		//  col9: = col3  (for consistency with FHDI results)

		//----------------------

		std::vector<int> v_obsg_times_mg; v_obsg_times_mg.clear(); 

		if(s_M.compare("FEFI") == 0) //0=equal string

		{

			double** fmat_FEFI = New_dMatrix(ng*mg, 7+2*ncol); //7columns and two blocks of ncol 

			

			Fully_Efficient_Fractional_Imputation(ng, mg, 

					v_obsg, v_mxl, 

					y, z, nrow, ncol,

					v_cn_z_i, fwij, i_size_v_cn_obsg_ncp,

					w, id,

					fmat_FEFI);



			//testout

			//RPrint("in ==== M=FEFI =after making fmat_FEFI[][]====");

			//RPrint("fmat_FEFI : "); RPrint(fmat_FEFI, ng*mg, 7+2*ncol);

			

			//------------------

			//Append fmat_FEFI onto global storage imat

			//------------------

			rbind_imat_FEFI.bind_blocks(ng*mg, 7+2*ncol, fmat_FEFI);



			//-----------------------

			//local deallocation

			//-----------------------

			Del_dMatrix(fmat_FEFI, ng*mg, 7+2*ncol); 		

		}





		//------------------------------------

		//FHDI

		//Fractional Hot Deck Imputation

		//------------------------------------

		//if(s_M.compare("FHDI") == 0) //0= equal string



		const int i_mxl = (int)v_mxl.size();

		

		//prepare return matrix. Note the different row size from fmat of FEFI 

		int i_row_fmat_FHDI = i_M*mg; //default row size of return matrix of FHDI

		if(i_size_v_obsg <= i_M) i_row_fmat_FHDI=i_size_v_obsg*mg;  //if donors are less than i_M

		

		if(s_M.compare("FHDI") == 0) //0= equal string

		{

			double** fmat_FHDI= New_dMatrix(i_row_fmat_FHDI, 7+2*ncol); //return matrix from FHDI

			//below random number between 0 and 1 should use appropriate library depending upon version	

		    //d_myran = static_cast<double>(std::rand())/static_cast<double>(RAND_MAX); //Window ver

			d_myran = Rf_runif(0.0, 1.0); //R package version 
			
			//testout
			
			//Rprintf("d_myran: "); Rprintf("%g ", d_myran); 

			Fractional_Hot_Deck_Imputation(i,

					ng, List_ocsg, ncol,

					mox, y, nrow, i_M, 

					mg, z, i_mxl, 

					v_cn_z_i, v_mxl,

					v_obsg,

					fwij, i_size_v_cn_obsg_ncp,

					d_obsp, i_obsn, 

					d_myran,

					w, id, 

					fmat_FHDI); 

					

			//------------------

			//Append fmat_FHDI onto global storage imat

			//------------------

			rbind_imat_FHDI.bind_blocks(i_row_fmat_FHDI, 7+2*ncol, fmat_FHDI);		



			//-----------------------

			//local deallocation

			//-----------------------

			Del_dMatrix(fmat_FHDI, i_row_fmat_FHDI, 7+2*ncol);

		}

		

		//-----------------------

		//local deallocation

		//-----------------------

		delete[] s_icn; 

		delete[] cn_obsg; 

		delete[] fwij;

		delete[] d_obsp;

		delete[] i_obsn; 

		

	} //end of Main loop for all rows of missing patterns 

	

	

	//---------------

	//---------------

	//Step 4: construct output results

	//---------------

	//------------------------------------------------------

	//ipmat  = final imputation results

	//     	col1: ID 	= unit index

	//		col2: FID 	= ID of fractionally imputed value

	// 		col3: WGT 	= weight 

	//		col4: FWGT	= Frational weight

	//		col5: Variables 

	//		col6: Responses

	//irmat  = imputation results related to the categorized matrix 

	//     	col1: ID 	= unit index

	//		col2: FID 	= ID of fractionally imputed value

	//		col3: OID	= original rank of the imputed value

	//		col4: ORDER = SN(selected donor)

	//		col5: FEFIW	= Fefi weights 

	//		col6: CELL	= cells 

	//----------------------------------------------------

	//FEFI                           FEFI //

	//get ipmat, Resp (separately), irmat from FEFI results 

	//rbind_FHDI  rbind_ipmat_FEFI(4+ncol); //column size is 4+ncol

	//rbind_FHDI  rbind_Resp_FEFI(ncol+1);  //separate response matrix  

	//rbind_FHDI  rbind_irmat_FEFI(5+ncol); //column size is 5+ncol

	if(s_M.compare("FEFI") == 0) //0= equal string

	{

		Results_Fully_Efficient_Fractional_Imputation(i_size_ol, 

						ncol,  nrow,

		                id_ol, w_ol, d_oy, d_ox, 

						rbind_imat_FEFI, r, 

						

						rbind_ipmat_FEFI, rbind_Resp_FEFI, rbind_irmat_FEFI); 	

		//testout

		/*

		RPrint("after Results_... rbind_ipmat_FEFI after binding :");

		rbind_ipmat_FEFI.print_rbind_FHDI(); 

		RPrint("after Results_... rbind_Resp_FEFI after binding :");

		rbind_Resp_FEFI.print_rbind_FHDI(); 						

		RPrint("after Results_... rbind_irmat_FEFI after binding :");

		rbind_irmat_FEFI.print_rbind_FHDI(); 

		*/

	}

	

	//FHDI ------------------------- FHDI //

	//get ipmat, Resp (separately), irmat from FHDI results 

	//rbind_FHDI  rbind_ipmat_FHDI(4+ncol); //column size is 4+ncol

	//rbind_FHDI  rbind_Resp_FHDI(ncol+1);  //separate response matrix  

	//rbind_FHDI  rbind_irmat_FHDI(5+ncol); //column size is 5+ncol



	if(s_M.compare("FHDI") == 0) //0= equal string

	{	

		Results_Fractional_Hot_Deck_Imputation(i_size_ol, 

						ncol,  nrow,

		                id_ol, w_ol, d_oy, d_ox, 

						rbind_imat_FHDI, r, 

						

						rbind_ipmat_FHDI, rbind_Resp_FHDI, rbind_irmat_FHDI); 	

		//testout

		/*

		RPrint("after Results_... rbind_ipmat_FHDI after binding :");

		rbind_ipmat_FHDI.print_rbind_FHDI(); 

		RPrint("after Results_... rbind_Resp_FHDI after binding :");

		rbind_Resp_FHDI.print_rbind_FHDI(); 						

		RPrint("after Results_... rbind_irmat_FHDI after binding :");

		rbind_irmat_FHDI.print_rbind_FHDI(); 	

		*/

	}



	//------

	//prep returns of other matrices

	//------

	rbind_uox.bind_blocks(i_count_uox, ncol, uox);

	rbind_mox.bind_blocks(i_count_mox, ncol, mox);



	//testout

	Rprintf(" ========= FHDI has successfully finished! \n");

	

	

	//----------------

	//Deallocation

	//----------------

	delete[] cn;

	delete[] s_ocn; 

	delete[] s_mcn; 

	delete[] s_ocn_temp; 

	delete[] s_mcn_temp; 

	

	delete[] i_rn; 

	//lete[] w; 

	//lete[] id; 

	Del_dMatrix(d_oy, i_size_ol, ncol);

	Del_dMatrix(d_my, i_size_ml, ncol);

	Del_dMatrix(d_ox, i_size_ol, ncol);

	Del_dMatrix(d_mx, i_size_ml, ncol);

	

	delete[] w_ml; 

	delete[] w_ol; 

	delete[] id_ml; 

	delete[] id_ol; 



	Del_dMatrix(uox, nrow, ncol);

	Del_dMatrix(mox, nrow, ncol);

	

	delete[] i_temp_x; 

	delete[] i_srst;

	delete[] d_temp_cn; 

	

	

	return 1;



}





void yorder(double** y, const int nrow, const int ncol, 

            double* mox_1, 

			std::vector<int> v_loc,

			int* i_ym_return)

//Description=========================================

// order the donors in  

// half-ascending and half-descending manner

// 

//

// original R code: Dr. Im, J. and Dr. Kim, J. 

// c++ code: 		Dr. Cho, I. 

// All rights reserved

// 

// updated: Nov 07, 2016

//----------------------------------------------------

//IN    : double y(nrow, ncol)= original data matrix

//IN    : double mox_1(ncol)= one row of unique missing patterns

//IN	: vector<int> v_loc  = ACTUAL locations of donors 

//OUT   : int i_ym_return(i_size_v_loc)  = index in half-ascending and -descending order

//====================================================

{

	//----------------

	//get ready original data at columns of zero mox_i

	//----------------

	std::vector<int> rloc; //ACTUAL location of zero in mox_1

	for(int i=0; i<ncol; i++)

	{ if(mox_1[i] == 0) rloc.push_back(i+1); } 

	const int i_size_rloc = (int)rloc.size(); 

	

	double** yv = New_dMatrix(nrow, i_size_rloc); 

	

	for(int i=0; i<nrow; i++)

	{

		for(int j=0; j< i_size_rloc; j++)

		{

			yv[i][j] = y[i][rloc[j]-1]; //-1 for actual location 	

		}

	}

	//testout

	//RPrint(" in yorder() yv:"); RPrint(yv, nrow, i_size_rloc);

	

	//---------------------

	//extract donors at the known locations only

	//---------------------

	const int i_size_v_loc = (int)v_loc.size(); 

	double** yl = New_dMatrix(i_size_v_loc, i_size_rloc); 

	for(int i=0; i<i_size_v_loc; i++)

	{

		for(int j=0; j<i_size_rloc; j++)

		{

			yl[i][j] = yv[v_loc[i]-1][j]; //-1 for actual location 

		}

	}

	//testout

	//RPrint(" in yorder() yl:"); RPrint(yl, i_size_v_loc, i_size_rloc);

	

	//----------------------

	//column-wise mean calculation

	//----------------------

	double d_sum=0.0; 

	double* my = new double[i_size_rloc]; //mean of each column

	for(int j=0; j<i_size_rloc; j++)

	{

		d_sum = 0.0; 

		for(int i=0; i<i_size_v_loc; i++)	

		{

			d_sum += yl[i][j] ;

		}

		my[j] = d_sum/i_size_v_loc; 

	}

	

	//----------------------

	//(1) combine loc & yl       -> ym

	//(2) ym(without 1st column) -> dym  

	//----------------------

	double** ym = New_dMatrix(i_size_v_loc, i_size_rloc+1); 

	double** ym_sorted = New_dMatrix(i_size_v_loc, i_size_rloc+1); 

	for(int i=0; i<i_size_v_loc; i++)

	{

		ym[i][0] = v_loc[i]; //first column is loc (Actual loc)

		for(int j=0; j<i_size_rloc; j++) ym[i][j+1] = yl[i][j]; 

	}

	//testout

	//RPrint(" in yorder() ym:"); RPrint(ym, i_size_v_loc, i_size_rloc+1);

	//------------

	//get ready return 

	//------------

	for(int j=0; j<i_size_v_loc; j++) //default

	{

		i_ym_return[j] = (int)ym[j][0]; //default is unsorted locations   

	}



	

	//-------

	//covariance matrix of yl

	//-------

	double** VM = New_dMatrix(i_size_rloc, i_size_rloc); 

	double** VM_backup = New_dMatrix(i_size_rloc, i_size_rloc); //temp

	double** dif = New_dMatrix(i_size_v_loc, i_size_rloc); //yl - mean

	double** VM_inv = New_dMatrix(i_size_rloc, i_size_rloc); //inverse of VM

	double** dif_T = New_dMatrix(i_size_rloc, i_size_v_loc); //transpose of dif

	double** mat_temp = New_dMatrix(i_size_v_loc,i_size_v_loc); //temporary matrix

	double*  score = new double[i_size_v_loc]; 

	if(i_size_v_loc > 1)

	{

		//----------

		//"Estimated covariance" of yl by column-to-column method

		//----------

		cov_FHDI(yl, i_size_v_loc, i_size_rloc, VM); 

		

		//----------

		//column-wise difference between yl[][] - mean(yl)

		//----------

		for(int j=0; j<i_size_rloc; j++) //each column

		{

			for(int k=0; k<i_size_v_loc; k++) 

			{	dif[k][j] = yl[k][j] - my[j]; }

		}

		

		//----------

		//matrix multiplication among dif * inverse(VM) * dif^T

		//the "score" is similar to the Mahalanobis distance (MD) function

		//  MD measures the number of stdev from a point P to the mean of distribution Deallocation

		//  along each principal component axis. 

		//  Unitless and scale-invariant and taking into account the data's correlations.

		//  MD corresponds to standard Euclidean distance in the transformed space. 

		//----------

		Copy_dMatrix(VM, i_size_rloc, i_size_rloc, VM_backup); 

		bool b_success_VM = Inverse_dMatrix_FHDI(VM_backup, i_size_rloc, VM_inv); //backup is to avoid pivotting of VM

		//if(!b_success_VM){ Rprintf("CAUTION! inverse matrix may be incorrect!");}

		if (!b_success_VM) {} // Updated on Aug 19, 2020 by Yicheng Yang, this error message is inside main loop, which prints so many times!!!
		
		for(int j=0; j<i_size_v_loc; j++)

		{  for(int k=0; k<i_size_rloc; k++) dif_T[k][j] = dif[j][k]; }

	    dMatrix_Mul_AtBA(dif_T,  i_size_rloc, i_size_v_loc, 

 		                 VM_inv, mat_temp); 

		//diagonal terms are score

		for(int j=0; j<i_size_v_loc; j++) score[j] = mat_temp[j][j]; 

                      

		//------------

		//order the score array

		//------------

		int* i_return = new int[i_size_v_loc]; //order of score actual loc

		order_FHDI(score, i_size_v_loc, i_return) ;

		

		//-------------

		//sorted ym

		//-------------

		int i_loc_ym = 0;

		for(int j=0; j<i_size_v_loc; j++)

		{

			i_loc_ym = i_return[j]; //actual location 

			for(int k=0; k<i_size_rloc+1; k++)

			{

				ym_sorted[j][k] = ym[i_loc_ym-1][k]; //-1 for actual loc

			}

		}

		

		//-------------

		//half ascending ym's first column

		//-------------

		int* i_ym_ascending  = new int[i_size_v_loc]; 

		int* i_ym_descending = new int[i_size_v_loc]; 

		for(int j=0; j<i_size_v_loc; j++)

		{

			i_ym_ascending[j]  = (int)ym_sorted[j][0];

			i_ym_descending[j] = (int)ym_sorted[i_size_v_loc-1 -j][0];

		}

		

		//------------

		//get ready return 

		//------------

		int i_temp = 0; 

		for(int j=0; j<i_size_v_loc; j=j+2) //ascending

		{

			i_ym_return[j]   = i_ym_ascending[i_temp];  

			//below condition is necessary when odd sized vector 

			if(j<=(i_size_v_loc-2)) i_ym_return[j+1] = i_ym_descending[i_temp];

			i_temp++; 

		}



		//-----

		//local deallocation

		//-----

		delete[] i_return; 

		delete[] i_ym_ascending;

		delete[] i_ym_descending;

	}	



	

	//----------------------

	//Deallocation

	//----------------------

	Del_dMatrix(yv, nrow, i_size_rloc); 

	Del_dMatrix(yl, i_size_v_loc, i_size_rloc);

	delete[] my; 

	Del_dMatrix(ym, i_size_v_loc, i_size_rloc+1);

	Del_dMatrix(ym_sorted, i_size_v_loc, i_size_rloc+1);

	Del_dMatrix(VM, i_size_rloc, i_size_rloc);

	Del_dMatrix(VM_backup, i_size_rloc, i_size_rloc);

	Del_dMatrix(dif, i_size_v_loc, i_size_rloc);

	Del_dMatrix(VM_inv, i_size_rloc, i_size_rloc); //inverse of VM

	Del_dMatrix(dif_T, i_size_rloc, i_size_v_loc); //transpose of dif	

	Del_dMatrix(mat_temp, i_size_v_loc,i_size_v_loc);

	delete[] score; 

	

	return; 

}		



} //end of namespace


 //Fn===========================================================================

 //FHDI_Extension_Bigp_cpp.cc-----------------------------------------------------------------------------

 //Fn===========================================================================

namespace FHDI {

	bool FHDI_Extension_Bigp_cpp(double** y, double** z, int** r,

		const int nrow, const int ncol, const int i_option_collapsing,

		std::vector<std::string> jp_name,

		std::vector<double> 	 jp_prob,

		std::string s_M, const int i_M, double* w, int* id, int ** codes,



		rbind_FHDI &rbind_ipmat_FEFI,

		rbind_FHDI &rbind_Resp_FEFI,

		rbind_FHDI &rbind_irmat_FEFI,



		rbind_FHDI &rbind_ipmat_FHDI,

		rbind_FHDI &rbind_Resp_FHDI,

		rbind_FHDI &rbind_irmat_FHDI,



		rbind_FHDI &rbind_uox,

		rbind_FHDI &rbind_mox,

		List_FHDI  &List_ord,

		List_FHDI  &List_ocsg)

		//Description=========================================

		// perform

		// Fully Efficient Fractional Imputation OR

		// Fractional Hot Deck Imputation

		// 

		// Algorithm: FEFI of Dr Jae Kwang. Kim and FHDI of Dr Jong Ho. Im

		//

		// original R code: Dr. Im, J. and Dr. Kim, J. 

		// c++ code: 		Dr. Cho, In-Ho and Yicheng Yang

		// All rights reserved

		// 

		// updated: Feb 28, 2020

		//----------------------------------------------------

		//IN    : double y(nrow, ncol)= original data matrix with missing cells 

		//IN    : double z(nrow, ncol)= categorized matrix of y. 0 for missing cell

		//IN    : int    r(nrow, ncol) = index matrix of missing unit (0)/observed unit (1)  

		//IN	: vector<string> jp_name  = name of table of joint probability

		//IN	: vector<double> jp_prob  = joint probability 

		//IN  	: string s_M = "FEFI" fully efficient fractional imputation 

		// 					   "FHDI" Fractional Hot Deck Imputation  

		//IN    : int i_M = number of donors used for FHDI

		//IN    : int i_option_collapsing = choice of big-p algorithm 

		//                            0= no big-p algorithms

		//                           !0= perform big-p algorithms

		//IN   : int codes(nrow, i_option_collapsing); // storage to record most correlated variables of mox

		//OUT   : rbind_FHDI  rbind_ipmat_FEFI(4+ncol); //column size is 4+ncol (i.e., for R: ID, FID, WGT, FWGT, Variables)

		//OUT   : rbind_FHDI  rbind_Resp_FEFI(ncol+1);  //separate response matrix  (i.e. for R: unit responses and Resp0)

		//OUT   : rbind_FHDI  rbind_irmat_FEFI(5+ncol); //column size is 5+ncol (i.e. for R:ID, FID, OID, ORDER, FEFIW, CELL )

		//OUT   : rbind_FHDI  rbind_ipmat_FHDI(4+ncol); //column size is 4+ncol

		//OUT   : rbind_FHDI  rbind_Resp_FHDI(ncol+1);  //separate response matrix  

		//OUT   : rbind_FHDI  rbind_irmat_FHDI(5+ncol); //column size is 5+ncol

		//OUT   : rbind_FHDI  rbind_uox (ncol) //observed unique categorized matrix

		//OUT   : rbind_FHDI  rbind_mox (ncol) //missing  unique categorized matrix

		//OUT   : List_FHDI   List_ord(nrow) //but meaningful up to i_count_mox rows

		//OUT   : List_FHDI   List_ocsg(nrow)//but meaningful up to i_count_mox rows

		//====================================================

	{

		//-----------------

		//random location using uniform distribution   

		//using Numerical Recipes of Press et al 2007. 

		//-----------------

		//Ran_FHDI myran(1); 	//not used for CRAN Compatibility

		//std::srand(123); //window version 


		//set.seed(123);// R package version, this should be done at R main 

		double d_myran = 0.0;





		//-------------

		//column-wise sum of r matrix

		//-------------

		int* i_rn = new int[ncol];

		int i_temp = 0;

		for (int i = 0; i < ncol; i++)

		{

			i_temp = 0;

			for (int j = 0; j < nrow; j++) i_temp += r[j][i];

			i_rn[i] = i_temp;

		}



		//-------------

		//sample weight (default is 1)

		//id array (default is row number)

		//-------------
		/*

		double* w = new double[nrow];

		int* id   = new int[nrow];

		for(int i=0; i<nrow; i++)

		{

		w[i] = 1.0;

		id[i] = i+1; //ACTUAL id

		}
		*/



		//--------------

		//locations of missing cells (ml) and observed cells (ol)

		//Note: unlike in Cell_Make..(), std vector is used here

		//--------------

		std::vector<int> ol; //Actual row number 

		std::vector<int> ml; //Actual row number 



		double d_temp = 0.0;

		for (int i_row = 0; i_row < nrow; i_row++)

		{

			d_temp = 1.0;

			for (int i_col = 0; i_col < ncol; i_col++)

			{

				if (z[i_row][i_col] == 0) { d_temp = 0.0; break; } //found zero, i.e. missing cell

			}



			if (fabs_FHDI(d_temp) > 1e-15) //this row has no missing cells

			{
				ol.push_back(i_row + 1);
			} //actual number of the row having no missing cells



			if (fabs_FHDI(d_temp) < 1e-15) //this row has AT LEAST one missing cells

			{
				ml.push_back(i_row + 1);
			}  //actual number of the row having missing cells

		}

		const int i_size_ol = (int)ol.size();

		const int i_size_ml = (int)ml.size();

		if (i_size_ol == 0) { Rprintf("Error! no observed unit in FHDI_Extension. \n"); return 0; }

		if (i_size_ml == 0) { Rprintf("Error! no missing  unit in FHDI_Extension. \n"); return 0; }



		//--------------

		//Rows of observed RAW data 

		//Rows of missing  RAW data

		//--------------

		double** d_oy = New_dMatrix(i_size_ol, ncol);

		double** d_my = New_dMatrix(i_size_ml, ncol);

		for (int i = 0; i < i_size_ol; i++)

		{

			for (int j = 0; j < ncol; j++)

			{

				d_oy[i][j] = y[ol[i] - 1][j]; //-1 for Actual loc

			}

		}

		for (int i = 0; i < i_size_ml; i++)

		{

			for (int j = 0; j < ncol; j++)

			{

				d_my[i][j] = y[ml[i] - 1][j];	//-1 for Actual loc

			}

		}



		//--------------

		//Rows of observed data 

		//Rows of missing data

		//--------------

		double** d_ox = New_dMatrix(i_size_ol, ncol);

		double** d_mx = New_dMatrix(i_size_ml, ncol);

		for (int i = 0; i < i_size_ol; i++)

		{

			for (int j = 0; j < ncol; j++)

			{

				d_ox[i][j] = z[ol[i] - 1][j]; //-1 for Actual loc

			}

		}

		for (int i = 0; i < i_size_ml; i++)

		{

			for (int j = 0; j < ncol; j++)

			{

				d_mx[i][j] = z[ml[i] - 1][j];	//-1 for Actual loc

			}

		}



		//----------------

		//weights corresponding to missing/observed rows. 

		//select out weights at missing rows and observed rows

		//----------------

		double* w_ml = new double[i_size_ml]; //same as mw

		double* w_ol = new double[i_size_ol]; //same as ow

		for (int i = 0; i < i_size_ml; i++) w_ml[i] = w[ml[i] - 1]; //-1 for actual loc

		for (int i = 0; i < i_size_ol; i++) w_ol[i] = w[ol[i] - 1]; //-1 for actual loc



																	//----------------

																	//index corresponding to missing/observed rows. 

																	//select out weights at missing rows and observed rows

																	//----------------

		int* id_ml = new int[i_size_ml]; //same as mid

		int* id_ol = new int[i_size_ol]; //same as oid

		for (int i = 0; i < i_size_ml; i++) id_ml[i] = id[ml[i] - 1]; //-1 for actual loc

		for (int i = 0; i < i_size_ol; i++) id_ol[i] = id[ol[i] - 1]; //-1 for actual loc



																	  //-------------------------------

																	  //Step 1: generate uox and mox

																	  //-------------------------------

																	  //make UNIQUE patterns of z by cn

																	  //--------------

																	  //transform z into condensed string format

																	  //--------------

																	  //std::string cn[nrow]; //declaration of concatenated vector of z

		std::string *cn = new std::string[nrow]; //declaration of concatenated vector of z

		Trans(z, nrow, ncol, cn);



		//---------------

		//Rows of Condensed Strings with Observed cells

		//                          with Missing  cells

		//---------------

		//std::string s_ocn[i_size_ol];

		//std::string s_mcn[i_size_ml];

		std::string *s_ocn = new std::string[i_size_ol];

		std::string *s_mcn = new std::string[i_size_ml];

		for (int i = 0; i < i_size_ol; i++) s_ocn[i] = cn[ol[i] - 1]; //-1 for actual row

		for (int i = 0; i < i_size_ml; i++) s_mcn[i] = cn[ml[i] - 1]; //-1 for actual row



																	  //std::string s_ocn_temp[i_size_ol]; //string vector of observed patterns only

																	  //std::string s_mcn_temp[i_size_ml]; //string vector of missing patterns only

		std::string *s_ocn_temp = new std::string[i_size_ol]; //string vector of observed patterns only

		std::string *s_mcn_temp = new std::string[i_size_ml]; //string vector of missing patterns only	

		for (int i = 0; i < i_size_ol; i++) { s_ocn_temp[i] = s_ocn[i]; }

		for (int i = 0; i < i_size_ml; i++) { s_mcn_temp[i] = s_mcn[i]; }

		//sort 		

		std::sort(s_ocn_temp, s_ocn_temp + i_size_ol); //knowing that s_ocn_temp[] has i_size_ol entities

		std::sort(s_mcn_temp, s_mcn_temp + i_size_ml); //knowing that s_mcn_temp[] has i_size_ml entities



													   //------------

													   //memorize observed patterns. Only unique patterns are stored  

													   //------------

		double** uox = New_dMatrix(nrow, ncol);

		double** mox = New_dMatrix(nrow, ncol);



		int i_count_uox = 0; //total number of unique uox 

		std::string s_temp;

		for (int i = 0; i < i_size_ol; i++)

		{

			s_temp = s_ocn_temp[i]; //get a string 

			for (int j = 0; j < nrow; j++) //search all rows 

			{

				//----

				//below condition is needed for finding UNIQUE pattern

				//----

				//if(j==0 && s_temp == cn[j]) 

				//if(i==0 && s_temp == cn[j]) //with first string, find the same string in cn 

				if (i == 0 && s_temp.compare(cn[j]) == 0) //0: equal string

				{

					for (int k = 0; k < ncol; k++)

					{
						uox[i_count_uox][k] = z[j][k];
					} //store the found observed pattern

					i_count_uox++;

					break;

				}

				//if(j>0 && s_temp == cn[j] && s_temp != cn[j-1])

				//if(i>0 && s_temp == cn[j] && s_temp != s_ocn_temp[i-1]) //find UNIQUE matching 

				if (i > 0 && s_temp.compare(cn[j]) == 0 && s_temp.compare(s_ocn_temp[i - 1]) != 0)

				{

					for (int k = 0; k < ncol; k++)

					{
						uox[i_count_uox][k] = z[j][k];
					} //store the found observed pattern				

					i_count_uox++;

					break;

				}

			}

		}

		//Now, i_count_uox means the total number of unique observed patterns



		//------------

		//memorize missing patterns 

		//------------

		int i_count_mox = 0; //total number of unique mox 



		for (int i = 0; i < i_size_ml; i++)

		{

			s_temp = s_mcn_temp[i]; //get a string 

			for (int j = 0; j < nrow; j++) //search all rows 

			{

				//----

				//below condition is needed for finding unique pattern

				//----

				//if(j==0 && s_temp == cn[j]) 

				//if(i==0 && s_temp == cn[j]) //with first string, find matching string in cn

				if (i == 0 && s_temp.compare(cn[j]) == 0) //0: equal string 

				{

					for (int k = 0; k < ncol; k++)

					{
						mox[i_count_mox][k] = z[j][k];
					} //store the found missing pattern

					i_count_mox++;

					break;

				}

				//if(j>0 && s_temp == cn[j] && s_temp != cn[j-1])

				//if(i>0 && s_temp == cn[j] && s_temp != s_mcn_temp[i-1]) //find UNIQUE matching string

				if (i > 0 && s_temp.compare(cn[j]) == 0 && s_temp.compare(s_mcn_temp[i - 1]) != 0) //0: equal

				{

					for (int k = 0; k < ncol; k++)

					{
						mox[i_count_mox][k] = z[j][k];
					} //store the found missing pattern				

					i_count_mox++;

					break;

				}

			}

		}

		//Now, i_count_mox means the total number of unique missing patterns



		//----------------

		//additional check for unique observed and missing patterns

		//----------------

		//observed patterns//

		d_temp = 0.0;

		double** uox_final = New_dMatrix(nrow, ncol);

		for (int j = 0; j < ncol; j++) { uox_final[0][j] = uox[0][j]; } //first row initialization

		int i_count_uox_final = 1; //starting from the second row



		for (int i = 1; i < i_count_uox; i++) //starting from the second one

		{

			d_temp = 0.0; //initialize 

			for (int j = 0; j < ncol; j++) { d_temp += fabs_FHDI(uox[i][j] - uox[i - 1][j]); } //difference of adjacent rows



			if (d_temp > 1e-3) //adjacent rows are NOT the same each other

			{

				for (int j = 0; j < ncol; j++) { uox_final[i_count_uox_final][j] = uox[i][j]; }

				i_count_uox_final++;

			}

		}

		i_count_uox = i_count_uox_final; //replace with the accurate value

										 //store the final matrix 

		for (int i = 0; i < i_count_uox; i++)

		{

			for (int j = 0; j < ncol; j++)

				uox[i][j] = uox_final[i][j];

		}

		Del_dMatrix(uox_final, nrow, ncol);



		//--------------------------

		//missing patterns//

		//--------------------------

		double** mox_final = New_dMatrix(nrow, ncol);

		for (int j = 0; j < ncol; j++) { mox_final[0][j] = mox[0][j]; } //first row initialization

		int i_count_mox_final = 1; //starting from the second row



		for (int i = 1; i < i_count_mox; i++) //starting from the second one

		{

			d_temp = 0.0; //initialize

			for (int j = 0; j < ncol; j++) { d_temp += fabs_FHDI(mox[i][j] - mox[i - 1][j]); } //difference of adjacent rows



			if (d_temp > 1e-3) //adjacent rows are NOT the same each other

			{

				for (int j = 0; j < ncol; j++) { mox_final[i_count_mox_final][j] = mox[i][j]; }

				i_count_mox_final++;

			}

		}

		i_count_mox = i_count_mox_final; //replace with the accurate value



										 //store the final matrix	

		for (int i = 0; i < i_count_mox; i++)

		{

			for (int j = 0; j < ncol; j++)

				mox[i][j] = mox_final[i][j];

		}

		Del_dMatrix(mox_final, nrow, ncol);



		//!!!!! now uox and mox have the UNIQUE observed and missing patterns

		//!!!!! i_count_mox and _uox have the final number of meaningful rows of mox and uox, respectively

		const int nrm = i_count_mox;

		const int nru = i_count_uox;



		//-------------------------------------------

		//-------------------------------------------

		//Step 2: Impute missing cells

		//        using all possible donors per missing pattern

		//-------------------------------------------

		//-------------------------------------------

		int* i_temp_x = new int[ncol];

		int i_sum_x = 0;



		std::vector<int> v_mxl; //a row's columns having observed cells  

		rbind_FHDI rbind_icell(ncol); //all possible donor cells 



		std::vector<int> v_cn_z_i;

		//int* zid = NULL;

		//int i_size_zid=0; 

		//int i_loc=0;	

		int* i_srst = new int[nru];

		std::vector<int> loc_srst_nl;

		double* d_temp_cn = new double[ncol];



		//List_FHDI List_ord(nrm); //order records used for variance estimation

		//List_FHDI List_ocsg(nrm); //order records used for variance estimation



		//--------------------------------

		//--------------------------------

		//--------------------------------

		//Main Loop for FEFI and FHDI

		//--------------------------------

		//--------------------------------

		//--------------------------------

		rbind_FHDI rbind_imat_FEFI(7 + 2 * ncol); //large storage that will accumulate fmat from FEFI  

		rbind_FHDI rbind_imat_FHDI(7 + 2 * ncol); //large storage that will accumulate fmat from FHDI





		for (int i = 0; i < nrm; i++)

		{

			//get current row of missing cell 

			for (int j = 0; j < ncol; j++) i_temp_x[j] = (int)mox[i][j];

			i_sum_x = sum_FHDI(i_temp_x, ncol);





			//-------

			//re-initialization for this missing row 

			//-------

			rbind_icell.initialize(ncol);



			//----------------------

			//Condition 1: this row's cells are all missing

			//-----------------------

			if (i_sum_x == 0)

			{

				v_mxl.clear(); //no missing cells  

				rbind_icell.bind_blocks(i_count_uox, ncol, uox); //fine due to row-based copy

			}



			//----------------------

			//Condition 2: this row's cells are partly missing

			//-----------------------

			int nl = 0;

			if (i_sum_x > 0)

			{

				//------

				//number of observed cells on this row

				//------

				nl = 0;

				v_mxl.clear();

				for (int j = 0; j < ncol; j++)

				{

					if (mox[i][j] > 0)

					{

						nl++;

						//v_mxl.push_back(j + 1); //Actual non-missing cell location 

					}

				}

				if (nl > i_option_collapsing) {
					nl = i_option_collapsing;
				}



				//-------

				//indicator matrix that matches the donors

				//srst: row-wise sum of the indicator matrix 

				//-------

				loc_srst_nl.clear(); //re-initialize

				Fill_iVector(i_srst, nru, 0); //re-initialize 

											  //inherents the most correlated variables of mox[i]

				for (int k = 0; k < i_option_collapsing; k++) {
					if (codes[i][k] != 0) {
						v_mxl.push_back(codes[i][k]);
						//TestOut << "code[" << i << "]: " << codes[i][k] << endl;
					}
				}

				int v_mxl_size = v_mxl.size();

				for (int j = 0; j < nru; j++)

				{

					int i_sum_crst = 0;

					for (int k = 0; k < v_mxl_size; k++)

					{

						//Note: in below check, mox is fixed at ith row 

						if (fabs_FHDI(mox[i][v_mxl[k] - 1] - uox[j][v_mxl[k] - 1]) < 1e-3) //part of missing cell = obserbed cell 

						{

							i_sum_crst++; // increment if a cell of missing row = obs. cell 

						}

					}

					//---

					//store how many cells of missing row match those of observed row

					//---

					i_srst[j] = i_sum_crst;

					if (i_sum_crst == nl) loc_srst_nl.push_back(j + 1); //Actual location 				

				}



				//-----

				//total matching rows

				//-----

				const int i_size_loc_srst_nl = (int)loc_srst_nl.size();

				if (i_size_loc_srst_nl == 0) //error case

				{
					Rprintf("Error! there is no matched cell! \n"); return 0;
				}



				if (i_size_loc_srst_nl > 0)

				{

					double* d_temp_srst = new double[ncol];

					for (int j = 0; j < i_size_loc_srst_nl; j++)

					{

						for (int k = 0; k < ncol; k++)

						{
							d_temp_srst[k] = uox[loc_srst_nl[j] - 1][k];
						}//-1 for actual loc



						rbind_icell.append_block(d_temp_srst); //ncol is the same 

					}

					delete[] d_temp_srst;

				}

			}



			//----------------------------

			//step 3: Assign donors to missing cell

			//----------------------------

			const int nic = rbind_icell.size_row(); //get the number of total rows

													//std::string s_icn[nic];

			std::string * s_icn = new std::string[nic];

			double* d_temp_icell = new double[ncol];

			std::string s_icn_temp;

			for (int j = 0; j < nic; j++)

			{

				for (int k = 0; k < ncol; k++) d_temp_icell[k] = rbind_icell(j, k); //get jth row

				Trans1(d_temp_icell, ncol, s_icn_temp); //transform one row

				s_icn[j] = s_icn_temp;

			}

			delete[] d_temp_icell;



			//------------------

			//search locations where icn = name of jp_name

			//------------------

			const int i_size_jp_name = (int)jp_name.size();

			std::vector<double>      v_cp;  //selected joint probability 

			std::vector<std::string> v_ncp; //names of the selected joint probability

			v_cp.clear();

			v_ncp.clear();



			for (int j = 0; j < nic; j++)

			{

				s_temp = s_icn[j]; //one donor 

				for (int k = 0; k < i_size_jp_name; k++) //search all names of jp

				{

					if (s_temp.compare(jp_name[k]) == 0) //0 means the same string

					{

						v_cp.push_back(jp_prob[k]); //store the joint probability 

						v_ncp.push_back(jp_name[k]); //store name

						break; //stop searching after finding the first match 

					}

				}

			}

			const int i_size_v_cp = (int)v_cp.size();

			double d_sum_v_cp = 0.0;

			for (int j = 0; j < i_size_v_cp; j++) d_sum_v_cp += v_cp[j];

			if (d_sum_v_cp != 0)

			{

				for (int j = 0; j < i_size_v_cp; j++) v_cp[j] = v_cp[j] / d_sum_v_cp;

			}



			//-----------

			//transform current missing row to string for step 3

			//-----------

			for (int j = 0; j < ncol; j++) d_temp_cn[j] = mox[i][j];

			Trans1(d_temp_cn, ncol, s_temp);

			v_cn_z_i.clear(); //re-initialize 

			which(cn, nrow, s_temp, v_cn_z_i); //Note: Actual location is returned

			int i_size_v_cn_z_i = (int)v_cn_z_i.size(); //number of locations in cn having s_temp

			const int mg = i_size_v_cn_z_i; //Note: loc2 =  i_size_v_cn_z_i





											//---------------------

											//select out all cells that have s_icn[1:nic] from cn

											//---------------------

			std::vector<int> v_obsg0; v_obsg0.clear();

			for (int j = 0; j < nic; j++)

			{

				s_temp = s_icn[j];

				for (int k = 0; k < nrow; k++)

				{

					if (s_temp.compare(cn[k]) == 0) //0=equal string

					{
						v_obsg0.push_back(k + 1);
					}//ACTUAL location stored. No exit 

				}

			}

			const int i_size_v_obsg0 = (int)v_obsg0.size();



			//---------------

			//sort the found cells

			//---------------

			int* i_obsg0_sorted = new int[i_size_v_obsg0];

			for (int k = 0; k < i_size_v_obsg0; k++) i_obsg0_sorted[k] = v_obsg0[k];

			std::sort(i_obsg0_sorted, i_obsg0_sorted + i_size_v_obsg0);

			for (int k = 0; k < i_size_v_obsg0; k++) v_obsg0[k] = i_obsg0_sorted[k];

			delete[] i_obsg0_sorted;





			//------------------

			//half-ascending and -descending ordering 

			//------------------

			std::vector<int> v_obsg; //half-asc and desc obsg0



			int* i_ym_return = new int[i_size_v_obsg0]; //half-asc and desc 

			yorder(y, nrow, ncol,

				d_temp_cn,

				v_obsg0, i_ym_return);



			v_obsg.clear();

			for (int j = 0; j < i_size_v_obsg0; j++) v_obsg.push_back(i_ym_return[j]);

			//below is temporary for wrong yorder()

			//for(int j=0; j<i_size_v_obsg0; j++) v_obsg.push_back(v_obsg0[j]); 



			const int i_size_v_obsg = (int)v_obsg.size();

			delete[] i_ym_return;





			//testout

			//RPrint("============= after yorder() ================");

			//RPrint("v_obsg0 :"); RPrint(v_obsg0);

			//RPrint("v_obsg :");  RPrint(v_obsg);



			//-------------

			// find positions of matches between obsg in obsg0

			// then Store them into List 

			//-------------

			std::vector<int> v_rbsg; v_rbsg.clear();

			match_FHDI(v_obsg, v_obsg0, v_rbsg); //get loc stored in v_rbsg

			const int i_size_v_rbsg = (int)v_rbsg.size();

			//store rbsg

			double* d_temp_rbsg = new double[i_size_v_rbsg];

			for (int k = 0; k < i_size_v_rbsg; k++) d_temp_rbsg[k] = v_rbsg[k];

			List_ord.put_block(i, i_size_v_rbsg, d_temp_rbsg); //put into storage as ith row

			delete[] d_temp_rbsg;

			//store obsg

			double* d_temp_obsg = new double[i_size_v_obsg];

			for (int k = 0; k < i_size_v_obsg; k++) d_temp_obsg[k] = v_obsg[k];

			List_ocsg.put_block(i, i_size_v_obsg, d_temp_obsg); //put into storage as ith row

			delete[] d_temp_obsg;



			const int ng = i_size_v_obsg;

			//------------------------------

			//------------------------------

			//Compute Fractional Weights (fwij)

			//Fractional weights for FEFI representing sampling w

			//------------------------------

			//------------------------------

			//std::string cn_obsg[i_size_v_obsg]; //cn at locations of obsg

			std::string * cn_obsg = new std::string[i_size_v_obsg]; //cn at locations of obsg

			for (int k = 0; k < i_size_v_obsg; k++)

			{
				cn_obsg[k] = cn[v_obsg[k] - 1];
			}//-1 for actual location 





			 //-----

			 //make a table of cn at obsg locations

			 //------

			std::vector<std::string> v_table_name_cn_obsg;

			std::vector<int>         v_table_count_cn_obsg;



			table_cpp(cn_obsg, i_size_v_obsg,

				v_table_name_cn_obsg, v_table_count_cn_obsg);

			//const int i_size_table_cn_obsg = (int)v_table_count_cn_obsg.size(); 



			//------

			//get joint probability of the selected donor 

			//------

			std::vector<int> v_cn_obsg_ncp; //positions of cn_obsg in ncp 

			match_FHDI(cn_obsg, i_size_v_obsg, v_ncp,

				v_cn_obsg_ncp); //Note: Actual locations are returned 

			const int i_size_v_cn_obsg_ncp = (int)v_cn_obsg_ncp.size();



			//------

			//calculate fractional weights

			//------

			double* fwij = new double[i_size_v_cn_obsg_ncp]; //fractional weights 

			double* d_obsp = new double[i_size_v_cn_obsg_ncp]; //joint prob of selected donors

			int*    i_obsn = new int[i_size_v_cn_obsg_ncp]; //counts of the selected donors

			for (int k = 0; k < i_size_v_cn_obsg_ncp; k++)

			{

				d_obsp[k] = v_cp[v_cn_obsg_ncp[k] - 1];// -1 for actual location 

				i_obsn[k] = v_table_count_cn_obsg[v_cn_obsg_ncp[k] - 1];// -1 for actual location  



				fwij[k] = 1.0; //default for error case  

				if (i_obsn[k] != 0) fwij[k] = d_obsp[k] / i_obsn[k];

				if (i_obsn[k] == 0) Rprintf("Error! zero count in obsn!");

			}

			//testout

			//RPrint("fwij[] :"); RPrint(fwij, i_size_v_cn_obsg_ncp);

			//===============================================
			// Note that v_mxl hereafter this function indicates locations of observed variables of mox[i], similar functionality of response indicator, not selected variables for donors. Written by Yicheng Yang
			//===============================================
			v_mxl.clear();

			for (int j = 0; j < ncol; j++)

			{

				if (mox[i][j] > 0)

				{

					v_mxl.push_back(j + 1); //Actual non-missing cell location 

				}

			}


			//----------------------

			//FEFI Imputation

			//  Algorithm: impute by using all possible donors

			//  final outcome is "fmat" in which each column means that

			//  col1: id

			//  col2: fid, i.e., id of imputed value

			//  col3: sampling weight

			//  col4: fractional weights 

			//  col5: imputed original data (matrix with column of ncol) 

			//  col6: imputed category data (matrix with column of ncol)

			//  col7: 1:ng 

			//  col8: = col2  (for consistency with FHDI results)

			//  col9: = col3  (for consistency with FHDI results)

			//----------------------

			std::vector<int> v_obsg_times_mg; v_obsg_times_mg.clear();

			if (s_M.compare("FEFI") == 0) //0=equal string

			{

				double** fmat_FEFI = New_dMatrix(ng*mg, 7 + 2 * ncol); //7columns and two blocks of ncol 



				Fully_Efficient_Fractional_Imputation(ng, mg,

					v_obsg, v_mxl,

					y, z, nrow, ncol,

					v_cn_z_i, fwij, i_size_v_cn_obsg_ncp,

					w, id,

					fmat_FEFI);



				//testout

				//RPrint("in ==== M=FEFI =after making fmat_FEFI[][]====");

				//RPrint("fmat_FEFI : "); RPrint(fmat_FEFI, ng*mg, 7+2*ncol);



				//------------------

				//Append fmat_FEFI onto global storage imat

				//------------------

				rbind_imat_FEFI.bind_blocks(ng*mg, 7 + 2 * ncol, fmat_FEFI);



				//-----------------------

				//local deallocation

				//-----------------------

				Del_dMatrix(fmat_FEFI, ng*mg, 7 + 2 * ncol);

			}





			//------------------------------------

			//FHDI

			//Fractional Hot Deck Imputation

			//------------------------------------

			//if(s_M.compare("FHDI") == 0) //0= equal string



			const int i_mxl = (int)v_mxl.size();



			//prepare return matrix. Note the different row size from fmat of FEFI 

			int i_row_fmat_FHDI = i_M*mg; //default row size of return matrix of FHDI

			if (i_size_v_obsg <= i_M) i_row_fmat_FHDI = i_size_v_obsg*mg;  //if donors are less than i_M



			if (s_M.compare("FHDI") == 0) //0= equal string

			{

				double** fmat_FHDI = New_dMatrix(i_row_fmat_FHDI, 7 + 2 * ncol); //return matrix from FHDI

																				 //below random number between 0 and 1 should use appropriate library depending upon version	

																				 //d_myran = static_cast<double>(std::rand())/static_cast<double>(RAND_MAX); //Window ver
																				 //d_myran = static_cast<double>(std::rand()) / static_cast<double>(RAND_MAX);
				d_myran = Rf_runif(0.0, 1.0); //R package version 

											  //testout

											  // Rprintf("d_myran: "); Rprintf("%g ", d_myran); 
											  //cout<<"rand(): "<<std::rand()<<endl;
											  //cout << "RAND_MAX: " << RAND_MAX << endl;
											  //cout<<"d_myran: "<< d_myran <<endl;

				Fractional_Hot_Deck_Imputation(i,

					ng, List_ocsg, ncol,

					mox, y, nrow, i_M,

					mg, z, i_mxl,

					v_cn_z_i, v_mxl,

					v_obsg,

					fwij, i_size_v_cn_obsg_ncp,

					d_obsp, i_obsn,

					d_myran,

					w, id,

					fmat_FHDI);



				//------------------

				//Append fmat_FHDI onto global storage imat

				//------------------

				rbind_imat_FHDI.bind_blocks(i_row_fmat_FHDI, 7 + 2 * ncol, fmat_FHDI);



				//-----------------------

				//local deallocation

				//-----------------------

				Del_dMatrix(fmat_FHDI, i_row_fmat_FHDI, 7 + 2 * ncol);

			}



			//-----------------------

			//local deallocation

			//-----------------------

			delete[] s_icn;

			delete[] cn_obsg;

			delete[] fwij;

			delete[] d_obsp;

			delete[] i_obsn;



		} //end of Main loop for all rows of missing patterns 





		  //---------------

		  //---------------

		  //Step 4: construct output results

		  //---------------

		  //------------------------------------------------------

		  //ipmat  = final imputation results

		  //     	col1: ID 	= unit index

		  //		col2: FID 	= ID of fractionally imputed value

		  // 		col3: WGT 	= weight 

		  //		col4: FWGT	= Frational weight

		  //		col5: Variables 

		  //		col6: Responses

		  //irmat  = imputation results related to the categorized matrix 

		  //     	col1: ID 	= unit index

		  //		col2: FID 	= ID of fractionally imputed value

		  //		col3: OID	= original rank of the imputed value

		  //		col4: ORDER = SN(selected donor)

		  //		col5: FEFIW	= Fefi weights 

		  //		col6: CELL	= cells 

		  //----------------------------------------------------

		  //FEFI                           FEFI //

		  //get ipmat, Resp (separately), irmat from FEFI results 

		  //rbind_FHDI  rbind_ipmat_FEFI(4+ncol); //column size is 4+ncol

		  //rbind_FHDI  rbind_Resp_FEFI(ncol+1);  //separate response matrix  

		  //rbind_FHDI  rbind_irmat_FEFI(5+ncol); //column size is 5+ncol

		if (s_M.compare("FEFI") == 0) //0= equal string

		{

			Results_Fully_Efficient_Fractional_Imputation(i_size_ol,

				ncol, nrow,

				id_ol, w_ol, d_oy, d_ox,

				rbind_imat_FEFI, r,



				rbind_ipmat_FEFI, rbind_Resp_FEFI, rbind_irmat_FEFI);

			//testout

			/*

			RPrint("after Results_... rbind_ipmat_FEFI after binding :");

			rbind_ipmat_FEFI.print_rbind_FHDI();

			RPrint("after Results_... rbind_Resp_FEFI after binding :");

			rbind_Resp_FEFI.print_rbind_FHDI();

			RPrint("after Results_... rbind_irmat_FEFI after binding :");

			rbind_irmat_FEFI.print_rbind_FHDI();

			*/

		}



		//FHDI ------------------------- FHDI //

		//get ipmat, Resp (separately), irmat from FHDI results 

		//rbind_FHDI  rbind_ipmat_FHDI(4+ncol); //column size is 4+ncol

		//rbind_FHDI  rbind_Resp_FHDI(ncol+1);  //separate response matrix  

		//rbind_FHDI  rbind_irmat_FHDI(5+ncol); //column size is 5+ncol



		if (s_M.compare("FHDI") == 0) //0= equal string

		{

			Results_Fractional_Hot_Deck_Imputation(i_size_ol,

				ncol, nrow,

				id_ol, w_ol, d_oy, d_ox,

				rbind_imat_FHDI, r,



				rbind_ipmat_FHDI, rbind_Resp_FHDI, rbind_irmat_FHDI);

			//testout

			/*

			RPrint("after Results_... rbind_ipmat_FHDI after binding :");

			rbind_ipmat_FHDI.print_rbind_FHDI();

			RPrint("after Results_... rbind_Resp_FHDI after binding :");

			rbind_Resp_FHDI.print_rbind_FHDI();

			RPrint("after Results_... rbind_irmat_FHDI after binding :");

			rbind_irmat_FHDI.print_rbind_FHDI();

			*/

		}



		//------

		//prep returns of other matrices

		//------

		rbind_uox.bind_blocks(i_count_uox, ncol, uox);

		rbind_mox.bind_blocks(i_count_mox, ncol, mox);



		//testout

		Rprintf(" ========= FHDI has successfully finished! \n");





		//----------------

		//Deallocation

		//----------------

		delete[] cn;

		delete[] s_ocn;

		delete[] s_mcn;

		delete[] s_ocn_temp;

		delete[] s_mcn_temp;



		delete[] i_rn;

		//lete[] w; 

		//lete[] id; 

		Del_dMatrix(d_oy, i_size_ol, ncol);

		Del_dMatrix(d_my, i_size_ml, ncol);

		Del_dMatrix(d_ox, i_size_ol, ncol);

		Del_dMatrix(d_mx, i_size_ml, ncol);



		delete[] w_ml;

		delete[] w_ol;

		delete[] id_ml;

		delete[] id_ol;



		Del_dMatrix(uox, nrow, ncol);

		Del_dMatrix(mox, nrow, ncol);



		delete[] i_temp_x;

		delete[] i_srst;

		delete[] d_temp_cn;





		return 1;



	}



} //end of namespace


  //Fn===========================================================================

  //FHDI_Neighbor_cpp.cc-----------------------------------------------------------------------------

  //Fn===========================================================================

namespace FHDI {

	bool FHDI_Neighbor_cpp(double** y, double** z, int** r,

		const int nrow, const int ncol,

		std::vector<std::string> jp_name,

		std::vector<double> 	 jp_prob,

		std::string s_M, const int i_M, double* w, int* id, List_FHDI &List_nU,



		rbind_FHDI &rbind_ipmat_FEFI,

		rbind_FHDI &rbind_Resp_FEFI,

		rbind_FHDI &rbind_irmat_FEFI,



		rbind_FHDI &rbind_ipmat_FHDI,

		rbind_FHDI &rbind_Resp_FHDI,

		rbind_FHDI &rbind_irmat_FHDI,



		rbind_FHDI &rbind_uox,

		rbind_FHDI &rbind_mox,

		List_FHDI  &List_ord,

		List_FHDI  &List_ocsg)

		//Description=========================================

		// perform

		// Fully Efficient Fractional Imputation OR

		// Fractional Hot Deck Imputation

		// 

		// Algorithm: FEFI of Dr Jae Kwang. Kim and FHDI of Dr Jong Ho. Im

		//

		// original R code: Dr. Im, J. and Dr. Kim, J. 

		// c++ code: 		Dr. Cho, In-Ho and Yicheng Yang

		// All rights reserved

		// 

		// updated: Aug 11, 2020

		//----------------------------------------------------

		//IN    : double y(nrow, ncol)= original data matrix with missing cells 

		//IN    : double z(nrow, ncol)= categorized matrix of y. 0 for missing cell

		//IN    : int    r(nrow, ncol) = index matrix of missing unit (0)/observed unit (1)  

		//IN	: vector<string> jp_name  = name of table of joint probability

		//IN	: vector<double> jp_prob  = joint probability 

		//IN  	: string s_M = "FEFI" fully efficient fractional imputation 

		// 					   "FHDI" Fractional Hot Deck Imputation  

		//IN    : int i_M = number of donors used for FHDI

		//OUT   : rbind_FHDI  rbind_ipmat_FEFI(4+ncol); //column size is 4+ncol (i.e., for R: ID, FID, WGT, FWGT, Variables)

		//OUT   : rbind_FHDI  rbind_Resp_FEFI(ncol+1);  //separate response matrix  (i.e. for R: unit responses and Resp0)

		//OUT   : rbind_FHDI  rbind_irmat_FEFI(5+ncol); //column size is 5+ncol (i.e. for R:ID, FID, OID, ORDER, FEFIW, CELL )

		//OUT   : rbind_FHDI  rbind_ipmat_FHDI(4+ncol); //column size is 4+ncol

		//OUT   : rbind_FHDI  rbind_Resp_FHDI(ncol+1);  //separate response matrix  

		//OUT   : rbind_FHDI  rbind_irmat_FHDI(5+ncol); //column size is 5+ncol

		//OUT   : rbind_FHDI  rbind_uox (ncol) //observed unique categorized matrix

		//OUT   : rbind_FHDI  rbind_mox (ncol) //missing  unique categorized matrix

		//OUT   : List_FHDI   List_ord(nrow) //but meaningful up to i_count_mox rows; List_ord(nrow_mox) record transform from ascending donor locations to half-a-half-d order

		//OUT   : List_FHDI   List_ocsg(nrow)//but meaningful up to i_count_mox rows; List_ocsg(nrow_mox) record half-a-half-d donor locations in z 

		//====================================================

	{

		//-----------------

		//random location using uniform distribution   

		//using Numerical Recipes of Press et al 2007. 

		//-----------------

		//Ran_FHDI myran(1); 	//not used for CRAN Compatibility

		//std::srand(123); //window version 


		//set.seed(123);// R package version, this should be done at R main 

		double d_myran = 0.0;





		//-------------

		//column-wise sum of r matrix

		//-------------

		int* i_rn = new int[ncol];

		int i_temp = 0;

		for (int i = 0; i<ncol; i++)

		{

			i_temp = 0;

			for (int j = 0; j<nrow; j++) i_temp += r[j][i];

			i_rn[i] = i_temp;

		}



		//-------------

		//sample weight (default is 1)

		//id array (default is row number)

		//-------------
		/*

		double* w = new double[nrow];

		int* id   = new int[nrow];

		for(int i=0; i<nrow; i++)

		{

		w[i] = 1.0;

		id[i] = i+1; //ACTUAL id

		}
		*/



		//--------------

		//locations of missing cells (ml) and observed cells (ol)

		//Note: unlike in Cell_Make..(), std vector is used here

		//--------------

		std::vector<int> ol; //Actual row number 

		std::vector<int> ml; //Actual row number 



		double d_temp = 0.0;

		for (int i_row = 0; i_row<nrow; i_row++)

		{

			d_temp = 1.0;

			for (int i_col = 0; i_col<ncol; i_col++)

			{

				if (z[i_row][i_col] == 0) { d_temp = 0.0; break; } //found zero, i.e. missing cell

			}



			if (fabs_FHDI(d_temp) > 1e-15) //this row has no missing cells

			{
				ol.push_back(i_row + 1);
			} //actual number of the row having no missing cells



			if (fabs_FHDI(d_temp) < 1e-15) //this row has AT LEAST one missing cells

			{
				ml.push_back(i_row + 1);
			}  //actual number of the row having missing cells

		}

		const int i_size_ol = (int)ol.size();

		const int i_size_ml = (int)ml.size();

		if (i_size_ol == 0) { Rprintf("Error! no observed unit in FHDI_Extension. \n"); return 0; }

		if (i_size_ml == 0) { Rprintf("Error! no missing  unit in FHDI_Extension. \n"); return 0; }



		//--------------

		//Rows of observed RAW data 

		//Rows of missing  RAW data

		//--------------

		double** d_oy = New_dMatrix(i_size_ol, ncol);

		double** d_my = New_dMatrix(i_size_ml, ncol);

		for (int i = 0; i<i_size_ol; i++)

		{

			for (int j = 0; j<ncol; j++)

			{

				d_oy[i][j] = y[ol[i] - 1][j]; //-1 for Actual loc

			}

		}

		for (int i = 0; i<i_size_ml; i++)

		{

			for (int j = 0; j<ncol; j++)

			{

				d_my[i][j] = y[ml[i] - 1][j];	//-1 for Actual loc

			}

		}



		//--------------

		//Rows of observed data 

		//Rows of missing data

		//--------------

		double** d_ox = New_dMatrix(i_size_ol, ncol);

		double** d_mx = New_dMatrix(i_size_ml, ncol);

		for (int i = 0; i<i_size_ol; i++)

		{

			for (int j = 0; j<ncol; j++)

			{

				d_ox[i][j] = z[ol[i] - 1][j]; //-1 for Actual loc

			}

		}

		for (int i = 0; i<i_size_ml; i++)

		{

			for (int j = 0; j<ncol; j++)

			{

				d_mx[i][j] = z[ml[i] - 1][j];	//-1 for Actual loc

			}

		}



		//----------------

		//weights corresponding to missing/observed rows. 

		//select out weights at missing rows and observed rows

		//----------------

		double* w_ml = new double[i_size_ml]; //same as mw

		double* w_ol = new double[i_size_ol]; //same as ow

		for (int i = 0; i<i_size_ml; i++) w_ml[i] = w[ml[i] - 1]; //-1 for actual loc

		for (int i = 0; i<i_size_ol; i++) w_ol[i] = w[ol[i] - 1]; //-1 for actual loc



																  //----------------

																  //index corresponding to missing/observed rows. 

																  //select out weights at missing rows and observed rows

																  //----------------

		int* id_ml = new int[i_size_ml]; //same as mid

		int* id_ol = new int[i_size_ol]; //same as oid

		for (int i = 0; i<i_size_ml; i++) id_ml[i] = id[ml[i] - 1]; //-1 for actual loc

		for (int i = 0; i<i_size_ol; i++) id_ol[i] = id[ol[i] - 1]; //-1 for actual loc



																	//-------------------------------

																	//Step 1: generate uox and mox

																	//-------------------------------

																	//make UNIQUE patterns of z by cn

																	//--------------

																	//transform z into condensed string format

																	//--------------

																	//std::string cn[nrow]; //declaration of concatenated vector of z

		std::string *cn = new std::string[nrow]; //declaration of concatenated vector of z

		Trans(z, nrow, ncol, cn);



		//---------------

		//Rows of Condensed Strings with Observed cells

		//                          with Missing  cells

		//---------------

		//std::string s_ocn[i_size_ol];

		//std::string s_mcn[i_size_ml];

		std::string *s_ocn = new std::string[i_size_ol];

		std::string *s_mcn = new std::string[i_size_ml];

		for (int i = 0; i<i_size_ol; i++) s_ocn[i] = cn[ol[i] - 1]; //-1 for actual row

		for (int i = 0; i<i_size_ml; i++) s_mcn[i] = cn[ml[i] - 1]; //-1 for actual row





		std::string *s_ocn_temp = new std::string[i_size_ol]; //string vector of observed patterns only

		std::string *s_mcn_temp = new std::string[i_size_ml]; //string vector of missing patterns only	

		for (int i = 0; i<i_size_ol; i++) { s_ocn_temp[i] = s_ocn[i]; }

		for (int i = 0; i<i_size_ml; i++) { s_mcn_temp[i] = s_mcn[i]; }

		//sort 		

		std::sort(s_ocn_temp, s_ocn_temp + i_size_ol); //knowing that s_ocn_temp[] has i_size_ol entities

		std::sort(s_mcn_temp, s_mcn_temp + i_size_ml); //knowing that s_mcn_temp[] has i_size_ml entities



													   //------------

													   //memorize observed patterns. Only unique patterns are stored  

													   //------------

		double** uox = New_dMatrix(nrow, ncol);

		double** mox = New_dMatrix(nrow, ncol);



		int i_count_uox = 0; //total number of unique uox 

		std::string s_temp;

		for (int i = 0; i<i_size_ol; i++)

		{

			s_temp = s_ocn_temp[i]; //get a string 

			for (int j = 0; j<nrow; j++) //search all rows 

			{

				//----

				//below condition is needed for finding UNIQUE pattern

				//----

				//if(j==0 && s_temp == cn[j]) 

				//if(i==0 && s_temp == cn[j]) //with first string, find the same string in cn 

				if (i == 0 && s_temp.compare(cn[j]) == 0) //0: equal string

				{

					for (int k = 0; k<ncol; k++)

					{
						uox[i_count_uox][k] = z[j][k];
					} //store the found observed pattern

					i_count_uox++;

					break;

				}

				//if(j>0 && s_temp == cn[j] && s_temp != cn[j-1])

				//if(i>0 && s_temp == cn[j] && s_temp != s_ocn_temp[i-1]) //find UNIQUE matching 

				if (i>0 && s_temp.compare(cn[j]) == 0 && s_temp.compare(s_ocn_temp[i - 1]) != 0)

				{

					for (int k = 0; k<ncol; k++)

					{
						uox[i_count_uox][k] = z[j][k];
					} //store the found observed pattern				

					i_count_uox++;

					break;

				}

			}

		}

		//Now, i_count_uox means the total number of unique observed patterns



		//------------

		//memorize missing patterns 

		//------------

		int i_count_mox = 0; //total number of unique mox 



		for (int i = 0; i<i_size_ml; i++)

		{

			s_temp = s_mcn_temp[i]; //get a string 

			for (int j = 0; j<nrow; j++) //search all rows 

			{

				//----

				//below condition is needed for finding unique pattern

				//----

				//if(j==0 && s_temp == cn[j]) 

				//if(i==0 && s_temp == cn[j]) //with first string, find matching string in cn

				if (i == 0 && s_temp.compare(cn[j]) == 0) //0: equal string 

				{

					for (int k = 0; k<ncol; k++)

					{
						mox[i_count_mox][k] = z[j][k];
					} //store the found missing pattern

					i_count_mox++;

					break;

				}

				//if(j>0 && s_temp == cn[j] && s_temp != cn[j-1])

				//if(i>0 && s_temp == cn[j] && s_temp != s_mcn_temp[i-1]) //find UNIQUE matching string

				if (i>0 && s_temp.compare(cn[j]) == 0 && s_temp.compare(s_mcn_temp[i - 1]) != 0) //0: equal

				{

					for (int k = 0; k<ncol; k++)

					{
						mox[i_count_mox][k] = z[j][k];
					} //store the found missing pattern				

					i_count_mox++;

					break;

				}

			}

		}

		//Now, i_count_mox means the total number of unique missing patterns



		//----------------

		//additional check for unique observed and missing patterns

		//----------------

		//observed patterns//

		d_temp = 0.0;

		double** uox_final = New_dMatrix(nrow, ncol);

		for (int j = 0; j<ncol; j++) { uox_final[0][j] = uox[0][j]; } //first row initialization

		int i_count_uox_final = 1; //starting from the second row



		for (int i = 1; i<i_count_uox; i++) //starting from the second one

		{

			d_temp = 0.0; //initialize 

			for (int j = 0; j<ncol; j++) { d_temp += fabs_FHDI(uox[i][j] - uox[i - 1][j]); } //difference of adjacent rows



			if (d_temp > 1e-3) //adjacent rows are NOT the same each other

			{

				for (int j = 0; j<ncol; j++) { uox_final[i_count_uox_final][j] = uox[i][j]; }

				i_count_uox_final++;

			}

		}

		i_count_uox = i_count_uox_final; //replace with the accurate value

										 //store the final matrix 

		for (int i = 0; i<i_count_uox; i++)

		{

			for (int j = 0; j<ncol; j++)

				uox[i][j] = uox_final[i][j];

		}

		Del_dMatrix(uox_final, nrow, ncol);



		//--------------------------

		//missing patterns//

		//--------------------------

		double** mox_final = New_dMatrix(nrow, ncol);

		for (int j = 0; j<ncol; j++) { mox_final[0][j] = mox[0][j]; } //first row initialization

		int i_count_mox_final = 1; //starting from the second row



		for (int i = 1; i<i_count_mox; i++) //starting from the second one

		{

			d_temp = 0.0; //initialize

			for (int j = 0; j<ncol; j++) { d_temp += fabs_FHDI(mox[i][j] - mox[i - 1][j]); } //difference of adjacent rows



			if (d_temp > 1e-3) //adjacent rows are NOT the same each other

			{

				for (int j = 0; j<ncol; j++) { mox_final[i_count_mox_final][j] = mox[i][j]; }

				i_count_mox_final++;

			}

		}

		i_count_mox = i_count_mox_final; //replace with the accurate value



										 //store the final matrix	

		for (int i = 0; i<i_count_mox; i++)

		{

			for (int j = 0; j<ncol; j++)

				mox[i][j] = mox_final[i][j];

		}

		Del_dMatrix(mox_final, nrow, ncol);


		//!!!!! now uox and mox have the UNIQUE observed and missing patterns

		//!!!!! i_count_mox and _uox have the final number of meaningful rows of mox and uox, respectively

		const int nrm = i_count_mox;

		//const int nru = i_count_uox;



		//-------------------------------------------

		//-------------------------------------------

		//Step 2: Impute missing cells

		//        using all possible donors per missing pattern

		//-------------------------------------------

		//-------------------------------------------

		int* i_temp_x = new int[ncol];

		int i_sum_x = 0;



		std::vector<int> v_mxl; //a row's columns having observed cells  

		rbind_FHDI rbind_icell(ncol); //all possible donor cells 



		std::vector<int> v_cn_z_i;

		//int* zid = NULL;

		//int i_size_zid=0; 

		//int i_loc=0;	

		//int* i_srst = new int[nru];

		std::vector<int> loc_srst_nl;

		double* d_temp_cn = new double[ncol];



		//List_FHDI List_ord(nrm); //order records used for variance estimation

		//List_FHDI List_ocsg(nrm); //order records used for variance estimation



		//--------------------------------

		//--------------------------------

		//--------------------------------

		//Main Loop for FEFI and FHDI

		//--------------------------------

		//--------------------------------

		//--------------------------------

		rbind_FHDI rbind_imat_FEFI(7 + 2 * ncol); //large storage that will accumulate fmat from FEFI  

		rbind_FHDI rbind_imat_FHDI(7 + 2 * ncol); //large storage that will accumulate fmat from FHDI





		for (int i = 0; i<nrm; i++)

		{

			//get current row of missing cell 

			for (int j = 0; j<ncol; j++) i_temp_x[j] = (int)mox[i][j];

			i_sum_x = sum_FHDI(i_temp_x, ncol);





			//-------

			//re-initialization for this missing row 

			//-------

			rbind_icell.initialize(ncol);



			//----------------------

			//Condition 1: this row's cells are all missing

			//-----------------------

			if (i_sum_x == 0)

			{

				v_mxl.clear(); //no missing cells  

				rbind_icell.bind_blocks(i_count_uox, ncol, uox); //fine due to row-based copy

			}



			//----------------------

			//Condition 2: this row's cells are partly missing

			//-----------------------

			int nl = 0;

			if (i_sum_x > 0)

			{

				//------

				//number of observed cells on this row

				//------

				nl = 0;

				v_mxl.clear();

				for (int j = 0; j<ncol; j++)

				{

					if (mox[i][j]>0)

					{

						nl++;

						v_mxl.push_back(j + 1); //Actual non-missing cell location 

					}

				}





				//-------

				//indicator matrix that matches the donors

				//srst: row-wise sum of the indicator matrix 

				//-------

				loc_srst_nl.clear(); //re-initialize

									 //Fill_iVector(i_srst, nru, 0); //re-initialize 



									 //for (int j = 0; j<nru; j++)

									 //{

									 //	int i_sum_crst = 0;

									 //	for (int k = 0; k<ncol; k++)

									 //	{

									 //		//Note: in below check, mox is fixed at ith row 

									 //		if (fabs_FHDI(mox[i][k] - uox[j][k])<1e-3) //part of missing cell = obserbed cell 

									 //		{

									 //			i_sum_crst++; // increment if a cell of missing row = obs. cell 

									 //		}

									 //	}

									 //	//---

									 //	//store how many cells of missing row match those of observed row

									 //	//---

									 //	i_srst[j] = i_sum_crst;

									 //	if (i_sum_crst == nl) loc_srst_nl.push_back(j + 1); //Actual location 				

									 //}

				List_nU.get_block_yicheng(i, loc_srst_nl);

				//-----

				//total matching rows

				//-----

				const int i_size_loc_srst_nl = (int)loc_srst_nl.size();

				if (i_size_loc_srst_nl == 0) //error case

				{
					Rprintf("Error! there is no matched cell! \n"); return 0;
				}



				if (i_size_loc_srst_nl > 0)

				{

					double* d_temp_srst = new double[ncol];

					for (int j = 0; j<i_size_loc_srst_nl; j++)

					{

						for (int k = 0; k<ncol; k++)

						{
							d_temp_srst[k] = uox[loc_srst_nl[j] - 1][k];
						}//-1 for actual loc



						rbind_icell.append_block(d_temp_srst); //ncol is the same 

					}

					delete[] d_temp_srst;

				}

			}



			//----------------------------

			//step 3: Assign donors in uox to missing cell 

			//----------------------------

			const int nic = rbind_icell.size_row(); //number of donors for mox[i] in uox

													//std::string s_icn[nic];

			std::string * s_icn = new std::string[nic]; // uox as donors for mox[i] in string format

			double* d_temp_icell = new double[ncol];

			std::string s_icn_temp;

			for (int j = 0; j<nic; j++)

			{

				for (int k = 0; k<ncol; k++) d_temp_icell[k] = rbind_icell(j, k); //get jth row

				Trans1(d_temp_icell, ncol, s_icn_temp); //transform one row

				s_icn[j] = s_icn_temp;

			}

			delete[] d_temp_icell;



			//------------------

			//search locations where icn = name of jp_name

			//------------------

			const int i_size_jp_name = (int)jp_name.size();

			std::vector<double>      v_cp;  //selected joint probability of donors in uox

			std::vector<std::string> v_ncp; //names of the selected joint probability of donors in uox

			v_cp.clear();

			v_ncp.clear();



			for (int j = 0; j<nic; j++)

			{

				s_temp = s_icn[j]; //one donor 

				for (int k = 0; k<i_size_jp_name; k++) //search all names of jp

				{

					if (s_temp.compare(jp_name[k]) == 0) //0 means the same string

					{

						v_cp.push_back(jp_prob[k]); //store the joint probability 

						v_ncp.push_back(jp_name[k]); //store name

						break; //stop searching after finding the first match 

					}

				}

			}


			const int i_size_v_cp = (int)v_cp.size();

			double d_sum_v_cp = 0.0;

			for (int j = 0; j<i_size_v_cp; j++) d_sum_v_cp += v_cp[j];

			if (d_sum_v_cp != 0)

			{

				for (int j = 0; j<i_size_v_cp; j++) v_cp[j] = v_cp[j] / d_sum_v_cp;

			}


			//-----------

			//transform current missing row to string for step 3

			//-----------

			for (int j = 0; j<ncol; j++) d_temp_cn[j] = mox[i][j];

			Trans1(d_temp_cn, ncol, s_temp);

			v_cn_z_i.clear(); //re-initialize 

			which(cn, nrow, s_temp, v_cn_z_i); //Note: Actual location of mox[i] in z is returned

			int i_size_v_cn_z_i = (int)v_cn_z_i.size(); //number of locations in cn having s_temp

			const int mg = i_size_v_cn_z_i; //Note: loc2 =  i_size_v_cn_z_i





											//---------------------

											//select out all cells that have s_icn[1:nic] from cn

											//---------------------

			std::vector<int> v_obsg0; v_obsg0.clear();//actual locations of all donors in cn

			for (int j = 0; j<nic; j++)

			{

				s_temp = s_icn[j];

				for (int k = 0; k<nrow; k++)

				{

					if (s_temp.compare(cn[k]) == 0) //0=equal string

					{
						v_obsg0.push_back(k + 1);
					}//ACTUAL location stored. No exit 

				}

			}

			const int i_size_v_obsg0 = (int)v_obsg0.size();



			//---------------

			//sort the found cells

			//---------------

			int* i_obsg0_sorted = new int[i_size_v_obsg0];

			for (int k = 0; k<i_size_v_obsg0; k++) i_obsg0_sorted[k] = v_obsg0[k];

			std::sort(i_obsg0_sorted, i_obsg0_sorted + i_size_v_obsg0);

			for (int k = 0; k<i_size_v_obsg0; k++) v_obsg0[k] = i_obsg0_sorted[k];

			delete[] i_obsg0_sorted;





			//------------------

			//half-ascending and -descending ordering 

			//------------------

			std::vector<int> v_obsg; //half-asc and desc obsg0 of actual locations of donors in cn



			int* i_ym_return = new int[i_size_v_obsg0]; //half-asc and desc 

			yorder(y, nrow, ncol,

				d_temp_cn,

				v_obsg0, i_ym_return);



			v_obsg.clear();

			for (int j = 0; j<i_size_v_obsg0; j++) v_obsg.push_back(i_ym_return[j]);

			//below is temporary for wrong yorder()

			//for(int j=0; j<i_size_v_obsg0; j++) v_obsg.push_back(v_obsg0[j]); 



			const int i_size_v_obsg = (int)v_obsg.size(); // size of donors in cn

			delete[] i_ym_return;





			//testout

			//RPrint("============= after yorder() ================");

			//RPrint("v_obsg0 :"); RPrint(v_obsg0);

			//RPrint("v_obsg :");  RPrint(v_obsg);



			//-------------

			// find positions of matches between obsg in obsg0

			// then Store them into List 

			//-------------

			std::vector<int> v_rbsg; v_rbsg.clear();

			match_FHDI(v_obsg, v_obsg0, v_rbsg); //get loc stored in v_rbsg

			const int i_size_v_rbsg = (int)v_rbsg.size();

			//store rbsg

			double* d_temp_rbsg = new double[i_size_v_rbsg];

			for (int k = 0; k<i_size_v_rbsg; k++) d_temp_rbsg[k] = v_rbsg[k];

			List_ord.put_block(i, i_size_v_rbsg, d_temp_rbsg); //put into storage as ith row

			delete[] d_temp_rbsg;

			//store obsg

			double* d_temp_obsg = new double[i_size_v_obsg];

			for (int k = 0; k<i_size_v_obsg; k++) d_temp_obsg[k] = v_obsg[k];

			List_ocsg.put_block(i, i_size_v_obsg, d_temp_obsg); //put into storage as ith row

			delete[] d_temp_obsg;



			const int ng = i_size_v_obsg;

			//------------------------------

			//------------------------------

			//Compute Fractional Weights (fwij)

			//Fractional weights for FEFI representing sampling w

			//------------------------------

			//------------------------------

			//std::string cn_obsg[i_size_v_obsg]; //cn at locations of obsg

			std::string * cn_obsg = new std::string[i_size_v_obsg]; //cn at locations of obsg; actual uox as donors for mox[i] 

			for (int k = 0; k<i_size_v_obsg; k++)

			{
				cn_obsg[k] = cn[v_obsg[k] - 1];
			}//-1 for actual location 



			 //-----

			 //make a table of cn at obsg locations

			 //------

			std::vector<std::string> v_table_name_cn_obsg; // unique names of donors for mox[i]

			std::vector<int>         v_table_count_cn_obsg; // occurances of donors for mox[i]


															//Important!!! Note that the v_table_name_cn_obsg will be sorted
															//Make sure the names of selected donors from uox are sorted as well
															//There may be an issue of unmatch of name of donors

			table_cpp(cn_obsg, i_size_v_obsg,

				v_table_name_cn_obsg, v_table_count_cn_obsg);

			//const int i_size_table_cn_obsg = (int)v_table_count_cn_obsg.size(); 

			//------

			//get joint probability of the selected donor 

			//------

			std::vector<int> v_cn_obsg_ncp; //positions of cn_obsg in ncp 


			match_FHDI(cn_obsg, i_size_v_obsg, v_ncp,

				v_cn_obsg_ncp); //Note: Actual locations are returned 

								//Example:
								// cn_obsg = 11, 22,22, 11,22
								// v_ncp = 11, 22
								//v_cn_obsg_ncp = 1, 2, 2, 1, 2

			const int i_size_v_cn_obsg_ncp = (int)v_cn_obsg_ncp.size(); //size of donors in cn


																		//------

																		//calculate fractional weights

																		//------


			double* fwij = new double[i_size_v_cn_obsg_ncp]; //fractional weights 

			double* d_obsp = new double[i_size_v_cn_obsg_ncp]; //joint prob of selected donors

			int*    i_obsn = new int[i_size_v_cn_obsg_ncp]; //counts of the selected donors

															//fwij is in half-ascending and half-descending order
															//Note that v_cp and v_table_name_cn_obsg must be in the ascend orders, or there will be mismacth problem leading to wrong fractional weights
			for (int k = 0; k<i_size_v_cn_obsg_ncp; k++)

			{

				d_obsp[k] = v_cp[v_cn_obsg_ncp[k] - 1];// -1 for actual location 

				i_obsn[k] = v_table_count_cn_obsg[v_cn_obsg_ncp[k] - 1];// -1 for actual location  

				fwij[k] = 1.0; //default for error case  

				if (i_obsn[k] != 0) fwij[k] = d_obsp[k] / i_obsn[k];

				if (i_obsn[k] == 0) Rprintf("Error! zero count in obsn!");

			}

			//testout

			//RPrint("fwij[] :"); RPrint(fwij, i_size_v_cn_obsg_ncp);


			//----------------------

			//FEFI Imputation

			//  Algorithm: impute by using all possible donors

			//  final outcome is "fmat" in which each column means that

			//  col1: id

			//  col2: fid, i.e., id of imputed value

			//  col3: sampling weight

			//  col4: fractional weights 

			//  col5: imputed original data (matrix with column of ncol) 

			//  col6: imputed category data (matrix with column of ncol)

			//  col7: 1:ng 

			//  col8: = col2  (for consistency with FHDI results)

			//  col9: = col3  (for consistency with FHDI results)

			//----------------------

			std::vector<int> v_obsg_times_mg; v_obsg_times_mg.clear();

			if (s_M.compare("FEFI") == 0) //0=equal string

			{

				double** fmat_FEFI = New_dMatrix(ng*mg, 7 + 2 * ncol); //7columns and two blocks of ncol 



				Fully_Efficient_Fractional_Imputation(ng, mg,

					v_obsg, v_mxl,

					y, z, nrow, ncol,

					v_cn_z_i, fwij, i_size_v_cn_obsg_ncp,

					w, id,

					fmat_FEFI);



				//testout

				//RPrint("in ==== M=FEFI =after making fmat_FEFI[][]====");

				//RPrint("fmat_FEFI : "); RPrint(fmat_FEFI, ng*mg, 7+2*ncol);



				//------------------

				//Append fmat_FEFI onto global storage imat

				//------------------

				rbind_imat_FEFI.bind_blocks(ng*mg, 7 + 2 * ncol, fmat_FEFI);



				//-----------------------

				//local deallocation

				//-----------------------

				Del_dMatrix(fmat_FEFI, ng*mg, 7 + 2 * ncol);

			}





			//------------------------------------

			//FHDI

			//Fractional Hot Deck Imputation

			//------------------------------------

			//if(s_M.compare("FHDI") == 0) //0= equal string



			const int i_mxl = (int)v_mxl.size();



			//prepare return matrix. Note the different row size from fmat of FEFI 

			int i_row_fmat_FHDI = i_M*mg; //default row size of return matrix of FHDI

			if (i_size_v_obsg <= i_M) i_row_fmat_FHDI = i_size_v_obsg*mg;  //if donors are less than i_M



			if (s_M.compare("FHDI") == 0) //0= equal string

			{

				double** fmat_FHDI = New_dMatrix(i_row_fmat_FHDI, 7 + 2 * ncol); //return matrix from FHDI

																				 //below random number between 0 and 1 should use appropriate library depending upon version	

																				 //d_myran = static_cast<double>(std::rand())/static_cast<double>(RAND_MAX); //Window ver

				d_myran = Rf_runif(0.0, 1.0); //R package version 

											  //testout

											  // Rprintf("d_myran: "); Rprintf("%g ", d_myran); 
											  //cout<<"rand(): "<<std::rand()<<endl;
											  //cout << "RAND_MAX: " << RAND_MAX << endl;
											  //cout<<"d_myran: "<< d_myran <<endl;

				Fractional_Hot_Deck_Imputation(i,

					ng, List_ocsg, ncol,

					mox, y, nrow, i_M,

					mg, z, i_mxl,

					v_cn_z_i, v_mxl,

					v_obsg,

					fwij, i_size_v_cn_obsg_ncp,

					d_obsp, i_obsn,

					d_myran,

					w, id,

					fmat_FHDI);



				//------------------

				//Append fmat_FHDI onto global storage imat

				//------------------

				rbind_imat_FHDI.bind_blocks(i_row_fmat_FHDI, 7 + 2 * ncol, fmat_FHDI);



				//-----------------------

				//local deallocation

				//-----------------------

				Del_dMatrix(fmat_FHDI, i_row_fmat_FHDI, 7 + 2 * ncol);

			}



			//-----------------------

			//local deallocation

			//-----------------------

			delete[] s_icn;

			delete[] cn_obsg;

			delete[] fwij;

			delete[] d_obsp;

			delete[] i_obsn;



		} //end of Main loop for all rows of missing patterns 





		  //---------------

		  //---------------

		  //Step 4: construct output results

		  //---------------

		  //------------------------------------------------------

		  //ipmat  = final imputation results

		  //     	col1: ID 	= unit index

		  //		col2: FID 	= ID of fractionally imputed value

		  // 		col3: WGT 	= weight 

		  //		col4: FWGT	= Frational weight

		  //		col5: Variables 

		  //		col6: Responses

		  //irmat  = imputation results related to the categorized matrix 

		  //     	col1: ID 	= unit index

		  //		col2: FID 	= ID of fractionally imputed value

		  //		col3: OID	= original rank of the imputed value

		  //		col4: ORDER = SN(selected donor)

		  //		col5: FEFIW	= Fefi weights 

		  //		col6: CELL	= cells 

		  //----------------------------------------------------

		  //FEFI                           FEFI //

		  //get ipmat, Resp (separately), irmat from FEFI results 

		  //rbind_FHDI  rbind_ipmat_FEFI(4+ncol); //column size is 4+ncol

		  //rbind_FHDI  rbind_Resp_FEFI(ncol+1);  //separate response matrix  

		  //rbind_FHDI  rbind_irmat_FEFI(5+ncol); //column size is 5+ncol

		if (s_M.compare("FEFI") == 0) //0= equal string

		{

			Results_Fully_Efficient_Fractional_Imputation(i_size_ol,

				ncol, nrow,

				id_ol, w_ol, d_oy, d_ox,

				rbind_imat_FEFI, r,



				rbind_ipmat_FEFI, rbind_Resp_FEFI, rbind_irmat_FEFI);

			//testout

			/*

			RPrint("after Results_... rbind_ipmat_FEFI after binding :");

			rbind_ipmat_FEFI.print_rbind_FHDI();

			RPrint("after Results_... rbind_Resp_FEFI after binding :");

			rbind_Resp_FEFI.print_rbind_FHDI();

			RPrint("after Results_... rbind_irmat_FEFI after binding :");

			rbind_irmat_FEFI.print_rbind_FHDI();

			*/

		}



		//FHDI ------------------------- FHDI //

		//get ipmat, Resp (separately), irmat from FHDI results 

		//rbind_FHDI  rbind_ipmat_FHDI(4+ncol); //column size is 4+ncol

		//rbind_FHDI  rbind_Resp_FHDI(ncol+1);  //separate response matrix  

		//rbind_FHDI  rbind_irmat_FHDI(5+ncol); //column size is 5+ncol



		if (s_M.compare("FHDI") == 0) //0= equal string

		{

			Results_Fractional_Hot_Deck_Imputation(i_size_ol,

				ncol, nrow,

				id_ol, w_ol, d_oy, d_ox,

				rbind_imat_FHDI, r,



				rbind_ipmat_FHDI, rbind_Resp_FHDI, rbind_irmat_FHDI);

			//testout

			/*

			RPrint("after Results_... rbind_ipmat_FHDI after binding :");

			rbind_ipmat_FHDI.print_rbind_FHDI();

			RPrint("after Results_... rbind_Resp_FHDI after binding :");

			rbind_Resp_FHDI.print_rbind_FHDI();

			RPrint("after Results_... rbind_irmat_FHDI after binding :");

			rbind_irmat_FHDI.print_rbind_FHDI();

			*/

		}



		//------

		//prep returns of other matrices

		//------

		rbind_uox.bind_blocks(i_count_uox, ncol, uox);

		rbind_mox.bind_blocks(i_count_mox, ncol, mox);



		//testout

		Rprintf(" ========= FHDI KNN has successfully finished! \n");





		//----------------

		//Deallocation

		//----------------

		delete[] cn;

		delete[] s_ocn;

		delete[] s_mcn;

		delete[] s_ocn_temp;

		delete[] s_mcn_temp;



		delete[] i_rn;

		//lete[] w; 

		//lete[] id; 

		Del_dMatrix(d_oy, i_size_ol, ncol);

		Del_dMatrix(d_my, i_size_ml, ncol);

		Del_dMatrix(d_ox, i_size_ol, ncol);

		Del_dMatrix(d_mx, i_size_ml, ncol);



		delete[] w_ml;

		delete[] w_ol;

		delete[] id_ml;

		delete[] id_ol;



		Del_dMatrix(uox, nrow, ncol);

		Del_dMatrix(mox, nrow, ncol);



		delete[] i_temp_x;

		//delete[] i_srst;

		delete[] d_temp_cn;





		return 1;



	}



} //end of namespace



//Fn===========================================================================

//Variance_Est_FEFI_Extension_cpp.cc-----------------------------------------------------------------------------

//Fn===========================================================================

namespace FHDI

{

void RepWeight(const int n, double** d_rw)

//Description -------------------------------------

// Jackknife replicate weight for simpler random sampling

// 

// original R code: Dr. Im, J. and Dr. Kim, J. 

// c++ code: 		Dr. Cho, I. 

// All rights reserved

// 

// updated: Nov 17, 2016

//IN   : int n = matrix dimension

//OUT  : double d_rw[n,n] = replicate weights 

//--------------------------------------------------

{

	

	const double d_rw0 = (1.0*n)/(n-1);



	//--------

	//initialize

	//--------

	Fill_dMatrix(d_rw, n, n, d_rw0);

	

	//--------

	//put 0 into diagonal terms

	//--------

	for(int i=0; i<n; i++)

	{

		d_rw[i][i] = 0.0; 

	}

	

	return;

}



bool Rep_CellP(double** d_cx, const int nrow, const int ncol, 
               FHDI::RepWeight_FHDI &d_rw, int*  id, 

               

			   List_FHDI        &List_rst_prob,

			   List_string_FHDI &List_rst_name,

			   std::vector<std::string> &s_ncx)

//Description============================================

// compute cell probability using replicate weight rw

// 

// R code: Dr. Im, J., and Dr. Kim, J. 

// C++   : Dr. Cho, I.

// All rights reserved

// Last update: March 28, 2017

//



//IN   : double d_cx[nrow, ncol] = categoraized matrix

//IN   : double d_rw[nrow, nrow] = replicate weights

//IN   : int    id[nrow] = index of rows

//

//below two lists have meaningful values up to i_nc rows  

//OUT  : List_FHDI List_rst_prob(nrow->i_nc); //list of joint probabilities for all missing patterns 

//OUT  : List_string_FHDI List_rst_name(nrow->i_nc); //names of joint probabilities for all missing patterns 

//OUT  : std::vector<std::string> s_ncx; //uniqe cn0

//======================================================== 

{

	//--------------

	//make a condensed expression "cn0" of cx, i.e. z

	//--------------

	//std::string cn0[nrow];

	//std::string cn0_backup[nrow];

    std::string *cn0 = new std::string[nrow];	

	std::string *cn0_backup = new std::string[nrow]; 



	Trans(d_cx, nrow, ncol, cn0);



	for(int i=0; i<nrow; i++) cn0_backup[i] = cn0[i]; 

	

	//---------------------

	//SORT & UNIQUE patterns of cn0

	//---------------------

	//std::string s_cn0_temp[nrow]; 

	std::string *s_cn0_temp = new std::string[nrow]; 

	for(int i=0; i<nrow; i++) s_cn0_temp[i] = cn0[i]; 

	std::sort(s_cn0_temp, s_cn0_temp+nrow); 

	

	//------------

	//memorize observed patterns 

	//------------

	//std::vector<std::string> s_ncx; //uniqe cn0

	

	int i_count_cn0 = 0; //total number of unique cn0 

	std::string s_temp ; 

	for(int i=0; i<nrow; i++)

	{

		s_temp = s_cn0_temp[i]; //get a string from the sorted strings 

		for(int j=0; j<nrow; j++) //search all rows 

		{

			//----

			//below condition is needed for finding UNIQUE pattern

			//----

			if(s_temp.compare(cn0_backup[j]) == 0) //0: equal string

			{

				s_ncx.push_back(cn0_backup[j]);  //store the found observed pattern

				

				//----------

				//remove all identical string after the current string

				//----------

				for(int k=j; k<nrow; k++)

				{

					if(s_temp.compare(cn0_backup[k]) == 0) //0: equal string

					{

						cn0_backup[k] = ""; //nullify for the next search

					}

				}

				

				i_count_cn0++; 

				break; 

			}



		}

	}

	//Now, i_count_cn0 means the total number of unique sorted strings

	const int i_nc = i_count_cn0; 





	

	//-----------------------------

	//calculate joint probability and names of all missing patterns

	//using the Jackknife replicate weights

	//------------------------------

	//List_FHDI        List_rst_prob(i_nc); //list of joint probabilities for all missing patterns 

	//List_string_FHDI List_rst_name(i_nc); //names of joint probabilities for all missing patterns 



	std::vector<double> jp_prob_return; 

	std::vector<std::string> jp_name_return;

	//std::vector<double> w_UserDefined; 

	double* w_UserDefined = new double[nrow]; 



	for(int i=0; i<i_nc; i++)

	{

		//---

		//search current missing pattern from all strings

		//---

		std::string s_temp = s_ncx[i]; 

		int i_loc = 0; 

		for(int j=0; j<nrow; j++) 

		{

			if(s_temp.compare(cn0[j])==0)

			{

				i_loc = j; 

				break; 

			}

		}

		

		//----

		//joint probability and names

		//----

		jp_prob_return.clear(); 

		jp_name_return.clear();

		//w_UserDefined.clear();

		//for(int j=0; j<nrow; j++) w_UserDefined.push_back(d_rw[j][i_loc]) ; 

		for(int j=0; j<nrow; j++) w_UserDefined[j] = d_rw(j, i_loc); //previous: d_rw[j][i_loc];

		

		bool b_success_CellProb = Cell_Prob_Extension_cpp(d_cx, nrow, ncol,

							    jp_prob_return,

							    jp_name_return, 

							    w_UserDefined, id);		

		if(!b_success_CellProb)
		{			
			Rprintf("Error! Cell Prob Failed! Change k, check data quality, further break down categorical variables, or so. It may help \n");
			
			return 0; //abnormal ending 								
		}
		
		//---

		//prep return

		//---

		List_rst_prob.put_block(i, jp_prob_return); //jth row has joint prob

		List_rst_name.put_block(i, jp_name_return); //jth row has name of the joint prob

		

	}

		



	delete[] cn0; 

	delete[] cn0_backup; 

	delete[] s_cn0_temp;

	

	delete[] w_UserDefined;

	

	return 1;				   

}





bool Variance_Est_FEFI_Extension_cpp(double** y, double** z, const int nrow, const int ncol, 

	FHDI::RepWeight_FHDI &d_rw, double* w, int* id, 

	rbind_FHDI  &rbind_ipmat_FEFI,

	rbind_FHDI  &rbind_Resp_FEFI,

	rbind_FHDI  &rbind_irmat_FEFI,

	rbind_FHDI  &rbind_ipmat_FHDI,

	rbind_FHDI  &rbind_Resp_FHDI,

	rbind_FHDI  &rbind_irmat_FHDI,

	rbind_FHDI  &rbind_uox,

	rbind_FHDI  &rbind_mox,

	List_FHDI 	&List_ord,

	List_FHDI 	&List_ocsg, 

	std::string s_M,

	double** wmat)



//Description----------------------

//estimate variance for FEFI using Jackknife method 

//  Algorithm: 

//

// original R code: Dr. Im, J. and Dr. Kim, J. 

// c++ code: 		Dr. Cho, I. 

// All rights reserved

// 

// updated: March 28, 2017

//

//IN   : double y(nrow, ncol)= original data matrix with missing cells 

//IN   : double z(nrow, ncol)= categorized matrix of y. 0 for missing cell

//IN   : double d_rw[nrow, nrow] = replicate weights 

//IN   : double w(nrow) = sampling weight (default = 1.0)

//IN   : int    id(nrow) = id number of each row (default = 1 to nrow)

//FEFI --------returns----------------- FEFI //

//IN   : rbind_FHDI  rbind_ipmat_FEFI(4+ncol); //column size is 4+ncol

//IN   : rbind_FHDI  rbind_Resp_FEFI(ncol+1);  //separate response matrix  

//IN   : rbind_FHDI  rbind_irmat_FEFI(5+ncol); //column size is 5+ncol

//FHDI --------returns----------------- FHDI //

//IN   : rbind_FHDI  rbind_ipmat_FHDI(4+ncol); //column size is 4+ncol

//IN   : rbind_FHDI  rbind_Resp_FHDI(ncol+1);  //separate response matrix  

//IN   : rbind_FHDI  rbind_irmat_FHDI(5+ncol); //column size is 5+ncol

//other matrices

//IN   : rbind_FHDI  rbind_uox(ncol); //observed unique categorized matrix 

//IN   : rbind_FHDI  rbind_mox(ncol); //missing  unique categorized matrix

//Note: below Lists contain meaningful items up to i_count_mox rows  

//IN   : List_FHDI 	List_ord(nrow); //order records used for variance estimation

//IN   : List_FHDI 	List_ocsg(nrow); //order records used for variance estimation

//IN   : std::string s_M = "FEFI" = fully efficient fractional imputation

//						   "FHDI" = fractional hot deck imputation

//

//OUT  : double** wmat = New_dMatrix(nrow_dat2_FEFI, L=nrow); //nrow_dat2_FHDI = rows of w1

//

//Data Structure Note

//----------------------

//dat1 in R version:  id   w  y_matrix  z_matrix

//     in C++ ver  :  id   w  y         z

//----------------------

//dat2 in R version:  id  FID  WGT  FWGT  imputed matrix     Response(0/1)

//     in C++ ver  :  ----- rbind_ipmat_FEFI------------     rbind_Resp_FEFI 

//                 :  ----- rbind_ipmat_FHDI------------     rbind_Resp_FHDI

//----------------------

//

//ipmat  = final imputation results

//     	col1: ID 	= unit index

//		col2: FID 	= ID of fractionally imputed value

// 		col3: WGT 	= weight 

//		col4: FWGT	= Frational weight

//		col5: Variables 

//		col6: Responses (separately in  rbind_Resp_...)

//------------------------

//

//irmat  = imputation results related to the categorized matrix 

//     	col1: ID 	= unit index

//		col2: FID 	= ID of fractionally imputed value

//		col3: OID	= original rank of the imputed value

//		col4: ORDER = SN(selected donor)

//		col5: FEFIW	= Fefi weights 

//		col6: CELL	= cells 

//----------------------

{

	

	//----------------------------

	//Basic constants declaration

	//----------------------------

	const int nrow_dat2_FEFI = rbind_ipmat_FEFI.size_row(); 

	const int nrow_dat2_FHDI = rbind_ipmat_FHDI.size_row(); 	

	const int nrow_mox 		 = rbind_mox.size_row(); 

	const int L = nrow; //size of d_rw 

	

	//--------------------

	//get ready id table of FEFI

	//--------------------

	double* d_id_FEFI = new double[nrow_dat2_FEFI]; 

	for(int i=0; i<nrow_dat2_FEFI; i++) d_id_FEFI[i] = rbind_ipmat_FEFI(i,0); //id, 1st col 

	std::vector<double> v_table_name_id_FEFI; //same as "nimp" in R version

	std::vector<int>    v_table_count_id_FEFI;//same as "nimp" in R version 

	table_cpp(d_id_FEFI, nrow_dat2_FEFI, v_table_name_id_FEFI, v_table_count_id_FEFI);  





	//---------------------

	//imputed real data matrix

	//---------------------

	int nrow_d_iy = nrow_dat2_FEFI; //default

	if(s_M== "FDFI") {nrow_d_iy = nrow_dat2_FEFI;}

	if(s_M== "FHDI") {nrow_d_iy = nrow_dat2_FHDI;}

	double** d_iy = New_dMatrix(nrow_d_iy, ncol); //imputed matrix of real values 

	for(int i=0; i<ncol; i++)

	{

		for(int j=0; j<nrow_d_iy; j++)

		{

			if(s_M == "FEFI")

			{d_iy[j][i] = rbind_ipmat_FEFI(j,4+i);} //col5~ncol contains imputed real values 

			if(s_M == "FHDI")

			{d_iy[j][i] = rbind_ipmat_FHDI(j,4+i);} //col5~ncol contains imputed real values 



		}

	}



	

	//----------------------

	//categorized matrix

	//----------------------

	double** d_cx = New_dMatrix(nrow, ncol);

	Copy_dMatrix(z, nrow, ncol, d_cx);



	

	//---------------------

	//ocg, observed donors for each missing pattern. 

	//---------------------

    //the same as List_ocsg[nrow_mox]

	//---------------------

	int* i_locg = new int[nrow_mox]; //length of each list of ocg

	for(int i=0; i<nrow_mox; i++)

	{

		int i_temp = 0; 

		List_ocsg.get_a_row_size(i, i_temp);

		i_locg[i] = i_temp; 

	}



	

	//------------------------

	//cell probability using replicate weight

	//------------------------

    List_FHDI         List_rst_prob(nrow); //only i_nc rows are meaningful

    List_string_FHDI  List_rst_name(nrow); //only i_nc rows are meaningful

    std::vector<std::string> s_ncx;

	

    bool b_success_Rep_CellP = Rep_CellP(d_cx, nrow, ncol, d_rw, id, 

			  List_rst_prob,

			  List_rst_name,

			  s_ncx);

	if(!b_success_Rep_CellP)
	{			
		Rprintf("Error! Rep_CellP Failed! Change k, check data quality, further break down categorical variables, or so. It may help \n");
		
		return 0; //abnormal ending 								
	}
	



	//--------------------

	//put 1 into fully observed rows

	//--------------------

	double* d_rr0 = new double[nrow];

	for(int i=0; i<nrow; i++)

	{

		double d_prod = 1.0; 

		for(int j=0; j<ncol; j++) 

		{

			d_prod = d_prod*d_cx[i][j]; 

		}

		//-----

		//0: at least one missing;  1: all observed

		//-----

		d_rr0[i] = d_prod; 

		if(fabs_FHDI(d_prod) >0.0) d_rr0[i] = 1; 

	}

	

	//--------------

	//calculate w1 = sampling weight

	//--------------

	//std::string cn[nrow]; 

	std::string *cn = new std::string[nrow]; 

	Trans(d_cx, nrow, ncol, cn);

    double* d_w1 = new double[nrow_dat2_FEFI]; 

	for(int i=0; i<nrow_dat2_FEFI; i++) 

		d_w1[i] = rbind_ipmat_FEFI(i,2); //3rd column contains WGT

	

	

	//------------------

	//make Covariance matrix for following replication process

	//------------------

	int* i_lloc; 

	std::vector<int> v_mox_0; 

	double** d_dy; 

	double** V_var; //covariance matrix of dy

	List_FHDI List_V(nrow_mox); //storage of covariance matrix

								//Note: store cov mat by "row-first" rule 

	for(int i=0; i<nrow_mox; i++)

	{

		int i_size_lloc = i_locg[i]; //length of the ocg associated with current missing row

		

		//-----------

		//when zero size continue to next iteration

	    //-----------

		if(i_size_lloc <= 0) {continue;}

		i_lloc = new int[i_size_lloc]; 

		for(int j=0; j< i_size_lloc; j++) i_lloc[j] =(int)List_ocsg(i,j); //ith row, jth entity

		

		//-----

		//find missing column in current row

		//------

		v_mox_0.clear();

		for(int j=0; j<ncol; j++) 

		{

			if(rbind_mox(i,j) == 0.0) v_mox_0.push_back(j+1); //ACTUAL zero column id 

		}

		const int i_size_v_mox_0 = (int)v_mox_0.size(); 

		

		//-------

		//extract matrix of missing patterns

		//-------

		d_dy  = New_dMatrix(i_size_lloc, i_size_v_mox_0);

		V_var = New_dMatrix(i_size_v_mox_0, i_size_v_mox_0); //column-wise covariance 

		for(int j=0; j< i_size_lloc; j++)

		{

			for(int k=0; k<i_size_v_mox_0; k++)

			{

				d_dy[j][k] = y[i_lloc[j]-1][v_mox_0[k]-1]; //-1 for actual location 

			}

		}

		//----------

		//"Estimated covariance" of d_dy by column-to-column method

		//----------

		cov_FHDI(d_dy, i_size_lloc, i_size_v_mox_0, V_var); 



		List_V.put_block(i, i_size_v_mox_0, i_size_v_mox_0, V_var); //direct matrix saving



		//---

		//local deallocation

		//---

		delete[] i_lloc; 

		Del_dMatrix(d_dy,  i_size_lloc, i_size_v_mox_0);

		Del_dMatrix(V_var, i_size_v_mox_0, i_size_v_mox_0); 

		//delete[] d_V_temp; 

		

	}

	

	//------------------------------

	//------------------------------

	//MAIn loop for L replications

	//------------------------------

	//------------------------------

	double* rw0 = new double[nrow];

	

	int i_sum_Rw = 0; 

	for(int i=0; i<nrow; i++) i_sum_Rw+= v_table_count_id_FEFI[i]; 

	double* Rw  = new double[i_sum_Rw];

	

	double* wijk = new double[nrow_dat2_FEFI]; //FWGT from ipmat 



	

	for(int l=0; l<L; l++)

	{

		

		//-------

		//replicate weight from lth column

		//-------

		for(int i=0; i<nrow; i++) rw0[i] = d_rw(i, l);  //previous: d_rw[i][l]; //l_th column 

		int i_sum =0; 

		for(int i=0; i<nrow; i++)

		{

			for(int j=0; j<v_table_count_id_FEFI[i]; j++) Rw[i_sum++]=rw0[i];

		}			



		//---------

		//FWGT of ipmat

		//----------

		for(int i=0; i<nrow_dat2_FEFI; i++)

			wijk[i] = rbind_ipmat_FEFI(i,3); //4th column is FWGT 

		

		//----------

		//joint probability associated with current string

		//-----------

		std::string cn_current = cn[l]; //lth string 

		std::vector<int> v_ncx_cn; 

		which(s_ncx, cn_current, v_ncx_cn); //actual location 

		//const int i_size_v_ncx_cn = (int)v_ncx_cn.size(); //MUST BE "1"

		

		int i_size_cellp = 0;

		List_rst_prob.get_a_row_size(v_ncx_cn[0]-1, i_size_cellp); //get a size of the row in the list 

		

		//-----------

		//when zero size continue to next iteration

	    //-----------

		if(i_size_cellp <= 0) {continue;}

		double* d_cellp = new double[i_size_cellp]; 

		List_rst_prob.get_block(v_ncx_cn[0]-1, d_cellp); //-1 for actual row location 

		

		

		//----------------------------------------

		//1. if the deleted is missing unit, no further action is taken

		//2. if the deleted is observed unit, then the fractional weights are re-computed 

		//----------------------------------------

		int* idd = new int[nrow_mox]; //location of the deleted donor in ocg 

		Fill_iVector(idd, nrow_mox, 0); 

		if(fabs_FHDI(d_rr0[l]) > 0)

		{

			//---------------------

			//locations of the deleted unit in observed list

			//---------------------

			std::vector<int> v_lg; //Actual locations 

			v_lg.clear(); 

			for(int j=0; j<nrow_mox; j++) //list length 

			{

				for(int k=0; k<i_locg[j]; k++) //a row in the List

				{

					int i_temp_lg = (int)List_ocsg(j,k); 

					if( i_temp_lg == (l+1)) //+1 for actual location  

					{

						v_lg.push_back(j+1); //actual row location 

						idd[j] = k+1; //actual location 

						break; 

					}

				}

			}

			const int nlg = (int)v_lg.size(); 

			

			

			//--------------------------

			//Adjust fractional weights for all units in lg

			//--------------------------

			if(nlg>0)

			{

				for(int j=0; j<nlg; j++)

				{

					int i_row_lg = v_lg[j]-1; // row number [0,...) 

					double* d_1_mox = new double[ncol];

					for(int k=0; k<ncol; k++) d_1_mox[k] = rbind_mox(i_row_lg,k);

					

					//---

					//actual col number of missing cell in current missing row

					//---

					std::vector<int> v_rloc; v_rloc.clear(); 

					for(int k=0; k<ncol; k++) 

					{

						if(d_1_mox[k] == 0.0) {v_rloc.push_back(k+1);} //actual col

					}	

					//const int nrloc = (int)v_rloc.size();

					std::string cng; 

					Trans1(d_1_mox, ncol, cng); 

					

					//-------

					//location of cn which has cng

					//-------

					std::vector<int> v_mlog; v_mlog.clear();

					which(cn, nrow, cng, v_mlog); 

					const int nmlog = (int)v_mlog.size(); 

					

					

					//------------------------

					//------------------------

					//FEFI

					//------------------------

					//------------------------

					std::vector<int> v_elog; 

					if(s_M.compare("FEFI")==0) //0=equal 

					{

						//-----

						//find locations of mlog in dat2$ID

						//v_mlog contains the row numbers that have the same string as

						//current missing row 

						//nmlog = n(v_mlog)

						//-----

						v_elog.clear(); 

						for(int k1=0; k1<nmlog; k1++) //loop for mlog

						{

							int i_temp1 = id[v_mlog[k1]-1]; //dat1$ID in R version  

							for(int k2=0; k2<nrow_dat2_FEFI; k2++)

							{

								int i_temp2 = rbind_ipmat_FEFI(k2, 0); //1st col is dat2$ID

								if(i_temp1 == i_temp2)

								{

									v_elog.push_back(k2+1); //actual location 

								}

							}

						}

						const int i_size_v_elog = (int)v_elog.size(); 

						

						//----

						//donor id

						//-----

						std::vector<int> v_did; v_did.clear(); 

						

						int i_temp_lg_j = v_lg[j]-1; //-1 for actual location  

						for(int k1=0; k1<i_locg[i_temp_lg_j]; k1++) //a row in the List 

						{

							v_did.push_back((int)List_ocsg(i_temp_lg_j, k1));	

						}

						const int i_size_nic = (int)v_did.size(); //length of did 

						

						//----

						//donor string patterns

						//----

						//std::string s_icn[i_size_nic];

						std::string *s_icn = new std::string[i_size_nic];

						for(int k1=0; k1<i_size_nic; k1++)

							s_icn[k1] = cn[v_did[k1]-1]; //-1 for actual location  		

						

						

						//------

						//unique icn

						//-------

						std::vector<std::string> v_unique_icn; v_unique_icn.clear(); 

						//std::string s_icn_backup[i_size_nic];

						std::string *s_icn_backup = new std::string[i_size_nic];

						for(int k1=0; k1<i_size_nic; k1++) s_icn_backup[k1] = s_icn[k1]; 

						

						for(int k1=0; k1<i_size_nic; k1++)

						{

							std::string s_uicn_temp = s_icn[k1]; 

							for(int k2=0; k2<i_size_nic; k2++)

							{

								if(s_uicn_temp.compare(s_icn_backup[k2]) == 0)

								{

									//store the found unique string pattern 

									v_unique_icn.push_back(s_uicn_temp); 

									

									//nullify all the remaining unit that has the same string

									for(int k3=k2; k3<i_size_nic; k3++) 

									{

										if(s_uicn_temp.compare(s_icn_backup[k3])==0) 

											s_icn_backup[k3] = ""; //nullify

									}

									

									break; 

								}									

							}

						}

						const int i_unic = (int)v_unique_icn.size(); 

						

						

						//----------

						//sort the unique donors

						//----------

						//std::string s_unique_icn_sorted[i_unic]; //sorted donors

						std::string *s_unique_icn_sorted = new std::string[i_unic]; //sorted donors

						for(int k1=0; k1<i_unic; k1++) s_unique_icn_sorted[k1] = v_unique_icn[k1];

						std::sort(s_unique_icn_sorted, s_unique_icn_sorted+i_unic);



						//---------

						//match the sorted unique donors to njp

						//---------

						//make "njp" 

						int i_temp2=0; List_rst_name.get_a_row_size(0, i_temp2); 

						//std::string s_njp[i_temp2]; 

						std::string *s_njp = new std::string[i_temp2]; 

						for(int k1=0; k1<i_temp2; k1++) s_njp[k1] = List_rst_name(0, k1); 

						

						std::vector<int> v_icn_njp; v_icn_njp.clear(); 

						for(int k1=0; k1<i_unic; k1++)

						{

							std::string s_temp_cp = s_unique_icn_sorted[k1];

							

							for(int k2=0; k2<i_temp2; k2++)

							{

								if(s_temp_cp.compare(s_njp[k2]) == 0) 

								{ v_icn_njp.push_back(k2+1); break; } //+1 for actual location 

							}

						}

						const int i_size_v_icn_njp = (int)v_icn_njp.size(); 

						

						

						//-------------------------------------

						//get joint probability at the matched location only

						//-------------------------------------

						//-----------

						//when zero size continue to next iteration

						//-----------

						if(i_size_v_icn_njp <= 0) {continue;}						

						double* d_cp = new double[i_size_v_icn_njp]; //joint prob

						//std::string s_ncp[i_size_v_icn_njp];          //names of the matches

						std::string *s_ncp = new std::string[i_size_v_icn_njp];          //names of the matches

						double d_sum_cp = 0.0; 

						for(int k1=0; k1<i_size_v_icn_njp; k1++)

						{

							d_cp[k1] = d_cellp[v_icn_njp[k1]-1]; //-1 for actual location 

							s_ncp[k1] = s_njp[v_icn_njp[k1]-1]; 

							

							d_sum_cp += d_cp[k1]; //summation of d_cp[]

						}

						for(int k1=0; k1<i_size_v_icn_njp; k1++)

						{ d_cp[k1] = d_cp[k1]/d_sum_cp; }

					

						//-----------

						//match donors 

						//-----------

						std::vector<int> v_obsp; v_obsp.clear(); 

						std::vector<double> v_obsp_cp; v_obsp_cp.clear(); 

						match_FHDI(s_icn, i_size_nic, 

								   s_ncp,     i_size_v_icn_njp, 

		                           v_obsp);

						const int i_size_v_obsp = (int)v_obsp.size();

						for(int k1=0; k1<i_size_v_obsp; k1++)

							v_obsp_cp.push_back(d_cp[v_obsp[k1]-1]);

						

						//---------------------------------------

						//Replicated sampling weights for donors

						//---------------------------------------

						//-----------

						//when zero size continue to next iteration

						//-----------

						if(i_size_nic <= 0) {continue;}												

						double* d_drw0 = new double[i_size_nic]; //length of did

						double d_sum_drw0=0.0; 

						for(int k1=0; k1<i_size_nic; k1++) 

						{ 

							d_drw0[k1] = rw0[v_did[k1]-1]; 	

							d_sum_drw0 += d_drw0[k1]; 

						}



						//----------

						//update weighted probability

						//----------

						std::vector<std::string> jp_name_icn; jp_name_icn.clear(); 

						std::vector<double> 	 jp_prob_icn; jp_prob_icn.clear(); 

						

						wpct_FHDI(s_icn, i_size_nic, d_drw0, 

								  jp_name_icn, jp_prob_icn); 

						const int i_size_jp_prob_icn = (int)jp_prob_icn.size(); //=i_unic

						for(int k1=0; k1<i_size_jp_prob_icn; k1++) 

							jp_prob_icn[k1] = jp_prob_icn[k1]*d_sum_drw0;  //ws.icn

						



						const int i_current_locg = i_locg[v_lg[j]-1]; //-1 for actual loc

						//-----------

						//when zero size continue to next iteration

						//-----------

						if(i_size_v_obsp <= 0) {continue;}

						double* d_Fefiw = new double[i_size_v_obsp]; 

						//---------------

						//weight update I: at deleted donor locations

						//---------------

						if(i_unic == i_current_locg)

						{

							for(int k1=0; k1<i_size_v_obsp; k1++)

								d_Fefiw[k1] = v_obsp_cp[k1];

						}

						//-----------------------------------------------

						//weight update II: at deleted donor locations

						//-----------------------------------------------

						//-----------

						//when zero size continue to next iteration

						//-----------

						if(i_size_nic <= 0) {continue;}						

						double* d_drw1 = new double[i_size_nic];

						Fill_dVector(d_drw1, i_size_nic, 0.0); //initialized with 0

						if(i_unic < i_current_locg)

						{

							//-----------

							//match donors 

							//-----------

							std::vector<int> v_temp; v_temp.clear(); 

							match_FHDI(s_icn, i_size_nic, 

									   s_unique_icn_sorted, i_unic, 									  

									   v_temp);

							

							for(int k1=0; k1<i_size_nic; k1++)

							{

								if(jp_prob_icn[v_temp[k1]-1] != 0.0)

									d_drw1[k1] = d_drw0[k1]/jp_prob_icn[v_temp[k1]-1];

							}

							

							//------------

							//final updated weights

							//------------

							for(int k1=0; k1<i_size_nic; k1++)

								d_Fefiw[k1] = v_obsp_cp[k1]*d_drw1[k1]; 



							

						}

						

						//------------

						//update wijk: repeated copy, if needed

						//------------

						int i_cycle_wijk = (int)floor(i_size_v_elog*1.0/i_size_nic*1.0); //expected evenly divisible

						int i_loc_elog = 0; //sequential id





						for(int k1=0; k1<i_cycle_wijk ;k1++)

						{

							for(int k2=0; k2<i_size_nic; k2++)

							{

								wijk[v_elog[i_loc_elog++]-1] = d_Fefiw[k2]; 

							}

						}

						

						//---

						//local deallocation

						//---

						delete[] s_icn; 

						delete[] s_icn_backup; 

						delete[] d_cp; 

						delete[] d_drw0; 

						delete[] d_Fefiw;

						delete[] d_drw1;						

					}

					

					//local deallocation 

					delete[] d_1_mox; 

				}

			}

			

		}

		

		

		//-------------------------------

		//store the updated weights

		//-------------------------------

		int i_nrow_imputation = nrow; 

		if(s_M == "FEFI") i_nrow_imputation = nrow_dat2_FEFI;

		if(s_M == "FHDI") i_nrow_imputation = nrow_dat2_FHDI;

		

		for(int k1=0; k1<i_nrow_imputation; k1++)

		{

			wmat[k1][l] = Rw[k1]*wijk[k1]; 

		}

		

		

		//--------------------

		//local deallocation

		//--------------------

		delete[] idd; 

		delete[] d_cellp;

		

		

		

	} //end of main loop for L

	//testout

	Rprintf(" ========= Variance estimation has successfully finished!\n");

	

				  

	

	//-------------

	//deallocation

	//-------------

	//delete[] w; 

	//delete[] id; 

	delete[] cn; 

	delete[] d_id_FEFI; 

	Del_dMatrix(d_iy, nrow_d_iy, ncol);

	Del_dMatrix(d_cx, nrow, ncol);

	delete[] i_locg; 

	delete[] d_rr0; 

	delete[] d_w1;

	//Del_dMatrix(wmat, nrow_dat2_FEFI, L);

	delete[] rw0; 

	delete[] Rw; 

	delete[] wijk;

	

	return 1;

}



}//end of namespace


 //Fn===========================================================================

 //Variance_Est_FEFI_Extension_Bigp_cpp.cc-----------------------------------------------------------------------------

 //Fn===========================================================================

namespace FHDI
{

	bool Rep_CellP_Bigp(double** d_cx, const int nrow, const int ncol, const int i_option_collapsing, FHDI::RepWeight_FHDI &d_rw, int*  id,

		List_FHDI        &List_rst_prob,
		List_string_FHDI &List_rst_name,
		std::vector<std::string> &s_ncx, int** codes)
		//Description============================================
		// compute cell probability using replicate weight rw
		// 
		// R code: Dr. Im, J., and Dr. Kim, J. 
		// C++   : Dr. Cho, I. and Yicheng Yang
		// All rights reserved
		// Last update: Feb 28, 2020
		//

		//IN   : double d_cx[nrow, ncol] = categoraized matrix
		//IN   : double d_rw[nrow, nrow] = replicate weights
		//IN   : int    id[nrow] = index of rows
		//IN   : int i_option_collapsing = choice of big-p algorithm 
		//                              0= no big-p algorithms
		//                             !0= perform big-p algorithms
		//IN   : int codes(nrow, i_option_collapsing); // storage to record most correlated variables of mox
		//
		//below two lists have meaningful values up to i_nc rows  
		//OUT  : List_FHDI List_rst_prob(nrow->i_nc); //list of joint probabilities for all missing patterns 
		//OUT  : List_string_FHDI List_rst_name(nrow->i_nc); //names of joint probabilities for all missing patterns 
		//OUT  : std::vector<std::string> s_ncx; //uniqe cn0
		//======================================================== 
	{
		//--------------
		//make a condensed expression "cn0" of cx, i.e. z
		//--------------
		//std::string cn0[nrow];
		//std::string cn0_backup[nrow];
		std::string *cn0 = new std::string[nrow];
		std::string *cn0_backup = new std::string[nrow];

		Trans(d_cx, nrow, ncol, cn0);

		for (int i = 0; i<nrow; i++) cn0_backup[i] = cn0[i];

		//---------------------
		//SORT & UNIQUE patterns of cn0
		//---------------------
		//std::string s_cn0_temp[nrow]; 
		std::string *s_cn0_temp = new std::string[nrow];
		for (int i = 0; i<nrow; i++) s_cn0_temp[i] = cn0[i];
		std::sort(s_cn0_temp, s_cn0_temp + nrow);

		//------------
		//memorize observed patterns 
		//------------
		//std::vector<std::string> s_ncx; //uniqe cn0

		int i_count_cn0 = 0; //total number of unique cn0 
		std::string s_temp;
		for (int i = 0; i<nrow; i++)
		{
			s_temp = s_cn0_temp[i]; //get a string from the sorted strings 
			for (int j = 0; j<nrow; j++) //search all rows 
			{
				//----
				//below condition is needed for finding UNIQUE pattern
				//----
				if (s_temp.compare(cn0_backup[j]) == 0) //0: equal string
				{
					s_ncx.push_back(cn0_backup[j]);  //store the found observed pattern

													 //----------
													 //remove all identical string after the current string
													 //----------
					for (int k = j; k<nrow; k++)
					{
						if (s_temp.compare(cn0_backup[k]) == 0) //0: equal string
						{
							cn0_backup[k] = ""; //nullify for the next search
						}
					}

					i_count_cn0++;
					break;
				}

			}
		}
		//Now, i_count_cn0 means the total number of unique sorted strings
		const int i_nc = i_count_cn0;


		//testout
		//RPrint("=====in Rep_CellP ========");
		//RPrint("s_ncx"); RPrint(s_ncx);
		//RPrint("i_nc"); RPrint(i_nc);
		/*cout<<"=====in Rep_CellP ========"<<endl;
		cout<<"s_ncx"<<endl;
		for(int i=0; i<(int)s_ncx.size(); i++) cout<<s_ncx[i]<<" ,  ";
		cout<<endl;
		cout<<"i_nc: "<<i_nc<<endl;
		*/

		//-----------------------------
		//calculate joint probability and names of all missing patterns
		//using the Jackknife replicate weights
		//------------------------------
		//List_FHDI        List_rst_prob(i_nc); //list of joint probabilities for all missing patterns 
		//List_string_FHDI List_rst_name(i_nc); //names of joint probabilities for all missing patterns 

		std::vector<double> jp_prob_return;
		std::vector<std::string> jp_name_return;
		//std::vector<double> w_UserDefined; 
		double* w_UserDefined = new double[nrow];

		for (int i = 0; i<i_nc; i++)
		{
			//---
			//search current missing pattern from all strings
			//---
			std::string s_temp = s_ncx[i];
			int i_loc = 0;
			for (int j = 0; j<nrow; j++)
			{
				if (s_temp.compare(cn0[j]) == 0)
				{
					i_loc = j;
					break;
				}
			}
			//testout
			/*
			cout<<"loop i (1:i_nc) :"<<i<<"  found i_loc:"<<i_loc<<endl;
			if(i==7)
			{
			for(int j_temp=0; j_temp<nrow; j_temp++) cout<<d_rw[j_temp][i_loc]<<",  ";
			}
			cout<<endl;
			*/

			//----
			//joint probability and names
			//----
			jp_prob_return.clear();
			jp_name_return.clear();
			//w_UserDefined.clear();
			//for(int j=0; j<nrow; j++) w_UserDefined.push_back(d_rw[j][i_loc]) ; 
			for (int j = 0; j<nrow; j++) w_UserDefined[j] = d_rw(j, i_loc); //previous: d_rw[j][i_loc];


			bool b_success_CellProb = Cell_Prob_Extension_Bigp_cpp(d_cx, nrow, ncol, i_option_collapsing,
				jp_prob_return,
				jp_name_return,
				w_UserDefined, id, codes);

			if (!b_success_CellProb)
			{
				Rprintf("Error! Cell Prob Failed! Change k, check data quality, further break down categorical variables, or so. It may help \n");

				return 0; //abnormal ending 								
			}

			//---
			//prep return
			//---
			List_rst_prob.put_block(i, jp_prob_return); //jth row has joint prob
			List_rst_name.put_block(i, jp_name_return); //jth row has name of the joint prob

		}

		//testout
		//RPrint("List_rst_name"); List_rst_name.print_List_string_FHDI();
		//RPrint("List_rst_prob"); List_rst_prob.print_List_FHDI();


		delete[] cn0;
		delete[] cn0_backup;
		delete[] s_cn0_temp;

		delete[] w_UserDefined;

		return 1;
	}


	bool Variance_Est_FEFI_Extension_Bigp_cpp(double** y, double** z, const int nrow, const int ncol, const int i_option_collapsing,
		FHDI::RepWeight_FHDI &d_rw, double* w, int* id,
		rbind_FHDI  &rbind_ipmat_FEFI,
		rbind_FHDI  &rbind_Resp_FEFI,
		rbind_FHDI  &rbind_irmat_FEFI,
		rbind_FHDI  &rbind_ipmat_FHDI,
		rbind_FHDI  &rbind_Resp_FHDI,
		rbind_FHDI  &rbind_irmat_FHDI,
		rbind_FHDI  &rbind_uox,
		rbind_FHDI  &rbind_mox,
		List_FHDI 	&List_ord,
		List_FHDI 	&List_ocsg,
		std::string s_M,
		double** wmat, int** codes)

		//Description----------------------
		//estimate variance for FEFI using Jackknife method 
		//  Algorithm: 
		//
		// original R code: Dr. Im, J. and Dr. Kim, J. 
		// c++ code: 		Dr. Cho, I. and Yicheng Yang
		// All rights reserved
		// 
		// updated: Feb 28, 2020
		//
		//IN   : double y(nrow, ncol)= original data matrix with missing cells 
		//IN   : double z(nrow, ncol)= categorized matrix of y. 0 for missing cell
		//IN   : double d_rw[nrow, nrow] = replicate weights 
		//IN   : double w(nrow) = sampling weight (default = 1.0)
		//IN   : int    id(nrow) = id number of each row (default = 1 to nrow)
		//FEFI --------returns----------------- FEFI //
		//IN   : rbind_FHDI  rbind_ipmat_FEFI(4+ncol); //column size is 4+ncol
		//IN   : rbind_FHDI  rbind_Resp_FEFI(ncol+1);  //separate response matrix  
		//IN   : rbind_FHDI  rbind_irmat_FEFI(5+ncol); //column size is 5+ncol
		//FHDI --------returns----------------- FHDI //
		//IN   : rbind_FHDI  rbind_ipmat_FHDI(4+ncol); //column size is 4+ncol
		//IN   : rbind_FHDI  rbind_Resp_FHDI(ncol+1);  //separate response matrix  
		//IN   : rbind_FHDI  rbind_irmat_FHDI(5+ncol); //column size is 5+ncol
		//other matrices
		//IN   : rbind_FHDI  rbind_uox(ncol); //observed unique categorized matrix 
		//IN   : rbind_FHDI  rbind_mox(ncol); //missing  unique categorized matrix
		//Note: below Lists contain meaningful items up to i_count_mox rows  
		//IN   : List_FHDI 	List_ord(nrow); //order records used for variance estimation
		//IN   : List_FHDI 	List_ocsg(nrow); //order records used for variance estimation
		//IN   : std::string s_M = "FEFI" = fully efficient fractional imputation
		//						   "FHDI" = fractional hot deck imputation
		//IN   : int i_option_collapsing  = choice of big-p algorithm 
		//                               0= no big-p algorithms
		//                              !0= perform big-p algorithms
		//IN   : int codes(nrow, i_option_collapsing); // storage to record most correlated variables of mox
		//
		//OUT  : double** wmat = New_dMatrix(nrow_dat2_FEFI, L=nrow); //nrow_dat2_FHDI = rows of w1
		//
		//Data Structure Note
		//----------------------
		//dat1 in R version:  id   w  y_matrix  z_matrix
		//     in C++ ver  :  id   w  y         z
		//----------------------
		//dat2 in R version:  id  FID  WGT  FWGT  imputed matrix     Response(0/1)
		//     in C++ ver  :  ----- rbind_ipmat_FEFI------------     rbind_Resp_FEFI 
		//                 :  ----- rbind_ipmat_FHDI------------     rbind_Resp_FHDI
		//----------------------
		//
		//ipmat  = final imputation results
		//     	col1: ID 	= unit index
		//		col2: FID 	= ID of fractionally imputed value
		// 		col3: WGT 	= weight 
		//		col4: FWGT	= Frational weight
		//		col5: Variables 
		//		col6: Responses (separately in  rbind_Resp_...)
		//------------------------
		//
		//irmat  = imputation results related to the categorized matrix 
		//     	col1: ID 	= unit index
		//		col2: FID 	= ID of fractionally imputed value
		//		col3: OID	= original rank of the imputed value
		//		col4: ORDER = SN(selected donor)
		//		col5: FEFIW	= Fefi weights 
		//		col6: CELL	= cells 
		//----------------------
	{
		//testout
		//RPrint("=========== Begin Variance Estimation of FEFI ================");
		//cout<<"=========== Begin Variance Estimation of FEFI  ================"<<endl;


		//----------------------------
		//Basic constants declaration
		//----------------------------
		const int nrow_dat2_FEFI = rbind_ipmat_FEFI.size_row();
		const int nrow_dat2_FHDI = rbind_ipmat_FHDI.size_row();
		const int nrow_mox = rbind_mox.size_row();
		const int L = nrow; //size of d_rw 

							//--------------------
							//get ready id table of FEFI
							//--------------------
		double* d_id_FEFI = new double[nrow_dat2_FEFI];
		for (int i = 0; i<nrow_dat2_FEFI; i++) d_id_FEFI[i] = rbind_ipmat_FEFI(i, 0); //id, 1st col 
		std::vector<double> v_table_name_id_FEFI; //same as "nimp" in R version
		std::vector<int>    v_table_count_id_FEFI;//same as "nimp" in R version 
		table_cpp(d_id_FEFI, nrow_dat2_FEFI, v_table_name_id_FEFI, v_table_count_id_FEFI);

		//testout 
		//cout<<"Main Var Est FEFI: after table_"<<endl;

		//---------------------
		//imputed real data matrix
		//---------------------
		int nrow_d_iy = nrow_dat2_FEFI; //default
		if (s_M == "FDFI") { nrow_d_iy = nrow_dat2_FEFI; }
		if (s_M == "FHDI") { nrow_d_iy = nrow_dat2_FHDI; }
		double** d_iy = New_dMatrix(nrow_d_iy, ncol); //imputed matrix of real values 
		for (int i = 0; i<ncol; i++)
		{
			for (int j = 0; j<nrow_d_iy; j++)
			{
				if (s_M == "FEFI")
				{
					d_iy[j][i] = rbind_ipmat_FEFI(j, 4 + i);
				} //col5~ncol contains imputed real values 
				if (s_M == "FHDI")
				{
					d_iy[j][i] = rbind_ipmat_FHDI(j, 4 + i);
				} //col5~ncol contains imputed real values 

			}
		}

		//testout 
		/*
		cout<<"Main Var Est FEFI: after getting ipmat"<<endl;
		cout<<"d_iy matrix"<<endl;
		for(int i=0; i<nrow_d_iy; i++)
		{
		for(int j=0; j<ncol; j++)
		{
		cout<<d_iy[i][j]<<",  ";
		}
		cout<<endl;
		}
		*/

		//----------------------
		//categorized matrix
		//----------------------
		double** d_cx = New_dMatrix(nrow, ncol);
		Copy_dMatrix(z, nrow, ncol, d_cx);

		//testout 
		/*
		cout<<"Main Var Est FEFI: after getting z = d_cx"<<endl;
		cout<<"d_cx matrix"<<endl;
		for(int i=0; i<nrow; i++)
		{
		for(int j=0; j<ncol; j++)
		{
		cout<<d_cx[i][j]<<",  ";
		}
		cout<<endl;
		}
		*/

		//---------------------
		//ocg, observed donors for each missing pattern. 
		//---------------------
		//the same as List_ocsg[nrow_mox]
		//---------------------
		int* i_locg = new int[nrow_mox]; //length of each list of ocg
		for (int i = 0; i<nrow_mox; i++)
		{
			int i_temp = 0;
			List_ocsg.get_a_row_size(i, i_temp);
			i_locg[i] = i_temp;
		}

		//testout 
		//cout<<"Main Var Est FEFI: after i_locg"<<endl;

		//testout
		//RPrint("==========DEBUG: after i_locg ================");

		//testout
		/*
		RPrint("==== in Variance_Est_Extension_cpp ========");
		RPrint("id: "); RPrint(id, n);
		RPrint("n : "); RPrint(n);
		RPrint("nr: "); RPrint(nr);
		RPrint("nc: "); RPrint(nc);
		RPrint("--------dat1: id (above)and w,  y and z ");
		RPrint("w: "); RPrint(w, n);
		RPrint("y: "); RPrint(y, nrow, ncol);
		RPrint("z: "); RPrint(z, nrow, ncol);
		RPrint("--------dat2: ipmat_FEFI     Resp_FEFI ------- ");
		rbind_ipmat_FEFI.print_rbind_FHDI();
		rbind_Resp_FEFI.print_rbind_FHDI();
		RPrint("--------dat2: ipmat_FHDI     Resp_FHDI ------- ");
		rbind_ipmat_FHDI.print_rbind_FHDI();
		rbind_Resp_FHDI.print_rbind_FHDI();
		RPrint("iy : (imputed real data)"); RPrint(d_iy, nrow_d_iy, ncol);
		RPrint("cx : (categorized matrix)"); RPrint(d_cx, nrow, ncol);
		RPrint("ocg: ");  List_ocsg.print_List_FHDI();
		RPrint("locg: "); RPrint(i_locg, nrow_mox);
		RPrint("nr1: "); RPrint(nr1);
		RPrint("nr2: "); RPrint(nr2);
		*/

		//------------------------
		//cell probability using replicate weight
		//------------------------
		List_FHDI         List_rst_prob(nrow); //only i_nc rows are meaningful
		List_string_FHDI  List_rst_name(nrow); //only i_nc rows are meaningful
		std::vector<std::string> s_ncx;

		bool b_success_Rep_CellP = Rep_CellP_Bigp(d_cx, nrow, ncol, i_option_collapsing, d_rw, id,
			List_rst_prob,
			List_rst_name,
			s_ncx, codes);


		if (!b_success_Rep_CellP)
		{
			Rprintf("Error! Rep_CellP Failed! Change k, check data quality, further break down categorical variables, or so. It may help \n");

			return 0; //abnormal ending 								
		}

		//--------------------
		//put 1 into fully observed rows
		//--------------------
		double* d_rr0 = new double[nrow];
		for (int i = 0; i<nrow; i++)
		{
			double d_prod = 1.0;
			for (int j = 0; j<ncol; j++)
			{
				d_prod = d_prod*d_cx[i][j];
			}
			//-----
			//0: at least one missing;  1: all observed
			//-----
			d_rr0[i] = d_prod;
			if (fabs_FHDI(d_prod) >0.0) d_rr0[i] = 1;
		}

		//--------------
		//calculate w1 = sampling weight
		//--------------
		//std::string cn[nrow]; 
		std::string *cn = new std::string[nrow];
		Trans(d_cx, nrow, ncol, cn);
		double* d_w1 = new double[nrow_dat2_FEFI];
		for (int i = 0; i<nrow_dat2_FEFI; i++)
			d_w1[i] = rbind_ipmat_FEFI(i, 2); //3rd column contains WGT

											  //testout
											  //RPrint("rr0: "); RPrint(d_rr0, nrow);
											  //RPrint("w1: "); RPrint(d_w1, nrow_dat2_FEFI);
											  //testout 
											  //cout<<"Main Var Est FEFI: after d_w1"<<endl;			  

											  //------------------
											  //make Covariance matrix for following replication process
											  //------------------
		int* i_lloc;
		std::vector<int> v_mox_0;
		double** d_dy;
		double** V_var; //covariance matrix of dy
		List_FHDI List_V(nrow_mox); //storage of covariance matrix
									//Note: store cov mat by "row-first" rule 
		for (int i = 0; i<nrow_mox; i++)
		{
			int i_size_lloc = i_locg[i]; //length of the ocg associated with current missing row

										 //-----------
										 //when zero size continue to next iteration
										 //-----------
			if (i_size_lloc <= 0) { continue; }
			i_lloc = new int[i_size_lloc];
			for (int j = 0; j< i_size_lloc; j++) i_lloc[j] = (int)List_ocsg(i, j); //ith row, jth entity

																				   //-----
																				   //find missing column in current row
																				   //------
			v_mox_0.clear();
			for (int j = 0; j<ncol; j++)
			{
				if (rbind_mox(i, j) == 0.0) v_mox_0.push_back(j + 1); //ACTUAL zero column id 
			}
			const int i_size_v_mox_0 = (int)v_mox_0.size();

			//-------
			//extract matrix of missing patterns
			//-------
			d_dy = New_dMatrix(i_size_lloc, i_size_v_mox_0);
			V_var = New_dMatrix(i_size_v_mox_0, i_size_v_mox_0); //column-wise covariance 
			for (int j = 0; j< i_size_lloc; j++)
			{
				for (int k = 0; k<i_size_v_mox_0; k++)
				{
					d_dy[j][k] = y[i_lloc[j] - 1][v_mox_0[k] - 1]; //-1 for actual location 
				}
			}
			//----------
			//"Estimated covariance" of d_dy by column-to-column method
			//----------
			cov_FHDI(d_dy, i_size_lloc, i_size_v_mox_0, V_var);


			//----------
			//store the covariance matrix 
			//row-first rule
			//----------
			//const int i_List_V_col = i_size_v_mox_0*i_size_v_mox_0;  
			//double* d_V_temp = new double[i_List_V_col];
			//for(int j=0; j<i_size_v_mox_0; j++) 
			//{
			//	for(int k=0; k<i_size_v_mox_0; k++)
			//		d_V_temp[j*i_size_v_mox_0 + k]= V_var[k][j]; 
			//}

			//List_V.put_block(i, i_List_V_col, d_V_temp); //ith covariance matrix 
			List_V.put_block(i, i_size_v_mox_0, i_size_v_mox_0, V_var); //direct matrix saving

																		//---
																		//local deallocation
																		//---
			delete[] i_lloc;
			Del_dMatrix(d_dy, i_size_lloc, i_size_v_mox_0);
			Del_dMatrix(V_var, i_size_v_mox_0, i_size_v_mox_0);
			//delete[] d_V_temp; 

		}
		//testout 
		//cout<<"Main Var Est FEFI: after V_var"<<endl;			  

		//testout
		//RPrint("==========DEBUG: after V_var ================");	

		//testout
		//RPrint("FEFI  List_V");
		//List_V.print_List_FHDI(); 

		//----------------
		//wmat: Replication Weights
		//----------------
		//double** wmat = New_dMatrix(nrow_dat2_FEFI, L); //nrow_dat2_FEFI = rows of w1

		//------------------------------
		//------------------------------
		//MAIn loop for L replications
		//------------------------------
		//------------------------------
		double* rw0 = new double[nrow];

		int i_sum_Rw = 0;
		for (int i = 0; i<nrow; i++) i_sum_Rw += v_table_count_id_FEFI[i];
		double* Rw = new double[i_sum_Rw];

		double* wijk = new double[nrow_dat2_FEFI]; //FWGT from ipmat 

												   //testout
												   //RPrint("==========DEBUG: begin main loop of l=L ==============");	
												   //testout 
												   //cout<<"Main Var Est FEFI: begin main loop of l=L"<<endl;			  

		for (int l = 0; l<L; l++)
		{
			//testout
			//RPrint("==========DEBUG: Inside main loop of l :"); RPrint(l); 			
			//cout<<"main loop of l:"<<l<<endl;

			//-------
			//replicate weight from lth column
			//-------
			for (int i = 0; i<nrow; i++) rw0[i] = d_rw(i, l);  //previous: d_rw[i][l]; //l_th column 
			int i_sum = 0;
			for (int i = 0; i<nrow; i++)
			{
				for (int j = 0; j<v_table_count_id_FEFI[i]; j++) Rw[i_sum++] = rw0[i];
			}

			//---------
			//FWGT of ipmat
			//----------
			for (int i = 0; i<nrow_dat2_FEFI; i++)
				wijk[i] = rbind_ipmat_FEFI(i, 3); //4th column is FWGT 

												  //----------
												  //joint probability associated with current string
												  //-----------
			std::string cn_current = cn[l]; //lth string 
			std::vector<int> v_ncx_cn;
			which(s_ncx, cn_current, v_ncx_cn); //actual location 
												//const int i_size_v_ncx_cn = (int)v_ncx_cn.size(); //MUST BE "1"

			int i_size_cellp = 0;
			List_rst_prob.get_a_row_size(v_ncx_cn[0] - 1, i_size_cellp); //get a size of the row in the list 

																		 //-----------
																		 //when zero size continue to next iteration
																		 //-----------
			if (i_size_cellp <= 0) { continue; }
			double* d_cellp = new double[i_size_cellp];
			List_rst_prob.get_block(v_ncx_cn[0] - 1, d_cellp); //-1 for actual row location 

															   //testout
															   //RPrint(" ======== in Main Loop l+1: "); RPrint(l+1); 
															   //RPrint("Rw"); RPrint(Rw, i_sum_Rw); 
															   //RPrint("wijk"); RPrint(wijk, nrow_dat2_FEFI); 
															   //RPrint("d_cellp"); RPrint(d_cellp, i_size_cellp); 

															   //----------------------------------------
															   //1. if the deleted is missing unit, no further action is taken
															   //2. if the deleted is observed unit, then the fractional weights are re-computed 
															   //----------------------------------------
			int* idd = new int[nrow_mox]; //location of the deleted donor in ocg 
			Fill_iVector(idd, nrow_mox, 0);
			if (fabs(d_rr0[l]) > 0)
			{
				//---------------------
				//locations of the deleted unit in observed list
				//---------------------
				std::vector<int> v_lg; //Actual locations 
				v_lg.clear();
				for (int j = 0; j<nrow_mox; j++) //list length 
				{
					for (int k = 0; k<i_locg[j]; k++) //a row in the List
					{
						int i_temp_lg = (int)List_ocsg(j, k);
						if (i_temp_lg == (l + 1)) //+1 for actual location  
						{
							v_lg.push_back(j + 1); //actual row location 
							idd[j] = k + 1; //actual location 
							break;
						}
					}
				}
				const int nlg = (int)v_lg.size();

				//testout
				//RPrint(" in condition rr0[l]!=0 at l+1 ="); RPrint(l+1);
				//RPrint("idd:"); RPrint(idd, nrow_mox);
				//RPrint("lg :"); RPrint(v_lg);
				//RPrint("nlg:"); RPrint(nlg); 

				//--------------------------
				//Adjust fractional weights for all units in lg
				//--------------------------
				if (nlg>0)
				{
					for (int j = 0; j<nlg; j++)
					{
						int i_row_lg = v_lg[j] - 1; // row number [0,...) 
						double* d_1_mox = new double[ncol];
						for (int k = 0; k<ncol; k++) d_1_mox[k] = rbind_mox(i_row_lg, k);

						//---
						//actual col number of missing cell in current missing row
						//---
						std::vector<int> v_rloc; v_rloc.clear();
						for (int k = 0; k<ncol; k++)
						{
							if (d_1_mox[k] == 0.0) { v_rloc.push_back(k + 1); } //actual col
						}
						//const int nrloc = (int)v_rloc.size();
						std::string cng;
						Trans1(d_1_mox, ncol, cng);

						//-------
						//location of cn which has cng
						//-------
						std::vector<int> v_mlog; v_mlog.clear();
						which(cn, nrow, cng, v_mlog);
						const int nmlog = (int)v_mlog.size();

						//testout
						//RPrint("rloc: "); RPrint(v_rloc);
						//RPrint("mlog: "); RPrint(v_mlog);

						//------------------------
						//------------------------
						//FEFI
						//------------------------
						//------------------------
						std::vector<int> v_elog;
						if (s_M.compare("FEFI") == 0) //0=equal 
						{
							//-----
							//find locations of mlog in dat2$ID
							//v_mlog contains the row numbers that have the same string as
							//current missing row 
							//nmlog = n(v_mlog)
							//-----
							v_elog.clear();
							for (int k1 = 0; k1<nmlog; k1++) //loop for mlog
							{
								int i_temp1 = id[v_mlog[k1] - 1]; //dat1$ID in R version  
								for (int k2 = 0; k2<nrow_dat2_FEFI; k2++)
								{
									int i_temp2 = rbind_ipmat_FEFI(k2, 0); //1st col is dat2$ID
									if (i_temp1 == i_temp2)
									{
										v_elog.push_back(k2 + 1); //actual location 
									}
								}
							}
							const int i_size_v_elog = (int)v_elog.size();
							//testout
							//RPrint(" elog: "); RPrint(v_elog); 

							//----
							//donor id
							//-----
							std::vector<int> v_did; v_did.clear();

							int i_temp_lg_j = v_lg[j] - 1; //-1 for actual location  
							for (int k1 = 0; k1<i_locg[i_temp_lg_j]; k1++) //a row in the List 
							{
								v_did.push_back((int)List_ocsg(i_temp_lg_j, k1));
							}
							const int i_size_nic = (int)v_did.size(); //length of did 

																	  //----
																	  //donor string patterns
																	  //----
																	  //std::string s_icn[i_size_nic];
							std::string *s_icn = new std::string[i_size_nic];
							for (int k1 = 0; k1<i_size_nic; k1++)
								s_icn[k1] = cn[v_did[k1] - 1]; //-1 for actual location  		

															   //testout 
															   //RPrint("did :"); RPrint(v_did);
															   //RPrint("icn :"); RPrint(s_icn, i_size_nic);

															   //------
															   //unique icn
															   //-------
							std::vector<std::string> v_unique_icn; v_unique_icn.clear();
							//std::string s_icn_backup[i_size_nic];
							std::string *s_icn_backup = new std::string[i_size_nic];
							for (int k1 = 0; k1<i_size_nic; k1++) s_icn_backup[k1] = s_icn[k1];

							for (int k1 = 0; k1<i_size_nic; k1++)
							{
								std::string s_uicn_temp = s_icn[k1];
								for (int k2 = 0; k2<i_size_nic; k2++)
								{
									if (s_uicn_temp.compare(s_icn_backup[k2]) == 0)
									{
										//store the found unique string pattern 
										v_unique_icn.push_back(s_uicn_temp);

										//nullify all the remaining unit that has the same string
										for (int k3 = k2; k3<i_size_nic; k3++)
										{
											if (s_uicn_temp.compare(s_icn_backup[k3]) == 0)
												s_icn_backup[k3] = ""; //nullify
										}

										break;
									}
								}
							}
							const int i_unic = (int)v_unique_icn.size();

							//testout 
							//RPrint("v_unique_icn :"); RPrint(v_unique_icn);
							//RPrint("i_unic :"); 	  RPrint(i_unic);

							//----------
							//sort the unique donors
							//----------
							//std::string s_unique_icn_sorted[i_unic]; //sorted donors
							std::string *s_unique_icn_sorted = new std::string[i_unic]; //sorted donors
							for (int k1 = 0; k1<i_unic; k1++) s_unique_icn_sorted[k1] = v_unique_icn[k1];
							std::sort(s_unique_icn_sorted, s_unique_icn_sorted + i_unic);

							//---------
							//match the sorted unique donors to njp
							//---------
							//make "njp" 
							int i_temp2 = 0; List_rst_name.get_a_row_size(0, i_temp2);
							//std::string s_njp[i_temp2]; 
							std::string *s_njp = new std::string[i_temp2];
							for (int k1 = 0; k1<i_temp2; k1++) s_njp[k1] = List_rst_name(0, k1);

							std::vector<int> v_icn_njp; v_icn_njp.clear();
							for (int k1 = 0; k1<i_unic; k1++)
							{
								std::string s_temp_cp = s_unique_icn_sorted[k1];

								for (int k2 = 0; k2<i_temp2; k2++)
								{
									if (s_temp_cp.compare(s_njp[k2]) == 0)
									{
										v_icn_njp.push_back(k2 + 1); break;
									} //+1 for actual location 
								}
							}
							const int i_size_v_icn_njp = (int)v_icn_njp.size();


							//-------------------------------------
							//get joint probability at the matched location only
							//-------------------------------------
							//-----------
							//when zero size continue to next iteration
							//-----------
							if (i_size_v_icn_njp <= 0) { continue; }
							double* d_cp = new double[i_size_v_icn_njp]; //joint prob
																		 //std::string s_ncp[i_size_v_icn_njp];          //names of the matches
							std::string *s_ncp = new std::string[i_size_v_icn_njp];          //names of the matches
							double d_sum_cp = 0.0;
							for (int k1 = 0; k1<i_size_v_icn_njp; k1++)
							{
								d_cp[k1] = d_cellp[v_icn_njp[k1] - 1]; //-1 for actual location 
								s_ncp[k1] = s_njp[v_icn_njp[k1] - 1];

								d_sum_cp += d_cp[k1]; //summation of d_cp[]
							}
							for (int k1 = 0; k1<i_size_v_icn_njp; k1++)
							{
								d_cp[k1] = d_cp[k1] / d_sum_cp;
							}

							//testout
							//RPrint("njp"); RPrint(s_njp, i_temp2); 
							//RPrint("match unique icn and njp :"); RPrint(v_icn_njp); 
							//RPrint("cp"); RPrint(d_cp, i_size_v_icn_njp);
							//RPrint("ncp"); RPrint(s_ncp, i_size_v_icn_njp);						

							//-----------
							//match donors 
							//-----------
							std::vector<int> v_obsp; v_obsp.clear();
							std::vector<double> v_obsp_cp; v_obsp_cp.clear();
							match_FHDI(s_icn, i_size_nic,
								s_ncp, i_size_v_icn_njp,
								v_obsp);
							const int i_size_v_obsp = (int)v_obsp.size();
							for (int k1 = 0; k1<i_size_v_obsp; k1++)
								v_obsp_cp.push_back(d_cp[v_obsp[k1] - 1]);

							//---------------------------------------
							//Replicated sampling weights for donors
							//---------------------------------------
							//-----------
							//when zero size continue to next iteration
							//-----------
							if (i_size_nic <= 0) { continue; }
							double* d_drw0 = new double[i_size_nic]; //length of did
							double d_sum_drw0 = 0.0;
							for (int k1 = 0; k1<i_size_nic; k1++)
							{
								d_drw0[k1] = rw0[v_did[k1] - 1];
								d_sum_drw0 += d_drw0[k1];
							}

							//----------
							//update weighted probability
							//----------
							std::vector<std::string> jp_name_icn; jp_name_icn.clear();
							std::vector<double> 	 jp_prob_icn; jp_prob_icn.clear();

							wpct_FHDI(s_icn, i_size_nic, d_drw0,
								jp_name_icn, jp_prob_icn);
							const int i_size_jp_prob_icn = (int)jp_prob_icn.size(); //=i_unic
							for (int k1 = 0; k1<i_size_jp_prob_icn; k1++)
								jp_prob_icn[k1] = jp_prob_icn[k1] * d_sum_drw0;  //ws.icn

																				 //testout
																				 //RPrint("obsp: "); RPrint(v_obsp);
																				 //RPrint("jp_name_icn :"); RPrint(jp_name_icn);
																				 //RPrint("jp_prob_icn = ws.icn :"); RPrint(jp_prob_icn);



							const int i_current_locg = i_locg[v_lg[j] - 1]; //-1 for actual loc
																			//-----------
																			//when zero size continue to next iteration
																			//-----------
							if (i_size_v_obsp <= 0) { continue; }
							double* d_Fefiw = new double[i_size_v_obsp];
							//---------------
							//weight update I: at deleted donor locations
							//---------------
							if (i_unic == i_current_locg)
							{
								for (int k1 = 0; k1<i_size_v_obsp; k1++)
									d_Fefiw[k1] = v_obsp_cp[k1];
							}
							//-----------------------------------------------
							//weight update II: at deleted donor locations
							//-----------------------------------------------
							//-----------
							//when zero size continue to next iteration
							//-----------
							if (i_size_nic <= 0) { continue; }
							double* d_drw1 = new double[i_size_nic];
							Fill_dVector(d_drw1, i_size_nic, 0.0); //initialized with 0
							if (i_unic < i_current_locg)
							{
								//-----------
								//match donors 
								//-----------
								std::vector<int> v_temp; v_temp.clear();
								match_FHDI(s_icn, i_size_nic,
									s_unique_icn_sorted, i_unic,
									v_temp);

								for (int k1 = 0; k1<i_size_nic; k1++)
								{
									if (jp_prob_icn[v_temp[k1] - 1] != 0.0)
										d_drw1[k1] = d_drw0[k1] / jp_prob_icn[v_temp[k1] - 1];
								}

								//------------
								//final updated weights
								//------------
								for (int k1 = 0; k1<i_size_nic; k1++)
									d_Fefiw[k1] = v_obsp_cp[k1] * d_drw1[k1];

								//testout
								/*
								RPrint("d_drw0 :"); RPrint(d_drw0, i_size_nic);
								RPrint("jp_prob_icn[v_temp[k1]-1] :");
								for(int k1=0; k1<i_size_nic; k1++)
								RPrint(jp_prob_icn[v_temp[k1]-1]);
								RPrint("jp_name_icn[v_temp[k1]-1] :");
								std::vector<std::string> v_s_temp; v_s_temp.clear();
								for(int k1=0; k1<i_size_nic; k1++)
								{	v_s_temp.push_back(jp_name_icn[v_temp[k1]-1]); }
								RPrint(v_s_temp);
								RPrint("v_obsp_cp :"); RPrint(v_obsp_cp);
								RPrint("d_drw1 :"); RPrint(d_drw1, i_size_nic);
								RPrint("d_Fefiw :"); RPrint(d_Fefiw, i_size_nic);
								*/

							}

							//------------
							//update wijk: repeated copy, if needed
							//------------
							int i_cycle_wijk = (int)floor(i_size_v_elog*1.0 / i_size_nic*1.0); //expected evenly divisible
							int i_loc_elog = 0; //sequential id

												//testout
												//RPrint("v_elog :"); RPrint(v_elog);
												//RPrint("wijk (updated one only):"); 

							for (int k1 = 0; k1<i_cycle_wijk;k1++)
							{
								for (int k2 = 0; k2<i_size_nic; k2++)
								{
									wijk[v_elog[i_loc_elog++] - 1] = d_Fefiw[k2];
									//testout
									//RPrint(d_Fefiw[k2]);
								}
							}

							//---
							//local deallocation
							//---
							delete[] s_icn;
							delete[] s_icn_backup;
							delete[] d_cp;
							delete[] d_drw0;
							delete[] d_Fefiw;
							delete[] d_drw1;
						}

						//local deallocation 
						delete[] d_1_mox;
					}
				}

			}


			//-------------------------------
			//store the updated weights
			//-------------------------------
			int i_nrow_imputation = nrow;
			if (s_M == "FEFI") i_nrow_imputation = nrow_dat2_FEFI;
			if (s_M == "FHDI") i_nrow_imputation = nrow_dat2_FHDI;

			for (int k1 = 0; k1<i_nrow_imputation; k1++)
			{
				wmat[k1][l] = Rw[k1] * wijk[k1];
			}

			//testout

			/*
			double* d_temp_wmat1 = new double[i_nrow_imputation];
			for(int j=0; j<i_nrow_imputation; j++) d_temp_wmat1[j] = wmat[j][l];
			RPrint("wmat[,l]:");
			RPrint(d_temp_wmat1, i_nrow_imputation);
			delete[] d_temp_wmat1;
			*/

			//--------------------
			//local deallocation
			//--------------------
			delete[] idd;
			delete[] d_cellp;



		} //end of main loop for L
		  //testout
		Rprintf(" ========= Variance estimation has successfully finished!\n");

		//testout 
		//cout<<"Main Var Est FEFI: Variance_Est_FEFI.. has successfully finished!"<<endl;			  

		//-------------
		//deallocation
		//-------------
		//delete[] w; 
		//delete[] id; 
		delete[] cn;
		delete[] d_id_FEFI;
		Del_dMatrix(d_iy, nrow_d_iy, ncol);
		Del_dMatrix(d_cx, nrow, ncol);
		delete[] i_locg;
		delete[] d_rr0;
		delete[] d_w1;
		//Del_dMatrix(wmat, nrow_dat2_FEFI, L);
		delete[] rw0;
		delete[] Rw;
		delete[] wijk;

		return 1;
	}

}//end of namespace


 //Fn===========================================================================

 //Variance_Est_FEFI_Neighbor_cpp.cc-----------------------------------------------------------------------------

 //Fn===========================================================================

namespace FHDI
{

	bool Rep_CellP_Neighbor(double** d_cx, const int nrow, const int ncol, FHDI::RepWeight_FHDI &d_rw, int*  id,
		List_FHDI        &List_nU,
		List_FHDI        &List_rst_prob,
		List_string_FHDI &List_rst_name,
		std::vector<std::string> &s_ncx)
		//Description============================================
		// compute cell probability using replicate weight rw
		// 
		// R code: Dr. Im, J., and Dr. Kim, J. 
		// C++   : Dr. Cho, I. and Yicheng Yang
		// All rights reserved
		// Last update: Aug 11, 2020
		//

		//IN   : double d_cx[nrow, ncol] = categoraized matrix
		//IN   : double d_rw[nrow, nrow] = replicate weights
		//IN   : int    id[nrow] = index of rows
		//
		//below two lists have meaningful values up to i_nc rows  
		//OUT  : List_FHDI List_rst_prob(nrow->i_nc); //list of joint probabilities for all missing patterns 
		//OUT  : List_string_FHDI List_rst_name(nrow->i_nc); //names of joint probabilities for all missing patterns 
		//OUT  : std::vector<std::string> s_ncx; //uniqe cn0
		//======================================================== 
	{
		//--------------
		//make a condensed expression "cn0" of cx, i.e. z
		//--------------
		//std::string cn0[nrow];
		//std::string cn0_backup[nrow];
		std::string *cn0 = new std::string[nrow];
		std::string *cn0_backup = new std::string[nrow];

		Trans(d_cx, nrow, ncol, cn0);

		for (int i = 0; i<nrow; i++) cn0_backup[i] = cn0[i];

		//---------------------
		//SORT & UNIQUE patterns of cn0
		//---------------------
		//std::string s_cn0_temp[nrow]; 
		std::string *s_cn0_temp = new std::string[nrow];
		for (int i = 0; i<nrow; i++) s_cn0_temp[i] = cn0[i];
		std::sort(s_cn0_temp, s_cn0_temp + nrow);

		//------------
		//memorize observed patterns 
		//------------
		//std::vector<std::string> s_ncx; //uniqe cn0

		int i_count_cn0 = 0; //total number of unique cn0 
		std::string s_temp;
		for (int i = 0; i<nrow; i++)
		{
			s_temp = s_cn0_temp[i]; //get a string from the sorted strings 
			for (int j = 0; j<nrow; j++) //search all rows 
			{
				//----
				//below condition is needed for finding UNIQUE pattern
				//----
				if (s_temp.compare(cn0_backup[j]) == 0) //0: equal string
				{
					s_ncx.push_back(cn0_backup[j]);  //store the found observed pattern

													 //----------
													 //remove all identical string after the current string
													 //----------
					for (int k = j; k<nrow; k++)
					{
						if (s_temp.compare(cn0_backup[k]) == 0) //0: equal string
						{
							cn0_backup[k] = ""; //nullify for the next search
						}
					}

					i_count_cn0++;
					break;
				}

			}
		}
		//Now, i_count_cn0 means the total number of unique sorted strings
		const int i_nc = i_count_cn0;


		//testout
		//RPrint("=====in Rep_CellP ========");
		//RPrint("s_ncx"); RPrint(s_ncx);
		//RPrint("i_nc"); RPrint(i_nc);
		/*cout<<"=====in Rep_CellP ========"<<endl;
		cout<<"s_ncx"<<endl;
		for(int i=0; i<(int)s_ncx.size(); i++) cout<<s_ncx[i]<<" ,  ";
		cout<<endl;
		cout<<"i_nc: "<<i_nc<<endl;
		*/

		//-----------------------------
		//calculate joint probability and names of all missing patterns
		//using the Jackknife replicate weights
		//------------------------------
		//List_FHDI        List_rst_prob(i_nc); //list of joint probabilities for all missing patterns 
		//List_string_FHDI List_rst_name(i_nc); //names of joint probabilities for all missing patterns 

		std::vector<double> jp_prob_return;
		std::vector<std::string> jp_name_return;
		//std::vector<double> w_UserDefined; 
		double* w_UserDefined = new double[nrow];

		for (int i = 0; i<i_nc; i++)
		{
			//---
			//search current missing pattern from all strings
			//---
			std::string s_temp = s_ncx[i];
			int i_loc = 0;
			for (int j = 0; j<nrow; j++)
			{
				if (s_temp.compare(cn0[j]) == 0)
				{
					i_loc = j;
					break;
				}
			}
			//testout
			/*
			cout<<"loop i (1:i_nc) :"<<i<<"  found i_loc:"<<i_loc<<endl;
			if(i==7)
			{
			for(int j_temp=0; j_temp<nrow; j_temp++) cout<<d_rw[j_temp][i_loc]<<",  ";
			}
			cout<<endl;
			*/

			//----
			//joint probability and names
			//----
			jp_prob_return.clear();
			jp_name_return.clear();
			//w_UserDefined.clear();
			//for(int j=0; j<nrow; j++) w_UserDefined.push_back(d_rw[j][i_loc]) ; 
			for (int j = 0; j<nrow; j++) w_UserDefined[j] = d_rw(j, i_loc);

			bool b_success_CellProb_KNN = Cell_Prob_Neighbor_cpp(d_cx, nrow, ncol, List_nU,
				jp_prob_return,
				jp_name_return,
				w_UserDefined, id);

			if (!b_success_CellProb_KNN)
			{
				Rprintf("Error! Cell Prob KNN Failed! Change k, check data quality, further break down categorical variables, or so. It may help \n");

				return 0; //abnormal ending 								
			}

			//---
			//prep return
			//---
			List_rst_prob.put_block(i, jp_prob_return); //jth row has joint prob
			List_rst_name.put_block(i, jp_name_return); //jth row has name of the joint prob

		}

		//testout
		//RPrint("List_rst_name"); List_rst_name.print_List_string_FHDI();
		//RPrint("List_rst_prob"); List_rst_prob.print_List_FHDI();


		delete[] cn0;
		delete[] cn0_backup;
		delete[] s_cn0_temp;

		delete[] w_UserDefined;

		return 1;
	}


	bool Variance_Est_FEFI_Neighbor_cpp(double** y, double** z, const int nrow, const int ncol,
		FHDI::RepWeight_FHDI &d_rw, double* w, int* id, List_FHDI &List_nU,
		rbind_FHDI  &rbind_ipmat_FEFI,
		rbind_FHDI  &rbind_Resp_FEFI,
		rbind_FHDI  &rbind_irmat_FEFI,
		rbind_FHDI  &rbind_ipmat_FHDI,
		rbind_FHDI  &rbind_Resp_FHDI,
		rbind_FHDI  &rbind_irmat_FHDI,
		rbind_FHDI  &rbind_uox,
		rbind_FHDI  &rbind_mox,
		List_FHDI 	&List_ord,
		List_FHDI 	&List_ocsg,
		std::string s_M,
		double** wmat)

		//Description----------------------
		//estimate variance for FEFI using Jackknife method 
		//  Algorithm: 
		//
		// original R code: Dr. Im, J. and Dr. Kim, J. 
		// c++ code: 		Dr. Cho, I. 
		// All rights reserved
		// 
		// updated: March 28, 2017
		//
		//IN   : double y(nrow, ncol)= original data matrix with missing cells 
		//IN   : double z(nrow, ncol)= categorized matrix of y. 0 for missing cell
		//IN   : double d_rw[nrow, nrow] = replicate weights 
		//IN   : double w(nrow) = sampling weight (default = 1.0)
		//IN   : int    id(nrow) = id number of each row (default = 1 to nrow)
		//FEFI --------returns----------------- FEFI //
		//IN   : rbind_FHDI  rbind_ipmat_FEFI(4+ncol); //column size is 4+ncol
		//IN   : rbind_FHDI  rbind_Resp_FEFI(ncol+1);  //separate response matrix  
		//IN   : rbind_FHDI  rbind_irmat_FEFI(5+ncol); //column size is 5+ncol
		//FHDI --------returns----------------- FHDI //
		//IN   : rbind_FHDI  rbind_ipmat_FHDI(4+ncol); //column size is 4+ncol
		//IN   : rbind_FHDI  rbind_Resp_FHDI(ncol+1);  //separate response matrix  
		//IN   : rbind_FHDI  rbind_irmat_FHDI(5+ncol); //column size is 5+ncol
		//other matrices
		//IN   : rbind_FHDI  rbind_uox(ncol); //observed unique categorized matrix 
		//IN   : rbind_FHDI  rbind_mox(ncol); //missing  unique categorized matrix
		//Note: below Lists contain meaningful items up to i_count_mox rows  
		//IN   : List_FHDI 	List_ord(nrow); //order records used for variance estimation
		//IN   : List_FHDI 	List_ocsg(nrow); //order records used for variance estimation
		//IN   : std::string s_M = "FEFI" = fully efficient fractional imputation
		//						   "FHDI" = fractional hot deck imputation
		//
		//OUT  : double** wmat = New_dMatrix(nrow_dat2_FEFI, L=nrow); //nrow_dat2_FHDI = rows of w1
		//
		//Data Structure Note
		//----------------------
		//dat1 in R version:  id   w  y_matrix  z_matrix
		//     in C++ ver  :  id   w  y         z
		//----------------------
		//dat2 in R version:  id  FID  WGT  FWGT  imputed matrix     Response(0/1)
		//     in C++ ver  :  ----- rbind_ipmat_FEFI------------     rbind_Resp_FEFI 
		//                 :  ----- rbind_ipmat_FHDI------------     rbind_Resp_FHDI
		//----------------------
		//
		//ipmat  = final imputation results
		//     	col1: ID 	= unit index
		//		col2: FID 	= ID of fractionally imputed value
		// 		col3: WGT 	= weight 
		//		col4: FWGT	= Frational weight
		//		col5: Variables 
		//		col6: Responses (separately in  rbind_Resp_...)
		//------------------------
		//
		//irmat  = imputation results related to the categorized matrix 
		//     	col1: ID 	= unit index
		//		col2: FID 	= ID of fractionally imputed value
		//		col3: OID	= original rank of the imputed value
		//		col4: ORDER = SN(selected donor)
		//		col5: FEFIW	= Fefi weights 
		//		col6: CELL	= cells 
		//----------------------
	{
		//testout
		//RPrint("=========== Begin Variance Estimation of FEFI ================");
		//cout<<"=========== Begin Variance Estimation of FEFI  ================"<<endl;


		//----------------------------
		//Basic constants declaration
		//----------------------------
		const int nrow_dat2_FEFI = rbind_ipmat_FEFI.size_row();
		const int nrow_dat2_FHDI = rbind_ipmat_FHDI.size_row();
		const int nrow_mox = rbind_mox.size_row();
		const int L = nrow; //size of d_rw 

							//--------------------
							//get ready id table of FEFI
							//--------------------
		double* d_id_FEFI = new double[nrow_dat2_FEFI];
		for (int i = 0; i<nrow_dat2_FEFI; i++) d_id_FEFI[i] = rbind_ipmat_FEFI(i, 0); //id, 1st col 
		std::vector<double> v_table_name_id_FEFI; //same as "nimp" in R version
		std::vector<int>    v_table_count_id_FEFI;//same as "nimp" in R version 
		table_cpp(d_id_FEFI, nrow_dat2_FEFI, v_table_name_id_FEFI, v_table_count_id_FEFI);

		//testout 
		//cout<<"Main Var Est FEFI: after table_"<<endl;

		//---------------------
		//imputed real data matrix
		//---------------------
		int nrow_d_iy = nrow_dat2_FEFI; //default
		if (s_M == "FDFI") { nrow_d_iy = nrow_dat2_FEFI; }
		if (s_M == "FHDI") { nrow_d_iy = nrow_dat2_FHDI; }
		double** d_iy = New_dMatrix(nrow_d_iy, ncol); //imputed matrix of real values 
		for (int i = 0; i<ncol; i++)
		{
			for (int j = 0; j<nrow_d_iy; j++)
			{
				if (s_M == "FEFI")
				{
					d_iy[j][i] = rbind_ipmat_FEFI(j, 4 + i);
				} //col5~ncol contains imputed real values 
				if (s_M == "FHDI")
				{
					d_iy[j][i] = rbind_ipmat_FHDI(j, 4 + i);
				} //col5~ncol contains imputed real values 

			}
		}

		//testout 
		/*
		cout<<"Main Var Est FEFI: after getting ipmat"<<endl;
		cout<<"d_iy matrix"<<endl;
		for(int i=0; i<nrow_d_iy; i++)
		{
		for(int j=0; j<ncol; j++)
		{
		cout<<d_iy[i][j]<<",  ";
		}
		cout<<endl;
		}
		*/

		//----------------------
		//categorized matrix
		//----------------------
		double** d_cx = New_dMatrix(nrow, ncol);
		Copy_dMatrix(z, nrow, ncol, d_cx);

		//testout 
		/*
		cout<<"Main Var Est FEFI: after getting z = d_cx"<<endl;
		cout<<"d_cx matrix"<<endl;
		for(int i=0; i<nrow; i++)
		{
		for(int j=0; j<ncol; j++)
		{
		cout<<d_cx[i][j]<<",  ";
		}
		cout<<endl;
		}
		*/

		//---------------------
		//ocg, observed donors for each missing pattern. 
		//---------------------
		//the same as List_ocsg[nrow_mox]
		//---------------------
		int* i_locg = new int[nrow_mox]; //length of each list of ocg
		for (int i = 0; i<nrow_mox; i++)
		{
			int i_temp = 0;
			List_ocsg.get_a_row_size(i, i_temp);
			i_locg[i] = i_temp;
		}

		//testout 
		//cout<<"Main Var Est FEFI: after i_locg"<<endl;

		//testout
		//RPrint("==========DEBUG: after i_locg ================");

		//testout
		/*
		RPrint("==== in Variance_Est_Extension_cpp ========");
		RPrint("id: "); RPrint(id, n);
		RPrint("n : "); RPrint(n);
		RPrint("nr: "); RPrint(nr);
		RPrint("nc: "); RPrint(nc);
		RPrint("--------dat1: id (above)and w,  y and z ");
		RPrint("w: "); RPrint(w, n);
		RPrint("y: "); RPrint(y, nrow, ncol);
		RPrint("z: "); RPrint(z, nrow, ncol);
		RPrint("--------dat2: ipmat_FEFI     Resp_FEFI ------- ");
		rbind_ipmat_FEFI.print_rbind_FHDI();
		rbind_Resp_FEFI.print_rbind_FHDI();
		RPrint("--------dat2: ipmat_FHDI     Resp_FHDI ------- ");
		rbind_ipmat_FHDI.print_rbind_FHDI();
		rbind_Resp_FHDI.print_rbind_FHDI();
		RPrint("iy : (imputed real data)"); RPrint(d_iy, nrow_d_iy, ncol);
		RPrint("cx : (categorized matrix)"); RPrint(d_cx, nrow, ncol);
		RPrint("ocg: ");  List_ocsg.print_List_FHDI();
		RPrint("locg: "); RPrint(i_locg, nrow_mox);
		RPrint("nr1: "); RPrint(nr1);
		RPrint("nr2: "); RPrint(nr2);
		*/

		//------------------------
		//cell probability using replicate weight
		//------------------------
		List_FHDI         List_rst_prob(nrow); //only i_nc rows are meaningful
		List_string_FHDI  List_rst_name(nrow); //only i_nc rows are meaningful
		std::vector<std::string> s_ncx;

		bool b_success_Rep_CellP_KNN = Rep_CellP_Neighbor(d_cx, nrow, ncol, d_rw, id, List_nU,
			List_rst_prob,
			List_rst_name,
			s_ncx);


		if (!b_success_Rep_CellP_KNN)
		{
			Rprintf("Error! Rep_CellP_KNN Failed! Change k, check data quality, further break down categorical variables, or so. It may help \n");

			return 0; //abnormal ending 								
		}

		//--------------------
		//put 1 into fully observed rows
		//--------------------
		double* d_rr0 = new double[nrow];
		for (int i = 0; i<nrow; i++)
		{
			double d_prod = 1.0;
			for (int j = 0; j<ncol; j++)
			{
				d_prod = d_prod*d_cx[i][j];
			}
			//-----
			//0: at least one missing;  1: all observed
			//-----
			d_rr0[i] = d_prod;
			if (fabs(d_prod) >0.0) d_rr0[i] = 1;
		}

		//--------------
		//calculate w1 = sampling weight
		//--------------
		//std::string cn[nrow]; 
		std::string *cn = new std::string[nrow];
		Trans(d_cx, nrow, ncol, cn);
		double* d_w1 = new double[nrow_dat2_FEFI];
		for (int i = 0; i<nrow_dat2_FEFI; i++)
			d_w1[i] = rbind_ipmat_FEFI(i, 2); //3rd column contains WGT

											  //testout
											  //RPrint("rr0: "); RPrint(d_rr0, nrow);
											  //RPrint("w1: "); RPrint(d_w1, nrow_dat2_FEFI);
											  //testout 
											  //cout<<"Main Var Est FEFI: after d_w1"<<endl;			  

											  //------------------
											  //make Covariance matrix for following replication process
											  //------------------
		int* i_lloc;
		std::vector<int> v_mox_0;
		double** d_dy;
		double** V_var; //covariance matrix of dy
		List_FHDI List_V(nrow_mox); //storage of covariance matrix
									//Note: store cov mat by "row-first" rule 
		for (int i = 0; i<nrow_mox; i++)
		{
			int i_size_lloc = i_locg[i]; //length of the ocg associated with current missing row

										 //-----------
										 //when zero size continue to next iteration
										 //-----------
			if (i_size_lloc <= 0) { continue; }
			i_lloc = new int[i_size_lloc];
			for (int j = 0; j< i_size_lloc; j++) i_lloc[j] = (int)List_ocsg(i, j); //ith row, jth entity

																				   //-----
																				   //find missing column in current row
																				   //------
			v_mox_0.clear();
			for (int j = 0; j<ncol; j++)
			{
				if (rbind_mox(i, j) == 0.0) v_mox_0.push_back(j + 1); //ACTUAL zero column id 
			}
			const int i_size_v_mox_0 = (int)v_mox_0.size();

			//-------
			//extract matrix of missing patterns
			//-------
			d_dy = New_dMatrix(i_size_lloc, i_size_v_mox_0);
			V_var = New_dMatrix(i_size_v_mox_0, i_size_v_mox_0); //column-wise covariance 
			for (int j = 0; j< i_size_lloc; j++)
			{
				for (int k = 0; k<i_size_v_mox_0; k++)
				{
					d_dy[j][k] = y[i_lloc[j] - 1][v_mox_0[k] - 1]; //-1 for actual location 
				}
			}
			//----------
			//"Estimated covariance" of d_dy by column-to-column method
			//----------
			cov_FHDI(d_dy, i_size_lloc, i_size_v_mox_0, V_var);


			//----------
			//store the covariance matrix 
			//row-first rule
			//----------
			//const int i_List_V_col = i_size_v_mox_0*i_size_v_mox_0;  
			//double* d_V_temp = new double[i_List_V_col];
			//for(int j=0; j<i_size_v_mox_0; j++) 
			//{
			//	for(int k=0; k<i_size_v_mox_0; k++)
			//		d_V_temp[j*i_size_v_mox_0 + k]= V_var[k][j]; 
			//}

			//List_V.put_block(i, i_List_V_col, d_V_temp); //ith covariance matrix 
			List_V.put_block(i, i_size_v_mox_0, i_size_v_mox_0, V_var); //direct matrix saving

																		//---
																		//local deallocation
																		//---
			delete[] i_lloc;
			Del_dMatrix(d_dy, i_size_lloc, i_size_v_mox_0);
			Del_dMatrix(V_var, i_size_v_mox_0, i_size_v_mox_0);
			//delete[] d_V_temp; 

		}
		//testout 
		//cout<<"Main Var Est FEFI: after V_var"<<endl;			  

		//testout
		//RPrint("==========DEBUG: after V_var ================");	

		//testout
		//RPrint("FEFI  List_V");
		//List_V.print_List_FHDI(); 

		//----------------
		//wmat: Replication Weights
		//----------------
		//double** wmat = New_dMatrix(nrow_dat2_FEFI, L); //nrow_dat2_FEFI = rows of w1

		//------------------------------
		//------------------------------
		//MAIn loop for L replications
		//------------------------------
		//------------------------------
		double* rw0 = new double[nrow];

		int i_sum_Rw = 0;
		for (int i = 0; i<nrow; i++) i_sum_Rw += v_table_count_id_FEFI[i];
		double* Rw = new double[i_sum_Rw];

		double* wijk = new double[nrow_dat2_FEFI]; //FWGT from ipmat 

												   //testout
												   //RPrint("==========DEBUG: begin main loop of l=L ==============");	
												   //testout 
												   //cout<<"Main Var Est FEFI: begin main loop of l=L"<<endl;			  

		for (int l = 0; l<L; l++)
		{
			//testout
			//RPrint("==========DEBUG: Inside main loop of l :"); RPrint(l); 			
			//cout<<"main loop of l:"<<l<<endl;

			//-------
			//replicate weight from lth column
			//-------
			for (int i = 0; i<nrow; i++) rw0[i] = d_rw(i, l); //l_th column 
			int i_sum = 0;
			for (int i = 0; i<nrow; i++)
			{
				for (int j = 0; j<v_table_count_id_FEFI[i]; j++) Rw[i_sum++] = rw0[i];
			}

			//---------
			//FWGT of ipmat
			//----------
			for (int i = 0; i<nrow_dat2_FEFI; i++)
				wijk[i] = rbind_ipmat_FEFI(i, 3); //4th column is FWGT 

												  //----------
												  //joint probability associated with current string
												  //-----------
			std::string cn_current = cn[l]; //lth string 
			std::vector<int> v_ncx_cn;
			which(s_ncx, cn_current, v_ncx_cn); //actual location 
												//const int i_size_v_ncx_cn = (int)v_ncx_cn.size(); //MUST BE "1"

			int i_size_cellp = 0;
			List_rst_prob.get_a_row_size(v_ncx_cn[0] - 1, i_size_cellp); //get a size of the row in the list 

																		 //-----------
																		 //when zero size continue to next iteration
																		 //-----------
			if (i_size_cellp <= 0) { continue; }
			double* d_cellp = new double[i_size_cellp];
			List_rst_prob.get_block(v_ncx_cn[0] - 1, d_cellp); //-1 for actual row location 

															   //testout
															   //RPrint(" ======== in Main Loop l+1: "); RPrint(l+1); 
															   //RPrint("Rw"); RPrint(Rw, i_sum_Rw); 
															   //RPrint("wijk"); RPrint(wijk, nrow_dat2_FEFI); 
															   //RPrint("d_cellp"); RPrint(d_cellp, i_size_cellp); 

															   //----------------------------------------
															   //1. if the deleted is missing unit, no further action is taken
															   //2. if the deleted is observed unit, then the fractional weights are re-computed 
															   //----------------------------------------
			int* idd = new int[nrow_mox]; //location of the deleted donor in ocg 
			Fill_iVector(idd, nrow_mox, 0);
			if (fabs(d_rr0[l]) > 0)
			{
				//---------------------
				//locations of the deleted unit in observed list
				//---------------------
				std::vector<int> v_lg; //Actual locations 
				v_lg.clear();
				for (int j = 0; j<nrow_mox; j++) //list length 
				{
					for (int k = 0; k<i_locg[j]; k++) //a row in the List
					{
						int i_temp_lg = (int)List_ocsg(j, k);
						if (i_temp_lg == (l + 1)) //+1 for actual location  
						{
							v_lg.push_back(j + 1); //actual row location 
							idd[j] = k + 1; //actual location 
							break;
						}
					}
				}
				const int nlg = (int)v_lg.size();

				//testout
				//RPrint(" in condition rr0[l]!=0 at l+1 ="); RPrint(l+1);
				//RPrint("idd:"); RPrint(idd, nrow_mox);
				//RPrint("lg :"); RPrint(v_lg);
				//RPrint("nlg:"); RPrint(nlg); 

				//--------------------------
				//Adjust fractional weights for all units in lg
				//--------------------------
				if (nlg>0)
				{
					for (int j = 0; j<nlg; j++)
					{
						int i_row_lg = v_lg[j] - 1; // row number [0,...) 
						double* d_1_mox = new double[ncol];
						for (int k = 0; k<ncol; k++) d_1_mox[k] = rbind_mox(i_row_lg, k);

						//---
						//actual col number of missing cell in current missing row
						//---
						std::vector<int> v_rloc; v_rloc.clear();
						for (int k = 0; k<ncol; k++)
						{
							if (d_1_mox[k] == 0.0) { v_rloc.push_back(k + 1); } //actual col
						}
						//const int nrloc = (int)v_rloc.size();
						std::string cng;
						Trans1(d_1_mox, ncol, cng);

						//-------
						//location of cn which has cng
						//-------
						std::vector<int> v_mlog; v_mlog.clear();
						which(cn, nrow, cng, v_mlog);
						const int nmlog = (int)v_mlog.size();

						//testout
						//RPrint("rloc: "); RPrint(v_rloc);
						//RPrint("mlog: "); RPrint(v_mlog);

						//------------------------
						//------------------------
						//FEFI
						//------------------------
						//------------------------
						std::vector<int> v_elog;
						if (s_M.compare("FEFI") == 0) //0=equal 
						{
							//-----
							//find locations of mlog in dat2$ID
							//v_mlog contains the row numbers that have the same string as
							//current missing row 
							//nmlog = n(v_mlog)
							//-----
							v_elog.clear();
							for (int k1 = 0; k1<nmlog; k1++) //loop for mlog
							{
								int i_temp1 = id[v_mlog[k1] - 1]; //dat1$ID in R version  
								for (int k2 = 0; k2<nrow_dat2_FEFI; k2++)
								{
									int i_temp2 = rbind_ipmat_FEFI(k2, 0); //1st col is dat2$ID
									if (i_temp1 == i_temp2)
									{
										v_elog.push_back(k2 + 1); //actual location 
									}
								}
							}
							const int i_size_v_elog = (int)v_elog.size();
							//testout
							//RPrint(" elog: "); RPrint(v_elog); 

							//----
							//donor id
							//-----
							std::vector<int> v_did; v_did.clear();

							int i_temp_lg_j = v_lg[j] - 1; //-1 for actual location  
							for (int k1 = 0; k1<i_locg[i_temp_lg_j]; k1++) //a row in the List 
							{
								v_did.push_back((int)List_ocsg(i_temp_lg_j, k1));
							}
							const int i_size_nic = (int)v_did.size(); //length of did 

																	  //----
																	  //donor string patterns
																	  //----
																	  //std::string s_icn[i_size_nic];
							std::string *s_icn = new std::string[i_size_nic];
							for (int k1 = 0; k1<i_size_nic; k1++)
								s_icn[k1] = cn[v_did[k1] - 1]; //-1 for actual location  		

															   //testout 
															   //RPrint("did :"); RPrint(v_did);
															   //RPrint("icn :"); RPrint(s_icn, i_size_nic);

															   //------
															   //unique icn
															   //-------
							std::vector<std::string> v_unique_icn; v_unique_icn.clear();
							//std::string s_icn_backup[i_size_nic];
							std::string *s_icn_backup = new std::string[i_size_nic];
							for (int k1 = 0; k1<i_size_nic; k1++) s_icn_backup[k1] = s_icn[k1];

							for (int k1 = 0; k1<i_size_nic; k1++)
							{
								std::string s_uicn_temp = s_icn[k1];
								for (int k2 = 0; k2<i_size_nic; k2++)
								{
									if (s_uicn_temp.compare(s_icn_backup[k2]) == 0)
									{
										//store the found unique string pattern 
										v_unique_icn.push_back(s_uicn_temp);

										//nullify all the remaining unit that has the same string
										for (int k3 = k2; k3<i_size_nic; k3++)
										{
											if (s_uicn_temp.compare(s_icn_backup[k3]) == 0)
												s_icn_backup[k3] = ""; //nullify
										}

										break;
									}
								}
							}
							const int i_unic = (int)v_unique_icn.size();

							//testout 
							//RPrint("v_unique_icn :"); RPrint(v_unique_icn);
							//RPrint("i_unic :"); 	  RPrint(i_unic);

							//----------
							//sort the unique donors
							//----------
							//std::string s_unique_icn_sorted[i_unic]; //sorted donors
							std::string *s_unique_icn_sorted = new std::string[i_unic]; //sorted donors
							for (int k1 = 0; k1<i_unic; k1++) s_unique_icn_sorted[k1] = v_unique_icn[k1];
							std::sort(s_unique_icn_sorted, s_unique_icn_sorted + i_unic);

							//---------
							//match the sorted unique donors to njp
							//---------
							//make "njp" 
							int i_temp2 = 0; List_rst_name.get_a_row_size(0, i_temp2);
							//std::string s_njp[i_temp2]; 
							std::string *s_njp = new std::string[i_temp2];
							for (int k1 = 0; k1<i_temp2; k1++) s_njp[k1] = List_rst_name(0, k1);

							std::vector<int> v_icn_njp; v_icn_njp.clear();
							for (int k1 = 0; k1<i_unic; k1++)
							{
								std::string s_temp_cp = s_unique_icn_sorted[k1];

								for (int k2 = 0; k2<i_temp2; k2++)
								{
									if (s_temp_cp.compare(s_njp[k2]) == 0)
									{
										v_icn_njp.push_back(k2 + 1); break;
									} //+1 for actual location 
								}
							}
							const int i_size_v_icn_njp = (int)v_icn_njp.size();


							//-------------------------------------
							//get joint probability at the matched location only
							//-------------------------------------
							//-----------
							//when zero size continue to next iteration
							//-----------
							if (i_size_v_icn_njp <= 0) { continue; }
							double* d_cp = new double[i_size_v_icn_njp]; //joint prob
																		 //std::string s_ncp[i_size_v_icn_njp];          //names of the matches
							std::string *s_ncp = new std::string[i_size_v_icn_njp];          //names of the matches
							double d_sum_cp = 0.0;
							for (int k1 = 0; k1<i_size_v_icn_njp; k1++)
							{
								d_cp[k1] = d_cellp[v_icn_njp[k1] - 1]; //-1 for actual location 
								s_ncp[k1] = s_njp[v_icn_njp[k1] - 1];

								d_sum_cp += d_cp[k1]; //summation of d_cp[]
							}
							for (int k1 = 0; k1<i_size_v_icn_njp; k1++)
							{
								d_cp[k1] = d_cp[k1] / d_sum_cp;
							}

							//testout
							//RPrint("njp"); RPrint(s_njp, i_temp2); 
							//RPrint("match unique icn and njp :"); RPrint(v_icn_njp); 
							//RPrint("cp"); RPrint(d_cp, i_size_v_icn_njp);
							//RPrint("ncp"); RPrint(s_ncp, i_size_v_icn_njp);						

							//-----------
							//match donors 
							//-----------
							std::vector<int> v_obsp; v_obsp.clear();
							std::vector<double> v_obsp_cp; v_obsp_cp.clear();
							match_FHDI(s_icn, i_size_nic,
								s_ncp, i_size_v_icn_njp,
								v_obsp);
							const int i_size_v_obsp = (int)v_obsp.size();
							for (int k1 = 0; k1<i_size_v_obsp; k1++)
								v_obsp_cp.push_back(d_cp[v_obsp[k1] - 1]);

							//---------------------------------------
							//Replicated sampling weights for donors
							//---------------------------------------
							//-----------
							//when zero size continue to next iteration
							//-----------
							if (i_size_nic <= 0) { continue; }
							double* d_drw0 = new double[i_size_nic]; //length of did
							double d_sum_drw0 = 0.0;
							for (int k1 = 0; k1<i_size_nic; k1++)
							{
								d_drw0[k1] = rw0[v_did[k1] - 1];
								d_sum_drw0 += d_drw0[k1];
							}

							//----------
							//update weighted probability
							//----------
							std::vector<std::string> jp_name_icn; jp_name_icn.clear();
							std::vector<double> 	 jp_prob_icn; jp_prob_icn.clear();

							wpct_FHDI(s_icn, i_size_nic, d_drw0,
								jp_name_icn, jp_prob_icn);
							const int i_size_jp_prob_icn = (int)jp_prob_icn.size(); //=i_unic
							for (int k1 = 0; k1<i_size_jp_prob_icn; k1++)
								jp_prob_icn[k1] = jp_prob_icn[k1] * d_sum_drw0;  //ws.icn

																				 //testout
																				 //RPrint("obsp: "); RPrint(v_obsp);
																				 //RPrint("jp_name_icn :"); RPrint(jp_name_icn);
																				 //RPrint("jp_prob_icn = ws.icn :"); RPrint(jp_prob_icn);



							const int i_current_locg = i_locg[v_lg[j] - 1]; //-1 for actual loc
																			//-----------
																			//when zero size continue to next iteration
																			//-----------
							if (i_size_v_obsp <= 0) { continue; }
							double* d_Fefiw = new double[i_size_v_obsp];
							//---------------
							//weight update I: at deleted donor locations
							//---------------
							if (i_unic == i_current_locg)
							{
								for (int k1 = 0; k1<i_size_v_obsp; k1++)
									d_Fefiw[k1] = v_obsp_cp[k1];
							}
							//-----------------------------------------------
							//weight update II: at deleted donor locations
							//-----------------------------------------------
							//-----------
							//when zero size continue to next iteration
							//-----------
							if (i_size_nic <= 0) { continue; }
							double* d_drw1 = new double[i_size_nic];
							Fill_dVector(d_drw1, i_size_nic, 0.0); //initialized with 0
							if (i_unic < i_current_locg)
							{
								//-----------
								//match donors 
								//-----------
								std::vector<int> v_temp; v_temp.clear();
								match_FHDI(s_icn, i_size_nic,
									s_unique_icn_sorted, i_unic,
									v_temp);

								for (int k1 = 0; k1<i_size_nic; k1++)
								{
									if (jp_prob_icn[v_temp[k1] - 1] != 0.0)
										d_drw1[k1] = d_drw0[k1] / jp_prob_icn[v_temp[k1] - 1];
								}

								//------------
								//final updated weights
								//------------
								for (int k1 = 0; k1<i_size_nic; k1++)
									d_Fefiw[k1] = v_obsp_cp[k1] * d_drw1[k1];

								//testout
								/*
								RPrint("d_drw0 :"); RPrint(d_drw0, i_size_nic);
								RPrint("jp_prob_icn[v_temp[k1]-1] :");
								for(int k1=0; k1<i_size_nic; k1++)
								RPrint(jp_prob_icn[v_temp[k1]-1]);
								RPrint("jp_name_icn[v_temp[k1]-1] :");
								std::vector<std::string> v_s_temp; v_s_temp.clear();
								for(int k1=0; k1<i_size_nic; k1++)
								{	v_s_temp.push_back(jp_name_icn[v_temp[k1]-1]); }
								RPrint(v_s_temp);
								RPrint("v_obsp_cp :"); RPrint(v_obsp_cp);
								RPrint("d_drw1 :"); RPrint(d_drw1, i_size_nic);
								RPrint("d_Fefiw :"); RPrint(d_Fefiw, i_size_nic);
								*/

							}

							//------------
							//update wijk: repeated copy, if needed
							//------------
							int i_cycle_wijk = (int)floor(i_size_v_elog*1.0 / i_size_nic*1.0); //expected evenly divisible
							int i_loc_elog = 0; //sequential id

												//testout
												//RPrint("v_elog :"); RPrint(v_elog);
												//RPrint("wijk (updated one only):"); 

							for (int k1 = 0; k1<i_cycle_wijk;k1++)
							{
								for (int k2 = 0; k2<i_size_nic; k2++)
								{
									wijk[v_elog[i_loc_elog++] - 1] = d_Fefiw[k2];
									//testout
									//RPrint(d_Fefiw[k2]);
								}
							}

							//---
							//local deallocation
							//---
							delete[] s_icn;
							delete[] s_icn_backup;
							delete[] d_cp;
							delete[] d_drw0;
							delete[] d_Fefiw;
							delete[] d_drw1;
						}

						//local deallocation 
						delete[] d_1_mox;
					}
				}

			}


			//-------------------------------
			//store the updated weights
			//-------------------------------
			int i_nrow_imputation = nrow;
			if (s_M == "FEFI") i_nrow_imputation = nrow_dat2_FEFI;
			if (s_M == "FHDI") i_nrow_imputation = nrow_dat2_FHDI;

			for (int k1 = 0; k1<i_nrow_imputation; k1++)
			{
				wmat[k1][l] = Rw[k1] * wijk[k1];
			}

			//testout

			/*
			double* d_temp_wmat1 = new double[i_nrow_imputation];
			for(int j=0; j<i_nrow_imputation; j++) d_temp_wmat1[j] = wmat[j][l];
			RPrint("wmat[,l]:");
			RPrint(d_temp_wmat1, i_nrow_imputation);
			delete[] d_temp_wmat1;
			*/

			//--------------------
			//local deallocation
			//--------------------
			delete[] idd;
			delete[] d_cellp;



		} //end of main loop for L
		  //testout
		  //RPrint(" ========= Variance_Est_FEFI.. has successfully finished!");
		Rprintf(" ========= Variance estimation KNN has successfully finished!\n");
		//testout 
		//cout<<"Main Var Est FEFI: Variance_Est_FEFI.. has successfully finished!"<<endl;			  

		//-------------
		//deallocation
		//-------------
		//delete[] w; 
		//delete[] id; 
		delete[] cn;
		delete[] d_id_FEFI;
		Del_dMatrix(d_iy, nrow_d_iy, ncol);
		Del_dMatrix(d_cx, nrow, ncol);
		delete[] i_locg;
		delete[] d_rr0;
		delete[] d_w1;
		//Del_dMatrix(wmat, nrow_dat2_FEFI, L);
		delete[] rw0;
		delete[] Rw;
		delete[] wijk;

		return 1;
	}

}//end of namespace


//Fn===========================================================================

//Variance_Est_FHDI_Extension_cpp.cc-----------------------------------------------------------------------------

//Fn===========================================================================

namespace FHDI

{

//below are defined in Variance_Est_FEFI_Extension_cpp.cc



//void RepWeight(const int n, double** d_rw) 

//void Rep_CellP(double** d_cx, const int nrow, const int ncol, double** d_rw, int*  id, 

//			   List_FHDI        &List_rst_prob,

//			   List_string_FHDI &List_rst_name,

//			   std::vector<std::string> &s_ncx)





bool Variance_Est_FHDI_Extension_cpp(double** y, double** z, const int nrow, const int ncol, 

	FHDI::RepWeight_FHDI &d_rw, double* w, int* id, 

	rbind_FHDI  &rbind_ipmat_FEFI,

	rbind_FHDI  &rbind_Resp_FEFI,

	rbind_FHDI  &rbind_irmat_FEFI,

	rbind_FHDI  &rbind_ipmat_FHDI,

	rbind_FHDI  &rbind_Resp_FHDI,

	rbind_FHDI  &rbind_irmat_FHDI,

	rbind_FHDI  &rbind_uox,

	rbind_FHDI  &rbind_mox,

	List_FHDI 	&List_ord,

	List_FHDI 	&List_ocsg, 

	std::string s_M, 

	double** wmat)



//Description----------------------

//estimate variance for FHDI using Jackknife method 

//  Algorithm: 

//

// original R code: Dr. Im, J. and Dr. Kim, J. 

// c++ code: 		Dr. Cho, I. 

// All rights reserved

// 

// updated: March 28, 2017

//

//IN   : double y(nrow, ncol)= original data matrix with missing cells 

//IN   : double z(nrow, ncol)= categorized matrix of y. 0 for missing cell

//IN   : double d_rw[nrow, nrow] = replicate weights 

//IN   : double w(nrow) = sampling weight (default = 1.0)

//IN   : int    id(nrow) = id number of each row (default = 1 to nrow)

//FEFI --------returns----------------- FEFI //

//IN   : rbind_FHDI  rbind_ipmat_FEFI(4+ncol); //column size is 4+ncol

//IN   : rbind_FHDI  rbind_Resp_FEFI(ncol+1);  //separate response matrix  

//IN   : rbind_FHDI  rbind_irmat_FEFI(5+ncol); //column size is 5+ncol

//FHDI --------returns----------------- FHDI //

//IN   : rbind_FHDI  rbind_ipmat_FHDI(4+ncol); //column size is 4+ncol

//IN   : rbind_FHDI  rbind_Resp_FHDI(ncol+1);  //separate response matrix  

//IN   : rbind_FHDI  rbind_irmat_FHDI(5+ncol); //column size is 5+ncol

//other matrices

//IN   : rbind_FHDI  rbind_uox(ncol); //observed unique categorized matrix 

//IN   : rbind_FHDI  rbind_mox(ncol); //missing  unique categorized matrix

//Note: below Lists contain meaningful items up to i_count_mox rows  

//IN   : List_FHDI 	List_ord(nrow); //order records used for variance estimation

//IN   : List_FHDI 	List_ocsg(nrow); //order records used for variance estimation

//IN   : std::string s_M = "FEFI" = fully efficient fractional imputation

//						   "FHDI" = fractional hot deck imputation

//OUT  : double** wmat = New_dMatrix(nrow_dat2_FHDI, L=nrow); //nrow_dat2_FHDI = rows of w1

//

//    

//Data Structure Note

//----------------------

//dat1 in R version:  id   w  y_matrix  z_matrix

//     in C++ ver  :  id   w  y         z

//----------------------

//dat2 in R version:  id  FID  WGT  FWGT  imputed matrix     Response(0/1)

//     in C++ ver  :  ----- rbind_ipmat_FEFI------------     rbind_Resp_FEFI 

//                 :  ----- rbind_ipmat_FHDI------------     rbind_Resp_FHDI

//----------------------

//

//ipmat  = final imputation results

//     	col1: ID 	= unit index

//		col2: FID 	= ID of fractionally imputed value

// 		col3: WGT 	= weight 

//		col4: FWGT	= Frational weight

//		col5: Variables 

//		col6: Responses (separately in  rbind_Resp_...)

//------------------------

//

//irmat  = imputation results related to the categorized matrix 

//     	col1: ID 	= unit index

//		col2: FID 	= ID of fractionally imputed value

//		col3: OID	= original rank of the imputed value

//		col4: ORDER = SN(selected donor)

//		col5: FEFIW	= Fefi weights 

//		col6: CELL	= cells 

//----------------------

{

	

	//----------------------------

	//Basic constants declaration

	//----------------------------



	const int nrow_dat2_FEFI = rbind_ipmat_FEFI.size_row(); 

	const int nrow_dat2_FHDI = rbind_ipmat_FHDI.size_row(); 	

	const int nrow_mox 		 = rbind_mox.size_row(); 

	const int L = nrow; //size of d_rw 

	

	//--------------------

	//get ready id table of FEFI

	//--------------------

	double* d_id_FHDI = new double[nrow_dat2_FHDI]; 

	for(int i=0; i<nrow_dat2_FHDI; i++) d_id_FHDI[i] = rbind_ipmat_FHDI(i,0); //id, 1st col 

	std::vector<double> v_table_name_id_FHDI; //same as "nimp" in R version

	std::vector<int>    v_table_count_id_FHDI;//same as "nimp" in R version 

	table_cpp(d_id_FHDI, nrow_dat2_FHDI, v_table_name_id_FHDI, v_table_count_id_FHDI);  



	//---------------------

	//imputed real data matrix

	//---------------------

	int nrow_d_iy = nrow_dat2_FHDI; //default

	if(s_M== "FDFI") {nrow_d_iy = nrow_dat2_FEFI;}

	if(s_M== "FHDI") {nrow_d_iy = nrow_dat2_FHDI;}

	double** d_iy = New_dMatrix(nrow_d_iy, ncol); //imputed matrix of real values 

	for(int i=0; i<ncol; i++)

	{

		for(int j=0; j<nrow_d_iy; j++)

		{

			if(s_M == "FEFI")

			{d_iy[j][i] = rbind_ipmat_FEFI(j,4+i);} //col5~ncol contains imputed real values 

			if(s_M == "FHDI")

			{d_iy[j][i] = rbind_ipmat_FHDI(j,4+i);} //col5~ncol contains imputed real values 



		}

	}

	

	//----------------------

	//categorized matrix

	//----------------------

	double** d_cx = New_dMatrix(nrow, ncol);

	Copy_dMatrix(z, nrow, ncol, d_cx);

	

	//---------------------

	//ocg, observed donors for each missing pattern. 

	//---------------------

    //the same as List_ocsg[nrow_mox]

	//---------------------

	int* i_locg = new int[nrow_mox]; //length of donor rows for each missing pattern 

	for(int i=0; i<nrow_mox; i++)

	{

		int i_temp = 0; 

		List_ocsg.get_a_row_size(i, i_temp);

		i_locg[i] = i_temp; //meaning how many rows used as the donor for ith missing pattern 

	}

	

	

	//------------------------

	//cell probability using replicate weight

	//------------------------

    List_FHDI         List_rst_prob(nrow); //only i_nc rows are meaningful

    List_string_FHDI  List_rst_name(nrow); //only i_nc rows are meaningful

    std::vector<std::string> s_ncx;

	

    bool b_success_Rep_CellP = Rep_CellP(d_cx, nrow, ncol, d_rw, id, 

			  List_rst_prob,

			  List_rst_name,

			  s_ncx);	

	if(!b_success_Rep_CellP)
	{			
		Rprintf("Error! Rep_CellP Failed! Change k, check data quality, further break down categorical variables, or so. It may help \n");
		
		return 0; //abnormal ending 								
	}
			  

	//--------------------

	//put 1 into fully observed rows

	//--------------------

	double* d_rr0 = new double[nrow];

	for(int i=0; i<nrow; i++)

	{

		double d_prod = 1.0; 

		for(int j=0; j<ncol; j++) 

		{

			d_prod = d_prod*d_cx[i][j]; 

		}

		//-----

		//0: at least one missing;  1: all observed

		//-----

		d_rr0[i] = d_prod; 

		if(fabs_FHDI(d_prod) >0.0) d_rr0[i] = 1; 

	}

	

	//--------------

	//calculate w1 = sampling weight

	//--------------

	//std::string cn[nrow]; 

	std::string *cn = new std::string[nrow]; 

	Trans(d_cx, nrow, ncol, cn);

    double* d_w1 = new double[nrow_dat2_FHDI]; 

	for(int i=0; i<nrow_dat2_FHDI; i++) 

		d_w1[i] = rbind_ipmat_FHDI(i,2); //3rd column contains WGT

	

	

	//------------------

	//make Covariance matrix for the subsequent replication process

	//------------------

	int* i_lloc; 

	std::vector<int> v_mox_0; 

	double** d_dy; 

	double** V_var; //covariance matrix of dy

	List_FHDI List_V(nrow_mox); //storage of covariance matrix

								//Note: store cov mat by "row-first" rule 

	for(int i=0; i<nrow_mox; i++)

	{

		//---------------------------

		//how many donor rows for the ith missing pattern

		//------------------------

		int i_size_lloc = i_locg[i]; 

		//-----------

		//when zero size continue to next iteration

	    //-----------

		if(i_size_lloc <= 0) {continue;}		

		i_lloc = new int[i_size_lloc]; //vector of actual donor row numbers 

		for(int j=0; j< i_size_lloc; j++) i_lloc[j] =(int)List_ocsg(i,j); //ith row, jth entity

		

		//-----

		//find missing columns in current missing row

		//------

		v_mox_0.clear();

		for(int j=0; j<ncol; j++) 

		{

			if(rbind_mox(i,j) == 0.0) v_mox_0.push_back(j+1); //ACTUAL zero column id 

		}

		const int i_size_v_mox_0 = (int)v_mox_0.size(); 

		

		//----------------------------------

		//extract matrix of missing patterns

		//----------------------------------

		//-----------

		//when zero size continue to next iteration

	    //-----------

		if(i_size_lloc <= 0) {continue;}		

		if(i_size_v_mox_0 <= 0) {continue;}		

		d_dy  = New_dMatrix(i_size_lloc, i_size_v_mox_0);

		V_var = New_dMatrix(i_size_v_mox_0, i_size_v_mox_0); //column-wise covariance 

		for(int j=0; j< i_size_lloc; j++) //LOOP for donor rows 

		{

			for(int k=0; k<i_size_v_mox_0; k++) //LOOP for missing columns 

			{

				d_dy[j][k] = y[i_lloc[j]-1][v_mox_0[k]-1]; //-1 for actual location 

			}

		}

		//----------

		//"Estimated covariance" of d_dy by column-to-column method

		//----------

		cov_FHDI(d_dy, i_size_lloc, i_size_v_mox_0, V_var); 



		

		//----------

		//store the covariance matrix 

		//row-first rule

		//----------

		List_V.put_block(i, i_size_v_mox_0, i_size_v_mox_0, V_var); //direct matrix saving



		//---

		//local deallocation

		//---

		delete[] i_lloc; 

		Del_dMatrix(d_dy,  i_size_lloc, i_size_v_mox_0);

		Del_dMatrix(V_var, i_size_v_mox_0, i_size_v_mox_0); 

		//delete[] d_V_temp; 

		

	}

	

	//------------------------------

	//------------------------------

	//MAIn loop for L replications

	//------------------------------

	//------------------------------

	double* rw0 = new double[nrow];

	

	int i_sum_Rw = 0; 

	for(int i=0; i<nrow; i++) i_sum_Rw+= v_table_count_id_FHDI[i]; 

	double* Rw  = new double[i_sum_Rw];

	

	double* wijk = new double[nrow_dat2_FHDI]; //FWGT from ipmat 

	

	for(int l=0; l<L; l++)

	{

		//-------

		//replicate weight from lth column

		//-------

		for(int i=0; i<nrow; i++) rw0[i] = d_rw(i, l);   //previous:  d_rw[i][l]; //l_th column 

		int i_sum =0; 

		for(int i=0; i<nrow; i++)

		{

			for(int j=0; j<v_table_count_id_FHDI[i]; j++) Rw[i_sum++]=rw0[i];

		}			



		//---------

		//FWGT of ipmat

		//----------

		for(int i=0; i<nrow_dat2_FHDI; i++)

			wijk[i] = rbind_ipmat_FHDI(i,3); //4th column is FWGT 

		

		//----------

		//joint probability associated with current string

		//-----------

		std::string cn_current = cn[l]; //lth string 

		std::vector<int> v_ncx_cn; 

		which(s_ncx, cn_current, v_ncx_cn); //actual location 

		//const int i_size_v_ncx_cn = (int)v_ncx_cn.size(); //MUST BE "1"

		

		int i_size_cellp = 0;

		List_rst_prob.get_a_row_size(v_ncx_cn[0]-1, i_size_cellp); //get a size of the row in the list 

		//-----------

		//when zero size continue to next iteration

	    //-----------

		if(i_size_cellp <= 0) {continue;}		

		double* d_cellp = new double[i_size_cellp]; 

		List_rst_prob.get_block(v_ncx_cn[0]-1, d_cellp); //-1 for actual row location 

		

		//----------------------------------------

		//1. if the deleted is missing unit, no further action is taken

		//2. if the deleted is observed unit, then the fractional weights are re-computed 

		//----------------------------------------

		int* idd = new int[nrow_mox]; //location of the deleted donor in ocg 

		Fill_iVector(idd, nrow_mox, 0); 

		if(fabs_FHDI(d_rr0[l]) > 0)

		{

			//---------------------

			//locations of the deleted unit in observed list

			//---------------------

			std::vector<int> v_lg; //Actual locations 

			v_lg.clear(); 

			for(int j=0; j<nrow_mox; j++) //all missing patterns

			{

				for(int k=0; k<i_locg[j]; k++) //donor rows for the jth missing pattern

				{

					int i_temp_lg = (int)List_ocsg(j,k); 

					if( i_temp_lg == (l+1)) //+1 for actual location  

					{

						v_lg.push_back(j+1); //actual id of jth missing pattern  

						idd[j] = k+1; //actual number of donor rows for jth missing pattern 

						break; 

					}

				}

			}

			const int nlg = (int)v_lg.size(); 

			

			

			//--------------------------

			//Adjust fractional weights for all units in lg

			//--------------------------

			if(nlg>0)

			{

				for(int j=0; j<nlg; j++)

				{

					int i_row_lg = v_lg[j]-1; // row number [0,...) 

					double* d_1_mox = new double[ncol];

					for(int k=0; k<ncol; k++) d_1_mox[k] = rbind_mox(i_row_lg,k);



					//---

					//actual col number of missing cell in current missing row

					//---

					std::vector<int> v_rloc; v_rloc.clear(); 

					for(int k=0; k<ncol; k++) 

					{

						if(d_1_mox[k] == 0.0) {v_rloc.push_back(k+1);} //actual col

					}	



					//---

					//number of missing columns in current missing row

					//---

					const int nrloc = (int)v_rloc.size(); 

					std::string cng; 

					Trans1(d_1_mox, ncol, cng); 

					

					//-------

					//location of cn which has cng

					//-------

					std::vector<int> v_mlog; v_mlog.clear();

					which(cn, nrow, cng, v_mlog); 

					const int nmlog = (int)v_mlog.size(); 

										



					//------------------------

					//------------------------

					//FHDI

					//------------------------

					//------------------------

					std::vector<int> v_elog; v_elog.clear(); 

					if(s_M.compare("FHDI")==0) //0=equal 

					{

						//-----

						//find locations of mlog in dat2$ID

						//v_mlog contains the row numbers that have the same string as

						//current missing row 

						//nmlog = n(v_mlog)

						//-----

						v_elog.clear(); 

						for(int k1=0; k1<nmlog; k1++) //loop for mlog

						{

							int i_temp1 = id[v_mlog[k1]-1]; //dat1$ID in R version  

							for(int k2=0; k2<nrow_dat2_FHDI; k2++)

							{

								int i_temp2 = rbind_ipmat_FHDI(k2, 0); //1st col is dat2$ID

								if(i_temp1 == i_temp2)

								{

									v_elog.push_back(k2+1); //actual location 

								}

							}

						}

						const int i_size_v_elog = (int)v_elog.size(); 

						

						//------------------------------------

						//set of donors for missing columns

                        //------------------------------------

						//-----------

						//when zero size continue to next iteration

						//-----------

						if(i_size_v_elog <= 0) {continue;}

						if(nrloc <= 0) {continue;}						

						double** dy_FHDI = New_dMatrix(i_size_v_elog, nrloc); 

						for(int k1=0; k1<nrloc; k1++)

						{

							for(int k2=0; k2<i_size_v_elog; k2++)

							{

								dy_FHDI[k2][k1] = d_iy[v_elog[k2]-1][v_rloc[k1]-1]; 

							}

						}

						

						//---------------------

						// nrloc >= 1: number of missing columns in current missing row

						//---------------------

						if(nrloc >= 1)

						{

							

							//----

							//make dk matrix

							//filled with l_th original data 

							//at missing column locations  

							//Note: this is the Jackknifed row

							//----------------------------------------

							//-----------

							//when zero size continue to next iteration

							//-----------

							if(i_size_v_elog <= 0) {continue;}	

							if(nrloc <= 0) {continue;}								

							double** dk = New_dMatrix(i_size_v_elog, nrloc); 

							for(int k1=0; k1<nrloc; k1++)

							{

								double d_temp_dk = y[l][v_rloc[k1]-1]; //-1 for actual location  

								for(int k2=0; k2<i_size_v_elog; k2++)

								{

									dk[k2][k1] =  d_temp_dk; 

								}

							}

							

							//---------

							//difference between   dy         and dk 

							//i.e., difference b/w donor rows and jackknifed row  

							//-------------------------------------------

							//-----------

							//when zero size continue to next iteration

							//-----------

							if(i_size_v_elog <= 0) {continue;}	

							if(nrloc <= 0) {continue;}												

							double** diff = New_dMatrix(i_size_v_elog, nrloc);

							for(int k1=0; k1<nrloc; k1++)

							{

								for(int k2=0; k2<i_size_v_elog; k2++)

								{

									diff[k2][k1] = dk[k2][k1] - dy_FHDI[k2][k1]; 

								}

							}

							

							

							//----------

							//get l_th covariance matrix

							//--------------------------------------------

							//-----------

							//when zero size continue to next iteration

							//-----------

							if(nrloc <= 0) {continue;}									

							double** V_var_l = New_dMatrix(nrloc, nrloc); 

							int i_loc_lg_j = v_lg[j]-1; //-1 for actual location 

							List_V.get_block(i_loc_lg_j, nrloc, nrloc, V_var_l); //matrix read by row-first rule

							

						

							//-------------------------------------

							// diff * (V)^-1 *diff^T

							//-------------------------------------

							//-----------

							//when zero size continue to next iteration

							//-----------

							if(nrloc <= 0) {continue;}										

							double** V_inv = New_dMatrix(nrloc, nrloc); 

							

							bool b_success_V_inv = true; //false when abrupt exit due to zero diagonal

							if(nrloc > 1) b_success_V_inv = Inverse_dMatrix_FHDI(V_var_l, nrloc, V_inv);

							if(nrloc ==1) V_inv[0][0] = 1.0/V_var_l[0][0];

							if( !b_success_V_inv ) 

							{

							    

                                //----

								//below is an option to abort 

								//zero-diagonal Variance matrix

								//However, if donor vector was zero

								//vector, exceptional consideration

								//is needed

								//Feb 07, 2017

								//----

								//Rprintf("Error! zero diagonal term in V_var_l"); return; 

								

								//----

								//special remedy for zero diagonal covariance matrix

								//Feb 7 2017

								//This is enough since we are interested in 

								//relative ordering to find minimum distance later

								//----

								for(int i_inv1=0; i_inv1<nrloc; i_inv1++)

								{

									double d_temp_inv = V_var_l[i_inv1][i_inv1];

									

									//for zero diagonal term: a big number 

									if(fabs_FHDI(d_temp_inv) <= 1e-13) 

									{	V_inv[i_inv1][i_inv1] = 1E15; }



									//for non-zero diagonal term: simple inverse

									if(fabs_FHDI(d_temp_inv) > 1e-13) 

									{V_inv[i_inv1][i_inv1] = 1.0/d_temp_inv; }

								}

							}

							//-----------

							//when zero size continue to next iteration

							//-----------

							if(nrloc <= 0) {continue;}			

							if(i_size_v_elog <= 0) {continue;}										

							double** diff_T = New_dMatrix(nrloc, i_size_v_elog);

							for(int k1=0; k1<nrloc; k1++)

							{

								for(int k2=0; k2<i_size_v_elog; k2++)

								{

									diff_T[k1][k2] = diff[k2][k1]; 

								}

							}

							//-----------

							//when zero size continue to next iteration

							//-----------

							if(i_size_v_elog <= 0) {continue;}										

							double** diff_V_diffT = New_dMatrix(i_size_v_elog, i_size_v_elog); 

							dMatrix_Mul_AtBA(diff_T, nrloc, i_size_v_elog,

                                             V_inv, diff_V_diffT); 



							//-----------

							//score = diagonal terms

							//-----------

							double* d_score = new double[i_size_v_elog];

							for(int k1=0; k1<i_size_v_elog; k1++)

								d_score[k1] = diff_V_diffT[k1][k1]; 



							

							//------

							//MM calculation

							//------

							if(nmlog == 0) 

							{Rprintf("Caution! zero mlog!"); continue; }

							

							const int MM = i_size_v_elog/nmlog; 

							const int ncol_imt = (int)floor(i_size_v_elog*1.0/(MM*1.0));

							//-----------

							//when zero size continue to next iteration

							//-----------

							if(ncol_imt <= 0) {continue;}	

							if(MM <= 0) {continue;}			

							double** d_imt = New_dMatrix(MM, ncol_imt);

							int i_score=0; 

							for(int k1=0; k1<ncol_imt; k1++)

							{

								for(int k2=0; k2<MM; k2++) //row-first rule 

								{

									d_imt[k2][k1] = d_score[i_score++]; 

								}

							}

							

							//-----------------------------------------

							//extract weights

							//-----------------------------------------

							//-----------

							//when zero size continue to next iteration

							//-----------

							if(i_size_v_elog <= 0) {continue;}	

							double* ewijk = new double[i_size_v_elog];

							double* fefiw = new double[i_size_v_elog];

							

							for(int k1=0; k1<i_size_v_elog; k1++)

							{

								ewijk[k1] = wijk[v_elog[k1]-1]; //-1 for actual loc

								//5th column is FEFIW of fhdi[[2]] in r version 

								fefiw[k1] = rbind_irmat_FHDI(v_elog[k1]-1, 4); //5th column

							}

							

							

							//-------------------

							//find eloc

							//-------------------

							std::vector<int> LMM; LMM.clear(); 

							for(int k1=0; k1<nmlog; k1++) LMM.push_back(k1*MM); 

							//-----------

							//when zero size continue to next iteration

							//-----------

							if(nmlog <= 0) {continue;}	

							int* i_eloc = new int[nmlog]; 

							//column-wise min location (Actual)

							for(int k1=0; k1<nmlog; k1++)

							{

								double d_temp = d_imt[0][k1]; //1st row  

								int    i_min  = 1; 

								for(int k2=1; k2<MM; k2++)

								{

									if((d_temp - d_imt[k2][k1]) > 1e-3)

									{

										d_temp = d_imt[k2][k1];

										i_min = k2+1; //actual row location 

									}

								}

								//----

								//store the column-wise min location (Actual)

								//----

								i_eloc[k1] = i_min + LMM[k1]; 





							}

							

							//-------------

							//maximum difference (>0) between ewijk and fefiw

							//-------------------------------------

							//-----------

							//when zero size continue to next iteration

							//-----------

							if(nmlog <= 0) {continue;}								

							double* d_maxew = new double[nmlog]; //nmlog = length of i_eloc

							double* d_maxval = new double[nmlog]; //nmlog = length of i_eloc

							for(int k1=0; k1<nmlog; k1++)

							{

								double d_temp = ewijk[i_eloc[k1]-1] - fefiw[i_eloc[k1]-1]; //-1 for actual loc

								d_maxew[k1] = 0.0; 

								if(d_temp > 0.0) d_maxew[k1] =d_temp; 

								

								d_maxval[k1] = ewijk[i_eloc[k1]-1] - d_maxew[k1]; 

							}

							

							//------

							//ewijk update

							//------

							for(int k1=0; k1<nmlog; k1++)

							{

								ewijk[i_eloc[k1]-1] = d_maxew[k1]; 

							}

							

							//-------------

							//extend maxval array

							//-------------

							if(MM == 1) {Rprintf("Error! MM is 1 in Var FHDI \n"); return 0;}

							//-----------

							//when zero size continue to next iteration

							//-----------

							if(nmlog <= 0) {continue;}								

							double* d_maxval_extended = new double[nmlog*(MM-1)]; 

							int i_maxval = 0; 

							for(int k1=0; k1<nmlog; k1++)

							{

								double d_temp = d_maxval[k1]/(MM-1);

								for(int k2=0; k2<(MM-1); k2++)

								{

									d_maxval_extended[i_maxval++] = d_temp; 

								}

							}

							

							//---------------

							//update ewijk with extended maxval

							//---------------

							//exclude eloc locations

							//-----------

							//when zero size continue to next iteration

							//-----------

							if((i_size_v_elog - nmlog) <= 0) {continue;}								

							int* i_without_eloc = new int[i_size_v_elog - nmlog];

							int i_eloc_temp = 0; 

							for(int k1=0; k1<i_size_v_elog; k1++)

							{

								bool b_same = false; 

								for(int k2=0; k2<nmlog; k2++)

								{

									if(i_eloc[k2]-1 == k1) 

									{

										b_same=true;

										break; 

									}

								}

								if(!b_same) {i_without_eloc[i_eloc_temp++] = k1;}

							}

							//store values

							for(int k1=0; k1<(i_size_v_elog - nmlog); k1++)

							{

								int i_loc_ew = i_without_eloc[k1];  

								ewijk[i_loc_ew] = ewijk[i_loc_ew] + d_maxval_extended[k1];

							}

							//----------------

							//final update wijk with ewijk

							//----------------

							for(int k1=0; k1<i_size_v_elog; k1++)

							{

								wijk[v_elog[k1]-1] = ewijk[k1]; 

							}

							

							

							//---

							//local deallocation

							//---

							Del_dMatrix(dk,   i_size_v_elog, nrloc);

							Del_dMatrix(diff, i_size_v_elog, nrloc);

							Del_dMatrix(V_var_l, nrloc, nrloc); 

							Del_dMatrix(V_inv, nrloc, nrloc); 

							Del_dMatrix(diff_T, nrloc, i_size_v_elog);

							Del_dMatrix(diff_V_diffT, i_size_v_elog, i_size_v_elog);

							delete[] d_score; 

							Del_dMatrix(d_imt, MM, ncol_imt); 

							delete[] ewijk; 

							delete[] fefiw; 

							delete[] i_eloc; 

							delete[] d_maxew;

							delete[] d_maxval;

							delete[] d_maxval_extended;

							delete[] i_without_eloc;

							

							//NOTE: dy_FHDI has different order from R version

							//as of Nov 27, 2016. It looks fine overall, but 

							//need to check later !!!!

						}						

						

						

						//----------

						//local deallocation 

						//----------

						Del_dMatrix(dy_FHDI, i_size_v_elog, nrloc);

					}	

					

					//local deallocation 

					delete[] d_1_mox; 

				}

			}

			

		}

		

		

		//-------------------------------

		//store the updated weights

		//-------------------------------

		int i_nrow_imputation = nrow; 

		if(s_M == "FEFI") i_nrow_imputation = nrow_dat2_FEFI;

		if(s_M == "FHDI") i_nrow_imputation = nrow_dat2_FHDI;

		

		for(int k1=0; k1<i_nrow_imputation; k1++)

		{

			wmat[k1][l] = Rw[k1]*wijk[k1]; 

		}

		

		

		//--------------------

		//local deallocation

		//--------------------

		delete[] idd; 

		delete[] d_cellp;

		

		

		

	} //end of main loop for L

	

	

	//testout

	Rprintf(" ========= Variance estimation has successfully finished!\n");

		

	//-------------

	//deallocation

	//-------------

	//delete[] w; 

	//delete[] id;

	delete[] cn; 

	delete[] d_id_FHDI; 

	Del_dMatrix(d_iy, nrow_d_iy, ncol);

	Del_dMatrix(d_cx, nrow, ncol);

	delete[] i_locg; 

	delete[] d_rr0; 

	delete[] d_w1;

	//Del_dMatrix(wmat, nrow_dat2_FHDI, L);

	delete[] rw0; 

	delete[] Rw; 

	delete[] wijk;

	

	return 1;

}



}//end of namespace

 //Fn===========================================================================

 //Variance_Est_FHDI_Extension_Bigp_cpp.cc-----------------------------------------------------------------------------

 //Fn===========================================================================

namespace FHDI
{
	//below are defined in Variance_Est_FEFI_Extension_cpp.cc

	//void RepWeight(const int n, double** d_rw) 

	//void Rep_CellP(double** d_cx, const int nrow, const int ncol, double** d_rw, int*  id, 
	//			   List_FHDI        &List_rst_prob,
	//			   List_string_FHDI &List_rst_name,
	//			   std::vector<std::string> &s_ncx)


	bool Variance_Est_FHDI_Extension_Bigp_cpp(double** y, double** z, const int nrow, const int ncol, const int i_option_collapsing,
		FHDI::RepWeight_FHDI &d_rw, double* w, int* id,
		rbind_FHDI  &rbind_ipmat_FEFI,
		rbind_FHDI  &rbind_Resp_FEFI,
		rbind_FHDI  &rbind_irmat_FEFI,
		rbind_FHDI  &rbind_ipmat_FHDI,
		rbind_FHDI  &rbind_Resp_FHDI,
		rbind_FHDI  &rbind_irmat_FHDI,
		rbind_FHDI  &rbind_uox,
		rbind_FHDI  &rbind_mox,
		List_FHDI 	&List_ord,
		List_FHDI 	&List_ocsg,
		std::string s_M,
		double** wmat, int** codes)

		//Description----------------------
		//estimate variance for FHDI using Jackknife method 
		//  Algorithm: 
		//
		// original R code: Dr. Im, J. and Dr. Kim, J. 
		// c++ code: 		Dr. Cho, I. and Yicheng Yang
		// All rights reserved
		// 
		// updated: Feb 28, 2020
		//
		//IN   : double y(nrow, ncol)= original data matrix with missing cells 
		//IN   : double z(nrow, ncol)= categorized matrix of y. 0 for missing cell
		//IN   : double d_rw[nrow, nrow] = replicate weights 
		//IN   : double w(nrow) = sampling weight (default = 1.0)
		//IN   : int    id(nrow) = id number of each row (default = 1 to nrow)
		//FEFI --------returns----------------- FEFI //
		//IN   : rbind_FHDI  rbind_ipmat_FEFI(4+ncol); //column size is 4+ncol
		//IN   : rbind_FHDI  rbind_Resp_FEFI(ncol+1);  //separate response matrix  
		//IN   : rbind_FHDI  rbind_irmat_FEFI(5+ncol); //column size is 5+ncol
		//FHDI --------returns----------------- FHDI //
		//IN   : rbind_FHDI  rbind_ipmat_FHDI(4+ncol); //column size is 4+ncol
		//IN   : rbind_FHDI  rbind_Resp_FHDI(ncol+1);  //separate response matrix  
		//IN   : rbind_FHDI  rbind_irmat_FHDI(5+ncol); //column size is 5+ncol
		//other matrices
		//IN   : rbind_FHDI  rbind_uox(ncol); //observed unique categorized matrix 
		//IN   : rbind_FHDI  rbind_mox(ncol); //missing  unique categorized matrix
		//Note: below Lists contain meaningful items up to i_count_mox rows  
		//IN   : List_FHDI 	List_ord(nrow); //order records used for variance estimation
		//IN   : List_FHDI 	List_ocsg(nrow); //order records used for variance estimation
		//IN   : std::string s_M = "FEFI" = fully efficient fractional imputation
		//						   "FHDI" = fractional hot deck imputation
		//IN   : int i_option_collapsing = choice of big-p algorithm 
		//                              0= no big-p algorithms
		//                             !0= perform big-p algorithms
		//IN   : int codes(nrow, i_option_collapsing); // storage to record most correlated variables of mox
		//OUT  : double** wmat = New_dMatrix(nrow_dat2_FHDI, L=nrow); //nrow_dat2_FHDI = rows of w1
		//
		//    
		//Data Structure Note
		//----------------------
		//dat1 in R version:  id   w  y_matrix  z_matrix
		//     in C++ ver  :  id   w  y         z
		//----------------------
		//dat2 in R version:  id  FID  WGT  FWGT  imputed matrix     Response(0/1)
		//     in C++ ver  :  ----- rbind_ipmat_FEFI------------     rbind_Resp_FEFI 
		//                 :  ----- rbind_ipmat_FHDI------------     rbind_Resp_FHDI
		//----------------------
		//
		//ipmat  = final imputation results
		//     	col1: ID 	= unit index
		//		col2: FID 	= ID of fractionally imputed value
		// 		col3: WGT 	= weight 
		//		col4: FWGT	= Frational weight
		//		col5: Variables 
		//		col6: Responses (separately in  rbind_Resp_...)
		//------------------------
		//
		//irmat  = imputation results related to the categorized matrix 
		//     	col1: ID 	= unit index
		//		col2: FID 	= ID of fractionally imputed value
		//		col3: OID	= original rank of the imputed value
		//		col4: ORDER = SN(selected donor)
		//		col5: FEFIW	= Fefi weights 
		//		col6: CELL	= cells 
		//----------------------
	{
		//testout
		//RPrint("=========== Begin Variance Estimation of FHDI ================");

		//below is defined by User 
		/*	//-------------
		//sample weight (default is 1)
		//id array (default is row number)
		//-------------
		double* w = new double[nrow];
		int* id   = new int[nrow];
		for(int i=0; i<nrow; i++)
		{
		w[i] = 1.0;
		id[i] = i+1; //ACTUAL id
		}
		*/

		//----------------------------
		//Basic constants declaration
		//----------------------------
		/*
		const int n  = nrow;
		const int nr = nrow;
		const int nc = ncol;
		const int nr1 = rbind_uox.size_row();
		const int nr2 = rbind_mox.size_row();
		const int nrow_uox 		 = rbind_uox.size_row();
		*/
		const int nrow_dat2_FEFI = rbind_ipmat_FEFI.size_row();
		const int nrow_dat2_FHDI = rbind_ipmat_FHDI.size_row();
		const int nrow_mox = rbind_mox.size_row();
		const int L = nrow; //size of d_rw 

							//--------------------
							//get ready id table of FEFI
							//--------------------
		double* d_id_FHDI = new double[nrow_dat2_FHDI];
		for (int i = 0; i<nrow_dat2_FHDI; i++) d_id_FHDI[i] = rbind_ipmat_FHDI(i, 0); //id, 1st col 
		std::vector<double> v_table_name_id_FHDI; //same as "nimp" in R version
		std::vector<int>    v_table_count_id_FHDI;//same as "nimp" in R version 
		table_cpp(d_id_FHDI, nrow_dat2_FHDI, v_table_name_id_FHDI, v_table_count_id_FHDI);

		//---------------------
		//imputed real data matrix
		//---------------------
		int nrow_d_iy = nrow_dat2_FHDI; //default
		if (s_M == "FDFI") { nrow_d_iy = nrow_dat2_FEFI; }
		if (s_M == "FHDI") { nrow_d_iy = nrow_dat2_FHDI; }
		double** d_iy = New_dMatrix(nrow_d_iy, ncol); //imputed matrix of real values 
		for (int i = 0; i<ncol; i++)
		{
			for (int j = 0; j<nrow_d_iy; j++)
			{
				if (s_M == "FEFI")
				{
					d_iy[j][i] = rbind_ipmat_FEFI(j, 4 + i);
				} //col5~ncol contains imputed real values 
				if (s_M == "FHDI")
				{
					d_iy[j][i] = rbind_ipmat_FHDI(j, 4 + i);
				} //col5~ncol contains imputed real values 

			}
		}

		//----------------------
		//categorized matrix
		//----------------------
		double** d_cx = New_dMatrix(nrow, ncol);
		Copy_dMatrix(z, nrow, ncol, d_cx);

		//---------------------
		//ocg, observed donors for each missing pattern. 
		//---------------------
		//the same as List_ocsg[nrow_mox]
		//---------------------
		int* i_locg = new int[nrow_mox]; //length of donor rows for each missing pattern 
		for (int i = 0; i<nrow_mox; i++)
		{
			int i_temp = 0;
			List_ocsg.get_a_row_size(i, i_temp);
			i_locg[i] = i_temp; //meaning how many rows used as the donor for ith missing pattern 
		}

		//testout
		/*
		RPrint("==== in Variance_Est_Extension_cpp ========");
		RPrint("id: "); RPrint(id, n);
		RPrint("n : "); RPrint(n);
		RPrint("nr: "); RPrint(nr);
		RPrint("nc: "); RPrint(nc);
		RPrint("--------dat1: id (above)and w,  y and z ");
		RPrint("w: "); RPrint(w, n);
		RPrint("y: "); RPrint(y, nrow, ncol);
		RPrint("z: "); RPrint(z, nrow, ncol);
		RPrint("--------dat2: ipmat_FEFI     Resp_FEFI ------- ");
		rbind_ipmat_FEFI.print_rbind_FHDI();
		rbind_Resp_FEFI.print_rbind_FHDI();
		RPrint("--------dat2: ipmat_FHDI     Resp_FHDI ------- ");
		rbind_ipmat_FHDI.print_rbind_FHDI();
		rbind_Resp_FHDI.print_rbind_FHDI();
		RPrint("iy : (imputed real data)"); RPrint(d_iy, nrow_d_iy, ncol);
		RPrint("cx : (categorized matrix)"); RPrint(d_cx, nrow, ncol);
		RPrint("ocg: ");  List_ocsg.print_List_FHDI();
		RPrint("locg: "); RPrint(i_locg, nrow_mox);
		RPrint("nr1: "); RPrint(nr1);
		RPrint("nr2: "); RPrint(nr2);
		*/

		//------------------------
		//cell probability using replicate weight
		//------------------------
		List_FHDI         List_rst_prob(nrow); //only i_nc rows are meaningful
		List_string_FHDI  List_rst_name(nrow); //only i_nc rows are meaningful
		std::vector<std::string> s_ncx;

		bool b_success_Rep_CellP = Rep_CellP_Bigp(d_cx, nrow, ncol, i_option_collapsing, d_rw, id,
			List_rst_prob,
			List_rst_name,
			s_ncx, codes);
		//const int i_nc = (int)s_ncx.size(); //meaningful rows of List's 

		if (!b_success_Rep_CellP)
		{
			Rprintf("Error! Rep_CellP Failed! Change k, check data quality, further break down categorical variables, or so. It may help \n");

			return 0; //abnormal ending 								
		}

		//--------------------
		//put 1 into fully observed rows
		//--------------------
		double* d_rr0 = new double[nrow];
		for (int i = 0; i<nrow; i++)
		{
			double d_prod = 1.0;
			for (int j = 0; j<ncol; j++)
			{
				d_prod = d_prod*d_cx[i][j];
			}
			//-----
			//0: at least one missing;  1: all observed
			//-----
			d_rr0[i] = d_prod;
			if (fabs_FHDI(d_prod) >0.0) d_rr0[i] = 1;
		}

		//--------------
		//calculate w1 = sampling weight
		//--------------
		//std::string cn[nrow]; 
		std::string *cn = new std::string[nrow];
		Trans(d_cx, nrow, ncol, cn);
		double* d_w1 = new double[nrow_dat2_FHDI];
		for (int i = 0; i<nrow_dat2_FHDI; i++)
			d_w1[i] = rbind_ipmat_FHDI(i, 2); //3rd column contains WGT

											  //testout
											  //RPrint("rr0: "); RPrint(d_rr0, nrow);
											  //RPrint("w1: "); RPrint(d_w1, nrow_dat2_FHDI);

											  //------------------
											  //make Covariance matrix for the subsequent replication process
											  //------------------
		int* i_lloc;
		std::vector<int> v_mox_0;
		double** d_dy;
		double** V_var; //covariance matrix of dy
		List_FHDI List_V(nrow_mox); //storage of covariance matrix
									//Note: store cov mat by "row-first" rule 
		for (int i = 0; i<nrow_mox; i++)
		{
			//---------------------------
			//how many donor rows for the ith missing pattern
			//------------------------
			int i_size_lloc = i_locg[i];
			//-----------
			//when zero size continue to next iteration
			//-----------
			if (i_size_lloc <= 0) { continue; }
			i_lloc = new int[i_size_lloc]; //vector of actual donor row numbers 
			for (int j = 0; j< i_size_lloc; j++) i_lloc[j] = (int)List_ocsg(i, j); //ith row, jth entity

																				   //-----
																				   //find missing columns in current missing row
																				   //------
			v_mox_0.clear();
			for (int j = 0; j<ncol; j++)
			{
				if (rbind_mox(i, j) == 0.0) v_mox_0.push_back(j + 1); //ACTUAL zero column id 
			}
			const int i_size_v_mox_0 = (int)v_mox_0.size();

			//----------------------------------
			//extract matrix of missing patterns
			//----------------------------------
			//-----------
			//when zero size continue to next iteration
			//-----------
			if (i_size_lloc <= 0) { continue; }
			if (i_size_v_mox_0 <= 0) { continue; }
			d_dy = New_dMatrix(i_size_lloc, i_size_v_mox_0);
			V_var = New_dMatrix(i_size_v_mox_0, i_size_v_mox_0); //column-wise covariance 
			for (int j = 0; j< i_size_lloc; j++) //LOOP for donor rows 
			{
				for (int k = 0; k<i_size_v_mox_0; k++) //LOOP for missing columns 
				{
					d_dy[j][k] = y[i_lloc[j] - 1][v_mox_0[k] - 1]; //-1 for actual location 
				}
			}
			//----------
			//"Estimated covariance" of d_dy by column-to-column method
			//----------
			cov_FHDI(d_dy, i_size_lloc, i_size_v_mox_0, V_var);


			//----------
			//store the covariance matrix 
			//row-first rule
			//----------
			//const int i_List_V_col = i_size_v_mox_0*i_size_v_mox_0;  
			//double* d_V_temp = new double[i_List_V_col];
			//for(int j=0; j<i_size_v_mox_0; j++) 
			//{
			//	for(int k=0; k<i_size_v_mox_0; k++)
			//		d_V_temp[j*i_size_v_mox_0 + k]= V_var[k][j]; 
			//}

			//List_V.put_block(i, i_List_V_col, d_V_temp); //ith covariance matrix 
			List_V.put_block(i, i_size_v_mox_0, i_size_v_mox_0, V_var); //direct matrix saving

																		//---
																		//local deallocation
																		//---
			delete[] i_lloc;
			Del_dMatrix(d_dy, i_size_lloc, i_size_v_mox_0);
			Del_dMatrix(V_var, i_size_v_mox_0, i_size_v_mox_0);
			//delete[] d_V_temp; 

		}

		//testout
		//RPrint("  List_V");
		//List_V.print_List_FHDI(); 

		//----------------
		//wmat: Replication Weights
		//----------------
		//double** wmat = New_dMatrix(nrow_dat2_FHDI, L); //nrow_dat2_FHDI = rows of w1

		//------------------------------
		//------------------------------
		//MAIn loop for L replications
		//------------------------------
		//------------------------------
		double* rw0 = new double[nrow];

		int i_sum_Rw = 0;
		for (int i = 0; i<nrow; i++) i_sum_Rw += v_table_count_id_FHDI[i];
		double* Rw = new double[i_sum_Rw];

		double* wijk = new double[nrow_dat2_FHDI]; //FWGT from ipmat 

		for (int l = 0; l<L; l++)
		{
			//-------
			//replicate weight from lth column
			//-------
			for (int i = 0; i<nrow; i++) rw0[i] = d_rw(i, l);   //previous:  d_rw[i][l]; //l_th column 
			int i_sum = 0;
			for (int i = 0; i<nrow; i++)
			{
				for (int j = 0; j<v_table_count_id_FHDI[i]; j++) Rw[i_sum++] = rw0[i];
			}

			//---------
			//FWGT of ipmat
			//----------
			for (int i = 0; i<nrow_dat2_FHDI; i++)
				wijk[i] = rbind_ipmat_FHDI(i, 3); //4th column is FWGT 

												  //----------
												  //joint probability associated with current string
												  //-----------
			std::string cn_current = cn[l]; //lth string 
			std::vector<int> v_ncx_cn;
			which(s_ncx, cn_current, v_ncx_cn); //actual location 
												//const int i_size_v_ncx_cn = (int)v_ncx_cn.size(); //MUST BE "1"

			int i_size_cellp = 0;
			List_rst_prob.get_a_row_size(v_ncx_cn[0] - 1, i_size_cellp); //get a size of the row in the list 
																		 //-----------
																		 //when zero size continue to next iteration
																		 //-----------
			if (i_size_cellp <= 0) { continue; }
			double* d_cellp = new double[i_size_cellp];
			List_rst_prob.get_block(v_ncx_cn[0] - 1, d_cellp); //-1 for actual row location 



															   //----------------------------------------
															   //1. if the deleted is missing unit, no further action is taken
															   //2. if the deleted is observed unit, then the fractional weights are re-computed 
															   //----------------------------------------

			int* idd = new int[nrow_mox]; //location of the deleted donor in ocg 
			Fill_iVector(idd, nrow_mox, 0);
			if (fabs_FHDI(d_rr0[l]) > 0)
			{
				//---------------------
				//locations of the deleted unit in observed list
				//---------------------
				std::vector<int> v_lg; //Actual locations 
				v_lg.clear();
				for (int j = 0; j<nrow_mox; j++) //all missing patterns
				{
					for (int k = 0; k<i_locg[j]; k++) //donor rows for the jth missing pattern
					{
						int i_temp_lg = (int)List_ocsg(j, k);
						if (i_temp_lg == (l + 1)) //+1 for actual location  
						{
							v_lg.push_back(j + 1); //actual id of jth missing pattern  
							idd[j] = k + 1; //actual number of donor rows for jth missing pattern 
							break;
						}
					}
				}
				const int nlg = (int)v_lg.size();

				//testout
				//RPrint(" in condition rr0[l]!=0 at l+1 ="); RPrint(l+1);
				//RPrint("idd:"); RPrint(idd, nrow_mox);
				//RPrint("lg :"); RPrint(v_lg);
				//RPrint("nlg:"); RPrint(nlg); 

				//--------------------------
				//Adjust fractional weights for all units in lg
				//--------------------------
				if (nlg>0)
				{
					for (int j = 0; j<nlg; j++)
					{
						int i_row_lg = v_lg[j] - 1; // row number [0,...) 
						double* d_1_mox = new double[ncol];
						for (int k = 0; k<ncol; k++) d_1_mox[k] = rbind_mox(i_row_lg, k);

						//---
						//actual col number of missing cell in current missing row
						//---
						std::vector<int> v_rloc; v_rloc.clear();
						for (int k = 0; k<ncol; k++)
						{
							if (d_1_mox[k] == 0.0) { v_rloc.push_back(k + 1); } //actual col
						}

						//---
						//number of missing columns in current missing row
						//---
						const int nrloc = (int)v_rloc.size();
						std::string cng;
						Trans1(d_1_mox, ncol, cng);

						//-------
						//location of cn which has cng
						//-------
						std::vector<int> v_mlog; v_mlog.clear();
						which(cn, nrow, cng, v_mlog);
						const int nmlog = (int)v_mlog.size();

						//testout
						//RPrint("rloc: "); RPrint(v_rloc);
						//RPrint("mlog: "); RPrint(v_mlog);


						//------------------------
						//------------------------
						//FHDI
						//------------------------
						//------------------------
						std::vector<int> v_elog; v_elog.clear();
						if (s_M.compare("FHDI") == 0) //0=equal 
						{
							//-----
							//find locations of mlog in dat2$ID
							//v_mlog contains the row numbers that have the same string as
							//current missing row 
							//nmlog = n(v_mlog)
							//-----
							v_elog.clear();
							for (int k1 = 0; k1<nmlog; k1++) //loop for mlog
							{
								int i_temp1 = id[v_mlog[k1] - 1]; //dat1$ID in R version  
								for (int k2 = 0; k2<nrow_dat2_FHDI; k2++)
								{
									int i_temp2 = rbind_ipmat_FHDI(k2, 0); //1st col is dat2$ID
									if (i_temp1 == i_temp2)
									{
										v_elog.push_back(k2 + 1); //actual location 
									}
								}
							}
							const int i_size_v_elog = (int)v_elog.size();

							//------------------------------------
							//set of donors for missing columns
							//------------------------------------
							//-----------
							//when zero size continue to next iteration
							//-----------
							if (i_size_v_elog <= 0) { continue; }
							if (nrloc <= 0) { continue; }
							double** dy_FHDI = New_dMatrix(i_size_v_elog, nrloc);
							for (int k1 = 0; k1<nrloc; k1++)
							{
								for (int k2 = 0; k2<i_size_v_elog; k2++)
								{
									dy_FHDI[k2][k1] = d_iy[v_elog[k2] - 1][v_rloc[k1] - 1];
								}
							}
							//testout
							//RPrint(" = FHDI l+1: "); RPrint(l+1); 
							//RPrint(" elog: "); RPrint(v_elog); 
							//RPrint(" dy  : "); RPrint(dy_FHDI,i_size_v_elog, nrloc); 

							//---------------------
							// nrloc >= 1: number of missing columns in current missing row
							//---------------------
							if (nrloc >= 1)
							{

								//----
								//make dk matrix
								//filled with l_th original data 
								//at missing column locations  
								//Note: this is the Jackknifed row
								//----------------------------------------
								//-----------
								//when zero size continue to next iteration
								//-----------
								if (i_size_v_elog <= 0) { continue; }
								if (nrloc <= 0) { continue; }
								double** dk = New_dMatrix(i_size_v_elog, nrloc);
								for (int k1 = 0; k1<nrloc; k1++)
								{
									double d_temp_dk = y[l][v_rloc[k1] - 1]; //-1 for actual location  
									for (int k2 = 0; k2<i_size_v_elog; k2++)
									{
										dk[k2][k1] = d_temp_dk;
									}
								}
								//testout
								//RPrint("i_size_v_elog:"); RPrint(i_size_v_elog); 
								//RPrint("nrloc        :"); RPrint(nrloc); 
								//RPrint("dk:"); RPrint(dk, i_size_v_elog, nrloc); 

								//---------
								//difference between   dy         and dk 
								//i.e., difference b/w donor rows and jackknifed row  
								//-------------------------------------------
								//-----------
								//when zero size continue to next iteration
								//-----------
								if (i_size_v_elog <= 0) { continue; }
								if (nrloc <= 0) { continue; }
								double** diff = New_dMatrix(i_size_v_elog, nrloc);
								for (int k1 = 0; k1<nrloc; k1++)
								{
									for (int k2 = 0; k2<i_size_v_elog; k2++)
									{
										diff[k2][k1] = dk[k2][k1] - dy_FHDI[k2][k1];
									}
								}


								//----------
								//get l_th covariance matrix
								//--------------------------------------------
								//-----------
								//when zero size continue to next iteration
								//-----------
								if (nrloc <= 0) { continue; }
								double** V_var_l = New_dMatrix(nrloc, nrloc);
								int i_loc_lg_j = v_lg[j] - 1; //-1 for actual location 
								List_V.get_block(i_loc_lg_j, nrloc, nrloc, V_var_l); //matrix read by row-first rule

																					 //testout
																					 //RPrint(" = FHDI l+1: "); RPrint(l+1); 
																					 //RPrint(" nrloc: ");   RPrint(nrloc); 
																					 //RPrint(" V_var_l: "); RPrint(V_var_l, nrloc, nrloc); 


																					 //-------------------------------------
																					 // diff * (V)^-1 *diff^T
																					 //-------------------------------------
																					 //-----------
																					 //when zero size continue to next iteration
																					 //-----------
								if (nrloc <= 0) { continue; }
								double** V_inv = New_dMatrix(nrloc, nrloc);

								bool b_success_V_inv = true; //false when abrupt exit due to zero diagonal
								if (nrloc > 1) b_success_V_inv = Inverse_dMatrix_FHDI(V_var_l, nrloc, V_inv);
								if (nrloc == 1) V_inv[0][0] = 1.0 / V_var_l[0][0];
								if (!b_success_V_inv)
								{
									//testout
									/*cout<<"nrloc: "<<nrloc<<endl;
									cout<<"l: "<<l<<",  L :"<<j<<endl;
									cout<<"j: "<<j<<",  nlg :"<<nlg<<endl;
									cout<<"V_var_l[][]"<<endl;
									RPrint(V_var_l, nrloc, nrloc);
									for(int i_V_var = 0; i_V_var<nrloc; i_V_var++)
									{
									cout<<i_V_var<<":  "<<V_var_l[i_V_var][i_V_var]<<endl;
									}*/

									//----
									//below is an option to abort 
									//zero-diagonal Variance matrix
									//However, if donor vector was zero
									//vector, exceptional consideration
									//is needed
									//Feb 07, 2017
									//----
									//cout<<endl<<"Error! zero diagonal term in V_var_l"<<endl; return; 

									//----
									//special remedy for zero diagonal covariance matrix
									//Feb 7 2017
									//This is enough since we are interested in 
									//relative ordering to find minimum distance later
									//----
									for (int i_inv1 = 0; i_inv1<nrloc; i_inv1++)
									{
										double d_temp_inv = V_var_l[i_inv1][i_inv1];

										//for zero diagonal term: a big number 
										if (fabs(d_temp_inv) <= 1e-13)
										{
											V_inv[i_inv1][i_inv1] = 1E15;
										}

										//for non-zero diagonal term: simple inverse
										if (fabs(d_temp_inv) > 1e-13)
										{
											V_inv[i_inv1][i_inv1] = 1.0 / d_temp_inv;
										}
									}
								}
								//-----------
								//when zero size continue to next iteration
								//-----------
								if (nrloc <= 0) { continue; }
								if (i_size_v_elog <= 0) { continue; }
								double** diff_T = New_dMatrix(nrloc, i_size_v_elog);
								for (int k1 = 0; k1<nrloc; k1++)
								{
									for (int k2 = 0; k2<i_size_v_elog; k2++)
									{
										diff_T[k1][k2] = diff[k2][k1];
									}
								}
								//-----------
								//when zero size continue to next iteration
								//-----------
								if (i_size_v_elog <= 0) { continue; }
								double** diff_V_diffT = New_dMatrix(i_size_v_elog, i_size_v_elog);
								dMatrix_Mul_AtBA(diff_T, nrloc, i_size_v_elog,
									V_inv, diff_V_diffT);

								//-----------
								//score = diagonal terms
								//-----------
								double* d_score = new double[i_size_v_elog];
								for (int k1 = 0; k1<i_size_v_elog; k1++)
									d_score[k1] = diff_V_diffT[k1][k1];


								//testout
								//RPrint(" = FHDI l+1: "); RPrint(l+1); 
								//RPrint("score        :"); RPrint(d_score, i_size_v_elog); 

								//------
								//MM calculation
								//------
								if (nmlog == 0)

								{
									Rprintf("Caution! zero mlog!"); continue;
								}

								const int MM = i_size_v_elog / nmlog;
								const int ncol_imt = (int)floor(i_size_v_elog / MM);
								//-----------
								//when zero size continue to next iteration
								//-----------
								if (ncol_imt <= 0) { continue; }
								if (MM <= 0) { continue; }
								double** d_imt = New_dMatrix(MM, ncol_imt);
								int i_score = 0;
								for (int k1 = 0; k1<ncol_imt; k1++)
								{
									for (int k2 = 0; k2<MM; k2++) //row-first rule 
									{
										d_imt[k2][k1] = d_score[i_score++];
									}
								}

								//-----------------------------------------
								//extract weights
								//-----------------------------------------
								//-----------
								//when zero size continue to next iteration
								//-----------
								if (i_size_v_elog <= 0) { continue; }
								double* ewijk = new double[i_size_v_elog];
								double* fefiw = new double[i_size_v_elog];

								for (int k1 = 0; k1<i_size_v_elog; k1++)
								{
									ewijk[k1] = wijk[v_elog[k1] - 1]; //-1 for actual loc
																	  //5th column is FEFIW of fhdi[[2]] in r version 
									fefiw[k1] = rbind_irmat_FHDI(v_elog[k1] - 1, 4); //5th column
								}

								//testout
								//RPrint("MM :"); RPrint(MM);
								//RPrint("ewijk :"); RPrint(ewijk, i_size_v_elog);
								//RPrint("fefiw :"); RPrint(fefiw, i_size_v_elog);
								//RPrint("d_imt :"); RPrint(d_imt, MM, ncol_imt);

								//-------------------
								//find eloc
								//-------------------
								std::vector<int> LMM; LMM.clear();
								for (int k1 = 0; k1<nmlog; k1++) LMM.push_back(k1*MM);
								//-----------
								//when zero size continue to next iteration
								//-----------
								if (nmlog <= 0) { continue; }
								int* i_eloc = new int[nmlog];
								//column-wise min location (Actual)
								for (int k1 = 0; k1<nmlog; k1++)
								{
									double d_temp = d_imt[0][k1]; //1st row  
									int    i_min = 1;
									for (int k2 = 1; k2<MM; k2++)
									{
										if ((d_temp - d_imt[k2][k1]) > 1e-3)
										{
											d_temp = d_imt[k2][k1];
											i_min = k2 + 1; //actual row location 
										}
									}
									//----
									//store the column-wise min location (Actual)
									//----
									i_eloc[k1] = i_min + LMM[k1];
									//testout
									//RPrint("LMM[k1] :"); RPrint(LMM[k1]);
									//RPrint("i_eloc[k1] :"); RPrint(i_eloc[k1]);
									//RPrint("i_min :"); RPrint(i_min);

								}

								//-------------
								//maximum difference (>0) between ewijk and fefiw
								//-------------------------------------
								//-----------
								//when zero size continue to next iteration
								//-----------
								if (nmlog <= 0) { continue; }
								double* d_maxew = new double[nmlog]; //nmlog = length of i_eloc
								double* d_maxval = new double[nmlog]; //nmlog = length of i_eloc
								for (int k1 = 0; k1<nmlog; k1++)
								{
									double d_temp = ewijk[i_eloc[k1] - 1] - fefiw[i_eloc[k1] - 1]; //-1 for actual loc
									d_maxew[k1] = 0.0;
									if (d_temp > 0.0) d_maxew[k1] = d_temp;

									d_maxval[k1] = ewijk[i_eloc[k1] - 1] - d_maxew[k1];
								}
								//testout
								//RPrint("d_maxew :"); RPrint(d_maxew, nmlog);
								//RPrint("d_maxval :"); RPrint(d_maxval, nmlog);

								//------
								//ewijk update
								//------
								for (int k1 = 0; k1<nmlog; k1++)
								{
									ewijk[i_eloc[k1] - 1] = d_maxew[k1];
								}

								//-------------
								//extend maxval array
								//-------------
								if (MM == 1) { Rprintf("Error! MM is 1 in Var FHDI \n"); return 0; }
								//-----------
								//when zero size continue to next iteration
								//-----------
								if (nmlog <= 0) { continue; }
								double* d_maxval_extended = new double[nmlog*(MM - 1)];
								int i_maxval = 0;
								for (int k1 = 0; k1<nmlog; k1++)
								{
									double d_temp = d_maxval[k1] / (MM - 1);
									for (int k2 = 0; k2<(MM - 1); k2++)
									{
										d_maxval_extended[i_maxval++] = d_temp;
									}
								}

								//---------------
								//update ewijk with extended maxval
								//---------------
								//exclude eloc locations
								//-----------
								//when zero size continue to next iteration
								//-----------
								if ((i_size_v_elog - nmlog) <= 0) { continue; }
								int* i_without_eloc = new int[i_size_v_elog - nmlog];
								int i_eloc_temp = 0;
								for (int k1 = 0; k1<i_size_v_elog; k1++)
								{
									bool b_same = false;
									for (int k2 = 0; k2<nmlog; k2++)
									{
										if (i_eloc[k2] - 1 == k1)
										{
											b_same = true;
											break;
										}
									}
									if (!b_same) { i_without_eloc[i_eloc_temp++] = k1; }
								}
								//store values
								for (int k1 = 0; k1<(i_size_v_elog - nmlog); k1++)
								{
									int i_loc_ew = i_without_eloc[k1];
									ewijk[i_loc_ew] = ewijk[i_loc_ew] + d_maxval_extended[k1];
								}
								//testout
								//RPrint("ewijk :"); RPrint(ewijk, i_size_v_elog);


								//----------------
								//final update wijk with ewijk
								//----------------
								for (int k1 = 0; k1<i_size_v_elog; k1++)
								{
									wijk[v_elog[k1] - 1] = ewijk[k1];

									//testout
									//RPrint("v_elog[k1]-1 :"); RPrint(v_elog[k1]-1);
									//RPrint("wijk[..] :"); RPrint(wijk[v_elog[k1]-1]);

								}


								//---
								//local deallocation
								//---
								Del_dMatrix(dk, i_size_v_elog, nrloc);
								Del_dMatrix(diff, i_size_v_elog, nrloc);
								Del_dMatrix(V_var_l, nrloc, nrloc);
								Del_dMatrix(V_inv, nrloc, nrloc);
								Del_dMatrix(diff_T, nrloc, i_size_v_elog);
								Del_dMatrix(diff_V_diffT, i_size_v_elog, i_size_v_elog);
								delete[] d_score;
								Del_dMatrix(d_imt, MM, ncol_imt);
								delete[] ewijk;
								delete[] fefiw;
								delete[] i_eloc;
								delete[] d_maxew;
								delete[] d_maxval;
								delete[] d_maxval_extended;
								delete[] i_without_eloc;

								//NOTE: dy_FHDI has different order from R version
								//as of Nov 27, 2016. It looks fine overall, but 
								//need to check later !!!!
							}


							//----------
							//local deallocation 
							//----------
							Del_dMatrix(dy_FHDI, i_size_v_elog, nrloc);
						}

						//local deallocation 
						delete[] d_1_mox;
					}
				}

			}


			//-------------------------------
			//store the updated weights
			//-------------------------------
			int i_nrow_imputation = nrow;
			if (s_M == "FEFI") i_nrow_imputation = nrow_dat2_FEFI;
			if (s_M == "FHDI") i_nrow_imputation = nrow_dat2_FHDI;

			for (int k1 = 0; k1<i_nrow_imputation; k1++)
			{
				wmat[k1][l] = Rw[k1] * wijk[k1];
			}

			//testout
			/*
			double* d_temp_wmat1 = new double[i_nrow_imputation];
			for(int j=0; j<i_nrow_imputation; j++) d_temp_wmat1[j] = wmat[j][l];
			RPrint("wmat[,l]:");
			RPrint(d_temp_wmat1, i_nrow_imputation);
			delete[] d_temp_wmat1;
			*/

			//--------------------
			//local deallocation
			//--------------------
			delete[] idd;
			delete[] d_cellp;



		} //end of main loop for L


		  //testout
		Rprintf(" ========= Variance estimation has successfully finished!\n");

		//-------------
		//deallocation
		//-------------
		//delete[] w; 
		//delete[] id;
		delete[] cn;
		delete[] d_id_FHDI;
		Del_dMatrix(d_iy, nrow_d_iy, ncol);
		Del_dMatrix(d_cx, nrow, ncol);
		delete[] i_locg;
		delete[] d_rr0;
		delete[] d_w1;
		//Del_dMatrix(wmat, nrow_dat2_FHDI, L);
		delete[] rw0;
		delete[] Rw;
		delete[] wijk;

		return 1;
	}

}//end of namespace


 //Fn===========================================================================

 //Variance_Est_FHDI_Neighbor_cpp.cc-----------------------------------------------------------------------------

 //Fn===========================================================================

namespace FHDI
{



	bool Variance_Est_FHDI_Neighbor_cpp(double** y, double** z, const int nrow, const int ncol,
		FHDI::RepWeight_FHDI &d_rw, double* w, int* id, List_FHDI &List_nU,
		rbind_FHDI  &rbind_ipmat_FEFI,
		rbind_FHDI  &rbind_Resp_FEFI,
		rbind_FHDI  &rbind_irmat_FEFI,
		rbind_FHDI  &rbind_ipmat_FHDI,
		rbind_FHDI  &rbind_Resp_FHDI,
		rbind_FHDI  &rbind_irmat_FHDI,
		rbind_FHDI  &rbind_uox,
		rbind_FHDI  &rbind_mox,
		List_FHDI 	&List_ord,
		List_FHDI 	&List_ocsg,
		std::string s_M,
		double** wmat)

		//Description----------------------
		//estimate variance for FHDI using Jackknife method 
		//  Algorithm: 
		//
		// original R code: Dr. Im, J. and Dr. Kim, J. 
		// c++ code: 		Dr. Cho, I. and Yicheng Yang
		// All rights reserved
		// 
		// updated: Aug 11, 2020
		//
		//IN   : double y(nrow, ncol)= original data matrix with missing cells 
		//IN   : double z(nrow, ncol)= categorized matrix of y. 0 for missing cell
		//IN   : double d_rw[nrow, nrow] = replicate weights 
		//IN   : double w(nrow) = sampling weight (default = 1.0)
		//IN   : int    id(nrow) = id number of each row (default = 1 to nrow)
		//FEFI --------returns----------------- FEFI //
		//IN   : rbind_FHDI  rbind_ipmat_FEFI(4+ncol); //column size is 4+ncol
		//IN   : rbind_FHDI  rbind_Resp_FEFI(ncol+1);  //separate response matrix  
		//IN   : rbind_FHDI  rbind_irmat_FEFI(5+ncol); //column size is 5+ncol
		//FHDI --------returns----------------- FHDI //
		//IN   : rbind_FHDI  rbind_ipmat_FHDI(4+ncol); //column size is 4+ncol
		//IN   : rbind_FHDI  rbind_Resp_FHDI(ncol+1);  //separate response matrix  
		//IN   : rbind_FHDI  rbind_irmat_FHDI(5+ncol); //column size is 5+ncol
		//other matrices
		//IN   : rbind_FHDI  rbind_uox(ncol); //observed unique categorized matrix 
		//IN   : rbind_FHDI  rbind_mox(ncol); //missing  unique categorized matrix
		//Note: below Lists contain meaningful items up to i_count_mox rows  
		//IN   : List_FHDI 	List_ord(nrow); //order records used for variance estimation
		//IN   : List_FHDI 	List_ocsg(nrow); //order records used for variance estimation
		//IN   : std::string s_M = "FEFI" = fully efficient fractional imputation
		//						   "FHDI" = fractional hot deck imputation
		//OUT  : double** wmat = New_dMatrix(nrow_dat2_FHDI, L=nrow); //nrow_dat2_FHDI = rows of w1
		//
		//    
		//Data Structure Note
		//----------------------
		//dat1 in R version:  id   w  y_matrix  z_matrix
		//     in C++ ver  :  id   w  y         z
		//----------------------
		//dat2 in R version:  id  FID  WGT  FWGT  imputed matrix     Response(0/1)
		//     in C++ ver  :  ----- rbind_ipmat_FEFI------------     rbind_Resp_FEFI 
		//                 :  ----- rbind_ipmat_FHDI------------     rbind_Resp_FHDI
		//----------------------
		//
		//ipmat  = final imputation results
		//     	col1: ID 	= unit index
		//		col2: FID 	= ID of fractionally imputed value
		// 		col3: WGT 	= weight 
		//		col4: FWGT	= Frational weight
		//		col5: Variables 
		//		col6: Responses (separately in  rbind_Resp_...)
		//------------------------
		//
		//irmat  = imputation results related to the categorized matrix 
		//     	col1: ID 	= unit index
		//		col2: FID 	= ID of fractionally imputed value
		//		col3: OID	= original rank of the imputed value
		//		col4: ORDER = SN(selected donor)
		//		col5: FEFIW	= Fefi weights 
		//		col6: CELL	= cells 
		//----------------------
	{
		//testout
		//RPrint("=========== Begin Variance Estimation of FHDI ================");

		//below is defined by User 
		/*	//-------------
		//sample weight (default is 1)
		//id array (default is row number)
		//-------------
		double* w = new double[nrow];
		int* id   = new int[nrow];
		for(int i=0; i<nrow; i++)
		{
		w[i] = 1.0;
		id[i] = i+1; //ACTUAL id
		}
		*/

		//----------------------------
		//Basic constants declaration
		//----------------------------
		/*
		const int n  = nrow;
		const int nr = nrow;
		const int nc = ncol;
		const int nr1 = rbind_uox.size_row();
		const int nr2 = rbind_mox.size_row();
		const int nrow_uox 		 = rbind_uox.size_row();
		*/
		const int nrow_dat2_FEFI = rbind_ipmat_FEFI.size_row();
		const int nrow_dat2_FHDI = rbind_ipmat_FHDI.size_row();
		const int nrow_mox = rbind_mox.size_row();
		const int L = nrow; //size of d_rw 

							//--------------------
							//get ready id table of FEFI
							//--------------------
		double* d_id_FHDI = new double[nrow_dat2_FHDI];
		for (int i = 0; i<nrow_dat2_FHDI; i++) d_id_FHDI[i] = rbind_ipmat_FHDI(i, 0); //id, 1st col 
		std::vector<double> v_table_name_id_FHDI; //same as "nimp" in R version
		std::vector<int>    v_table_count_id_FHDI;//same as "nimp" in R version 
		table_cpp(d_id_FHDI, nrow_dat2_FHDI, v_table_name_id_FHDI, v_table_count_id_FHDI);

		//---------------------
		//imputed real data matrix
		//---------------------
		int nrow_d_iy = nrow_dat2_FHDI; //default
		if (s_M == "FDFI") { nrow_d_iy = nrow_dat2_FEFI; }
		if (s_M == "FHDI") { nrow_d_iy = nrow_dat2_FHDI; }
		double** d_iy = New_dMatrix(nrow_d_iy, ncol); //imputed matrix of real values 
		for (int i = 0; i<ncol; i++)
		{
			for (int j = 0; j<nrow_d_iy; j++)
			{
				if (s_M == "FEFI")
				{
					d_iy[j][i] = rbind_ipmat_FEFI(j, 4 + i);
				} //col5~ncol contains imputed real values 
				if (s_M == "FHDI")
				{
					d_iy[j][i] = rbind_ipmat_FHDI(j, 4 + i);
				} //col5~ncol contains imputed real values 

			}
		}

		//----------------------
		//categorized matrix
		//----------------------
		double** d_cx = New_dMatrix(nrow, ncol);
		Copy_dMatrix(z, nrow, ncol, d_cx);

		//---------------------
		//ocg, observed donors for each missing pattern. 
		//---------------------
		//the same as List_ocsg[nrow_mox]
		//---------------------
		int* i_locg = new int[nrow_mox]; //length of donor rows for each missing pattern 
		for (int i = 0; i<nrow_mox; i++)
		{
			int i_temp = 0;
			List_ocsg.get_a_row_size(i, i_temp);
			i_locg[i] = i_temp; //meaning how many rows used as the donor for ith missing pattern 
		}

		//testout
		/*
		RPrint("==== in Variance_Est_Extension_cpp ========");
		RPrint("id: "); RPrint(id, n);
		RPrint("n : "); RPrint(n);
		RPrint("nr: "); RPrint(nr);
		RPrint("nc: "); RPrint(nc);
		RPrint("--------dat1: id (above)and w,  y and z ");
		RPrint("w: "); RPrint(w, n);
		RPrint("y: "); RPrint(y, nrow, ncol);
		RPrint("z: "); RPrint(z, nrow, ncol);
		RPrint("--------dat2: ipmat_FEFI     Resp_FEFI ------- ");
		rbind_ipmat_FEFI.print_rbind_FHDI();
		rbind_Resp_FEFI.print_rbind_FHDI();
		RPrint("--------dat2: ipmat_FHDI     Resp_FHDI ------- ");
		rbind_ipmat_FHDI.print_rbind_FHDI();
		rbind_Resp_FHDI.print_rbind_FHDI();
		RPrint("iy : (imputed real data)"); RPrint(d_iy, nrow_d_iy, ncol);
		RPrint("cx : (categorized matrix)"); RPrint(d_cx, nrow, ncol);
		RPrint("ocg: ");  List_ocsg.print_List_FHDI();
		RPrint("locg: "); RPrint(i_locg, nrow_mox);
		RPrint("nr1: "); RPrint(nr1);
		RPrint("nr2: "); RPrint(nr2);
		*/

		//------------------------
		//cell probability using replicate weight
		//------------------------
		List_FHDI         List_rst_prob(nrow); //only i_nc rows are meaningful
		List_string_FHDI  List_rst_name(nrow); //only i_nc rows are meaningful
		std::vector<std::string> s_ncx;

		bool b_success_Rep_CellP_KNN = Rep_CellP_Neighbor(d_cx, nrow, ncol, d_rw, id, List_nU,
			List_rst_prob,
			List_rst_name,
			s_ncx);

		if (!b_success_Rep_CellP_KNN)
		{
			Rprintf("Error! Rep_CellP Failed! Change k, check data quality, further break down categorical variables, or so. It may help \n");

			return 0; //abnormal ending 								
		}

		//--------------------
		//put 1 into fully observed rows
		//--------------------
		double* d_rr0 = new double[nrow];
		for (int i = 0; i<nrow; i++)
		{
			double d_prod = 1.0;
			for (int j = 0; j<ncol; j++)
			{
				d_prod = d_prod*d_cx[i][j];
			}
			//-----
			//0: at least one missing;  1: all observed
			//-----
			d_rr0[i] = d_prod;
			if (fabs(d_prod) >0.0) d_rr0[i] = 1;
		}

		//--------------
		//calculate w1 = sampling weight
		//--------------
		//std::string cn[nrow]; 
		std::string *cn = new std::string[nrow];
		Trans(d_cx, nrow, ncol, cn);
		double* d_w1 = new double[nrow_dat2_FHDI];
		for (int i = 0; i<nrow_dat2_FHDI; i++)
			d_w1[i] = rbind_ipmat_FHDI(i, 2); //3rd column contains WGT

											  //testout
											  //RPrint("rr0: "); RPrint(d_rr0, nrow);
											  //RPrint("w1: "); RPrint(d_w1, nrow_dat2_FHDI);

											  //------------------
											  //make Covariance matrix for the subsequent replication process
											  //------------------
		int* i_lloc;
		std::vector<int> v_mox_0;
		double** d_dy;
		double** V_var; //covariance matrix of dy
		List_FHDI List_V(nrow_mox); //storage of covariance matrix
									//Note: store cov mat by "row-first" rule 
		for (int i = 0; i<nrow_mox; i++)
		{
			//---------------------------
			//how many donor rows for the ith missing pattern
			//------------------------
			int i_size_lloc = i_locg[i];
			//-----------
			//when zero size continue to next iteration
			//-----------
			if (i_size_lloc <= 0) { continue; }
			i_lloc = new int[i_size_lloc]; //vector of actual donor row numbers 
			for (int j = 0; j< i_size_lloc; j++) i_lloc[j] = (int)List_ocsg(i, j); //ith row, jth entity

																				   //-----
																				   //find missing columns in current missing row
																				   //------
			v_mox_0.clear();
			for (int j = 0; j<ncol; j++)
			{
				if (rbind_mox(i, j) == 0.0) v_mox_0.push_back(j + 1); //ACTUAL zero column id 
			}
			const int i_size_v_mox_0 = (int)v_mox_0.size();

			//----------------------------------
			//extract matrix of missing patterns
			//----------------------------------
			//-----------
			//when zero size continue to next iteration
			//-----------
			if (i_size_lloc <= 0) { continue; }
			if (i_size_v_mox_0 <= 0) { continue; }
			d_dy = New_dMatrix(i_size_lloc, i_size_v_mox_0);
			V_var = New_dMatrix(i_size_v_mox_0, i_size_v_mox_0); //column-wise covariance 
			for (int j = 0; j< i_size_lloc; j++) //LOOP for donor rows 
			{
				for (int k = 0; k<i_size_v_mox_0; k++) //LOOP for missing columns 
				{
					d_dy[j][k] = y[i_lloc[j] - 1][v_mox_0[k] - 1]; //-1 for actual location 
				}
			}
			//----------
			//"Estimated covariance" of d_dy by column-to-column method
			//----------
			cov_FHDI(d_dy, i_size_lloc, i_size_v_mox_0, V_var);


			//----------
			//store the covariance matrix 
			//row-first rule
			//----------
			//const int i_List_V_col = i_size_v_mox_0*i_size_v_mox_0;  
			//double* d_V_temp = new double[i_List_V_col];
			//for(int j=0; j<i_size_v_mox_0; j++) 
			//{
			//	for(int k=0; k<i_size_v_mox_0; k++)
			//		d_V_temp[j*i_size_v_mox_0 + k]= V_var[k][j]; 
			//}

			//List_V.put_block(i, i_List_V_col, d_V_temp); //ith covariance matrix 
			List_V.put_block(i, i_size_v_mox_0, i_size_v_mox_0, V_var); //direct matrix saving

																		//---
																		//local deallocation
																		//---
			delete[] i_lloc;
			Del_dMatrix(d_dy, i_size_lloc, i_size_v_mox_0);
			Del_dMatrix(V_var, i_size_v_mox_0, i_size_v_mox_0);
			//delete[] d_V_temp; 

		}

		//testout
		//RPrint("  List_V");
		//List_V.print_List_FHDI(); 

		//----------------
		//wmat: Replication Weights
		//----------------
		//double** wmat = New_dMatrix(nrow_dat2_FHDI, L); //nrow_dat2_FHDI = rows of w1

		//------------------------------
		//------------------------------
		//MAIn loop for L replications
		//------------------------------
		//------------------------------
		double* rw0 = new double[nrow];

		int i_sum_Rw = 0;
		for (int i = 0; i<nrow; i++) i_sum_Rw += v_table_count_id_FHDI[i];
		double* Rw = new double[i_sum_Rw];

		double* wijk = new double[nrow_dat2_FHDI]; //FWGT from ipmat 

		for (int l = 0; l<L; l++)
		{
			//-------
			//replicate weight from lth column
			//-------
			for (int i = 0; i<nrow; i++) rw0[i] = d_rw(i, l); //l_th column 
			int i_sum = 0;
			for (int i = 0; i<nrow; i++)
			{
				for (int j = 0; j<v_table_count_id_FHDI[i]; j++) Rw[i_sum++] = rw0[i];
			}

			//---------
			//FWGT of ipmat
			//----------
			for (int i = 0; i<nrow_dat2_FHDI; i++)
				wijk[i] = rbind_ipmat_FHDI(i, 3); //4th column is FWGT 

												  //----------
												  //joint probability associated with current string
												  //-----------
			std::string cn_current = cn[l]; //lth string 
			std::vector<int> v_ncx_cn;
			which(s_ncx, cn_current, v_ncx_cn); //actual location 
												//const int i_size_v_ncx_cn = (int)v_ncx_cn.size(); //MUST BE "1"

			int i_size_cellp = 0;
			List_rst_prob.get_a_row_size(v_ncx_cn[0] - 1, i_size_cellp); //get a size of the row in the list 
																		 //-----------
																		 //when zero size continue to next iteration
																		 //-----------
			if (i_size_cellp <= 0) { continue; }
			double* d_cellp = new double[i_size_cellp];
			List_rst_prob.get_block(v_ncx_cn[0] - 1, d_cellp); //-1 for actual row location 

															   //testout
															   //RPrint(" ======== in Main Loop l+1: "); RPrint(l+1); 
															   //RPrint("Rw"); RPrint(Rw, i_sum_Rw); 
															   //RPrint("wijk"); RPrint(wijk, nrow_dat2_FHDI); 
															   //RPrint("d_cellp"); RPrint(d_cellp, i_size_cellp); 

															   //----------------------------------------
															   //1. if the deleted is missing unit, no further action is taken
															   //2. if the deleted is observed unit, then the fractional weights are re-computed 
															   //----------------------------------------
			int* idd = new int[nrow_mox]; //location of the deleted donor in ocg 
			Fill_iVector(idd, nrow_mox, 0);
			if (fabs(d_rr0[l]) > 0)
			{
				//---------------------
				//locations of the deleted unit in observed list
				//---------------------
				std::vector<int> v_lg; //Actual locations 
				v_lg.clear();
				for (int j = 0; j<nrow_mox; j++) //all missing patterns
				{
					for (int k = 0; k<i_locg[j]; k++) //donor rows for the jth missing pattern
					{
						int i_temp_lg = (int)List_ocsg(j, k);
						if (i_temp_lg == (l + 1)) //+1 for actual location  
						{
							v_lg.push_back(j + 1); //actual id of jth missing pattern  
							idd[j] = k + 1; //actual number of donor rows for jth missing pattern 
							break;
						}
					}
				}
				const int nlg = (int)v_lg.size();

				//testout
				//RPrint(" in condition rr0[l]!=0 at l+1 ="); RPrint(l+1);
				//RPrint("idd:"); RPrint(idd, nrow_mox);
				//RPrint("lg :"); RPrint(v_lg);
				//RPrint("nlg:"); RPrint(nlg); 

				//--------------------------
				//Adjust fractional weights for all units in lg
				//--------------------------
				if (nlg>0)
				{
					for (int j = 0; j<nlg; j++)
					{
						int i_row_lg = v_lg[j] - 1; // row number [0,...) 
						double* d_1_mox = new double[ncol];
						for (int k = 0; k<ncol; k++) d_1_mox[k] = rbind_mox(i_row_lg, k);

						//---
						//actual col number of missing cell in current missing row
						//---
						std::vector<int> v_rloc; v_rloc.clear();
						for (int k = 0; k<ncol; k++)
						{
							if (d_1_mox[k] == 0.0) { v_rloc.push_back(k + 1); } //actual col
						}

						//---
						//number of missing columns in current missing row
						//---
						const int nrloc = (int)v_rloc.size();
						std::string cng;
						Trans1(d_1_mox, ncol, cng);

						//-------
						//location of cn which has cng
						//-------
						std::vector<int> v_mlog; v_mlog.clear();
						which(cn, nrow, cng, v_mlog);
						const int nmlog = (int)v_mlog.size();

						//testout
						//RPrint("rloc: "); RPrint(v_rloc);
						//RPrint("mlog: "); RPrint(v_mlog);


						//------------------------
						//------------------------
						//FHDI
						//------------------------
						//------------------------
						std::vector<int> v_elog; v_elog.clear();
						if (s_M.compare("FHDI") == 0) //0=equal 
						{
							//-----
							//find locations of mlog in dat2$ID
							//v_mlog contains the row numbers that have the same string as
							//current missing row 
							//nmlog = n(v_mlog)
							//-----
							v_elog.clear();
							for (int k1 = 0; k1<nmlog; k1++) //loop for mlog
							{
								int i_temp1 = id[v_mlog[k1] - 1]; //dat1$ID in R version  
								for (int k2 = 0; k2<nrow_dat2_FHDI; k2++)
								{
									int i_temp2 = rbind_ipmat_FHDI(k2, 0); //1st col is dat2$ID
									if (i_temp1 == i_temp2)
									{
										v_elog.push_back(k2 + 1); //actual location 
									}
								}
							}
							const int i_size_v_elog = (int)v_elog.size();

							//------------------------------------
							//set of donors for missing columns
							//------------------------------------
							//-----------
							//when zero size continue to next iteration
							//-----------
							if (i_size_v_elog <= 0) { continue; }
							if (nrloc <= 0) { continue; }
							double** dy_FHDI = New_dMatrix(i_size_v_elog, nrloc);
							for (int k1 = 0; k1<nrloc; k1++)
							{
								for (int k2 = 0; k2<i_size_v_elog; k2++)
								{
									dy_FHDI[k2][k1] = d_iy[v_elog[k2] - 1][v_rloc[k1] - 1];
								}
							}
							//testout
							//RPrint(" = FHDI l+1: "); RPrint(l+1); 
							//RPrint(" elog: "); RPrint(v_elog); 
							//RPrint(" dy  : "); RPrint(dy_FHDI,i_size_v_elog, nrloc); 

							//---------------------
							// nrloc >= 1: number of missing columns in current missing row
							//---------------------
							if (nrloc >= 1)
							{

								//----
								//make dk matrix
								//filled with l_th original data 
								//at missing column locations  
								//Note: this is the Jackknifed row
								//----------------------------------------
								//-----------
								//when zero size continue to next iteration
								//-----------
								if (i_size_v_elog <= 0) { continue; }
								if (nrloc <= 0) { continue; }
								double** dk = New_dMatrix(i_size_v_elog, nrloc);
								for (int k1 = 0; k1<nrloc; k1++)
								{
									double d_temp_dk = y[l][v_rloc[k1] - 1]; //-1 for actual location  
									for (int k2 = 0; k2<i_size_v_elog; k2++)
									{
										dk[k2][k1] = d_temp_dk;
									}
								}
								//testout
								//RPrint("i_size_v_elog:"); RPrint(i_size_v_elog); 
								//RPrint("nrloc        :"); RPrint(nrloc); 
								//RPrint("dk:"); RPrint(dk, i_size_v_elog, nrloc); 

								//---------
								//difference between   dy         and dk 
								//i.e., difference b/w donor rows and jackknifed row  
								//-------------------------------------------
								//-----------
								//when zero size continue to next iteration
								//-----------
								if (i_size_v_elog <= 0) { continue; }
								if (nrloc <= 0) { continue; }
								double** diff = New_dMatrix(i_size_v_elog, nrloc);
								for (int k1 = 0; k1<nrloc; k1++)
								{
									for (int k2 = 0; k2<i_size_v_elog; k2++)
									{
										diff[k2][k1] = dk[k2][k1] - dy_FHDI[k2][k1];
									}
								}


								//----------
								//get l_th covariance matrix
								//--------------------------------------------
								//-----------
								//when zero size continue to next iteration
								//-----------
								if (nrloc <= 0) { continue; }
								double** V_var_l = New_dMatrix(nrloc, nrloc);
								int i_loc_lg_j = v_lg[j] - 1; //-1 for actual location 
								List_V.get_block(i_loc_lg_j, nrloc, nrloc, V_var_l); //matrix read by row-first rule

																					 //testout
																					 //RPrint(" = FHDI l+1: "); RPrint(l+1); 
																					 //RPrint(" nrloc: ");   RPrint(nrloc); 
																					 //RPrint(" V_var_l: "); RPrint(V_var_l, nrloc, nrloc); 


																					 //-------------------------------------
																					 // diff * (V)^-1 *diff^T
																					 //-------------------------------------
																					 //-----------
																					 //when zero size continue to next iteration
																					 //-----------
								if (nrloc <= 0) { continue; }
								double** V_inv = New_dMatrix(nrloc, nrloc);

								bool b_success_V_inv = true; //false when abrupt exit due to zero diagonal
								if (nrloc > 1) b_success_V_inv = Inverse_dMatrix_FHDI(V_var_l, nrloc, V_inv);
								if (nrloc == 1) V_inv[0][0] = 1.0 / V_var_l[0][0];
								if (!b_success_V_inv)
								{
									//testout
									/*cout<<"nrloc: "<<nrloc<<endl;
									cout<<"l: "<<l<<",  L :"<<j<<endl;
									cout<<"j: "<<j<<",  nlg :"<<nlg<<endl;
									cout<<"V_var_l[][]"<<endl;
									RPrint(V_var_l, nrloc, nrloc);
									for(int i_V_var = 0; i_V_var<nrloc; i_V_var++)
									{
									cout<<i_V_var<<":  "<<V_var_l[i_V_var][i_V_var]<<endl;
									}*/

									//----
									//below is an option to abort 
									//zero-diagonal Variance matrix
									//However, if donor vector was zero
									//vector, exceptional consideration
									//is needed
									//Feb 07, 2017
									//----
									//cout<<endl<<"Error! zero diagonal term in V_var_l"<<endl; return; 

									//----
									//special remedy for zero diagonal covariance matrix
									//Feb 7 2017
									//This is enough since we are interested in 
									//relative ordering to find minimum distance later
									//----
									for (int i_inv1 = 0; i_inv1<nrloc; i_inv1++)
									{
										double d_temp_inv = V_var_l[i_inv1][i_inv1];

										//for zero diagonal term: a big number 
										if (fabs(d_temp_inv) <= 1e-13)
										{
											V_inv[i_inv1][i_inv1] = 1E15;
										}

										//for non-zero diagonal term: simple inverse
										if (fabs(d_temp_inv) > 1e-13)
										{
											V_inv[i_inv1][i_inv1] = 1.0 / d_temp_inv;
										}
									}
								}
								//-----------
								//when zero size continue to next iteration
								//-----------
								if (nrloc <= 0) { continue; }
								if (i_size_v_elog <= 0) { continue; }
								double** diff_T = New_dMatrix(nrloc, i_size_v_elog);
								for (int k1 = 0; k1<nrloc; k1++)
								{
									for (int k2 = 0; k2<i_size_v_elog; k2++)
									{
										diff_T[k1][k2] = diff[k2][k1];
									}
								}
								//-----------
								//when zero size continue to next iteration
								//-----------
								if (i_size_v_elog <= 0) { continue; }
								double** diff_V_diffT = New_dMatrix(i_size_v_elog, i_size_v_elog);
								dMatrix_Mul_AtBA(diff_T, nrloc, i_size_v_elog,
									V_inv, diff_V_diffT);

								//-----------
								//score = diagonal terms
								//-----------
								double* d_score = new double[i_size_v_elog];
								for (int k1 = 0; k1<i_size_v_elog; k1++)
									d_score[k1] = diff_V_diffT[k1][k1];


								//testout
								//RPrint(" = FHDI l+1: "); RPrint(l+1); 
								//RPrint("score        :"); RPrint(d_score, i_size_v_elog); 

								//------
								//MM calculation
								//------
								if (nmlog == 0)
								{
									Rprintf("Caution! zero mlog!"); continue;
								}

								const int MM = i_size_v_elog / nmlog;
								const int ncol_imt = (int)floor(i_size_v_elog / MM);
								//-----------
								//when zero size continue to next iteration
								//-----------
								if (ncol_imt <= 0) { continue; }
								if (MM <= 0) { continue; }
								double** d_imt = New_dMatrix(MM, ncol_imt);
								int i_score = 0;
								for (int k1 = 0; k1<ncol_imt; k1++)
								{
									for (int k2 = 0; k2<MM; k2++) //row-first rule 
									{
										d_imt[k2][k1] = d_score[i_score++];
									}
								}

								//-----------------------------------------
								//extract weights
								//-----------------------------------------
								//-----------
								//when zero size continue to next iteration
								//-----------
								if (i_size_v_elog <= 0) { continue; }
								double* ewijk = new double[i_size_v_elog];
								double* fefiw = new double[i_size_v_elog];

								for (int k1 = 0; k1<i_size_v_elog; k1++)
								{
									ewijk[k1] = wijk[v_elog[k1] - 1]; //-1 for actual loc
																	  //5th column is FEFIW of fhdi[[2]] in r version 
									fefiw[k1] = rbind_irmat_FHDI(v_elog[k1] - 1, 4); //5th column
								}

								//testout
								//RPrint("MM :"); RPrint(MM);
								//RPrint("ewijk :"); RPrint(ewijk, i_size_v_elog);
								//RPrint("fefiw :"); RPrint(fefiw, i_size_v_elog);
								//RPrint("d_imt :"); RPrint(d_imt, MM, ncol_imt);

								//-------------------
								//find eloc
								//-------------------
								std::vector<int> LMM; LMM.clear();
								for (int k1 = 0; k1<nmlog; k1++) LMM.push_back(k1*MM);
								//-----------
								//when zero size continue to next iteration
								//-----------
								if (nmlog <= 0) { continue; }
								int* i_eloc = new int[nmlog];
								//column-wise min location (Actual)
								for (int k1 = 0; k1<nmlog; k1++)
								{
									double d_temp = d_imt[0][k1]; //1st row  
									int    i_min = 1;
									for (int k2 = 1; k2<MM; k2++)
									{
										if ((d_temp - d_imt[k2][k1]) > 1e-3)
										{
											d_temp = d_imt[k2][k1];
											i_min = k2 + 1; //actual row location 
										}
									}
									//----
									//store the column-wise min location (Actual)
									//----
									i_eloc[k1] = i_min + LMM[k1];
									//testout
									//RPrint("LMM[k1] :"); RPrint(LMM[k1]);
									//RPrint("i_eloc[k1] :"); RPrint(i_eloc[k1]);
									//RPrint("i_min :"); RPrint(i_min);

								}

								//-------------
								//maximum difference (>0) between ewijk and fefiw
								//-------------------------------------
								//-----------
								//when zero size continue to next iteration
								//-----------
								if (nmlog <= 0) { continue; }
								double* d_maxew = new double[nmlog]; //nmlog = length of i_eloc
								double* d_maxval = new double[nmlog]; //nmlog = length of i_eloc
								for (int k1 = 0; k1<nmlog; k1++)
								{
									double d_temp = ewijk[i_eloc[k1] - 1] - fefiw[i_eloc[k1] - 1]; //-1 for actual loc
									d_maxew[k1] = 0.0;
									if (d_temp > 0.0) d_maxew[k1] = d_temp;

									d_maxval[k1] = ewijk[i_eloc[k1] - 1] - d_maxew[k1];
								}
								//testout
								//RPrint("d_maxew :"); RPrint(d_maxew, nmlog);
								//RPrint("d_maxval :"); RPrint(d_maxval, nmlog);

								//------
								//ewijk update
								//------
								for (int k1 = 0; k1<nmlog; k1++)
								{
									ewijk[i_eloc[k1] - 1] = d_maxew[k1];
								}

								//-------------
								//extend maxval array
								//-------------
								if (MM == 1) { Rprintf("Error! MM is 1 in Var FHDI \n"); return 0; }
								//-----------
								//when zero size continue to next iteration
								//-----------
								if (nmlog <= 0) { continue; }
								double* d_maxval_extended = new double[nmlog*(MM - 1)];
								int i_maxval = 0;
								for (int k1 = 0; k1<nmlog; k1++)
								{
									double d_temp = d_maxval[k1] / (MM - 1);
									for (int k2 = 0; k2<(MM - 1); k2++)
									{
										d_maxval_extended[i_maxval++] = d_temp;
									}
								}

								//---------------
								//update ewijk with extended maxval
								//---------------
								//exclude eloc locations
								//-----------
								//when zero size continue to next iteration
								//-----------
								if ((i_size_v_elog - nmlog) <= 0) { continue; }
								int* i_without_eloc = new int[i_size_v_elog - nmlog];
								int i_eloc_temp = 0;
								for (int k1 = 0; k1<i_size_v_elog; k1++)
								{
									bool b_same = false;
									for (int k2 = 0; k2<nmlog; k2++)
									{
										if (i_eloc[k2] - 1 == k1)
										{
											b_same = true;
											break;
										}
									}
									if (!b_same) { i_without_eloc[i_eloc_temp++] = k1; }
								}
								//store values
								for (int k1 = 0; k1<(i_size_v_elog - nmlog); k1++)
								{
									int i_loc_ew = i_without_eloc[k1];
									ewijk[i_loc_ew] = ewijk[i_loc_ew] + d_maxval_extended[k1];
								}
								//testout
								//RPrint("ewijk :"); RPrint(ewijk, i_size_v_elog);


								//----------------
								//final update wijk with ewijk
								//----------------
								for (int k1 = 0; k1<i_size_v_elog; k1++)
								{
									wijk[v_elog[k1] - 1] = ewijk[k1];

									//testout
									//RPrint("v_elog[k1]-1 :"); RPrint(v_elog[k1]-1);
									//RPrint("wijk[..] :"); RPrint(wijk[v_elog[k1]-1]);

								}


								//---
								//local deallocation
								//---
								Del_dMatrix(dk, i_size_v_elog, nrloc);
								Del_dMatrix(diff, i_size_v_elog, nrloc);
								Del_dMatrix(V_var_l, nrloc, nrloc);
								Del_dMatrix(V_inv, nrloc, nrloc);
								Del_dMatrix(diff_T, nrloc, i_size_v_elog);
								Del_dMatrix(diff_V_diffT, i_size_v_elog, i_size_v_elog);
								delete[] d_score;
								Del_dMatrix(d_imt, MM, ncol_imt);
								delete[] ewijk;
								delete[] fefiw;
								delete[] i_eloc;
								delete[] d_maxew;
								delete[] d_maxval;
								delete[] d_maxval_extended;
								delete[] i_without_eloc;

								//NOTE: dy_FHDI has different order from R version
								//as of Nov 27, 2016. It looks fine overall, but 
								//need to check later !!!!
							}


							//----------
							//local deallocation 
							//----------
							Del_dMatrix(dy_FHDI, i_size_v_elog, nrloc);
						}

						//local deallocation 
						delete[] d_1_mox;
					}
				}

			}


			//-------------------------------
			//store the updated weights
			//-------------------------------
			int i_nrow_imputation = nrow;
			if (s_M == "FEFI") i_nrow_imputation = nrow_dat2_FEFI;
			if (s_M == "FHDI") i_nrow_imputation = nrow_dat2_FHDI;

			for (int k1 = 0; k1<i_nrow_imputation; k1++)
			{
				wmat[k1][l] = Rw[k1] * wijk[k1];
			}

			//testout
			/*
			double* d_temp_wmat1 = new double[i_nrow_imputation];
			for(int j=0; j<i_nrow_imputation; j++) d_temp_wmat1[j] = wmat[j][l];
			RPrint("wmat[,l]:");
			RPrint(d_temp_wmat1, i_nrow_imputation);
			delete[] d_temp_wmat1;
			*/

			//--------------------
			//local deallocation
			//--------------------
			delete[] idd;
			delete[] d_cellp;



		} //end of main loop for L


		  //testout
		Rprintf(" ========= Variance estimation KNN has successfully finished!\n");

		//-------------
		//deallocation
		//-------------
		//delete[] w; 
		//delete[] id;
		delete[] cn;
		delete[] d_id_FHDI;
		Del_dMatrix(d_iy, nrow_d_iy, ncol);
		Del_dMatrix(d_cx, nrow, ncol);
		delete[] i_locg;
		delete[] d_rr0;
		delete[] d_w1;
		//Del_dMatrix(wmat, nrow_dat2_FHDI, L);
		delete[] rw0;
		delete[] Rw;
		delete[] wijk;

		return 1;
	}

}//end of namespace






//Fn===========================================================================

//Rfn_test.cc-----------------------------------------------------------------------------

//Fn===========================================================================



bool Rfn_test(double* x, int* r, int* nrow_x, int* ncol_x, double* k_original, 

                   double* d_w, int* M_donor,

				   int*i_option_imputation, int* i_option_variance, 

				   int* id, double* z_UserDefined, 
				   
				   int* NonCollapsible_categorical, int* i_option_SIS, int* s_option_SIS, int* i_option_cellmake, int* top_corr_var,

				   rbind_FHDI &rbind_ipmat_FEFI,

				   rbind_FHDI &rbind_Resp_FEFI, 

				   rbind_FHDI &rbind_irmat_FEFI,

				   rbind_FHDI &rbind_ipmat_FHDI,

				   rbind_FHDI &rbind_Resp_FHDI, 

				   rbind_FHDI &rbind_irmat_FHDI,

				   rbind_FHDI &rbind_vrst_FEFI, 

				   rbind_FHDI &rbind_vrst_FHDI,



				   rbind_FHDI  &rbind_uox_CellMake,

				   rbind_FHDI  &rbind_mox_CellMake,	

				   rbind_FHDI  &rbind_category_CellMake,	

	               rbind_FHDI  &rbind_codes_CellMake,
				   

				   std::vector<std::string> &jp_name_return_CellProb,				   

				   std::vector<double> &jp_prob_return_CellProb,				

				   

				   const int i_option_perform, 

				   int* i_option_merge)

				   

//Rfn_test::Rfn_test(double* x, int* r, int* nrow_x, int* ncol_x, double* k, 

//                   double* d, int* M_donor) //before 2017 0112

//Description================================================================

// perform FEFI and FHDI based on the theory of Drs. Im, Kim and Fuller

//

// Cell_make

// Cell_Prob

//

// October 5, 2016

// R code written by Dr. Im, J. H. and Dr. Kim, J. G. 

// C++ code by Dr. I. Cho

// All rights reserved

//---

//IN	: double x(nrow_x, ncol_x) = {y1, y2, ...} raw data containing missing values

//        Note: as of Oct 5, 2016. missing value is marked by a long number not NA 

//				to avoid interface error between C++ and R

//        Note: as of Feb 10, 2017

//              Cell Prob Only (option =3) uses x as the categorized value matrix 

// 

//IN    : int    r(nrow_x, ncol_x) = index for missing (0) or observed (1)

//IN	: double k_original(ncol_x)	= number of total categories per column of x

//IN	: double d_w(nrow_x) 	= weight of row (default = 1.0)

//IN    : int    M_donor = number of donors for FHDI

//IN    : int    i_option_imputation = 1: FEFE; 2:FHDI

//IN    : int    i_option_variance  = 0: skip variance estimation; 1: perform var. est. 

//IN    : int    i_option_SIS = 0: perform big-n algorithms; !0: perform big-p algorithms

//IN    : int    s_option_SIS = 1: SIS with intersection; 2: SIS with union; 3: SIS with global ranking 

//IN    : int    i_option_cellmake = 1: cell make with merging; 2: cell make with KNN

//IN    : int    top_corr_var = number to get top rankings of variables of correlation

//IN    : int    id(nrow_x) = ID of raw data 

//IN    : double z_UserDefined(nrow_x, ncol_x) = user-defined category matrix (i_option_perform=4 only)

//OUT   : rbind_FHDI  rbind_ipmat_FEFI(4+ncol) //column size is 4+ncol (i.e., for R: ID, FID, WGT, FWGT, Variables)

//OUT   : rbind_FHDI  rbind_Resp_FEFI(ncol+1)  //separate response matrix  (i.e. for R: unit responses and Resp0)

//OUT   : rbind_FHDI  rbind_irmat_FEFI(5+ncol) //column size is 5+ncol (i.e. for R:ID, FID, OID, ORDER, FEFIW, CELL )

//OUT   : rbind_FHDI  rbind_ipmat_FHDI(4+ncol) //column size is 4+ncol (i.e., for R: ID, FID, WGT, FWGT, Variables)

//OUT   : rbind_FHDI  rbind_Resp_FHDI(ncol+1)  //separate response matrix  (i.e. for R: unit responses and Resp0)

//OUT   : rbind_FHDI  rbind_irmat_FHDI(5+ncol) //column size is 5+ncol (i.e. for R:ID, FID, OID, ORDER, FEFIW, CELL )

//OUT   : rbind_FHDI  rbind_vrst_FEFI(nrow)    //variance estimates of FEFI

//OUT   : rbind_FHDI  rbind_vrst_FHDI(nrow)    //variance estimates of FHDI

//

//OUT   : rbind_FHDI  rbind_uox_CellMake(ncol) //unique patterns of observed rows

//OUT   : rbind_FHDI  rbind_mox_CellMake(ncol) //unique patterns of missing rows

//OUT   : rbind_FHDI  rbind_category_CellMake(ncol) //matrix of categorized values

//

//OUT   : std::vector<double> &jp_prob_return_CellProb  //joint prob for Cell Prob option

//OUT   : std::vector<std::string> &jp_name_return_CellProb //name of jp for Cell Prob option 			

//

//IN    : int i_option_perform = main performance option

//                            1=perform entire FEFI/FHDI (cellmake, jp, impute, var)

//                            2=only Cell Make  

//                            3=cell prob only

//                            4=perform all FEFI/FHDI but by using User-Defined Z matrix 

//IN    : int i_option_merge = random donor selection in Merge algorithm in Cell Make

//                            0= no random seed number setting

//						      1= random seed number setting 

//============================================================================

{

	const int nrow = *nrow_x;

	const int ncol = *ncol_x; 

	const int i_M  = *M_donor; 

	const int i_imputation = *i_option_imputation;

	const int i_variance  = *i_option_variance;

	const int i_merge = *i_option_merge; 

	const int i_SIS = *i_option_SIS;

	const int s_SIS = *s_option_SIS;
	
	const int i_cellmake = *i_option_cellmake;

	const int top = *top_corr_var;

	//Rprintf("i_cellmake inside main function is \n");
	//Rprintf("%d", i_cellmake);

	//Rprintf("top inside main function is \n");
	//Rprintf("%d", top);

	//------
	//copy the original k vector since it may be updated for categorical variables
	//------
	double* k = new double[ncol]; 
	for(int i=0; i<ncol; i++) k[i] = k_original[i]; 


	//----------------------------

	//get matrix format of x -> x_raw containing all data and missing cells  

	//based on row-first rule of R 

	//----------------------------

	double** x_raw = New_dMatrix(nrow,ncol); //initialized by 0.0

	for(int i=0; i<ncol; i++){ for(int j=0; j<nrow; j++) x_raw[j][i] = x[j+i*nrow]; } 



	//-------------

	//Special Individual Running for 

	//Cell Prob ONLY!

	//-------------

	if(i_option_perform ==3) 

	{

		//z is already stored in x_raw in this special option 

		//double** z_CellProb = New_dMatrix(nrow, ncol); //initialized by 0.0



		//===================================

		//ONLY Cell_prob(): calculate the joint probability of cells

		//===================================

	

		bool b_success_CellProb = FHDI::Cell_Prob_Extension_cpp(x_raw, nrow, ncol, 

		                          jp_prob_return_CellProb, jp_name_return_CellProb, 

	                              d_w, id);

		if(!b_success_CellProb)
		{			
			Rprintf("Error! Cell Prob Failed! Change k, check data quality, further break down categorical variables, or so. It may help \n");
			
			return 0; //abnormal ending 								
		}


		//=========================

		//Deallocate memories

		//=========================

		Del_dMatrix(x_raw, nrow, ncol);

		

		return 1; 		

	}

	

	//----------------------------

	//get matrix format of r -> r_raw containing index for missing/observed

	//based on row-first rule of R 

	//----------------------------

	int** r_raw = New_iMatrix(nrow, ncol); //initialized by 0

	for(int i=0; i<ncol; i++){ for(int j=0; j<nrow; j++) r_raw[j][i] = r[j+i*nrow]; } 

	

	//=====================================

	//=====================================

	//Cell_Make task

	//=====================================

	//=====================================

	//Storages to be returned

	double** z = New_dMatrix(nrow, ncol); //initialized by 0.0

	

	//--------------------------------------------------

	//--------------------------------------------------

	//CELL MAKE-----------------------------------------

	//--------------------------------------------------

	//--------------------------------------------------
	//if i_option_perform = 4, user-defined z option, skip Cell_Make
	//April 4, 2017 =======================
	if( (i_option_perform != 4) && (i_cellmake ==1) && (i_SIS == 0))
	{
		bool b_success_CM = FHDI::Cell_Make_Extension_cpp(x_raw, nrow, ncol, k, 
									NonCollapsible_categorical,
									z, 
								  rbind_uox_CellMake, rbind_mox_CellMake, 
								  i_merge);   
		if(!b_success_CM) 
		{
			Rprintf("ERROR! Cell Make failed! ");
			Rprintf(" Change k, check data quality, further break down categorical variables, or so. It may help ");
			
			return 0; //abnormal ending 
		}
		
	}


	int** codes = New_iMatrix(nrow, i_SIS);

	if ((i_option_perform != 4) && (i_cellmake==1) && (i_SIS != 0)) // Written by Yicheng Yang
	{

		//---------------------------------------------------------------------
		bool b_success_CM_Bigp = FHDI::Cell_Make_Extension_Bigp_cpp(x_raw, r_raw, nrow, ncol, k,
			NonCollapsible_categorical,
			z, codes,
			rbind_uox_CellMake, rbind_mox_CellMake, 
			i_merge, i_SIS, s_SIS, top);


		if (!b_success_CM_Bigp)
		{
			Rprintf("ERROR! Cell Make with SIS failed! ");
			Rprintf(" Change k, check data quality, further break down categorical variables, or so. It may help ");

			return 0; //abnormal ending 
		}

	}

	List_FHDI List_nU(nrow); //default for the size of nrow, but will be updated in the main loop

	if ((i_option_perform != 4) && (i_cellmake == 2) && (i_SIS == 0)) // Written by Yicheng Yang
	{
		bool b_success_CM_KNN = FHDI::Cell_Make_Neighbor_cpp(x_raw, nrow, ncol, k,
			NonCollapsible_categorical,
			z,
			rbind_uox_CellMake, rbind_mox_CellMake, List_nU,
			i_merge);
	
		if (!b_success_CM_KNN)
		{
			Rprintf("ERROR! Cell Make KNN failed! ");
			Rprintf(" Change k, check data quality, further break down categorical variables, increase top_corr_var or so. It may help ");

			return 0; //abnormal ending 
		}

		//Rprintf("List_nU in cell make KNN is \n");
		//List_nU.print_List_FHDI();

	}

	if ((i_option_perform != 4) && (i_cellmake == 2) && (i_SIS != 0)) // Written by Yicheng Yang
	{
		bool b_success_CM_Bigp_KNN = FHDI::Cell_Make_Neighbor_Bigp_cpp(x_raw, r_raw, nrow, ncol, k,
			NonCollapsible_categorical,
			z, codes,
			rbind_uox_CellMake, rbind_mox_CellMake, List_nU,
			i_merge, i_SIS, s_SIS, top);

		if (!b_success_CM_Bigp_KNN)
		{
			Rprintf("ERROR! Cell Make KNN with SIS failed! ");
			Rprintf(" Change k, check data quality, further break down categorical variables, increase top_corr_var or so. It may help ");

			return 0; //abnormal ending 
		}

		//Rprintf("List_nU in cell make bigp KNN is \n");
		//List_nU.print_List_FHDI();
	}
	//--------------------------------

	//User-Defined z matrix: Override z of Cell Make 

	//--------------------------------

	if(i_option_perform == 4) //full FEFI/FHDI using user-defined z 

	{

		Rprintf("Note: Category z matrix is using the user-defined matrix");

		for(int i=0; i<ncol; i++){ for(int j=0; j<nrow; j++) z[j][i] = z_UserDefined[j+i*nrow]; }

	}		



	

	if(i_option_perform==2) //Cell Make only option; prep z to be out

	{

		double* d_temp_z = new double[ncol];

		for(int i=0; i<nrow; i++)

		{

			for(int j=0; j<ncol; j++) d_temp_z[j] = z[i][j]; 

			

			rbind_category_CellMake.append_block(d_temp_z); 

		}

		if (i_SIS != 0) {

			double* d_temp_codes = new double[i_SIS];

			int i_count_mox_temp = rbind_mox_CellMake.size_row();

			for (int i = 0; i < i_count_mox_temp; i++)

			{

				for (int j = 0; j < i_SIS; j++) d_temp_codes[j] = codes[i][j];



				rbind_codes_CellMake.append_block(d_temp_codes);

			}

			delete[] d_temp_codes;
		}


		//=========================

		//Deallocate memories

		//=========================

		delete[] d_temp_z; 

		Del_dMatrix(z, nrow, ncol);

		Del_dMatrix(x_raw, nrow, ncol);

		Del_iMatrix(r_raw, nrow, ncol);

		Del_iMatrix(codes, nrow, i_SIS);
		

		return 1; 

	}

	

	

	//===================================

	//===================================

	//Cell_prob(): calculate the joint probability of cells

	//===================================

	//===================================

	std::vector<std::string> jp_name_return;   //name of the joint probability table

	std::vector<double> jp_prob_return; //the latest joint probability 

	
	if ( (i_cellmake == 1) && (i_SIS == 0)) {

		bool b_success_CellProb = FHDI::Cell_Prob_Extension_cpp(z, nrow, ncol, jp_prob_return, jp_name_return,

			d_w, id);

		if (!b_success_CellProb)
		{
			Rprintf("Error! Cell Prob Failed! Change k, check data quality, further break down categorical variables, or so. It may help \n");

			return 0; //abnormal ending 								
		}

	}

	
	if ( (i_cellmake == 1) && (i_SIS != 0)) { // Written by Yicheng Yang

		bool b_success_CellProb_Bigp = FHDI::Cell_Prob_Extension_Bigp_cpp(z, nrow, ncol, i_SIS, jp_prob_return, jp_name_return,
			d_w, id, codes);

		if (!b_success_CellProb_Bigp)
		{
			Rprintf("Error! Cell Prob with SIS Failed! Change k, check data quality, further break down categorical variables, or so. It may help \n");

			return 0; //abnormal ending 								
		}

	}

	if (i_cellmake == 2) {

		bool b_success_CellProb_KNN = FHDI::Cell_Prob_Neighbor_cpp(z, nrow, ncol, List_nU, jp_prob_return, jp_name_return,
			d_w, id);

		if (!b_success_CellProb_KNN)
		{
			Rprintf("Error! Cell Prob KNN Failed! Change k, check data quality, further break down categorical variables, or so. It may help \n");

			return 0; //abnormal ending 								
		}
	}


	//=====================================

	//=====================================

	//FEFI: Fully Efficient Fractional Imputation

	//=====================================

	//Note: as of Nov 15, 2016. FHDI is following the FEFI without returning

	//       					this is computationally efficient

	//=====================================

	//FEFI---------returns----------------- FEFI //

	//get ipmat, Resp (separately), irmat from FEFI results 

	//rbind_FHDI  rbind_ipmat_FEFI(4+ncol); //column size is 4+ncol    //defined at Interface.cc 0117_2017

	//rbind_FHDI  rbind_Resp_FEFI(ncol+1);  //separate response matrix //defined at Interface.cc 0117_2017 

	//rbind_FHDI  rbind_irmat_FEFI(5+ncol); //column size is 5+ncol    //defined at Interface.cc 0117_2017

	//--------

	//FHDI --------returns----------------- FHDI //

	//--------

	//get ipmat, Resp (separately), irmat from FHDI results 

	//rbind_FHDI  rbind_ipmat_FHDI(4+ncol); //column size is 4+ncol	//defined at Interface.cc 0117_2017

	//rbind_FHDI  rbind_Resp_FHDI(ncol+1);  //separate response matrix//defined at Interface.cc 0117_2017  

	//rbind_FHDI  rbind_irmat_FHDI(5+ncol); //column size is 5+ncol	//defined at Interface.cc 0117_2017

	//---------

	//---------

	//other matrices

	//---------

	rbind_FHDI  rbind_uox(ncol); //observed unique categorized matrix 

	rbind_FHDI  rbind_mox(ncol); //missing  unique categorized matrix

	//Note: below Lists contain meaningful items up to i_count_mox rows  

	List_FHDI 	List_ord(nrow); //order records used for variance estimation

	List_FHDI 	List_ocsg(nrow); //order records used for variance estimation



	

	std::string s_M = "FEFI"; 

	if( (i_imputation == 1) && (i_cellmake == 1) && (i_SIS == 0 )) //FEFI

	{

		bool b_success_FEFI = FHDI::FHDI_Extension_cpp(x_raw, z, r_raw, 

						     nrow, ncol, 

						     jp_name_return, 

						     jp_prob_return, 

						     s_M, i_M, d_w, id, 

							 

				rbind_ipmat_FEFI, rbind_Resp_FEFI, rbind_irmat_FEFI,

				rbind_ipmat_FHDI, rbind_Resp_FHDI, rbind_irmat_FHDI,

				rbind_uox, rbind_mox, 

				List_ord,  List_ocsg);
				
		if(!b_success_FEFI)
		{
			Rprintf(" FEFI failed! Change k, break down categories, or check data quality. It may help. \n");
			return 0; 
		}

	}

	if ((i_imputation == 1) && (i_cellmake == 1) && (i_SIS != 0)) //FEFI Written by Yicheng Yang

	{

		bool b_success_FEFI_Bigp = FHDI::FHDI_Extension_Bigp_cpp(x_raw, z, r_raw,

			nrow, ncol, i_SIS,

			jp_name_return,

			jp_prob_return,

			s_M, i_M, d_w, id, codes,



			rbind_ipmat_FEFI, rbind_Resp_FEFI, rbind_irmat_FEFI,

			rbind_ipmat_FHDI, rbind_Resp_FHDI, rbind_irmat_FHDI,

			rbind_uox, rbind_mox,

			List_ord, List_ocsg);

		if (!b_success_FEFI_Bigp)
		{
			Rprintf(" FEFI with SIS failed! Change k, break down categories, or check data quality. It may help. \n");
			return 0;
		}

	}




	if( (i_imputation == 2) && (i_cellmake == 1) && (i_SIS == 0)) //FHDI

	{

		s_M ="FHDI";

		bool b_success_FHDI = FHDI::FHDI_Extension_cpp(x_raw, z, r_raw, 

						     nrow, ncol, 

						     jp_name_return, 

						     jp_prob_return, 

						     s_M, i_M, d_w, id, 

							 

				rbind_ipmat_FEFI, rbind_Resp_FEFI, rbind_irmat_FEFI,

				rbind_ipmat_FHDI, rbind_Resp_FHDI, rbind_irmat_FHDI,

				rbind_uox, rbind_mox, 

				List_ord,  List_ocsg);

		if(!b_success_FHDI)
		{
			Rprintf(" FHDI failed! Change k, break down categories, or check data quality. It may help. \n");
			return 0; 
		}

	}	


	if ((i_imputation == 2) && (i_cellmake == 1) && (i_SIS != 0)) //FHDI Written by Yicheng Yang

	{

		s_M = "FHDI";

		bool b_success_FHDI_Bigp = FHDI::FHDI_Extension_Bigp_cpp(x_raw, z, r_raw,

			nrow, ncol, i_SIS,

			jp_name_return,

			jp_prob_return,

			s_M, i_M, d_w, id, codes,



			rbind_ipmat_FEFI, rbind_Resp_FEFI, rbind_irmat_FEFI,

			rbind_ipmat_FHDI, rbind_Resp_FHDI, rbind_irmat_FHDI,

			rbind_uox, rbind_mox,

			List_ord, List_ocsg);

		if (!b_success_FHDI_Bigp)
		{
			Rprintf(" FHDI with SIS failed! Change k, break down categories, or check data quality. It may help. \n");
			return 0;
		}

	}
	


	if ((i_imputation == 1) && (i_cellmake == 2)) {
	
		s_M = "FEFI";

		bool b_success_FEFI_KNN = FHDI::FHDI_Neighbor_cpp(x_raw, z, r_raw,
			nrow, ncol,
			jp_name_return,
			jp_prob_return,
			s_M, i_M, d_w, id, List_nU,

			rbind_ipmat_FEFI, rbind_Resp_FEFI, rbind_irmat_FEFI,
			rbind_ipmat_FHDI, rbind_Resp_FHDI, rbind_irmat_FHDI,
			rbind_uox, rbind_mox,
			List_ord, List_ocsg);

		if (!b_success_FEFI_KNN)
		{
			Rprintf(" FEFI KNN failed! Change k, break down categories, or check data quality. It may help. \n");
			return 0;
		}

	}

	if ((i_imputation == 2) && (i_cellmake == 2)) {
	
		s_M = "FHDI";

		bool b_success_FHDI_KNN = FHDI::FHDI_Neighbor_cpp(x_raw, z, r_raw,
			nrow, ncol,
			jp_name_return,
			jp_prob_return,
			s_M, i_M, d_w, id, List_nU,

			rbind_ipmat_FEFI, rbind_Resp_FEFI, rbind_irmat_FEFI,
			rbind_ipmat_FHDI, rbind_Resp_FHDI, rbind_irmat_FHDI,
			rbind_uox, rbind_mox,
			List_ord, List_ocsg);

		if (!b_success_FHDI_KNN)
		{
			Rprintf(" FHDI KNN failed! Change k, break down categories, or check data quality. It may help. \n");
			return 0;
		}

	}


	//---------

	//Jackknife weights for variance estimation

	//---------

	//double** d_rw = New_dMatrix(nrow, nrow); //replaced with compact version for large matrix 
    //FHDI::RepWeight(nrow, d_rw);
	FHDI::RepWeight_FHDI d_rw(nrow); //new compact version 2019, 06 10



	//---------------------------

	//---------------------------

	//Variable Estimation using FEFI results

	//---------------------------

	//---------------------------

	s_M = "FEFI"; 

	const int nrow_dat2_FEFI = rbind_ipmat_FEFI.size_row();

	

	if(i_variance == 1 && i_imputation == 1 && i_SIS==0 && i_cellmake ==1) //when FEFI's variance estimation is required

    { 

		

		if(nrow_dat2_FEFI<=0) {Rprintf("ERROR! dimension of ipmat is less than 0!");}

	

		double** wmat_FEFI = New_dMatrix(nrow_dat2_FEFI, nrow); //nrow_dat2_FEFI = rows of w1



		bool b_success_variance_FEFI = FHDI::Variance_Est_FEFI_Extension_cpp(x_raw, z, nrow, ncol,

				d_rw, d_w, id, 

				rbind_ipmat_FEFI, rbind_Resp_FEFI, rbind_irmat_FEFI,

				rbind_ipmat_FHDI, rbind_Resp_FHDI, rbind_irmat_FHDI,

				rbind_uox, rbind_mox, 

				List_ord,  List_ocsg, 

				s_M,

				wmat_FEFI);

		if (!b_success_variance_FEFI)
		{
			Rprintf(" Variance estimation of FEFI failed! \n");
			return 0;
		}

		//-------

		//prep return

		//-------

		double* d_vrst_temp = new double[nrow]; 

		for(int i=0; i<nrow_dat2_FEFI; i++)

		{

			for(int j=0; j<nrow; j++) d_vrst_temp[j] = wmat_FEFI[i][j]; 

			rbind_vrst_FEFI.append_block(d_vrst_temp); //append row-by-row

		}



		//deallocation 		

		Del_dMatrix(wmat_FEFI, nrow_dat2_FEFI, nrow);

		delete[] d_vrst_temp; 

	}


	if (i_variance == 1 && i_imputation == 1 && i_SIS != 0 && i_cellmake ==1) //when FEFI's variance estimation is required Written by Yicheng Yang

	{



		if (nrow_dat2_FEFI <= 0) { Rprintf("ERROR! dimension of ipmat is less than 0!"); }



		double** wmat_FEFI = New_dMatrix(nrow_dat2_FEFI, nrow); //nrow_dat2_FEFI = rows of w1



		bool b_success_variance_FEFI_bigp = FHDI::Variance_Est_FEFI_Extension_Bigp_cpp(x_raw, z, nrow, ncol, i_SIS,

			d_rw, d_w, id,

			rbind_ipmat_FEFI, rbind_Resp_FEFI, rbind_irmat_FEFI,

			rbind_ipmat_FHDI, rbind_Resp_FHDI, rbind_irmat_FHDI,

			rbind_uox, rbind_mox,

			List_ord, List_ocsg,

			s_M,

			wmat_FEFI, codes);


		if (!b_success_variance_FEFI_bigp)
		{
			Rprintf(" Variance estimation bigp of FEFI failed! \n");
			return 0;
		}

		//-------

		//prep return

		//-------

		double* d_vrst_temp = new double[nrow];

		for (int i = 0; i<nrow_dat2_FEFI; i++)

		{

			for (int j = 0; j<nrow; j++) d_vrst_temp[j] = wmat_FEFI[i][j];

			rbind_vrst_FEFI.append_block(d_vrst_temp); //append row-by-row

		}



		//deallocation 		

		Del_dMatrix(wmat_FEFI, nrow_dat2_FEFI, nrow);

		delete[] d_vrst_temp;

	}


	if (i_variance == 1 && i_imputation == 1 && i_cellmake == 2) {

		if (nrow_dat2_FEFI <= 0) { Rprintf("ERROR! dimension of ipmat is less than 0!"); }

		double** wmat_FEFI = New_dMatrix(nrow_dat2_FEFI, nrow); //nrow_dat2_FEFI = rows of w1

		bool b_success_variance_FEFI_KNN = FHDI::Variance_Est_FEFI_Neighbor_cpp(x_raw, z, nrow, ncol,
			d_rw, d_w, id, List_nU,
			rbind_ipmat_FEFI, rbind_Resp_FEFI, rbind_irmat_FEFI,
			rbind_ipmat_FHDI, rbind_Resp_FHDI, rbind_irmat_FHDI,
			rbind_uox, rbind_mox,
			List_ord, List_ocsg,
			s_M,
			wmat_FEFI);

		if (!b_success_variance_FEFI_KNN)
		{
			Rprintf(" Variance estimation KNN of FEFI failed! \n");
			return 0;
		}

		//-------

		//prep return

		//-------

		double* d_vrst_temp = new double[nrow];

		for (int i = 0; i<nrow_dat2_FEFI; i++)

		{

			for (int j = 0; j<nrow; j++) d_vrst_temp[j] = wmat_FEFI[i][j];

			rbind_vrst_FEFI.append_block(d_vrst_temp); //append row-by-row

		}



		//deallocation 		

		Del_dMatrix(wmat_FEFI, nrow_dat2_FEFI, nrow);

		delete[] d_vrst_temp;

	}
	//---------------------------

	//---------------------------

	//Variable Estimation using FHDI results

	//---------------------------

	//---------------------------

	s_M = "FHDI"; 

	

	const int nrow_dat2_FHDI = rbind_ipmat_FHDI.size_row();



    if(i_variance == 1 && i_imputation == 2 && i_SIS == 0 && i_cellmake == 1) //when FHDI's variance estimation is required

	{

		

		if(nrow_dat2_FHDI<=0) {Rprintf("ERROR! dimension of ipmat is less than 0!");}

		

		double** wmat_FHDI = New_dMatrix(nrow_dat2_FHDI, nrow); //nrow_dat2_FHDI = rows of w1

		

		bool b_success_variance_FHDI = FHDI::Variance_Est_FHDI_Extension_cpp(x_raw, z, nrow, ncol,

				d_rw, d_w, id, 

				rbind_ipmat_FEFI, rbind_Resp_FEFI, rbind_irmat_FEFI,

				rbind_ipmat_FHDI, rbind_Resp_FHDI, rbind_irmat_FHDI,

				rbind_uox, rbind_mox, 

				List_ord,  List_ocsg, 

				s_M, 

				wmat_FHDI);


		if (!b_success_variance_FHDI)
		{
			Rprintf(" Variance estimation of FHDI failed! \n");
			return 0;
		}

		//-------

		//prep return

		//-------

		double* d_vrst_temp = new double[nrow]; 

		for(int i=0; i<nrow_dat2_FHDI; i++)

		{

			for(int j=0; j<nrow; j++) d_vrst_temp[j] = wmat_FHDI[i][j]; 

			rbind_vrst_FHDI.append_block(d_vrst_temp); //append row-by-row

		}

				



		//deallocation		

		Del_dMatrix(wmat_FHDI, nrow_dat2_FHDI, nrow);

		delete[] d_vrst_temp; 

	}


	if (i_variance == 1 && i_imputation == 2 && i_SIS != 0 && i_cellmake == 1) //when FHDI's variance estimation is required

	{



		if (nrow_dat2_FHDI <= 0) { Rprintf("ERROR! dimension of ipmat is less than 0!"); }



		double** wmat_FHDI = New_dMatrix(nrow_dat2_FHDI, nrow); //nrow_dat2_FHDI = rows of w1



		bool b_success_variance_FHDI_bigp = FHDI::Variance_Est_FHDI_Extension_Bigp_cpp(x_raw, z, nrow, ncol, i_SIS,

			d_rw, d_w, id,

			rbind_ipmat_FEFI, rbind_Resp_FEFI, rbind_irmat_FEFI,

			rbind_ipmat_FHDI, rbind_Resp_FHDI, rbind_irmat_FHDI,

			rbind_uox, rbind_mox,

			List_ord, List_ocsg,

			s_M,

			wmat_FHDI, codes);

		if (!b_success_variance_FHDI_bigp)
		{
			Rprintf(" Variance estimation bigp of FHDI failed! \n");
			return 0;
		}

		//-------

		//prep return

		//-------

		double* d_vrst_temp = new double[nrow];

		for (int i = 0; i<nrow_dat2_FHDI; i++)

		{

			for (int j = 0; j<nrow; j++) d_vrst_temp[j] = wmat_FHDI[i][j];

			rbind_vrst_FHDI.append_block(d_vrst_temp); //append row-by-row

		}





		//deallocation		

		Del_dMatrix(wmat_FHDI, nrow_dat2_FHDI, nrow);

		delete[] d_vrst_temp;

	}


	if (i_variance == 1 && i_imputation == 2 && i_cellmake == 2) {
	
		if (nrow_dat2_FHDI <= 0) { Rprintf("ERROR! dimension of ipmat is less than 0!"); }

		double** wmat_FHDI = New_dMatrix(nrow_dat2_FHDI, nrow); //nrow_dat2_FHDI = rows of w1

		bool b_success_variance_FHDI_KNN = FHDI::Variance_Est_FHDI_Neighbor_cpp(x_raw, z, nrow, ncol,
			d_rw, d_w, id, List_nU,
			rbind_ipmat_FEFI, rbind_Resp_FEFI, rbind_irmat_FEFI,
			rbind_ipmat_FHDI, rbind_Resp_FHDI, rbind_irmat_FHDI,
			rbind_uox, rbind_mox,
			List_ord, List_ocsg,
			s_M,
			wmat_FHDI);

		if (!b_success_variance_FHDI_KNN)
		{
			Rprintf(" Variance estimation KNN of FHDI failed! \n");
			return 0;
		}

		//-------

		//prep return

		//-------

		double* d_vrst_temp = new double[nrow];

		for (int i = 0; i<nrow_dat2_FHDI; i++)

		{

			for (int j = 0; j<nrow; j++) d_vrst_temp[j] = wmat_FHDI[i][j];

			rbind_vrst_FHDI.append_block(d_vrst_temp); //append row-by-row

		}

		//deallocation		

		Del_dMatrix(wmat_FHDI, nrow_dat2_FHDI, nrow);

		delete[] d_vrst_temp;
	}

	

	//=========================

	//Deallocate memories

	//=========================

	Del_dMatrix(z, nrow, ncol);

	Del_dMatrix(x_raw, nrow, ncol);

	Del_iMatrix(r_raw, nrow, ncol);

	//Del_dMatrix(d_rw, nrow, nrow); //replaced with a compact version

	Del_iMatrix(codes, nrow, i_SIS);

	return 1; //test return of double* 



}



//Fn===========================================================================

//Extract_Imputed_Results.cc-----------------------------------------------------------------------------

//Fn===========================================================================

void Extract_Imputed_Results(const int nrow, const int ncol, rbind_FHDI &rbind_ipmat,

                             double* final_full_data)

                             

//Description========================================

//  extract imputed values from the ipmat 

//  with weights and fractional weights

//  

//  Algorithm: 

//  yi = sum( wi*wij*y_ij)/sum(wi*wij)

//  where

//  yi = final vector corresponding to the ith row of original data

//     = {yi1, yi2, ..., yi_ncol}

//  wi = weight of the ith row

//  wij = fractional weight of the jth imputed cell on the ith row

//  y_ij = j_th imputed cell for the missing cell 

//

//  Written by Dr. I. Cho

//  updated Feb, 03, 2017. 

//

//IN   : rbind_FHDI  rbind_ipmat(4+ncol) //column size is 4+ncol (i.e., for R: ID, FID, WGT, FWGT, Variables)

//IN   : int nrow = total rows of original data matrix

//OUT  : double final_full_data(nrow*ncol) = full matrix (in vector form) with original data and imputed values for missing parts

//===================================================  

{

	int i_loc = 0; 

	double* yi = new double[ncol]; 

	Fill_dVector(final_full_data, nrow*ncol, 0.0); 

	
	int ID = 0; 
	double wi = 0.0;
	double wij =0.0; 
	double d_temp = 0.0; 
	double d_sum_wij = 0.0; 
	
	//--------

	//main loop for all rows of original data matrix

	//--------

	for(int i=0; i<nrow; i++)

	{

		//-----

		//inner loop within the identical ID 

		//-----

		d_sum_wij = 0.0;
		//double d_sum_wij = 0.0; 

		Fill_dVector(yi, ncol, 0.0); //initialize ith row data vector 

		for(int j=0; j<nrow; j++) 

		{
			double d_temp1 = rbind_ipmat(i_loc, 0); 
			ID = static_cast<int>(d_temp1) - 1 ; //1st col means ID; "-1" for actual location  
			//below is disabled on April 21, 2018 to avoid AddressSanitizer UseAfterScope error
			//int ID = (int)rbind_ipmat(i_loc, 0) - 1 ; //1st col means ID; "-1" for actual location  

			if(ID == i) //as long as the same ID 

			{

				wi = rbind_ipmat(i_loc, 2); 
				//double wi = rbind_ipmat(i_loc, 2); 
				wij= rbind_ipmat(i_loc, 3);
				//double wij= rbind_ipmat(i_loc, 3);

				

				//----

				//accumulate fractional weight

				//----

				d_sum_wij += wi*wij; //WGT*FWGT



				//----

				//do weighted summation with all the imputed cells 

				//for current row 

				//----

				for(int i_var=0; i_var<ncol; i_var++)

				{

					yi[i_var] = yi[i_var] + wi*wij * rbind_ipmat(i_loc,4+i_var);	

				}	

		

				//----

				//increment location for next row 

				//----

				i_loc++; 

			}

			

			if(ID > i) {break;} //exit inner loop 

		}

		

		if(fabs_FHDI(d_sum_wij)== 0.0) 

		{

			Rprintf("ERROR! zero sum of fractional weight at the row: "); 
			Rprintf("%d ", i);

			//early deallocation 
			delete[] yi; 
			
			return;

		}



		//----

		//copy Resp of C++ into return storage

		//----

		for(int i_var=0; i_var< ncol; i_var++) //size of columns of ipmat matrix of C++

		{

			//for(int j=0; j<nrow; j++)

			//{
				
				d_temp = 0.0; 
				d_temp = yi[i_var]/d_sum_wij; 
				//double d_temp = yi[i_var]/d_sum_wij; 

				

				//NOTE: R works column-by-column 

				//hence, below sequence is different from C++ ordering 

				final_full_data[i_var*nrow + i] = d_temp;  //note: i=current row

			//}

		}		

		

	} //end of main loop of i = [0, nrow)



	

	//----

	//deallocation

	//----

	delete[] yi; 

	

	return; 

}



//Fn===========================================================================

//Extract_Variance_Results.cc-----------------------------------------------------------------------------

//Fn===========================================================================

void Extract_Variance_Results(const int nrow, const int ncol, 

							 rbind_FHDI &rbind_ipmat,

                             double* final_full_data, 

							 rbind_FHDI &rbind_vrst, 

							 double* final_variance_data)

                             

//Description========================================

//  Calculate Jackknife Variance Estimator of the mean estimator

//

//  Algorithm: 

//  step1: remove kth data vector (k=1, nrow)

//  step2: calculate kth mean estimate

//  yi^(k) = sum( wi^(k)*wij^(k)*y_ij)/sum(wi^(k)*wij^(k))

//  where

//  yi = final vector corresponding to the ith row of original data

//     = {yi1, yi2, ..., yi_ncol}

//  wi^(k) = weight of the ith row of kth replicate of data

//  wij^(k) = fractional weight of the jth imputed cell on the ith row of kth replicate

//  y_ij = j_th imputed cell for the missing cell 

//

//  Written by Dr. I. Cho

//  updated Feb, 07, 2017. 

//

//IN   : rbind_FHDI  rbind_ipmat(4+ncol) //column size is 4+ncol (i.e., for R: ID, FID, WGT, FWGT, Variables)

//IN   : int nrow = total rows of original data matrix

//IN   : double final_full_data(nrow*ncol) = full matrix (in vector form) with original data and imputed values for missing parts

//IN   : rbind_FHDI  rbind_vrst(nrow) = fractional weights matrix from Jackknife Var Est.

//                                      row number is the same as that of ipmat    

//OUT  : double final_variance_data(ncol) = Jackknife var est vector 

//                                          Variable-wise Variance estimates

//===================================================  

{

	int i_loc = 0; //sequential index of global total rows 

	double* yi = new double[ncol]; //column-wise mean 

	Fill_dVector(final_variance_data, ncol, 0.0); 

	

	//--------

	//new mean of k_th replicate of data y for Jackknife Var Est

	//--------

	double** y_bar_i_k = New_dMatrix(nrow, ncol); // for nrow replicates for ncol variables replicates

	Fill_dMatrix(y_bar_i_k, nrow, ncol, 0.0);

	//----------
	//global temporary variable declaration
	//----------
	double d_sum_wij = 0.0; 
	int ID =0; 
	double wi = 0.0; 
	double wij = 0.0; 
	double d_temp = 0.0; 

	for(int k=0; k<nrow; k++) //Jackknife replicates 

	{	

		//---

		//re-initialization! for new jackknifed data

		//---

		i_loc = 0; 

		d_sum_wij = 0.0;
		//double d_sum_wij = 0.0; 

		Fill_dVector(yi, ncol, 0.0); //initialize vector for column-wise means of all variables  

		for(int i=0; i<nrow; i++)

		{

			//-----

			//inner loop within the identical ID 

			//-----

			for(int j=0; j<nrow; j++) 

			{
				double d_temp1 = rbind_ipmat(i_loc, 0);
				ID = static_cast<int>(d_temp1) - 1 ; //1st col means ID; "-1" for actual location  

				//int ID = (int)rbind_ipmat(i_loc, 0) - 1 ; //1st col means ID; "-1" for actual location  

				

				

				

				

				if(ID == i) //as long as the same ID 

				{
					wi = rbind_ipmat(i_loc, 2);
					//double wi = rbind_ipmat(i_loc, 2); 

					//double wij= rbind_ipmat(i_loc, 3); //used in mean calculation

					

					//-------

					//NOTE: use the replicated fractional weight in lieu of ipmat

					//-------
					
					wij = rbind_vrst(i_loc, k); //k means current Jackknifed column

					//double wij = rbind_vrst(i_loc, k); //k means current Jackknifed column

					

					//----

					//accumulate fractional weight

					//variable-wise weight summation

					//----

					d_sum_wij += wi*wij; //WGT*FWGT



					//----

					//do weighted summation with all the imputed cells 

					//for current row 

					//----

					for(int i_var=0; i_var<ncol; i_var++)

					{

						yi[i_var] = yi[i_var] + wi*wij * rbind_ipmat(i_loc,4+i_var);	

					}	

			

					//----

					//increment location for next row 

					//----

					i_loc++; 

				}

				

				if(ID > i) {break;} //exit inner loop 

			}

			

		} //end of main loop of i = [0, nrow)



		if(fabs_FHDI(d_sum_wij)== 0.0) 

		{

			Rprintf("ERROR! zero sum of fractional weight at Jackknifed row :");
			Rprintf("%d ", k);

			//early deallocation 
			delete[] yi; 
			Del_dMatrix(y_bar_i_k, nrow, ncol);
			
			return;

		}

		

			

		//----

		//store Jackknife mean estimator 

		//----

		for(int i_var=0; i_var< ncol; i_var++) //size of columns of ipmat matrix of C++

		{

			d_temp = 0.0; 
			d_temp = yi[i_var]/d_sum_wij; 
			//double d_temp = yi[i_var]/d_sum_wij; 

				

			//-----

			//bar{y}_i^(k)

			//-----

			//NOTE: R works column-by-column 

			//hence, below sequence is different from C++ ordering 

			//final_full_data[i_var*nrow + i] = d_temp;  //note: i=current row

			y_bar_i_k[k][i_var] = d_temp;  //note: k= replicate; i_var = variable

		}

		

	}//end of Jackknifed data k = [0, nrow)



	//---------------------

	//Jackknife average of y_bar_i

	//---------------------

	double* y_bar_i = new double[ncol]; 

	for(int i_var=0; i_var<ncol; i_var++)

	{

		d_temp =0.0; 

		for(int k=0; k<nrow; k++)

		{

			d_temp += y_bar_i_k[k][i_var]; 

		}

		y_bar_i[i_var] = d_temp/nrow; 

	}

	

	//---------------------

	//Final Jackknife Variance Estimation of the mean 

	//---------------------

	for(int i_var=0; i_var< ncol; i_var++) //size of columns of ipmat matrix of C++

	{

		d_temp = 0.0; 

		for(int k=0; k<nrow; k++) //Jackknife replicate

		{

			d_temp += 

				(y_bar_i_k[k][i_var] - y_bar_i[i_var])

			   *(y_bar_i_k[k][i_var] - y_bar_i[i_var]); 				

		}



		//-----

		//final jackknife variance estimate

		//-----

		final_variance_data[i_var] = d_temp * (nrow-1)/nrow; 

	}

	

	//----

	//deallocation

	//----

	delete[] yi; 

	Del_dMatrix(y_bar_i_k, nrow, ncol);

	delete[] y_bar_i; 

	

	return; 

}







//Fn===========================================================================

//Rfn_Interface_using_dotCall.cc-----------------------------------------------------------------------------

//Fn===========================================================================

//----------------------------------------------------------------------------

//- This file defines TWO functions. 

// 

// The first one is a C++ function that directly accesses 

// the C++ class. 

// 

// The second one is a C function named ’CWrapper’, that INdirectly accesses 

// the C++ class, via a call to the function.

//----------------------------------------------------------------------------

bool Rfn_test_call(double* x, int* r, int * nrow_x, int * ncol_x, 

                   double* k, double* d, int * M, 

				   int * i_option_imputation, int * i_option_variance, 

				   int * id, double* z_UserDefined,

				   int * NonCollapsible_categorical, int * i_option_SIS, int * s_option_SIS, int * i_option_cellmake, int * top_corr_var,
				   
				   rbind_FHDI &rbind_ipmat_FEFI_return,

				   rbind_FHDI &rbind_Resp_FEFI_return, 

				   rbind_FHDI &rbind_irmat_FEFI_return,

				   rbind_FHDI &rbind_ipmat_FHDI_return,

				   rbind_FHDI &rbind_Resp_FHDI_return, 

				   rbind_FHDI &rbind_irmat_FHDI_return,

				   rbind_FHDI &rbind_vrst_FEFI_return, 

				   rbind_FHDI &rbind_vrst_FHDI_return, 

				   

				   rbind_FHDI  &rbind_uox_return,

				   rbind_FHDI  &rbind_mox_return,

				   rbind_FHDI  &rbind_category_return, 

	               rbind_FHDI  &rbind_codes_return,

				   

				   std::vector<std::string> &jp_name_return_CellProb,

				   std::vector<double> &jp_prob_return_CellProb,

				   

				   const int i_option_perform, 

				   int* i_option_merge)

{ 



	bool b_success_Rfn = Rfn_test(x, r, nrow_x, ncol_x, k, d, M, 

	         i_option_imputation, i_option_variance, 

			 id, z_UserDefined,
			 
			 NonCollapsible_categorical, i_option_SIS, s_option_SIS, i_option_cellmake, top_corr_var,

			 rbind_ipmat_FEFI_return, rbind_Resp_FEFI_return, rbind_irmat_FEFI_return,

			 rbind_ipmat_FHDI_return, rbind_Resp_FHDI_return, rbind_irmat_FHDI_return,

			 rbind_vrst_FEFI_return,  rbind_vrst_FHDI_return,

	         

			 rbind_uox_return, rbind_mox_return, rbind_category_return, rbind_codes_return,

			 

			 jp_name_return_CellProb, jp_prob_return_CellProb, 

			 

			 i_option_perform,

			 i_option_merge); 

	
    if(b_success_Rfn == 1) return 1; 
	if(b_success_Rfn == 0) return 0;
	return 0;



}

//===============================================================

//===============================================================

//===============================================================

//===============================================================

//===============================================================

extern "C" {

SEXP CWrapper(SEXP x_R, SEXP r_R, SEXP z_R, SEXP i_option_perform_R, 

              SEXP nrow_x_R, SEXP ncol_x_R, 

              SEXP k_R, SEXP d_R, SEXP M_R, 

			  SEXP i_option_imputation_R, SEXP i_option_variance_R, 

			  SEXP id_R, 
			  
			  SEXP NonCollapsible_categorical_R, SEXP i_option_SIS_R, SEXP s_option_SIS_R,
			  
			  SEXP i_option_merge_R, SEXP i_option_cellmake_R, SEXP top_corr_var_R)
			  


//Description -------------------------------------------

//----------------------------------------------------------------------------

//- CWrapper 

//  to perform 

//         (1) Cell Make, (2)Cell Prob, (3) FEFI/FHDI imputation, and 

//         (4) Jackknife Var Est. (if requested)  

// 

// This C function helps invoke the deeper C++ functions. 

// R can access C code, not C++ code directly. 

// This C function provides a C-C++ interface so that R can access the C++

// Note: Rinternals.h is used. In this case, to prevent R's automatic function mapping

//       at the very beginning, #define R_NO_REMAP should be added and also

//       "Rf_" should precede all R functions used herein

// Written by Dr. In Ho Cho

// All rights reserved 

// Jan 17, 2017

//---------------------------------------------------------------------------- 

//IN   : double x_R[nrow_x_R * ncol_x_R] = data matrix in a vector form with missing units

//IN   : int    r_R[nrow_x_R * ncol_x_R] = indices for data missingness 1=observed; 0=missing 			  

//IN   : double z_R[nrow_x_R * ncol_x_R] = user-defined Z matrix (i_option_perform =4) only

//IN   : int    i_option_perform_R = main control option (1: all; 2: CellMake; 3: CellProb

//                                                        4: all by using user-defined z)

//IN   : double k_R[ncol_x_R] = a vector of categories as an initial (can be known for discrete variables) 

//IN   : double d_R[nrow_x_R] = (sampling) weights for units

//IN   : int    M_R	= imputation size (can be generaized by M_i, i denotes missing unit)

//IN   : int 	i_option_imputation_R = 1: Fully Efficient Fractional Imputation

// 									    2: Fractional Hot Deck Imputation

//IN   : int    i_option_variance_R  = 0: skip variance estimation process

//                                      1: perform variance estimation using Jackknife method

//IN   : int    id_R[nrow_x_R] = id numbers of all data 

//IN   : int    NonCollapsible_categorical_R[ncol_x_R] = id of strictly non-collapsible categorical variable

//				(0: continuous or collapsible categorical;   1:non-collapsible categorical )

//

//IN   : int    i_option_merge = random donor selection in Merge algorithm in Cell Make

//                               0: no 

//                               1: random seed for donor selection

//OUT  : List of 

//       rbind_ipmat(4+ncol) // ID, FID, WGT, FWGT, Imputed Variables, Response indices (i.e., rbind_Resp(ncol+1)) 

//       cured data matrix(nrow, ncol)

//       Mean and Standard Error (2,ncol)

//       rbind_vrst(nrow)    // Jackknife variance estimates  (returned when i_option_variance_R = 1)

//-------------------------------------------------------------------------------

{

	//testout

	//FHDI::RPrint("Begin CWrapper in Rfn_Interface... ==== "); 

    //2020, 03 06
	//automatic cumulative number for protect
	//whenever declare "PROTECT" this must be increased 
	int n_protect_total = 0; 

	x_R = PROTECT(Rf_coerceVector(x_R, REALSXP));
    n_protect_total++;
	
	double *x = REAL(x_R);         	//pointer to double vector x[col*row] that contains all data with missing units

	r_R = PROTECT(Rf_coerceVector(r_R, INTSXP));
    n_protect_total++;
	
	int    *r = INTEGER(r_R); 			//pointer to an integer vector r[n_total_x] that contains indices of 0 and 1

	//testout

	//FHDI::RPrint("in Rfn_Interface... x:  "); 

	//FHDI::RPrint(x[0]);

	//testout

	//FHDI::RPrint("in Rfn_Interface... r:  "); 

	//FHDI::RPrint(r[0]);



	i_option_perform_R = PROTECT(Rf_coerceVector(i_option_perform_R, INTSXP));
    n_protect_total++;
	
	int    *i_option_perform = INTEGER(i_option_perform_R); 			



	z_R = PROTECT(Rf_coerceVector(z_R, REALSXP));
    n_protect_total++;
	
	double *z_UserDefined = REAL(z_R); 

	

	nrow_x_R = PROTECT(Rf_coerceVector(nrow_x_R, INTSXP));
    n_protect_total++;
	
	ncol_x_R = PROTECT(Rf_coerceVector(ncol_x_R, INTSXP));
    n_protect_total++;
	
	int    *nrow_x = INTEGER(nrow_x_R);

    int	   *ncol_x = INTEGER(ncol_x_R); //pointers to integer sizes of x

	//testout

	//FHDI::RPrint("in Rfn_Interface... nrow_x:  "); 

	//FHDI::RPrint(nrow_x[0]);

	

	k_R = PROTECT(Rf_coerceVector(k_R, REALSXP)); 
    n_protect_total++;
	
	double *k = REAL(k_R);			//pointer to a double value 

	d_R = PROTECT(Rf_coerceVector(d_R, REALSXP)); 
    n_protect_total++;
	
	double *d = REAL(d_R);  		//pointer to double vector d[nrow_x] 

	M_R = PROTECT(Rf_coerceVector(M_R, INTSXP)); 
    n_protect_total++;
	
	int    *M = INTEGER(M_R); 		//pointer to integer number of donors 



	id_R = PROTECT(Rf_coerceVector(id_R, INTSXP)); 
    n_protect_total++;
	
	int    *id = INTEGER(id_R); 	//pointer to integer number of id of data

	//-----------
	//additional +1 as of 2018, 0421
	//0: continuous or collapsible categorical;   1: Non-collapsible categorical
	//-----------
	NonCollapsible_categorical_R = PROTECT(Rf_coerceVector(NonCollapsible_categorical_R, INTSXP)); 
    n_protect_total++;
	
	int    *NonCollapsible_categorical = INTEGER(NonCollapsible_categorical_R); 	
	

	i_option_imputation_R = PROTECT(Rf_coerceVector(i_option_imputation_R, INTSXP)); 
    n_protect_total++;
	
	int    *i_option_imputation = INTEGER(i_option_imputation_R); 		

	i_option_variance_R = PROTECT(Rf_coerceVector(i_option_variance_R, INTSXP)); 
    n_protect_total++;
	
	int    *i_option_variance = INTEGER(i_option_variance_R); 		



	i_option_merge_R = PROTECT(Rf_coerceVector(i_option_merge_R, INTSXP)); 
    n_protect_total++;
	
	int    *i_option_merge = INTEGER(i_option_merge_R); 	


	i_option_SIS_R = PROTECT(Rf_coerceVector(i_option_SIS_R, INTSXP));
    n_protect_total++;
	
	int    *i_option_SIS = INTEGER(i_option_SIS_R);

	s_option_SIS_R = PROTECT(Rf_coerceVector(s_option_SIS_R, INTSXP));
	n_protect_total++;

	int    *s_option_SIS = INTEGER(s_option_SIS_R);

	i_option_cellmake_R = PROTECT(Rf_coerceVector(i_option_cellmake_R, INTSXP));
	n_protect_total++;

	int    *i_option_cellmake = INTEGER(i_option_cellmake_R);

	top_corr_var_R = PROTECT(Rf_coerceVector(top_corr_var_R, INTSXP));
	n_protect_total++;

	int    *top_corr_var = INTEGER(top_corr_var_R);
	//UP TO here 13 "protect" as of Feb 2017
	//+1    for NonCollapsible_categorical_R as of April 21, 2018

	

	//----------------

	//Basic Error Check

	//as of Feb 3, 2017

	//----------------

	//M

	if(M[0]<1) 
	{
		FHDI::RPrint("Error! M is less than 1 "); 
		//UNPROTECT(13); 	//prior to 2018, 0421
		//UNPROTECT(13+1); 	//2018, 0421 
		//UNPROTECT(13 + 1 + 1); 	//2020, 0301 
		
		UNPROTECT(n_protect_total); //2020, 0306
		
		return(R_NilValue);
	}

	if(M[0]>nrow_x[0]) 
	{
		FHDI::RPrint("Error! M is larger than total rows of data "); 
		//UNPROTECT(13); 	//prior to 2018, 0421
		//UNPROTECT(13+1); 	//2018, 0421
		//UNPROTECT(13 + 1 + 1); 	//2020, 0301 
		
		UNPROTECT(n_protect_total); //2020, 0306
		
		return(R_NilValue);
	}

	//k

	for(int i=0; i<ncol_x[0]; i++)

	{

		if(k[i] < 1)
		{
			FHDI::RPrint("Error! some k is less than 1 "); 
			//UNPROTECT(13); 	//prior to 2018, 0421
			//UNPROTECT(13+1); 	//2018, 0421
			//UNPROTECT(13 + 1 + 1); 	//2020, 0301 
			
			UNPROTECT(n_protect_total); //2020, 0306
			
			return(R_NilValue);
		}

		if(k[i] > 35)
		{
			FHDI::RPrint("Error! some k is larger than 35 "); 
			//UNPROTECT(13); 	//prior to 2018, 0421
			//UNPROTECT(13+1); 	//2018, 0421
			//UNPROTECT(13 + 1 + 1); 	//2020, 0301 
			UNPROTECT(n_protect_total); //2020, 0306
			
			return(R_NilValue);
		}

	}

	

	//--------

	//prep return variables

	//--------

	rbind_FHDI  rbind_ipmat_FEFI_return(4+ncol_x[0]); //column size is 4+ncol

	rbind_FHDI  rbind_Resp_FEFI_return(ncol_x[0]+1);  //separate response matrix 

	rbind_FHDI  rbind_irmat_FEFI_return(5+ncol_x[0]); //column size is 5+ncol    

	rbind_FHDI  rbind_ipmat_FHDI_return(4+ncol_x[0]); //column size is 4+ncol

	rbind_FHDI  rbind_Resp_FHDI_return(ncol_x[0]+1);  //separate response matrix 

	rbind_FHDI  rbind_irmat_FHDI_return(5+ncol_x[0]); //column size is 5+ncol    

	rbind_FHDI  rbind_vrst_FEFI_return(nrow_x[0]);    //variance estimates of FEFI

	rbind_FHDI  rbind_vrst_FHDI_return(nrow_x[0]);    //variance estimates of FHDI

	int i_size_codes = 0;

	if (i_option_SIS[0] == 0) i_size_codes = ncol_x[0];

	if (i_option_SIS[0] != 0) i_size_codes = i_option_SIS[0];

	rbind_FHDI  rbind_codes_return(i_size_codes);  // selected variables of unique missing patterns written by Yicheng Yang. Note that it can not be 0, or bugged


	//below is for output for Cell Make only option

	rbind_FHDI  rbind_uox_return(ncol_x[0]); //unique observed patterns

	rbind_FHDI  rbind_mox_return(ncol_x[0]); //unique missing patterns

	rbind_FHDI  rbind_category_return(ncol_x[0]); //cagetorized matrix 

	

	//below is for output for Cell Prob only option

	std::vector<std::string> jp_name_return_CellProb;   //name of the joint probability table

	std::vector<double> jp_prob_return_CellProb; //the latest joint probability 	

		

	//=====================

	//=====================

	//*********************

	//=====================

	//=====================

	int i_op_p_temp = 1; 

	if(i_option_perform[0] == 4){ i_op_p_temp = 4;} 

	bool b_success_Rfn_Call = Rfn_test_call(x, r, nrow_x, ncol_x, k, d, M, 

	              i_option_imputation, i_option_variance, id, z_UserDefined,
				  
				  NonCollapsible_categorical, i_option_SIS, s_option_SIS, i_option_cellmake, top_corr_var,

				  rbind_ipmat_FEFI_return, rbind_Resp_FEFI_return, rbind_irmat_FEFI_return,

				  rbind_ipmat_FHDI_return, rbind_Resp_FHDI_return, rbind_irmat_FHDI_return,

				  rbind_vrst_FEFI_return,  rbind_vrst_FHDI_return, 

				  

				  rbind_uox_return, rbind_mox_return, rbind_category_return, rbind_codes_return,

				  jp_name_return_CellProb, jp_prob_return_CellProb,

				  

				  i_op_p_temp, i_option_merge); //1: perform Entire FEFI/FHDI  

	

	if(!b_success_Rfn_Call) 
	{
		Rprintf("ERROR! Some function of FHDI failed! ");
		Rprintf(" Change k, check data quality, further break down categorical variables, or so. It may help ");
		
		//UNPROTECT(13); //prior to 2018, 0421 
		//UNPROTECT(13+1); //2018, 0421 
		//UNPROTECT(13 + 1 + 1); 	//2020, 0301 
		
		UNPROTECT(n_protect_total); //2020, 0306
		return(R_NilValue); //abnormal ending 
	}

	//testout

	//FHDI::RPrint("in Rfn_Interface... Rfn_test_call has finished  "); 

				  

	//----------

	//FEFI: copy the calculated matrix

	//----------
	double d_temp = 0.0; 
	
	if(i_option_imputation[0] == 1) //FEFI

	{

		//--------

		//ipmat return

		//--------

		//const int i_FEFI_ipmat_size_col = rbind_ipmat_FEFI_return.size_col();

		const int i_FEFI_ipmat_size_row = rbind_ipmat_FEFI_return.size_row();

		const int n_ipmat_Resp_total = (4+ncol_x[0]) ; //Feb 7, 2017 		

		SEXP return_ipmat_FEFI = PROTECT(Rf_allocMatrix(REALSXP, i_FEFI_ipmat_size_row, n_ipmat_Resp_total));
		n_protect_total++;
		
		double* d_return_ipmat_FEFI = REAL(return_ipmat_FEFI);

		//----

		//copy ipmat of C++ first into return storage

		//----

		for(int i=0; i< 4+ncol_x[0]; i++) //size of columns of ipmat matrix of C++

		{

			for(int j=0; j<i_FEFI_ipmat_size_row; j++)

			{

				d_temp = 0.0; 

				d_temp = rbind_ipmat_FEFI_return(j,i); //from ipmat of C++



				//NOTE: R works column-by-column 

				//hence, below sequence is different from C++ ordering 

				d_return_ipmat_FEFI[i*i_FEFI_ipmat_size_row + j] = d_temp; //get all stored values 

			}

		}

		//----------

        //codes return written by Yicheng Yang

        //----------

		//const int i_codes_size_col = i_option_SIS[0];

		//const int i_codes_size_row = rbind_codes_return.size_row();

		//SEXP return_codes = PROTECT(Rf_allocMatrix(REALSXP, i_codes_size_row, i_codes_size_col));
		//n_protect_total++;

		//double* d_return_codes = REAL(return_codes);

		//for (int i = 0; i < i_codes_size_col; i++) //size of columns 

		//{

		//	for (int j = 0; j < i_codes_size_row; j++)

		//	{

		//		double d_temp = 0.0;

		//		d_temp = rbind_codes_return(j, i); //from mox of C++



		//		//NOTE: R works column-by-column 

		//		//hence, below sequence is different from C++ ordering 

		//		d_return_codes[i*i_codes_size_row + j] = d_temp; //get all stored values 

		//	}

		//}


		//----------------------

		//----------------------

		//when FEFI's variance estimation is NOT required

		//----------------------

		//----------------------

		if(i_option_variance[0] == 0)

		{

			//----

			//create list for compact return

			//-----

			SEXP list_return;

			PROTECT(list_return = Rf_allocVector(VECSXP,2));
			n_protect_total++;


			//-------

			//extract the final full data matrix

			//-------

			SEXP Final_Full_Data = PROTECT(Rf_allocMatrix(REALSXP, nrow_x[0], ncol_x[0]));
			n_protect_total++;
			
			double* d_Final_Full_Data = REAL(Final_Full_Data);		

			Extract_Imputed_Results(nrow_x[0], ncol_x[0], rbind_ipmat_FEFI_return,

									d_Final_Full_Data); 



			//--------

			//Make a final retuan LIST

			//--------

			SET_VECTOR_ELT(list_return, 0, return_ipmat_FEFI);

			SET_VECTOR_ELT(list_return, 1, Final_Full_Data);

			//if (i_option_SIS[0] != 0) {

			//	SET_VECTOR_ELT(list_return, 2, return_codes);

			//}

			//Rf_setAttrib(list_return, R_NamesSymbol, list_names_return);

			

			

			//----

			//FEFI return

			//----

			//UNPROTECT(13+3);	//prior to 2018, 0421
			//UNPROTECT(13+1+3);	// 2018, 0421
			//UNPROTECT(13 + 1 + 1 + 3); 	//2020, 0301 
			UNPROTECT(n_protect_total); 	//2020, 0306
			

			return list_return; 

		}



		//--------------------

		//--------------------

		//when FEFI's variance estimation IS required

		//--------------------

		//--------------------

		if(i_option_variance[0] == 1) 

		{

			const int i_FEFI_vrst_size_row = rbind_vrst_FEFI_return.size_row();

			SEXP return_vrst_FEFI = PROTECT(Rf_allocMatrix(REALSXP, i_FEFI_vrst_size_row, nrow_x[0]));
			n_protect_total++;
			
			double* d_return_vrst_FEFI = REAL(return_vrst_FEFI);

			//----

			//copy vrst of C++ into return storage

			//----

			for(int i=0; i< nrow_x[0]; i++) //size of COLUMNs of vrst matrix of C++

			{

				for(int j=0; j<i_FEFI_vrst_size_row; j++) //rows

				{

					d_temp = 0.0; 

					d_temp = rbind_vrst_FEFI_return(j,i); //from vrst of C++



					//NOTE: R works column-by-column 

					//hence, below sequence is different from C++ ordering 

					d_return_vrst_FEFI[i*i_FEFI_vrst_size_row + j] = d_temp; //get all stored values 

				}

			}



			//----

			//create list for compact return

			//-----

			SEXP list_return;

			PROTECT(list_return = Rf_allocVector(VECSXP,4));
			n_protect_total++;


			//-------

			//extract the final full data matrix

			//-------

			SEXP Final_Full_Data = PROTECT(Rf_allocMatrix(REALSXP, nrow_x[0], ncol_x[0]));
			n_protect_total++;
			
			double* d_Final_Full_Data = REAL(Final_Full_Data);		

			Extract_Imputed_Results(nrow_x[0], ncol_x[0], rbind_ipmat_FEFI_return,

									d_Final_Full_Data); 

									

			//-------

			//extract the final variance data matrix

			//-------

			SEXP Final_Mean_SD_Data = PROTECT(Rf_allocMatrix(REALSXP, 2, ncol_x[0])); //mean & std. dev
			n_protect_total++;
			
			double* d_Final_Mean_SD_Data = REAL(Final_Mean_SD_Data); 

			

			//Variance calculation 

			double* d_Final_Variance_Data = new double[ncol_x[0]];

			Extract_Variance_Results(nrow_x[0], ncol_x[0], 

									rbind_ipmat_FEFI_return,

									d_Final_Full_Data, 

									rbind_vrst_FEFI_return,

									d_Final_Variance_Data); 						



            //Column-wise mean calculation 

			double* d_Final_Column_Mean = new double[ncol_x[0]];

			for(int i_var=0; i_var< ncol_x[0]; i_var++) { //size of columns of ipmat matrix of C++

				d_temp = 0.0; 

				for(int i=0; i<nrow_x[0]; i++) d_temp += d_Final_Full_Data[i_var*nrow_x[0] + i] ;  //note: i=current row

				d_Final_Column_Mean[i_var] = d_temp/nrow_x[0]; 

			}

			//store mean and standard error for return 

			Fill_dVector(d_Final_Mean_SD_Data, 2*ncol_x[0], 0.0); //initialize 

			for(int i_var=0; i_var<ncol_x[0]; i_var++){

				d_Final_Mean_SD_Data[i_var*2]   = d_Final_Column_Mean[i_var]; //mean of column i_var

				if(d_Final_Variance_Data[i_var]>0.0) d_Final_Mean_SD_Data[i_var*2+1] = sqrt(d_Final_Variance_Data[i_var]);

			}

			//deallocate local array

			delete[] d_Final_Variance_Data;

			delete[] d_Final_Column_Mean; 

			

			//---------------

			//Make a final return LIST 

			//---------------

			SET_VECTOR_ELT(list_return, 0, return_ipmat_FEFI);

			SET_VECTOR_ELT(list_return, 1, Final_Full_Data);

			SET_VECTOR_ELT(list_return, 2, Final_Mean_SD_Data);

			SET_VECTOR_ELT(list_return, 3, return_vrst_FEFI);

			//if (i_option_SIS[0] != 0) {

			//	SET_VECTOR_ELT(list_return, 4, return_codes);

			//}

			

			//Rf_setAttrib(list_return, R_NamesSymbol, list_names_return);

			

			//----

			//FEFI return

			//----

			//UNPROTECT(13+5);	//prior to 2018, 0421
			//UNPROTECT(13+1+5);	//2018, 0421
			//UNPROTECT(13 + 1 + 1 + 5); 	//2020, 0301
			UNPROTECT(n_protect_total); 	//2020, 0306			
			



			return list_return; 

		}

		

	}



	//----------

	//FHDI: copy the calculated matrix

	//----------

	if(i_option_imputation[0] == 2) //FHDI

	{

		//--------

		//ipmat return

		//NOTE: ipmat + Resp for comprehensive matrix in R 

		//--------

		//const int i_FHDI_ipmat_size_col = rbind_ipmat_FHDI_return.size_col();

		const int i_FHDI_ipmat_size_row = rbind_ipmat_FHDI_return.size_row();



        const int n_ipmat_Resp_total = (4+ncol_x[0]); //Feb 7, 2017		

		SEXP return_ipmat_FHDI = PROTECT(Rf_allocMatrix(REALSXP, i_FHDI_ipmat_size_row, n_ipmat_Resp_total));
		n_protect_total++;
		
		double* d_return_ipmat_FHDI = REAL(return_ipmat_FHDI);

		//----

		//copy ipmat of C++ first into return storage

		//----

		for(int i=0; i< 4+ncol_x[0]; i++) //size of columns of ipmat matrix of C++

		{

			for(int j=0; j<i_FHDI_ipmat_size_row; j++)

			{

				d_temp = 0.0; 

				d_temp = rbind_ipmat_FHDI_return(j,i); //from ipmat of C++



				//NOTE: R works column-by-column 

				//hence, below sequence is different from C++ ordering 

				d_return_ipmat_FHDI[i*i_FHDI_ipmat_size_row + j] = d_temp; //get all stored values 

			}

		}


		//----------

		//codes return written by Yicheng Yang

		//----------

		//const int i_codes_size_col = i_option_SIS[0];

		//const int i_codes_size_row = rbind_codes_return.size_row();

		//SEXP return_codes = PROTECT(Rf_allocMatrix(REALSXP, i_codes_size_row, i_codes_size_col));
		//n_protect_total++;

		//double* d_return_codes = REAL(return_codes);


		//for (int i = 0; i < i_codes_size_col; i++) //size of columns 

		//{

		//	for (int j = 0; j < i_codes_size_row; j++)

		//	{

		//		double d_temp = 0.0;

		//		d_temp = rbind_codes_return(j, i); //from mox of C++



		//		//NOTE: R works column-by-column 

		//		//hence, below sequence is different from C++ ordering 

		//		d_return_codes[i*i_codes_size_row + j] = d_temp; //get all stored values 

		//	}

		//}

		//----------------------

		//----------------------

		//when FHDI's variance estimation is NOT required

		//----------------------

		//----------------------

		if(i_option_variance[0] == 0)

		{		

			//----

			//create list for compact return

			//-----

			SEXP list_return;



			PROTECT(list_return = Rf_allocVector(VECSXP,2));
			n_protect_total++;
			
			//-------

			//extract the final full data matrix

			//-------

			SEXP Final_Full_Data = PROTECT(Rf_allocMatrix(REALSXP, nrow_x[0], ncol_x[0]));
			n_protect_total++;
			
			double* d_Final_Full_Data = REAL(Final_Full_Data);		

			Extract_Imputed_Results(nrow_x[0], ncol_x[0], rbind_ipmat_FHDI_return,

									d_Final_Full_Data); 			

			

			SET_VECTOR_ELT(list_return, 0, return_ipmat_FHDI);

			SET_VECTOR_ELT(list_return, 1, Final_Full_Data);

			//if (i_option_SIS[0] != 0) {

			//	SET_VECTOR_ELT(list_return, 2, return_codes);

			//}

			//Rf_setAttrib(list_return, R_NamesSymbol, list_names_return);			

			

			//----

			//FHDI return

			//----

			//UNPROTECT(13+3);	//prior to 2018, 0421		
			//UNPROTECT(13+1+3);	//2018, 0421
			//UNPROTECT(13 + 1 + 1 + 3); 	//2020, 0301
			UNPROTECT(n_protect_total); 	//2020, 0306			

			return list_return; 

		}



		//--------------------

		//--------------------

		//when FHDI's variance estimation IS required

		//--------------------

		//--------------------

		if(i_option_variance[0] == 1) 

		{

			const int i_FHDI_vrst_size_row = rbind_vrst_FHDI_return.size_row();

			

			SEXP return_vrst_FHDI = PROTECT(Rf_allocMatrix(REALSXP, i_FHDI_vrst_size_row, nrow_x[0]));
			n_protect_total++;
			
			double* d_return_vrst_FHDI = REAL(return_vrst_FHDI);

			//----

			//copy vrst of C++ into return storage

			//----

			for(int i=0; i< nrow_x[0]; i++) //size of COLUMNs of vrst matrix of C++

			{

				for(int j=0; j<i_FHDI_vrst_size_row; j++) //rows

				{

					d_temp = 0.0; 

					d_temp = rbind_vrst_FHDI_return(j,i); //from vrst of C++



					//NOTE: R works column-by-column 

					//hence, below sequence is different from C++ ordering 

					d_return_vrst_FHDI[i*i_FHDI_vrst_size_row + j] = d_temp; //get all stored values 

				}

			}



			//----

			//create list for compact return

			//-----

			SEXP list_return;



			PROTECT(list_return = Rf_allocVector(VECSXP,4));
			n_protect_total++;
			
			//-------

			//extract the final full data matrix

			//-------

			SEXP Final_Full_Data = PROTECT(Rf_allocMatrix(REALSXP, nrow_x[0], ncol_x[0]));
			n_protect_total++;
			
			double* d_Final_Full_Data = REAL(Final_Full_Data);		

			Extract_Imputed_Results(nrow_x[0], ncol_x[0], rbind_ipmat_FHDI_return,

									d_Final_Full_Data); 			



			//P16----

			//extract the final variance data matrix

			//-------

			SEXP Final_Mean_SD_Data = PROTECT(Rf_allocMatrix(REALSXP, 2, ncol_x[0])); //mean & std. dev
			n_protect_total++;
			double* d_Final_Mean_SD_Data = REAL(Final_Mean_SD_Data); 

			

			//Variance calculation 

			double* d_Final_Variance_Data = new double[ncol_x[0]];			

			Extract_Variance_Results(nrow_x[0], ncol_x[0], 

									rbind_ipmat_FHDI_return,

									d_Final_Full_Data, 

									rbind_vrst_FHDI_return,

									d_Final_Variance_Data); 		

									

            //Column-wise mean calculation 

			double* d_Final_Column_Mean = new double[ncol_x[0]];

			for(int i_var=0; i_var< ncol_x[0]; i_var++) { //size of columns of ipmat matrix of C++

				d_temp = 0.0; 

				for(int i=0; i<nrow_x[0]; i++) d_temp += d_Final_Full_Data[i_var*nrow_x[0] + i] ;  //note: i=current row

				d_Final_Column_Mean[i_var] = d_temp/nrow_x[0]; 

			}

			//store mean and standard error for return 

			Fill_dVector(d_Final_Mean_SD_Data, 2*ncol_x[0], 0.0); //initialize 

			for(int i_var=0; i_var<ncol_x[0]; i_var++){

				d_Final_Mean_SD_Data[i_var*2]   = d_Final_Column_Mean[i_var]; //mean of column i_var

				if(d_Final_Variance_Data[i_var]>0.0) d_Final_Mean_SD_Data[i_var*2+1] = sqrt(d_Final_Variance_Data[i_var]);

			}

			//deallocate local array

			delete[] d_Final_Variance_Data;

			delete[] d_Final_Column_Mean; 									



			//---------------

			//final preparation of return matrices

			//---------------									

			SET_VECTOR_ELT(list_return, 0, return_ipmat_FHDI);

			SET_VECTOR_ELT(list_return, 1, Final_Full_Data );

			SET_VECTOR_ELT(list_return, 2, Final_Mean_SD_Data);

			SET_VECTOR_ELT(list_return, 3, return_vrst_FHDI);

			//if (i_option_SIS[0] != 0) {

			//	SET_VECTOR_ELT(list_return, 4, return_codes);

			//}

			//Rf_setAttrib(list_return, R_NamesSymbol, list_names_return);

			

			//----

			//FHDI return

			//----

			//UNPROTECT(13+5);	//prior to 2018, 0421		
			//UNPROTECT(13+1+5);	//2018, 0421
			//UNPROTECT(13 + 1 + 1 + 5); 	//2020, 0301 
			UNPROTECT(n_protect_total); 	//2020, 0306
			

			return list_return; 

		}		

	}

	

	//----

	//general return

	//----

	//UNPROTECT(13);	//prior to 2018, 0421
	//UNPROTECT(13+1);	//2018, 0421
	//UNPROTECT(13 + 1 + 1); 	//2020, 0301 
	UNPROTECT(n_protect_total); 	//2020, 0306
	
	return(R_NilValue); 

}

}



//===============================================================

//===============================================================

//===============================================================

//===============================================================

//===============================================================

extern "C" {

SEXP CWrapper_CellMake(SEXP x_R, SEXP r_R, SEXP nrow_x_R, SEXP ncol_x_R, 

              SEXP k_R, SEXP d_R, SEXP M_R, 

			  SEXP i_option_imputation_R, SEXP i_option_variance_R, 

			  SEXP id_R, 
			  
			  SEXP NonCollapsible_categorical_R, SEXP i_option_SIS_R, SEXP s_option_SIS_R,
			  
			  SEXP i_option_merge_R, SEXP i_option_cellmake_R, SEXP top_corr_var_R)

//Description -------------------------------------------

//----------------------------------------------------------------------------

//- CWrapper_CellMake 

//  to perform 

//         (1) Cell Make only!

// 

// This C function helps invoke the deeper C++ functions. 

// R can access C code, not C++ code directly. 

// This C function provides a C-C++ interface so that R can access the C++

// Note: Rinternals.h is used. In this case, to prevent R's automatic function mapping

//       at the very beginning, #define R_NO_REMAP should be added and also

//       "Rf_" should precede all R functions used herein

// Written by Dr. In Ho Cho

// All rights reserved 

// Feb 9, 2017

//---------------------------------------------------------------------------- 

//IN   : double x_R[nrow_x_R * ncol_x_R] = data matrix in a vector form with missing units

//IN   : int    r_R[nrow_x_R * ncol_x_R] = indices for data missingness 1=observed; 0=missing 			  

//IN   : double k_R[ncol_x_R] = a vector of categories as an initial (can be known for discrete variables) 

//IN   : double d_R[nrow_x_R] = (sampling) "w" weights for units

//IN   : int    M_R	= imputation size (can be generaized by M_i, i denotes missing unit)

//IN   : int 	i_option_imputation_R = 1: Fully Efficient Fractional Imputation

// 									    2: Fractional Hot Deck Imputation

//IN   : int    i_option_variance_R  = 0: skip variance estimation process

//                                      1: perform variance estimation using Jackknife method

//IN   : int    id_R[nrow_x_R] = id numbers of all data 

//IN   : int    NonCollapsible_categorical_R[ncol_x_R] = id of strictly non-collapsible categorical variable

//				(0: continuous or collapsible categorical;   1:non-collapsible categorical )

//

//IN   : int    i_option_merge = random donor selection in Merge algorithm in Cell Make

//                               0: no 

//                               1: random seed for donor selection

//OUT:  LIST of 

//     [[1]] ID, WGT, V1, V2, ... 

//     [[2]] categorized V1, V2, ...

//     [[3]] uox: unique patterns of observed rows sorted in the ascending order

//     [[4]] mox: unique patterns of the missing rows sorted in the ascending order

//-------------------------------------------------------------------------------

{

	//testout

	//FHDI::RPrint("Begin CWrapper_CellMake in Rfn_Interface... ==== "); 

    //2020, 03 06
	//automatic cumulative number for protect
	//whenever declare "PROTECT" this must be increased 
	int n_protect_total = 0; 



	//P1-2

	x_R = PROTECT(Rf_coerceVector(x_R, REALSXP));
	n_protect_total++;

	double *x = REAL(x_R);         	//pointer to double vector x[col*row] that contains all data with missing units

	r_R = PROTECT(Rf_coerceVector(r_R, INTSXP));
	n_protect_total++;
	
	int    *r = INTEGER(r_R); 			//pointer to an integer vector r[n_total_x] that contains indices of 0 and 1

	

	//P3-4

	nrow_x_R = PROTECT(Rf_coerceVector(nrow_x_R, INTSXP));
	n_protect_total++;
	
	ncol_x_R = PROTECT(Rf_coerceVector(ncol_x_R, INTSXP));
	n_protect_total++;
	
	int    *nrow_x = INTEGER(nrow_x_R);

    int	   *ncol_x = INTEGER(ncol_x_R); //pointers to integer sizes of x

	

	//P5-7

	k_R = PROTECT(Rf_coerceVector(k_R, REALSXP)); 
	n_protect_total++;
	
	double *k = REAL(k_R);			//pointer to a double value 

	d_R = PROTECT(Rf_coerceVector(d_R, REALSXP)); 
	n_protect_total++;
	
	double *d = REAL(d_R);  		//pointer to double vector d[nrow_x] 

	M_R = PROTECT(Rf_coerceVector(M_R, INTSXP)); 
	n_protect_total++;
	
	int    *M = INTEGER(M_R); 		//pointer to integer number of donors 



	//P8

	id_R = PROTECT(Rf_coerceVector(id_R, INTSXP)); 
	n_protect_total++;
	
	int    *id = INTEGER(id_R); 	//pointer to integer number of id of data

	//+1-----------
	//additional +1 as of 2018, 0421
	//0: continuous or collapsible categorical;   1: Non-collapsible categorical
	//-----------
	NonCollapsible_categorical_R = PROTECT(Rf_coerceVector(NonCollapsible_categorical_R, INTSXP)); 
	n_protect_total++;
	
	int    *NonCollapsible_categorical = INTEGER(NonCollapsible_categorical_R); 	
	

	//options

    //P9-10	

	i_option_imputation_R = PROTECT(Rf_coerceVector(i_option_imputation_R, INTSXP)); 
	n_protect_total++;
	
	int    *i_option_imputation = INTEGER(i_option_imputation_R); 		

	i_option_variance_R = PROTECT(Rf_coerceVector(i_option_variance_R, INTSXP)); 
	n_protect_total++;
	
	int    *i_option_variance = INTEGER(i_option_variance_R); 		



	//P_additional_1

	i_option_merge_R = PROTECT(Rf_coerceVector(i_option_merge_R, INTSXP)); 
	n_protect_total++;
	
	int    *i_option_merge = INTEGER(i_option_merge_R); 		

	i_option_SIS_R = PROTECT(Rf_coerceVector(i_option_SIS_R, INTSXP));
	n_protect_total++;
	
	int    *i_option_SIS = INTEGER(i_option_SIS_R);

	s_option_SIS_R = PROTECT(Rf_coerceVector(s_option_SIS_R, INTSXP));
	n_protect_total++;

	int    *s_option_SIS = INTEGER(s_option_SIS_R);
	

	i_option_cellmake_R = PROTECT(Rf_coerceVector(i_option_cellmake_R, INTSXP));
	n_protect_total++;

	int    *i_option_cellmake = INTEGER(i_option_cellmake_R);

	top_corr_var_R = PROTECT(Rf_coerceVector(top_corr_var_R, INTSXP));
	n_protect_total++;

	int    *top_corr_var = INTEGER(top_corr_var_R);
	//--------

	//prep return variables

	//--------

	rbind_FHDI  rbind_ipmat_FEFI_return(4+ncol_x[0]); //column size is 4+ncol

	rbind_FHDI  rbind_Resp_FEFI_return(ncol_x[0]+1);  //separate response matrix 

	rbind_FHDI  rbind_irmat_FEFI_return(5+ncol_x[0]); //column size is 5+ncol    

	rbind_FHDI  rbind_ipmat_FHDI_return(4+ncol_x[0]); //column size is 4+ncol

	rbind_FHDI  rbind_Resp_FHDI_return(ncol_x[0]+1);  //separate response matrix 

	rbind_FHDI  rbind_irmat_FHDI_return(5+ncol_x[0]); //column size is 5+ncol    

	rbind_FHDI  rbind_vrst_FEFI_return(nrow_x[0]);    //variance estimates of FEFI

	rbind_FHDI  rbind_vrst_FHDI_return(nrow_x[0]);    //variance estimates of FHDI



	//below is for output for Cell Make only option

	rbind_FHDI  rbind_uox_return(ncol_x[0]); //unique observed patterns

	rbind_FHDI  rbind_mox_return(ncol_x[0]); //unique missing patterns

	rbind_FHDI  rbind_category_return(ncol_x[0]); //cagetorized matrix 

	int i_size_codes = 0;

	if (i_option_SIS[0] == 0) i_size_codes = ncol_x[0];

	if (i_option_SIS[0] != 0) i_size_codes = i_option_SIS[0];

    rbind_FHDI  rbind_codes_return(i_size_codes);  // selected variables of unique missing patterns written by Yicheng Yang. Note that it can not be 0, or bugged

	//below is for output for Cell Prob only option

	std::vector<std::string> jp_name_return_CellProb;   //name of the joint probability table

	std::vector<double> jp_prob_return_CellProb; //the latest joint probability 	



	//user-defined z matrix (i_option_perform =4) only

	double* z_UserDefined = new double[nrow_x[0]*ncol_x[0]];	



	bool b_success_Rfn_Call = Rfn_test_call(x, r, nrow_x, ncol_x, k, d, M, 

	              i_option_imputation, i_option_variance, id, z_UserDefined,
				  
				  NonCollapsible_categorical, i_option_SIS, s_option_SIS, i_option_cellmake, top_corr_var,

				  rbind_ipmat_FEFI_return, rbind_Resp_FEFI_return, rbind_irmat_FEFI_return,

				  rbind_ipmat_FHDI_return, rbind_Resp_FHDI_return, rbind_irmat_FHDI_return,

				  rbind_vrst_FEFI_return,  rbind_vrst_FHDI_return, 

				  

				  rbind_uox_return, rbind_mox_return, rbind_category_return, rbind_codes_return,

				  jp_name_return_CellProb, jp_prob_return_CellProb, 

				  

				  2, i_option_merge); //2: perform only Cell Make  



	delete[] z_UserDefined;

	if(!b_success_Rfn_Call) 
	{
		Rprintf("ERROR! Some function of FHDI failed! ");
		Rprintf(" Change k, check data quality, further break down categorical variables, or so. It may help ");
		

		//UNPROTECT(10+1); //prior to 2018 0421
		//UNPROTECT(10 + 1 + 1); //2018 0421
		//UNPROTECT(10 + 1 + 1 + 1); //2020 0301
		UNPROTECT(n_protect_total); //2020 0306
		return(R_NilValue); //abnormal ending 
	}

	//testout

	//FHDI::RPrint("in Rfn_Interface... Rfn_test_call has finished  "); 



	//-----------

	//prep ID, WGT, raw data

	//-----------

	//P11

	SEXP return_raw_y = PROTECT(Rf_allocMatrix(REALSXP, nrow_x[0], ncol_x[0]+2));
	n_protect_total++;
	
	double* d_return_raw_y = REAL(return_raw_y);	

	

	//----

	//copy ID, WGT, x of C++ into the return storage

	//----
	double d_temp = 0.0; 

	for(int i=0; i< ncol_x[0]+2; i++) //size of columns 

	{

		for(int j=0; j<nrow_x[0]; j++)

		{

			d_temp = 0.0; 

			if(i==0) //ID column

			{

				d_temp = id[j]; 

			}

			if(i==1) //WGT column

			{

				d_temp = d[j]; 

			}

			if(i>1) //raw data matrix

			{

				d_temp = x[(i-2)*nrow_x[0] + j]; //from x of R

			}

			

			//NOTE: R works column-by-column 

			//hence, below sequence is different from C++ ordering 

			d_return_raw_y[i*nrow_x[0] + j] = d_temp; //get all stored values 

		}

	}		



	//-----------

	//prep Categorized matrix 

	//-----------

	//P12

	SEXP return_category = PROTECT(Rf_allocMatrix(REALSXP, nrow_x[0], ncol_x[0]));
	n_protect_total++;
	
	double* d_return_category = REAL(return_category);	

	

	//----

	//copy category matrix from C++ storage

	//----

	for(int i=0; i< ncol_x[0]; i++) //size of columns 

	{

		for(int j=0; j<nrow_x[0]; j++)

		{

			d_temp = 0.0; 



			d_temp = rbind_category_return(j,i); //from z of C++

			

			//NOTE: R works column-by-column 

			//hence, below sequence is different from C++ ordering 

			d_return_category[i*nrow_x[0] + j] = d_temp; //get all stored values 

		}

	}		

	

	//----------

	//prep uox & mox

	//----------

	const int i_uox_size_col = rbind_uox_return.size_col(); 

	const int i_uox_size_row = rbind_uox_return.size_row(); 

	const int i_mox_size_col = rbind_mox_return.size_col(); 

	const int i_mox_size_row = rbind_mox_return.size_row(); 

	

	//P13

	SEXP return_uox = PROTECT(Rf_allocMatrix(REALSXP, i_uox_size_row, i_uox_size_col));
	n_protect_total++;
	
	double* d_return_uox = REAL(return_uox);

	//P14

	SEXP return_mox = PROTECT(Rf_allocMatrix(REALSXP, i_mox_size_row, i_mox_size_col));
	n_protect_total++;
	
	double* d_return_mox = REAL(return_mox);	

	

	//----

	//copy uox of C++ into the return storage

	//----

	for(int i=0; i< i_uox_size_col; i++) //size of columns 

	{

		for(int j=0; j<i_uox_size_row; j++)

		{

				d_temp = 0.0; 

				d_temp = rbind_uox_return(j,i); //from uox of C++



				//NOTE: R works column-by-column 

				//hence, below sequence is different from C++ ordering 

				d_return_uox[i*i_uox_size_row + j] = d_temp; //get all stored values 

			}

	}	

	//----

	//copy mox of C++ into the return storage

	//----

	for(int i=0; i< i_mox_size_col; i++) //size of columns 

	{

		for(int j=0; j<i_mox_size_row; j++)

		{

				d_temp = 0.0; 

				d_temp = rbind_mox_return(j,i); //from mox of C++



				//NOTE: R works column-by-column 

				//hence, below sequence is different from C++ ordering 

				d_return_mox[i*i_mox_size_row + j] = d_temp; //get all stored values 

			}

	}	



	//----------

	//codes return written by Yicheng Yang

	//----------

	const int i_codes_size_col = i_option_SIS[0];

	const int i_codes_size_row = rbind_codes_return.size_row();

	SEXP return_codes = PROTECT(Rf_allocMatrix(REALSXP, i_codes_size_row, i_codes_size_col));
	n_protect_total++;

	double* d_return_codes = REAL(return_codes);


	for (int i = 0; i < i_codes_size_col; i++) //size of columns 

	{

		for (int j = 0; j < i_codes_size_row; j++)

		{

			double d_temp = 0.0;

			d_temp = rbind_codes_return(j, i); //from mox of C++



			//NOTE: R works column-by-column 

			//hence, below sequence is different from C++ ordering 

			d_return_codes[i*i_codes_size_row + j] = d_temp; //get all stored values 

		}

	}
	

	//----

	//create list for compact return

	//-----

	SEXP list_return;

	//P15

	PROTECT(list_return = Rf_allocVector(VECSXP,5));
	n_protect_total++;


	//--------

	//Make a final retuan LIST

	//--------

	SET_VECTOR_ELT(list_return, 0, return_raw_y);

	SET_VECTOR_ELT(list_return, 1, return_category);

	SET_VECTOR_ELT(list_return, 2, return_uox);

	SET_VECTOR_ELT(list_return, 3, return_mox);

	//SET_VECTOR_ELT(list_return, 4, return_codes);

	if (i_option_SIS[0] != 0) {

		SET_VECTOR_ELT(list_return, 4, return_codes);

	}

	//Rf_setAttrib(list_return, R_NamesSymbol, list_names_return);

			

	
	//UNPROTECT(15+1); //prior to 2018 0421
	//UNPROTECT(15 + 1 + 1); //2018 0421
	//UNPROTECT(15 + 1 + 1 + 1); //2020 0301
	UNPROTECT(n_protect_total); //2020 0306

	return list_return; 	

}

}





//===============================================================

//===============================================================

//===============================================================

//===============================================================

//===============================================================

extern "C" {

SEXP CWrapper_CellProb(SEXP x_R, SEXP nrow_x_R, SEXP ncol_x_R, 

                       SEXP d_R,  

			           SEXP id_R,
					   
					   SEXP NonCollapsible_categorical_R, SEXP i_option_SIS_R, SEXP s_option_SIS_R, SEXP i_option_cellmake_R, SEXP top_corr_var_R)

//Description -------------------------------------------

//----------------------------------------------------------------------------

//- CWrapper 

//  to perform 

//         Cell Prob only!

// 

// This C function helps invoke the deeper C++ functions. 

// R can access C code, not C++ code directly. 

// This C function provides a C-C++ interface so that R can access the C++

// Note: Rinternals.h is used. In this case, to prevent R's automatic function mapping

//       at the very beginning, #define R_NO_REMAP should be added and also

//       "Rf_" should precede all R functions used herein

// Written by Dr. In Ho Cho

// All rights reserved 

// Feb 9, 2017

//---------------------------------------------------------------------------- 

//IN   : double x_R[nrow_x_R * ncol_x_R] = data matrix in a vector form with missing units

//       !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

//       NOTE! x_R contains categorized values instead of original data values

//       !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

//IN   : double d_R[nrow_x_R] = (sampling) "w" weights for units

//IN   : int    id_R[nrow_x_R] = id numbers of all data 

//IN   : int    NonCollapsible_categorical_R[ncol_x_R] = id of strictly non-collapsible categorical variable

//				(0: continuous or collapsible categorical;   1:non-collapsible categorical )

//

//OUT:  LIST of 

//     [[1]] name of joint probability

//     [[2]] joint probability

//-------------------------------------------------------------------------------

{

	//testout

	//FHDI::RPrint("Begin CWrapper_CellProb in Rfn_Interface... ==== "); 

    //2020, 03 06
	//automatic cumulative number for protect
	//whenever declare "PROTECT" this must be increased 
	int n_protect_total = 0; 


	//P1

	x_R = PROTECT(Rf_coerceVector(x_R, REALSXP));
	n_protect_total++;
	double *x = REAL(x_R);         	//pointer to double vector x[col*row] that contains all data with missing units

	

	//P2-3

	nrow_x_R = PROTECT(Rf_coerceVector(nrow_x_R, INTSXP));
	n_protect_total++;
	
	ncol_x_R = PROTECT(Rf_coerceVector(ncol_x_R, INTSXP));
	n_protect_total++;
	
	int    *nrow_x = INTEGER(nrow_x_R);

    int	   *ncol_x = INTEGER(ncol_x_R); //pointers to integer sizes of x

	

	//P4

	d_R = PROTECT(Rf_coerceVector(d_R, REALSXP)); 
	n_protect_total++;
	
	double *d = REAL(d_R);  		//pointer to double vector d[nrow_x] 



	//P5

	id_R = PROTECT(Rf_coerceVector(id_R, INTSXP)); 
	n_protect_total++;
	
	int    *id = INTEGER(id_R); 	//pointer to integer number of id of data

	//+1-----------
	//additional +1 as of 2018, 0421
	//0: continuous or collapsible categorical;   1: Non-collapsible categorical
	//-----------
	NonCollapsible_categorical_R = PROTECT(Rf_coerceVector(NonCollapsible_categorical_R, INTSXP)); 
	n_protect_total++;
	
	int    *NonCollapsible_categorical = INTEGER(NonCollapsible_categorical_R); 	


	//for this Cell Prob Only option, other variables are nullified 

	//int    *r = new int[1]; 			//pointer to an integer vector r[n_total_x] that contains indices of 0 and 1
	int    *r = new int[ncol_x[0]*nrow_x[0]]; //may be needed for heap overflow
	
	//double *k = new double[1];			//pointer to a double value 
	double *k = new double[ncol_x[0]];		//since k may be updated later for categorical variable 

	int    *M = new int[1]; 		//pointer to integer number of donors 

	int    *i_option_imputation = new int[1]; 		

	int    *i_option_variance  = new int[1]; 		

	int    *i_option_merge      = new int[1]; 

	i_option_SIS_R = PROTECT(Rf_coerceVector(i_option_SIS_R, INTSXP));
	n_protect_total++;
	
	int    *i_option_SIS = INTEGER(i_option_SIS_R);

	s_option_SIS_R = PROTECT(Rf_coerceVector(s_option_SIS_R, INTSXP));
	n_protect_total++;

	int    *s_option_SIS = INTEGER(s_option_SIS_R);



	i_option_cellmake_R = PROTECT(Rf_coerceVector(i_option_cellmake_R, INTSXP));
	n_protect_total++;

	int    *i_option_cellmake = INTEGER(i_option_cellmake_R);

	top_corr_var_R = PROTECT(Rf_coerceVector(top_corr_var_R, INTSXP));
	n_protect_total++;

	int    *top_corr_var = INTEGER(top_corr_var_R);

	//--------

	//prep return variables

	//--------

	rbind_FHDI  rbind_ipmat_FEFI_return(4+ncol_x[0]); //column size is 4+ncol

	rbind_FHDI  rbind_Resp_FEFI_return(ncol_x[0]+1);  //separate response matrix 

	rbind_FHDI  rbind_irmat_FEFI_return(5+ncol_x[0]); //column size is 5+ncol    

	rbind_FHDI  rbind_ipmat_FHDI_return(4+ncol_x[0]); //column size is 4+ncol

	rbind_FHDI  rbind_Resp_FHDI_return(ncol_x[0]+1);  //separate response matrix 

	rbind_FHDI  rbind_irmat_FHDI_return(5+ncol_x[0]); //column size is 5+ncol    

	rbind_FHDI  rbind_vrst_FEFI_return(nrow_x[0]);    //variance estimates of FEFI

	rbind_FHDI  rbind_vrst_FHDI_return(nrow_x[0]);    //variance estimates of FHDI



	//below is for output for Cell Make only option

	rbind_FHDI  rbind_uox_return(ncol_x[0]); //unique observed patterns

	rbind_FHDI  rbind_mox_return(ncol_x[0]); //unique observed patterns

	rbind_FHDI  rbind_category_return(ncol_x[0]); //cagetorized matrix 

	int i_size_codes = 0;

	if (i_option_SIS[0] == 0) i_size_codes = ncol_x[0];

	if (i_option_SIS[0] != 0) i_size_codes = i_option_SIS[0];

	rbind_FHDI  rbind_codes_return(i_size_codes);  // selected variables of unique missing patterns written by Yicheng Yang. Note that it can not be 0, or bugged


	//below is for output for Cell Prob only option

	std::vector<std::string> jp_name_return_CellProb;   //name of the joint probability table

	std::vector<double> jp_prob_return_CellProb; //the latest joint probability 	



	//user-defined z matrix (i_option_perform =4) only

	double* z_UserDefined = new double[nrow_x[0]*ncol_x[0]];	



	

	bool b_success_Rfn_Call = Rfn_test_call(x, r, nrow_x, ncol_x, k, d, M, 

	              i_option_imputation, i_option_variance, id, z_UserDefined,
				  
				  NonCollapsible_categorical, i_option_SIS, s_option_SIS, i_option_cellmake, top_corr_var,

				  rbind_ipmat_FEFI_return, rbind_Resp_FEFI_return, rbind_irmat_FEFI_return,

				  rbind_ipmat_FHDI_return, rbind_Resp_FHDI_return, rbind_irmat_FHDI_return,

				  rbind_vrst_FEFI_return,  rbind_vrst_FHDI_return, 

				  

				  rbind_uox_return, rbind_mox_return, rbind_category_return, rbind_codes_return,

				  jp_name_return_CellProb, jp_prob_return_CellProb, 

				  

				  3, i_option_merge); //3: perform only Cell Prob; x has category values   


	//----
	//deallocation
	//----
	delete[] r;

	delete[] k;

	delete[] M;

	delete[] i_option_imputation;

	delete[] i_option_variance;

	delete[] i_option_merge;
	
	delete[] z_UserDefined;

	if(!b_success_Rfn_Call) 
	{
		Rprintf("ERROR! Some function of FHDI failed! ");
		Rprintf(" Change k, check data quality, further break down categorical variables, or so. It may help ");
		

		//UNPROTECT(5); //prior to 2018 0421
		//UNPROTECT(5 + 1); //2018 0421
		//UNPROTECT(5 + 1 + 1); //2018 0421
		UNPROTECT(n_protect_total); //2020 0306

		return(R_NilValue); //abnormal ending 
	}
	

	//-----------

    // prep return list

    // (1) names 

    // (2) joint probability

    //-----------

	const int i_size_jp_Final = (int)jp_name_return_CellProb.size();  

    if(i_size_jp_Final <= 0) 

	{Rprintf("Error! zero size of the table of joint probability table");}



	//P6

	SEXP jp_name_return_Final;

	PROTECT(jp_name_return_Final = Rf_allocVector(STRSXP, i_size_jp_Final)); 
	n_protect_total++;
	
	for(int i=0; i<i_size_jp_Final; i++) 

	{	

		char* ch_temp = (char*)jp_name_return_CellProb[i].c_str(); 

		SET_STRING_ELT(jp_name_return_Final, i, Rf_mkChar(ch_temp));

	}

	

	//P7

	SEXP jp_prob_return_Final;

	PROTECT(jp_prob_return_Final = Rf_allocVector(REALSXP,i_size_jp_Final)); 
	n_protect_total++;
	
	double* d_jp_prob_return_Final = REAL(jp_prob_return_Final);

	for(int i=0; i<i_size_jp_Final; i++) 

	{	

		d_jp_prob_return_Final[i] = jp_prob_return_CellProb[i]; 

	}

	

	//----

	//create list for compact return

	//-----

	SEXP list_return;

	//P8

	PROTECT(list_return = Rf_allocVector(VECSXP,2));
	n_protect_total++;


	

	//--------

	//Make a final retuan LIST

	//--------

	SET_VECTOR_ELT(list_return, 0, jp_name_return_Final);

	SET_VECTOR_ELT(list_return, 1, jp_prob_return_Final);

	//Rf_setAttrib(list_return, R_NamesSymbol, list_names_return);

			

	


	//UNPROTECT(8); //prior to 2018 0421
	//UNPROTECT(8 + 1); //2018 0421
	//UNPROTECT(8 + 1 + 1); //2020 0301
	UNPROTECT(n_protect_total); //2020 0306
	

	return list_return; 	

}

}













//embedded above 

//#include "matrix_utility_FHDI.cc" //for local matrix utilities

//#include "base_FHDI.cc"

//#include "List_FHDI.cc"

//#include "List_string_FHDI.cc"

//#include "rbind_FHDI.cc"

//#include "categorize_cpp.cc"

//#include "Zmat_Extension_cpp.cc"  

//#include "nDAU_cpp.cc"

//#include "Merge_Extension_cpp.cc"

//#include "Cell_Make_Extension_cpp.cc"

//#include "AGMAT_Extension_cpp.cc"

//#include "Cal_W_Extension_cpp.cc"

//#include "Cell_Prob_Extension_cpp.cc"

//#include "ran_FHDI.h" //for uniform distribution//not included for CRAN compatibility

//#include "Fully_Efficient_Fractional_Imputation.cc"

//#include "Fractional_Hot_Deck_Imputation.cc"

//#include "Results_Fully_Efficient_Fractional_Imputation.cc"

//#include "Results_Fractional_Hot_Deck_Imputation.cc"

//#include "FHDI_Extension_cpp.cc"

//#include "Variance_Est_FEFI_Extension_cpp.cc"

//#include "Variance_Est_FHDI_Extension_cpp.cc"

//#include "Rfn_test.cc"



//#include "Extract_Imputed_Results.cc"

//#include "Extract_Variance_Results.cc"

//Rfn_Interface_using_dotCall.cc





