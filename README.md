# Multipledescent
This is the supplementary material for the paper "Multiple descent in the multiple random feature model". We also provide the codes of experiments in the paper.


The code of theoretical curves for multiple random feature models are given in the folder MRFM. One can install the packages to get the theoretical points. 

The package contains the function:
1. getvalue: an ensemble function, the first four inputs should be $\lambda$, the tuning parameter on ridge regression, $\psi:n/d$ (sample size/data dimension), F1: the norm of $\beta$, and $\tau$: the standard deviation of  noise. Then the remained inputs should be activation functions $\sigma_j(x)$, and the corresponding ratio $N_j/d$. $N_j$ is the dimension of the random feature with $\sigma_j$ and $d$ is data dimension. The outputs are Values, which contains all the information of the MRFM.
2. getuniquesolution.    the input is  Values: Ensemble values from getvalue function. the output is the  of the implicit function in our paper.
3. L0,MatrixH and MatrixV are functions return (MN,MD), H and V defined in our paper. The input is nu: the stantionary fixed points and Values: the ensembled parameters in MRFM.
4. mainvalue.  The input is Values: Ensemble values from getvalue function.  output is the theoretical value of MRFM.
One can download the MRFM folder, and use R CMD build MRFM to get the R package. It allows us to have different R functions which help the calculation of theoretical values in MRFM.





The code of experimental points for multiple random feature models are given in multidescent.ipynb: The code is given for the four different activation functions, but readers could use it to reformulate the 2activation functions or 3activation functions.
