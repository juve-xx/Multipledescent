Please refer to the paper: multiple descent in multiple random feature model. To get the theoretical value, here is an example:

library(MRFM)

Values=getvalue(lbd=0.001,psi=10/3,F1=1,tau=0.1,ReLU,ReLU,ReLU,psi1=1,psi2=2,psi3=1)

mainvalue(Values)



The package contains the function:
1. getvalue: an ensemble function, the first four inputs should be $\lambda$, the tuning parameter on ridge regression, $\psi:n/d$ (sample size/data dimension), F1: the norm of $\beta$, and $\tau$: the standard deviation of  noise. Then the remained inputs should be activation functions $\sigma_j(x)$, and the corresponding ratio $N_j/d$. $N_j$ is the dimension of the random feature with $\sigma_j$ and $d$ is data dimension. The outputs are Values, which contains all the information of the MRFM.

Example: Values=getvalue(lbd=0.001,psi=10/3,F1=1,tau=0.1,ReLU,psi1=1,ReLU,psi2=2)
or    Values=getvalue(lbd=0.001,psi=10/3,F1=1,tau=0.1,ReLU,ReLU,ReLU,psi1=1,psi2=2,psi3=1)

2. getuniquesolution.    the input is  Values: Ensemble values from getvalue function. the output is the  of the implicit function in our paper.

Example: fixpoints=getuniquesolution(Values).  Solution of (3.1) 

3. L0,MatrixH and MatrixV are functions return (MN,MD), H and V defined in our paper. The input is nu: the stantionary fixed points and Values: the ensembled parameters in MRFM.

Example: MatrixH(fixpoints,Values)  MatrixV(fixpoints,Values)  L0(fixpoints,Values). Value in Definition 3.5.


4. mainvalue.  The input is Values: Ensemble values from getvalue function.  output is the theoretical value of MRFM.

Example: mainvalue(Values).  Value in  Main theorem. 
  
