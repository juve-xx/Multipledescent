#'@title ReLU
#'
#'@description  Example of activation function
#'@param x The input vector
#'@return ReLU(x)
#'@export
ReLU<-function(x){
x*I(x>0)
}

