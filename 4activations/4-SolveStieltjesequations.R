BBsolve0<-function(a,fun){
  while(sum(abs(fun(a)-a))>1e-7){
    a=fun(a)
  }
  a
}