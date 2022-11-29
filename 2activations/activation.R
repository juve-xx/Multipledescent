activation1<-function(x){
  # m=cos(pi/2*x)
  # m=x*I(x>0)
  # m=1/(1+exp(-x))
  m=x*I(x>0)*3+I(x<0)*(exp(3*x)-1)
  m
}
activation2<-function(x,alpha=1){
  # m=x*I(x>=0)+alpha*(exp(x)-1)*I(x<0)
  # m=sin(pi/2*x)
  # m=x*I(x>0)*3+I(x<0)*(exp(3*x)-1)
  # m=x*I(x>0)*0.25
  m=I(x>0)*x/4
  # m=1/(1+exp(-x))/50
  m
}

f10<-function(x){
  activation1(x)*exp(-x^2/2)/sqrt(2*pi)
}

f11<-function(x){
  activation1(x)*x*exp(-x^2/2)/sqrt(2*pi)
}

f12<-function(x){
  (activation1(x))^2*exp(-x^2/2)/sqrt(2*pi)
}





f20<-function(x){
  activation2(x)*exp(-x^2/2)/sqrt(2*pi)
}

f21<-function(x){
  activation2(x)*x*exp(-x^2/2)/sqrt(2*pi)
}

f22<-function(x){
  (activation2(x))^2*exp(-x^2/2)/sqrt(2*pi)
}

mu10=as.numeric(integrate(f10,-8,8)$value)
mu11=as.numeric(integrate(f11,-8,8)$value)
mu1s=sqrt(as.numeric(integrate(f12,-8,8)$value)-mu10^2-mu11^2)


mu20=as.numeric(integrate(f20,-8,8)$value)
mu21=as.numeric(integrate(f21,-8,8)$value)
mu2s=sqrt(as.numeric(integrate(f22,-8,8)$value)-mu21^2-mu20^2)
c(mu10,mu11,mu1s)

c(mu20,mu21,mu2s)



