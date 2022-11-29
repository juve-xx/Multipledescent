activation1<-function(x){
  # m=cos(pi/2*x)
  m=x*I(x>0)*80
  m
}
activation2<-function(x,alpha=1){
  # m=8*x*I(x>=0)+alpha*(exp(8*x)-1)*I(x<0)
  # m=sin(pi/2*x)
  # m=x*I(x>0)*3+I(x<0)*(exp(3*x)-1)
  m=I(x>0)*x*9
  # m=1/(1+exp(-x))
  m
}

activation3<-function(x){
  m=x*I(x>0)*1
  m
}
activation4<-function(x,alpha=1){
  # m=8*x*I(x>=0)+alpha*(exp(8*x)-1)*I(x<0)
  # m=sin(pi/2*x)
  # m=x*I(x>0)*3+I(x<0)*(exp(3*x)-1)
  m=I(x>0)*x*0.1
  # m=1/(1+exp(-x))
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




f30<-function(x){
  activation3(x)*exp(-x^2/2)/sqrt(2*pi)
}

f31<-function(x){
  activation3(x)*x*exp(-x^2/2)/sqrt(2*pi)
}

f32<-function(x){
  (activation3(x))^2*exp(-x^2/2)/sqrt(2*pi)
}


f40<-function(x){
  activation4(x)*exp(-x^2/2)/sqrt(2*pi)
}

f41<-function(x){
  activation4(x)*x*exp(-x^2/2)/sqrt(2*pi)
}

f42<-function(x){
  (activation4(x))^2*exp(-x^2/2)/sqrt(2*pi)
}




mu10=as.numeric(integrate(f10,-8,8)$value)
mu11=as.numeric(integrate(f11,-8,8)$value)
mu1s=sqrt(as.numeric(integrate(f12,-8,8)$value)-mu10^2-mu11^2)


mu20=as.numeric(integrate(f20,-8,8)$value)
mu21=as.numeric(integrate(f21,-8,8)$value)
mu2s=sqrt(as.numeric(integrate(f22,-8,8)$value)-mu21^2-mu20^2)


mu30=as.numeric(integrate(f30,-8,8)$value)
mu31=as.numeric(integrate(f31,-8,8)$value)
mu3s=sqrt(as.numeric(integrate(f32,-8,8)$value)-mu31^2-mu30^2)


mu40=as.numeric(integrate(f40,-8,8)$value)
mu41=as.numeric(integrate(f41,-8,8)$value)
mu4s=sqrt(as.numeric(integrate(f42,-8,8)$value)-mu41^2-mu40^2)






c(mu10,mu11,mu1s)

c(mu20,mu21,mu2s)

c(mu30,mu31,mu3s)

c(mu40,mu41,mu4s)


