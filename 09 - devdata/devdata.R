library(ggplot2)
library(caret)
a<-rep(seq(1,pi,by=0.1),5)
plot(sin(a)+rnorm(sd=0.1,length(a))+seq(-1,4,length.out = length(a)))