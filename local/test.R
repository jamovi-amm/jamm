source("R/0constants.R")
source("R/functions.R")
source("R/smart.R")

library(lavaan)

x1<-rnorm(100)
x2<-x1+rnorm(100)
x3<-rnorm(100)
x4<-rnorm(100)
fact1<-rep(c(1:4),25)
m1<-x1+rnorm(100)
m2<-x1+x2+rnorm(100)
m3<-x1+x2+x3+rnorm(100)
m4<-x1*x2+rnorm(100)
y<-m1+m2+m4+x3+x4*fact1+rnorm(100)
data<-data.frame(cbind(y,m1,m2,m3,m4,fact1,x1,x2,x3,x4))
data$fact1<-factor(data$fact1)
head(data)
#write.csv(data,"../jamm_docs/data/somedata.csv")

mm<-lm(y~x1*x2+fact1)
ss<-summary(mm)
ss$df[2]
qq<-ss$coefficients
mm$coefficients
all.vars(as.formula("y~x1*x2+fact1"))
toscale<-names(which(sapply(names(data),function(a) class(data[[a]]))=="numeric"))
noscale<-names(which(sapply(names(data),function(a) class(data[[a]]))=="factor"))
zdata<-cbind(scale(data[,toscale]),data[,noscale])


lmeds<-list(
  list(dep="m1",ind=list("x1","x2","x3",list("x1","x2"),list("x1","x3"),list("x1","x2","x3")))
)
moderators<-list("x1")
full<-list(dep="y",ind=c("m1", "x1","x2","x3","m1:x1"))

(infos<-smartMediation$new(lmeds,full,moderators))

infos$fullmodel

tab<-jmf.mediationTable(infos,data = data)
tab<-tab[tab$op=="~" & tab$model=="med",]

tab
a<-"1....ciao"
rr<-strsplit(a,"....",fixed = T)[[1]]
rr[[2]]
