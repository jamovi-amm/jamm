library(foreign)
data<-read.spss("../jamm_docs/data/muller_mediation.sav", to.data.frame = T)
head(data)
data$SVO<-data$SVO-mean(data$SVO)
data$EXP<-data$EXP-mean(data$EXP)

library(lavaan)
models<-"
EXP~prime+SVO+prime:SVO
BEH~EXP+prime+SVO+prime:SVO+EXP:SVO
"
miPowerFit(mod)
mod<-lavaan::sem(models,data,fixed.x=T)
mod
library(semPlot)
semPaths(mod)  
summary(mod,fit.measures = TRUE)
parameterestimates(mod)
models<-"
EXP~prime+a*SVO+b*prime:SVO
BEH~EXP+prime+SVO+c*prime:SVO+d*EXP:SVO
d==0
"

models<-"
EXP~prime+a*SVO
BEH~EXP+prime+SVO
"
mod<-lavaan::sem(models,data,fixed.x=T)
mod
summary(mod)


data$prime_SVO<-data$prime*data$SVO
data$EXP_SVO<-data$EXP*data$SVO

models<-"
EXP~prime+a*SVO+b*prime_SVO
BEH~EXP+prime+SVO+c*prime_SVO+d*EXP_SVO
a==0
b==0
d==0
"
mod1<-lavaan::sem(models,data,fixed.x=T)
summary(mod1,fit=T)
parameterestimates(mod1)[1:8,]
semPaths(mod1)
models<-"
EXP~prime+SVO+a*prime_SVO
BEH~EXP+prime+SVO+b*prime_SVO+c*EXP_SVO
"
mod2<-lavaan::sem(models,data,fixed.x=T)
summary(mod2,fit=T)
parameterestimates(mod2)[1:8,]
semPaths(mod1)
anova(mod1,mod2)
2*(-710.807+736.323)

sleepstudy$m<-sleepstudy$Days+3*rnorm(length(sleepstudy$Subject))

library (lmerTest)
q<-function(a) as.numeric(scale(a,scale = F))
sleepstudy$cdays<-unlist(tapply(sleepstudy$Days, sleepstudy$Subject, q))
fm1<- lmer (Reaction ~ m+cdays + (cdays| Subject), sleepstudy, REML=FALSE)
summary(fm1)
fm1<- lmer (Reaction ~m+ Days + (Days| Subject), sleepstudy, REML=FALSE)
summary(fm1)
fm1<- lmer(m ~ (1|Subject)+ Days, sleepstudy)
str(fm1)
summary(fm1)

library (OpenMx)
if (is.factor (sleepstudy$Subject)) {
 subjnum <- unclass (sleepstudy$Subject)
 sleepstudy$Subject <- as.integer(levels(sleepstudy$Subject)[subjnum])
}

bySubj <- mxModel(model='bySubj', type='RAM',
latentVars=c('slope', 'intercept'),
mxData(data.frame(Subject=unique(sleepstudy$Subject)),
type='raw', primaryKey = 'Subject'),
mxPath(from=c('intercept', 'slope'), arrows =2, values =1),
mxPath(from='intercept', to='slope', arrows =2, values =.25, labels='cov1'))

sleepModel <- mxModel(
model='sleep', type='RAM', bySubj,
manifestVars=c('Reaction'), latentVars = c('cdays',"m"),
mxData(sleepstudy, type='raw', sort=FALSE),
mxPath(from='one', to='Reaction', arrows =1, free=TRUE),
mxPath(from='one', to='cdays', arrows =1, free=FALSE, labels='data.cdays'),
mxPath(from='one', to='m', arrows =1, free=FALSE, labels='data.m'),
mxPath(from='cdays', to='Reaction', arrows =1, free=TRUE),
#mxPath(from='cdays', to='m', arrows =1, free=TRUE),
mxPath(from='m', to='Reaction', arrows =1, free=TRUE),
mxPath(from='m', arrows =2, free = T),
mxPath(from='Reaction', arrows =2, values =1),
mxPath(paste0('bySubj.', c('intercept', 'slope')),'Reaction', arrows=1, free=FALSE, values=c(1,NA),
labels=c(NA, 'data.cdays'), joinKey='Subject'))
m1 <- mxRun(sleepModel)
m1
summary(m1)

bySubj <- mxModel(model='bySubj', type='RAM',
                  latentVars=c('slope', 'intercept'),
                  mxData(data.frame(Subject=unique(sleepstudy$Subject)),
                         type='raw', primaryKey = 'Subject'),
                  mxPath(from=c('intercept', 'slope'), arrows =2, values =1),
                  mxPath(from='intercept', to='slope', arrows =2, values =.25, labels='cov1'))

sleepModel <- mxModel(
  model='sleep', type='RAM', bySubj,
  manifestVars=c('Reaction'), latentVars = c('cdays'),
  mxData(sleepstudy, type='raw', sort=FALSE),
  mxPath(from='one', to='Reaction', arrows =1, free=TRUE),
  mxPath(from='one', to='cdays', arrows =1, free=FALSE, labels='data.cdays'),
  mxPath(from='cdays', to='Reaction', arrows =1, free=TRUE),
  mxPath(from='Reaction', arrows =2, values =1),
  mxPath(paste0('bySubj.', c('intercept', 'slope')),'Reaction', arrows=1, free=FALSE, values=c(1,NA),
         labels=c(NA, 'data.Days'), joinKey='Subject'))
m1 <- mxRun(sleepModel)
m1
summary(m1)


n<-100
k<-50
lo<-n*k
a1<-rnorm(k,0,3)
b1<-rnorm(k,0,1)+2
a2<-rnorm(k,0,3)
b2<-rnorm(k,0,1)+2

x<-rnorm(lo)
clu<-rep(1:k,each=n)
m<-as.numeric(sapply(1:length(a),function(i) a1[i]+b1[i]*x[clu==i]+rnorm(n)))
y<-as.numeric(sapply(1:length(a),function(i) a2[i]+b2[i]*m[clu==i]+rnorm(n)))

clus<-as.factor(clu)
mod1<-lmer(m~(1+x|clus)+x)
mod2<-lmer(y~(1+m|clus)+x+m)

summary(mod2)

bySubj <- mxModel(model='bySubj', type='RAM',
                  latentVars=c('slope', 'intercept'),
                  mxData(data.frame(Subject=unique(k)),
                         type='raw', primaryKey = 'Subject'),
                  mxPath(from=c('intercept', 'slope'), arrows =2, values =1),
                  mxPath(from='intercept', to='slope', arrows =2, values =.25, labels='cov1'))

model <- mxModel(
  model='all', type='RAM', bySubj,
  manifestVars=c('Reaction'), latentVars = c('cdays',"m"),
  mxData(sleepstudy, type='raw', sort=FALSE),
  mxPath(from='one', to='Reaction', arrows =1, free=TRUE),
  mxPath(from='one', to='cdays', arrows =1, free=FALSE, labels='data.cdays'),
  mxPath(from='one', to='m', arrows =1, free=FALSE, labels='data.m'),
  mxPath(from='cdays', to='Reaction', arrows =1, free=TRUE),
  #mxPath(from='cdays', to='m', arrows =1, free=TRUE),
  mxPath(from='m', to='Reaction', arrows =1, free=TRUE),
  mxPath(from='m', arrows =2, free = T),
  mxPath(from='Reaction', arrows =2, values =1),
  mxPath(paste0('bySubj.', c('intercept', 'slope')),'Reaction', arrows=1, free=FALSE, values=c(1,NA),
         labels=c(NA, 'data.cdays'), joinKey='Subject'))
m1 <- mxRun(sleepModel)
m1
summary(m1)
