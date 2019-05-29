library(OpenMx)

flatData <- read.table("ch4mv.dat")
colnames(flatData) <- c("orgid",'female','white',
                        'satpay','morale','org1','org2','benefit','cond',
                        'resour','zproduct','lev1wt','lev2wt')

level1 <- flatData[,c('benefit','cond','female','white','orgid')]
level2 <- flatData[!duplicated(flatData$orgid),c('orgid', 'org1','org2','zproduct')]

orgModel <- mxModel(
  "org", type="RAM",
  mxData(level2, 'raw', primaryKey = "orgid"),
  manifestVars = c('zproduct'),
  latentVars = c('Benefits','Conditions', 'org1','org2'),
  mxPath(c('zproduct','Benefits','Conditions'), arrows=2, values=1),
  mxPath('Benefits','Conditions', arrows=2),
  mxPath('one', 'org1', labels='data.org1', free=FALSE),
  mxPath('one', 'org2', labels='data.org2', free=FALSE),
  mxPath('org1', c('Benefits', 'Conditions')),
  mxPath('org2', 'zproduct'),
  mxPath('org2', c('Benefits', 'Conditions')),
  mxPath('one', 'zproduct'),
  mxPath('zproduct', c('Benefits','Conditions'))
)

empModel <- mxModel(
  "emp", type="RAM", orgModel,
  mxData(level1, 'raw'),
  manifestVars = c('benefit','cond'),
  latentVars = c('female','white'),
  mxPath('one','female', labels='data.female', free=FALSE),
  mxPath('one','white', labels='data.white', free=FALSE),
  mxPath(c('female','white'), c('benefit','cond'), connect = "all.bivariate"),
  mxPath(c('benefit','cond'), arrows=2, connect = "unique.pairs", values=c(1,0,1)),
  mxPath('org.Benefits', 'benefit', values=1, free=FALSE, joinKey = "orgid"),
  mxPath('org.Conditions', 'cond', values=1, free=FALSE, joinKey = "orgid"),
  mxPath('one', c('benefit','cond'))
)

empModel <- mxRun(empModel)
summary(empModel)