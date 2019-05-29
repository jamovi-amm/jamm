
########## the following understands all possible mediated effects ######


.makeTwoSteps<-function(infos) {
  paths<-diag.paths(infos)    
  M<-paths$paths
  al<-list(infos$dep,infos$mediators)
  infos$models[-infos$nmodels]
  meds<-lapply(seq_along(infos$mediators),function(m) infos$models[[m]][["ind"]])
  if (is.null(meds[[1]]))
    meds<-infos$independents
  ygrid<-expand.grid(al,stringsAsFactors = F)
  grid<-list()
  grid[["y"]]<-as.data.frame(ygrid)
  for (i in seq_along(infos$mediators)) {
    .grid<-as.data.frame(expand.grid(c(infos$mediators[i],meds[i]),stringsAsFactors = F))
    if (!("Var2" %in% names(.grid))) {
      .grid<-as.data.frame(t(.grid),stringsAsFactors = F)
      names(.grid)<-c("Var1","Var2")
    }
    grid[[infos$mediators[i]]]<-.grid
  } 
  grid
}


.mergeCombine<-function(grid) {
  merged<-list()
  k<-0
  for (j in seq_along(grid)) {
    for (i in seq_along(grid)) {
      if (i!=j) {
        print(grid[[j]])
        datum<-merge(grid[[i]],grid[[j]],by.x="Var1",by.y="Var2",all = F)
        if(nrow(datum)) {
          k<-k+1
          datum<-datum[,c(2,1,3:dim(datum)[2])]
          merged[[k]]<-datum
        }
      }
    }
  }
  merged
}


.exaustSteps<-function(grid) {
  alist<-list()
  .grid<-.mergeCombine(grid)
  alist[[1]]<-.grid
  k<-1
  inc<-1
  while(inc<3) {
    k<-k+1
    .grid<-.mergeCombine(.grid)
    alist[[k]]<-.grid
    inc<-k
  }
  if (length(alist)>1)
    alist[-length(alist)]
  else 
    alist
}


.makefinalList<-function(allList, infos) {
  results<-list()
  
  for (i in seq_along(allList)) {
    for (j in seq_along(allList[[i]])) {
      for (k in 1:dim(allList[[i]][[j]])[1]) {
        aResult<-unique(unlist(allList[[i]][[j]][k,]),fromLast=T)
        if (aResult[[length(aResult)]]==infos$dep)
          results<-c(results,list(aResult))
      }
    }
  }
  unique(results)
}

computeIndirect<-function(infos) {
  agrid<-.makeTwoSteps(infos)
  allList<-.exaustSteps(agrid)
  print(allList)
  .makefinalList(allList, infos)
}

computeIndirect(infos)
