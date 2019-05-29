mr.initConditionalTable<-function(infos,resultsTable,n64,cov_condition,ciType,ciWidth,tableOptions) {
 
    moderators<-unique(unlist(sapply(infos$moderators,n64$factorName)))
    moderators64<-jmvcore::toB64(moderators)
    
    resultsTable$setTitle("Conditional Mediation")
    labelsList<-list()
    for (i in seq_along(moderators)) {
      resultsTable$addColumn(moderators64[i],index=i,title=moderators[i],superTitle="Moderator levels")
    }
    combs<-expand.levels(moderators64,cov_condition)
    ierecoded<-lapply(infos$ieffects, function(x) gsub(":","____",x))
    components<-list()
    indirects<-list()
    for (i in seq_along(ierecoded)) {
        ie <- ierecoded[[i]]
        ienames<-infos$ieffects[[i]]
        rowKey=paste0(ie,collapse = "_")
        aRow=list(source=.nicifychain64(ienames,n64),type="Indirect")
        indirects[[rowKey]]<-aRow
        for (j in seq_len(length(ie)-1)) {
          valueName=c(ienames[[j]],ienames[[j+1]])
          valueKey=c(ie[[j]],ie[[j+1]])
          crowKey <- paste0(valueKey,collapse = "_")
          row<-list(source=.nicifychain64(valueName,n64),type="Component")
          components[[crowKey]]<-row
        }
        
    }

    totalrecoded<-lapply(infos$totaleffects, function(x) gsub(":","____",x))
    directs<-list()    
    for (i in seq_along(totalrecoded)) {
        teKey<-totalrecoded[[i]]
        teName<-infos$totaleffects[[i]]
        rowKey=paste0(teKey,collapse = "_")
        row<-list(source=.nicifychain64(teName,n64),type="Direct")
        directs[[rowKey]]<-row
        }

    totals<-list()
    for (i  in seq_along(totalrecoded)) {
         teKey<-totalrecoded[[i]]
         teName<-infos$totaleffects[[i]]
         rowKey=paste0(teKey,collapse = "_t_")
         row<-list(source=.nicifychain64(teName,n64),type="Total")
         totals[[rowKey]]<-row
    }

    for (j in 1:dim(combs)[1]) {
       for (rowKey in names(indirects)) {
          newKey<-paste(j,rowKey,sep = "_..._")
          row<-indirects[[rowKey]]
          row[names(combs)]<-combs[j,]
          resultsTable$addRow(rowKey=newKey,row)
       }
      if ("component" %in% tableOptions)
        for (rowKey in names(components)) {
          newKey<-paste(j,rowKey,sep = "_..._")
          row<-components[[rowKey]]
          row[names(combs)]<-combs[j,]
          resultsTable$addRow(rowKey=newKey,row)
          n<-length(resultsTable$rowKeys)
          resultsTable$addFormat(rowNo=n, col=2+(length(infos$moderators)), jmvcore::Cell.INDENTED)
        }
      
       for (rowKey in names(directs)) {
          newKey<-paste(j,rowKey,sep = "_..._")
          row<-directs[[rowKey]]
          row[names(combs)]<-combs[j,]
          resultsTable$addRow(rowKey=newKey,row)
       }
      for (rowKey in names(totals)) {
        newKey<-paste(j,rowKey,sep = "_..._")
        row<-totals[[rowKey]]
        row[names(combs)]<-combs[j,]
        resultsTable$addRow(rowKey=newKey,row)
      }
      firstKey<-newKey<-paste(j,names(indirects)[1],sep = "_..._")
      resultsTable$addFormat(rowKey=firstKey,col=1,jmvcore::Cell.BEGIN_GROUP)
    }
    
    resultsTable$getColumn('ci.lower')$setSuperTitle(jmvcore::format('{}% C.I. (a)', ciWidth))
    resultsTable$getColumn('ci.upper')$setSuperTitle(jmvcore::format('{}% C.I. (a)', ciWidth))
    add<-ifelse(ciType=="standard" || ciType=="none","",". This may take a while")
    .note<-paste0(NOTES[["ci"]][[ciType]],add)
    resultsTable$setNote("cinote",paste("(a) Confidence intervals computed with method:",.note))
}

mr.initTable<-function(infos,resultsTable,n64,ciType,ciWidth,tableOptions) {
  
  resultsTable$getColumn('ci.lower')$setSuperTitle(jmvcore::format('{}% C.I. (a)', ciWidth))
  resultsTable$getColumn('ci.upper')$setSuperTitle(jmvcore::format('{}% C.I. (a)', ciWidth))
  ierecoded<-lapply(infos$ieffects, function(x) gsub(":","____",x))
  components<-list()
  for (i in seq_along(ierecoded)) {
    ie <- ierecoded[[i]]
    ienames<-infos$ieffects[[i]]
    rowKey=paste0(ie,collapse = "_")
    aRow=list(source=.nicifychain64(ienames,n64),type="Indirect")
    resultsTable$addRow(rowKey=rowKey,aRow)
    #        resultsTable$addFormat(rowKey=rowKey, col=1, jmvcore::Cell.BEGIN_GROUP)
    
    for (j in seq_len(length(ie)-1)) {
      valueName=c(ienames[[j]],ienames[[j+1]])
      valueKey=c(ie[[j]],ie[[j+1]])
      crowKey <- paste0(valueKey,collapse = "_")
      row<-list(source=.nicifychain64(valueName,n64),type="Component")
      components[[crowKey]]<-row
    }
    
  }
  resultsTable$addFormat(rowKey=rowKey, col=1, jmvcore::Cell.END_GROUP)
  if ("component" %in% tableOptions)
    for (rowKey in names(components)) {
      resultsTable$addRow(rowKey=rowKey,components[[rowKey]])
      n<-length(resultsTable$rowKeys)
      resultsTable$addFormat(rowNo=n, col=2, jmvcore::Cell.INDENTED)
    }
  
  totalrecoded<-lapply(infos$totaleffects, function(x) gsub(":","____",x))
  
  for (i in seq_along(totalrecoded)) {
    teKey<-totalrecoded[[i]]
    teName<-infos$totaleffects[[i]]
    rowKey=paste0(teKey,collapse = "_")
    row<-list(source=.nicifychain64(teName,n64),type="Direct")
    resultsTable$addRow(rowKey,row)
  }
  
  resultsTable$addFormat(rowKey=paste0(infos$totaleffects[[1]],collapse = "_"),col=1,jmvcore::Cell.BEGIN_GROUP)
  for (i  in seq_along(totalrecoded)) {
    teKey<-totalrecoded[[i]]
    teName<-infos$totaleffects[[i]]
    rowKey=paste0(teKey,collapse = "_t_")
    row<-list(source=.nicifychain64(teName,n64),type="Total")
    resultsTable$addRow(rowKey,row)
  }
  resultsTable$addFormat(rowKey=paste0(infos$totaleffects[[1]],collapse = "_t_"),col=1,jmvcore::Cell.BEGIN_GROUP)
  
  
  add<-ifelse(ciType=="standard" || ciType=="none","",". This may take a while")
  .note<-paste0(NOTES[["ci"]][[ciType]],add)
  resultsTable$setNote("cinote",paste("(a) Confidence intervals computed with method:",.note))
}
