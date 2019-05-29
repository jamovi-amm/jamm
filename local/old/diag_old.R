.columnize<-function(n,space=.80,default=.5) {
  if (n==1)
    return(default)
  ch<-space/(n-1)
  x<-.90
  res<-x
  for (i in 1:(n-1)) {
    x<-x-ch
    res<-c(res,x)
  }
  res
}

.paths<-function(infos,val="") {
  M<-matrix(0,nrow=infos$nvars,ncol=infos$nvars)
  colnames(M)<-rownames(M)<-infos$vars
  for (f in infos$models) {
    d<-which(infos$vars==f$dep)
    i<-which(infos$vars %in% f$ind)
    M[d,i]<-val
  }
  M
}




diag.curves<-function(infos) {
  M<-matrix(0,nrow=infos$nvars,ncol=infos$nvars)
  colnames(M)<-rownames(M)<-infos$vars
  ## curve from x to y if m in the middle
  if ((infos$nmeds+infos$ninds)==2)
    return(M)
  iodd<-infos$ninds %% 2
  modd<-infos$nmeds %% 2
  if (iodd==1 & modd==1)
    M[infos$nvars,median(1:infos$ninds)]<-.15
 ## curve extremes in a mediators chain
  if (length(infos$mediators)>2) {
     for (i in seq_len(infos$nmeds-2)) {
       cc<-infos$mediators[i]
       rr<-infos$mediators[infos$nmeds]
       mark(infos$mediators[i])
       M[rr,cc]<-.05*(infos$nmeds-i)
       M[cc,rr]<-.05*(infos$nmeds-i)
       
     }
     
  }
  mark(M)
  
  ##
  M
}



diag.paths<-function(infos,required=F,suggested=F,shiftmed=0) {

  if (is.null(infos))
          return()
  .infos<-infos
  COL<-matrix("black",ncol=.infos$nvars,nrow=.infos$nvars)
  COL<-matrix("black",ncol=.infos$nvars,nrow=.infos$nvars)
  diag(COL)<-"purple"
  BCOL<-rep("black", .infos$nvars)
  rownames(COL)<-colnames(COL)<-.infos$vars
  
  PAT<-.paths(.infos)

  hasRequired<-FALSE
  if (required) {
     .infos<-infos$required
     BCOL<-rep("black", .infos$nvars)
     COL<-matrix("black",ncol=.infos$nvars,nrow=.infos$nvars)
     COL<-matrix("black",ncol=.infos$nvars,nrow=.infos$nvars)
     diag(COL)<-"purple"
     rownames(COL)<-colnames(COL)<-.infos$vars
    .PAT<-.paths(.infos)
     if (dim(.PAT)[1]==dim(PAT)[1]) {
            test<-(.PAT!=PAT)
            if (any(test)) hasRequired<-TRUE
            COL[test]<-"red"
     } 
    PAT<-.PAT
    where<-(grep(".....",colnames(PAT),fixed = T))
    if (is.something(where)) hasRequired<-TRUE
    COL[,where]<-"red"
    COL[where,]<-"red"
    BCOL[where]<-"red"
  }

  hasSuggested<-FALSE
  if (suggested) {
    .infos<-infos$suggested
    .PAT<-.paths(.infos)
    test<-(.PAT!=PAT)
    if (any(test)) hasSuggested<-TRUE
    COL[test]<-"green"
    PAT<-.PAT
  }

  hasImpossible<-FALSE
  
  if (infos$isImpossible) {
    hasImpossible<-TRUE
    dead<-sapply(infos$impossibles, function(imp) {
      ii<-setdiff(imp,infos$independents)
      sapply(1:(length(ii)-1), function(i) {
        COL[ii[i+1],ii[i]]<<-"purple"  
      })
    })
    
  }
  CUR<-diag.curves(.infos)
  POS<-diag.positions(.infos,shiftmed)
  hasFakes<-(length(grep(".....",.infos$vars,fixed = T))>0)
  LABS<-gsub(".....","=?",.infos$vars,fixed = T)
  
  return(list(paths=PAT,colors=COL,
              curves=CUR,bcolors=BCOL,
              pos=POS,labs=LABS,
              hasImpossible=hasImpossible,
              hasSuggested=hasSuggested,
              hasRequired=hasRequired,
              hasFakes=hasFakes))
  
}


diag.positions<-function(infos,shiftmed=0) {
  nx<-infos$ninds
  nm<-infos$nmeds
  .nvar<-infos$nvars
  
  M<-matrix(0,ncol=2,nrow = .nvar)
  wherey<-ifelse(nx+nm>2,.5,.15)
  wherem<-1-wherey
  px=1/(nx+1)
  if (nx==1 && nm==1)
    M[1,2]<- .15    
  else 
    for (i in seq_len(nx)) {
      rat=1-(px*i)
      M[i,2]<- rat    
    }
  
  M[1:nx,1]<-.1
  
  medsx<-(nx+1):(nx+nm)
  M[medsx,2]<-.columnize(nm,default = wherem)
  M[medsx,1]<-.5
  M[.nvar,1]<-.90
  M[.nvar,2]<-ifelse(nx==1 & nm==1,.15,.50)
  
  if (infos$nmeds==1)
    return(as.matrix(M))
  ### shift mediators in a chain
  
  M[.nvar,2]<-wherey
  colnames(M)<-c("x","y")
  rownames(M)<-infos$vars
  PAT<-.paths(infos,val=1)
  PAT<-PAT[infos$mediators,infos$mediators]
  i<-0
  for (i in seq_len(infos$nmeds)) {
    m<-infos$mediators[i]
    where<-which(PAT[,m]==1)
    if (length(where)>0) {
      M[m,"x"]<-M[m,"x"]-shiftmed*(infos$nmeds^2-i^2)
      M[names(where),"x"]<-M[names(where),"x"]+.05
      #      if (i<infos$nmeds) {
      #          lefts<-infos$mediators[(i+1):infos$nmeds]
      #          M[lefts,"x"]<-M[lefts,"x"]+.04
      #      }      
    }
  }
  as.matrix(M)
}


diag.fake<-function(paths,vars) {
  labs<-paths$labs
  for (i in seq_along(paths$labs))
         if (!(labs[i] %in% vars)) {
            paths$colors[i,]<-"red"
            paths$colors[,i]<-"red"
            paths$bcolors[i]<-"red"
            paths$hasRequired<-TRUE
            paths$hasFakes<-TRUE
         }
    
  return(paths)
  
}
