.diag.moderators_coord<-function(j) {
  
  M<-list(c(xleft=.00,ytop=.99),
          c(xleft=.80,ytop=.99),
          c(xleft=.00,ytop=.20),
          c(xleft=.80,ytop=.20))
  m<-(j %% 4)
  if (m==0) m=4
  r<-(j %/% 4)
  line<-M[[m]]  
  line["xlef"]<-line["xlef"]+.1*r  
  line
}

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






diag.paths<-function(infos,suggested=F,shiftmed=0) {

  BCOL<-rep("black", infos$nvars)
  #define paths
  M<-infos$M
  PAT<-M
  PAT[PAT!="0"]<-""
  # define colors of paths
  COL<-matrix("black",ncol=infos$nvars,nrow=infos$nvars)
  rownames(COL)<-colnames(COL)<-infos$vars
  
  diag(COL)<-"purple"
  COL[M=="R"]<-"red"  
  if (suggested)
         COL[M=="S"]<-"green"
  else
         PAT[M=="S"]<-"0"
  
  fake<-grep(".....",infos$vars,fixed = TRUE)
  BCOL[fake]<-"red"
  COL[fake,]<-"red"
  COL[,fake]<-"red"
  
  if (infos$isImpossible) {
    for (imp in infos$impossibles) {
      COL[imp[[1]],imp[[2]]]<-"purple"
    }
  }

  

  LABS<-gsub("X.....",jmvcore::toB64("X=?"),infos$vars,fixed = T)
  LABS<-gsub("M.....",jmvcore::toB64("M=?"),LABS,fixed = T)
  LABS<-gsub("Y.....",jmvcore::toB64("Y=?"),LABS,fixed = T)
  CUR<-.curves(infos)
  POS<-.positions(infos,shiftmed = shiftmed )
  return(list(paths=PAT,colors=COL,
              curves=CUR,bcolors=BCOL,
              pos=POS,labs=LABS))
  
}


.positions<-function(infos,shiftmed=0) {
  nx<-length(infos$independents)
  nm<-length(infos$mediators)
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
  if (nm==1)
    return(as.matrix(M))

    ### shift mediators in a chain
  
  M[.nvar,2]<-wherey
  colnames(M)<-c("x","y")
  rownames(M)<-infos$vars
  PAT<-infos$M
  PAT<-PAT[infos$mediators,infos$mediators]

  i<-0
  for (i in seq_len(nm)) {
    m<-infos$mediators[i]
    where<-which(PAT[,m]!="0")
    if (length(where)>0) {
      M[m,"x"]<-M[m,"x"]-shiftmed*(nm^2-i^2)
      M[names(where),"x"]<-M[names(where),"x"]+.05
    }
  }
  as.matrix(M)
}


.curves<-function(infos) {
  nm<-length(infos$mediators)
  ni<-length(infos$independents)
  M<-matrix(0,nrow=infos$nvars,ncol=infos$nvars)
  colnames(M)<-rownames(M)<-infos$vars
  ## curve from x to y if m in the middle
  if ((nm+ni)==2)
    return(M)
  iodd<-ni %% 2
  modd<-nm %% 2
  if (iodd==1 & modd==1)
    M[infos$nvars,median(1:ni)]<-.15
  ## curve extremes in a mediators chain
  if (nm>2) {
    for (i in seq_len(nm-2)) {
      cc<-infos$mediators[i]
      rr<-infos$mediators[nm]
      M[rr,cc]<-.05*(nm-i)
      M[cc,rr]<-.05*(nm-i)
    }
  }
  ##
  M
}

.diag.moderators_coord<-function(nvars,j) {
  if (nvars==3) 
  M<-list(c(xleft=.10,ytop=.80),
          c(xleft=.85,ytop=.80),
          c(xleft=.10,ytop=.60),
          c(xleft=.85,ytop=.60))
  else
    M<-list(c(xleft=.25,ytop=.90),
            c(xleft=.75,ytop=.90),
            c(xleft=.25,ytop=.10),
            c(xleft=.70,ytop=.10))
  
  
  m<-(j %% 4)
  if (m==0) l=4
  else l=m
  r<-((j-1) %/% 4)
  print(r)
  line<-M[[l]]  
  line["xleft"]<-line["xleft"]+.05*r  
  line["ytop"]<-line["ytop"]-.1*r  
  line
}


diag.plot_mods<-function(plot, moderators) {
  
  hbox<-plot$rec[1,4]-plot$rec[1,2]
  boxwide<-.1
  nvars<-nrow(plot$rect)
  for (j in seq_along(moderators)) {
    mod<-moderators[j]
    coord<-.diag.moderators_coord(nvars,j)
    xleft<-coord["xleft"]
    ytop<-coord["ytop"]
    diagram::textrect(c(xleft,ytop), radx = boxwide, rady = .06, 
             shadow.size =0, adj = c(0.5, 0.5), lwd=2,
             lab = jmvcore::fromB64(moderators[j]), box.col = "white", 
             lcol = "black",  angle = 0)
  }
}

diag.plot_mod_arr<-function(plot,from,to,modindex) {
  
  nvars<-nrow(plot$comp)
  coord<-.diag.moderators_coord(nvars,modindex)
  xleft<-coord["xleft"]
  ytop<-coord["ytop"]
  
  target<-plot$arr[(plot$arr$row==to & plot$arr$col==from),]
  righty<-sign(xleft-target$ArrowX)
  uppy<-sign(ytop-target$ArrowY)
  
  xadj<-.03*righty
  yadj<-.03*uppy
  
  xfirst<-"xright"
  xsecond<-"xleft"
  yfirst<-"ytop"
  ysecond<-"ybot"
  
  tx<-xadj+(plot$rect[from,xfirst]+plot$rect[to,xsecond])/2
  ty<-yadj+(plot$rect[from,yfirst]+plot$rect[to,ysecond])/2
  arr.lenght=1/(nvars-1)
  shape::Arrows(xleft,ytop,tx,ty,arr.type="triangle",arr.width = .1, arr.length = arr.lenght,arr.col = "gray")
}


