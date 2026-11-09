# Build the conceptual diagram directly from the specified regression terms.
# This needs no estimation or UI objects.
model_diagram_data <- function(options) {
  mediators <- jmvcore::toB64(options$mediators)
  dependent <- jmvcore::toB64(options$dep)
  moderators <- unique(jmvcore::toB64(unlist(options$moderatorsTerms)))
  outcomes <- c(mediators, dependent)
  equations <- c(options$mediatorsTerms, list(options$modelTerms))
  sources <- vector("list", length(outcomes))
  moderated <- list()

  for (equation in seq_along(outcomes)) {
    for (term in equations[[equation]]) {
      variables <- jmvcore::toB64(term)
      termModerators <- intersect(variables, moderators)
      predictors <- setdiff(variables, moderators)
      if (length(predictors) == 0)
        next()
      source <- paste(predictors, collapse = ":")
      sources[[equation]] <- unique(c(sources[[equation]], source))
      for (moderator in termModerators) {
        moderated[[length(moderated) + 1]] <- list(
          moderator = moderator, from = source, to = outcomes[[equation]])
      }
    }
  }

  independents <- setdiff(unique(unlist(sources)), outcomes)
  variables <- c(independents, outcomes)
  paths <- matrix("0", length(variables), length(variables),
                  dimnames = list(variables, variables))

  # Suggested paths are visual guidance only; never change the fitted model.
  paths[outcomes, independents] <- "S"
  paths[dependent, mediators] <- "S"
  for (equation in seq_along(outcomes))
    paths[outcomes[[equation]], sources[[equation]]] <- "P"

  infos <- list(M = paths, vars = variables, nvars = length(variables),
                mediators = mediators, independents = independents,
                isImpossible = FALSE)
  suggested <- "suggested" %in% options$pathOptions
  diagram <- diag.paths(infos, suggested = suggested, shiftmed = .01)
  diagram$labels <- vapply(strsplit(variables, ":", fixed = TRUE),
                          function(term) paste(jmvcore::fromB64(term), collapse = " * "),
                          character(1))
  diagram$moderators <- moderators
  diagram$moderated <- lapply(moderated, function(path) {
    list(from = match(path$from, variables), to = match(path$to, variables),
         moderator = match(path$moderator, moderators))
  })
  diagram$nindependents <- length(independents)
  diagram$nmediators <- length(mediators)
  diagram$notes <- character()
  if (suggested && any(paths == "S"))
    diagram$notes <- c(diagram$notes, "Green paths indicate suggested coefficients")
  if (length(moderators) > 0)
    diagram$notes <- c(diagram$notes, "Moderators main effects are not shown")
  if (length(independents) > 1)
    diagram$notes <- c(diagram$notes, "Covariances among IV are estimated but not shown")
  if (any(options$factors %in% unlist(c(options$mediatorsTerms, options$modelTerms))))
    diagram$notes <- c(diagram$notes,
      "Categorical independent variables are shown with one rectangle; their effects are estimated using contrast variables")
  diagram
}

# Rendering uses image state alone, including when jamovi reopens a saved file.
plot_model_diagram <- function(state) {
  nvariables <- length(state$labels)
  plot <- diagram::plotmat(
    state$paths, pos = state$pos, name = state$labels,
    box.size = .1 + (max(state$nmediators, state$nindependents) + 3)^-8,
    box.type = "rect", box.prop = .4, box.cex = .80 + nvariables^-2,
    curve = state$curves, endhead = FALSE, arr.type = "triangle",
    arr.length = 1 / (nvariables - 1), arr.pos = .7, arr.col = "gray",
    lcol = state$colors, box.lcol = state$bcolors)
  for (path in state$moderated)
    diag.plot_mod_arr(plot, path$from, path$to, path$moderator)
  diag.plot_mods(plot, state$moderators)
  invisible(plot)
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
    M[infos$nvars,stats::median(1:ni)]<-.15
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

