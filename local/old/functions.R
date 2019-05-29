is.something<- function(x,...) UseMethod(".is.something")

.is.something.default<-function(obj) (!is.null(obj))

.is.something.list<-function(obj) (length(obj)>0)

.is.something.numeric<-function(obj) (length(obj)>0)

.is.something.character<-function(obj) (length(obj)>0)

.is.something.logical<-function(obj) !is.na(obj)



fill.if<-function(test,ifyes,ifnot) {
  
  if (test)
    return(ifyes)
  else 
    return(ifnot)
}


info<-function(what=NULL) {
  if (GAMLj_INFO) {
    if (!is.null(what))
        print(what)
  }
}

mark<-function(what=NULL,obj=NULL) {
  if (GAMLj_DEBUG) {
    if (!is.null(what))
      print(what)
    else print("you got here")
    
    if (!is.null(obj)) {
      print(obj)
      print("#### end ###")
    }
  }
}

c.real<-function(...) {
  obj <- c(...)
  obj[!is.null(obj)]
}


.afield<-function(aList,what) {
  aList[[what]]
}

lolapply<-function(listOfLists,tracer=seq_along(listOfLists),FUN=identity,...) {
  results<-list()
  for (i in seq_along(tracer)) {
    aList<-try(listOfLists[[i]],silent = T)
    if ("try-error" %in% class(aList))
      res<-NULL
    else
      res<-FUN(aList,...)
    results[[tracer[i]]]<-res
  }
  results 
}


list_field<-function(listOfLists,tracer,field) 
                lolapply(listOfLists,tracer,.afield,what=field)

flat_list<-function(aList) unique(unlist(aList))

flat_named_list<-function(aList,field) unique(unlist((sapply(aList, function(a) a[[field]]))))

concat<-function(a,b) {
  a[[length(a)+1]]<-b
  a
}

remove_a_from_b<-function(a,b) {
  
  .fun<-function(x,remove) {
    out=FALSE
    for (rem in remove)
      if (is.something(grep(rem,x,fixed = T)))
        out=TRUE
      if (out)
        NULL
      else
        x
  }
  if (!is.list(b))
    b<-as.list(b)
  rr<-rapply(b,.fun,how="list",remove=a)
  rr[!sapply(rr, is.null)]
  
}

findTerms<-function(what,terms,order=1) {
  if (!is.something(terms))
    return(FALSE)
  unlist(lapply(terms,function(a) {
    if (length(a)>=order)
       any(sapply(what,function(b) b %in% a))
    else
       FALSE
  }))
}

