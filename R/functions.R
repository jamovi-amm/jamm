

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



c.real<-function(...) {
  obj <- c(...)
  obj[!is.null(obj)]
}


.afield<-function(aList,what) {
  aList[[what]]
}

### apply for list of lists
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

sourcifyList<-function(option,def) {
  alist<-option$value
  test<-all(sapply(alist,function(a) a$type)==def)
  if (test)
    return("")
  paste0(option$name,"=c(",paste(sapply(alist,function(a) paste0(a$var,' = \"',a$type,'\"')),collapse=", "),")")
}



####### models and formuals #########

expand.formula<-function(aform) {
            af<-paste(aform[[2]],paste(attr(terms(aform),"term.labels"),collapse = " + "),sep=" ~ ")
            af<-as.formula(af)
            af
}


#### This function run an expression and returns any warnings or errors without stopping the execution.
try_hard<-function(exp,max_warn=5) {
  
  .results<-list(error=FALSE,warning=list(),message=FALSE,obj=FALSE)
  
  .results$obj <- withCallingHandlers(
    tryCatch(exp, error=function(e) {
      mark("SOURCE:")
      mark(conditionCall(e))
      .results$error<<-conditionMessage(e)
      NULL
    }), warning=function(w) {
      
      if (length(.results$warning)==max_warn) 
        .results$warning[[length(.results$warning)+1]]<<-"Additional warnings are present."
      
      if (length(.results$warning)<max_warn)
        .results$warning[[length(.results$warning)+1]]<<-conditionMessage(w)
      
      invokeRestart("muffleWarning")
    }, message = function(m) {
      .results$message<<-conditionMessage(m)
      invokeRestart("muffleMessage")
    })
  
  
  if (!isFALSE(.results$error)) {
    mark("CALLER:")
    mark(rlang::enquo(exp))
    mark("ERROR:")
    mark(.results$error)
  }
  if(length(.results$warning)==0) .results$warning<-FALSE
  if(length(.results$warning)==1) .results$warning<-.results$warning[[1]]
  
  
  return(.results)
}

