is.something<- function(x,...) UseMethod(".is.something")

.is.something.default<-function(obj) (!is.null(obj))

.is.something.list<-function(obj) (length(obj)>0)

.is.something.numeric<-function(obj) (length(obj)>0)

.is.something.character<-function(obj) (length(obj)>0)

.is.something.logical<-function(obj) !is.na(obj)


lgrep<-function(...) {
  
  if (length(grep(...))>0)
    TRUE
  else
    FALSE
  
}



ginfo<-function(what=NULL,obj=NULL) {
  if (j_INFO) {
    if (!is.null(what))
      print(what)
    if (!is.null(obj)) {
      print(obj)
      cat("------------\n")
    }
  }
}

wmark<-function(what=NULL,obj=NULL) {
  sink("C://Users/marcello/Documents/debug.log",append = T)
  
  if (j_DEBUG) {
    if (!is.null(what))
      print(what)
    else print("you got here")
    
    if (!is.null(obj)) {
      print(obj)
      print("#### end ###")
    }
  }
  sink()
}

mark<-function(what=NULL,obj=NULL) {

  if (j_DEBUG) {
    if (!is.null(what))
       print(what)
    else print("you got here")
    
    if (!is.null(obj)) {
       print(obj)
      print("#### end ###")
    }
  }
  
}


sourcifyList<-function(option,def) {
  alist<-option$value
  test<-all(sapply(alist,function(a) a$type)==def)
  if (test)
    return("")
  paste0(option$name,"=c(",paste(sapply(alist,function(a) paste0(a$var,' = \"',a$type,'\"')),collapse=", "),")")
}


#### This function run an expression and returns any warnings or errors without stopping the execution.
#### It does not reterun the results, so the expr should assign a valut to the results
#### something like try_hard({a<-3^2}) and not a<-try_hard(3^2)

try_hard<-function(exp) {
  results<-list(error=FALSE,warning=FALSE,message=FALSE,obj=FALSE)
  
  results$obj <- withCallingHandlers(
    tryCatch(exp, error=function(e) {
      results$error<<-conditionMessage(e)
      NULL
    }), warning=function(w) {
      results$warning<<-conditionMessage(w)
      invokeRestart("muffleWarning")
    }, message = function(m) {
      results$message<<-conditionMessage(m)
      invokeRestart("muffleMessage")
    })
  
  return(results)
}


####### models and formuals #########

ordered_pairs<-function(alist) {
  many<-expand.grid(alist,alist,stringsAsFactors = F)
  different<-many[many$Var1!=many$Var2,]
  ordered<-lapply(1:nrow(different), function(i) {
    x<-as.character(different[i,])
    x[order(x)]
  } )
   unique(ordered)
}

remove_pair<-function(base,old) {
  
  if (inherits(old,"data.frame"))
    old<-lapply(1:nrow(old), function(i) as.character(old[i,]))
  
  if (inherits(base,"data.frame"))
    base<-lapply(1:nrow(base), function(i) as.character(base[i,]))
  base[sapply(base,function(b) {
    b<-b[order(b)]
    !any(sapply(old,function(x) all(x==b)))
  })]
  
}



expand.formula<-function(aform) {
            af<-paste(aform[[2]],paste(attr(stats::terms(aform),"term.labels"),collapse = " + "),sep=" ~ ")
            af<-stats::as.formula(af)
            af
}

