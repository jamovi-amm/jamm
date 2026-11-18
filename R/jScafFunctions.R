j_DEBUG = FALSE
j_INFO = FALSE
t_INFO  <- FALSE


#### Helper functions used by Scaffold (not exported)

tinfo <- function(...) {
  if (t_INFO) {
    cat(paste(list(...)))
    cat("\n")
  }
}


jinfo <- function(...) {
  if (j_INFO) {

    cat("\n")
    cat(paste(list(...)))
    cat("\n")
  }
}


mark <- function(...) {
  if (!j_DEBUG)
    return()
  

  if (missing(...)) cat("Mark here\n")
  
  items<-list(...)
  
  if (length(items)>1)  cat("______begin________\n\n")
  
  for (a in items)
    if (is.character(a))
      cat(a,"\n")
  else
    print(a)
  
  if (length(items)>1)  cat("_____end_______\n\n")
  
}

# Returns FALSE for "empty" values: NULL, zero-length vectors and lists, NA.
# Chooses the rule with plain tests rather than UseMethod(".is.something"): a
# generic whose method prefix (".is.something") differs from its own name only
# dispatches when called by name, and fails with "no applicable method" from the
# console or when passed as a function (Filter(is.something, x)).
# Results are identical to the former S3 methods, including two quirks: a logical
# vector gives one value per element, and objects with their own class (factor,
# data.frame, formula, R6, S4, ...) always count as something.
is.something <- function(x, ...) {
  if (is.null(x))
    return(FALSE)
  if (is.object(x) && !inherits(x, c("list", "numeric", "integer", "character", "logical")))
    return(TRUE)
  if (is.logical(x))
    return(!is.na(x))
  if (is.list(x) || is.numeric(x) || is.character(x))
    return(length(x) > 0)
  TRUE
}

is.there<-function(pattern,string) length(grep(pattern,string,fixed=T))>0

#### This function run an expression and returns any warnings or errors without stopping the execution.
try_hard<-function(exp,max_warn=5, silent=FALSE) {
  
  .results<-list(error=FALSE,warning=list(),message=FALSE,obj=FALSE)
  
  .results$obj <- withCallingHandlers(
    tryCatch(exp, error=function(e) {
      if (!silent) mark("SOURCE:")
      if (!silent) mark(conditionCall(e))
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
  
  
  if (!isFALSE(.results$error) && isFALSE(silent)) {
    mark("CALLER:")
    mark(rlang::enquo(exp))
    mark("ERROR:")
    mark(.results$error)
  }
  if(length(.results$warning)==0) .results$warning<-FALSE
  if(length(.results$warning)==1) .results$warning<-.results$warning[[1]]
  
  
  return(.results)
}



sourcifyOption<- function(x,...) UseMethod(".sourcifyOption")

.sourcifyOption.default=function(option,def=NULL) {
  
  if (option$name == 'data')
    return('data = data')
  
  if (startsWith(option$name, 'results/'))
    return('')
  
  value <- option$value
  def <- option$default
  
  if ( ! ((is.numeric(value) && isTRUE(all.equal(value, def))) || base::identical(value, def))) {
    valueAsSource <- option$valueAsSource
    if ( ! identical(valueAsSource, ''))
      return(paste0(option$name, ' = ', valueAsSource))
  }
  ''
}
.sourcifyOption.OptionVariables<-function(option,def=NULL) {
  
  if (is.null(option$value))
     return('')
  
  values<-sourcifyName(option$value)
  
  if (length(values)==1)
     return(paste0(option$name,"=",values))
  else
    return(paste0(option$name,"=c(",paste0(values,collapse = ","),")"))
}
  
.sourcifyOption.OptionTerms<-function(option,def=NULL)
     .sourcifyOption.default(option,def)
  
.sourcifyOption.OptionArray<-function(option,def=NULL) {
  
  alist<-option$value
  if (length(alist)==0)
    return('')
  if (is.something(def) & option$name %in% names(def)) {
    test<-all(sapply(alist,function(a) a$type)==def[[option$name]])
    if (test)
      return('')
  }
  test<-all(sapply(alist,function(a) ("var" %in% names(a) & "type" %in% names(a))))
  if (test)
    return(paste0(option$name,"=c(",paste(sapply(alist,function(a) paste0(sourcifyName(a$var),' = \"',a$type,'\"')),collapse=", "),")"))
  test<-all(sapply(alist,function(a) length(a)==0))
  if (test) 
     return('')
  return(.sourcifyOption.default(option, def))
}


.sourcifyOption.OptionList<-function(option,def=NULL) {
  
  if (length(option$value)==0)
    return('')
  if (option$value==option$default)
       return('')
  paste0(option$name,"='",option$value,"'")
}


sourcifyName<-function(name) {
  
  what<-which(make.names(name)!=name)
  for (i in what)
    name[[i]]<-paste0('"',name[[i]],'"')
  name
}

sourcifyVars<-function(value) {
  
  paste0(sourcifyName(value),collapse = ",")
  
}

listify <- function(adata) {
  res <- lapply(1:dim(adata)[1], function(a) as.list(adata[a, ]))
  names(res) <- rownames(adata)
  res
}            

smartTableName<-function(root,alist,end=NULL) {
    paste(root,make.names(paste(alist,collapse = ".")),end,sep="_")
}


transnames<-function(original,ref) {
  unlist(lapply(original,function(x) {
    i<-names(ref)[sapply(ref,function(y) any(y %in% trimws(x)))]
    ifelse(length(i)>0,i,x)
  }))
}

is.listOfList<-function(obj) {
  if (length(obj)==0)
     return(FALSE)
   
  if (inherits(obj,"list")) {
    child<-obj[[1]]
    return(inherits(obj,"list"))
  }
  return(FALSE)
}

ebind<-function(...) {
  tabs<-list(...)
  .names<-unique(unlist(sapply(tabs,colnames)))
  tabs<-lapply(tabs, function(atab) {
    atab<-as.data.frame(atab)
    for (name in .names)
      if (!utils::hasName(atab,name))
        atab[[name]]<-NA
    atab
  })
  return(do.call(rbind,tabs))
  
}

ebind_square<-function(...) {
  tabs<-list(...)
  .names<-unique(unlist(sapply(tabs,colnames)))
  .max<-max(unlist(sapply(tabs,dim)))
 
  tabs<-lapply(tabs, function(atab) {
    atab<-as.data.frame(atab)
    for (name in .names) 
      if (!utils::hasName(atab,name))
        atab[[name]]<-NA
    if (dim(atab)[1]<.max)
        atab[(dim(atab)[1]+1):.max,]<-NA
    atab
  })
  return(do.call(rbind,tabs))
  
}





#########

# remove null from list of lists
clean_lol<-function(alist) {
  il<-list()
  for (i in seq_along(alist)) {
    jl<-list()
    for (j in seq_along(alist[[i]])) {
      if (length(alist[[i]][[j]])>0) jl[[length(jl)+1]]<-alist[[i]][[j]]
    }
    if (length(jl)>0) il[[length(il)+1]]<-jl
  }
  il
}

### syntax functions

`%+%`<-function(a,b) {
  paste0(a,b)
}

`%<+%`<-function(a,b) {
  sprintf(a, b)   
}


`ladd<-`<-function(x,value) {
  x[[length(x)+1]]<-value
  return(x)
}

`padd<-` <- function(x, value) {
  x <- c(0, x)
  x[[1]] <- value
  x
}


is.joption <- function(obj, option) {
  (option %in% obj$names)
}

###########

### formatting and stuff ###

quote_name <- function(value) {
  encodeString(value, quote = '"')
}

format5<-function(x) format(x,digits=5)


check_parameters <- function(values, fun=is.null, verbose = TRUE, head="Please fill in the required input:") {
  
      needed <- names(values)
      what   <- unlist(sapply(values,fun))
      needed <- needed[what]
      
      if (length(needed)>0) {
            if (verbose) {
            text <- "<p>" %+% head %+% "</p> <ul>" 
            for (ned in needed) text <- text %+% "<li>" %+% ned %+% "</li>" 
            text <-  text %+% "</ul>"
            return(text)
            } else 
                return(needed)
      }
      return()
}

test_parameters <- function(obj,values, fun=is.null, verbose = TRUE, head="Please fill in the required input:") {
  
  test<-check_parameters(values,fun=fun,verbose=verbose,head=head) 
 
  if (length(test)>0) {
     obj$ok <-FALSE
     obj$warning<-list(topic="issues",message=paste(test,collapse=", "), head="info") 
     call <- rlang::expr(return()) 
     rlang::eval_bare(call, env = parent.frame())
     
  }
}

## Escape user-supplied labels before using them as regular expressions.
escape_regex <- function(x) {
  special <- c("\\", ".", "|", "(", ")", "[", "]", "{", "}", "^", "$", "*", "+", "?")
  for (char in special)
    x <- gsub(char, paste0("\\", char), x, fixed=TRUE)
  x
}

