
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

sourcifyList<-function(option,def) {
  alist<-option$value
  test<-all(sapply(alist,function(a) a$type)==def)
  if (test)
    return("")
  paste0(option$name,"=c(",paste(sapply(alist,function(a) paste0(a$var,' = \"',a$type,'\"')),collapse=", "),")")
}

expand.formula<-function(aform) {
            af<-paste(aform[[2]],paste(attr(stats::terms(aform),"term.labels"),collapse = " + "),sep=" ~ ")
            af<-stats::as.formula(af)
            af
}

