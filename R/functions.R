
.setContent <- function(element, value) {
  if (requireNamespace("knitr", quietly = TRUE)) {
    progress <- knitr::opts_knit$get("progress")
    knitr::opts_knit$set(progress = FALSE)
    on.exit(knitr::opts_knit$set(progress = progress))
  }
  element$setContent(value)
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

sourcifyList<-function(option,def) {
  alist<-option$value
  test<-all(sapply(alist,function(a) a$type)==def)
  if (test)
    return("")
  paste0(option$name,"=c(",paste(sapply(alist,function(a) paste0(a$var,' = \"',a$type,'\"')),collapse=", "),")")
}

expand.formula<-function(aform) {
            if (!inherits(aform, "formula")) {
              if (!is.character(aform) || length(aform) != 1 || is.na(aform) || !nzchar(trimws(aform)))
                stop("Each list element must contain one equation as a string or formula object.", call. = FALSE)
              expressions <- parse(text = aform)
              if (length(expressions) != 1)
                stop("Each list element must contain exactly one equation. Put equations in separate list elements, or pass a single string separated by semicolons or newlines.", call. = FALSE)
            }
            aform<-stats::as.formula(aform)
            if (length(aform) != 3)
              stop("Each equation must have a dependent variable and predictors, for example 'y ~ x'.", call. = FALSE)
            af<-paste(aform[[2]],paste(attr(stats::terms(aform),"term.labels"),collapse = " + "),sep=" ~ ")
            af<-stats::as.formula(af)
            af
}

