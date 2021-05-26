### This class takes care of producing lavaan syntax with B64 and normal names and all tables with information about the estimation
### tables are `parameter table` in Yves Rosseel terminology (see lavaan manual)
### It takes care also of producing and checking constraints
### free parameters, indirect effects and the like. 
### it assumes that the estimation will be done on B64 names, and the results reported in plain names
### It assumes that all interactions term are computed as new variables with INTERACTION_SYMBOL as separator (in place of ":") in the 
### variable name. 
### It assumes that all factors are present in the data as K-1 new variables with appropriated constrast value as numeric variables. 
### each "dummy" variable in named `VAR{FACTOR_SYMBOL}k`` 
### Inherit from Dispatch, which provides $warnings and $errors mechanisms to store info. Requires the try_hard() function to catch errors
##  naming convention: all objects containing lavaan syntax, objects that are needed to build objects that can be passed directly
##                     to lavaan are named $lav_*.   $lav_* objects contains B64 variable names 
##                     All object containing tables to be passed to the results init tables are called $tab_*. 


Syntax <- R6::R6Class(
         "Syntax",
          class=FALSE, ## this and the next 
          cloneable=FALSE, ## should improve performance https://r6.r-lib.org/articles/Performance.html ###
          inherit = Dispatch,
          public=list(
              endogenous=NULL,
              endogenousTerms=NULL,
              lav_terms=NULL,
              lav_structure=NULL,
              tab_coefficients=NULL,
              tab_covariances=NULL,
              tab_intercepts=NULL,
              tab_defined=NULL,
              tab_r2=NULL,
              tab_info=NULL,
              structure=NULL,
              options=NULL,
              constraints=NULL,
              defined=NULL,
              hasInteractions=FALSE,
              interactions=list(),
              factorinfo=NULL,
              contrasts_names=NULL,
              indirect_names=NULL,
              direct_names=NULL,
              total_names=NULL,
              ols_r2=NULL,
              ols_mediators=NULL,
              ols_full=NULL,
              
              initialize=function(options,datamatic) {
                super$initialize(options=options,vars=unlist(c(options$dep,options$mediators,options$factors,options$covs)))
                self$contrasts_names<-datamatic$contrasts_names
                
                # here we force the mediation analysis models to look like a path model
                

                # here we prepare the variables. Factors are expanded to dummies and all variables are B64 named
                # this produce two lists of terms, in plain names self$lav_terms and in B64 private$.lav_terms
                self$endogenousTerms<-c(options$mediatorsTerms,list(options$modelTerms))
                self$endogenous<-unlist(c(options$mediators,options$dep))
                
                factorinfo<-sapply(self$options$factors,function(f) length(datamatic$factors_levels[[f]])-1 )
                self$factorinfo<-factorinfo
                self$lav_terms<-lapply(self$endogenousTerms, function(alist) private$.factorlist(alist,factorinfo))
                names(factorinfo)<-tob64(names(factorinfo))
                private$.lav_terms<-lapply(tob64(self$endogenousTerms), function(alist) private$.factorlist(alist,factorinfo))
                
                # check_* check the input options and produces tables and list with names
                ### prepare list of models for lavaan
                private$.check_models()
                ### check if there are interactions and warn if variables are not centered
                private$.check_interactions()
                ### check and build constraints and defined parameter lavaan directives
                private$.check_constraints()

                ## check and build indirect effect
                private$.indirect()
                private$.total()
                
                ### check and build lavaan directives to free covariances
                private$.check_varcov()
                
                ### here we update to build a lavaanify structure

                private$.make_structure()  
                private$.check_ols()
                private$.make_tables()  
                
                }, # here initialize ends
               models=function() {
                  lapply(seq_along(self$endogenousTerms), 
                       function(i) list(info="Model",
                                        value=as.character(jmvcore::constructFormula(dep=self$endogenous[i],
                                                                                     self$endogenousTerms[[i]])))
                )
              }
              
          ),   # End public
          active=list(
           ### inherited warnings and errors are overriden to check specific message to change, and then passed to super 
           warnings=function(obj) {
             if (missing(obj))
               return(private$.warnings)
             if (is.null(obj))
               return()
             if (is.null(obj$message))
                 return()
             check<-length(grep("fixed.x=FALSE",obj$message,fixed = T)>0) 
             if (check) 
               obj$message<-WARNS[["usercov"]]
               
             super$warnings<-obj
           },
           errors=function(obj) {
             
             if (missing(obj))
               return(private$.errors)
             if (is.null(obj))
               return()
             check<-length(grep("infinite or missing",obj,fixed = T)>0) 
             if (check) 
               super$errors<-ERRS[["noluck"]]
             
             super$errors<-obj
           }
           
          ),
          private=list(
            .lav_terms=NULL,
            .lav_structure=NULL,
            .lav_constraints=NULL,
            .lav_defined=NULL,
            .lav_models=NULL,
            .lav_indirect=NULL,
            .lav_total=NULL,
            .lav_varcov=NULL,
            

            ### collapse the informations in the private lists of terms and constraints and produce a lavaan syntax string
            .lavaan_syntax=function() {
                  models<-private$.lav_models
                  f<-glue::glue_collapse(unlist(models),sep = " ; ")
                  con<-paste(private$.lav_constraints,collapse = " ; ")
                  f<-paste(f,con,sep=" ; ")
                  est<-paste(private$.lav_defined,collapse = " ; ")
                  f<-paste(f,est,sep=" ; ")
                  if (is.something(private$.lav_indirect)) {
                        f<-paste(f,";")
                        f<-paste(f,private$.lav_indirect,collapse = " ; ")
                  }
                  if (is.something(private$.lav_total)) {
                    f<-paste(f,";")
                    f<-paste(f,private$.lav_total,collapse = " ; ")
                  }
                  if (is.something(private$.lav_varcov)) {
                    f<-paste(f,";")
                    f<-paste(f,private$.lav_varcov,collapse = " ; ")
                  }
                  
                  f
            },
            ## lavaanify the information available to obtain a raw (B64) table representing the parameters structure
            ## parameter structure means their names, labels, 

            .make_structure=function() {
              
              lavoptions<-list(
                model=private$.lavaan_syntax(),
                int.ov.free = TRUE, 
                auto.var = TRUE,
                auto.th = TRUE, 
                auto.cov.y = TRUE,
                fixed.x=TRUE,
                meanstructure=TRUE
              )

              results<-try_hard({
                do.call(lavaan::lavaanify, lavoptions)
              })
              self$warnings<-list(topic="main",message=results$warning)
              self$errors<-results$error
              if (is.something(self$errors))
                stop(paste(self$errors,collapse = "\n"))
              
              ## raw (B64ed) structure of results gos in .lav_structure. Here are all parameters to be estimated, with info regarding
              ## their nature (exogenous, endogenous, coefficient vs variances, etc), labels and names 
              
              private$.lav_structure<-results$obj
              ## create easy labels to be used by the user in constraints and defined parameters  
              private$.lav_structure$label<-gsub(".","",private$.lav_structure$plabel,fixed=T)
              
            },            

            ### here we create the tables we need for init results. Those tables contains the information needed to init the results tables
            ### ideally, there should be one publich table for each results table, but a few table cannot be defined before
            ### estimation, so here are missing
            
            .make_tables=function() {
              

              ## now we start the filling of the tables to be used in the init of results
              .lav_structure<-private$.lav_structure
                  ## translate names
                  .lav_structure$lhs<-fromb64(.lav_structure$lhs,self$vars)
                  .lav_structure$rhs<-fromb64(.lav_structure$rhs,self$vars)

              ### self$structure containts all parameters with plain names. Useful for children to refer to parameters properties
              ### is not a tab_* which will be displayed in results
                  
              sel<-grep("==|<|>",.lav_structure$op,invert = T)
              self$structure<-.lav_structure[sel,]

              
              ### tab_coefficients contains all direct, indirect and component coefficients
              
              inames<-names(self$indirect_names)
              self$tab_coefficients<-list()
              
              for (i in seq_along(self$indirect_names)) {
                arow<-.lav_structure[.lav_structure$rhs==inames[i],]
                arow$type<-"Indirect"
                arow$effect<-self$indirect_names[[i]]
                self$tab_coefficients[[length(self$tab_coefficients)+1]]<-arow
                if ("component" %in% self$options$tableOptions) {
                  comps<-stringr::str_split(inames[i],"\\*")[[1]]
                  rows<-.lav_structure[.lav_structure$label %in% comps,]
                  for (j in 1:nrow(rows)) {
                    arow<-rows[j,]
                    arow$type<-"Component"
                    arow$effect<-paste(arow$rhs," \U21d2 ",arow$lhs)
                    self$tab_coefficients[[length(self$tab_coefficients)+1]]<-arow
                  }
                }
              }
              dnames<-names(self$direct_names)
              for (i in seq_along(self$direct_names)) {
                arow<-.lav_structure[.lav_structure$label==dnames[i],]
                arow$type<-"Direct"
                arow$effect<-self$direct_names[[i]]
                self$tab_coefficients[[length(self$tab_coefficients)+1]]<-arow
              }
              
              tnames<-names(self$total_names)
              for (i in seq_along(self$total_names)) {
                arow<-.lav_structure[.lav_structure$rhs==tnames[i],]
                arow$type<-"Total"
                arow$effect<-self$total_names[[i]]
                self$tab_coefficients[[length(self$tab_coefficients)+1]]<-arow
                }
              
              
              
              ### tab_r2 contains the r-squares for endogenous variables
              r2test<-((.lav_structure$op=="~~") & (.lav_structure$lhs %in% self$endogenous) & (.lav_structure$lhs==.lav_structure$rhs))
              self$tab_r2<-.lav_structure[r2test,c("lhs")]

              ### tab_covariances contains variances and covariances
              self$tab_covariances<-.lav_structure[.lav_structure$op=="~~",]
              
              ### intercepts table
              self$tab_intercepts<-.lav_structure[.lav_structure$op=="~1",]
              if (nrow(self$tab_intercepts)==0) self$tab_intercepts<-NULL
              
              ### info contains the info table, with some loose information about the model
              alist<-list()
              alist[[length(alist)+1]]<-c(info="Estimation Method",value="ML")
              alist[[length(alist)+1]]<-c(info="Number of observations",value="") 
              alist[[length(alist)+1]]<-c(info="Free parameters",value=max(.lav_structure$free))
              alist[[length(alist)+1]]<-c(info="Converged","") 
              alist[[length(alist)+1]]<-c(info="",value="")
              alist[[length(alist)+1]]<-c(info="Loglikelihood user model",value="" )
              alist[[length(alist)+1]]<-c(info="Loglikelihood unrestricted model",value="")
              alist[[length(alist)+1]]<-c(info="",value="")
              
              self$tab_info<-alist
              

            },
            .check_models=function() {
              
              terms<-private$.lav_terms
              endogenous<-tob64(self$endogenous)
              
              models<-lapply(seq_along(terms), function(i)
                jmvcore::constructFormula(dep=endogenous[i],terms[[i]]))
              
              private$.lav_models<-lapply(models,function(m) {
                res<-gsub(":",INTERACTION_SYMBOL,m)
                if (res!=m) {
                  self$hasInteractions=TRUE
                  int<-strsplit(res,"+",fixed = T)[[1]]

                  ind<-grep(INTERACTION_SYMBOL,int,fixed = TRUE)
                  for (j in ind)
                    self$interactions[[length(self$interactions)+1]]<-trimws(int[j])
                }
                res
              })
            
            },
            
            
            .check_constraints=function() {
              
              ### nothing for the moment

            },
            .check_interactions=function() {

                            noscale<-unlist(sapply(self$options$scaling , function(a) if (a$type=="none") return(a$var)))
                            res<-lapply(seq_along(self$endogenousTerms), function(i) {
                                    sapply(self$endogenousTerms[[1]],function(term) {
                                          if (length(term)>1)
                                                return(intersect(term,noscale))
                                          else return(NULL)
                                    })
                            })
                            res<-unique(unlist(res))
                            if (is.something(res)) {
                                    w<-glue::glue(WARNS[["nocenterd"]],vars=paste(res,collapse = ", "))
                                   self$warnings<-list(topic="main",message=w)
                            }
                            

            },
            .check_varcov=function() {
              
              # not used at the moment
              return()
              
              ## here we check for possible correlations among parallel mediators
              med<-tob64(self$options$mediators)
              if (length(med)==1)
                   return()
              medpairs<-ordered_pairs(med)
              tab<-private$.lav_structure[private$.lav_structure$op=="~",]
              tab<-tab[tab$lhs %in% med & tab$rhs %in% med,c("lhs","rhs")] 
              cov<-remove_pair(medpairs,tab)
              synt<-lapply(cov, paste,collapse="~~")
              mark(synt)
              private$.lav_varcov<-NULL


            },
            .factorlist=function(terms,factorslen) {
              .terms<-list()
              for (f in names(factorslen)) {
                for (term in terms) {
                  ind<-which(term==f)
                  for (i in ind) {
                    for (j in seq_len(factorslen[[f]])) {
                      .term<-term
                      .term[[i]]<-paste0(trimws(.term[[i]]),FACTOR_SYMBOL,j)
                      .terms[[length(.terms)+1]]<-.term
                    }
                  }
                  if (length(ind)==0)
                    .terms[[length(.terms)+1]]<-trimws(term)
                }
                terms<-.terms
              }
              terms  
            },
            .indirect=function() {

              ## first, we update the lavaanified structure table
              private$.make_structure()
              tab<-private$.lav_structure
              termslist<-list()
              labslist<-list()
              groupslist<-list()
              sel<-grep(":",tab$rhs,fixed = T,invert=T) 
              tab<-tab[sel,]
              sel<-tab$op=="~" 
              tab<-tab[sel,]
              tab<-tab[tab$group>0,]
              
              ## recursive function to extact indirect effects.
              .doit<-function(tab,term,alist=list(),blist=list(),lab=NULL) {
                alist<-c(alist,term)
                blist<-c(blist,lab)
                if (term %in% deps) {
                  final<-unlist(alist)
                  if (length(final)>2) {
                    termslist[[length(termslist)+1]]<<-final
                    labslist[[length(labslist)+1]]<<-unlist(blist)
                    groupslist[[length(groupslist)+1]]<<-tab$group[1]
                    
                  }
                  return()
                }
                a<-tab[tab$rhs==term,]
                if (length(a$lhs)==0)
                  return()
                for (i in 1:nrow(a)) {
                  x<-a$lhs[i]
                  lab<-a$label[i]
                  .doit(tab,x,alist,blist,lab)
                }
                
              }
              
              terms<-setdiff(tab$rhs,tab$lhs)
              deps<-unique(setdiff(tab$lhs,tab$rhs))

              if (length(deps)==0) {
                self$warnings<-list(topic="defined",message=WARNS[["noindirect"]])
                return()
              }
              tabs<-list()
              for (i in tab$group) 
                tabs[[i]]<-tab[tab$group==i,]
              
              results<-list()
              ### we run the recursive function for each group and for each rhs term
              for (i in seq_along(tabs)) {
                .results<-try_hard({
                  for (tt in terms)
                    .doit(tabs[[i]],term=tt)
                })
                if (.results$error!=FALSE)
                    self$warnings<-list(topic="defined",message=WARNS[["noindirect"]])
                results[[i]]<-.results
              }
              pars<-sapply(labslist,paste,collapse="*")
              
              if (!is.something(pars))
                return()
              
#              termslist<-termslist[order(sapply(termslist,length),decreasing = T)]
              labs<-sapply(termslist,paste,collapse=" \U21d2 ")
              plabs<-paste0("IE",1:length(pars))
              synt<-paste(plabs,pars,sep=":=",collapse = " ; ")
              private$.lav_indirect<-synt
              self$indirect_names<-as.list(fromb64(labs,self$vars))
              names(self$indirect_names)<-pars
            },
            
            .total=function() {
              ## first, we update the lavaanified structure table
              private$.make_structure()
              tab<-private$.lav_structure

              inds<-stringr::str_split(self$indirect_names,"\U21d2")
              inames<-names(self$indirect_names)
              totals<-list()
              total_names<-list()
              direct_names<-list()
              for (i in seq_along(inds)) {
                ind<-inds[[i]]
                rhs<-tob64(trimws(ind[1]))
                lhs<-tob64(trimws(ind[length(ind)]))
                lab<-tab$label[tab$rhs==rhs & tab$lhs==lhs]
                if (lab %in% names(totals)) totals[[lab]]<-c(totals[[lab]],inames[i])
                else {
                    totals[[lab]]<-c(lab,inames[i])
                    total_names[[lab]]<-paste0(ind[1],"\U21d2",ind[length(ind)])
                    direct_names[[lab]]<-paste0(ind[1],"\U21d2",ind[length(ind)])
                }
              }
              plabs<-paste0("TOT",1:length(totals))
              totals<-sapply(totals, function(a) paste(a,collapse ="+"))
              synt<-paste(plabs,totals,sep=":=",collapse = " ; ")
              private$.lav_total<-synt
              self$total_names<-total_names
              names(self$total_names)<-totals
              self$direct_names<-direct_names
            },
            .check_ols=function() {
              
              if (!("regression" %in% self$options$tableOptions))
                 return()
              deps<-unlist(c(self$options$mediators,self$options$dep))
              
              self$ols_total<-lapply(deps, function(x) {
                  x
              })
              names(self$ols_mediators)<-deps
              
              ols_med1<-lapply(self$options$mediators, function(x) {
                x
              })
              names(self$ols_mediators)<-self$options$mediators
              
              ols_med2<-lapply(self$options$mediatorsTerms, function(x) {
                 x
              })
              names(self$ols_mediators)<-self$options$mediators
              
              self$ols_full<-self$options$modelTerms

            }
            
            
            
          ) # end of private
) # End Rclass

