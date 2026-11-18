### This class takes care of producing lavaan syntax with B64 and normal names and all tables with information about the estimation
### tables are `parameter table` in Yves Rosseel terminology (see lavaan manual)
### It takes care also of producing and checking constraints
### free parameters, indirect effects and the like. 
### it assumes that the estimation will be done on B64 names, and the results reported in plain names
### It assumes that all interactions term are computed as new variables with INTERACTION_SYMBOL as separator (in place of ":") in the 
### variable name. 
### It assumes that all factors are present in the data as K-1 new variables with appropriated constrast value as numeric variables. 
### each "dummy" variable in named `VAR{FACTOR_SYMBOL}k`` 
##  naming convention: all objects containing lavaan syntax, objects that are needed to build objects that can be passed directly
##                     to lavaan are named $lav_*.   $lav_* objects contains B64 variable names 
##                     All object containing tables to be passed to the results init tables are called $tab_*. 


Syntax <- R6::R6Class(
         "Syntax",
          class=FALSE, ## this and the next 
          cloneable=FALSE, ## should improve performance https://r6.r-lib.org/articles/Performance.html ###
          inherit = Scaffold,
          public=list(
              vars=NULL,
              endogenous=NULL,
              endogenousTerms=NULL,
              moderators=NULL,
              lav_terms=NULL,
              lav_structure=NULL,
              tab_coefficients=NULL,
              tab_covariances=NULL,
              tab_indirect=NULL,
              tab_defined=NULL,
              tab_r2=NULL,
              tab_info=NULL,
              tab_interactions=NULL,
              tab_moderators_main=NULL,
              structure=NULL,
              options=NULL,
              constraints=NULL,
              defined=NULL,
              hasInteractions=FALSE,
              interactions=list(),
              factorinfo=NULL,
              contrasts_names=NULL,
              indirect_names=NULL,
              # one entry per X -> Y pair with indirect effects, in the order of the
              # totals: list(name = "X⇒Y", label = lavaan label of the direct path,
              # or NULL when the user left the path out of the full model)
              direct_effects=NULL,
              total_names=NULL,
              
              initialize=function(jmvobj,datamatic) {
                super$initialize(jmvobj)
                options<-self$options
                self$vars<-datamatic$vars
                private$.conditional_datamatic <- datamatic
                self$contrasts_names<-datamatic$contrasts_names
                self$moderators<-unlist(options$mod_terms)
                
                # here we force the mediation analysis models to look like a path model
                

                # here we prepare the variables. Factors are expanded to dummies and all variables are B64 named
                # this produce two lists of terms, in plain names self$lav_terms and in B64 private$.lav_terms
                self$endogenousTerms<-c(options$mediatorsTerms,list(options$modelTerms))
                self$endogenous<-unlist(c(options$mediators,options$dep))
                
                factorinfo<-sapply(self$options$factors,function(f) length(datamatic$factors_levels[[f]])-1 )
                self$factorinfo<-factorinfo
                self$lav_terms<-lapply(self$endogenousTerms, function(alist) private$.factorlist(alist,factorinfo))
                names(factorinfo)<-tob64(names(factorinfo))
                private$.factorinfo64<-factorinfo
                private$.lav_terms<-lapply(tob64(self$endogenousTerms), function(alist) private$.factorlist(alist,factorinfo))
                private$.lav_moderators<-unlist(lapply(tob64(self$moderators), function(alist) private$.factorlist(alist,factorinfo)))
                
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
                
                ### here we update to build a lavaanify structure

                private$.make_structure()
                private$.make_tables()
                self$prepareConditional()
                
                }, # here initialize ends
               # Initialization supplies row labels; estimation replaces placeholder
               # moderator values with values computed from complete-case data.
               prepareConditional=function(data = NULL) {
                 if (length(self$moderators) == 0)
                   return(invisible(NULL))
                 private$.conditional_effects <- conditional_effects(
                   self$options, private$.conditional_datamatic,
                   private$.lav_structure, private$.indirect_names, data)
                 if (length(private$.conditional_effects) == 0)
                   return(invisible(NULL))
                 # Direct effects fixed to 0 by the model are not lavaan parameters.
                 estimated <- Filter(function(effect) !isTRUE(effect$fixed), private$.conditional_effects)
                 private$.lav_defined <- vapply(estimated, function(effect) {
                   paste(effect$parameter, ":=", effect$expression)
                 }, character(1))
                 private$.make_structure()
                 private$.make_tables()
               },
               models=function() {
                  lapply(seq_along(self$endogenousTerms),
                       function(i) list(info="Model",
                                        value=as.character(jmvcore::constructFormula(dep=self$endogenous[i],
                                                                                     self$endogenousTerms[[i]])))
                )
              },
              ### builds a `dep ~ terms` formula string using the actual B64-named, factor-dummy-expanded
              ### columns that exist in the cleaned data (same convention as private$.lav_terms), so callers
              ### (e.g. regressions.R) can fit plain lm() models consistently with the main mediation model.
              modelFormula64=function(dep,terms) {
                dep64<-tob64(dep)
                terms64<-self$expandTerms(terms,b64=TRUE)
                jmvcore::composeFormula(dep64,terms64)
              },
              ### expands a list of (plain-name) terms so that any term mentioning a factor becomes
              ### one term per contrast dummy (matching how many rows a fitted lm() will actually produce).
              ### b64=TRUE additionally B64-encodes everything, giving the exact column names present
              ### in the cleaned data (same convention as private$.lav_terms / Datamatic$cleandata()).
              expandTerms=function(terms,b64=FALSE) {
                if (b64)
                  terms<-lapply(tob64(terms), function(t) private$.factorlist(list(t),private$.factorinfo64))
                else
                  terms<-lapply(terms, function(t) private$.factorlist(list(t),self$factorinfo))
                unlist(terms,recursive=FALSE)
              }

          ),   # End public
          active=list(
           warning=function(obj) {
             if (is.null(obj))
               return()
             if (is.null(obj$message))
                 return()
             obj$message<-lapply(obj$message,function(message) {
               if (isFALSE(message)) return(FALSE)
               fromb64(message)
             })
               
             super$warning<-obj
           }
           
          ),
          private=list(
            .conditional_datamatic=NULL,
            .conditional_effects=list(),
            .lav_terms=NULL,
            .lav_structure=NULL,
            .lav_constraints=NULL,
            .lav_defined=NULL,
            .lav_models=NULL,
            .lav_indirect=NULL,
            .lav_total=NULL,
            .lav_moderators=NULL,
            .indirect_names=NULL,
            .factorinfo64=NULL,


            ### collapse the informations in the private lists of terms and constraints and produce a lavaan syntax string
            .lavaan_syntax=function() {
                  models<-private$.lav_models
                  f<-paste(unlist(models),collapse = " ; ")
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
              self$warning<-list(topic="jamovi",message=results$warning)
              if (!isFALSE(results$error))
                stop(fromb64(results$error),call.=FALSE)
              
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
              
              # deal with interactions
              if (is.something(self$moderators)) {
                sel<-grep(INTERACTION_SYMBOL,.lav_structure$rhs,fixed = TRUE)
                self$tab_interactions<-.lav_structure[sel,]
                self$tab_interactions<-self$tab_interactions[self$tab_interactions$op=="~",]
                self$tab_interactions$lhs<-fromb64(self$tab_interactions$lhs)
                self$tab_interactions$rhs<-fromb64(self$tab_interactions$rhs)
                # Moderator factors expand into several contrast columns.
                # Match all their main effects, not just the first grep pattern.
                sel<-!(.lav_structure$rhs %in% private$.lav_moderators)
                .lav_structure<-.lav_structure[sel,]

              }
              

                  ## translate names
                  .lav_structure$lhs<-fromb64(.lav_structure$lhs)
                  .lav_structure$rhs<-fromb64(.lav_structure$rhs)

              ### self$structure containts all parameters with plain names. Useful for children to refer to parameters properties
              ### is not a tab_* which will be displayed in results
                  
              sel<-grep("==|<|>",.lav_structure$op,invert = T)
              self$structure<-.lav_structure[sel,]

              
              ### tab_coefficients contains all direct, indirect and component coefficients
              
              inames<-names(self$indirect_names)
              self$tab_coefficients<-list()
              
              # Indirect and total effects are found by their parameter names
              # (IE1, IE2, ... from .indirect(); TOT1, TOT2, ... from .total(), in
              # the same order), not by their expression: lavaan expands a total
              # made of a single indirect effect (TOT1 := IE1, full mediation)
              # into the same expression as that indirect effect.
              for (i in seq_along(self$indirect_names)) {
                arow<-.lav_structure[.lav_structure$op==":=" & .lav_structure$lhs==paste0("IE",i),]
                arow$type<-"Indirect"
                arow$effect<-self$indirect_names[[i]]
                # The Contrast column: the comparison of the factor contrast the
                # effect starts from, empty for continuous variables.
                start<-private$.indirect_names[[i]][1]
                arow$label<-private$.comparison(start)
                self$tab_coefficients[[length(self$tab_coefficients)+1]]<-arow
                if ("component" %in% self$options$tableOptions) {
                  comps<-stringr::str_split(inames[i],"\\*")[[1]]
                  rows<-.lav_structure[.lav_structure$label %in% comps,]
                  for (j in 1:nrow(rows)) {
                    arow<-rows[j,]
                    arow$type<-"Component"
                    arow$effect<-paste(arow$rhs,arow$lhs,sep=" \U21d2 ")
                    # only the path leaving the contrast variable is a comparison
                    arow$label<-if (identical(arow$rhs,fromb64(start))) private$.comparison(start) else ""
                    self$tab_coefficients[[length(self$tab_coefficients)+1]]<-arow
                  }
                }
              }
              for (direct in self$direct_effects) {
                if (is.null(direct$label)) {
                  # Not in the full model: shown as fixed to 0 (see the note set in .total()).
                  arow<-private$.fixed_direct_row(direct$name, contrast=direct$contrast)
                } else {
                  arow<-.lav_structure[.lav_structure$label==direct$label,]
                  arow$type<-"Direct"
                  arow$effect<-direct$name
                  arow$label<-direct$contrast
                }
                self$tab_coefficients[[length(self$tab_coefficients)+1]]<-arow
              }
              
              for (i in seq_along(self$total_names)) {
                arow<-.lav_structure[.lav_structure$op==":=" & .lav_structure$lhs==paste0("TOT",i),]
                arow$type<-"Total"
                arow$effect<-self$total_names[[i]]
                # totals are in the same order as direct_effects (see .total())
                arow$label<-self$direct_effects[[i]]$contrast
                self$tab_coefficients[[length(self$tab_coefficients)+1]]<-arow
                }

              # Conditional parameters are estimated by lavaan itself, so delta
              # method and bootstrap intervals include all path covariances.
              if (length(private$.conditional_effects) > 0) {
                self$tab_coefficients <- lapply(private$.conditional_effects, function(effect) {
                  if (isTRUE(effect$fixed))
                    return(private$.fixed_direct_row(effect$effect, effect$condition, effect$contrast))
                  row <- .lav_structure[.lav_structure$lhs == effect$parameter & .lav_structure$op == ":=", ]
                  row$type <- effect$type
                  row$effect <- effect$effect
                  row$condition <- effect$condition
                  row$label <- effect$contrast
                  if ("est" %in% names(row))
                    row$std.all <- row$est * effect$scale
                  row
                })
              }
              
              
              
              ### tab_r2 contains the r-squares for endogenous variables
              r2test<-((.lav_structure$op=="~~") & (.lav_structure$lhs %in% self$endogenous) & (.lav_structure$lhs==.lav_structure$rhs))
              self$tab_r2<-.lav_structure[r2test,c("lhs")]

              ### tab_covariances contains variances and covariances
              self$tab_covariances<-.lav_structure[.lav_structure$op=="~~",]
              

              ### info contains the info table, with some loose information about the model
              alist<-list()
              alist[[length(alist)+1]]<-c(info="Estimation Method",value="ML")
              alist[[length(alist)+1]]<-c(info="Number of observations",value="") 
              alist[[length(alist)+1]]<-c(info="Converged","") 
              alist[[length(alist)+1]]<-c(info="",value="")
              
              self$tab_info<-alist
              

            },
            .check_models=function() {
              
              terms<-private$.lav_terms
              endogenous<-tob64(self$endogenous)
              
              models<-lapply(seq_along(terms), function(i)
                jmvcore::constructFormula(dep=endogenous[i],terms[[i]]))
              
              interactions<-Filter(function(term) length(term)>1, unlist(terms,recursive=FALSE))
              self$interactions<-unique(lapply(interactions, function(term)
                paste(term,collapse=INTERACTION_SYMBOL)))
              self$hasInteractions<-length(self$interactions)>0
              private$.lav_models<-lapply(models,function(model)
                gsub(":",INTERACTION_SYMBOL,model,fixed=TRUE))
            
            },
            
            
            .check_constraints=function() {
              
              ### nothing for the moment

            },
            .check_interactions=function() {

                            noscale<-unlist(sapply(self$options$scaling , function(a) if (a$type=="none") return(a$var)))
                            res<-lapply(seq_along(self$endogenousTerms), function(i) {
                                    sapply(self$endogenousTerms[[i]],function(term) {
                                          if (length(term)>1)
                                                return(intersect(term,noscale))
                                          else return(NULL)
                                    })
                            })
                            res<-unique(unlist(res))
                            if (is.something(res)) {
                                    w<-gsub("{vars}",paste(res,collapse = ", "),WARNS[["nocenterd"]],fixed = TRUE)
                                   self$warning<-list(topic="infotab",message=w)
                            }
                            

            },
            .factorlist=function(terms,factorslen) {
              for (f in names(factorslen)) {
                .terms<-list()
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
              ## remove the contraints
              sel<-grep(":",tab$rhs,fixed = T,invert=T) 
              tab<-tab[sel,]
              # remove interactions
              sel<-grep(INTERACTION_SYMBOL,tab$rhs, fixed=T,invert = T)
              tab<-tab[sel,]
              sel<-tab$rhs!=""
              tab<-tab[sel,]

              # remove moderators
              if (is.something(self$moderators)) {
                sel<-!(tab$rhs %in% private$.lav_moderators)
                tab<-tab[sel,]
              }
              # select the path rows
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
                self$warning<-list(topic="jamovi",message=WARNS[["noindirect"]])
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
                self$warning<-list(topic="infotab",message=.results$warning)
                if (.results$error!=FALSE)
                    self$warning<-list(topic="jamovi",message=WARNS[["noindirect"]])
                results[[i]]<-.results
              }
              # we moltiply the parameters labs to compose the indirect effect
              pars<-sapply(labslist,paste,collapse="*")
              
              if (!is.something(pars)) {
                self$warning<-list(topic="jamovi",message=WARNS[["noindirect"]])
                return()
              }
              # labels for the new parameter for lavaaan
              plabs<-paste0("IE",1:length(pars))
              # collapse into lavaan syntax
              synt<-paste(plabs,pars,sep=":=",collapse = " ; ")
              private$.lav_indirect<-synt
              # we save the indirect effects as list of b64 terms for later usage
              private$.indirect_names=termslist
              ## compose into string with the arrow
              labs<-sapply(termslist,paste,collapse=" \U21d2 ")
              self$indirect_names<-as.list(fromb64(labs))
              names(self$indirect_names)<-pars
              # save also as a list for the tab_info
              self$tab_indirect<-lapply(self$indirect_names,function(a) list(info="Indirect effects",value=a))
            },
            
            .total=function() {
              ## first, we update the lavaanified structure table
              private$.make_structure()
              tab<-private$.lav_structure
              inds<-private$.indirect_names
              inames<-names(self$indirect_names)
              if (length(inds)==0)
                return()

              # Group the indirect chains by their endpoints X -> Y. The direct
              # path X -> Y may be absent (wrong model but estimable, or an IV left out of
              # the full model), so its label cannot be the grouping key.
              pairs<-list()
              for (i in seq_along(inds)) {
                from<-trimws(inds[[i]][1])
                to<-trimws(utils::tail(inds[[i]],1))
                key<-paste(from,to,sep="\r")
                if (is.null(pairs[[key]])) {
                  direct<-tab$label[tab$op=="~" & tab$rhs==from & tab$lhs==to]  # character(0) if absent
                  pairs[[key]]<-list(name=paste(fromb64(from),fromb64(to),sep=" \U21d2 "),
                                     direct=direct, indirect=character(),
                                     contrast=private$.comparison(from))
                }
                pairs[[key]]$indirect<-c(pairs[[key]]$indirect,inames[i])
              }

              # Total = direct (when estimated) + every indirect effect between X and Y.
              # Without a direct path, the model fixes it at 0 and the total is the
              # sum of the indirect effects.
              totals<-vapply(pairs,function(p) paste(c(p$direct,p$indirect),collapse="+"),character(1))
              private$.lav_total<-paste(paste0("TOT",seq_along(totals)),totals,sep=":=",collapse=" ; ")
              # .make_tables() finds the Total rows by these names (the total expressions)...
              self$total_names<-stats::setNames(lapply(pairs,function(p) p$name),totals)

              # ...and the Direct rows from these. A direct path the user left out of
              # the full model is still listed (label NULL): the table shows it as
              # fixed to 0, so the constraint is visible.
              self$direct_effects<-unname(lapply(pairs,function(p)
                list(name=p$name, label=if (length(p$direct)==1) p$direct else NULL,
                     contrast=p$contrast)))
              if (any(vapply(pairs,function(p) length(p$direct)==0,logical(1))))
                self$warning<-list(topic="models_coefficients",message=WARNS[["fixeddirect"]])
            },
            # The comparison made by a factor contrast variable (encoded name), for
            # the Contrast column of the effects table; "" for other variables.
            .comparison=function(code) {
              comparison<-contrast_comparison(code,private$.conditional_datamatic)
              if (is.null(comparison)) "" else comparison
            },
            # A Direct row for a path that is not in the full model: its value is
            # fixed to 0 by the model, so it has no standard error, interval or test.
            .fixed_direct_row=function(name, condition=NULL, contrast="") {
              row<-data.frame(type="Direct", effect=name, label=contrast, est=0, se=NA, ci.lower=NA,
                              ci.upper=NA, std.all=0, z=NA, pvalue=NA, stringsAsFactors=FALSE)
              if (!is.null(condition))
                row$condition<-condition
              row
            }
            
            
            
          ) # end of private
) # End Rclass

