## This class takes care of estimating the model and return the results. It inherit from Syntax, and define the same tables
## defined by Syntax, but it fill them with the results.

Estimate <- R6::R6Class("Estimate",
                        inherit = Syntax,
                        cloneable=FALSE,
                        class=FALSE,
                        public=list(
                          model=NULL,
                          tab_fit=NULL,
                          tab_fitindices=NULL,
                          ciwidth=NULL,
                          tab_constfit=NULL,
                          initialize=function(jmvobj,datamatic) {
                            super$initialize(
                              jmvobj=jmvobj,
                              datamatic=datamatic)
                            self$ciwidth<-self$options$ciWidth/100
                          },
                          estimate=function(data) {
                            # Never expose a previous fit after a failed re-estimation.
                            self$model <- NULL
                            self$ok <- TRUE
                            # Build conditional definitions on the same data and
                            # scale used to fit the regression interactions.
                            self$prepareConditional(data)
                            private$.lavaan_estimate(data)
                            if (!self$ok)
                              return(invisible(NULL))
                            
                            # fit indices
                            alist<-list()
                            alist[[length(alist)+1]]<-c(info="Estimation Method",value=lavaan::lavInspect(self$model,"options")$estimator)
                            alist[[length(alist)+1]]<-c(info="Number of observations",value=lavaan::lavInspect(self$model,"ntotal")) 
                            alist[[length(alist)+1]]<-c(info="Converged",value=lavaan::lavInspect(self$model,"converged"))
                            alist[[length(alist)+1]]<-c(info="",value="")
                            
                            self$tab_info<-alist
                            
                          }, 
                          
                          computeR2=function(data) {
                            
                            end<-private$.lav_structure
                            endvars<-unique(end$lhs[end$op=="~"])
                            end<-end[end$lhs==end$rhs,]
                            end<-end[end$lhs %in% endvars,]
                            end$lhs<-fromb64(end$lhs)
                            end$rhs<-fromb64(end$rhs)
                            
                            end$var<-end$est/end$std.all
                            end$r2<-1-end$std.all
                             for (i in seq_along(end$r2)) 
                                    if (end$r2[[i]]>1 | end$r2[[i]]<0)  {
                                      end$r2[[i]]<-NA
                                      self$warning<-list(topic="models_r2",message="Some R-squared indices cannot be computed for this model.")
                                    }
                            
                              ### https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3821705/
                              N<-lavaan::lavInspect(self$model,"ntotal")
                              r<-sqrt(end$r2)
                              f<-.5 * log((1 + r)/(1 - r))
                              zr<-f*sqrt((N-3))
                              z0<-qnorm((1-self$ciwidth)/2,lower.tail = F)
                              lower<-zr-z0
                              upper<-zr+z0
                              flower<-lower/sqrt(N-3)
                              fupper<-upper/sqrt(N-3)
                              rupper<-(exp(2*fupper)-1)/(1+exp(2*fupper))
                              rupper<-rupper^2
                              rlower<-(exp(2*flower)-1)/(1+exp(2*flower))
                              rlower<-pmax(rlower,0)^2
                              end$ci.upper<-rupper
                              end$ci.lower<-rlower
                              ####
                            self$tab_r2<-end


                        }
                        ), ## end public
                        private=list(
                          
                         .lavaan_estimate=function(data) {
                           # Always define the argument, even when no bootstrap
                           # is requested and lavaan normally leaves it unevaluated.
                           bootci <- "perc"
                           if (self$options$ciType=="standard") {
                             ci<-TRUE
                             se="standard"
                           } else {
                             
                             if (self$options$ciType=="none") {
                               ci<-FALSE
                               se="standard"
                             } else {
                               ci<-TRUE
                               se="bootstrap"
                               # 'bca' is the option value saved by jAMM 1.x; lavaan calls
                               # the bias-corrected (non-accelerated) interval 'bca.simple'.
                               bootci <- switch(self$options$ciType, bca = "bca.simple", self$options$ciType)
                             }
                           } 
                           
                           
                           ## prepare the options based on Syntax definitions
                           lavoptions<-list(model = private$.lav_structure, 
                                            data = data,
                                            se=se,
                                            bootstrap=self$options$bootN,
                                            estimator="ML",
                                            likelihood="wishart"
                           )
                           
                           ## estimate the models
                           results<-try_hard({
                             model<-do.call(lavaan::lavaan,lavoptions)
                             parameters<-lavaan::parameterestimates(
                               model,
                               ci=ci,
                               standardized = TRUE,
                               level = self$ciwidth,
                               boot.ci.type = bootci
                             )
                             list(model=model,parameters=parameters)
                           })
                           
                           
                           
                           self$warning<-list(topic="jamovi",message=results$warning)
                           if (!isFALSE(results$error)) {
                             details<-fromb64(results$error)
                             if (grepl("infinite or missing",details,fixed=TRUE))
                               details<-paste(ERRS[["noluck"]],details)
                             if (!isFALSE(results$warning)) {
                               warnings<-unique(fromb64(
                                 unlist(results$warning,use.names=FALSE)
                               ))
                               details<-paste(
                                 paste(warnings,collapse="\n"),
                                 paste("Final error:",details),sep="\n"
                               )
                             }
                             self$stop(paste("Model cannot be estimated:",details))
                           }

                           # A fit that lavaan returns without an error can still be unusable:
                           # stop if it did not converge, or if lavaan warned that the sample
                           # covariance matrix is not positive definite (its "icov" warning,
                           # e.g. with perfectly collinear predictors). lavaan 0.6 stops by itself
                           # in that case, lavaan 0.7 returns a non-converged fit holding the
                           # starting values, which must not be shown as estimates.
                           warnings<-character(0)
                           if (!isFALSE(results$warning))
                             warnings<-unique(fromb64(unlist(results$warning,use.names=FALSE)))
                           icov<-grepl("icov\\(\\)|sample covariance matrix is not positive.definite",warnings)
                           converged<-isTRUE(lavaan::lavInspect(results$obj$model,"converged"))
                           if (!converged || any(icov)) {
                             reason<-if (!converged) "the estimation did not converge." else
                                       "the sample covariance matrix is not positive definite."
                             details<-paste(c(reason,warnings),collapse="\n")
                             self$stop(paste("Model cannot be estimated:",details))
                           }

                           ## ask for the paramters estimates
                           self$model<-results$obj$model
                           .lav_params<-results$obj$parameters
                           
                           ## we need some info initialized by Syntax regarding the parameters properties
                           private$.lav_structure<-.lav_params
                           private$.make_tables()
                           self$computeR2(data)
                           ginfo("Estimation is done...")
                         }
                
                
              ) #end of private 
                          

)  # end of class

