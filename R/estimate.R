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
                          initialize=function(options,datamatic) {
                            super$initialize(
                              options=options,
                              datamatic=datamatic)
                            self$ciwidth<-options$ciWidth/100
                          },
                          estimate=function(data) {
                            
                            private$.lavaan_estimate(data)
                            private$.ols_estimate(data)
                            
                          }, # end of private function estimate
                          
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
                                      self$warnings<-list(topic="r2",message="Some R-square indexex cannot be computed for this model")
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
                              rlower<-rlower^2
                              end$ci.upper<-rupper
                              end$ci.lower<-rlower
                              ####
                            self$tab_r2<-end


                        }
                        ), ## end public
                        private=list(
                        .lav_regressions=NULL,
                          
                         .lavaan_estimate=function(data) {
                           
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
                               bootci=self$options$ciType
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
                           results<-try_hard({do.call(lavaan::lavaan,lavoptions)  })
                           
                           
                           
                           self$warnings<-list(topic="main",message=results$warning)
                           self$errors<-results$error
                           
                           if (is.something(self$errors))
                             return(self$errors)
                           
                           ## ask for the paramters estimates
                           self$model<-results$obj
                           .lav_params<-lavaan::parameterestimates(
                             self$model,
                             ci=ci,
                             standardized = T,
                             level = self$ciwidth,
                             boot.ci.type = bootci
                           )
                           
                           ## we need some info initialized by Syntax regarding the parameters properties
                           private$.lav_structure<-.lav_params
                           private$.make_tables()
                           self$computeR2(data)
                           ginfo("Estimation is done...")
                         },
                         .ols_estimate=function(data) {
                           if (!("regression" %in% self$options$tableOptions)) {
                             .lav_regressions=NULL
                             return()
                             
                           }
                             
                           res<-lapply(private$.lav_models, function(mod) {
                             summary(lm(mod,data))
                           })
                           
                           
                           
                           }
                
                
              ) #end of private 
                          

)  # end of class

