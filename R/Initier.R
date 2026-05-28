Initer <- R6::R6Class(
    "Initer",
    class = TRUE,
    cloneable = FALSE, ## should improve performance https://r6.r-lib.org/articles/Performance.html ###
    inherit = Scaffold,
    public = list(
        dispatcher = NULL,
        data = NULL,
        initialize = function(jmvobj) {
            super$initialize(jmvobj)
            self$data<-jmvobj$data
            ### we want to clean the html message objects
            dispatch_message_cleaner(jmvobj)
            ## initialize the "info" accordion
            

            if (self$options$.interface=="jamovi") {
               si<-SmartInfo$new(jmvobj)
               si$infovec<-INFO
               si$infotag<-self$options$method  
            #   si$info()
            }
          #### check if we can go
         
            if (is.null(self$options$dep)) {
                         self$warning<-list(topic="issues",
                                      message="Please select the dependent variable",
                                      head="info")
              self$ok<-FALSE
            }
            if (length(c(self$options$covs,self$options$factors))==0) {
                         self$warning<-list(topic="issues",
                                      message="Please select at least one predictor.",
                                      head="info")
              self$ok<-FALSE
            }
            
            if (!self$ok) return()
          
          ## set the selector clss as the selector

            
        },
        init_infotab= function() {
          

          }



        #### init functions #####
    ), # End public

    private = list(
      

      
    ) # end of private
) # End Rclass
