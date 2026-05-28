## This class takes care of estimating the models and return the results. It inherit from Initer, and defines the same tables
## defined by Initer, but it fills them with the results. It also adds a few tables not defined in Initer
## Any function that produce a table goes here

Runner <- R6::R6Class("Runner",
    inherit = Initer,
    cloneable = FALSE,
    class = TRUE,
    public = list(
        run = function() {
          
            ## we stop if initier is not ok
            if (!self$ok) return()
            jinfo("jAMM: Runner: estimations")
            ### self$selector is the selector initialized in Initier


        },
        run_infotab= function() {

        }
        
    ), # end of public 

    private = list(
      

    ) # end of private
) # end of class
