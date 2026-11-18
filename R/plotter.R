Plotter <- R6::R6Class(
  "Plotter",
  cloneable=FALSE,
  class=FALSE,
  inherit = Scaffold,
  public=list(
      vars=NULL,
      initialize=function(jmvobj,datamatic,operator,resultsplots) {
            super$initialize(jmvobj)
            self$vars<-datamatic$vars
            private$.plotgroup<-resultsplots
            private$.operator<-operator
            private$.datamatic<-datamatic
      },

      initPlots=function() {
           if (!self$options$path_diagram)
               return()
            private$.plotgroup$diagrams$addItem("0")
            private$.plotgroup$diagrams$get(key = "0")$setTitle("")
      },
      preparePlots=function() {
        if (self$options$model_diagram)
          private$.prepareModelDiagram()
        if (!self$options$path_diagram)
          return()

        notes <- private$.plotgroup$notes
        notes$deleteRows()
        notes$setVisible(FALSE)
        images <- private$.plotgroup$diagrams
        images$clear()

        prepared <- try_hard({
          drawing <- path_diagram_data(private$.operator$model)
          nodeLabels <- fromb64(drawing$Vars$name)
          if (self$options$diag_abbrev != "0")
            nodeLabels <- abbreviate(nodeLabels,
              minlength = as.numeric(self$options$diag_abbrev), strict = TRUE)
          size <- switch(self$options$diag_labsize,
                         small = 8, large = 18, vlarge = 24, 12)
          size <- size * exp(-length(nodeLabels) / 80) + 1
          rotation <- as.numeric(self$options$diag_rotate)
          if (self$options$diag_type %in% c("circle", "circle2") && rotation %in% c(2, 4)) {
            rotation <- 1
            notes$addRow("rotation", list(message =
              "The circle layout requires Exogenous top or Exogenous bottom rotation. Rotation has been set to Exogenous top."))
          }
          options <- list(
            layout = private$.layout(drawing), residuals = self$options$diag_resid,
            rotation = rotation, intercepts = FALSE, nodeLabels = nodeLabels,
            whatLabels = self$options$diag_paths, sizeMan = size, sizeMan2 = size / 2,
            curve = 2, shapeMan = self$options$diag_shape, edge.label.cex = 1.3)
          groups <- unique(drawing$Pars$group)
          for (index in seq_along(groups)) {
            key <- as.character(index - 1L)
            if (!key %in% images$itemKeys)
              images$addItem(key)
            image <- images$get(key = key)
            image$setTitle("")
            parameters <- drawing$Pars[drawing$Pars$group == groups[[index]], , drop = FALSE]
            thresholds <- drawing$Thresholds
            if (nrow(thresholds) > 0)
              thresholds <- thresholds[thresholds$group == groups[[index]], , drop = FALSE]
            parameters$group <- ""
            if (nrow(thresholds) > 0)
              thresholds$group <- ""
            image$setState(new_plot_state(
              list(Pars = parameters, Vars = drawing$Vars, Thresholds = thresholds), options))
          }
        }, silent = TRUE)
        if (!isFALSE(prepared$error)) {
          images$clear()
          notes$addRow("error", list(message = prepared$error))
        }
        if (!isFALSE(prepared$warning))
          notes$addRow("warning", list(message = paste(unique(unlist(prepared$warning)), collapse = "\n")))
        notes$setVisible(notes$rowCount > 0)
        invisible(NULL)
      }
      
      
  ), # end of public
  private = list(
    .datamatic=FALSE,
    .plotgroup=NULL,
    .operator=NULL,
    .layout=function(semModel) {
      if (self$options$diag_type != "mediation")
        return(self$options$diag_type)

      # Pathj uses indirect-effect chains to place nodes by mediation stage.
      # jAMM instead exposes fitted regressions, including expanded contrasts
      # and interactions, so derive the same stages from their directed paths.
      variables <- semModel$Vars$name
      parameters <- semModel$Pars
      regressions <- data.frame(lhs = parameters$rhs[parameters$edge == "~>"],
                                rhs = parameters$lhs[parameters$edge == "~>"])
      stages <- stats::setNames(rep(1, length(variables)), variables)
      remaining <- variables
      while (length(remaining) > 0) {
        ready <- remaining[vapply(remaining, function(variable) {
          !any(regressions$rhs[regressions$lhs == variable] %in% remaining)
        }, logical(1))]
        # A feedback loop has no left-to-right mediation ordering.
        if (length(ready) == 0)
          return("tree")
        for (variable in ready) {
          parents <- regressions$rhs[regressions$lhs == variable]
          if (length(parents) > 0)
            stages[[variable]] <- max(stages[parents]) + 1
        }
        remaining <- setdiff(remaining, ready)
      }

      # Match Pathj's triangular single-chain layout and spread parallel nodes
      # vertically; keep rows in semPlot's node order, not the sorted stages.
      vertical <- rep(.8, length(variables))
      if (length(unique(stages)) == length(stages)) {
        vertical[stages == min(stages) | stages == max(stages)] <- .2
      } else {
        for (stage in unique(stages)) {
          members <- which(stages == stage)
          vertical[members] <- seq_along(members) / (length(members) + 1)
        }
      }
      cbind(x = unname(stages), y = vertical)
    },
    .prepareModelDiagram=function() {
      # Store only plot data in image state so saved analyses can render again.
      state <- model_diagram_data(self$options)
      private$.plotgroup$pathmodel$setState(new_plot_state(state))
      notes <- private$.plotgroup$pathnotes
      notes$deleteRows()
      notes$setVisible(length(state$notes) > 0)
      for (index in seq_along(state$notes))
        notes$addRow(index, list(info = state$notes[[index]]))
    }

  ) # end of private
) # end of class
