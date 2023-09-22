library(R6)

source("utils.R")
source("model.R")

FederatedLearning <- R6Class(
  classname = "FederatedLearning",
  inherit = Model,
  public = list(
    fit_round = function(bar) {
      for (node in private$nodes) {
        bar$tick()

        node$set_weights(get_weights(private$global_model))
        node$train()
      }
    },
    aggregation_round = function() {
      global_weights <- combine_users_models(
        private$nodes,
        private$total_rows_num
      )

      set_weights(private$global_model, global_weights)
    }
  )
)
