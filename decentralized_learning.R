library(R6)
library(igraph)

source("utils.R")
source("model.R")

DecentralizedLearning <- R6Class(
  classname = "DecentralizedLearning",
  inherit = Model,
  public = list(
    initialize = function(..., graph) {
      super$initialize(...)

      graph <- set_vertex_attr(
        graph,
        name = "user",
        value = private$nodes
      )
      private$graph <- graph
    },
    start = function(...) {
      super$start(...)

      global_weights <- combine_users_models(
        private$nodes,
        private$total_rows_num
      )
      set_weights(private$global_model, global_weights)
    },
    fit_round = function(bar) {
      for (node in private$nodes) {
        bar$tick()

        node$train()
      }
    },
    aggregation_round = function() {
      global_weights <- NULL

      # Aggregation
      for (i in V(private$graph)) {
        user <- self$get_user(i)
        neighs <- neighbors(private$graph, i)

        users <- list(user)
        total_rows_num <- user$rows_num

        for (j in neighs) {
          neigh_user <- self$get_user(j)

          users <- append(users, list(neigh_user))
          total_rows_num <- total_rows_num + neigh_user$rows_num
        }

        user_weights <- combine_users_models(users, total_rows_num)
        user$set_weights(user_weights)
      }
    },
    add_round = function(...) {
      super$add_round(...)

      global_weights <- combine_users_models(
        private$nodes,
        private$total_rows_num
      )
      set_weights(private$global_model, global_weights)
    },
    get_user = function(node_num) {
      return(V(private$graph)[node_num]$user[[1]])
    }
  ),
  private = list(
    graph = NULL
  )
)
