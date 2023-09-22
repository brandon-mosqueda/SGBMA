library(R6)
library(igraph)

source("utils.R")
source("model.R")
source("tree.R")

TreeAggregation <- R6Class(
  classname = "TreeAggregation",
  inherit = Model,
  public = list(
    initialize = function(..., group_size) {
      super$initialize(...)

      # Random permutation of nodes
      nodes_num <- length(private$nodes)
      private$nodes <- private$nodes[sample(nodes_num, nodes_num)]

      # Generate the subgroups for each node in the tree
      tree_nodes_num <- ceiling(nodes_num / group_size)
      groups <- rep(seq(tree_nodes_num), length.out = nodes_num)
      private$groups <- vector("list", tree_nodes_num)

      for (i in seq(tree_nodes_num)) {
        private$groups[[i]] <- private$nodes[groups == i]
      }

      private$tree <- Tree$new(private$groups, private$total_rows_num)
    },
    fit_round = function(bar) {
      for (node in private$nodes) {
        bar$tick()

        node$set_weights(get_weights(private$global_model))
        node$train()
      }
    },
    aggregation_round = function() {
      global_weights <- private$tree$aggregate()
      set_weights(private$global_model, global_weights)
    }
  ),
  private = list(
    tree = NULL,
    group_size = NULL,
    groups = NULL
  )
)
