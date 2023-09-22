library(R6)

Node <- R6Class(
  classname = "Node",
  public = list(
    static = new.env(),
    name = NULL,
    group = NULL,
    parent = NULL,
    children = list(),
    initialize = function(group) {
      if (is.null(self$static$num)) {
        self$static$num <- 1
      }

      self$group <- group
      self$name <- self$static$num

      self$static$num <- self$static$num + 1
    },
    add_child = function(child) {
      child$parent <- self
      self$children <- append(self$children, list(child))
    }
  )
)

build_balanced_binary_tree <- function(nodes, groups) {
  if (length(groups) == 0) {
    return()
  }
  next_level_nodes <- list()

  for (node in nodes) {
    if (length(groups) == 0) {
      return()
    }

    left <- Node$new(groups[[1]])
    groups <- groups[-1]
    node$add_child(left)
    next_level_nodes <- append(next_level_nodes, list(left))

    if (length(groups) == 0) {
      return()
    }

    right <- Node$new(groups[[1]])
    groups <- groups[-1]
    node$add_child(right)
    next_level_nodes <- append(next_level_nodes, list(right))
  }

  return(build_balanced_binary_tree(
    next_level_nodes,
    groups
  ))
}

print_by_level <- function(nodes, level) {
  if (length(nodes) == 0) {
    return()
  }

  SKM::echo("Level %s", level)
  next_level_nodes <- list()

  for (node in nodes) {
    SKM::echo("\t- %s (%s)", node$name, length(node$group))
    next_level_nodes <- append(next_level_nodes, node$children)
  }

  return(print_by_level(next_level_nodes, level + 1))
}

Tree <- R6Class(
  classname = "Tree",
  public = list(
    root = NULL,
    total_rows_num = NULL,
    leaves = list(),
    initialize = function(groups, total_rows_num) {
      Node$static$num <- 1
      self$root <- Node$new(groups[[1]])
      self$total_rows_num <- total_rows_num
      groups <- groups[-1]

      self$leaves <- build_balanced_binary_tree(
        list(self$root),
        groups
      )
    },
    print = function() {
      invisible(print_by_level(list(self$root), 0))
    },
    aggregate = function() {
      return(self$aggregate_recursive(self$root))
    },
    aggregate_recursive = function(node) {
      weights <- combine_users_models(node$group, self$total_rows_num)

      for (child in node$children) {
        weights <- combine_weights(
          weights,
          self$aggregate_recursive(child)
        )
      }

      return(weights)
    }
  )
)
