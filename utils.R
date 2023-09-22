combine_weights <- function(weights1, weights2) {
  if (is.null(weights1)) {
    return(weights2)
  }

  return(purrr::map2(weights1, weights2, ~ .x + .y))
}

progress_bar <- function(total) {
  bar <- progress::progress_bar$new(
    format = "[:bar] :current/:total (:percent)",
    total = total
  )

  return(bar)
}

neural_net <- function(hidden_units = 200) {
  model <- keras_model_sequential()

  model %>%
    layer_flatten(input_shape = c(28, 28)) %>%
    layer_dense(units = hidden_units, activation = "relu") %>%
    layer_dense(units = hidden_units, activation = "relu") %>%
    layer_dense(units = 10, activation = "softmax") %>%
    compile(
      optimizer = "adam",
      loss = "sparse_categorical_crossentropy",
      metrics = c("accuracy")
    )

  return(model)
}

idd_split <- function(x, y, splits_num) {
  indices <- sample(nrow(x), nrow(x))
  splits_indices <- split(indices, seq(splits_num))

  splits <- purrr::map(splits_indices, ~ list(x = x[.x, , ], y = y[.x]))

  return(splits)
}

non_idd_split <- function(x, y, splits_num) {
  ordered_indices <- order(y)
  # We divide ordered indices into splits_num * 2 parts, so for each split we
  # can take two parts and combine them into one split. This will generate must
  # of splits with data of two classes.
  cut_indices <- cut(seq_along(y), splits_num * 2, labels = FALSE)
  splits_indices <- split(ordered_indices, cut_indices)

  # We select random pairs of splits for combining them.
  splits_pairs <- sample(length(splits_indices)) %>%
    split(seq(splits_num))

  # Combine the pairs of splits into one split.
  splits <- purrr::map(
    splits_pairs,
    function(pair) {
      indices <- c(splits_indices[[pair[[1]]]], splits_indices[[pair[[2]]]])

      return(list(x = x[indices, , ], y = y[indices]))
    }
  )

  return(splits)
}

combine_users_models <- function(users, total_rows_num) {
  global_weights <- NULL

  for (user in users) {
    user_weights <- user$get_weights()

    # Weighted average
    alpha <- user$rows_num / total_rows_num

    user_weights <- purrr::map(
      user_weights,
      ~ alpha * .x
    )

    global_weights <- combine_weights(global_weights, user_weights)
  }

  return(global_weights)
}
