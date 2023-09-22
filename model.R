library(keras)
library(dplyr)
library(R6)

source("utils.R")
source("user.R")

Model <- R6Class(
  classname = "Model",
  public = list(
    rounds_num = NULL,
    metrics = tibble(),

    initialize = function(x_training,
                          y_training,
                          x_testing,
                          y_testing,
                          splits,
                          model_function,
                          learning_rate,
                          rounds_num,
                          inner_epochs,
                          inner_batch_size) {
      self$rounds_num <- rounds_num

      private$global_model <- model_function()

      private$total_rows_num <- nrow(x_training)

      private$x_training <- x_training
      private$y_training <- y_training

      private$x_testing <- x_testing
      private$y_testing <- y_testing

      private$nodes <- vector("list", length(splits))

      for (i in seq_along(splits)) {
        split <- splits[[i]]

        node <- User$new(
          x = split$x,
          y = split$y,
          epochs = inner_epochs,
          batch_size = inner_batch_size,
          model = model_function(),
          learning_rate = learning_rate
        )
        node$set_weights(get_weights(private$global_model))

        private$nodes[[i]] <- node
      }
    },
    start = function(rounds_num = NULL) {
      start_time <- Sys.time()

      if (!is.null(rounds_num)) {
        self$rounds_num <- rounds_num
      }

      for (i in seq(self$rounds_num)) {
        SKM::echo("* Round %s/%s", i, self$rounds_num)
        bar <- progress_bar(length(private$nodes))
        bar$tick(0)

        self$fit_round(bar)
        self$aggregation_round()
        self$add_round(start_time, i)
      }
    },
    fit_round = invisible,
    aggregation_round = invisible,
    add_round = function(start_time, round_num) {
      score <- evaluate(
        private$global_model,
        private$x_testing,
        private$y_testing,
        verbose = 0
      )

      training_score <- evaluate(
        private$global_model,
        private$x_training,
        private$y_training,
        verbose = 0
      )

      SKM::echo(
        "Testing: loss: %.3f | accuracy: %.3f   Training: loss: %.3f | accuracy: %.3f",
        score["loss"],
        score["accuracy"],
        training_score["loss"],
        training_score["accuracy"]
      )

      end_time <- Sys.time()
      elapsed_time <- difftime(end_time, start_time, units = "mins") %>%
        as.double()

      self$metrics <- self$metrics %>%
        bind_rows(tibble(
          Round = round_num,
          Loss = c(score["loss"], training_score["loss"]),
          Accuracy = c(score["accuracy"], training_score["accuracy"]),
          Data = c("Testing", "Training"),
          Minutes = elapsed_time
        ))
    },
    predict = function(x) {
      predictions <- predict(private$global_model, x)
      predictions <- apply(predictions, 1, which.max) - 1

      return(predictions)
    }
  ),
  private = list(
    nodes = list(),
    total_rows_num = NULL,
    global_model = NULL,
    x_testing = NULL,
    y_testing = NULL,
    x_training = NULL,
    y_training = NULL
  )
)
