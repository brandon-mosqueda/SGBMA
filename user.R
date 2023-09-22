library(keras)
library(R6)

User <- R6Class(
  classname = "User",
  public = list(
    rows_num = NULL,

    initialize = function(x, y, model, learning_rate, epochs, batch_size) {
      private$x <- x
      self$rows_num <- nrow(private$x)
      private$y <- y
      private$model <- model
      private$learning_rate <- learning_rate
      private$epochs <- epochs
      private$batch_size <- batch_size
    },
    set_weights = function(weights) {
      set_weights(private$model, weights)
    },
    get_weights = function() {
      get_weights(private$model)
    },
    train = function() {
      fit(
        private$model,
        private$x,
        private$y,
        epochs = private$epochs,
        batch_size = private$batch_size,
        learning_rate = private$learning_rate,
        verbose = 0,
        validation_split = 0
      )
    }
  ),
  private = list(
    x = NULL,
    y = NULL,
    epochs = NULL,
    batch_size = NULL,
    learning_rate = NULL,
    model = NULL
  )
)
