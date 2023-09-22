rm(list = ls())

source("tree_aggregation.R")

# GLOBAL PARAMETERS ------------------------------------------------------------
hidden_units <- 200
type <- "IDD" # "IDD" or "nonIDD"
rounds_num <- 250
nodes_num <- 100
inner_epochs <- 5
inner_batch_size <- 10
group_size <- 5
model <- sprintf("SGBMA %s", group_size)
results_dir <- "results"
# GLOBAL PARAMETERS END --------------------------------------------------------

load("data/mnist.RData", verbose = TRUE)

tree_neural_net <- function() {
  neural_net(hidden_units)
}

split_function <- ifelse(type == "IDD", idd_split, non_idd_split)
splits <- split_function(x_training, y_training, nodes_num)

controller <- TreeAggregation$new(
  x_training = x_training,
  y_training = y_training,
  x_testing = x_testing,
  y_testing = y_testing,
  splits = splits,
  group_size = group_size,
  model_function = tree_neural_net,
  learning_rate = 1.47,
  inner_epochs = inner_epochs,
  inner_batch_size = inner_batch_size,
  rounds_num = rounds_num
)

controller$start()

metrics <- controller$metrics %>%
  mutate(
    Model = model,
    Type = type,
    HiddenUnits = hidden_units,
    NodesNum = nodes_num,
    RoundsNum = rounds_num,
    InnerEpochs = inner_epochs,
    InnerBatchSize = inner_batch_size
  )

results_dir <- file.path(
  results_dir,
  model,
  type,
  hidden_units,
  nodes_num,
  rounds_num,
  inner_epochs,
  inner_batch_size
)
SKM::mkdir(results_dir)
SKM::write_csv(
  metrics,
  file = file.path(results_dir, "sgbma.csv")
)
