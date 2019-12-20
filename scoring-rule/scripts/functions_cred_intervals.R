get_credible_intervals <- function(data, probs = c(0.025, 0.975), names = FALSE){
  # compute credible interval for each parameter
  #
  # Args:
  #   data: A matrix of posterior samples, with each row corresponding to a parameter to be estimated,
  #     and columns being posterior samples of that parameter
  #   probs: tuple of probabilities with values in [0, 1] marking left and right endpoints of credible interval
  #
  # Returns:
  #   a matrix with p rows and 2 columns, each row being a credible interval and p being number of rows in data
  p <- nrow(data)
  credible_intervals <- matrix(data = NA, nrow = p, ncol = 2)
  for (i in 1:p) {
    quantiles <- quantile(x = data[i, ], probs = probs, names = names)
    credible_intervals[i, ] <- quantiles
  }
  return(credible_intervals)
}



credible_intervals_vs_true_probs_general <- function(credible_intervals, true_probs, samples){
  
  row_names <- c("categories_smaller_than_interval", "categories_in_interval", "categories_bigger_than_interval")
  col_names <- c("number", "proportion")
  summary <- matrix(data = NA, nrow = length(row_names), ncol = length(col_names))
  rownames(summary) <- row_names
  colnames(summary) <- col_names
  
  categories_smaller_than_interval <- which(credible_intervals[, 1] > true_probs)
  categories_in_interval <- which(credible_intervals[, 1] < true_probs & true_probs < credible_intervals[, 2])
  categories_bigger_than_interval <- which(credible_intervals[, 2] < true_probs)
  
  summary["categories_smaller_than_interval", ] <- c(length(categories_smaller_than_interval),
                                                     length(categories_smaller_than_interval) / length(samples))
  summary["categories_in_interval", ] <- c(length(categories_in_interval),
                                           length(categories_in_interval) / length(samples))
  summary["categories_bigger_than_interval", ] <- c(length(categories_bigger_than_interval),
                                                    length(categories_bigger_than_interval) / length(samples))
  
  return(summary)
  
}



credible_intervals_vs_true_probs_by_obs <- function(credible_intervals, true_probs, samples){
  
  category_sizes <- sort(unique(samples))
  column_names <- c("num_obs", "num_categories_w_such_obs", "percent_categories_w_such_obs",
                    "num_less_than_interval", "num_in_interval", "num_greater_than_interval",
                    "percent_less_than_interval", "percent_in_interval", "percent_greater_than_interval")
  summary <- matrix(data = NA, nrow = length(category_sizes), ncol = length(column_names))
  colnames(summary) <- column_names
  summary[, "num_obs"] <- category_sizes
  summary[, "num_categories_w_such_obs"] <- as.vector(table(samples))
  summary[, "percent_categories_w_such_obs"] <- summary[, "num_categories_w_such_obs"] / length(samples)
  
  categories_smaller_than_interval <- which(credible_intervals[, 1] > true_probs)
  categories_in_interval <- which(credible_intervals[, 1] < true_probs & true_probs < credible_intervals[, 2])
  categories_bigger_than_interval <- which(credible_intervals[, 2] < true_probs)
  
  
  for (i in 1:nrow(summary)) {
    num_obs <- summary[i, "num_obs"]
    summary[i, "num_less_than_interval"] <- length(which(samples[categories_smaller_than_interval] == num_obs))
    summary[i, "num_in_interval"] <- length(which(samples[categories_in_interval] == num_obs))
    summary[i, "num_greater_than_interval"] <- length(which(samples[categories_bigger_than_interval] == num_obs))
    summary[i, "percent_less_than_interval"] <- summary[i, "num_less_than_interval"] / summary[i , "num_categories_w_such_obs"]
    summary[i, "percent_in_interval"] <- summary[i, "num_in_interval"] / summary[i , "num_categories_w_such_obs"]
    summary[i, "percent_greater_than_interval"] <- summary[i, "num_greater_than_interval"] / summary[i , "num_categories_w_such_obs"]
  }
  
  return(summary)
  
}