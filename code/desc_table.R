library(tidyverse)

desc_table <- function(x, functions = c(mean = mean, median = median, sd = sd)){
 
  if(!is.data.frame(x)){stop("No df.")}
  if(!is.vector(functions)){stop("Function vector no vector.")}
  if(!all(sapply(functions, is.function))){"There is an element of functions that is not a function."}
      
  # test if logical, numeric or integer, give back vector indicating that
  test_if_num_int_log <- rep(NA, ncol(x))
  for(var in 1:ncol(x)) {
    test_if_num_int_log[var] <- any(c("numeric", "logical", "integer") %in% class(x[, var]))
  }
      
  # select only column that are num, int, or log
  x <- x[, test_if_num_int_log]
      
  # run functions on these columns
  df_output <- data.frame(matrix(NA, nrow = ncol(x), ncol = length(functions)))
    for (var in 1:ncol(x)) {
        
      df_output[var, ] <- sapply(functions, function(fun, x) fun(x), x = na.omit(x[, var]))
          
    }
  #no_obs <- df_try %>% n()
  #df_output needs number of obs automatically !!!
  
  if(!is.null(colnames(x))) rownames(df_output) <- colnames(x)
  if(!is.null(names(functions))) colnames(df_output) <- names(functions)
  return(df_output)
}

df_try <- data.frame(a = 1:10, b = 1:10)

df_out_try <- desc_table(df_try, functions = c(n = n))

df_out_try
