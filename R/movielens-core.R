if(!require(tidyverse))
  install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if (!require(caret))
  install.packages("caret", repos = "http://cran.us.r-project.org")
if (!require(lubridate))
  install.packages("lubridate", repos = "http://cran.us.r-project.org")
if (!require(recommenderlab))
  install.packages("recommenderlab", repos = "http://cran.us.r-project.org")
if (!require(data.table))
  install.packages("data.table", repos = "http://cran.us.r-project.org")

###############################################################################
# Compute the naive  model: estimated rating is the avergae of the rating in
# train set
## Parameters:
#   - train_set: The train set
#   - test_set: The test set
## Return values: a named list of 1 entry:
#   - mu_hat: the estimated rating average on train set
#   - rmse: the rmse obtained in test set with this model
###############################################################################
get_naive_model <- function(train_set, test_set) {
  print("Naive bias model")
  mu_hat <- mean(train_set$rating)
  rmse <- NULL
  if (!is.null(test_set))
    rmse <-
    RMSE(test_set$rating, rep(mu_hat, length(test_set$rating)))
  return (list(mu_hat = mu_hat,
               rmse = rmse))
}
###############################################################################
# Compute the movie  model.
# It's based on naive bias model
## Parameters:
#   - train_set: The train set
#   - test_set: The test set
#   - lambda: regularization parameter, default value is 0
#   - naive model: the naive model to be used
#                       If the naive model is already computed, we can
#                       pass it as a parameter to this method.
#                       default value is NULL
## Return values: a named list of 1 entry:
#   - naive_model: the naive model used to build this model,
#   - lambda: The regularization parameter used for this model,
#   - movie_bias: a data frame with movie bias values in train set
#   - rmse: the rmse obtained in test set with this model
###############################################################################
get_movie_bias_model <-function(train_set,
                                test_set,
                                lambda = 0,
                                naive_model = NULL) {
  if (is.null(naive_model)) {
    # if we transmit a naive model as a method parameter
    # compute the naive model first, we don't need to compute the prediction
    # of the sub model so we pass NULL as test_set
    print(paste0("Movie bias, compute naive bias for lambda=", lambda))
    naive_model <- get_naive_model(train_set, NULL)
  }
  # shortcut
  mu_hat <- naive_model$mu_hat
  # movie bias estimation on train set
  if (lambda == 0)
    # without regularization
    movie_bias <- train_set %>% group_by(movieId) %>%
    summarize(bi_hat = mean(rating - mu_hat))
  else
    # with regularization
    movie_bias <- train_set %>% group_by(movieId) %>%
    summarize(bi_hat = sum(rating - mu_hat) / (lambda + n()))
  rmse <- NULL
  if (!is.null(test_set)) {
    # compute the predicted rating on test set using the bi_hat and mu_hat
    prediction_test <- test_set %>%
      left_join(movie_bias, by = "movieId") %>%
      mutate(rating_hat = mu_hat + bi_hat)
    rmse <- RMSE(prediction_test$rating, prediction_test$rating_hat)
  }
  return(list(
    naive_model = naive_model,
    movie_bias = movie_bias,
    rmse = rmse,
    lambda = lambda
  ))
}
###############################################################################
# Compute the user bias model.
# It's based on movie bias model
## Parameters:
#   - train_set: The train set
#   - test_set: The test set
#   - lambda: regularization parameter, default value is 0
#   - movie_bias_model: the movie bias model to be used
#                       If the movie bias model is already computed, we can
#                       pass it as a parameter to this method.
#                       default value is NULL
## Return values: a named list of 1 entry:
#   - movie_bias_model: the movie bias model used to build this model,
#   - lambda: The regularization parameter used for this model,
#   - user_bias: a data frame with user bias values in train set
#   - rmse: the rmse obtained in test set with this model
###############################################################################
get_user_bias_model <-function(train_set,
                               test_set,
                               lambda = 0,
                               movie_bias_model = NULL) {
  if (!is.null(movie_bias_model)) {
    # if we transmit a movia bias model as a method parameter
    # check if the transmitted model is compatible with the other parameters
    if (lambda != movie_bias_model$lambda)
      stop("The transmitted movie bias model is not compatible with the used data set")
  } else {
    # compute the movie bias model first, we don't need to compute the prediction
    # of the sub model so we pass NULL as test_set
    print(paste0("User bias model, compute movie bias model for lambda=", lambda))
    movie_bias_model <- get_movie_bias_model(train_set, NULL, lambda)
  }
  #Shortcuts
  mu_hat <- movie_bias_model$naive_model$mu_hat
  movie_bias <- movie_bias_model$movie_bias
  
  # compute user bias estimation
  if (lambda == 0)
    # without regularization
    user_bias <- train_set %>%
    left_join(movie_bias, by = "movieId") %>%
    group_by(userId) %>%
    summarize(bu_hat = mean(rating - mu_hat - bi_hat))
  else
    # with regularization
    user_bias <- train_set %>%
    left_join(movie_bias, by = "movieId") %>%
    group_by(userId) %>%
    summarize(bu_hat = sum(rating - mu_hat - bi_hat) / (lambda + n()))
  rmse <- NULL
  if (!is.null(test_set)) {
    # compute the predicted rating on test set using the bi_hat, bu_hat and mu_hat
    prediction_test <- test_set %>%
      left_join(movie_bias, by = "movieId") %>%
      left_join(user_bias, by = "userId") %>%
      mutate(rating_hat = mu_hat + bi_hat + bu_hat)
    # get the RMSE on test set
    rmse <- RMSE(prediction_test$rating, prediction_test$rating_hat)
  }
  return(
    list(
      movie_bias_model = movie_bias_model,
      user_bias = user_bias,
      rmse = rmse,
      lambda = lambda
    )
  )
}
###############################################################################
# Compute the genre bias model.
# It's based on user bias model
## Parameters:
#   - train_set: The train set
#   - test_set: The test set
#   - train_set_sep: The train set with separated rows for every movie genre
#   - test_set_sep: The test set with separated rows for every movie genre
#   - lambda: regularization parameter, default value is 0
#   - user_bias_model: the user bias model to be used
#                       If the user bias model is already computed, we can
#                       pass it as a parameter to this method.
#                       default value is NULL
## Return values: a named list of 1 entry:
#   - user_bias_model: the user bias model used to build this model,
#   - lambda: The regularization parameter used for this model,
#   - genre_bias: a data frame with genre bias values in train set
#   - rmse: the rmse obtained in test set with this model
###############################################################################
get_genre_bias_model <-function(train_set,
                                test_set,
                                train_set_sep,
                                test_set_sep,
                                lambda = 0,
                                user_bias_model = NULL) {
  if (!is.null(user_bias_model)) {
    # if we transmit a user bias model as a method parameter
    # check if the transmitted model is compatible with the other parameters
    if (lambda != user_bias_model$lambda)
      stop("The transmitted user bias model is not compatible with the used data set")
  } else {
    # compute the user bias  model first, we don't need to compute the prediction
    # of the sub model so we pass NULL as test_set
    print(paste0("Genre bias model, compute User bias model for lambda=", lambda))
    user_bias_model <- get_user_bias_model(train_set, NULL, lambda)
  }
  #Shortcuts
  mu_hat <- user_bias_model$movie_bias_model$naive_model$mu_hat
  movie_bias <- user_bias_model$movie_bias_model$movie_bias
  user_bias <- user_bias_model$user_bias
  
  # genres bias estimation on train set
  if (lambda == 0)
    # without regularization
    genre_bias <- train_set_sep %>%
    left_join(movie_bias, by = "movieId") %>%
    left_join(user_bias, by = "userId") %>%
    group_by(genres) %>%
    summarize(beta_g = mean(rating - mu_hat - bi_hat - bu_hat))
  else
    #with regularization
    genre_bias <- train_set_sep %>%
    left_join(movie_bias, by = "movieId") %>%
    left_join(user_bias, by = "userId") %>%
    group_by(genres) %>%
    summarize(beta_g = sum(rating - mu_hat - bi_hat - bu_hat) / (lambda + n()))
  rmse <- NULL
  if (!is.null(test_set)) {
    # compute the predicted rating on test set using the bi_hat, bu_hat, bg_hat and mu_hat
    prediction_test <- test_set_sep %>%
      left_join(genre_bias, by = "genres") %>%
      group_by(userId, movieId) %>%
      summarize(bg_hat = sum(beta_g)) %>%  #Sum all the genre bias
      left_join(movie_bias, by = "movieId") %>%
      left_join(user_bias, by = "userId") %>%
      left_join(test_set, by = c("userId", "movieId")) %>%
      mutate(rating_hat = mu_hat + bi_hat + bg_hat + bu_hat)
    rmse <- RMSE(prediction_test$rating, prediction_test$rating_hat)
  }
  return(
    list(
      user_bias_model = user_bias_model,
      genre_bias = genre_bias,
      rmse = rmse,
      lambda = lambda
    )
  )
}

###############################################################################
# Compute the time bias model.
# It's based on genre bias model
## Parameters:
#   - train_set: The train set
#   - test_set: The test set
#   - train_set_sep: The train set with separated rows for every movie genre
#   - test_set_sep: The test set with separated rows for every movie genre
#   - lambda: regularization parameter, default value is 0
#   - genre_bias_model: the genre bias model to be used
#                       If the genre bias model is already computed, we can
#                       pass it as a parameter to this method.
#                       default value is NULL
## Return values: a named list of 1 entry:
#   - genre_bias_model: the genre bias model used to build this model,
#   - lambda: The regularization parameter used for this model,
#   - time_bias: a data frame with the time bias values in train set
#   - rmse: the rmse obtained in test set with this model
###############################################################################
get_time_bias_model <-function(train_set,
                               test_set,
                               train_set_sep,
                               test_set_sep,
                               lambda = 0,
                               genre_bias_model = NULL) {
  if (!is.null(genre_bias_model)) {
    # if we transmit a genre bias model as a method parameter
    # check if the transmitted model is compatible with the other parameters
    if (lambda != genre_bias_model$lambda)
      stop("The transmitted genre bias model is not compatible with the used data set")
  } else {
    # compute the genre bias  model first, we don't need to compute the prediction
    # of the sub model so we pass NULL as test_set
    print(paste0("Time bias model, compute Genre bias model for lambda=", lambda))
    genre_bias_model <-
      get_genre_bias_model(train_set, NULL, train_set_sep, NULL, lambda)
  }
  # sortcuts to the used value
  mu_hat <-
    genre_bias_model$user_bias_model$movie_bias_model$naive_model$mu_hat
  movie_bias <-
    genre_bias_model$user_bias_model$movie_bias_model$movie_bias
  user_bias <- genre_bias_model$user_bias_model$user_bias
  genre_bias <- genre_bias_model$genre_bias
  
  # time bias estimation on train set
  if (lambda == 0) {
    # without regularization
    time_bias <- train_set_sep %>%
      left_join(genre_bias, by = "genres") %>%
      group_by(userId, movieId) %>%
      summarize(bg_hat = sum(beta_g)) %>%  #Sum all the genre biais
      left_join(movie_bias, by = "movieId") %>%
      left_join(user_bias, by = "userId") %>%
      left_join(train_set, by = c("userId", "movieId")) %>%
      group_by(years) %>%
      summarize(bt_hat = mean(rating - mu_hat - bi_hat - bg_hat - bu_hat))
  }
  else {
    # with regularization
    time_bias <- train_set_sep %>%
      left_join(genre_bias, by = "genres") %>%
      group_by(userId, movieId) %>%
      summarize(bg_hat = sum(beta_g)) %>%  #Sum all the genre biais
      left_join(movie_bias, by = "movieId") %>%
      left_join(user_bias, by = "userId") %>%
      left_join(train_set, by = c("userId", "movieId")) %>%
      group_by(years) %>%
      summarize(bt_hat = sum(rating - mu_hat - bi_hat - bg_hat - bu_hat) /
                  (lambda + n()))
  }
  rmse <- NULL
  prediction_test <- NULL
  if (!is.null(test_set)) {
    # compute the predicted rating on test set using the regularized movie, genres, user and time effects
    prediction_test <-
      get_bias_model_prediction(test_set,
                                test_set_sep,
                                mu_hat,
                                movie_bias,
                                user_bias,
                                genre_bias,
                                time_bias)
    rmse <- RMSE(prediction_test$rating, prediction_test$rating_hat)
  }
  return(
    list(
      genre_bias_model = genre_bias_model,
      time_bias = time_bias,
      rmse = rmse,
      lambda = lambda
    )
  )
}

###############################################################################
# Compute the regularized bias model
# We use time bias, genre bias, user bias, movie bias
## Parameters:
#   - train_set: The train set
#   - test_set: The test set
#   - train_set_sep: The train set with separated rows for every movie genre
#   - test_set_sep: The test set with separated rows for every movie genre
#   - lambdas: a list of regularization values to train the linear model
## Return values: a named list of 1 entry:
#   - time_bias_model: the regularized time bias model with the optimal lambda value,
#   - lambdas: The regularization parameters list
#   - rmses: The RMSE list for every entry in lambdas list
#   - rmse: The rmse value of the regularized model, it's the minimum of the computed
#           rmses
#   - lambda: optimal lambda value that minimize the rmses
#
###############################################################################
get_bias_regularized_model <-function(train_set,
                                      test_set,
                                      train_set_sep,
                                      test_set_sep,
                                      lambdas) {
  compute <- T
  # If you want to skip long computation the regularization results fron the data file provided in rdas directory
  # switch compute to TRUE
  t1 <- Sys.time()
  
  # Compute RMSE for lambdas list using the time bias model
  if (compute) {
    # we do computation
    rmses <- sapply(lambdas, function(l) {
      print(paste0("Regularization, lambda=", l))
      (
        get_time_bias_model(
          train_set,
          test_set,
          train_set_sep,
          test_set_sep,
          l,
          genre_bias_model = NULL
        )
      )$rmse
    })
  }
  else {
    # Load data from the model file
    load(pca_model_file)
    rmses <- pca_model$bias_regularized_model$rmses
    rm(pca_model)
  }
  lambda <- lambdas[which.min(rmses)]
  rmse <- min(rmses)
  print(paste0(
    "Duration = ",
    Sys.time() - t1,
    ", lambda=",
    lambda,
    "",
    ", rmse=",
    rmse
  ))
  time_bias_model <-
    get_time_bias_model(train_set,
                        NULL,
                        train_set_sep,
                        NULL,
                        lambda,
                        genre_bias_model = NULL)
  
  return(
    list(
      time_bias_model = time_bias_model,
      lambdas = lambdas,
      rmses = rmses,
      lambda = lambda,
      rmse = rmse
    )
  )
}
###############################################################################
# Compute the PCA model: First, apply the regularized bias model (Linear model),
# Then, on the residuals, apply PCA, and compute the new residuals  using the
# Weight and patterns matrix.
## Parameters:
#   - train_set: The train set
#   - test_set: The test set
#   - train_set_sep: The train set with separated rows for every movie genre
#   - test_set_sep: The test set with separated rows for every movie genre
#   - lambdas: a list of regularization value to train the linear model used to
#             compute the residuals before applying PCA
#   - bias_regularized_model: the regularized bias model. Default value is NULL.
#                       If the bias regularized model is already computed, we can
#                       pass it as a parameter to this method
## Return values: a named list of 1 entry:
#   - bias_regularized_model: the regularized bias model,
#   - pca: pca object result of prcomp function
#   - optimal_rank: the PC rank that minimize the RMSE
#   - rmse: the minimum rmse value obtained using the optimal_rank
#   - rmses:The RMSE list using all the PC in the range [1,rank]
###############################################################################
get_pca_model <- function(train_set,
                          test_set,
                          lambdas,
                          bias_regularized_model = NULL) {
  #separate rows for train and test set: one row per genre
  train_set_sep <- train_set %>% separate_rows(genres, sep = "\\|")
  test_set_sep <- test_set %>% separate_rows(genres, sep = "\\|")
  # If bias regularized model is not null, we need to check if we use the same parameters
  if (!is.null(bias_regularized_model)) {
    # if we transmit a bias regularized bias model as a method parameter
    # check if the transmitted model is compatible with the function parameters
    if (!identical(lambdas, bias_regularized_model$lambdas))
      stop("The transmitted bias regularized model is not compatible with the used data set")
  } else {
    # compute the bias regularized model first
    print("PCA bias model, compute bias regularized model")
    bias_regularized_model <-
      get_bias_regularized_model(train_set, test_set, train_set_sep, test_set_sep, lambdas)
  }
  # Shortcuts for needed objects
  mu_hat <-
    bias_regularized_model$time_bias_model$genre_bias_model$user_bias_model$movie_bias_model$naive_model$mu_hat
  movie_bias <-
    bias_regularized_model$time_bias_model$genre_bias_model$user_bias_model$movie_bias_model$movie_bias
  user_bias <-
    bias_regularized_model$time_bias_model$genre_bias_model$user_bias_model$user_bias
  genre_bias <-
    bias_regularized_model$time_bias_model$genre_bias_model$genre_bias
  time_bias <- bias_regularized_model$time_bias_model$time_bias
  compute <- T
  # If you want to skip long computation and load the PCA from the data file proivided in rdas directory
  # switch compute to TRUE
  if (compute) {
    # compute residuals on train set and create a matrix of (row=user,col=movie) residuals
    residuals_matrix <- get_bias_model_prediction(train_set,
                                                  train_set_sep,
                                                  mu_hat,
                                                  movie_bias,
                                                  user_bias,
                                                  genre_bias,
                                                  time_bias) %>%
      mutate(residual = rating - rating_hat) %>%
      select(userId, movieId, residual) %>%
      spread(movieId, residual) %>%
      as.matrix()
    # set rownames
    rownames(residuals_matrix) <- residuals_matrix[, 1]
    residuals_matrix <- residuals_matrix[,-1]
    # we need to assign a value to missed values in the matrix
    # as the residuals is a centered at 0, we assign zero
    residuals_matrix[is.na(residuals_matrix)] <- 0
  }
  # Principal components to consider
  # For optimization purpose, we compute only the first 'rank' PCs
  rank <- min(100, nrow(movie_bias), nrow(user_bias))
  t0 <- Sys.time()
  #If PCA modele data file is provided, use it to get the PCA , elsewhere, compute the PCA with prcomp
  if (!compute && file.exists(pca_model_file)) {
    load(pca_model_file)
    pca <- pca_model$pca
    rmses_pca <- pca_model$rmses
    rm(pca_model)
  } else {
    # compute the PCA, we use the "rank." parameter to optimize computation, it prevents prcomp to
    # compute all the possible PC.
    # tol parameter is used to ignore components with low variation
    # Residuals are already scaled and centered as a result of our previous linear model
    pca <-
      prcomp(
        residuals_matrix,
        rank. = rank,
        # max PCs numbers
        tol = sqrt(.Machine$double.eps),
        # ignore very small values
        center = FALSE,
        scale = FALSE
      )
    print(paste0("PCA rank =", rank, ", time = ", Sys.time() - t0))
  }
  # Duration : "PCA rank =10, time = 8h23min"
  #            "PCA rank =100, time = 10.6651770608293=10h40min"  "9h53min"
  # save some RAM, no further needs of residuals matrix
  
  rm(residuals_matrix)
  gc(reset = TRUE,
     full = TRUE,
     verbose = FALSE)
  # get rating predictions matrix using the linear model
  if (compute) {
    bias_model_prediction <- get_bias_model_prediction(test_set,
                                                       test_set_sep,
                                                       mu_hat,
                                                       movie_bias,
                                                       user_bias,
                                                       genre_bias,
                                                       time_bias)  %>%
      select(userId, movieId, rating_hat) %>%
      spread(movieId, rating_hat) %>%
      as.matrix()
    
    # set rownames
    rownames(bias_model_prediction) <- bias_model_prediction[, 1]
    bias_model_prediction <- bias_model_prediction[,-1]
    
    
    # transform test set to rating matrix o (rows=movies, cols=movies)
    test_matrix <- test_set %>%
      select(userId, movieId, rating) %>%
      spread(movieId, rating) %>%
      as.matrix()
    # set rownames
    #rownames(test_matrix) <- test_matrix[, 1]
    test_matrix <- test_matrix[,-1]
  }
  gc(reset = TRUE,
     full = TRUE,
     verbose = FALSE)
  t0 <- Sys.time()
  # for all rank values within the range [1,rank], compute the RMSE between the test set rating (true rating)
  # and the estimated rating computed on with linear model added to residuals computed with the PC transformation
  if (!exists("rmses_pca"))
    rmses_pca <- sapply(1:rank, function(k) {
      t1 <- Sys.time()
      rmse <-
        frobenius(
          test_matrix,
          get_pca_model_prediction(train_set, pca, k, bias_model_prediction)
        )
      print(paste0("k=", k, ", time=", Sys.time() - t1, ", rmse=", rmse))
      gc(reset = TRUE,
         full = TRUE,
         verbose = FALSE)
      return(rmse)
    })
  gc(reset = TRUE,
     full = TRUE,
     verbose = FALSE)
  optimal_rank <- which.min(rmses_pca)
  rmse <- min(rmses_pca)
  print(paste0(
    "Min rmse =",
    rmse,
    ", optimal k=",
    optimal_rank,
    ", time=",
    Sys.time() - t0
  ))
  
  return(
    list(
      bias_regularized_model = bias_regularized_model,
      pca = pca,
      optimal_rank = optimal_rank,
      rmses = rmses_pca,
      rmse = rmse,
      rank = rank
    )
  )
}

###############################################################################
# Get the results on the validation set using the trained PCA model.
## Parameters:
#   - validation_set: The validation set
#   - pca_model: trained PCA model

## Return values: a named list of 1 entry:
#   - rmses: the rmse on the validation set,
#   - pca_model_prediction: a data frame with the estimated rating using 
#                           the PCA model
###############################################################################
get_validation_result <- function(validation_set, pca_model) {
  if (is.null(pca_model))
    # PCA model argument is mandatory
    stop("First you should train a PCA model using get_pca_model(...)")
  #Shortcuts
  mu_hat <-
    pca_model$bias_regularized_model$time_bias_model$genre_bias_model$user_bias_model$movie_bias_model$naive_model$mu_hat
  movie_bias <-
    pca_model$bias_regularized_model$time_bias_model$genre_bias_model$user_bias_model$movie_bias_model$movie_bias
  user_bias <-
    pca_model$bias_regularized_model$time_bias_model$genre_bias_model$user_bias_model$user_bias
  genre_bias <-
    pca_model$bias_regularized_model$time_bias_model$genre_bias_model$genre_bias
  time_bias <-
    pca_model$bias_regularized_model$time_bias_model$time_bias
  
  #Get the bias model rating estimation matrix using the linear model
  bias_model_prediction <- get_bias_model_prediction(
    validation_set,
    validation_set %>% separate_rows(genres, sep = "\\|") ,
    #separate genre data
    mu_hat,
    movie_bias,
    user_bias,
    genre_bias,
    time_bias
  ) %>%
    select(userId, movieId, rating_hat) %>%
    spread(movieId, rating_hat) %>%
    as.matrix()
  # set rownames
  rownames(bias_model_prediction) <-
    bias_model_prediction[, 1]
  bias_model_prediction <-
    bias_model_prediction[,-1]
  pca_model_prediction <-
    get_pca_model_prediction(validation_set,
                             pca_model$pca,
                             pca_model$optimal_rank,
                             bias_model_prediction)
  #True rating matrix
  validation_matrix <- validation_set %>%
    select(userId, movieId, rating) %>%
    spread(movieId, rating) %>%
    as.matrix()
  #rownames(validation_matrix) <- validation_matrix[, 1]
  validation_matrix <- validation_matrix[,-1]
  # compute the RMSE
  rmse <- frobenius(validation_matrix, pca_model_prediction)
  return(list(rmse = rmse, pca_model_prediction = pca_model_prediction))
}
###############################################################################
#########################     Helpers               ###########################
###############################################################################

###############################################################################
# Function that receive a MovieLens data set or sub data set and split it into
# 2 sets, using the proportion parameter.
# Data is splited in such a way that all movies, users, and years in the small
# set are totally included in the big set.
# It rises an error if partition size is less than 2.
## Parameters:
#   - data: the data set to split
#   - proportion: The proportion of the small set, it's a number into ]0,1[,
#                 default value = 0.1
## Return values: A named list of 1 entry:
#   - small_set : Data set with the indicated 'proportion' of data
#   - big_set: The rest of the data
###############################################################################
split_data <-function(data, proportion = 0.1) {
  # indexes for validation set with the indicated proportion of the whole data
  print(paste0("split data, total  rows=", data %>% nrow()))
  ind <-
    createDataPartition(data$rating,
                        times = 1,
                        p = proportion,
                        list = FALSE)
  #print(paste0("ind  length =",ind%>% length()))
  big_set <- data[-ind, ]
  temp <- data [ind, ]
  # Make sure userId, movieId and years in test set are also in train set
  # So we are sure to have test set totally included in train set
  # We do not the semi join on genres column, because there are few genres set values (20 genres), and
  # we suppose that the small set is big enough to cover all  genres within the big set.
  small_set <- temp %>%
    semi_join(big_set, by = "movieId") %>%
    semi_join(big_set, by = "userId") %>%
    semi_join(big_set, by = "years")
  print(paste0("small_set  length =", small_set %>% nrow()))
  # Add rows removed from validation set back into data set
  removed <-
    anti_join(temp, small_set, by = c("userId", "movieId", "years"))
  #print(paste0("removed  length =",removed%>% nrow()))
  big_set <- rbind(big_set, removed)
  #print(paste0("big_set  length =",big_set%>% nrow()))
  #Because of matrix manipulation, we need at least 2 elements in each partition
  if (nrow(small_set) < 2 || nrow(big_set) < 2) {
    stop("Data sample is too small")
  }
  return(list(big_set = big_set, small_set = small_set))
}

###############################################################################
# This function take as entry a movielens data set (or sub data set) set and
# creates work, train, test and validations sets. It can also sample the data
# set before spliting it.
## Parameters:
#   - data: The data set to split
#   - nusers: Distinct users number, used for sampling data, when it's not
#             null, we create a sub data set of 'nusers' distinct users
#             default value = NULL
#   - nmovies: Distinct movies number, used for sampling data, when it's not
#              null, we create a sub data set of 'nmovies' distinct movies
#              default value = NULL
#   - p_validation: the proportion of the validation set, it's a number into
#                   ]0,1[, default value = 0.1
#   - p_test: the proportion of the test set, it's a number into
#                   ]0,1[, default value = 0.1
## Return values: a named list of 1 entry:
#   - validation_set : Validation data set
#   - train_set: Train data set
#   - test_set: Test data set
#   - work_set: Train and Test set before split
###############################################################################
create_data_partitions <- function(data,
                                   nusers = NULL ,
                                   nmovies = NULL ,
                                   p_validation = 0.1,
                                   p_test = 0.1) {
  print(paste0("Initial Data set rows=", data %>% nrow()))
  # Are we sampling?
  if (!is.null(nusers) && !is.null(nmovies)) {
    # get sample ids
    print(paste0("Sampling data, users =", nusers, ", movies=", nmovies))
    user_id_sample <-
      sample(unique(data$userId), nusers, replace = FALSE)
    movie_id_sample <-
      sample(unique(data$movieId), nmovies, replace = FALSE)
    # fiter data set using the sample ids
    data <-
      data %>% filter(movieId %in% movie_id_sample  &
                        userId %in% user_id_sample)
  }
  #Transform data before creating partitions
  #Years: years between movie launch date (in the title) and movie rating date (timestamp field)
  data <- data %>%
    mutate(years = year(as_datetime(timestamp)) -  as.numeric(str_match(title, pattern) [, 2]))
  # first split: create work and validation data sets
  work_validation_sets <- split_data(data, p_validation)
  work_set <- work_validation_sets$big_set
  validation_set <- work_validation_sets$small_set
  print(paste0("Work set rows=", work_set %>% nrow()))
  print(paste0("Validation set rows=", validation_set %>% nrow()))
  rm(work_validation_sets)
  # second split: create train and test data sets
  train_test_sets <- split_data(work_set, p_test)
  train_set = train_test_sets$big_set
  test_set = train_test_sets$small_set
  print(paste0("Train set rows=", train_set %>% nrow()))
  print(paste0("Test set rows=", test_set %>% nrow()))
  return(
    list(
      work_set = work_set,
      validation_set = validation_set,
      train_set = train_set,
      test_set = test_set
    )
  )
}

###############################################################################
## Function to compute the RMSE
## Parameters:
#   - rating_hat: The estimated rating list
#   - rating: The true rating list
## Return values: A number representing the RMSE value
###############################################################################
RMSE <- function(rating, rating_hat) {
  sqrt(mean((rating_hat - rating) ^ 2))
}
###############################################################################
## Function to compute the prediction (rating_hat) for a given data set using
#  the linear model (bias regularized model)
## Parameters:
#   - data_set: The data set to be predicted
#   - data_set: Same as data_set, but with separated rowd dor every genre value
#   - mu_hat: The estimated rating average on the trained model
#   - movie_bias: a data frame with movie bias values in train set
#   - user_bias: a data frame with user bias values in train set
#   - genre_bias: a data frame with genre bias values in train set
#   - time_bias: a data frame with the time bias values in train set
## Return values: A data frame with the estimated rating value
#                 column "rating_hat"
###############################################################################
get_bias_model_prediction <- function(data_set,
                                      data_set_sep,
                                      mu_hat,
                                      movie_bias,
                                      user_bias,
                                      genre_bias,
                                      time_bias) {
  data_set_sep %>%
    left_join(genre_bias, by = "genres") %>%
    group_by(userId, movieId) %>%
    summarize(bg_hat = sum(beta_g)) %>%  #Sum all the genre biais
    left_join(data_set, by = c("userId", "movieId")) %>%
    left_join(movie_bias, by = "movieId") %>%
    left_join(user_bias, by = "userId") %>%
    left_join(time_bias, by = "years") %>%
    mutate(rating_hat = mu_hat + bi_hat + bu_hat + bg_hat + bt_hat)
}

###############################################################################
## Function to compute the PCA model predictions for a given data set using
#  the linear model predictions (bias regularized model) and the PCA
## Parameters:
#   - data_set: The data set to be predicted
#   - pca: The principal components analysis as result of the prcomp function
#   - r: The principal components number to be used for the prediction
#   - bias_model_prediction: a matrix of bias model predictions for the data set
## Return values: A data frame with the estimated rating value
#                 column "rating_hat"
###############################################################################
get_pca_model_prediction <- function(data_set, pca, r, bias_model_prediction) {
  # Add the PCA residuals to the estimated rating of the linear models using the specific PC rank
  # linear model rating + (W * t(P))
  pca_model_prediction <- bias_model_prediction +
    (pca$x[rownames(bias_model_prediction), 1:r] %*% t(pca$rotation[colnames(bias_model_prediction), 1:r]))
  return(pca_model_prediction)
}
###############################################################################
########################Script begins run here ################################
###############################################################################
# For dplyr >= 1.0.0 : To not display the message:
# summarise()` regrouping output by 'genres' (override with `.groups` argument)
options(dplyr.summarise.inform = F)

# # if using R 3.5 or earlier, use `set.seed(1)` instead
set.seed(1, sample.kind = "Rounding")

# five decimal digits
options(digits = 5)

#Pattern to extract the movie year in the title
pattern <- "\\((\\d{4})\\)$"

#Regularization list parameter
lambdas <- seq(3.5, 6, by = 0.25)

# pca_model_file is The file where is stored the PCA model
# the Rmd document is inside 'doc' directory, and the data file is in 'rdas'
# This variable is filled to TRUE in the report file (Rmd) to indicate that
# we are generating the report
if (!exists("generate_report") || !generate_report) {
  # we are executing directly the code from the project root directory
  pca_model_file <- "rdas/pca_model_v.1.0.rda"
 } else {
   # we are sourcing the file from the Rmd report, so, we need to use the 
   # relative path "../" to step into the data directory
  pca_model_file <- "../rdas/pca_model_v.1.0.rda"
}

# Get the data from text file
dl <- tempfile()
print("Download data file from web page")
#download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
download.file("http://localhost/~odahhani/ml-10m.zip", dl)
#Load data into the RAM
ratings <-
  fread(
    text = gsub("::", "\t", readLines(unzip(
      dl, "ml-10M100K/ratings.dat"
    ))),
    col.names = c("userId", "movieId", "rating", "timestamp")
  )
#Wrangling data
movies <-
  str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <-
  as.data.frame(movies, stringsAsFactors = FALSE) %>% mutate(
    movieId = as.numeric(movieId),
    title = as.character(title),
    genres = as.character(genres)
  )
movielens <- left_join(ratings, movies, by = "movieId")

#Create edx set, validation set, train set, test set
data_sets <- create_data_partitions(movielens)
edx <- data_sets$work_set
validation <- data_sets$validation_set
train <- data_sets$train_set
test <- data_sets$test_set
rm(ratings, movies, dl, data_sets)
unlink("ml-10M100K", recursive = TRUE)

gc(reset = TRUE,
   full = TRUE,
   verbose = FALSE)
#To RUN the script on a sample data set, assign TRUE to sample variable
# Elsewhere, it's the entire movilens that will be used
# create sample data sets
sample <-F
if (sample) {
  sample_data_sets <-
    create_data_partitions(edx, nusers = 400, nmovies = 400)
  train_set <- sample_data_sets$train_set
  test_set <- sample_data_sets$test_set
  validation_set <- sample_data_sets$validation_set
} else {
  train_set <- train
  test_set <- test
  validation_set <- validation
}
rm(sample_data_sets, pattern)
generate_model <- TRUE
## If we are generating the report file, we don't have to build the model here
if (!exists("generate_report") || !generate_report) {
  pca_model <- get_pca_model(train_set, test_set, lambdas)
  validation_result <-
    get_validation_result(validation_set, pca_model)
}