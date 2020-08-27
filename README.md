# 1. Summary
Building a recommendation system of movies based on user rating. Data set used is Movielens 10M
We tried the following linear models in an incremental way:

* Naive model: the avergage
* Movie bias model
* User bias model
* genre bias model
* time bias model: we chose as time criteria the years between the movie launch and the rating date
* Regularized bias model: Regularization of the time bias model

Then we used Matrix Factorization on the residuals of the linear model, applying Principal Components Analysis (PCA) techniques to enhance the prediction.

# 2. Project File

* R/movielens-core.R: R code source to train the models and get the predictions
* doc/movielens_report.Rmd Rmarkdown report based on the movielens-core. It renders only PDF format.
* doc/edx-logo-hearder.png: the logo of rdx, used in the report file
* rdas/pca_model_v1.0.rda: the rda data file of the PCA model. Used if you want to skip heavy computations(PCA or regularized model). The structure of the model is described in the function get_pca_model of (R/moviens-core.R)

# 3. Train the model, Validation

In the R console, be sure to be in the movielens root project directory, and run the script:
`source R/movielens-score.R`.

### 3.1 With the full MovieLens 10M dataset
Be sure that the following variables are set correctly in the R/movielens-core.R:

* `sample <- FALSE  #use the entire MovieLens 10M data set`
* `compute <- TRUE      #do all the needed computations`

Results are in the validation_result object, its structure is described in the function get_validation_result.  
On a MacBook Pro, processor 2.8 GHz Intel Core i7, 16 GB RAM, 1 TB SSD Hard Drive,  iOs 10.13.6, R version is 3.6.3 it would take almost 15 hours.  

### 3.2 With a data sample

You can build a model on a sample, it's fast to run and it's useful for dev/test purpose. You need to edit the script

* `sample <-TRUE #use a data sample`
* `compute <- TRUE   #do all the needed computations`

Results are in the validation_result object, its structure is described in the function get_validation_result. 

Default data sample is 400 users and 400 movies. If you need modify sample size, you edit the script and modify the parameter values (nusers,nmovies)  of the sampling function :

`r 
create_data_partitions(edx, nusers = 400, nmovies = 400) `

### 3.3 Use the provided model data to skip heavy computation time

We provide the data file 'rdas/pca_model_v1.0.rda', it contains the PCA model data result of training the PCA model on the MovieLens 10M. It could be useful if you want to run the model on the whole MovieLens 10M, but you don't have the computational resources or you simply don't want to wait several hours to get the results.  
Be sure that the following variables are set correctly in the R/movielens-score.R 

* `sample <-FALSE  #use the entire MovieLens 10M data set`
* `compute <- FALSE    #do not heavy computational tasks, use the provided data model`

# 4. Generate the report:

The report source code "doc/movielens_report_v1.0.Rmd"" uses the script "R/movielens-core.R" as a library to build, train and validate all the models. No need to run the R script before generating the report.   
To generate the report, use the R console, be sure to be in the movielens root project directory, and run the following command: 
`rmarkdown::render("doc/movielens_report_v1.0.Rmd","pdf_document")`


### 4.1 With the full MovieLens 10M dataset

If you want to generate the report using the entire MovieLens 10M data set, be sure to have the following variables set correctly in the R/movielens-core.R file:

* `sample <- FALSE #use the entire MovieLens 10M data set`
* `compute <- TRUE     #do all the needed computations`


### 4.2 With a data sample

If you want to generate the report using sample data, be sure to have the following variables set correctly in the R/movielens-core.R file:

* `sample <- TRUE     #use a data sample`
* `compute <- TRUE        #do all the needed computations`

Default data sample is 400 users and 400 movies. If you need modify sample size, you edit the script and modify the parameter values (nusers,nmovies)  of the sampling function :

`r 
create_data_partitions(edx, nusers = 400, nmovies = 400) `

### 4.3 Use the provided model data to skip heavy computation time

We provide the data file 'rdas/pca_model_v1.0.rda', it contains the PCA model data result of training the PCA model on the whole MovieLens 10M data set. It could be useful if you want to generate the report on the entire MovieLens 10M, but you don't have the computational resources or you simply don't want to wait several hours to get the pdf file.  
Be sure that the following variables are set correctly in the R/movielens-score.R 

* `sample <-FALSE #use the entire MovieLens 10M data set`
* `compute <-FALSE    #do not heavy computational tasks, use the provided data model`



 
