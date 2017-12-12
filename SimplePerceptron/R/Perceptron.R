library("devtools")
library(roxygen2)

setwd("parent_directory")
create("SimplePerceptron")

# The function accepts a dataframe from the user
# It also accepts the learning rate - learn, and number of iterations needed - iterations
simple_perceptron <- function(x, y, learn, iterations)
{
  # initialize weight vector
  weight_vector <- rep(0, dim(x)[2] + 1)
  err <- rep(0, iterations)
  

  for (i in 1:iterations)
  {
    
    # looping through training data set
    for (j in 1:length(y)) 
    {
      z <- sum(weight_vector[2:length(weight_vector)] * 
                 as.numeric(x[j, ])) + weight_vector[1]
      if(z < 0) 
      {
        pred_y <- -1
      } 
      else 
      {
        pred_y <- 1
      }
      
      # Change weight - the formula doesn't do anything 
      # if the predicted value is correct
      weightdiff <- learn * (y[j] - pred_y) * c(1, as.numeric(x[j, ]))
      weight_vector <- weight_vector + weightdiff
      
      # Updating error function
      if ((y[j] - pred_y) != 0.0) 
      {
        err[i] <- err[i] + 1
      }
    }
  }

  print(weight_vector)
  plot(1:10, err, type="l", lwd=2, col="blue", xlab="epoch #", ylab="errors")
  title("Errors vs epoch - learning rate eta = 1")
  return(err)
}