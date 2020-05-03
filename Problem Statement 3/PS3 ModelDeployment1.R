### Model deployment via RESTful API
# install.packages('plumber')
library('plumber')


# Load in our saved model
load(file = 'C:/Users/manis/Desktop/SVM.rda')
load(file = 'C:/Users/manis/Desktop/DT.rda')
load(file = 'C:/Users/manis/Desktop/RF.rda')

# We can use the plumber package to do this
# We add comments in a special structure.
# D = "2019-01-17"

#* @post /predictSVM
predictSVM <- function(x){
  y_pred3 = predict(classifier3, newdata = Test)
  Test$Maintenance = y_pred3
  x = as.Date(x)
  xx=unique(Test[Test$MainDate==x & Test$Maintenance==1,c("bikeid", "Weather_Icon")])
  # l = length(xx)
  return(xx)
  # return(l)
}

#* @post /predictDT
predictDT <- function(x){
  y_pred1 = predict(classifier1, newdata = Test, type = 'class')
  Test$Maintenance = y_pred1
  x = as.Date(x)
  xx=unique(Test[Test$MainDate==x & Test$Maintenance==1,c("bikeid","Weather_Icon")])
  # l = print("Number of bikes to be put for Maintenance :", length(xx))
  return(xx)
  # return(l)
}

#* @post /predictRF
predictRF <- function(x){
  y_pred2 = predict(classifier2, newdata = data.frame(Test$TotalDuration, Test$No.Of.Trips, Test$Weather_Icon, Test$TotalTrips))
  Test$Maintenance = y_pred2
  x = as.Date(x)
  xx=unique(Test[Test$MainDate==x & Test$Maintenance==1,c("bikeid", "Weather_Icon")])
  return(xx)
}

# predictSVM(D)
# predictRF("2019-02-18")

