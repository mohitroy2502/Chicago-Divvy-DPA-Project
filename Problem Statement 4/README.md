# Chicago-Divvy-DPA-Project Problem Statement 4

Predicting the need of bikes according to season in Chicago in order to ensure that the customers do not have to wait for more bikes, by ensuring the increase or decrease in the number or bikes at the docking station to maintain the profit. We have opted for the most frequently used Divvy station id=35 for the given problem statement. The programming language used here is R (version 3.6).

Steps taken to solve the problem statement:

Step 1: Creating the Weather Dataset 2017-19 using Dark Sky API.

Step 2: Creating the Available Bikes 2017-2019 using RSocrata API.

Step 3: Merging the Divvy Trip Quarterly Data into 2017-2019 data.

Step 4: Cleaning and Normalizing the Dataset.

Step 5: Plotting some graphs and maps for data visualization.

Step 6: Creating the Train(2017-18) and Test(2019) datatset. Target Variable: Trip count on an hourly basis. Independent variable: Available bikes on an hourly basis, date, hour and the weather icon provided by Dark Sky.

Step 7: Running 4 Algorithms: Multiple Linear Regression, Ridge Regression, Lasso Regression and ElasticNet Regression.

Step 8: Tuning the hyperparameters.

Step 9: Calculating RMSE on Test Data and comparing the values.

Step 10 (Conclusion): ElasticNet proves to be most accurate model amongst the four models with lowest RMSE.

