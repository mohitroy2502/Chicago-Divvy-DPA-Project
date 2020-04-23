# Chicago-Divvy-DPA-Project

## Problem Statement 2

Identifying the need and feasibility (revenue) for new stations between 2 stations for providing parking spaces at heavy traffic stations. This is to ensure that divvy does not lose customers, based on the parking docks unavailability.

Description: Heavy stations are identified as stations from where most trips start and most trips end and not whose dock capacity is initially larger because we need to identify stations which are the busiest.

Ml Models-
1. Multi regression and Correlation
Applying Multi regression model to find out the relationship between variables to determine our target variable and predictors which in this case was also visually analyzed by correlation plot.
Departure and arrival for all 621 stations was calculated and then the difference between arrival and departure was calculated in order to predict the excess or deficit in docks.

Conclusion: Assuming $400 for each dock and median docks in each station is 23 and for 22 extra stations we require about 200k $ for all these developments which is well in the budget of Divvy.

2. KNN classification
Using optimal k value we could categorize Dock_diff in the manner of ‘Yes’ or ‘No’(0,1) depending on if they needed a new dock station or not.
This was calculated with the help of predictor variables and target variable which were outcome of correlation plot.

Conclusion: There are 25 dock stations which require an additional dock stations. Total estimated cost of building these 25 dock stations is around 230k $ if dock stations are setup as per this prediction model.

3. Logistic Regression
Applying logistic regression to predict number of dock stations required based on predictor variables duration.mins, to_station_id, Longitude, Latitude, Total.docks identified using correlation matrix. 
Obtained probabilities were rounded off only when greater than 0.8 and costing was calculated assuming each dock setup requires $400.

Conclusion: There are 29 dock stations which require an additional dock stations. Total estimated cost of building these 29 dock stations is around 260k $ if dock stations are setup as per this prediction model.

Final Conclusion-
Out of three ML algorithms applied it can be observed that KNN Classification gives the best result because if we remove outliers the remaining dock stations that require new dock stations between them are 24 which is closes to the prediction of KNN classifier.  Divvy can either add in more docks at particular stations or open new dock stations even though opening new stations will attract new customers. So it is better to add in new stations between the predicted station names.
