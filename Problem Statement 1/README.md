# Chicago-Divvy-DPA-Project
Problem Statement: 

Predicting which station needs more docks on the basis of the traffic ( to and fro) at the station. This is also by keeping in mind the revenue generated v/s the cost required to build more docks


ML Model Applied: 

knn Classification
Logistic Regression
Naive Bayes Classification

Conclusion: 

Out of three ML algorithms applied it can be observed that knn Classification gives the best result. Majority of stations that needed upgrade in docks number are in Central Chicago where most probably daily office goers must be using divvy. Whereas stations that needs to cut short on number of docks are also identified which can be used to save money and use them for stations where there is need to increase docks


In order to load model from .sav file use following command:

import pickle
loaded_model = joblib.load(filename)
