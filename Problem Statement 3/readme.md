# Problem Statement 3

At what time/season can the bikes be put for maintenance so that the Divvy company does not lose any revenue by putting the bikes into maintenance.  

Description: On a given day, which bikes can be put to maintenance based on the bike usage (trip duration), the weather, and the need for the bike on that day. Considering the need of the bike ensures that Divvy doesn’t lose any customers or the profits.

End Result: After providing the Date to the model, it returns the bike ids that need to be put on maintenance based on the weather of that day

Models Used: The algorithms used to obtain the required results are Decision Tree Algorithm, the Random Forest Algorithm, and the Support Vector Machine algorithm.

Decision Tree: On 2019-01-17, using Decision Tree it is estimated by the model that Divvy can put 7 bikes on maintenance to ensure apt availability. Accuracy for decision tree was observed to be 82.15%

Random Forest: On 2019-01-17, using Random Forest it is estimated by the model that Divvy can put 3 bikes on maintenance to ensure apt availability. Accuracy for decision tree was observed to be 76%
  
Support Vector Machine: On 2019-01-17, using SVM it is estimated by the model that Divvy can put 13 bikes on maintenance to ensure apt availability.  Accuracy for decision tree was observed to be 69.27%

Conclusion: Out of three algorithms, the Decision Tree model gives the best result with an accuracy of 82.15%. It then rightly predicts the bikeIds of the number of bikes that can be put on maintenance on a given day considering the weather.

