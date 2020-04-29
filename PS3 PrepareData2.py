# -*- coding: utf-8 -*-
"""
Created on Sat Apr 11 04:27:24 2020

@author: Manish Tandel
"""
import pandas as pd

RQ12017 = pd.read_csv('Q1_2017.csv', delimiter=',', usecols = ["bikeid", "TotalDuration", "No.Of.Trips", "Maintenance", "MainDate"])
Q1_2017 = pd.read_csv('Divvy_Trips_2017_Q1.csv')
Q1_2017 = pd.DataFrame(Q1_2017)
Q1_2017['tripduration'] = pd.to_numeric(Q1_2017['tripduration'], errors='coerce')

RQ22017 = pd.read_csv('Q2_2017.csv', delimiter=',', usecols = ["bikeid", "TotalDuration", "No.Of.Trips", "Maintenance", "MainDate"])
Q2_2017 = pd.read_csv('Divvy_Trips_2017_Q2.csv')
Q2_2017 = pd.DataFrame(Q2_2017)
Q2_2017['tripduration'] = pd.to_numeric(Q2_2017['tripduration'], errors='coerce')

RQ32017 = pd.read_csv('Q3_2017.csv', delimiter=',', usecols = ["bikeid", "TotalDuration", "No.Of.Trips", "Maintenance", "MainDate"])
Q3_2017 = pd.read_csv('Divvy_Trips_2017_Q3.csv')
Q3_2017 = pd.DataFrame(Q3_2017)
Q3_2017['tripduration'] = pd.to_numeric(Q3_2017['tripduration'], errors='coerce')

RQ42017 = pd.read_csv('Q4_2017.csv', delimiter=',', usecols = ["bikeid", "TotalDuration", "No.Of.Trips", "Maintenance", "MainDate"])
Q4_2017 = pd.read_csv('Divvy_Trips_2017_Q4.csv')
Q4_2017 = pd.DataFrame(Q4_2017)
Q4_2017['tripduration'] = pd.to_numeric(Q4_2017['tripduration'], errors='coerce')

#for b in range(0,len(dst["bikeid"])):
#    t = 0
#    c = 0
#    Q=Q1_2017.loc[Q1_2017["bikeid"]==dst["bikeid"][b],:]    
#    Q=Q.sort_values(by=['start_time'])
#    Q=Q.reset_index(drop=True)
#    for i in range(0,len(Q["bikeid"])):
#        if(t < 110000 or c < 80):
#            t = t + Q["tripduration"][i]
#            c = c + 1
#            dst["MainDate"][b] = Q["start_time"][i]
        
# 2018
RQ12018 = pd.read_csv('Q1_2018.csv', delimiter=',', usecols = ["bikeid", "TotalDuration", "No.Of.Trips", "Maintenance", "MainDate"])
Q1_2018 = pd.read_csv('Divvy_Trips_2018_Q1.csv')
Q1_2018 = pd.DataFrame(Q1_2018)
Q1_2018['tripduration'] = pd.to_numeric(Q1_2018['tripduration'], errors='coerce')

RQ22018 = pd.read_csv('Q2_2018.csv', delimiter=',', usecols = ["bikeid", "TotalDuration", "No.Of.Trips", "Maintenance", "MainDate"])
Q2_2018 = pd.read_csv('Divvy_Trips_2018_Q2.csv')
Q2_2018 = pd.DataFrame(Q2_2018)
Q2_2018['tripduration'] = pd.to_numeric(Q2_2018['tripduration'], errors='coerce')

RQ32018 = pd.read_csv('Q3_2018.csv', delimiter=',', usecols = ["bikeid", "TotalDuration", "No.Of.Trips", "Maintenance", "MainDate"])
Q3_2018 = pd.read_csv('Divvy_Trips_2018_Q3.csv')
Q3_2018 = pd.DataFrame(Q3_2018)
Q3_2018['tripduration'] = pd.to_numeric(Q3_2018['tripduration'], errors='coerce')

RQ42018 = pd.read_csv('Q4_2018.csv', delimiter=',', usecols = ["bikeid", "TotalDuration", "No.Of.Trips", "Maintenance", "MainDate"])
Q4_2018 = pd.read_csv('Divvy_Trips_2018_Q4.csv')
Q4_2018 = pd.DataFrame(Q4_2018)
Q4_2018['tripduration'] = pd.to_numeric(Q4_2018['tripduration'], errors='coerce')

# 2019
RQ12019 = pd.read_csv('Q1_2019.csv', delimiter=',', usecols = ["bikeid", "TotalDuration", "No.Of.Trips", "Maintenance", "MainDate"])
Q1_2019 = pd.read_csv('Divvy_Trips_2019_Q1.csv')
Q1_2019 = pd.DataFrame(Q1_2019)
Q1_2019['tripduration'] = pd.to_numeric(Q1_2019['tripduration'], errors='coerce')

RQ22019 = pd.read_csv('Q2_2019.csv', delimiter=',', usecols = ["bikeid", "TotalDuration", "No.Of.Trips", "Maintenance", "MainDate"])
Q2_2019 = pd.read_csv('Divvy_Trips_2019_Q2.csv')
Q2_2019 = pd.DataFrame(Q2_2019)
Q2_2019['tripduration'] = pd.to_numeric(Q2_2019['tripduration'], errors='coerce')

RQ32019 = pd.read_csv('Q3_2019.csv', delimiter=',', usecols = ["bikeid", "TotalDuration", "No.Of.Trips", "Maintenance", "MainDate"])
Q3_2019 = pd.read_csv('Divvy_Trips_2019_Q3.csv')
Q3_2019 = pd.DataFrame(Q3_2019)
Q3_2019['tripduration'] = pd.to_numeric(Q3_2019['tripduration'], errors='coerce')

RQ42019 = pd.read_csv('Q4_2019.csv', delimiter=',', usecols = ["bikeid", "TotalDuration", "No.Of.Trips", "Maintenance", "MainDate"])
Q4_2019 = pd.read_csv('Divvy_Trips_2019_Q4.csv')
Q4_2019 = pd.DataFrame(Q4_2019)
Q4_2019['tripduration'] = pd.to_numeric(Q4_2019['tripduration'], errors='coerce')

     
def DataPrepFunc (x, y):
    for b in range(0,len(y["bikeid"])):
        t = 0
        c = 0
        Q=x.loc[x["bikeid"]==y["bikeid"][b],:]
        Q=Q.sort_values(by=['start_time'])
        Q=Q.reset_index(drop=True)
        for i in range(0,len(Q["bikeid"])):
            if(t < 110000 or c < 80):
                t = t + Q["tripduration"][i]
                c = c + 1
                y["MainDate"][b] = Q["start_time"][i]
    y.to_csv("Q2_2019_.csv")
        

DataPrepFunc(Q1_2017, RQ12017)    
DataPrepFunc(Q2_2017, RQ22017) 
DataPrepFunc(Q3_2017, RQ32017)
DataPrepFunc(Q4_2017, RQ42017)    

DataPrepFunc(Q1_2018, RQ12018)    
DataPrepFunc(Q2_2018, RQ22018) 
DataPrepFunc(Q3_2018, RQ32018)
DataPrepFunc(Q4_2018, RQ42018)  
        
DataPrepFunc(Q1_2019, RQ12019)    
DataPrepFunc(Q2_2019, RQ22019) 
DataPrepFunc(Q3_2019, RQ32019)
DataPrepFunc(Q4_2019, RQ42019)          
        