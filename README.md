# Cleaning-Student-Data

INTRODUCTION
This project is made in partial fulfillment of the Service Design Innovation Project for my master degree. The student data is from the University of Maastricht (UM). Do note that this piece of code is part one of four for the entire project. However, since the purpose of this github account is to serve as my portfolio and proof of my coding capabilities, I will only include code that I have personally written. With that being said, this piece of code is made by me and another group member, Robbert Lalisang. The whole code was written by me, except from lines 117-1148, which was Mr. Lalisang's part. 

BUSINESS PROBLEM
The university has tasked us to create a disruptive business idea and provided student data for us to work with. Our group was assigned to create a service for the internal communications department of the unviersity. We worked around the idea of creating a tool for the marketing department to see which bachelor students are likely to become master studets so they can make tailored marketing strategies.

DATA PROBLEM 
In order to create tailored marketing strategies, we need to predict the probability of a student continuing their master studies at UM. This can be done by using a classification type machine learning model. In order to do that, we have to clean the data first. 

DATA SOLUTION
This code exhibits my skills in data cleaning, wrangling, and feature engineering. It served as a learning experience in getting proficient with different packages, especially the dplyr. The main challenge is in finding alternatives to inefficient for loops. For more details about the project, one can read it in the code itself. It is heavily documented via comments. 

BUSINESS SOLUTION
This code was the first step to creating a machine learning model. Since the models were not made by me, I cannot post the code as part of my portfolio. But for the sake of completeness, the cleaned data was fed to several preliminary models: logistic regression, SVM, ANN, Random Forest, and Gradient Boosting. We focused on tweaking Random Forest models, one for each specialization of the student. From this, we proposed dividing the students into four groups: Loyalists (0.75-1 probabilit), Hostages (0.5-0.75), Defectors (0.25-0.5), and Mercenaries (0-0.25). Based on the characteristics of the student during their bachelor, their probability of staying can be predicted with up to 79% accuracy. With this information, the marketing department can make tailored marketing strategies. 

If this project interests you, I have included the full report called "Final-Report-Team-8.pdf"
