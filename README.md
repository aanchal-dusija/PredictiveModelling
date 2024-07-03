# Predictive Modeling of Flight Delays
This repository is dedicated to the project "Predictive Modeling of Flight Delays", which aims to analyze and predict flight delays using various statistical and machine learning techniques. This project is a comprehensive study that takes into account various factors contributing to flight delays and attempts to predict them with the highest accuracy possible.

## Project Overview
The study focuses on domestic flight data from the United States, specifically from January 2018 to October 2018 from Bureau of Transportation Statistics. It utilizes predictive modeling techniques to identify patterns and factors leading to flight delays, aiming to provide insights that could help in mitigating these delays and improving airline efficiency and passenger satisfaction.

## Key Objectives
- To describe and construct a predictive modeling engine using machine learning and statistical models to quantify flight delays in advance.
- To identify and highlight significant factors that contribute to flight delays, providing insight into the root causes.
- To compare various models and determine the one which best fits and performs on the dataset.

## Methodology
The project involves several statistical and machine learning techniques, including:
- Multiple Linear Regression (MLR): To model the quantitative response based on multiple predictor variables.
- Binary Logistic Regression: For modeling the log odds of the dependent variable as a linear combination of the independent variables.
- Decision Trees: For both regression and classification tasks, involving stratifying or segmenting the predictor space into several simple regions.
- Bagging and Random Forest: Techniques used to improve stability and accuracy of decision trees, reduce variance, and avoid overfitting.

## Exploratory Data Analysis
Initial exploration to understand the distribution of data, missing value treatment, and outlier detection using various statistical methods.

## Model Evaluation
Comparative analysis of different models based on Root Mean Squared Error (RMSE) for regression models and Accuracy, Misclassification Error, AUROC, and Concordance for classification models.

## Key Findings
- Significant Variables: Late Aircraft Delay and Carrier Delay were found to be significant variables affecting on-time flight departure.
- Model Performance: Random Forest technique outperformed other models based on the evaluation criteria, with Binary Logistic regression providing high accuracy for classification tasks.
- Insights: Identified specific airports with higher proportions of delays, potentially guiding more focused improvement efforts.

## Limitations and Future Scope
- The study is limited by the number of variables considered and the time frame of the data. Future expansions could include more exhaustive variables and longer periods.
- Advanced techniques like Principle Component Analysis, Artificial Neural Networks, or Support Vector Machines could be explored for potentially better performance.
- A similar analysis for other countries' flight data could provide more comprehensive insights and help understand region-specific factors.
