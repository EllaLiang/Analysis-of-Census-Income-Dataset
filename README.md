# Analysis of Census Income Dataset
In this project we analyze a U.S. census data taken from the UCI Machine Learning Repository. 
Our final goal is to build a model, which can predict whether the income of a random American citizen is less or greater than 50000$ a year based on given features, such as age, education, occupation, gender, race, etc. 
The project is divided into three parts: Cleaning and Preprocessing the Data, Exploratory Data Analysis and Predictive Analysis. We begin with data cleaning and data visualization, we also perform dimension reduction and clustering. 
Then we build seven predictive models, conduct the performance evaluation by using ROC curve and AUC to find the one with the highest prediction accuracy.

# Data Set
The Census Dataset is provided by UC Irvine Machine Learning Repository. The dataset is in the data folder. It contains adult.data for training and adult.test for testing. It describes 15 variables on a sample of individuals from the US Census database. Please see [UCI Website](https://archive.ics.uci.edu/ml/datasets/adult) for more details and attribute information.

# Machine Learning Models
- Logistic Regression
- Decision Tree and Random Forest
- Neural Network
- Naive Bayes
- KNN
- SVMs

# Result
The Neural Network is the strongest classifier of the seven methods. The Random Forest classifier also gives very high prediction accuracy on test set. Please see the Report for more details.

# File Structure

```
 README.md
 report/
    Report.pdf
 data/
    adult.data
    adult.test
    adult.names
 code/
 ```
 
# Team
Team member: Xue Cao, Xiyu (Ella) Liang, Ke (Vicky) Xu
