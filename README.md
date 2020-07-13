# Prediction of used-car quality using machine learning (in R).

## About
The current project aims to perform the prediction of used cars quality by using machine learning. As this field of research has only starting to gain attention, the reluctance of most car vendors to reveal their research findings may have indirectly contributed to the lack of literature on this regard. However, the rapid expansion of the used car industry may lead to an influx of number of used cars to be evaluated by the used car dealers. Consequently, the increased number of vehicles in varying conditions and types may overwhelm the dealers’ ability to correctly discern the vehicle quality. Machine learning has been proposed as a viable solution as the judgment of the vehicle quality can be automated by analysing the vehicle attributes. The findings of the current project revealed that machine learning may be the suitable solution to predict vehicle quality. Particularly, most models were able to detect large portion of the bad buy (vehicle that suffer from irreparable damage). 

## How to open the R programme
```
1. This assignment consists of 4 R programmes.
	a.) aml.assignment.R (will consist of all the data exploration, missing values imputation, outlier 
	    treatment and normality checking.
	b.) LogisticRegR.R (will consist of all the step in performing logistic regression)
	c.) NaiveBayesR (will consist of all the step in performing naive bayesian)
	d.) RandomForestR (will consist of all the steps in performing random forest)

2. File a (aml.assignment.R) has to be run before performing file b,c, and d for obtaining the cleaned dataset (kickDataset_4_Transformed.csv)
3. For your information, i will include the cleaned dataset and dataset which has not be performing any cleansing into this folder.
4. Kindly run b, c, d by setting working directory that has csv (kickDataset_4_Transformed).
5. kindly run a by setting working directory that has csv (kick.csv).

In this folder, we will have final document, 4 R scripts, 2 dataset (cleaned and uncleaned dataset)
```

## How to get the dataset
```
1. Download kick dataset: https://www.openml.org/d/41162 Or Open dataset/kick (original data)
2. Open the preprocessed data with dataset/kickDataset_4_Transformed
```


## Problem statement
The large amount and variety of used cars with different levels of quality makes the judgment of the vehicle quality an overwhelming task for the dealers. Aside from the used car dealers, the consumers may also be less confident towards used cars and may consequently affect the industry.  Given the financial and reputational consequences of purchasing used cars that suffers from serious mechanical issues, the identification of factors that can help to determine the quality of used cars will be very useful to the dealers and the consumers. Although recent works in the field of machine learning has demonstrated some promising results, the serious lack of empirical findings still make the field an uncharted ground. 
	Stemming from this, the current project aims to explore the various research conducted on such regard, the current report will be organized into a few sections. The section for related works will first review the previous studies conducted on the usage of machine learning in vehicle quality inspection whereby the choice of algorithms and features selection will be critically discussed. Following the related section, the procedures taken for data pre-processing is also discussed. Then, the implementation of chosen algorithms is also outlined, and the results of each algorithm is compared and discussed. Additionally, the evaluation metrics for the present work is also introduced. Recommendations and findings are also discussed at the end of the report. 

## Research Aim and Objectives
The aim of the current work is to predict the quality of used cars using machine learning. In order to achieve the research aim, a few objectives must be fulfilled.
1.	To review past research conducted on vehicle inspection and identify relevant factors influencing quality of used cars. 
2.	To develop 3 machine learning models and experiment with different model parameters. 

## The best performing model from naive bayes, logistic regression and random forest
![rex](best-performing-model.png)

