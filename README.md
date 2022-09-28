# Exploratory Data Analysis
To help researcher performing primary steps in analyzing data, an annotated script 'EDA.R' is provided, which is intented to serve as a guide for a systematic exploration of typical environmental data with R.

Understanding the structure, basic properties and relationships in a given dataset is a fundamental prerequisite for an appropriate statistical analysis. This code provides a roadmap for a systematic and reproducible analysis based on key questions. It demonstrates how several techniques can be used and evaluated in order to address common tasks of data analysis such as understanding the structure of the dataset, detecting temporal and spatial dependence among observations, identifying outliers and influential observations, checking normality and homogeneity of model residuals and exploring the relationships of variables.

## Outline

1. SETUP R
2. IMPORT AND OVERVIEW
2.1. Inspect and format raw data
2.2. Screen for missing values in the data
2.3. Screen for zeros in the data
2.4. Count observations per group
2.5. Locate measurement points
3.INDEPENDENCE OF OBSERVATIONS
3.1. Inspect the response vs. space
3.1.1. ... for single time points
3.1.2. ... aggregating time points
3.2. Inspect the response vs. time
4. OUTLIERS
4.1. Draw boxplots
4.2. Draw Cleveland dotplots
4.3. Draw scatterplots
4.4. Perform formal tests
4.5. Identify influential observations
5. NORMALITY AND HOMOSCEDASTICITY
5.1. Inspect the distribution of the response
5.2. Inspect the group-specific spread of the response
6. RELATIONSHIPS OF VARIABLES
6.1. Inspect relations and interactions of variables
6.2. Inspect collinearity
7. TRANSFORMATIONS
7.1. Routines for potential transformations and back transformation
7.2. Regression example: gross precipiation vs. throughfall

## Example

To understand the major principles as demonstrated in the book chapter (see reference below), the code can be run using an exemplary dataset on throughfall measurements ("throughfall_dataset.txt").

## Reference
This R code is provided to guide researchers and undergraduates in their primary steps in analyzing data. A full description of tasks and tools for a systematic data exploration are given in the following book chapter, which provides the same R code and exemplary data:

Zwanzig, M., Schlicht, R., Frischbier, N., Berger, U. (2020). Primary Steps in Analyzing Data: Tasks and Tools for a Systematic Data Exploration. In: Levia, D.F., Carlyle-Moses, D.E., Iida, S., Michalzik, B., Nanko, K., Tischer, A. (eds) Forest-Water Interactions. Ecological Studies, vol 240. Springer, Cham. https://doi.org/10.1007/978-3-030-26086-6_7

Please indicate any use of this code that contributes to a publication with a reference to this article.
