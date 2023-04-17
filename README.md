# Reservoir Water Level Data Analysis
This project aims to analyze the impact of economic output and income inequality on the Reservoir Water Level. The steps taken to complete this project are listed below:
## Data Selection
Reservoir Water Level was chosen as the environmental quality measure for analysis. The data was obtained from the National Data Analytics Platform (NDAP).

## Data Transformation
The original dataset obtained from NDAP was transformed into a district-year level dataset. This included creating a unique district-year ID for each row in the sample.

## Data Merging
The district-year level environmental quality data was merged with the corresponding state-year wise economic output data, i.e., the net state domestic product (SDP) at constant prices. This data was obtained from the Reserve Bank of India accessed on the Database for the Indian Economy (DBIE) portal. Finally, the dataset was merged with the district-level Gini index from the paper by Mohanty et al. (2016).

## Summary Statistics
Detailed summary statistics for all variables were prepared including tables, histograms, box-plots, shape of the distribution, and skew. Outliers were also identified.

## Regression Analysis
Using the data, a regression model was estimated for Reservoir Water Level. The model equation was as follows:
```python
Reservoir Water Level (i,t) = β0 + β1SDP(i,t) + ui,t
```
The results of the regression analysis were summarized in a table and interpreted

## Visualizing Residuals
The model residuals (i.e., ûi,t) were visualized on a plot having Reservoir Water Level on the Y-axis and SDP on the X-axis. Three plots were constructed, including one having ûi,t on the Y-axis and SDP on the X-axis, and another having predicted values of the Reservoir Water Level on the Y-axis and true values of the Reservoir Water Level on the X-axis. The relationship between these three plots was explained.<br>
Histogram of Residuals: A histogram of ûi,t was plotted to verify that ∑i,t ûi,t = 0.

## Multiple Regression Analysis
Finally, a multiple regression model was estimated to analyze the impact of economic output, income inequality, and their interaction on Reservoir Water Level. The model equation was as follows:
```python
Reservoir Water Level (i,t) = α0 + α1SDP(i,t) + α2SDP(i,t)^2 + α3SDP(i,t)^3 + α4GINI(i) + γi,t
```
