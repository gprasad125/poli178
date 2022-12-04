R files for POLI178 @ UCSD, Fall 2022.

Authors:

Gokul Prasad, Jack Ryan, Chong Zhang, Anya Van Noord, Amanda Quinonez, Nicolette Satrya, and Shiv Sakthivel

Description:

## Data:
- contains the data used in the R files. 

## Visualization

`variable_visualization.r`: Generates exploratory visualizations (grouped barchart, boxplot, 2 geospatial heatmaps)
- authored by Gokul Prasad

`text_visualization.r`: Generates text-based visuals from data (Word cloud, Latent Dirichlet model)
- authored by Chong Zhang

`ml_results_visual.r`: Generates two heatmaps based on results generated by both ML models
- authored by Gokul Prasad

## Machine Learning

`model_with_nulls_dropped.r`: Trains KNN and Random Forest on dataset of 41 countries, after dropping NaNs 
- authored by Jack Ryan

`model_with_nulls_handled.r`: Trains KNN and Random Forest on dataset of 117 countries, after imputing NaNs with mean 
- authored by Jack Ryan 
