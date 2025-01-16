# tornado-severity

This analysis contained in this repository explores data compiled from the 
[Severe Weather Maps, Graphics, and Data](https://www.spc.noaa.gov/wcm/#data) page 
of the National Oceanic and Atmospheric Administration's National Weather Service
Storm Prediction Center. 

The data document measurements associated with 68,693 tornadoes from 1950 to 2022.
The analysis includes some exploratory data analysis, feature engineering,
model specification, hyperparameter tuning, and model evaluation.

The model predicts a number of predictors, including effect encodings for state-level data to predict the 
magnitude of tornadoes via:

* XGBoost  (with a number of tuned hyperparameters)
* Standard Poisson Regression
* Zero-Inflated Poisson Regression

## Project Structure
- `analysis.Rmd`: Analysis and modeling 
- `data/`: Source data
- `docs/`: Rendered analysis

## Getting Started
1. Clone this repository
2. Install required packages:
   
   ```r
   install.packages(c(
     "embed",
     "finetune",
     "ggrepel",
     "knitr",
     "kableExtra",
     "poissonreg",
     "pscl",
     "RColorBrewer",
     "rmarkdown",
     "tidymodels",
     "tidyverse",
     "vip",
     "xgboost"))
   ```
   
3. Open `analysis.Rmd` and run the analysis

## Viewing Results
- View the rendered analysis [here](https://vbashyakarla.github.io/tornado-mag/analysis.html)