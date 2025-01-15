analysis
================
Varoon Bashyakarla
2025-01-14

## Setup

``` r
knitr::opts_chunk$set(echo = TRUE, fig.path = "docs/images/")

library(embed)
```

    ## Loading required package: recipes

    ## Loading required package: dplyr

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    ## 
    ## Attaching package: 'recipes'

    ## The following object is masked from 'package:stats':
    ## 
    ##     step

``` r
library(finetune)
```

    ## Loading required package: tune

``` r
library(ggrepel)
```

    ## Loading required package: ggplot2

``` r
library(knitr)
library(RColorBrewer)
library(tidymodels)
```

    ## ── Attaching packages ──────────────────────────────────────── tidymodels 1.2.0 ──

    ## ✔ broom        1.0.7     ✔ rsample      1.2.1
    ## ✔ dials        1.3.0     ✔ tibble       3.2.1
    ## ✔ infer        1.0.7     ✔ tidyr        1.3.1
    ## ✔ modeldata    1.4.0     ✔ workflows    1.1.4
    ## ✔ parsnip      1.2.1     ✔ workflowsets 1.1.0
    ## ✔ purrr        1.0.2     ✔ yardstick    1.3.1

    ## ── Conflicts ─────────────────────────────────────────── tidymodels_conflicts() ──
    ## ✖ purrr::discard() masks scales::discard()
    ## ✖ dplyr::filter()  masks stats::filter()
    ## ✖ dplyr::lag()     masks stats::lag()
    ## ✖ recipes::step()  masks stats::step()
    ## • Use suppressPackageStartupMessages() to eliminate package startup messages

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ────────────────────────── tidyverse 2.0.0 ──
    ## ✔ forcats   1.0.0     ✔ readr     2.1.5
    ## ✔ lubridate 1.9.4     ✔ stringr   1.5.1

    ## ── Conflicts ──────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ readr::col_factor() masks scales::col_factor()
    ## ✖ purrr::discard()    masks scales::discard()
    ## ✖ dplyr::filter()     masks stats::filter()
    ## ✖ stringr::fixed()    masks recipes::fixed()
    ## ✖ dplyr::lag()        masks stats::lag()
    ## ✖ readr::spec()       masks yardstick::spec()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(vip)
```

    ## 
    ## Attaching package: 'vip'
    ## 
    ## The following object is masked from 'package:utils':
    ## 
    ##     vi

``` r
library(xgboost)
```

    ## 
    ## Attaching package: 'xgboost'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     slice

``` r
theme_set(theme_linedraw())
```

## Import Data

``` r
tornado_df <- read_csv("data/tornado.csv")
```

    ## Rows: 68693 Columns: 27
    ## ── Column specification ──────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (2): tz, st
    ## dbl  (21): om, yr, mo, dy, stf, mag, inj, fat, loss, slat, slon, elat, elon, l...
    ## lgl   (1): fc
    ## dttm  (1): datetime_utc
    ## date  (1): date
    ## time  (1): time
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

## Exploratory Data Analysis

The outcome of interest is the tornado’s severity (i.e., its magnitude).

``` r
tornado_df |> 
  ggplot(aes(x = mag)) +
  geom_bar(fill = "darkorange") +
  xlab("Magnitude") +
  ylab("Count") +
  ggtitle("Distribution of Tornado Severity") +
  scale_y_continuous(labels = scales::comma_format())
```

    ## Warning: Removed 756 rows containing non-finite outside the scale range
    ## (`stat_count()`).

![](docs/images/severity-dist-1.png)<!-- -->

Most tornadoes are mild, but some are devastating and occur rarely.
Additionally, as shown below, none of the magnitude 5 earthquakes had
been predicted as such.

``` r
tornado_df |> 
  ggplot(aes(x = mag, fill = fc)) +
  geom_bar(position = position_dodge(preserve = "single")) +
  xlab("Magnitude") +
  ylab("Count") +
  ggtitle("Distribution of Tornado Severity by \nWhether or Not the Magnitude was Esimated") +
  scale_y_log10(labels = scales::comma_format()) +
  labs(fill = "Magnitude Estimated")
```

    ## Warning: Removed 756 rows containing non-finite outside the scale range
    ## (`stat_count()`).

![](docs/images/severity-by-est-dist-1.png)<!-- -->

The magnitude is a numeric value but not continuous.

- To predict it, we could treat this exercise as a classifiation
  problem, but doing so would lose information regarding the ordinality
  of the magnitude.
- An ordered logistic / probit regression is an option (using
  `MASS::polr()`).
- Treating the data-generating process as one that produces counts would
  lend itself to Poisson regression (using `poissonreg::poissonreg()`).
  However, both approaches risk failing to capture non-linearity in the
  data.
- XGBoost seems like a suitable option, but doing so requires us to
  treat the magnitude (the outcome of interest) as a continuous outcome.
  This is the model I have chosen to use.

``` r
tornado_df |> 
  group_by(st) |> 
  summarise(mean_mag = mean(mag, na.rm = TRUE),
            n = n()) |> 
  ggplot(aes(x = n, y = mean_mag, label = st)) +
  geom_point(color = "darkred") +
  geom_text_repel(color = "darkred") +
  xlab("Total Number of Tornadoes") +
  ylab("Mean Magnitude") +
  ggtitle("Mean Magnitude vs. Total Number of Tornadoes by State") +
  scale_x_continuous(labels = scales::comma_format())
```

![](docs/images/mag-num-by-state-1.png)<!-- -->

State information is clearly associated with magnitude. We can capture
this signal using effect encodings.

``` r
tornado_df |> 
  filter(!is.na(mag)) |>
  mutate(mag = factor(mag)) |> 
  ggplot(aes(x = mag, y = inj, fill = mag)) +
  geom_boxplot(alpha = 0.5, show.legend = FALSE) +
  scale_y_log10(trans = scales::pseudo_log_trans(base = 10),
                labels = scales::comma_format()) +
  scale_fill_brewer(palette = "Set3") +
  xlab("Magnitude") + 
  ylab("Number of Injuries") +
  ggtitle("Injuries vs. Tornado Magnitude")
```

![](docs/images/inj-mag-dist-1.png)<!-- -->

As expected, higher magnitude earthquakes are associated with more
injuries, though plenty of outliers exist. We see similar trends for
fatalities, as shown below, but the effect is dampened.

``` r
tornado_df |> 
  filter(!is.na(mag)) |>
  mutate(mag = factor(mag)) |> 
  ggplot(aes(x = mag, y = fat, fill = mag)) +
  geom_boxplot(alpha = 0.5, show.legend = FALSE) +
  scale_y_log10(trans = scales::pseudo_log_trans(base = 10),
                labels = scales::comma_format()) +
  scale_fill_brewer(palette = "Set2") +
  xlab("Magnitude") + 
  ylab("Number of Fatalities") +
  ggtitle("Fatalities vs. Tornado Magnitude")
```

![](docs/images/fat-mag-dist-1.png)<!-- -->

## Set Up Framework

``` r
# Defining Training (Analysis and Assessment) Datasets and Test Sets
set.seed(1234)

tornado_split_df <- 
 tornado_df |> 
  filter(!is.na(mag),
         !is.na(loss)) |>
  initial_split(strata = mag) # default 75% train, 25% test

tornado_train <- training(tornado_split_df)
tornado_test <- testing(tornado_split_df)

# xval resamples
set.seed(123)
tornado_folds <- vfold_cv(tornado_train, strata = mag)
tornado_folds # 10 folds by default
```

    ## #  10-fold cross-validation using stratification 
    ## # A tibble: 10 × 2
    ##    splits               id    
    ##    <list>               <chr> 
    ##  1 <split [28021/3115]> Fold01
    ##  2 <split [28021/3115]> Fold02
    ##  3 <split [28021/3115]> Fold03
    ##  4 <split [28022/3114]> Fold04
    ##  5 <split [28023/3113]> Fold05
    ##  6 <split [28023/3113]> Fold06
    ##  7 <split [28023/3113]> Fold07
    ##  8 <split [28023/3113]> Fold08
    ##  9 <split [28023/3113]> Fold09
    ## 10 <split [28024/3112]> Fold10

## Pre-Processing and Feature Engineering

``` r
tornado_recipe <- 
  recipe(mag ~ date + st + inj + fat + len + wid + ns,
       data = tornado_train) |> 
  # maps states to effect on the outcome
  step_lencode_glm(st, outcome = vars(mag)) |> 
  step_date(date, features = c("month", "year"), keep_original_cols = FALSE) |> 
  # change existing nominal variables to dummy / indicators
  step_dummy(all_nominal_predictors())
  
tornado_recipe 
```

    ## 

    ## ── Recipe ────────────────────────────────────────────────────────────────────────

    ## 

    ## ── Inputs

    ## Number of variables by role

    ## outcome:   1
    ## predictor: 7

    ## 

    ## ── Operations

    ## • Linear embedding for factors via GLM for: st

    ## • Date features from: date

    ## • Dummy variables from: all_nominal_predictors()

Here, we are modeling the tornado’s magnitude as a function of:

- `date` - When the tornado occurred
- `st` - The state in which the tornado started
- `inj` - The number of injuries caused by the tornado
- `fat` - The number of fatalities
- `len` - The length of the tornado (measured in miles)
- `wid` - The width of the tornado (measured in yards)
- `ns` - The number of states affected by the tornado

``` r
# For debugging
prep(tornado_recipe) |> bake(new_data = NULL) |> glimpse()
```

    ## Rows: 31,136
    ## Columns: 19
    ## $ st             <dbl> 1.2663014, 1.1072304, 1.0354342, 1.0354342, 1.0864012, 1.…
    ## $ inj            <dbl> 0, 3, 0, 32, 2, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,…
    ## $ fat            <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ len            <dbl> 15.8, 2.0, 2.3, 7.7, 2.0, 0.5, 0.1, 0.2, 2.0, 19.8, 0.5, …
    ## $ wid            <dbl> 10, 37, 233, 100, 33, 27, 10, 10, 100, 10, 77, 10, 27, 10…
    ## $ ns             <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
    ## $ mag            <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, …
    ## $ date_year      <int> 1950, 1950, 1950, 1950, 1950, 1950, 1950, 1950, 1950, 195…
    ## $ date_month_Feb <dbl> 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ date_month_Mar <dbl> 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ date_month_Apr <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ date_month_May <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
    ## $ date_month_Jun <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ date_month_Jul <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ date_month_Aug <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ date_month_Sep <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ date_month_Oct <dbl> 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ date_month_Nov <dbl> 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ date_month_Dec <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …

## Model Specification and Hyperparameter Tuning via Racing

``` r
xgb_details <- 
  boost_tree(
    trees = tune(),
    min_n = tune(), 
    mtry = tune(),
    learn_rate = 0.005, # shrinkage
    engine = "xgboost"
  ) |> 
  set_mode("regression")

xgb_workflow <- workflow(preprocessor = tornado_recipe, spec = xgb_details)
xgb_workflow
```

    ## ══ Workflow ══════════════════════════════════════════════════════════════════════
    ## Preprocessor: Recipe
    ## Model: boost_tree()
    ## 
    ## ── Preprocessor ──────────────────────────────────────────────────────────────────
    ## 3 Recipe Steps
    ## 
    ## • step_lencode_glm()
    ## • step_date()
    ## • step_dummy()
    ## 
    ## ── Model ─────────────────────────────────────────────────────────────────────────
    ## Boosted Tree Model Specification (regression)
    ## 
    ## Main Arguments:
    ##   mtry = tune()
    ##   trees = tune()
    ##   min_n = tune()
    ##   learn_rate = 0.005
    ## 
    ## Computational engine: xgboost

- The hyperparameters being tuned via grid search are:
  - `trees` - The number of trees used in the ensemble.
  - `min_n` - The minimum number of observations required at a node
    before the node is split further.
  - `mtry` - The number of predictors randomly sampled at each split
    during tree model creation.

The `learn_rate` was set to `0.005` to keep local computations
tractable.

The hyperparameters are selected and tuned via racing in which
unpromising parameters are eliminated (based on the outcomes of ANOVA)
to save processing time.

The model runs one set of hyperparameters on each of the 10
cross-validation folds through racing. Poor-performing hyperparameters
(based on the outcomes of ANOVA) are eliminated to save processing time.
The process continues until the best hyperparameters have been
identified.

``` r
set.seed(456)

xgb_tuning_rs <- tune_race_anova(
  xgb_workflow, 
  resamples = tornado_folds, # used for tuning
  grid = 20,
  # metrics = "rmse",
  control = control_race(verbose_elim = TRUE)
)
```

    ## ℹ Evaluating against the initial 3 burn-in resamples.
    ## ℹ Racing will minimize the rmse metric.
    ## ℹ Resamples are analyzed in a random order.
    ## ℹ Fold07: 13 eliminated; 7 candidates remain.
    ## 
    ## ℹ Fold09: 5 eliminated; 2 candidates remain.
    ## 
    ## ℹ Fold08: 0 eliminated; 2 candidates remain.
    ## 
    ## ℹ Fold06: 0 eliminated; 2 candidates remain.
    ## 
    ## ℹ Fold10: 0 eliminated; 2 candidates remain.
    ## 
    ## ℹ Fold03: 0 eliminated; 2 candidates remain.
    ## 
    ## ℹ Fold05: 0 eliminated; 2 candidates remain.

## Race Results, Hyperparameter Configuration, and Model Finalization

``` r
collect_metrics(xgb_tuning_rs)
```

    ## # A tibble: 4 × 9
    ##    mtry trees min_n .metric .estimator  mean     n std_err .config              
    ##   <int> <int> <int> <chr>   <chr>      <dbl> <int>   <dbl> <chr>                
    ## 1    10  1769    18 rmse    standard   0.629    10 0.00341 Preprocessor1_Model12
    ## 2    10  1769    18 rsq     standard   0.528    10 0.00566 Preprocessor1_Model12
    ## 3    12  1912    31 rmse    standard   0.629    10 0.00335 Preprocessor1_Model13
    ## 4    12  1912    31 rsq     standard   0.528    10 0.00553 Preprocessor1_Model13

``` r
plot_race(xgb_tuning_rs) +
  ggtitle("XGBoost HyperParameter Tuning Race Results")
```

![](docs/images/race-results-1.png)<!-- -->

``` r
select_best(xgb_tuning_rs, metric = "rmse")
```

    ## # A tibble: 1 × 4
    ##    mtry trees min_n .config              
    ##   <int> <int> <int> <chr>                
    ## 1    12  1912    31 Preprocessor1_Model13

Using rmse as our evaluation metric, the hyperparameters for the best
model were found as shown above.

## Model Fit and Performance

With the hyperparameters tuned, we can now fit the model once on the
full set of training data and evaluate its performance on the test data.

``` r
tornado_fit <- 
  xgb_workflow |> 
  finalize_workflow(select_best(xgb_tuning_rs, metric = "rmse")) |> 
  last_fit(tornado_split_df)
```

``` r
collect_metrics(tornado_fit) # results on testing data
```

    ## # A tibble: 2 × 4
    ##   .metric .estimator .estimate .config             
    ##   <chr>   <chr>          <dbl> <chr>               
    ## 1 rmse    standard       0.627 Preprocessor1_Model1
    ## 2 rsq     standard       0.533 Preprocessor1_Model1

The outcome metrics do not suggest that we have overfit the model in
training.

``` r
collect_predictions(tornado_fit) |> 
  ggplot(aes(x = .pred)) +
  geom_histogram(fill = "darkgreen", col = "white") +
  xlab("Magnitude Prediction") +
  ylab("Count") +
  ggtitle("Distribution of Predicted Magnitudes")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](docs/images/pred-mag-dist-1.png)<!-- -->

Though we treated the magnitude as a continouous variable, we have not
predicted many magnitudes less than 0. The majority of the mass of the
distribution of predicted magnitudes is concentrated further to the
right than the true values are. In other words, we are predicting
low-magnitude tornadoes to be higher in magnitude and higher-magnitude
tornadoes as lower in magnitude. The plot below demonstrates this aspect
of the model’s performance more clearly.

``` r
collect_predictions(tornado_fit) |> 
  mutate(mag = factor(mag)) |> 
  ggplot(aes(x = mag, y = .pred, fill = mag)) +
  geom_boxplot(alpha = 0.6, show.legend = FALSE) +
  scale_fill_brewer(palette = "Set2") +
  xlab("Actual Magnitude") +
  ylab("Predicted Magnitude") +
  ggtitle("Predicted vs. Actual Tornado Magnitudes")
```

![](docs/images/pred-vs-actual-mag-1.png)<!-- -->

``` r
extract_workflow(tornado_fit) |> 
  extract_fit_parsnip() |> 
  vip(num_features = 10) +
  geom_bar(stat = "identity", fill = "goldenrod") +
  ylab("Feature") +
  ggtitle("XGBoost Feature Importance")
```

![](docs/images/feat-imp-plot-1.png)<!-- -->

The features deemed important by the model are the following:

- The number of injuries
- The length of the tornado
- The year in which the tornado occurred
- The width of the tornado
- The number of fatalities resulting from the tornado
- The state in which the tornado started
- Month-level data

These predictors behave as expected. The importance of the state
variable suggests the the effect encoding was sensible.
