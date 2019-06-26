  - [Overview](#overview)
      - [Logistic Regression](#logistic-regression)

# Overview

I want to create a decision tree to guide the user to determine who is
likely to win the nomination. Fortunately, I have created a tidy dataset
for the Democratic primaries, so lets load it up.

``` r
primary_data <- rbind(read_csv("../data/primary/democrat_1984.csv"),
                      read_csv("../data/primary/democrat_1988.csv"),
                      read_csv("../data/primary/democrat_1992.csv"),
                      read_csv("../data/primary/democrat_2004.csv"),
                      read_csv("../data/primary/democrat_2008.csv")) %>%
  group_by(state,event_type) %>%
  filter(any(won_state),!is.na(date)) %>%
  ungroup() %>%
  filter(!is.na(date)) %>%
  mutate(year = format(date,"%Y"),
         won_nomination = ifelse(is_nominee, 1, 0),
         event_type = factor(event_type, labels = c("primary","caucus"))) %>%
  group_by(year) %>%
  mutate(days_since_start = as.numeric(date - min(date))) %>%
  ungroup() %>%
  select(-year)
```

    ## Parsed with column specification:
    ## cols(
    ##   date = col_date(format = ""),
    ##   state = col_character(),
    ##   candidate = col_character(),
    ##   vote_perc = col_double(),
    ##   won_state = col_logical(),
    ##   is_nominee = col_logical(),
    ##   event_type = col_character()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   date = col_date(format = ""),
    ##   state = col_character(),
    ##   candidate = col_character(),
    ##   vote_perc = col_double(),
    ##   won_state = col_logical(),
    ##   is_nominee = col_logical(),
    ##   event_type = col_character()
    ## )

    ## Parsed with column specification:
    ## cols(
    ##   state = col_character(),
    ##   candidate = col_character(),
    ##   date = col_date(format = ""),
    ##   vote_perc = col_double(),
    ##   won_state = col_logical(),
    ##   is_nominee = col_logical(),
    ##   event_type = col_character()
    ## )

    ## Parsed with column specification:
    ## cols(
    ##   date = col_date(format = ""),
    ##   state = col_character(),
    ##   candidate = col_character(),
    ##   vote_perc = col_double(),
    ##   is_nominee = col_logical(),
    ##   won_state = col_logical(),
    ##   event_type = col_character()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   date = col_date(format = ""),
    ##   state = col_character(),
    ##   candidate = col_character(),
    ##   vote_perc = col_double(),
    ##   is_nominee = col_logical(),
    ##   won_state = col_logical(),
    ##   event_type = col_character()
    ## )

``` r
dt1 <- primary_data
dt1$state <- tolower(str_replace_all(dt1$state, "\\s+", "_"))
dt1$state <- str_replace(dt1$state, "\\[\\d+\\]", "")
dt1$year <- format(dt1$date, "%Y")


dt2 <- dt1 %>% 
  mutate(won_state = ifelse(won_state, 1, 0)) %>%
  filter(days_since_start < 20) %>%
  select(candidate,year,state,won_nomination,won_state) %>%
  rowid_to_column() %>%
  group_by(candidate,year) %>%
  nest() %>%
  mutate(data = map(data, spread, state, won_state, fill = 0)) %>%
  unnest() %>% select(-rowid) %>% group_by(candidate,year,won_nomination) %>% summarise_each(funs(max))
```

We can now construct the decision tree. The result is rather
interesting. It is folklore that New Hampshire is only relevant when
trying to weed out a sitting President in a primary challenge (like
Teddy Kennedy challenging Carter in 1980), and all other circumstances
is irrelevant. We find this reflected in the decision
tree:

``` r
rtree_fit <- rpart(won_nomination ~ iowa + minnesota + new_hampshire + south_dakota + maine + district_of_columbia, 
                   dt2, method = "class", model = T, 
                   parms = list(prior = c(.65,.35), split = "information"),
                   control = rpart.control(minsplit = 2)
                   )
rpart.plot(rtree_fit)
```

![](2019-06-19-decision-tree-democratic-primary_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

We should read this tree as stating three rows in each outcome bubble:

1.  Did the candidate become the nominee? `0` for “No”, `1` for “Yes”
2.  The probability of this outcome (0.00 for “impossible to become
    nominee” to 1.00 for “almost certainly the nominee”)
3.  The percentage of data points which fall in the outcome.

In other words, the unsurprising outcome discerned: to become the
nominee, the candidate must win *either* Iowa *or* New Hampshire.

## Logistic Regression

We filter data, in case we accidentally allowed bad data through.

``` r
dt0 <- filter(primary_data, !is.na(vote_perc))
```

Now we construct a few logistic regressions.

``` r
logitMod <- glm(won_nomination ~ won_state*days_since_start*vote_perc, 
                data = dt0, family = binomial(link = "logit"))
summary(logitMod)
```

    ## 
    ## Call:
    ## glm(formula = won_nomination ~ won_state * days_since_start * 
    ##     vote_perc, family = binomial(link = "logit"), data = dt0)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.0147  -0.2842  -0.2495  -0.2271   2.7578  
    ## 
    ## Coefficients:
    ##                                            Estimate Std. Error z value
    ## (Intercept)                              -3.7802699  0.2682919 -14.090
    ## won_stateTRUE                             1.8893499  0.8087677   2.336
    ## days_since_start                          0.0038367  0.0033666   1.140
    ## vote_perc                                 0.0560683  0.0111948   5.008
    ## won_stateTRUE:days_since_start           -0.0056222  0.0077331  -0.727
    ## won_stateTRUE:vote_perc                  -0.0066955  0.0191811  -0.349
    ## days_since_start:vote_perc                0.0001617  0.0001765   0.916
    ## won_stateTRUE:days_since_start:vote_perc -0.0002412  0.0002270  -1.062
    ##                                          Pr(>|z|)    
    ## (Intercept)                               < 2e-16 ***
    ## won_stateTRUE                              0.0195 *  
    ## days_since_start                           0.2544    
    ## vote_perc                                5.49e-07 ***
    ## won_stateTRUE:days_since_start             0.4672    
    ## won_stateTRUE:vote_perc                    0.7270    
    ## days_since_start:vote_perc                 0.3595    
    ## won_stateTRUE:days_since_start:vote_perc   0.2881    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1646.4  on 2311  degrees of freedom
    ## Residual deviance: 1104.1  on 2304  degrees of freedom
    ## AIC: 1120.1
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
summary(glm(won_nomination ~ won_state:vote_perc:days_since_start,
            data = dt0, family = binomial(link = "logit")))
```

    ## 
    ## Call:
    ## glm(formula = won_nomination ~ won_state:vote_perc:days_since_start, 
    ##     family = binomial(link = "logit"), data = dt0)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -3.2195  -0.3853  -0.3653  -0.3653   2.3410  
    ## 
    ## Coefficients:
    ##                                             Estimate Std. Error z value
    ## (Intercept)                               -2.673e+00  9.049e-02 -29.543
    ## won_stateFALSE:vote_perc:days_since_start  5.646e-04  8.535e-05   6.615
    ## won_stateTRUE:vote_perc:days_since_start   6.456e-04  4.690e-05  13.766
    ##                                           Pr(>|z|)    
    ## (Intercept)                                < 2e-16 ***
    ## won_stateFALSE:vote_perc:days_since_start 3.71e-11 ***
    ## won_stateTRUE:vote_perc:days_since_start   < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1646.4  on 2311  degrees of freedom
    ## Residual deviance: 1353.1  on 2309  degrees of freedom
    ## AIC: 1359.1
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
predicted <- predict(logitMod, dt0, type = "response")  # predicted scores
dt0$prediction <- predicted

precision <- (dt0 %>% filter(is_nominee) %>% select(prediction) %>% sum)/sum(predicted)
recall <- (dt0 %>% filter(is_nominee) %>% select(prediction) %>% sum)/(dt0 %>% filter(is_nominee) %>% nrow)
Fmeasure <- 2 * precision * recall / (precision + recall)
print(Fmeasure)
```

    ## [1] 0.4071637
