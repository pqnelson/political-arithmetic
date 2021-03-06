  - [Estimating Turnout](#estimating-turnout)
      - [Exit Poll Data](#exit-poll-data)
      - [Loading Census Data](#loading-census-data)
  - [Walking to the polls](#walking-to-the-polls)
  - [Model for Voter Turnout (Test Case:
    Arizona)](#model-for-voter-turnout-test-case-arizona)
  - [Turnout Estimates Using Clinton
    Votes](#turnout-estimates-using-clinton-votes)
  - [Post-Stratified Model](#post-stratified-model)

# Estimating Turnout

## Exit Poll Data

We try to fit exit poll data, which is extremely
noisy.

``` r
exit_poll_path <- "../data/elections/presidential/exit_polls/cnn_04022017.csv"
exit_poll_df <- read.csv(file=exit_poll_path, header=TRUE, sep=",")
national_exit_polls <- exit_poll_df[which(exit_poll_df$state == 'nation'),]
```

The logistic coefficients we compute:

``` r
wilson_center <- function(p, n, z) {
  (p + 0.5*z*z/n)/(1.0 + z*z*1.0/n);
}
wilson_width <- function(p, n, z) {
  (z/(1.0 + z*z*1.0/n))*sqrt(p*(1.0-p)/n  + (z*0.5/n)**2);
}
log_coefs <- function(row) {
  # adjust the estimated proportions using center of Wilson interval
  c <- wilson_center(row$Clinton_perc*0.01, row$options_perc*0.01*row$num_respondents,1)
  t <- wilson_center(row$Trump_perc*0.01, row$options_perc*0.01*row$num_respondents,1)
  # then use them for odds-ratios
  row$clinton_numerator <- c/(1.0-c)
  row$trump_numerator <- t/(1.0-t)
  
  row$clinton_coef <- log(row$clinton_numerator)
  row$trump_coef <- log(row$trump_numerator)
  row$log_var <- (2*wilson_width(row$Clinton_perc*0.01, row$options_perc*0.01*row$num_respondents, 2)/c)**2 + (2*wilson_width(row$Trump_perc*0.01, row$options_perc*0.01*row$num_respondents, 2)/t)**2;
  row$log_sd <- sqrt(row$log_var);
  return(row);
}
```

## Loading Census Data

We load the census data with some helper functions. First, we simply
load the data:

``` r
state_census_data <- function() {
  df <- data.frame()
  for (state in levels(unique(exit_poll_df$state))) {
    if (state != "nation") {
      state_data <- read.csv(file=paste0('D:/src/pa/data/census/acs5/2016/',state,'.csv'),header=TRUE,sep=',');
      state_data$state <- state;
      df <- rbind(df,state_data)
    }
  }
  df[is.na(df)] <- 0
  df
}
```

Next, we have some helper functions to clean up the data.

``` r
voting_age_pop <- function(row) {
  sum(row[,c("female.18_19E","female.20E","female.21E","female.22_24E","female.25_29E","female.30_34E","female.35_39E","female.40_44E","female.45_49E","female.50_54E","female.55_59E","female.60_61E","female.62_64E","female.65_66E","female.67_69E","female.70_74E","female.75_79E","female.80_84E","female.85_plusE","male.18_19E","male.20E","male.21E","male.22_24E","male.25_29E","male.30_34E","male.35_39E","male.40_44E","male.45_49E","male.50_54E","male.55_59E","male.60_61E","male.62_64E","male.65_66E","male.67_69E","male.70_74E","male.75_79E","male.80_84E","male.85_plusE")])
}

categorize <- function(census_df) {
  return(census_df %>% transmute(
    name = NAME,
    FIPS = GEOID,
    state = state,
    # age
    age.18_19 = male.18_19E+female.18_19E,
    age.20 = male.20E+female.20E,
    age.21 = male.21E+female.21E,
    age.22_24 = male.22_24E+female.22_24E,
    age.25_29 = male.25_29E+female.25_29E,
    age.30_34 = male.30_34E+female.30_34E,
    age.35_39 = male.35_39E+female.35_39E,
    age.40_44 = male.40_44E+female.40_44E,
    age.45_49 = male.45_49E+female.45_49E,
    age.50_54 = male.50_54E+female.50_54E,
    age.55_59 = male.55_59E+female.55_59E,
    age.60_61 = male.60_61E+female.60_61E,
    age.62_64 = male.62_64E+female.62_64E,
    age.retirees = male.65_66E+male.67_69E+male.70_74E+male.75_79E+male.80_84E+male.85_plusE+female.65_66E+female.67_69E+female.70_74E+female.75_79E+female.80_84E+female.85_plusE,
    # ethnicity
    # male.white = male.white.18_19E+male.white.20_24E+male.white.25_29E+male.white.30_34E+male.white.35_44E+male.white.45_54E+male.white.55_64E+male.white.65_74E+male.white.75_84E + male.white.85_plusE + male.white_only.18_19E+male.white_only.20_24E+male.white_only.25_29E+male.white_only.30_34E+male.white_only.35_44E+male.white_only.45_54E+male.white_only.55_64E+male.white_only.65_74E+male.white_only.75_84E + male.white_only.85_plusE + 0*abs(round(rnorm(1,0,max(1,sqrt((male.white.18_19M**2) + (male.white.20_24M**2) + (male.white.25_29M**2) + (male.white.30_34M**2) + (male.white.35_44M**2) + (male.white.45_54M**2) + (male.white.55_64M**2) + (male.white.65_74M**2) + (male.white.75_84M**2) + (male.white.85_plusM**2) + (male.white_only.18_19M**2) + (male.white_only.20_24M**2) + (male.white_only.25_29M**2) + (male.white_only.30_34M**2) + (male.white_only.35_44M**2) + (male.white_only.45_54M**2) + (male.white_only.55_64M**2) + (male.white_only.65_74M**2) + (male.white_only.75_84M**2) + (male.white_only.85_plusM**2)))))),
    # female.white = female.white.18_19E+female.white.20_24E+female.white.25_29E+female.white.30_34E+female.white.35_44E+female.white.45_54E+female.white.55_64E+female.white.65_74E+female.white.75_84E+female.white.85_plusE + female.white_only.18_19E+female.white_only.20_24E+female.white_only.25_29E+female.white_only.30_34E+female.white_only.35_44E+female.white_only.45_54E+female.white_only.55_64E+female.white_only.65_74E+female.white_only.75_84E + 0*abs(round(rnorm(1,0,max(1,sqrt((female.white.18_19M**2) + (female.white.20_24M**2) + (female.white.25_29M**2) + (female.white.30_34M**2) + (female.white.35_44M**2) + (female.white.45_54M**2) + (female.white.55_64M**2) + (female.white.65_74M**2) + (female.white.75_84M**2) + (female.white.85_plusM**2) + (female.white_only.18_19M**2) + (female.white_only.20_24M**2) + (female.white_only.25_29M**2) + (female.white_only.30_34M**2) + (female.white_only.35_44M**2) + (female.white_only.45_54M**2) + (female.white_only.55_64M**2) + (female.white_only.65_74M**2) + (female.white_only.75_84M**2) + (female.white_only.85_plusM**2)))))),
    male.white = male.white_only.18_19E+male.white_only.20_24E+male.white_only.25_29E+male.white_only.30_34E+male.white_only.35_44E+male.white_only.45_54E+male.white_only.55_64E+male.white_only.65_74E+male.white_only.75_84E + male.white_only.85_plusE + 0*abs(round(rnorm(1,0,max(1,sqrt((male.white_only.18_19M**2) + (male.white_only.20_24M**2) + (male.white_only.25_29M**2) + (male.white_only.30_34M**2) + (male.white_only.35_44M**2) + (male.white_only.45_54M**2) + (male.white_only.55_64M**2) + (male.white_only.65_74M**2) + (male.white_only.75_84M**2) + (male.white_only.85_plusM**2)))))),
    female.white = female.white_only.18_19E+female.white_only.20_24E+female.white_only.25_29E+female.white_only.30_34E+female.white_only.35_44E+female.white_only.45_54E+female.white_only.55_64E+female.white_only.65_74E+female.white_only.75_84E + 0*abs(round(rnorm(1,0,max(1,sqrt((female.white_only.18_19M**2) + (female.white_only.20_24M**2) + (female.white_only.25_29M**2) + (female.white_only.30_34M**2) + (female.white_only.35_44M**2) + (female.white_only.45_54M**2) + (female.white_only.55_64M**2) + (female.white_only.65_74M**2) + (female.white_only.75_84M**2) + (female.white_only.85_plusM**2)))))),
    # male.white = male.white.18_19E+male.white.20_24E+male.white.25_29E+male.white.30_34E+male.white.35_44E+male.white.45_54E+male.white.55_64E+male.white.65_74E+male.white.75_84E + male.white.85_plusE + 0*abs(round(rnorm(1,0,max(1,sqrt((male.white.18_19M**2) + (male.white.20_24M**2) + (male.white.25_29M**2) + (male.white.30_34M**2) + (male.white.35_44M**2) + (male.white.45_54M**2) + (male.white.55_64M**2) + (male.white.65_74M**2) + (male.white.75_84M**2) + (male.white.85_plusM**2)))))),
    # female.white = female.white.18_19E+female.white.20_24E+female.white.25_29E+female.white.30_34E+female.white.35_44E+female.white.45_54E+female.white.55_64E+female.white.65_74E+female.white.75_84E + 0*abs(round(rnorm(1,0,max(1,sqrt((female.white.18_19M**2) + (female.white.20_24M**2) + (female.white.25_29M**2) + (female.white.30_34M**2) + (female.white.35_44M**2) + (female.white.45_54M**2) + (female.white.55_64M**2) + (female.white.65_74M**2) + (female.white.75_84M**2) + (female.white.85_plusM**2)))))),
    male.black = male.black.18_19E+male.black.20_24E+male.black.25_29E+male.black.30_34E+male.black.35_44E+male.black.45_54E+male.black.55_64E+male.black.65_74E+male.black.75_84E+male.black.85_plusE + 0*abs(round(rnorm(1,0,max(1,sqrt((male.black.18_19M**2) + (male.black.20_24M**2) + (male.black.25_29M**2) + (male.black.30_34M**2) + (male.black.35_44M**2) + (male.black.45_54M**2) + (male.black.55_64M**2) + (male.black.65_74M**2) + (male.black.75_84M**2) + (male.black.85_plusM**2)))))),
    female.black = female.black.18_19E+female.black.20_24E+female.black.25_29E+female.black.30_34E+female.black.35_44E+female.black.45_54E+female.black.55_64E+female.black.65_74E+female.black.75_84E+female.black.85_plusE + 0*abs(round(rnorm(1,0,max(1,abs(round(sqrt((female.black.18_19M**2) + (female.black.20_24M**2) + (female.black.25_29M**2) + (female.black.30_34M**2) + (female.black.35_44M**2) + (female.black.45_54M**2) + (female.black.55_64M**2) + (female.black.65_74M**2) + (female.black.75_84M**2) + (female.black.85_plusM**2)))))))),
    male.hispanic = male.hispanic.18_19E+male.hispanic.20_24E+male.hispanic.25_29E+male.hispanic.30_34E+male.hispanic.35_44E+male.hispanic.45_54E+male.hispanic.55_64E+male.hispanic.65_74E+male.hispanic.75_84E+male.hispanic.85_plusE + 0*abs(round(rnorm(1,0,max(1,abs(round(sqrt((male.hispanic.18_19M**2) + (male.hispanic.20_24M**2) + (male.hispanic.25_29M**2) + (male.hispanic.30_34M**2) + (male.hispanic.35_44M**2) + (male.hispanic.45_54M**2) + (male.hispanic.55_64M**2) + (male.hispanic.65_74M**2) + (male.hispanic.75_84M**2) + (male.hispanic.85_plusM**2)))))))),
    female.hispanic = female.hispanic.18_19E+female.hispanic.20_24E+female.hispanic.25_29E+female.hispanic.30_34E+female.hispanic.35_44E+female.hispanic.45_54E+female.hispanic.55_64E+female.hispanic.65_74E+female.hispanic.75_84E+female.hispanic.85_plusE + 0*abs(round(rnorm(1,0,max(1,abs(round(sqrt((female.hispanic.18_19M**2) + (female.hispanic.20_24M**2) + (female.hispanic.25_29M**2) + (female.hispanic.30_34M**2) + (female.hispanic.35_44M**2) + (female.hispanic.45_54M**2) + (female.hispanic.55_64M**2) + (female.hispanic.65_74M**2) + (female.hispanic.75_84M**2) + (female.hispanic.85_plusM**2)))))))),
    others = male.american_indian.18_19E+male.american_indian.20_24E+male.american_indian.25_29E+male.american_indian.30_34E+male.american_indian.35_44E+male.american_indian.45_54E+male.american_indian.55_64E+male.american_indian.65_74E+male.american_indian.75_84E+male.american_indian.85_plusE+female.american_indian.18_19E+female.american_indian.20_24E+female.american_indian.25_29E+female.american_indian.30_34E+female.american_indian.35_44E+female.american_indian.45_54E+female.american_indian.55_64E+female.american_indian.65_74E+female.american_indian.75_84E+female.american_indian.85_plusE+male.asian.18_19E+male.asian.20_24E+male.asian.25_29E+male.asian.30_34E+male.asian.35_44E+male.asian.45_54E+male.asian.55_64E+male.asian.65_74E+male.asian.75_84E+male.asian.85_plusE+female.asian.18_19E+female.asian.20_24E+female.asian.25_29E+female.asian.30_34E+female.asian.35_44E+female.asian.45_54E+female.asian.55_64E+female.asian.65_74E+female.asian.75_84E+female.asian.85_plusE+male.pacific_islander.18_19E+male.pacific_islander.20_24E+male.pacific_islander.25_29E+male.pacific_islander.30_34E+male.pacific_islander.35_44E+male.pacific_islander.45_54E+male.pacific_islander.55_64E+male.pacific_islander.65_74E+male.pacific_islander.75_84E+male.pacific_islander.85_plusE+female.pacific_islander.18_19E+female.pacific_islander.20_24E+female.pacific_islander.25_29E+female.pacific_islander.30_34E+female.pacific_islander.35_44E+female.pacific_islander.45_54E+female.pacific_islander.55_64E+female.pacific_islander.65_74E+female.pacific_islander.75_84E+female.pacific_islander.85_plusE + 0*abs(round(rnorm(1,0,max(1,abs(round(sqrt((male.american_indian.18_19M**2) + (male.american_indian.20_24M**2) + (male.american_indian.25_29M**2) + (male.american_indian.30_34M**2) + (male.american_indian.35_44M**2) + (male.american_indian.45_54M**2) + (male.american_indian.55_64M**2) + (male.american_indian.65_74M**2) + (male.american_indian.75_84M**2) + (male.american_indian.85_plusM**2) + (female.american_indian.18_19M**2) + (female.american_indian.20_24M**2) + (female.american_indian.25_29M**2) + (female.american_indian.30_34M**2) + (female.american_indian.35_44M**2) + (female.american_indian.45_54M**2) + (female.american_indian.55_64M**2) + (female.american_indian.65_74M**2) + (female.american_indian.75_84M**2) + (female.american_indian.85_plusM**2) + (male.asian.18_19M**2) + (male.asian.20_24M**2) + (male.asian.25_29M**2) + (male.asian.30_34M**2) + (male.asian.35_44M**2) + (male.asian.45_54M**2) + (male.asian.55_64M**2) + (male.asian.65_74M**2) + (male.asian.75_84M**2) + (male.asian.85_plusM**2) + (female.asian.18_19M**2) + (female.asian.20_24M**2) + (female.asian.25_29M**2) + (female.asian.30_34M**2) + (female.asian.35_44M**2) + (female.asian.45_54M**2) + (female.asian.55_64M**2) + (female.asian.65_74M**2) + (female.asian.75_84M**2) + (female.asian.85_plusM**2) + (male.pacific_islander.18_19M**2) + (male.pacific_islander.20_24M**2) + (male.pacific_islander.25_29M**2) + (male.pacific_islander.30_34M**2) + (male.pacific_islander.35_44M**2) + (male.pacific_islander.45_54M**2) + (male.pacific_islander.55_64M**2) + (male.pacific_islander.65_74M**2) + (male.pacific_islander.75_84M**2) + (male.pacific_islander.85_plusM**2) + (female.pacific_islander.18_19M**2) + (female.pacific_islander.20_24M**2) + (female.pacific_islander.25_29M**2) + (female.pacific_islander.30_34M**2) + (female.pacific_islander.35_44M**2) + (female.pacific_islander.45_54M**2) + (female.pacific_islander.55_64M**2) + (female.pacific_islander.65_74M**2) + (female.pacific_islander.75_84M**2) + (female.pacific_islander.85_plusM**2)))))))),
    # education
    education.high_school = male.18_24.less_than_9th_gradeE +  male.18_24.less_than_high_schoolE +  male.18_24.high_school_gradE +  male.25_34.less_than_9th_gradeE +  male.25_34.less_than_high_schoolE +  male.25_34.high_school_gradE +  male.35_44.less_than_9th_gradeE +  male.35_44.less_than_high_schoolE +  male.35_44.high_school_gradE +  male.45_64.less_than_9th_gradeE +  male.45_64.less_than_high_schoolE +  male.45_64.high_school_gradE +  male.65_plus.less_than_9th_gradeE +  male.65_plus.less_than_high_schoolE +  male.65_plus.high_school_gradE +  female.18_24.less_than_9th_gradeE+female.18_24.less_than_high_schoolE+female.18_24.high_school_gradE +  female.25_34.less_than_9th_gradeE +  female.25_34.less_than_high_schoolE +  female.25_34.high_school_gradE +  female.35_44.less_than_9th_gradeE +  female.35_44.less_than_high_schoolE +  female.35_44.high_school_gradE +  female.45_64.less_than_9th_gradeE +  female.45_64.less_than_high_schoolE +  female.45_64.high_school_gradE +  female.65_plus.less_than_9th_gradeE +  female.65_plus.less_than_high_schoolE +  female.65_plus.high_school_gradE,
    education.some_college = male.18_24.some_collegeE+male.18_24.associate_degreeE+male.25_34.some_collegeE+male.25_34.associate_degreeE+male.45_64.some_collegeE+male.45_64.associate_degreeE+male.65_plus.some_collegeE+male.65_plus.associate_degreeE+female.18_24.some_collegeE+female.18_24.associate_degreeE+female.25_34.some_collegeE+female.25_34.associate_degreeE+female.45_64.some_collegeE+female.45_64.associate_degreeE+female.65_plus.some_collegeE+female.65_plus.associate_degreeE,
    education.college_grad = male.18_24.bachelor_degreeE + male.25_34.bachelor_degreeE + male.35_44.bachelor_degreeE + male.45_64.bachelor_degreeE + male.65_plus.bachelor_degreeE + female.18_24.bachelor_degreeE + female.25_34.bachelor_degreeE + female.35_44.bachelor_degreeE + female.45_64.bachelor_degreeE + female.65_plus.bachelor_degreeE,
    education.postgrad = male.18_24.graduate_degreeE + male.25_34.graduate_degreeE + male.35_44.graduate_degreeE + male.45_64.graduate_degreeE + male.65_plus.graduate_degreeE + female.18_24.graduate_degreeE + female.25_34.graduate_degreeE + female.35_44.graduate_degreeE + female.45_64.graduate_degreeE + female.65_plus.graduate_degreeE
  ))
}
```

We load and categorize the Census data from
2016:

``` r
census_data <- categorize(state_census_data())
```

``` r
census_data %>% transmute(delta_person = male.white + female.white + male.black + female.black + male.hispanic + female.hispanic + others - (age.18_19 + age.20 + age.21 + age.22_24 + age.25_29 + age.30_34 + age.35_39 + age.40_44 + age.45_49 + age.50_54 + age.55_59 + age.60_61 + age.62_64 + age.retirees))  %>% sum
```

    ## [1] -4934593

The error when using `ethnicity_only` figures, compared against the age,
is about -4.9m.

``` r
census_data %>% group_by(state) %>%
  summarize(vap = sum(age.18_19 + age.20 + age.21 + age.22_24 + age.25_29 + age.30_34 + age.35_39 + age.40_44 + age.45_49 + age.50_54 + age.55_59 + age.60_61 + age.62_64 + age.retirees))
```

    ## # A tibble: 28 x 2
    ##    state           vap
    ##    <chr>         <int>
    ##  1 Arizona     5108959
    ##  2 California 29513923
    ##  3 Colorado    4113114
    ##  4 Florida    15868175
    ##  5 Georgia     7604145
    ##  6 Illinois    9861055
    ##  7 Indiana     5007586
    ##  8 Iowa        2379075
    ##  9 Kentucky    3397799
    ## 10 Maine       1070422
    ## # ... with 18 more rows

``` r
census_data %>% group_by(state) %>%
  summarize(vap = sum(male.white + female.white + male.black + female.black + male.hispanic + female.hispanic + others))
```

    ## # A tibble: 28 x 2
    ##    state           vap
    ##    <chr>         <dbl>
    ##  1 Arizona     4996360
    ##  2 California 28742272
    ##  3 Colorado    4015747
    ##  4 Florida    15501669
    ##  5 Georgia     7468360
    ##  6 Illinois    9624903
    ##  7 Indiana     4875975
    ##  8 Iowa        2305805
    ##  9 Kentucky    3314360
    ## 10 Maine       1036714
    ## # ... with 18 more rows

We will need to regress against the election data by county level, to
figure out voter
turnout.

``` r
load('D:/src/pa/data/elections/presidential/county/countypres_2000-2016.RData')
election_2016 <- x[which(x$year == 2016),]
#data <- inner_join(election_2016[which(election_2016$party=='democrat' | election_2016$party == 'republican'),c('FIPS','candidate','party','candidatevotes','totalvotes')],categorize_state(fl_census_data),by='FIPS')
```

# Walking to the polls

Gelman, Carilin, Stern, and Rubin’s *Bayesian Data Analysis* (Second
ed.) discusses Melbourne school children walking to school, and that
basic model applies to walking to the polls to vote. The variables in
question, slightly reprashed in our model’s domain specific terminology:

  - `y[i,j,k] = 1` if the person `i` in ~~class~~ ethnic group `j` (in
    ~~school~~ county `k`) walked to the poll to vote
  - `y[i,j,k] = 0` if person `i` did not vote
  - `M[k] = 7` is the number of ethnic groups we’re considering
  - `N[k]` is the number of people in county `k`
  - `N[j,k]` is the number of people of ethnic group `j` living in
    county `k`
  - `N` is the total number of people in the state
  - `y_bar[j,k]` is the proportion of ~~students at school `k`~~ people
    of ethnic group `j` in county `k` who voted
  - `y_bar[k]` is the proportion of people in county `k` who voted
  - `y_bar` is the proportion of people in the state who voted

We assume `Pr(y[i,j,k]) = theta[j,k]` all the individuals are
independent random variables. We also model `y_bar[j,k] ~
Binomial(prob=theta[j,k], size=N[j,k])`.

# Model for Voter Turnout (Test Case: Arizona)

To gauge how good our test case is, we use the RMSE.

``` r
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)**2))
}
```

Then we simply follow the model outlined above, but with the additional
constraint that the state-wide vote counts must be “near” the predicted
voter turnout (modulo some noise).

``` r
cat(paste0(
"model
{
  # likelihood
  tv ~ dnorm(sum(mw,fw,mb,fb,ml,fl,ov), utv); # 2764143243
  for (i in 1:N) {
    totalvotes[i] ~ dnorm(mw[i] + fw[i] + mb[i] + fb[i] + ml[i] + fl[i] + ov[i], u.totalvotes[i]); # 983241381
    
    mw[i] ~ dbin(p_male_white[i], male.white[i]);
    fw[i] ~ dbin(p_female_white[i], female.white[i]);
    mb[i] ~ dbin(p_male_black[i], max(1, male.black[i]));
    fb[i] ~ dbin(p_female_black[i], max(1, female.black[i]));
    ml[i] ~ dbin(p_male_latino[i], max(1,male.hispanic[i]));
    fl[i] ~ dbin(p_female_latino[i], max(1,female.hispanic[i]));
    ov[i] ~ dbin(p_others[i], max(1, others[i]));
  }
  # prior
  for (i in 1:N) {
    p_male_white[i] ~ dbeta(w_male_white*male.white[i] + 1, (1 - w_male_white)*male.white[i] + 1)
    p_female_white[i] ~ dbeta(w_female_white*female.white[i] + 1, (1 - w_female_white)*female.white[i] + 1)
    p_male_black[i] ~ dbeta(w_male_black*male.black[i] + 1, (1 - w_male_black)*male.black[i] + 1)
    p_female_black[i] ~ dbeta(w_female_black*female.black[i] + 1, (1 - w_female_black)*female.black[i] + 1)
    p_male_latino[i] ~ dbeta(w_male_latino*male.hispanic[i] + 1, (1 - w_male_latino)*male.hispanic[i] + 1)
    p_female_latino[i] ~ dbeta(w_female_latino*female.hispanic[i] + 1, (1 - w_female_latino)*female.hispanic[i] + 1)
    p_others[i] ~ dbeta(w_others*others[i] + 1, (1 - w_others)*others[i] + 1)
  }
  #w_male_white ~ dbeta(48,52)
  #w_female_white ~ dbeta(27,74)
  #w_male_black ~ dbeta(22,78)
  #w_female_black ~ dbeta(21,79)
  #w_male_latino ~ dbeta(30,70)
  #w_female_latino ~ dbeta(27,73)
  #w_others ~ dbeta(47,53)

  w_male_white ~ dbeta(65,45)
  w_female_white ~ dbeta(65,45)
  w_male_black ~ dbeta(59,41)
  w_female_black ~ dbeta(59,41)
  w_male_latino ~ dbeta(47,53)
  w_female_latino ~ dbeta(47,53)
  w_others ~ dbeta(47,53)

}"), file="az_election2016.model")
```

Then we run the simulation for
Arizona:

``` r
az_data <- inner_join(election_2016[which(election_2016$party=='democrat' & election_2016$state == 'Arizona'),c('FIPS','candidate','party','candidatevotes','totalvotes')],census_data,by='FIPS')
az_data$N <- nrow(az_data)
az_inits <- function() {
  N = nrow(az_data)
  return(list(w_male_white = rbeta(1,48,52),
       w_female_white = rbeta(1,27,74),
       w_male_black = rbeta(1,22,78),
       w_female_black = rbeta(1,21,79),
       w_male_latino = rbeta(1,30,70),
       w_female_latino = rbeta(1,27,73),
       w_others = rbeta(1,47,53),
       p_male_white = rbeta(N,2,2),
       p_female_white = rbeta(N,3,7),
       p_male_black = rbeta(N,2,8),
       p_female_black = rbeta(N,2,8),
       p_male_latino = rbeta(N,3,7),
       p_female_latino = rbeta(N,3,7),
       p_others = rbeta(N,5,5),
       number_of_states = 1
       ))
}
jagsModel = jags.model(file="az_election2016.model" , 
                       data=list(male.white = az_data$male.white,
                                 female.white = az_data$female.white,
                                 male.black = az_data$male.black,
                                 female.black = az_data$female.black,
                                 male.hispanic = az_data$male.hispanic,
                                 female.hispanic = az_data$female.hispanic,
                                 others = az_data$others,
                                 totalvotes = az_data$totalvotes,
                                 u.totalvotes = az_data$totalvotes,
                                 tv = sum(az_data$totalvotes),
                                 utv = sum(az_data$totalvotes),
                                 N = nrow(az_data)
                                 ),
                       inits=az_inits,
                       n.chains=5, n.adapt=12000)
```

    ## Compiling model graph
    ##    Resolving undeclared variables
    ##    Allocating nodes
    ## Graph information:
    ##    Observed stochastic nodes: 16
    ##    Unobserved stochastic nodes: 217
    ##    Total graph size: 887

    ## Warning in jags.model(file = "az_election2016.model", data =
    ## list(male.white = az_data$male.white, : Unused initial value for
    ## "number_of_states" in chain 1

    ## Warning in jags.model(file = "az_election2016.model", data =
    ## list(male.white = az_data$male.white, : Unused initial value for
    ## "number_of_states" in chain 2

    ## Warning in jags.model(file = "az_election2016.model", data =
    ## list(male.white = az_data$male.white, : Unused initial value for
    ## "number_of_states" in chain 3

    ## Warning in jags.model(file = "az_election2016.model", data =
    ## list(male.white = az_data$male.white, : Unused initial value for
    ## "number_of_states" in chain 4

    ## Warning in jags.model(file = "az_election2016.model", data =
    ## list(male.white = az_data$male.white, : Unused initial value for
    ## "number_of_states" in chain 5

    ## Initializing model
    ## 
    ## 
      |                                                        
      |                                                  |   0%
      |                                                        
      |+                                                 |   2%
      |                                                        
      |++                                                |   4%
      |                                                        
      |+++                                               |   6%
      |                                                        
      |++++                                              |   8%
      |                                                        
      |+++++                                             |  10%
      |                                                        
      |++++++                                            |  12%
      |                                                        
      |+++++++                                           |  14%
      |                                                        
      |++++++++                                          |  16%
      |                                                        
      |+++++++++                                         |  18%
      |                                                        
      |++++++++++                                        |  20%
      |                                                        
      |+++++++++++                                       |  22%
      |                                                        
      |++++++++++++                                      |  24%
      |                                                        
      |+++++++++++++                                     |  26%
      |                                                        
      |++++++++++++++                                    |  28%
      |                                                        
      |+++++++++++++++                                   |  30%
      |                                                        
      |++++++++++++++++                                  |  32%
      |                                                        
      |+++++++++++++++++                                 |  34%
      |                                                        
      |++++++++++++++++++                                |  36%
      |                                                        
      |+++++++++++++++++++                               |  38%
      |                                                        
      |++++++++++++++++++++                              |  40%
      |                                                        
      |+++++++++++++++++++++                             |  42%
      |                                                        
      |++++++++++++++++++++++                            |  44%
      |                                                        
      |+++++++++++++++++++++++                           |  46%
      |                                                        
      |++++++++++++++++++++++++                          |  48%
      |                                                        
      |+++++++++++++++++++++++++                         |  50%
      |                                                        
      |++++++++++++++++++++++++++                        |  52%
      |                                                        
      |+++++++++++++++++++++++++++                       |  54%
      |                                                        
      |++++++++++++++++++++++++++++                      |  56%
      |                                                        
      |+++++++++++++++++++++++++++++                     |  58%
      |                                                        
      |++++++++++++++++++++++++++++++                    |  60%
      |                                                        
      |+++++++++++++++++++++++++++++++                   |  62%
      |                                                        
      |++++++++++++++++++++++++++++++++                  |  64%
      |                                                        
      |+++++++++++++++++++++++++++++++++                 |  66%
      |                                                        
      |++++++++++++++++++++++++++++++++++                |  68%
      |                                                        
      |+++++++++++++++++++++++++++++++++++               |  70%
      |                                                        
      |++++++++++++++++++++++++++++++++++++              |  72%
      |                                                        
      |+++++++++++++++++++++++++++++++++++++             |  74%
      |                                                        
      |++++++++++++++++++++++++++++++++++++++            |  76%
      |                                                        
      |+++++++++++++++++++++++++++++++++++++++           |  78%
      |                                                        
      |++++++++++++++++++++++++++++++++++++++++          |  80%
      |                                                        
      |+++++++++++++++++++++++++++++++++++++++++         |  82%
      |                                                        
      |++++++++++++++++++++++++++++++++++++++++++        |  84%
      |                                                        
      |+++++++++++++++++++++++++++++++++++++++++++       |  86%
      |                                                        
      |++++++++++++++++++++++++++++++++++++++++++++      |  88%
      |                                                        
      |+++++++++++++++++++++++++++++++++++++++++++++     |  90%
      |                                                        
      |++++++++++++++++++++++++++++++++++++++++++++++    |  92%
      |                                                        
      |+++++++++++++++++++++++++++++++++++++++++++++++   |  94%
      |                                                        
      |++++++++++++++++++++++++++++++++++++++++++++++++  |  96%
      |                                                        
      |+++++++++++++++++++++++++++++++++++++++++++++++++ |  98%
      |                                                        
      |++++++++++++++++++++++++++++++++++++++++++++++++++| 100%

``` r
update(jagsModel, n.iter=2000)
```

    ## 
      |                                                        
      |                                                  |   0%
      |                                                        
      |*                                                 |   2%
      |                                                        
      |**                                                |   4%
      |                                                        
      |***                                               |   6%
      |                                                        
      |****                                              |   8%
      |                                                        
      |*****                                             |  10%
      |                                                        
      |******                                            |  12%
      |                                                        
      |*******                                           |  14%
      |                                                        
      |********                                          |  16%
      |                                                        
      |*********                                         |  18%
      |                                                        
      |**********                                        |  20%
      |                                                        
      |***********                                       |  22%
      |                                                        
      |************                                      |  24%
      |                                                        
      |*************                                     |  26%
      |                                                        
      |**************                                    |  28%
      |                                                        
      |***************                                   |  30%
      |                                                        
      |****************                                  |  32%
      |                                                        
      |*****************                                 |  34%
      |                                                        
      |******************                                |  36%
      |                                                        
      |*******************                               |  38%
      |                                                        
      |********************                              |  40%
      |                                                        
      |*********************                             |  42%
      |                                                        
      |**********************                            |  44%
      |                                                        
      |***********************                           |  46%
      |                                                        
      |************************                          |  48%
      |                                                        
      |*************************                         |  50%
      |                                                        
      |**************************                        |  52%
      |                                                        
      |***************************                       |  54%
      |                                                        
      |****************************                      |  56%
      |                                                        
      |*****************************                     |  58%
      |                                                        
      |******************************                    |  60%
      |                                                        
      |*******************************                   |  62%
      |                                                        
      |********************************                  |  64%
      |                                                        
      |*********************************                 |  66%
      |                                                        
      |**********************************                |  68%
      |                                                        
      |***********************************               |  70%
      |                                                        
      |************************************              |  72%
      |                                                        
      |*************************************             |  74%
      |                                                        
      |**************************************            |  76%
      |                                                        
      |***************************************           |  78%
      |                                                        
      |****************************************          |  80%
      |                                                        
      |*****************************************         |  82%
      |                                                        
      |******************************************        |  84%
      |                                                        
      |*******************************************       |  86%
      |                                                        
      |********************************************      |  88%
      |                                                        
      |*********************************************     |  90%
      |                                                        
      |**********************************************    |  92%
      |                                                        
      |***********************************************   |  94%
      |                                                        
      |************************************************  |  96%
      |                                                        
      |************************************************* |  98%
      |                                                        
      |**************************************************| 100%

``` r
expected_values <- function(model, prefix="p_") {
  params <- c(paste0(prefix, 'male_white'),
              paste0(prefix, 'female_white'),
              paste0(prefix, 'male_black'),
              paste0(prefix, 'female_black'),
              paste0(prefix, 'male_latino'),
              paste0(prefix, 'female_latino'),
              paste0(prefix, 'others'))
  samps <- coda.samples(model, params, n.iter=4000)

  mc1 <- as.data.frame(as.matrix(samps))
  white_male <- rep(0,nrow(az_data))
  white_female <- rep(0,nrow(az_data))
  black_male <- rep(0,nrow(az_data))
  black_female <- rep(0,nrow(az_data))
  latino_male <- rep(0,nrow(az_data))
  latino_female <- rep(0,nrow(az_data))
  others <- rep(0,nrow(az_data))
  wm_sd <- rep(0,nrow(az_data))
  wf_sd <- rep(0,nrow(az_data))
  bm_sd <- rep(0,nrow(az_data))
  bf_sd <- rep(0,nrow(az_data))
  lm_sd <- rep(0,nrow(az_data))
  lf_sd <- rep(0,nrow(az_data))
  oth_sd <- rep(0,nrow(az_data))
  results <- list();
  for(i in 1:nrow(az_data)) {
    k1 <- paste0(prefix, "male_white[",i,"]");
    r1 <- hdi(mc1[k1]);
    vs <- unlist(Filter(function(x) min(r1) <= x & x <= max(r1), as.list(mc1[k1])[[1]]));
    white_male[i] <- mean(vs);
    wm_sd[i] <- sd(vs);
    k1 <- paste0(prefix, "female_white[",i,"]")
    r1 <- hdi(mc1[k1])
    vs <- unlist(Filter(function(x) min(r1) <= x & x <= max(r1), as.list(mc1[k1])[[1]]));
    white_female[i] <- mean(vs)
    wf_sd[i] <- sd(vs);
    k1 <- paste0(prefix, "male_black[",i,"]")
    r1 <- hdi(mc1[k1])
    vs <- unlist(Filter(function(x) min(r1) <= x & x <= max(r1), as.list(mc1[k1])[[1]]));
    black_male[i] <- mean(vs)
    bm_sd[i] <- sd(vs);
    k1 <- paste0(prefix, "female_black[",i,"]")
    r1 <- hdi(mc1[k1])
    vs <- unlist(Filter(function(x) min(r1) <= x & x <= max(r1), as.list(mc1[k1])[[1]]));
    black_female[i] <- mean(vs)
    bf_sd[i] <- sd(vs);
    k1 <- paste0(prefix, "male_latino[",i,"]")
    r1 <- hdi(mc1[k1])
    vs <- unlist(Filter(function(x) min(r1) <= x & x <= max(r1), as.list(mc1[k1])[[1]]));
    latino_male[i] <- mean(vs)
    lm_sd[i] <- sd(vs);
    k1 <- paste0(prefix, "female_latino[",i,"]")
    r1 <- hdi(mc1[k1])
    vs <- unlist(Filter(function(x) min(r1) <= x & x <= max(r1), as.list(mc1[k1])[[1]]));
    latino_female[i] <- mean(vs)
    lf_sd[i] <- sd(vs);
    k1 <- paste0(prefix, "others[",i,"]")
    r1 <- hdi(mc1[k1])
    vs <- unlist(Filter(function(x) min(r1) <= x & x <= max(r1), as.list(mc1[k1])[[1]]));
    others[i] <- mean(vs)
    oth_sd[i] <- sd(vs);
  }
  k1 <- paste0(prefix, "white_male");
  results[k1] <- list(white_male)
  k1 <- paste0(prefix, "white_female");
  results[k1] <- list(white_female)
  k1 <- paste0(prefix, "black_male");
  results[k1] <- list(black_male)
  k1 <- paste0(prefix, "black_female");
  results[k1] <- list(black_female)
  k1 <- paste0(prefix, "latino_male");
  results[k1] <- list(latino_male)
  k1 <- paste0(prefix, "latino_female");
  results[k1] <- list(latino_female)
  k1 <- paste0(prefix, "others")
  results[k1] <- list(others)
  k1 <- paste0(prefix, "white_male.sd");
  results[k1] <- list(wm_sd)
  k1 <- paste0(prefix, "white_female.sd");
  results[k1] <- list(wf_sd)
  k1 <- paste0(prefix, "black_male.sd");
  results[k1] <- list(bm_sd)
  k1 <- paste0(prefix, "black_female.sd");
  results[k1] <- list(bf_sd)
  k1 <- paste0(prefix, "latino_male.sd");
  results[k1] <- list(lm_sd)
  k1 <- paste0(prefix, "latino_female.sd");
  results[k1] <- list(lf_sd)
  k1 <- paste0(prefix, "others.sd")
  results[k1] <- list(oth_sd)
  return(results);
}
```

``` r
az_totalvote_ps <- function(model) {
  return(expected_values(model,"p_"))
}
az_rmse_totalvote <- function(model) {
  estimates <- az_totalvote_ps(model);
  return(rmse(az_data$totalvotes, estimates$p_white_male*az_data$male.white + estimates$p_white_female*az_data$female.white + estimates$p_black_male*az_data$male.black + estimates$p_black_female*az_data$female.black + estimates$p_latino_male*az_data$male.hispanic + estimates$p_latino_female*az_data$female.hispanic + estimates$p_others*az_data$others));
}
```

It’s hard to gauge how bad this is, since anything over 100 votes
“sounds scary”. Lets see the RMSE, which is in units of “votes”, and
what fraction of the total votes is in
error:

``` r
rmse(az_data$totalvotes, p_white_male*az_data$male.white + p_white_female*az_data$female.white + p_black_male*az_data$male.black + p_black_female*az_data$female.black + p_latino_male*az_data$male.hispanic + p_latino_female*az_data$female.hispanic + p_others*az_data$others)
```

    ## [1] 12579.19

And as a fraction of total
votes:

``` r
rmse(az_data$totalvotes, p_white_male*az_data$male.white + p_white_female*az_data$female.white + p_black_male*az_data$male.black + p_black_female*az_data$female.black + p_latino_male*az_data$male.hispanic + p_latino_female*az_data$female.hispanic + p_others*az_data$others)/sum(az_data$totalvotes)
```

    ## [1] 0.004785225

When `tau=16` uniformly, rmse = 12486.46 and as fraction of vote
0.004749949. When `tau` is not uniform, rmse = 7190.069 and fraction of
vote is 0.00273516 When `tau=16**2`, rmse = 18965.42 and fraction of
vote is 0.0072146

# Turnout Estimates Using Clinton Votes

``` r
cat(paste0(
"model
{
  # likelihood
  tv ~ dnorm(sum(mw,fw,mb,fb,ml,fl,ov), utv); # 2764143243
  c_tv ~ dnorm(sum(c_mw, c_fw, c_mb, c_fb, c_ml, c_fl, c_ov), tau_ctv);
  for (i in 1:N) {
    totalvotes[i] ~ dnorm(mw[i] + fw[i] + mb[i] + fb[i] + ml[i] + fl[i] + ov[i], u.totalvotes[i]); # 983241381
    
    mw[i] ~ dbin(p_male_white[i], male.white[i]);
    fw[i] ~ dbin(p_female_white[i], female.white[i]);
    mb[i] ~ dbin(p_male_black[i], max(1, male.black[i]));
    fb[i] ~ dbin(p_female_black[i], max(1, female.black[i]));
    ml[i] ~ dbin(p_male_latino[i], max(1,male.hispanic[i]));
    fl[i] ~ dbin(p_female_latino[i], max(1,female.hispanic[i]));
    ov[i] ~ dbin(p_others[i], max(1, others[i]));
    
    clintonvotes[i] ~ dnorm(c_mw[i] + c_fw[i] + c_mb[i] + c_fb[i] + c_ml[i] + c_fl[i] + c_ov[i], u.clintonvotes[i]); # 983241381
    
    c_mw[i] ~ dbin(ilogit(beta_clinton_male_white[i]), mw[i]);
    c_fw[i] ~ dbin(ilogit(beta_clinton_female_white[i]), fw[i]);
    c_mb[i] ~ dbin(ilogit(beta_clinton_male_black[i]), max(1, mb[i]));
    c_fb[i] ~ dbin(ilogit(beta_clinton_female_black[i]), max(1, fb[i]));
    c_ml[i] ~ dbin(ilogit(beta_clinton_male_latino[i]), max(1,ml[i]));
    c_fl[i] ~ dbin(ilogit(beta_clinton_female_latino[i]), max(1,fl[i]));
    c_ov[i] ~ dbin(ilogit(beta_clinton_others[i]), max(1, ov[i]));
  }
  # prior
  for (i in 1:N) {
    p_male_white[i] ~ dbeta(w_male_white*male.white[i] + 1, (1 - w_male_white)*male.white[i] + 1)
    p_female_white[i] ~ dbeta(w_female_white*female.white[i] + 1, (1 - w_female_white)*female.white[i] + 1)
    p_male_black[i] ~ dbeta(w_male_black*male.black[i] + 1, (1 - w_male_black)*male.black[i] + 1)
    p_female_black[i] ~ dbeta(w_female_black*female.black[i] + 1, (1 - w_female_black)*female.black[i] + 1)
    p_male_latino[i] ~ dbeta(w_male_latino*male.hispanic[i] + 1, (1 - w_male_latino)*male.hispanic[i] + 1)
    p_female_latino[i] ~ dbeta(w_female_latino*female.hispanic[i] + 1, (1 - w_female_latino)*female.hispanic[i] + 1)
    p_others[i] ~ dbeta(w_others*others[i] + 1, (1 - w_others)*others[i] + 1)

    beta_clinton_male_white[i] ~ dnorm(beta.white_male, 1/beta.white_male.var)
    beta_clinton_female_white[i] ~ dnorm(beta.white_female, 1/beta.white_female.var)
    beta_clinton_male_latino[i] ~ dnorm(beta.latino_male, 1/beta.latino_male.var)
    beta_clinton_female_latino[i] ~ dnorm(beta.latino_female, 1/beta.latino_female.var)
    beta_clinton_male_black[i] ~ dnorm(beta.black_male, 1/beta.black_male.var)
    beta_clinton_female_black[i] ~ dnorm(beta.black_female, 1/beta.black_female.var)
    beta_clinton_others[i] ~ dnorm(beta.others, 1/beta.others.var)
  }
  #w_male_white ~ dbeta(48,52)
  #w_female_white ~ dbeta(27,74)
  #w_male_black ~ dbeta(22,78)
  #w_female_black ~ dbeta(21,79)
  #w_male_latino ~ dbeta(30,70)
  #w_female_latino ~ dbeta(27,73)
  #w_others ~ dbeta(47,53)

  w_male_white ~ dbeta(65,45)
  w_female_white ~ dbeta(65,45)
  w_male_black ~ dbeta(59,41)
  w_female_black ~ dbeta(59,41)
  w_male_latino ~ dbeta(47,53)
  w_female_latino ~ dbeta(47,53)
  w_others ~ dbeta(47,53)

}"), file="az_turnout_with_clinton_election2016.model")
```

``` r
national_exit_polls <- log_coefs(national_exit_polls)
az_race <- log_coefs(exit_poll_df[which(exit_poll_df$state=='Arizona'),])
beta.white_male <- az_race[which(az_race$questions_id==7 & az_race$options=='White men'),]$clinton_coef
beta.white_male.var <- az_race[which(az_race$questions_id==7 & az_race$options=='White men'),]$log_var
beta.white_female <- az_race[which(az_race$questions_id==7 & az_race$options=='White women'),]$clinton_coef
beta.white_female.var <- az_race[which(az_race$questions_id==7 & az_race$options=='White women'),]$log_var
beta.latino_male <- az_race[which(az_race$questions_id==7 & az_race$options=='Latino men'),]$clinton_coef
beta.latino_male.var <- az_race[which(az_race$questions_id==7 & az_race$options=='Latino men'),]$log_var
beta.latino_female <- az_race[which(az_race$questions_id==7 & az_race$options=='Latino women'),]$clinton_coef
beta.latino_female.var <- az_race[which(az_race$questions_id==7 & az_race$options=='Latino women'),]$log_var
beta.black_male <- national_exit_polls[which(national_exit_polls$questions_id == 7 & national_exit_polls$options=='Black men'),]$clinton_coef
beta.black_male.var <- national_exit_polls[which(national_exit_polls$questions_id == 7 & national_exit_polls$options=='Black men'),]$log_var
beta.black_female <- national_exit_polls[which(national_exit_polls$questions_id == 7 & national_exit_polls$options=='Black women'),]$clinton_coef
beta.black_female.var <- national_exit_polls[which(national_exit_polls$questions_id == 7 & national_exit_polls$options=='Black women'),]$log_var
beta.others <- national_exit_polls[which(national_exit_polls$questions_id == 7 & national_exit_polls$options=='Others'),]$clinton_coef
beta.others.var <- national_exit_polls[which(national_exit_polls$questions_id == 7 & national_exit_polls$options=='Others'),]$log_var
```

``` r
az_data <- inner_join(election_2016[which(election_2016$party=='democrat' & election_2016$state == 'Arizona'),c('FIPS','candidate','party','candidatevotes','totalvotes')],census_data,by='FIPS')
colnames(az_data)[colnames(az_data)=='candidatevotes'] <- 'clintonvotes';
az_data$N <- nrow(az_data)
az_inits <- function() {
  N = nrow(az_data)
  return(list(w_male_white = rbeta(1,48,52),
       w_female_white = rbeta(1,27,74),
       w_male_black = rbeta(1,22,78),
       w_female_black = rbeta(1,21,79),
       w_male_latino = rbeta(1,30,70),
       w_female_latino = rbeta(1,27,73),
       w_others = rbeta(1,47,53),
       p_male_white = rbeta(N,2,2),
       p_female_white = rbeta(N,3,7),
       p_male_black = rbeta(N,2,8),
       p_female_black = rbeta(N,2,8),
       p_male_latino = rbeta(N,3,7),
       p_female_latino = rbeta(N,3,7),
       p_others = rbeta(N,5,5),
       number_of_states = 1
       ))
}
jagsModel = jags.model(file="az_turnout_with_clinton_election2016.model" , 
                       data=list(male.white = az_data$male.white,
                                 female.white = az_data$female.white,
                                 male.black = az_data$male.black,
                                 female.black = az_data$female.black,
                                 male.hispanic = az_data$male.hispanic,
                                 female.hispanic = az_data$female.hispanic,
                                 others = az_data$others,
                                 totalvotes = az_data$totalvotes,
                                 u.totalvotes = az_data$totalvotes,
                                 tv = sum(az_data$totalvotes),
                                 utv = sum(az_data$totalvotes),
                                 N = nrow(az_data),
                                 clintonvotes = az_data$clintonvotes,
                                 tau_ctv = sum(az_data$clintonvotes),
                                 u.clintonvotes = az_data$clintonvotes,
                                 beta.white_male = az_race[which(az_race$questions_id==7 & az_race$options=='White men'),]$clinton_coef,
                                 beta.white_male.var = az_race[which(az_race$questions_id==7 & az_race$options=='White men'),]$log_var,
                                 beta.white_female = az_race[which(az_race$questions_id==7 & az_race$options=='White women'),]$clinton_coef,
                                 beta.white_female.var = az_race[which(az_race$questions_id==7 & az_race$options=='White women'),]$log_var,
                                 beta.latino_male = az_race[which(az_race$questions_id==7 & az_race$options=='Latino men'),]$clinton_coef,
                                 beta.latino_male.var = az_race[which(az_race$questions_id==7 & az_race$options=='Latino men'),]$log_var,
                                 beta.latino_female = az_race[which(az_race$questions_id==7 & az_race$options=='Latino women'),]$clinton_coef,
                                 beta.latino_female.var = az_race[which(az_race$questions_id==7 & az_race$options=='Latino women'),]$log_var,
                                 beta.black_male = national_exit_polls[which(national_exit_polls$questions_id == 7 & national_exit_polls$options=='Black men'),]$clinton_coef,
                                 beta.black_male.var = national_exit_polls[which(national_exit_polls$questions_id == 7 & national_exit_polls$options=='Black men'),]$log_var,
                                 beta.black_female = national_exit_polls[which(national_exit_polls$questions_id == 7 & national_exit_polls$options=='Black women'),]$clinton_coef,
                                 beta.black_female.var = national_exit_polls[which(national_exit_polls$questions_id == 7 & national_exit_polls$options=='Black women'),]$log_var,
                                 beta.others = national_exit_polls[which(national_exit_polls$questions_id == 7 & national_exit_polls$options=='Others'),]$clinton_coef,
                                 beta.others.var = national_exit_polls[which(national_exit_polls$questions_id == 7 & national_exit_polls$options=='Others'),]$log_var
                       ),
                       inits=az_inits,
                       n.chains=4, n.adapt=22000)
```

    ## Compiling model graph
    ##    Resolving undeclared variables
    ##    Allocating nodes
    ## Graph information:
    ##    Observed stochastic nodes: 31
    ##    Unobserved stochastic nodes: 428
    ##    Total graph size: 1353

    ## Warning in jags.model(file =
    ## "az_turnout_with_clinton_election2016.model", : Unused initial value for
    ## "number_of_states" in chain 1

    ## Warning in jags.model(file =
    ## "az_turnout_with_clinton_election2016.model", : Unused initial value for
    ## "number_of_states" in chain 2

    ## Warning in jags.model(file =
    ## "az_turnout_with_clinton_election2016.model", : Unused initial value for
    ## "number_of_states" in chain 3

    ## Warning in jags.model(file =
    ## "az_turnout_with_clinton_election2016.model", : Unused initial value for
    ## "number_of_states" in chain 4

    ## Initializing model
    ## 
    ## 
      |                                                        
      |                                                  |   0%
      |                                                        
      |+                                                 |   2%
      |                                                        
      |++                                                |   4%
      |                                                        
      |+++                                               |   6%
      |                                                        
      |++++                                              |   8%
      |                                                        
      |+++++                                             |  10%
      |                                                        
      |++++++                                            |  12%
      |                                                        
      |+++++++                                           |  14%
      |                                                        
      |++++++++                                          |  16%
      |                                                        
      |+++++++++                                         |  18%
      |                                                        
      |++++++++++                                        |  20%
      |                                                        
      |+++++++++++                                       |  22%
      |                                                        
      |++++++++++++                                      |  24%
      |                                                        
      |+++++++++++++                                     |  26%
      |                                                        
      |++++++++++++++                                    |  28%
      |                                                        
      |+++++++++++++++                                   |  30%
      |                                                        
      |++++++++++++++++                                  |  32%
      |                                                        
      |+++++++++++++++++                                 |  34%
      |                                                        
      |++++++++++++++++++                                |  36%
      |                                                        
      |+++++++++++++++++++                               |  38%
      |                                                        
      |++++++++++++++++++++                              |  40%
      |                                                        
      |+++++++++++++++++++++                             |  42%
      |                                                        
      |++++++++++++++++++++++                            |  44%
      |                                                        
      |+++++++++++++++++++++++                           |  46%
      |                                                        
      |++++++++++++++++++++++++                          |  48%
      |                                                        
      |+++++++++++++++++++++++++                         |  50%
      |                                                        
      |++++++++++++++++++++++++++                        |  52%
      |                                                        
      |+++++++++++++++++++++++++++                       |  54%
      |                                                        
      |++++++++++++++++++++++++++++                      |  56%
      |                                                        
      |+++++++++++++++++++++++++++++                     |  58%
      |                                                        
      |++++++++++++++++++++++++++++++                    |  60%
      |                                                        
      |+++++++++++++++++++++++++++++++                   |  62%
      |                                                        
      |++++++++++++++++++++++++++++++++                  |  64%
      |                                                        
      |+++++++++++++++++++++++++++++++++                 |  66%
      |                                                        
      |++++++++++++++++++++++++++++++++++                |  68%
      |                                                        
      |+++++++++++++++++++++++++++++++++++               |  70%
      |                                                        
      |++++++++++++++++++++++++++++++++++++              |  72%
      |                                                        
      |+++++++++++++++++++++++++++++++++++++             |  74%
      |                                                        
      |++++++++++++++++++++++++++++++++++++++            |  76%
      |                                                        
      |+++++++++++++++++++++++++++++++++++++++           |  78%
      |                                                        
      |++++++++++++++++++++++++++++++++++++++++          |  80%
      |                                                        
      |+++++++++++++++++++++++++++++++++++++++++         |  82%
      |                                                        
      |++++++++++++++++++++++++++++++++++++++++++        |  84%
      |                                                        
      |+++++++++++++++++++++++++++++++++++++++++++       |  86%
      |                                                        
      |++++++++++++++++++++++++++++++++++++++++++++      |  88%
      |                                                        
      |+++++++++++++++++++++++++++++++++++++++++++++     |  90%
      |                                                        
      |++++++++++++++++++++++++++++++++++++++++++++++    |  92%
      |                                                        
      |+++++++++++++++++++++++++++++++++++++++++++++++   |  94%
      |                                                        
      |++++++++++++++++++++++++++++++++++++++++++++++++  |  96%
      |                                                        
      |+++++++++++++++++++++++++++++++++++++++++++++++++ |  98%
      |                                                        
      |++++++++++++++++++++++++++++++++++++++++++++++++++| 100%

``` r
update(jagsModel, n.iter=4000)
```

    ## 
      |                                                        
      |                                                  |   0%
      |                                                        
      |*                                                 |   2%
      |                                                        
      |**                                                |   4%
      |                                                        
      |***                                               |   6%
      |                                                        
      |****                                              |   8%
      |                                                        
      |*****                                             |  10%
      |                                                        
      |******                                            |  12%
      |                                                        
      |*******                                           |  14%
      |                                                        
      |********                                          |  16%
      |                                                        
      |*********                                         |  18%
      |                                                        
      |**********                                        |  20%
      |                                                        
      |***********                                       |  22%
      |                                                        
      |************                                      |  24%
      |                                                        
      |*************                                     |  26%
      |                                                        
      |**************                                    |  28%
      |                                                        
      |***************                                   |  30%
      |                                                        
      |****************                                  |  32%
      |                                                        
      |*****************                                 |  34%
      |                                                        
      |******************                                |  36%
      |                                                        
      |*******************                               |  38%
      |                                                        
      |********************                              |  40%
      |                                                        
      |*********************                             |  42%
      |                                                        
      |**********************                            |  44%
      |                                                        
      |***********************                           |  46%
      |                                                        
      |************************                          |  48%
      |                                                        
      |*************************                         |  50%
      |                                                        
      |**************************                        |  52%
      |                                                        
      |***************************                       |  54%
      |                                                        
      |****************************                      |  56%
      |                                                        
      |*****************************                     |  58%
      |                                                        
      |******************************                    |  60%
      |                                                        
      |*******************************                   |  62%
      |                                                        
      |********************************                  |  64%
      |                                                        
      |*********************************                 |  66%
      |                                                        
      |**********************************                |  68%
      |                                                        
      |***********************************               |  70%
      |                                                        
      |************************************              |  72%
      |                                                        
      |*************************************             |  74%
      |                                                        
      |**************************************            |  76%
      |                                                        
      |***************************************           |  78%
      |                                                        
      |****************************************          |  80%
      |                                                        
      |*****************************************         |  82%
      |                                                        
      |******************************************        |  84%
      |                                                        
      |*******************************************       |  86%
      |                                                        
      |********************************************      |  88%
      |                                                        
      |*********************************************     |  90%
      |                                                        
      |**********************************************    |  92%
      |                                                        
      |***********************************************   |  94%
      |                                                        
      |************************************************  |  96%
      |                                                        
      |************************************************* |  98%
      |                                                        
      |**************************************************| 100%

``` r
totalvotes_rmse <- az_rmse_totalvote(jagsModel);
```

    ## 
      |                                                        
      |                                                  |   0%
      |                                                        
      |*                                                 |   2%
      |                                                        
      |**                                                |   4%
      |                                                        
      |***                                               |   6%
      |                                                        
      |****                                              |   8%
      |                                                        
      |*****                                             |  10%
      |                                                        
      |******                                            |  12%
      |                                                        
      |*******                                           |  14%
      |                                                        
      |********                                          |  16%
      |                                                        
      |*********                                         |  18%
      |                                                        
      |**********                                        |  20%
      |                                                        
      |***********                                       |  22%
      |                                                        
      |************                                      |  24%
      |                                                        
      |*************                                     |  26%
      |                                                        
      |**************                                    |  28%
      |                                                        
      |***************                                   |  30%
      |                                                        
      |****************                                  |  32%
      |                                                        
      |*****************                                 |  34%
      |                                                        
      |******************                                |  36%
      |                                                        
      |*******************                               |  38%
      |                                                        
      |********************                              |  40%
      |                                                        
      |*********************                             |  42%
      |                                                        
      |**********************                            |  44%
      |                                                        
      |***********************                           |  46%
      |                                                        
      |************************                          |  48%
      |                                                        
      |*************************                         |  50%
      |                                                        
      |**************************                        |  52%
      |                                                        
      |***************************                       |  54%
      |                                                        
      |****************************                      |  56%
      |                                                        
      |*****************************                     |  58%
      |                                                        
      |******************************                    |  60%
      |                                                        
      |*******************************                   |  62%
      |                                                        
      |********************************                  |  64%
      |                                                        
      |*********************************                 |  66%
      |                                                        
      |**********************************                |  68%
      |                                                        
      |***********************************               |  70%
      |                                                        
      |************************************              |  72%
      |                                                        
      |*************************************             |  74%
      |                                                        
      |**************************************            |  76%
      |                                                        
      |***************************************           |  78%
      |                                                        
      |****************************************          |  80%
      |                                                        
      |*****************************************         |  82%
      |                                                        
      |******************************************        |  84%
      |                                                        
      |*******************************************       |  86%
      |                                                        
      |********************************************      |  88%
      |                                                        
      |*********************************************     |  90%
      |                                                        
      |**********************************************    |  92%
      |                                                        
      |***********************************************   |  94%
      |                                                        
      |************************************************  |  96%
      |                                                        
      |************************************************* |  98%
      |                                                        
      |**************************************************| 100%

``` r
totalvotes_rmse/sum(az_data$totalvotes)
```

    ## [1] 0.003450348

Predictions for Clinton votes, we find similarly.

``` r
az_clinton_betas <- function(model) {
  expected_values(model, prefix="beta_clinton_");
}
```

``` r
az_rmse_clintonvote <- function(model) {
  estimates <- az_clinton_betas(model);
  return(rmse(az_data$totalvotes, invlogit(estimates$beta_clinton_white_male)*az_data$male.white + invlogit(estimates$beta_clinton_white_female)*az_data$female.white + invlogit(estimates$beta_clinton_black_male)*az_data$male.black + invlogit(estimates$beta_clinton_black_female)*az_data$female.black + invlogit(estimates$beta_clinton_latino_male)*az_data$male.hispanic + invlogit(estimates$beta_clinton_latino_female)*az_data$female.hispanic + invlogit(estimates$beta_clinton_others)*az_data$others));
}
```

# Post-Stratified Model

We can now use the technique of post-stratification to better estimate
the vote turnout.

``` r
cat(paste0(
"model
{
  # likelihood
  tv ~ dnorm(sum(mw,fw,mb,fb,ml,fl,ov, high_school, some_college, college_grad, postgrad, age_18_19_voter, age_20_voter, age_21_voter,age_22_24_voter, age_25_29_voter, age_30_34_voter, age_35_39_voter, age_40_44_voter, age_45_49_voter, age_50_54_voter, age_55_59_voter, age_60_61_voter, age_62_64_voter, age_retiree_voter)/3.0, utv); # 2764143243
  for (i in 1:N) {
    totalvotes[i] ~ dnorm( ((mw[i] + fw[i] + mb[i] + fb[i] + ml[i] + fl[i] + ov[i])*(high_school[i] + some_college[i] + college_grad[i] + postgrad[i]) * (age_18_19_voter[i] + age_20_voter[i] + age_21_voter[i] + age_22_24_voter[i] + age_25_29_voter[i] + age_30_34_voter[i] + age_35_39_voter[i] + age_40_44_voter[i] + age_45_49_voter[i] + age_50_54_voter[i] + age_55_59_voter[i] + age_60_61_voter[i] + age_62_64_voter[i] + age_retiree_voter[i]))**(1.0/3.0), u.totalvotes[i]); # 983241381
    
    mw[i] ~ dbin(p_male_white[i], male.white[i]);
    fw[i] ~ dbin(p_female_white[i], female.white[i]);
    mb[i] ~ dbin(p_male_black[i], max(1, male.black[i]));
    fb[i] ~ dbin(p_female_black[i], max(1, female.black[i]));
    ml[i] ~ dbin(p_male_latino[i], max(1,male.hispanic[i]));
    fl[i] ~ dbin(p_female_latino[i], max(1,female.hispanic[i]));
    ov[i] ~ dbin(p_others[i], max(1, others[i]));
    
    high_school[i] ~ dbin(ilogit(beta_high_school[i]), max(1, education.high_school[i]));
    some_college[i] ~ dbin(ilogit(beta_some_college[i]), max(1, education.some_college[i]));
    college_grad[i] ~ dbin(ilogit(beta_college_grad[i]), max(1, education.college_grad[i]));
    postgrad[i] ~ dbin(ilogit(beta_postgrad[i]), max(1, education.postgrad[i]));
    
    age_18_19_voter[i] ~ dbin(ilogit(p_age_18_19), max(1,age.18_19[i]));
    age_20_voter[i] ~ dbin(ilogit(p_age_20), max(1,age.20[i]));
    age_21_voter[i] ~ dbin(ilogit(p_age_21), max(1,age.21[i]));
    age_22_24_voter[i] ~ dbin(ilogit(p_age_22_24), max(1,age.22_24[i]));
    age_25_29_voter[i] ~ dbin(ilogit(p_age_25_29), max(1,age.25_29[i]));
    age_30_34_voter[i] ~ dbin(ilogit(p_age_30_34), max(1,age.30_34[i]));
    age_35_39_voter[i] ~ dbin(ilogit(p_age_35_39), max(1,age.35_39[i]));
    age_40_44_voter[i] ~ dbin(ilogit(p_age_40_44), max(1,age.40_44[i]));
    age_45_49_voter[i] ~ dbin(ilogit(p_age_45_49), max(1,age.45_49[i]));
    age_50_54_voter[i] ~ dbin(ilogit(p_age_50_54), max(1,age.50_54[i]));
    age_55_59_voter[i] ~ dbin(ilogit(p_age_55_59), max(1,age.55_59[i]));
    age_60_61_voter[i] ~ dbin(ilogit(p_age_60_61), max(1,age.60_61[i]));
    age_62_64_voter[i] ~ dbin(ilogit(p_age_62_64), max(1,age.62_64[i]));
    age_retiree_voter[i] ~ dbin(ilogit(beta_age_retiree), max(1,age.retirees[i]));
  }
  # prior
  for (i in 1:N) {
    beta_high_school[i] ~ dnorm(beta_education[1], tau_education[1]);
    beta_some_college[i] ~ dnorm(beta_education[2], tau_education[2]);
    beta_college_grad[i] ~ dnorm(beta_education[3], tau_education[3]);
    beta_postgrad[i] ~ dnorm(beta_education[4], tau_education[4])
    p_male_white[i] ~ dbeta(w_male_white*male.white[i] + 1, (1 - w_male_white)*male.white[i] + 1)
    p_female_white[i] ~ dbeta(w_female_white*female.white[i] + 1, (1 - w_female_white)*female.white[i] + 1)
    p_male_black[i] ~ dbeta(w_male_black*male.black[i] + 1, (1 - w_male_black)*male.black[i] + 1)
    p_female_black[i] ~ dbeta(w_female_black*female.black[i] + 1, (1 - w_female_black)*female.black[i] + 1)
    p_male_latino[i] ~ dbeta(w_male_latino*male.hispanic[i] + 1, (1 - w_male_latino)*male.hispanic[i] + 1)
    p_female_latino[i] ~ dbeta(w_female_latino*female.hispanic[i] + 1, (1 - w_female_latino)*female.hispanic[i] + 1)
    p_others[i] ~ dbeta(w_others*others[i] + 1, (1 - w_others)*others[i] + 1)
  }
  # these are the likelihoods of voting for Clinton, not the probability of voting at all...
  p_age_18_19 ~ dnorm(a + 18.5*b, 16);
  p_age_20 ~ dnorm(a + 20*b, 16);
  p_age_21 ~ dnorm(a + 21*b, 16);
  p_age_22_24 ~ dnorm(a + 23*b, 16);
  p_age_25_29 ~ dnorm(a + 27*b, 16);
  p_age_30_34 ~ dnorm(a + 32*b, 16);
  p_age_35_39 ~ dnorm(a + 37*b, 16);
  p_age_40_44 ~ dnorm(a + 42*b, 16);
  p_age_45_49 ~ dnorm(a + 47*b, 16);
  p_age_50_54 ~ dnorm(a + 52*b, 16);
  p_age_55_59 ~ dnorm(a + 57*b, 16);
  p_age_60_61 ~ dnorm(a + 60.5*b, 16);
  p_age_62_64 ~ dnorm(a + 63*b, 16);
  beta_age_retiree ~ dnorm(0.05314496, 152);
  a ~ dnorm(-2.098308 , 4);
  b ~ dnorm(0.03916914, 5170.81);

  w_male_white ~ dbeta(65,45);
  w_female_white ~ dbeta(65,45);
  w_male_black ~ dbeta(59,41);
  w_female_black ~ dbeta(59,41);
  w_male_latino ~ dbeta(47,53);
  w_female_latino ~ dbeta(47,53);
  w_others ~ dbeta(47,53);

}"), file="az_post_stratification_election2016.model")
```

We determine the numeric distribution of `a` and `b` by considering the
following:

``` r
age <- log_coefs(exit_poll_df[which(exit_poll_df$state=='Arizona' & exit_poll_df$questions_id==3),])

age_wilson_width <- wilson_width(0.01*age$options_perc, age$num_respondents, 2)

age_est <- wilson_center(0.01*age$options_perc, age$num_respondents, 2)

# fraction of age bracket went out to vote
proportion_of_age <- ((age_est-2*age_wilson_width)*sum(az_data$totalvotes))/c(sum(az_data$age.18_19) + sum(az_data$age.20) + sum(az_data$age.21) + sum(az_data$age.22_24), sum(az_data$age.25_29), sum(az_data$age.30_34 + az_data$age.35_39), sum(az_data$age.40_44 +az_data$age.45_49), sum(az_data$age.50_54 + az_data$age.55_59 + az_data$age.60_61 + az_data$age.62_64), sum(az_data$age.retirees))

beta_age <- log(proportion_of_age/(1.0 - proportion_of_age))
tau_age <- (4*age_wilson_width/proportion_of_age)**-2

beta_retired <- beta_age[6]
tau_retired <- tau_age[6]

lm0 <- lm(y ~ x, data.frame(y = beta_age[1:5],
                            x = 0.5*c(18+24, 25+29, 30+39, 40+49, 50+64)))
a <- lm0$coefficients[1]
b <- lm0$coefficients[2]
a_tau <- (summary(lm0)$coefficients[,2][1])**-2
b_tau <- (summary(lm0)$coefficients[,2][2])**-2
```

Similarly, education coefficients are determined
thus:

``` r
education <- log_coefs(exit_poll_df[which(exit_poll_df$questions == "Education" & exit_poll_df$state=='Arizona'),])
ed_wilson_width <- wilson_width(0.01*education$options_perc, education$num_respondents, 2)

est <- wilson_center(0.01*education$options_perc, education$num_respondents, 2)

proportion_of_education <- ((est-4*ed_wilson_width)*sum(az_data$totalvotes))/c(sum(az_data$education.high_school), sum(az_data$education.some_college), sum(az_data$education.college_grad), sum(az_data$education.postgrad))

beta_education <- log(proportion_of_education/(1.0 - proportion_of_education))
tau_education <- (4*ed_wilson_width/proportion_of_education)**-2
```

Now, time for
simulation\!

``` r
az_data <- inner_join(election_2016[which(election_2016$party=='democrat' & election_2016$state == 'Arizona'),c('FIPS','candidate','party','candidatevotes','totalvotes')],census_data,by='FIPS')
colnames(az_data)[colnames(az_data)=='candidatevotes'] <- 'clintonvotes';
az_data$N <- nrow(az_data)
az_inits <- function() {
  N = nrow(az_data)
  return(list(w_male_white = rbeta(1,48,52),
              w_female_white = rbeta(1,27,74),
              w_male_black = rbeta(1,22,78),
              w_female_black = rbeta(1,21,79),
              w_male_latino = rbeta(1,30,70),
              w_female_latino = rbeta(1,27,73),
              w_others = rbeta(1,47,53),
              p_male_white = rbeta(N,2,2),
              p_female_white = rbeta(N,3,7),
              p_male_black = rbeta(N,2,8),
              p_female_black = rbeta(N,2,8),
              p_male_latino = rbeta(N,3,7),
              p_female_latino = rbeta(N,3,7),
              p_others = rbeta(N,5,5),
              # p_age_18_19 = rnorm(1, a + 18.5*b, 1.0/16),
              # p_age_20 = rnorm(1, a + 20*b, 1.0/16),
              # p_age_21 = rnorm(1, a + 21*b, 16**-1**-1),
              # p_age_22_24 = rnorm(1, a + 23*b, 16**-1),
              # p_age_25_29 = rnorm(1, a + 27*b, 16**-1),
              # p_age_30_34 = rnorm(1, a + 32*b, 16**-1),
              # p_age_35_39 = rnorm(1, a + 37*b, 16**-1),
              # p_age_40_44 = rnorm(1, a + 42*b, 16**-1),
              # p_age_45_49 = rnorm(1, a + 47*b, 16**-1),
              # p_age_50_54 = rnorm(1, a + 52*b, 16**-1),
              # p_age_55_59 = rnorm(1, a + 57*b, 16**-1),
              # p_age_60_61 = rnorm(1, a + 60.5*b, 16**-1),
              # p_age_62_64 = rnorm(1, a + 63*b, 16**-1),
              # beta_age_retiree = rnorm(1, beta_age_retiree, beta_age_tau**-0.5),
              number_of_states = 1
  ))
}
jagsModel = jags.model(file="az_post_stratification_election2016.model" , 
                       data=list(male.white = az_data$male.white,
                                 female.white = az_data$female.white,
                                 male.black = az_data$male.black,
                                 female.black = az_data$female.black,
                                 male.hispanic = az_data$male.hispanic,
                                 female.hispanic = az_data$female.hispanic,
                                 education.high_school = az_data$education.high_school,
                                 education.some_college = az_data$education.some_college,
                                 education.college_grad = az_data$education.college_grad,
                                 education.postgrad = az_data$education.postgrad,
                                 age.18_19 = az_data$age.18_19,
                                 age.20 = az_data$age.20,
                                 age.21 = az_data$age.21,
                                 age.22_24 = az_data$age.22_24,
                                 age.25_29 = az_data$age.25_29,
                                 age.30_34 = az_data$age.30_34,
                                 age.35_39 = az_data$age.35_39,
                                 age.40_44 = az_data$age.40_44,
                                 age.45_49 = az_data$age.45_49,
                                 age.50_54 = az_data$age.50_54,
                                 age.55_59 = az_data$age.55_59,
                                 age.60_61 = az_data$age.60_61,
                                 age.62_64 = az_data$age.62_64,
                                 age.retirees = az_data$age.retirees,
                                 others = az_data$others,
                                 totalvotes = az_data$totalvotes,
                                 u.totalvotes = az_data$totalvotes,
                                 tv = sum(az_data$totalvotes),
                                 utv = sum(az_data$totalvotes),
                                 N = nrow(az_data),
                                 clintonvotes = az_data$clintonvotes,
                                 tau_ctv = sum(az_data$clintonvotes),
                                 u.clintonvotes = az_data$clintonvotes,
                                 tau_education = tau_education,
                                 beta_education = beta_education,
                                 a = a,
                                 b = b,
                                 p_age_18_19 = rnorm(1, a + 18.5*b, 1.0/16),
                                 p_age_20 = rnorm(1, a + 20*b, 1.0/16),
                                 p_age_21 = rnorm(1, a + 21*b, 16**-1**-1),
                                 p_age_22_24 = rnorm(1, a + 23*b, 16**-1),
                                 p_age_25_29 = rnorm(1, a + 27*b, 16**-1),
                                 p_age_30_34 = rnorm(1, a + 32*b, 16**-1),
                                 p_age_35_39 = rnorm(1, a + 37*b, 16**-1),
                                 p_age_40_44 = rnorm(1, a + 42*b, 16**-1),
                                 p_age_45_49 = rnorm(1, a + 47*b, 16**-1),
                                 p_age_50_54 = rnorm(1, a + 52*b, 16**-1),
                                 p_age_55_59 = rnorm(1, a + 57*b, 16**-1),
                                 p_age_60_61 = rnorm(1, a + 60.5*b, 16**-1),
                                 p_age_62_64 = rnorm(1, a + 63*b, 16**-1),
                                 # beta_age_retiree = rnorm(1, beta_age_retiree, beta_age_tau**-0.5),
                                 beta.white_male = az_race[which(az_race$questions_id==7 & az_race$options=='White men'),]$clinton_coef,
                                 beta.white_male.var = az_race[which(az_race$questions_id==7 & az_race$options=='White men'),]$log_var,
                                 beta.white_female = az_race[which(az_race$questions_id==7 & az_race$options=='White women'),]$clinton_coef,
                                 beta.white_female.var = az_race[which(az_race$questions_id==7 & az_race$options=='White women'),]$log_var,
                                 beta.latino_male = az_race[which(az_race$questions_id==7 & az_race$options=='Latino men'),]$clinton_coef,
                                 beta.latino_male.var = az_race[which(az_race$questions_id==7 & az_race$options=='Latino men'),]$log_var,
                                 beta.latino_female = az_race[which(az_race$questions_id==7 & az_race$options=='Latino women'),]$clinton_coef,
                                 beta.latino_female.var = az_race[which(az_race$questions_id==7 & az_race$options=='Latino women'),]$log_var,
                                 beta.black_male = national_exit_polls[which(national_exit_polls$questions_id == 7 & national_exit_polls$options=='Black men'),]$clinton_coef,
                                 beta.black_male.var = national_exit_polls[which(national_exit_polls$questions_id == 7 & national_exit_polls$options=='Black men'),]$log_var,
                                 beta.black_female = national_exit_polls[which(national_exit_polls$questions_id == 7 & national_exit_polls$options=='Black women'),]$clinton_coef,
                                 beta.black_female.var = national_exit_polls[which(national_exit_polls$questions_id == 7 & national_exit_polls$options=='Black women'),]$log_var,
                                 beta.others = national_exit_polls[which(national_exit_polls$questions_id == 7 & national_exit_polls$options=='Others'),]$clinton_coef,
                                 beta.others.var = national_exit_polls[which(national_exit_polls$questions_id == 7 & national_exit_polls$options=='Others'),]$log_var
                       ),
                       inits=az_inits,
                       n.chains=3, n.adapt=12000)
```

    ## Warning in jags.model(file = "az_post_stratification_election2016.model", :
    ## Unused variable "clintonvotes" in data

    ## Warning in jags.model(file = "az_post_stratification_election2016.model", :
    ## Unused variable "tau_ctv" in data

    ## Warning in jags.model(file = "az_post_stratification_election2016.model", :
    ## Unused variable "u.clintonvotes" in data

    ## Warning in jags.model(file = "az_post_stratification_election2016.model", :
    ## Unused variable "beta.white_male" in data

    ## Warning in jags.model(file = "az_post_stratification_election2016.model", :
    ## Unused variable "beta.white_male.var" in data

    ## Warning in jags.model(file = "az_post_stratification_election2016.model", :
    ## Unused variable "beta.white_female" in data

    ## Warning in jags.model(file = "az_post_stratification_election2016.model", :
    ## Unused variable "beta.white_female.var" in data

    ## Warning in jags.model(file = "az_post_stratification_election2016.model", :
    ## Unused variable "beta.latino_male" in data

    ## Warning in jags.model(file = "az_post_stratification_election2016.model", :
    ## Unused variable "beta.latino_male.var" in data

    ## Warning in jags.model(file = "az_post_stratification_election2016.model", :
    ## Unused variable "beta.latino_female" in data

    ## Warning in jags.model(file = "az_post_stratification_election2016.model", :
    ## Unused variable "beta.latino_female.var" in data

    ## Warning in jags.model(file = "az_post_stratification_election2016.model", :
    ## Unused variable "beta.black_male" in data

    ## Warning in jags.model(file = "az_post_stratification_election2016.model", :
    ## Unused variable "beta.black_male.var" in data

    ## Warning in jags.model(file = "az_post_stratification_election2016.model", :
    ## Unused variable "beta.black_female" in data

    ## Warning in jags.model(file = "az_post_stratification_election2016.model", :
    ## Unused variable "beta.black_female.var" in data

    ## Warning in jags.model(file = "az_post_stratification_election2016.model", :
    ## Unused variable "beta.others" in data

    ## Warning in jags.model(file = "az_post_stratification_election2016.model", :
    ## Unused variable "beta.others.var" in data

    ## Compiling model graph
    ##    Resolving undeclared variables
    ##    Allocating nodes
    ## Graph information:
    ##    Observed stochastic nodes: 31
    ##    Unobserved stochastic nodes: 548
    ##    Total graph size: 1977

    ## Warning in jags.model(file = "az_post_stratification_election2016.model", :
    ## Unused initial value for "number_of_states" in chain 1

    ## Warning in jags.model(file = "az_post_stratification_election2016.model", :
    ## Unused initial value for "number_of_states" in chain 2

    ## Warning in jags.model(file = "az_post_stratification_election2016.model", :
    ## Unused initial value for "number_of_states" in chain 3

    ## Initializing model
    ## 
    ## 
      |                                                        
      |                                                  |   0%
      |                                                        
      |+                                                 |   2%
      |                                                        
      |++                                                |   4%
      |                                                        
      |+++                                               |   6%
      |                                                        
      |++++                                              |   8%
      |                                                        
      |+++++                                             |  10%
      |                                                        
      |++++++                                            |  12%
      |                                                        
      |+++++++                                           |  14%
      |                                                        
      |++++++++                                          |  16%
      |                                                        
      |+++++++++                                         |  18%
      |                                                        
      |++++++++++                                        |  20%
      |                                                        
      |+++++++++++                                       |  22%
      |                                                        
      |++++++++++++                                      |  24%
      |                                                        
      |+++++++++++++                                     |  26%
      |                                                        
      |++++++++++++++                                    |  28%
      |                                                        
      |+++++++++++++++                                   |  30%
      |                                                        
      |++++++++++++++++                                  |  32%
      |                                                        
      |+++++++++++++++++                                 |  34%
      |                                                        
      |++++++++++++++++++                                |  36%
      |                                                        
      |+++++++++++++++++++                               |  38%
      |                                                        
      |++++++++++++++++++++                              |  40%
      |                                                        
      |+++++++++++++++++++++                             |  42%
      |                                                        
      |++++++++++++++++++++++                            |  44%
      |                                                        
      |+++++++++++++++++++++++                           |  46%
      |                                                        
      |++++++++++++++++++++++++                          |  48%
      |                                                        
      |+++++++++++++++++++++++++                         |  50%
      |                                                        
      |++++++++++++++++++++++++++                        |  52%
      |                                                        
      |+++++++++++++++++++++++++++                       |  54%
      |                                                        
      |++++++++++++++++++++++++++++                      |  56%
      |                                                        
      |+++++++++++++++++++++++++++++                     |  58%
      |                                                        
      |++++++++++++++++++++++++++++++                    |  60%
      |                                                        
      |+++++++++++++++++++++++++++++++                   |  62%
      |                                                        
      |++++++++++++++++++++++++++++++++                  |  64%
      |                                                        
      |+++++++++++++++++++++++++++++++++                 |  66%
      |                                                        
      |++++++++++++++++++++++++++++++++++                |  68%
      |                                                        
      |+++++++++++++++++++++++++++++++++++               |  70%
      |                                                        
      |++++++++++++++++++++++++++++++++++++              |  72%
      |                                                        
      |+++++++++++++++++++++++++++++++++++++             |  74%
      |                                                        
      |++++++++++++++++++++++++++++++++++++++            |  76%
      |                                                        
      |+++++++++++++++++++++++++++++++++++++++           |  78%
      |                                                        
      |++++++++++++++++++++++++++++++++++++++++          |  80%
      |                                                        
      |+++++++++++++++++++++++++++++++++++++++++         |  82%
      |                                                        
      |++++++++++++++++++++++++++++++++++++++++++        |  84%
      |                                                        
      |+++++++++++++++++++++++++++++++++++++++++++       |  86%
      |                                                        
      |++++++++++++++++++++++++++++++++++++++++++++      |  88%
      |                                                        
      |+++++++++++++++++++++++++++++++++++++++++++++     |  90%
      |                                                        
      |++++++++++++++++++++++++++++++++++++++++++++++    |  92%
      |                                                        
      |+++++++++++++++++++++++++++++++++++++++++++++++   |  94%
      |                                                        
      |++++++++++++++++++++++++++++++++++++++++++++++++  |  96%
      |                                                        
      |+++++++++++++++++++++++++++++++++++++++++++++++++ |  98%
      |                                                        
      |++++++++++++++++++++++++++++++++++++++++++++++++++| 100%

``` r
update(jagsModel, n.iter=2000)
```

    ## 
      |                                                        
      |                                                  |   0%
      |                                                        
      |*                                                 |   2%
      |                                                        
      |**                                                |   4%
      |                                                        
      |***                                               |   6%
      |                                                        
      |****                                              |   8%
      |                                                        
      |*****                                             |  10%
      |                                                        
      |******                                            |  12%
      |                                                        
      |*******                                           |  14%
      |                                                        
      |********                                          |  16%
      |                                                        
      |*********                                         |  18%
      |                                                        
      |**********                                        |  20%
      |                                                        
      |***********                                       |  22%
      |                                                        
      |************                                      |  24%
      |                                                        
      |*************                                     |  26%
      |                                                        
      |**************                                    |  28%
      |                                                        
      |***************                                   |  30%
      |                                                        
      |****************                                  |  32%
      |                                                        
      |*****************                                 |  34%
      |                                                        
      |******************                                |  36%
      |                                                        
      |*******************                               |  38%
      |                                                        
      |********************                              |  40%
      |                                                        
      |*********************                             |  42%
      |                                                        
      |**********************                            |  44%
      |                                                        
      |***********************                           |  46%
      |                                                        
      |************************                          |  48%
      |                                                        
      |*************************                         |  50%
      |                                                        
      |**************************                        |  52%
      |                                                        
      |***************************                       |  54%
      |                                                        
      |****************************                      |  56%
      |                                                        
      |*****************************                     |  58%
      |                                                        
      |******************************                    |  60%
      |                                                        
      |*******************************                   |  62%
      |                                                        
      |********************************                  |  64%
      |                                                        
      |*********************************                 |  66%
      |                                                        
      |**********************************                |  68%
      |                                                        
      |***********************************               |  70%
      |                                                        
      |************************************              |  72%
      |                                                        
      |*************************************             |  74%
      |                                                        
      |**************************************            |  76%
      |                                                        
      |***************************************           |  78%
      |                                                        
      |****************************************          |  80%
      |                                                        
      |*****************************************         |  82%
      |                                                        
      |******************************************        |  84%
      |                                                        
      |*******************************************       |  86%
      |                                                        
      |********************************************      |  88%
      |                                                        
      |*********************************************     |  90%
      |                                                        
      |**********************************************    |  92%
      |                                                        
      |***********************************************   |  94%
      |                                                        
      |************************************************  |  96%
      |                                                        
      |************************************************* |  98%
      |                                                        
      |**************************************************| 100%
