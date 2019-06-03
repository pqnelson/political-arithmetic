  - [How did Trump Win the White
    House?](#how-did-trump-win-the-white-house)
      - [Exit Poll Data](#exit-poll-data)
          - [GOAL: Infer Logistic
            Regression](#goal-infer-logistic-regression)
              - [Religion](#religion)
              - [Married](#married)
              - [Gender and Ethnicity](#gender-and-ethnicity)
              - [Education](#education)
              - [Income](#income)
              - [Age](#age)
              - [Different States](#different-states)
      - [Census Data](#census-data)
  - [Maximum turnout](#maximum-turnout)
      - [Post Stratified Terms](#post-stratified-terms)
  - [Voter turnout](#voter-turnout)

# How did Trump Win the White House?

Although we just found, by accident, one way to explain how Trump won
the White House (namely, Obama voters from 2012 who switched to vote for
Trump in 2016 explain how a sufficient number of swing states…swung…),
are there other ways to explain *how Trump won the White House?*

We will use a post-stratified multilevel model training a logistic
regression to match against exit poll data, then using those
coefficients in a linear regression to try to match the voting results.
This is a partially pooled model, so although the coefficients vary
state-to-state, they are still “in the same ballpark”. For reviews of
this approach, see:

  - the MRP Primer
    <http://www.princeton.edu/~jkastell/MRP_primer/mrp_primer.pdf>
  - David Park, Andrew Gelman, Joseph Bafumi, [Bayesian Multilevel
    Estimation with Poststratification: State-Level Estimates from
    National
    Polls](http://www.stat.columbia.edu/~gelman/research/published/parkgelmanbafumi.pdf)

## Exit Poll Data

We load CNN’s exit poll data. Fortunately, it is stored as a CSV
file.

``` r
exit_poll_path <- "../data/elections/presidential/exit_polls/cnn_04022017.csv"
exit_poll_df <- read.csv(file=exit_poll_path, header=TRUE, sep=",")
```

We combine race and gender into a single variable with 7 categories (the
3 race categories \[white, black, hispanic\] and 2 genders \[men,
women\] combinations, and 1 `other` category). Asians constitute about
4% of the respondents, and “others” constitutes 3%. Whites constitute
71% of respondents, non-whites constitute 29%.

The age brackets are: 18-29, 30-44, 45-64, and 65+. So, 5 categories.

The education brackets are: “High school or less”, “Some college”,
“College graduate”, “Postgraduate”.

The income brackets are `30k-50k`, `50k-100k`, `100k-150k`, `150k-200k`,
`200k-250k`, and `250k+`. We can use the `B19037` table from the ACS5
census data to get the estimates of number of householder (i.e.,
occupant who either owns or rents the residency as determined by whose
name is on the deed, renal agreement, etc.) by age, income, and race.
The ACS5 data has finer categories of income brackets. Unfortunately,
the age-brackets for this are younger than 25, 25-64, 45-64, 65+, so we
need to be careful not to toss out all the census data too quickly.

### GOAL: Infer Logistic Regression

We want to construct a logistic
    regression

    prob_clinton(ethnicity,gender,education,income) = invlogit(b0 + b1*ethnicity + b2*gender + b3*education + b4*income)

…and similarly for Trump, but we need to compute the coefficients `b0`,
…, `b4`. The constant coefficient is just the
`logit(clinton_votes/(clinton_votes + trump_votes)) =
logit(65853514.0/(62984828.0 + 65853514.0)) = 0.04453892`.

``` r
beta0 <- log(65853514.0/62984828.0)
```

Although left unstated, exit polls are usually plus-or-minus 4%, and
assuming it is with 95% confidence, (so approximating the numerator and
denominator as normally distributed random variables with expected
values given, and standard deviation being 2%) the [propagation of
uncertainty](https://en.wikipedia.org/wiki/Propagation_of_uncertainty#Example_formulae)
implies the variance of `beta0` is

``` r
var_beta <- function(A,B) {
  return( ((3.0/A)**2)+((3.0/B)**2) );
}
var_beta0 <- var_beta(65853514.0, 62984828.0) # nearly machine epsilon
```

The variance for other betas will be closer to half a percent. (It is
bounded from above for `A=B=50`, `var_beta(50,50)~0.06**2`.) Even if one
were to insist on the margin of error being twice that of a normal poll
(i.e., it should be 6% instead of our 4%), the variance’s upper bound is
moved to approximately `0.08485**2`. ([“This makes the margins for error
somewhere between 50-90% higher than they would be for comparable
telephone
surveys.”](https://fivethirtyeight.com/features/ten-reasons-why-you-should-ignore-exit/))

We can estimate the coefficients for any group by taking
`beta_clinton(group) = log(percent_clinton(group)/percent_trump(group))
- beta0`. This is the basic strategy of one-factor models, after
all.

``` r
national_exit_polls <- exit_poll_df[which(exit_poll_df$state == 'nation'),]

race_and_gender <- national_exit_polls[which(national_exit_polls$questions=="Race and gender"),]
age <- national_exit_polls[which(national_exit_polls$questions_id==3),]
income <- national_exit_polls[which(national_exit_polls$questions_id==13),]
married <- national_exit_polls[which(national_exit_polls$questions_id==20),]
religion <- national_exit_polls[which(national_exit_polls$questions_id==23),]
education <- national_exit_polls[which(national_exit_polls$questions == "Education"),]
```

We can estimate the error using the Wilson confidence interval.

``` r
wilson_center <- function(p, n, z) {
  (p + 0.5*z*z/n)/(1.0 + z*z*1.0/n);
}
wilson_width <- function(p, n, z) {
  (z/(1.0 + z*z*1.0/n))*sqrt(p*(1.0-p)/n  + (z*0.5/n)**2);
}
log_coefs <- function(r) {
  c <- wilson_center(r$Clinton_perc*0.01, r$options_perc*0.01*r$num_respondents,1)
  t <- wilson_center(r$Trump_perc*0.01, r$options_perc*0.01*r$num_respondents,1)
  r$clinton_numerator <- c/t
  r$trump_numerator <- t/c
  
  r$clinton_coef <- log(r$clinton_numerator) - beta0
  r$trump_coef <- log(r$trump_numerator) - beta0
  r$log_var <- (wilson_width(r$Clinton_perc*0.01, r$options_perc*0.01*r$num_respondents,1)/(0.01*r$Clinton_perc))**2 + (wilson_width(r$Trump_perc*0.01, r$options_perc*0.01*r$num_respondents,1)/(0.01*r$Trump_perc))**2;
  r$log_sd <- sqrt(r$log_var);
  return(r);
}
```

#### Religion

#### Married

``` r
married <- log_coefs(married)

kable(married[,c("options","clinton_coef","trump_coef")])
```

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:left;">

options

</th>

<th style="text-align:right;">

clinton\_coef

</th>

<th style="text-align:right;">

trump\_coef

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

82

</td>

<td style="text-align:left;">

Married

</td>

<td style="text-align:right;">

\-0.2115809

</td>

<td style="text-align:right;">

0.1225031

</td>

</tr>

<tr>

<td style="text-align:left;">

83

</td>

<td style="text-align:left;">

Unmarried

</td>

<td style="text-align:right;">

0.3518324

</td>

<td style="text-align:right;">

\-0.4409103

</td>

</tr>

</tbody>

</table>

#### Gender and Ethnicity

Then we have the numerators for the coefficients

``` r
race_and_gender <- log_coefs(race_and_gender)

beta_white_male = race_and_gender[which(race_and_gender$options=='White men'),c('clinton_coef')]
beta_white_female = race_and_gender[which(race_and_gender$options=='White women'),c('clinton_coef')]
beta_black_male = race_and_gender[which(race_and_gender$options=='Black men'),c('clinton_coef')]
beta_black_female = race_and_gender[which(race_and_gender$options=='Black women'),c('clinton_coef')]
beta_latino_male = race_and_gender[which(race_and_gender$options=='Latino men'),c('clinton_coef')]
beta_latino_female = race_and_gender[which(race_and_gender$options=='Latino women'),c('clinton_coef')]
beta_others_gender = race_and_gender[which(race_and_gender$options=='Others'),c('clinton_coef')]

kable(race_and_gender[,c("options","clinton_coef","trump_coef")])
```

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:left;">

options

</th>

<th style="text-align:right;">

clinton\_coef

</th>

<th style="text-align:right;">

trump\_coef

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

22

</td>

<td style="text-align:left;">

White men

</td>

<td style="text-align:right;">

\-0.7375895

</td>

<td style="text-align:right;">

0.6485117

</td>

</tr>

<tr>

<td style="text-align:left;">

23

</td>

<td style="text-align:left;">

White women

</td>

<td style="text-align:right;">

\-0.2345604

</td>

<td style="text-align:right;">

0.1454825

</td>

</tr>

<tr>

<td style="text-align:left;">

24

</td>

<td style="text-align:left;">

Black men

</td>

<td style="text-align:right;">

1.7946000

</td>

<td style="text-align:right;">

\-1.8836779

</td>

</tr>

<tr>

<td style="text-align:left;">

25

</td>

<td style="text-align:left;">

Black women

</td>

<td style="text-align:right;">

3.1055258

</td>

<td style="text-align:right;">

\-3.1946036

</td>

</tr>

<tr>

<td style="text-align:left;">

26

</td>

<td style="text-align:left;">

Latino men

</td>

<td style="text-align:right;">

0.6322344

</td>

<td style="text-align:right;">

\-0.7213122

</td>

</tr>

<tr>

<td style="text-align:left;">

27

</td>

<td style="text-align:left;">

Latino women

</td>

<td style="text-align:right;">

0.9698270

</td>

<td style="text-align:right;">

\-1.0589049

</td>

</tr>

<tr>

<td style="text-align:left;">

28

</td>

<td style="text-align:left;">

Others

</td>

<td style="text-align:right;">

0.6318098

</td>

<td style="text-align:right;">

\-0.7208877

</td>

</tr>

</tbody>

</table>

#### Education

For education we have:

``` r
education <- log_coefs(education)

kable(education[,c("options","clinton_coef","trump_coef")])
```

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:left;">

options

</th>

<th style="text-align:right;">

clinton\_coef

</th>

<th style="text-align:right;">

trump\_coef

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

42

</td>

<td style="text-align:left;">

High school or less

</td>

<td style="text-align:right;">

\-0.1476991

</td>

<td style="text-align:right;">

0.0586212

</td>

</tr>

<tr>

<td style="text-align:left;">

43

</td>

<td style="text-align:left;">

Some college

</td>

<td style="text-align:right;">

\-0.2151412

</td>

<td style="text-align:right;">

0.1260634

</td>

</tr>

<tr>

<td style="text-align:left;">

44

</td>

<td style="text-align:left;">

College graduate

</td>

<td style="text-align:right;">

0.0630770

</td>

<td style="text-align:right;">

\-0.1521548

</td>

</tr>

<tr>

<td style="text-align:left;">

45

</td>

<td style="text-align:left;">

Postgraduate

</td>

<td style="text-align:right;">

0.4048755

</td>

<td style="text-align:right;">

\-0.4939534

</td>

</tr>

</tbody>

</table>

Observe that education is linear progressing. We can assign the
following weights to education level: -1 to “High school or less”, 0 to
“Some college”, 1 to “College graduate”, and 2 to “Postgraduate”. Then
we find a linear regression the coefficient is better approximated by
`x1` in the following
table:

``` r
df <- data.frame(y=education$clinton_coef, z=education$trump_coef,x=1:4,x0=0:3,x1=-1:2)

clm <- lm(y~x1,df)
tlm <- lm(z~x1,df)

beta_education <- clm$coefficients[2] # == -tlm$coefficients[2]

kable(data.frame(Clinton_coefficients=clm$coefficients, Trump_coefficient=tlm$coefficients))
```

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

Clinton\_coefficients

</th>

<th style="text-align:right;">

Trump\_coefficient

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

(Intercept)

</td>

<td style="text-align:right;">

\-0.0705190

</td>

<td style="text-align:right;">

\-0.0185588

</td>

</tr>

<tr>

<td style="text-align:left;">

x1

</td>

<td style="text-align:right;">

0.1935942

</td>

<td style="text-align:right;">

\-0.1935942

</td>

</tr>

</tbody>

</table>

#### Income

``` r
income <- log_coefs(income)

kable(income[,c("options","clinton_coef","trump_coef")])
```

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:left;">

options

</th>

<th style="text-align:right;">

clinton\_coef

</th>

<th style="text-align:right;">

trump\_coef

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

57

</td>

<td style="text-align:left;">

Under $30,000

</td>

<td style="text-align:right;">

0.2368001

</td>

<td style="text-align:right;">

\-0.3258780

</td>

</tr>

<tr>

<td style="text-align:left;">

58

</td>

<td style="text-align:left;">

$30K-$49,999

</td>

<td style="text-align:right;">

0.1930775

</td>

<td style="text-align:right;">

\-0.2821553

</td>

</tr>

<tr>

<td style="text-align:left;">

59

</td>

<td style="text-align:left;">

$50K-$99,999

</td>

<td style="text-align:right;">

\-0.1077088

</td>

<td style="text-align:right;">

0.0186310

</td>

</tr>

<tr>

<td style="text-align:left;">

60

</td>

<td style="text-align:left;">

$100K-$199,999

</td>

<td style="text-align:right;">

\-0.0655886

</td>

<td style="text-align:right;">

\-0.0234893

</td>

</tr>

<tr>

<td style="text-align:left;">

61

</td>

<td style="text-align:left;">

$200K-$249,999

</td>

<td style="text-align:right;">

\-0.0029104

</td>

<td style="text-align:right;">

\-0.0861675

</td>

</tr>

<tr>

<td style="text-align:left;">

62

</td>

<td style="text-align:left;">

$250,000 or
more

</td>

<td style="text-align:right;">

\-0.0445389

</td>

<td style="text-align:right;">

\-0.0445389

</td>

</tr>

</tbody>

</table>

``` r
df <- data.frame(y = income$clinton_coef, z=income$trump_coef, x=1:6, x2=c(15,0.5*(30+45),0.5*(50+100),0.5*(100+200),0.5*(200+250),300),x3=c(15,0.5*(30+45),0.5*(50+100),0.5*(100+200),0.5*(200+250),300)**2)

clm <- lm(y~x**2,df)
```

#### Age

For age we have a partition of ages into 5 or 6 classes. We take the
midpoint of these intervals to approximate the value, and take a linear
regression of the logistic coefficients.

``` r
age <- log_coefs(age)

kable(age[,c("options","clinton_coef","trump_coef")])
```

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:left;">

options

</th>

<th style="text-align:right;">

clinton\_coef

</th>

<th style="text-align:right;">

trump\_coef

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

7

</td>

<td style="text-align:left;">

18-24

</td>

<td style="text-align:right;">

0.4542171

</td>

<td style="text-align:right;">

\-0.5432949

</td>

</tr>

<tr>

<td style="text-align:left;">

8

</td>

<td style="text-align:left;">

25-29

</td>

<td style="text-align:right;">

0.3066827

</td>

<td style="text-align:right;">

\-0.3957605

</td>

</tr>

<tr>

<td style="text-align:left;">

9

</td>

<td style="text-align:left;">

30-39

</td>

<td style="text-align:right;">

0.2236528

</td>

<td style="text-align:right;">

\-0.3127307

</td>

</tr>

<tr>

<td style="text-align:left;">

10

</td>

<td style="text-align:left;">

40-49

</td>

<td style="text-align:right;">

\-0.1077036

</td>

<td style="text-align:right;">

0.0186257

</td>

</tr>

<tr>

<td style="text-align:left;">

11

</td>

<td style="text-align:left;">

50-64

</td>

<td style="text-align:right;">

\-0.2115693

</td>

<td style="text-align:right;">

0.1224914

</td>

</tr>

<tr>

<td style="text-align:left;">

12

</td>

<td style="text-align:left;">

65 and older

</td>

<td style="text-align:right;">

\-0.1890821

</td>

<td style="text-align:right;">

0.1000043

</td>

</tr>

</tbody>

</table>

We try to approximate this using a linear regression, taking `age-46` as
the input signal appears to be best (note the intercept is approximately
`-beta0`):

``` r
df <- data.frame(y=age$clinton_coef, z=age$trump_coef,x=c(21,27,34.5,44.5,57,75),x0=-3:2,x1=c(21,27,34.5,44.5,57,75)-46)
df2 <- data.frame(y=age$clinton_coef[1:5], z=age$trump_coef[1:5], x=c(21,27,34.5,44.5,57),x0=-3:1,x1=c(21,27,34.5,44.5,57)-46)
clm <- lm(y~x1,df2)
tlm <- lm(z~x1,df2)

beta_age <- clm$coefficients[2] # == -tlm$coefficients[2] to machine epsilon
beta_retiree <- age[which(age$options=="65 and older"),c('clinton_coef')]


kable(data.frame(Clinton_coefficients=clm$coefficients, Trump_coefficient=tlm$coefficients))
```

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

Clinton\_coefficients

</th>

<th style="text-align:right;">

Trump\_coefficient

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

(Intercept)

</td>

<td style="text-align:right;">

\-0.0445754

</td>

<td style="text-align:right;">

\-0.0445024

</td>

</tr>

<tr>

<td style="text-align:left;">

x1

</td>

<td style="text-align:right;">

\-0.0193078

</td>

<td style="text-align:right;">

0.0193078

</td>

</tr>

</tbody>

</table>

#### Different States

We can consider coefficients for different states, lets see Florida:

``` r
fl_exit_polls <- exit_poll_df[which(exit_poll_df$state == 'Florida'),]

fl_race_and_gender <- fl_exit_polls[which(fl_exit_polls$questions=="Race and gender"),]

fl_race_and_gender <- log_coefs(fl_race_and_gender)

kable(fl_race_and_gender[,c("options","clinton_coef","trump_coef")])
```

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:left;">

options

</th>

<th style="text-align:right;">

clinton\_coef

</th>

<th style="text-align:right;">

trump\_coef

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

909

</td>

<td style="text-align:left;">

White men

</td>

<td style="text-align:right;">

\-0.9161313

</td>

<td style="text-align:right;">

0.8270534

</td>

</tr>

<tr>

<td style="text-align:left;">

910

</td>

<td style="text-align:left;">

White women

</td>

<td style="text-align:right;">

\-0.5549437

</td>

<td style="text-align:right;">

0.4658659

</td>

</tr>

<tr>

<td style="text-align:left;">

911

</td>

<td style="text-align:left;">

Black men

</td>

<td style="text-align:right;">

2.0292612

</td>

<td style="text-align:right;">

\-2.1183390

</td>

</tr>

<tr>

<td style="text-align:left;">

912

</td>

<td style="text-align:left;">

Black women

</td>

<td style="text-align:right;">

2.6056780

</td>

<td style="text-align:right;">

\-2.6947559

</td>

</tr>

<tr>

<td style="text-align:left;">

913

</td>

<td style="text-align:left;">

Latino men

</td>

<td style="text-align:right;">

0.4645553

</td>

<td style="text-align:right;">

\-0.5536331

</td>

</tr>

<tr>

<td style="text-align:left;">

914

</td>

<td style="text-align:left;">

Latino women

</td>

<td style="text-align:right;">

0.5705465

</td>

<td style="text-align:right;">

\-0.6596243

</td>

</tr>

<tr>

<td style="text-align:left;">

915

</td>

<td style="text-align:left;">

Others

</td>

<td style="text-align:right;">

0.7934278

</td>

<td style="text-align:right;">

\-0.8825056

</td>

</tr>

</tbody>

</table>

## Census Data

We get basic data from the census
[ACS5](https://api.census.gov/data/2017/acs/acs5/variables.html)
specifically the breakdown by age and demographics. I borrow their
terminology in naming the variables.

``` r
api_key <- read.table("../data/keys/census.txt", header = FALSE)[[1]]
census_api_key(api_key)
```

    ## To install your API key for use in future sessions, run this function with `install = TRUE`.

There are a variety of different “types” of variables we could use from
the US Census, they are called
[subjects](https://www.census.gov/programs-surveys/acs/guidance/subjects.html).

Note, the estimates are given with 90% margin of error, meaning we are
90% confident the “true value” lies within the “margin of error” of the
estimate. We can convert the margin of error into a standard deviation
by `sd = moe/qnorm(1 - 0.5*(1 - confidence_level))` where
`confidence_level=0.9` (which can be changed by the user). Ostensibly,
each variable should be represented by a normal random variable with
expected value given by the estimate and the sigma given by the `sd`
formula. (This is all “roughly true”, there are some nuances surrounding
this.)

We also need to request data which will correspond with possible options
in the exit polls.

``` r
spam <- get_acs(geography = "county", 
               variables = c(male.18_19 = "B01001_007",
                            male.20 = "B01001_008"),
        state="FL",
              output = "wide",
        year=2016)
```

    ## Getting data from the 2012-2016 5-year ACS

    ## Using FIPS code '12' for state 'FL'

``` r
census_fetch <- function(state_abbrev) {
  gender <- get_acs(geography = "county", 
               variables = c(male.18_19 = "B01001_007",
                            male.20 = "B01001_008",
                            male.21 = "B01001_009",
                            male.22_24 = "B01001_010",
                            male.25_29 = "B01001_011",
                            male.30_34 = "B01001_012",
                            male.35_39 = "B01001_013",
                            male.40_44 = "B01001_014",
                            male.45_49 = "B01001_015",
                            male.50_54 = "B01001_016",
                            male.55_59 = "B01001_017",
                            male.60_61 = "B01001_018",
                            male.62_64 = "B01001_019",
                            male.65_66 = "B01001_020",
                            male.67_69 = "B01001_021",
                            male.70_74 = "B01001_022",
                            male.75_79 = "B01001_023",
                            male.80_84 = "B01001_024",
                            male.85_plus = "B01001_025",
                            female.18_19 = "B01001_031",
                            female.20 = "B01001_032",
                            female.21 = "B01001_033",
                            female.22_24 = "B01001_034",
                            female.25_29 = "B01001_035",
                            female.30_34 = "B01001_036",
                            female.35_39 = "B01001_037",
                            female.40_44 = "B01001_038",
                            female.45_49 = "B01001_039",
                            female.50_54 = "B01001_040",
                            female.55_59 = "B01001_041",
                            female.60_61 = "B01001_042",
                            female.62_64 = "B01001_043",
                            female.65_66 = "B01001_044",
                            female.67_69 = "B01001_045",
                            female.70_74 = "B01001_046",
                            female.75_79 = "B01001_047",
                            female.80_84 = "B01001_048",
                            female.85_plus = "B01001_049"
                            ), 
              year = 2016,
              output = "wide",
              state = state_abbrev)
  ethnicity <- get_acs(geography = "county", 
               variables = c(male.white.18_19 = "B01001A_007",
                            male.white.20_24 = "B01001A_008",
                            male.white.25_29 = "B01001A_009",
                            male.white.30_34 = "B01001A_010",
                            male.white.35_44 = "B01001A_011",
                            male.white.45_54 = "B01001A_012",
                            male.white.55_64 = "B01001A_013",
                            male.white.65_74 = "B01001A_014",
                            male.white.75_84 = "B01001A_015",
                            male.white.85_plus = "B01001A_016",
                            female.white.18_19 = "B01001A_022",
                            female.white.20_24 = "B01001A_023",
                            female.white.25_29 = "B01001A_024",
                            female.white.30_34 = "B01001A_025",
                            female.white.35_44 = "B01001A_026",
                            female.white.45_54 = "B01001A_027",
                            female.white.55_64 = "B01001A_028",
                            female.white.65_74 = "B01001A_029",
                            female.white.75_84 = "B01001A_030",
                            female.white.85_plus = "B01001A_031",
                            male.black.18_19 = "B01001B_007",
                            male.black.20_24 = "B01001B_008",
                            male.black.25_29 = "B01001B_009",
                            male.black.30_34 = "B01001B_010",
                            male.black.35_44 = "B01001B_011",
                            male.black.45_54 = "B01001B_012",
                            male.black.55_64 = "B01001B_013",
                            male.black.65_74 = "B01001B_014",
                            male.black.75_84 = "B01001B_015",
                            male.black.85_plus = "B01001B_016",
                            female.black.18_19 = "B01001B_022",
                            female.black.20_24 = "B01001B_023",
                            female.black.25_29 = "B01001B_024",
                            female.black.30_34 = "B01001B_025",
                            female.black.35_44 = "B01001B_026",
                            female.black.45_54 = "B01001B_027",
                            female.black.55_64 = "B01001B_028",
                            female.black.65_74 = "B01001B_029",
                            female.black.75_84 = "B01001B_030",
                            female.black.85_plus = "B01001B_031",
                            male.american_indian.18_19 = "B01001C_007",
                            male.american_indian.20_24 = "B01001C_008",
                            male.american_indian.25_29 = "B01001C_009",
                            male.american_indian.30_34 = "B01001C_010",
                            male.american_indian.35_44 = "B01001C_011",
                            male.american_indian.45_54 = "B01001C_012",
                            male.american_indian.55_64 = "B01001C_013",
                            male.american_indian.65_74 = "B01001C_014",
                            male.american_indian.75_84 = "B01001C_015",
                            male.american_indian.85_plus = "B01001C_016",
                            female.american_indian.18_19 = "B01001C_022",
                            female.american_indian.20_24 = "B01001C_023",
                            female.american_indian.25_29 = "B01001C_024",
                            female.american_indian.30_34 = "B01001C_025",
                            female.american_indian.35_44 = "B01001C_026",
                            female.american_indian.45_54 = "B01001C_027",
                            female.american_indian.55_64 = "B01001C_028",
                            female.american_indian.65_74 = "B01001C_029",
                            female.american_indian.75_84 = "B01001C_030",
                            female.american_indian.85_plus = "B01001C_031",
                            male.asian.18_19 = "B01001D_007",
                            male.asian.20_24 = "B01001D_008",
                            male.asian.25_29 = "B01001D_009",
                            male.asian.30_34 = "B01001D_010",
                            male.asian.35_44 = "B01001D_011",
                            male.asian.45_54 = "B01001D_012",
                            male.asian.55_64 = "B01001D_013",
                            male.asian.65_74 = "B01001D_014",
                            male.asian.75_84 = "B01001D_015",
                            male.asian.85_plus = "B01001D_016",
                            female.asian.18_19 = "B01001D_022",
                            female.asian.20_24 = "B01001D_023",
                            female.asian.25_29 = "B01001D_024",
                            female.asian.30_34 = "B01001D_025",
                            female.asian.35_44 = "B01001D_026",
                            female.asian.45_54 = "B01001D_027",
                            female.asian.55_64 = "B01001D_028",
                            female.asian.65_74 = "B01001D_029",
                            female.asian.75_84 = "B01001D_030",
                            female.asian.85_plus = "B01001D_031",
                            male.pacific_islander.18_19 = "B01001E_007",
                            male.pacific_islander.20_24 = "B01001E_008",
                            male.pacific_islander.25_29 = "B01001E_009",
                            male.pacific_islander.30_34 = "B01001E_010",
                            male.pacific_islander.35_44 = "B01001E_011",
                            male.pacific_islander.45_54 = "B01001E_012",
                            male.pacific_islander.55_64 = "B01001E_013",
                            male.pacific_islander.65_74 = "B01001E_014",
                            male.pacific_islander.75_84 = "B01001E_015",
                            male.pacific_islander.85_plus = "B01001E_016",
                            female.pacific_islander.18_19 = "B01001E_022",
                            female.pacific_islander.20_24 = "B01001E_023",
                            female.pacific_islander.25_29 = "B01001E_024",
                            female.pacific_islander.30_34 = "B01001E_025",
                            female.pacific_islander.35_44 = "B01001E_026",
                            female.pacific_islander.45_54 = "B01001E_027",
                            female.pacific_islander.55_64 = "B01001E_028",
                            female.pacific_islander.65_74 = "B01001E_029",
                            female.pacific_islander.75_84 = "B01001E_030",
                            female.pacific_islander.85_plus = "B01001E_031",
                            male.white_only.18_19 = "B01001H_007",
                            male.white_only.20_24 = "B01001H_008",
                            male.white_only.25_29 = "B01001H_009",
                            male.white_only.30_34 = "B01001H_010",
                            male.white_only.35_44 = "B01001H_011",
                            male.white_only.45_54 = "B01001H_012",
                            male.white_only.55_64 = "B01001H_013",
                            male.white_only.65_74 = "B01001H_014",
                            male.white_only.75_84 = "B01001H_015",
                            male.white_only.85_plus = "B01001H_016",
                            female.white_only.18_19 = "B01001H_022",
                            female.white_only.20_24 = "B01001H_023",
                            female.white_only.25_29 = "B01001H_024",
                            female.white_only.30_34 = "B01001H_025",
                            female.white_only.35_44 = "B01001H_026",
                            female.white_only.45_54 = "B01001H_027",
                            female.white_only.55_64 = "B01001H_028",
                            female.white_only.65_74 = "B01001H_029",
                            female.white_only.75_84 = "B01001H_030",
                            female.white_only.85_plus = "B01001H_031",
                            male.hispanic.18_19 = "B01001I_007",
                            male.hispanic.20_24 = "B01001I_008",
                            male.hispanic.25_29 = "B01001I_009",
                            male.hispanic.30_34 = "B01001I_010",
                            male.hispanic.35_44 = "B01001I_011",
                            male.hispanic.45_54 = "B01001I_012",
                            male.hispanic.55_64 = "B01001I_013",
                            male.hispanic.65_74 = "B01001I_014",
                            male.hispanic.75_84 = "B01001I_015",
                            male.hispanic.85_plus = "B01001I_016",
                            female.hispanic.18_19 = "B01001I_022",
                            female.hispanic.20_24 = "B01001I_023",
                            female.hispanic.25_29 = "B01001I_024",
                            female.hispanic.30_34 = "B01001I_025",
                            female.hispanic.35_44 = "B01001I_026",
                            female.hispanic.45_54 = "B01001I_027",
                            female.hispanic.55_64 = "B01001I_028",
                            female.hispanic.65_74 = "B01001I_029",
                            female.hispanic.75_84 = "B01001I_030",
                            female.hispanic.85_plus = "B01001I_031"
                            ), 
              year = 2016,
              output = "wide",
              state = state_abbrev)
  econ <- get_acs(geography = "county", 
              variables = c(income.median = "B19013_001",
                            below_poverty_line = "B06012_002E",
                            between_100_to_149_percent_poverty_line = "B06012_003",
                            above_150_percent_poverty_line = "B06012_004",
                            # income.age-bracket.income-bracket
                            income.under_25.under_10k = "B19037_003",
                            income.under_25.10k_15k = "B19037_004",
                            income.under_25.15k_20k = "B19037_005",
                            income.under_25.20k_25k = "B19037_006",
                            income.under_25.25k_30k = "B19037_007",
                            income.under_25.30k_35k = "B19037_008",
                            income.under_25.35k_40k = "B19037_009",
                            income.under_25.40k_45k = "B19037_010",
                            income.under_25.45k_50k = "B19037_011",
                            income.under_25.50k_60k = "B19037_012",
                            income.under_25.60k_75k = "B19037_013",
                            income.under_25.75k_100k = "B19037_014",
                            income.under_25.100k_125k = "B19037_015",
                            income.under_25.125k_150k = "B19037_016",
                            income.under_25.150k_200k = "B19037_017",
                            income.under_25.200k_plus = "B19037_018",
                            income.25_44.under_10k = "B19037_020",
                            income.25_44.10k_15k = "B19037_021",
                            income.25_44.15k_20k = "B19037_022",
                            income.25_44.20k_25k = "B19037_023",
                            income.25_44.25k_30k = "B19037_024",
                            income.25_44.30k_35k = "B19037_025",
                            income.25_44.35k_40k = "B19037_026",
                            income.25_44.40k_45k = "B19037_027",
                            income.25_44.45k_50k = "B19037_028",
                            income.25_44.50k_60k = "B19037_029",
                            income.25_44.60k_75k = "B19037_030",
                            income.25_44.75k_100k = "B19037_031",
                            income.25_44.100k_125k = "B19037_032",
                            income.25_44.125k_150k = "B19037_033",
                            income.25_44.150k_200k = "B19037_034",
                            income.25_44.200k_plus = "B19037_035",
                            income.45_64.under_10k = "B19037_037",
                            income.45_64.10k_15k = "B19037_038",
                            income.45_64.15k_20k = "B19037_039",
                            income.45_64.20k_25k = "B19037_040",
                            income.45_64.25k_30k = "B19037_041",
                            income.45_64.30k_35k = "B19037_042",
                            income.45_64.35k_40k = "B19037_043",
                            income.45_64.40k_45k = "B19037_044",
                            income.45_64.45k_50k = "B19037_045",
                            income.45_64.50k_60k = "B19037_046",
                            income.45_64.60k_75k = "B19037_047",
                            income.45_64.75k_100k = "B19037_048",
                            income.45_64.100k_125k = "B19037_049",
                            income.45_64.125k_150k = "B19037_050",
                            income.45_64.150k_200k = "B19037_051",
                            income.45_64.200k_plus = "B19037_052",
                            income.65_plus.under_10k = "B19037_054",
                            income.65_plus.10k_15k = "B19037_055",
                            income.65_plus.15k_20k = "B19037_056",
                            income.65_plus.20k_25k = "B19037_057",
                            income.65_plus.25k_30k = "B19037_058",
                            income.65_plus.30k_35k = "B19037_059",
                            income.65_plus.35k_40k = "B19037_060",
                            income.65_plus.40k_45k = "B19037_061",
                            income.65_plus.45k_50k = "B19037_062",
                            income.65_plus.50k_60k = "B19037_063",
                            income.65_plus.60k_75k = "B19037_064",
                            income.65_plus.75k_100k = "B19037_065",
                            income.65_plus.100k_125k = "B19037_066",
                            income.65_plus.125k_150k = "B19037_067",
                            income.65_plus.150k_200k = "B19037_068",
                            income.65_plus.200k_plus = "B19037_069"
                            ), 
              year = 2016,
              output = "wide",
              state = state_abbrev)
  education <- get_acs(geography = "county", 
              variables = c(male.18_24.less_than_9th_grade = "B15001_004",
                            male.18_24.less_than_high_school = "B15001_005",
                            male.18_24.high_school_grad = "B15001_006",
                            male.18_24.some_college = "B15001_007",
                            male.18_24.associate_degree = "B15001_008",
                            male.18_24.bachelor_degree = "B15001_009",
                            male.18_24.graduate_degree = "B15001_010",
                            male.25_34.less_than_9th_grade = "B15001_012",
                            male.25_34.less_than_high_school = "B15001_013",
                            male.25_34.high_school_grad = "B15001_014",
                            male.25_34.some_college = "B15001_015",
                            male.25_34.associate_degree = "B15001_016",
                            male.25_34.bachelor_degree = "B15001_017",
                            male.25_34.graduate_degree = "B15001_018",
                            male.35_44.less_than_9th_grade = "B15001_020",
                            male.35_44.less_than_high_school = "B15001_021",
                            male.35_44.high_school_grad = "B15001_022",
                            male.35_44.some_college = "B15001_023",
                            male.35_44.associate_degree = "B15001_024",
                            male.35_44.bachelor_degree = "B15001_025",
                            male.35_44.graduate_degree = "B15001_026",
                            male.45_64.less_than_9th_grade = "B15001_028",
                            male.45_64.less_than_high_school = "B15001_029",
                            male.45_64.high_school_grad = "B15001_030",
                            male.45_64.some_college = "B15001_031",
                            male.45_64.associate_degree = "B15001_032",
                            male.45_64.bachelor_degree = "B15001_033",
                            male.45_64.graduate_degree = "B15001_034",
                            male.65_plus.less_than_9th_grade = "B15001_036",
                            male.65_plus.less_than_high_school = "B15001_037",
                            male.65_plus.high_school_grad = "B15001_038",
                            male.65_plus.some_college = "B15001_039",
                            male.65_plus.associate_degree = "B15001_040",
                            male.65_plus.bachelor_degree = "B15001_041",
                            male.65_plus.graduate_degree = "B15001_042",
                            # 25-64 
                            female.18_24.less_than_9th_grade = "B15001_045",
                            female.18_24.less_than_high_school = "B15001_046",
                            female.18_24.high_school_grad = "B15001_047",
                            female.18_24.some_college = "B15001_048",
                            female.18_24.associate_degree = "B15001_049",
                            female.18_24.bachelor_degree = "B15001_050",
                            female.18_24.graduate_degree = "B15001_051",
                            female.25_34.less_than_9th_grade = "B15001_053",
                            female.25_34.less_than_high_school = "B15001_054",
                            female.25_34.high_school_grad = "B15001_055",
                            female.25_34.some_college = "B15001_056",
                            female.25_34.associate_degree = "B15001_057",
                            female.25_34.bachelor_degree = "B15001_058",
                            female.25_34.graduate_degree = "B15001_059",
                            female.35_44.less_than_9th_grade = "B15001_061",
                            female.35_44.less_than_high_school = "B15001_062",
                            female.35_44.high_school_grad = "B15001_063",
                            female.35_44.some_college = "B15001_064",
                            female.35_44.associate_degree = "B15001_065",
                            female.35_44.bachelor_degree = "B15001_066",
                            female.35_44.graduate_degree = "B15001_067",
                            female.45_64.less_than_9th_grade = "B15001_069",
                            female.45_64.less_than_high_school = "B15001_070",
                            female.45_64.high_school_grad = "B15001_071",
                            female.45_64.some_college = "B15001_072",
                            female.45_64.associate_degree = "B15001_073",
                            female.45_64.bachelor_degree = "B15001_074",
                            female.45_64.graduate_degree = "B15001_075",
                            female.65_plus.less_than_9th_grade = "B15001_077",
                            female.65_plus.less_than_high_school = "B15001_078",
                            female.65_plus.high_school_grad = "B15001_079",
                            female.65_plus.some_college = "B15001_080",
                            female.65_plus.associate_degree = "B15001_081",
                            female.65_plus.bachelor_degree = "B15001_082",
                            female.65_plus.graduate_degree = "B15001_083"
                            # geographic mobility, B07001_001E et seq.
                            # geographic mobility AND income (as multiple of poverty level) B07012 et seq
                            # B08302 time leaving home to go to work
                            # B08134 how long it takes to get to work by travel time
  
                            # how to get to work B08006
                            # grandparents B10051
                            # household type [nonfamily, single male, single female, householder living alone, householder not alone]
                            # B12002 marital status
                            # B12006 marital status by labor force participation
                            # B12007 median age at first marriage (by sex, age, race)
                            # B12504 median duration of current marriage by (sex,age) by marital status
                            # B13002 women 15 to 50 who had a birth in past 12 months by marital status and age
                            # B15001 SEX BY AGE BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 18 YEARS AND OVER  
                            # B16001 language spoken at home
                            # B17001 poverty status in past 12 months by sex by age
                            # B18 disability status
                            # B19 household income
                            # B20 sex by earnings in past 12 months
                            # B21 sex by veteran status
                            # B22 receipts by food stamps
                            # B23 sex by age by employment status
                            # B24 sex by occupation and median earnings in past 12 months
                            # B25 Housing units
                            # B26 group quarters population
                            # B27 health insurance coverage status
                            # B28 types of computers in household
                            # C02003 detailed race
                            # C15 sex by educational attainment
                            # C16 languages spoken at home
                            # C17 ratio of income to poverty level in past 12 months
                            # C18 age by number of disabilities
                            # C21 sex by age by veteran status for civilian population
                            ), 
              year = 2016,
              output = "wide",
              state = state_abbrev)
  inner_join(gender,ethnicity,by=c('GEOID','NAME')) %>%
    inner_join(education,by=c('GEOID','NAME')) %>%
    inner_join(econ,by=c('GEOID','NAME'))
}

save_census_as_csv <- function(state_abbrev) {
  write.csv(census_fetch(state_abbrev), file=paste0(state_abbrev,'.csv'), row.names=FALSE)
}

save_all_exit_polled_states <- function() {
  for (state in levels(unique(national_exit_polls$state))) {
    if (state != "nation") {
      save_census_as_csv(state);
    }
  }
}
```

Train on the exit polls to create the coefficients for the logistic
regression, then use that to predict the turnout by county.

# Maximum turnout

Assuming there was maximum voter turnout, what would the election have
looked like in our polled states? We can use post-stratification to
estimate the proportion of each county won by Clinton, then multiply by
the voting age population to estimate the votes per county. Adding up
all the counties’s votes will give us the estimated votes for the state,
and if it is 50%+1 or greater of the voting age population Clinton would
have won the state.

We begin with computing the voting age population for a given county
(row of the CSV file):

``` r
voting_age_pop <- function(row) {
  sum(row[,c("female.18_19E","female.20E","female.21E","female.22_24E","female.25_29E","female.30_34E","female.35_39E","female.40_44E","female.45_49E","female.50_54E","female.55_59E","female.60_61E","female.62_64E","female.65_66E","female.67_69E","female.70_74E","female.75_79E","female.80_84E","female.85_plusE","male.18_19E","male.20E","male.21E","male.22_24E","male.25_29E","male.30_34E","male.35_39E","male.40_44E","male.45_49E","male.50_54E","male.55_59E","male.60_61E","male.62_64E","male.65_66E","male.67_69E","male.70_74E","male.75_79E","male.80_84E","male.85_plusE")])
}
```

## Post Stratified Terms

We start with the race and gender terms, age (for non-retirees), and
education.

``` r
safe_sum <- function(row) {
  if (nrow(row) == 0 | ncol(row) == 0) {
    return(0);
  }
  return(sum(row,na.rm=TRUE))
}
count_high_school_educated <- function(row) {
  safe_sum(row[,grep('.(less_than_9th_gradeE|less_than_high_schoolE|high_school_gradE)$',names(row))])
}
count_some_college <- function(row) {
  safe_sum(row[,grep(".(some_collegeE|associate_degreeE)$",names(row))])
}
count_college_grads <- function(row) {
  safe_sum(row[,grep(".bachelor_degreeE$",names(row))])
}
count_postgrads <- function(row) {
  safe_sum(row[,grep(".graduate_degreeE$",names(row))])
}
count_retirees <- function(row) {
  safe_sum(row[,c("male.65_66E","male.67_69E","male.70_74E","male.75_79E","male.80_84E","male.85_plusE","female.65_66E","female.67_69E","female.70_74E","female.75_79E","female.80_84E","female.85_plusE")])
}
weigh_county <- function(row) {
  (safe_sum(row[,grep(pattern='^male.white.*E$',names(row))])*invlogit(beta0+beta_white_male) +
    safe_sum(row[,grep(pattern='^female.white.*E$',names(row))])*invlogit(beta0+beta_white_female) +
    safe_sum(row[,grep(pattern='^male.black.*E$',names(row))])*invlogit(beta0+beta_black_male) +
    safe_sum(row[,grep(pattern='^female.black.*E$',names(row))])*invlogit(beta0+beta_black_female) +
    safe_sum(row[,grep(pattern='^male.hispanic.*E$',names(row))])*invlogit(beta0+beta_latino_male) +
    safe_sum(row[,grep(pattern='^female.hispanic.*E$',names(row))])*invlogit(beta0+beta_latino_female) +
    safe_sum(row[,grep(pattern='male.a*E$',names(row))])*invlogit(beta0+beta_others_gender) + 
    safe_sum(row[,grep(pattern='male.p*E$',names(row))])*invlogit(beta0+beta_others_gender) + 
    count_high_school_educated(row)*invlogit(beta0-1*beta_education) +
    count_some_college(row)*invlogit(beta0+0*beta_education) + 
    count_college_grads(row)*invlogit(beta0+1*beta_education) + 
    count_postgrads(row)*invlogit(beta0+2*beta_education) + 
     safe_sum(row[,c("male.18_19E","female.18_19E")])*invlogit(beta_age*(18.5-46)) +
     safe_sum(row[,c("male.20E","female.20E")])*invlogit(beta_age*(20-46)) +
     safe_sum(row[,c("male.21E","female.21E")])*invlogit(beta_age*(21-46)) +
     safe_sum(row[,c("male.22_24E","female.22_24E")])*invlogit(beta_age*(23-46)) +
     safe_sum(row[,c("male.25_29E","female.25_29E")])*invlogit(beta_age*(0.5*(25+29)-46)) +
     safe_sum(row[,c("male.30_34E","female.30_34E")])*invlogit(beta_age*(0.5*(30+34)-46)) +
     safe_sum(row[,c("male.35_39E","female.35_39E")])*invlogit(beta_age*(0.5*(35+39) - 46)) + 
     safe_sum(row[,c("male.40_44E","female.40_44E")])*invlogit(beta_age*(0.5*(40+44) - 46)) + 
     safe_sum(row[,c("male.45_49E","female.45_49E")])*invlogit(beta_age*(0.5*(45+49) - 46)) + 
     safe_sum(row[,c("male.50_54E","female.50_54E")])*invlogit(beta_age*(0.5*(50+54) - 46)) + 
     safe_sum(row[,c("male.55_59E","female.55_59E")])*invlogit(beta_age*(0.5*(55+59) - 46)) + 
     safe_sum(row[,c("male.60_61E","female.60_61E")])*invlogit(beta_age*(60.5 - 46)) + 
     safe_sum(row[,c("male.62_64E","female.62_64E")])*invlogit(beta_age*(63 - 46)) + 
    count_retirees(row)*invlogit(beta0+beta_retiree))/(
  safe_sum(row[,grep(pattern="male.white.*E$",names(row))]) +
    safe_sum(row[,grep(pattern="male.black.*E$",names(row))]) +
    safe_sum(row[,grep(pattern="male.hispanic.*E$",names(row))]) +
    safe_sum(row[,grep(pattern="male.a*E$",names(row))]) +
    safe_sum(row[,grep(pattern="male.p*E$",names(row))]) + 
    count_high_school_educated(row) +
    count_some_college(row) + 
    count_college_grads(row) + 
    count_postgrads(row) +
    safe_sum(row[,c("male.18_19E","male.20E","male.21E","male.22_24E","male.25_29E","male.30_34E","male.35_39E","male.40_44E","male.45_49E","male.50_54E","male.55_59E","male.60_61E","male.62_64E","female.18_19E","female.20E","female.21E","female.22_24E","female.25_29E","female.30_34E","female.35_39E","female.40_44E","female.45_49E","female.50_54E","female.55_59E","female.60_61E","female.62_64E"
)]) +
    count_retirees(row)
  )
}
```

The expected number of votes per county would be the county’s weight
multiplied by its voting age population:

``` r
county_expected_votes <- function(row) {
  weigh_county(row)*voting_age_pop(row)
}
```

``` r
state_expected_votes <- function(state) {
  state %>% rowwise %>% county_expected_votes %>% sum
}
```

We can then estimate if Clinton will win the state by comparing the
expected votes (summed over counties) to 50%+1 of the population:

``` r
would_clinton_have_won <- function(state) {
  expected_votes <- (state %>% rowwise %>% county_expected_votes %>% sum);
  vap <- voting_age_pop(state);
  print(paste0(expected_votes,' > ',vap*0.5))
  return(expected_votes > floor(vap*0.5))
}
```

``` r
states <- levels(unique(national_exit_polls$state))
states <- states[which(states!='nation')]
df <- data.frame(
  state=states
#  expected_votes = map(states,function(x) {
#read.csv(file=paste0('D:/src/pa/data/census/acs5/2016/',x,'.csv'),header=TRUE,sep=',') %>% rowwise %>% county_expected_votes %>% sum)
#  }),
#  threshold = map(states,function(x) {
#    1+floor((read.csv(file=paste0('D:/src/pa/data/census/acs5/2016/',x,'.csv'),header=TRUE,sep=',') %>% voting_age_pop)*0.5)
#  })
)
state_expected_votes_from_name <- function(name) {
  (read.csv(file=paste0('D:/src/pa/data/census/acs5/2016/',name,'.csv'),header=TRUE,sep=',') %>% rowwise %>% county_expected_votes %>% sum)
}
threshold_for_state <- function(name) {
  1+floor( (read.csv(file=paste0('D:/src/pa/data/census/acs5/2016/',name,'.csv'),header=TRUE,sep=',') %>% voting_age_pop)*0.5)
}
df <- df %>% mutate(expected_votes=map(state,state_expected_votes_from_name),
                    threshold=map(state,threshold_for_state)
                    )

max_turnout_test <- function() {
  for (state in levels(unique(national_exit_polls$state))) {
    if (state != "nation") {
      state_data <- read.csv(file=paste0('D:/src/pa/data/census/acs5/2016/',state,'.csv'),header=TRUE,sep=',')
      print(paste0(state,' => ',would_clinton_have_won(state_data)))
    }
  }
}
kable(df)
```

<table>

<thead>

<tr>

<th style="text-align:left;">

state

</th>

<th style="text-align:left;">

expected\_votes

</th>

<th style="text-align:left;">

threshold

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Arizona

</td>

<td style="text-align:left;">

2475103.07316235

</td>

<td style="text-align:left;">

2554480

</td>

</tr>

<tr>

<td style="text-align:left;">

California

</td>

<td style="text-align:left;">

14843310.9908173

</td>

<td style="text-align:left;">

14756962

</td>

</tr>

<tr>

<td style="text-align:left;">

Colorado

</td>

<td style="text-align:left;">

1967640.59771496

</td>

<td style="text-align:left;">

2056558

</td>

</tr>

<tr>

<td style="text-align:left;">

Florida

</td>

<td style="text-align:left;">

7842625.08975677

</td>

<td style="text-align:left;">

7934088

</td>

</tr>

<tr>

<td style="text-align:left;">

Georgia

</td>

<td style="text-align:left;">

3885785.57194592

</td>

<td style="text-align:left;">

3802073

</td>

</tr>

<tr>

<td style="text-align:left;">

Illinois

</td>

<td style="text-align:left;">

4838092.66715181

</td>

<td style="text-align:left;">

4930528

</td>

</tr>

<tr>

<td style="text-align:left;">

Indiana

</td>

<td style="text-align:left;">

2353876.66063265

</td>

<td style="text-align:left;">

2503794

</td>

</tr>

<tr>

<td style="text-align:left;">

Iowa

</td>

<td style="text-align:left;">

1096646.2574918

</td>

<td style="text-align:left;">

1189538

</td>

</tr>

<tr>

<td style="text-align:left;">

Kentucky

</td>

<td style="text-align:left;">

1580471.93551052

</td>

<td style="text-align:left;">

1698900

</td>

</tr>

<tr>

<td style="text-align:left;">

Maine

</td>

<td style="text-align:left;">

485979.062167357

</td>

<td style="text-align:left;">

535212

</td>

</tr>

<tr>

<td style="text-align:left;">

Michigan

</td>

<td style="text-align:left;">

3665512.8660522

</td>

<td style="text-align:left;">

3840919

</td>

</tr>

<tr>

<td style="text-align:left;">

Minnesota

</td>

<td style="text-align:left;">

1945975.40230296

</td>

<td style="text-align:left;">

2084386

</td>

</tr>

<tr>

<td style="text-align:left;">

Missouri

</td>

<td style="text-align:left;">

2205013.06704727

</td>

<td style="text-align:left;">

2332264

</td>

</tr>

<tr>

<td style="text-align:left;">

Nevada

</td>

<td style="text-align:left;">

1064836.05466317

</td>

<td style="text-align:left;">

1087271

</td>

</tr>

<tr>

<td style="text-align:left;">

New Hampshire

</td>

<td style="text-align:left;">

485630.96656623

</td>

<td style="text-align:left;">

530263

</td>

</tr>

<tr>

<td style="text-align:left;">

New Jersey

</td>

<td style="text-align:left;">

3415922.20962055

</td>

<td style="text-align:left;">

3452822

</td>

</tr>

<tr>

<td style="text-align:left;">

New Mexico

</td>

<td style="text-align:left;">

786405.725584163

</td>

<td style="text-align:left;">

790460

</td>

</tr>

<tr>

<td style="text-align:left;">

New York

</td>

<td style="text-align:left;">

7712203.90447862

</td>

<td style="text-align:left;">

7735525

</td>

</tr>

<tr>

<td style="text-align:left;">

North Carolina

</td>

<td style="text-align:left;">

3778591.41449635

</td>

<td style="text-align:left;">

3826502

</td>

</tr>

<tr>

<td style="text-align:left;">

Ohio

</td>

<td style="text-align:left;">

4227504.84922629

</td>

<td style="text-align:left;">

4473541

</td>

</tr>

<tr>

<td style="text-align:left;">

Oregon

</td>

<td style="text-align:left;">

1457130.04248031

</td>

<td style="text-align:left;">

1560437

</td>

</tr>

<tr>

<td style="text-align:left;">

Pennsylvania

</td>

<td style="text-align:left;">

4770210.65312913

</td>

<td style="text-align:left;">

5039855

</td>

</tr>

<tr>

<td style="text-align:left;">

South Carolina

</td>

<td style="text-align:left;">

1867173.46009275

</td>

<td style="text-align:left;">

1874414

</td>

</tr>

<tr>

<td style="text-align:left;">

Texas

</td>

<td style="text-align:left;">

9985952.96612459

</td>

<td style="text-align:left;">

9911980

</td>

</tr>

<tr>

<td style="text-align:left;">

Utah

</td>

<td style="text-align:left;">

960356.070588747

</td>

<td style="text-align:left;">

1021616

</td>

</tr>

<tr>

<td style="text-align:left;">

Virginia

</td>

<td style="text-align:left;">

3186845.09203355

</td>

<td style="text-align:left;">

3222373

</td>

</tr>

<tr>

<td style="text-align:left;">

Washington

</td>

<td style="text-align:left;">

2583054.16494792

</td>

<td style="text-align:left;">

2734276

</td>

</tr>

<tr>

<td style="text-align:left;">

Wisconsin

</td>

<td style="text-align:left;">

2073709.49346675

</td>

<td style="text-align:left;">

2226651

</td>

</tr>

</tbody>

</table>

# Voter turnout

We are told there were 138,846,571 ballots cast in 2016 but 230,931,921
eligible voters in 2016 (and 250,055,734 people of voting age).
(<http://www.electproject.org/2016g>)