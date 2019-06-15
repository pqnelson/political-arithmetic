  - [How did Trump Win the White
    House?](#how-did-trump-win-the-white-house)
  - [Testing Gelman’s Model](#testing-gelmans-model)
      - [Strategy](#strategy)
      - [Data Caveat: Exit Polls noisier than CPS
        estimates](#data-caveat-exit-polls-noisier-than-cps-estimates)
      - [Computational Details](#computational-details)
  - [Loading Data](#loading-data)
      - [Official Results](#official-results)
      - [Exit Poll Data](#exit-poll-data)
          - [Wilson Interval](#wilson-interval)
      - [Census Data](#census-data)
  - [Estimating Voter Turnout](#estimating-voter-turnout)
      - [State Effects](#state-effects)
      - [Education](#education)
          - [Overall Factor](#overall-factor)
          - [Interacting with State](#interacting-with-state)

``` r
# local code
source("../R/states.R")
source("../R/census.R")
```

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
  - Rob Trangucci, Imad Ali, Andrew Gelman, Doug Rivers, “Voting
    patterns in 2016: Exploration using multilevel regression and
    poststratification (MRP) on pre-election polls”
    [arXiv:1802.00842](https://arxiv.org/abs/1802.00842)

# Testing Gelman’s Model

In [arXiv:1802.00842](https://arxiv.org/abs/1802.00842), Gelman et
al. suggest a likely voter model specifically via a logistic regression
defined by

    logit(voted) ~  1 + female + pre_election_poll_for_state +
                    (1 | state) + (1 | age) +
                    (1 | education) + (1 + pre_election_poll_for_state | ethnicity) +
                    (1 | married) + (1 | married:age) +
                    (1 | married:state) + (1 | married:ethnicity) +
                    (1 | married:gender) + (1 | married:education) +
                    (1 | state:gender) + (1 | age:gender) +
                    (1 | education:gender) + (1 | ethnicity:gender) +
                    (1 | state:ethnicity) + (1 | state:age) +
                    (1 | state:education) + (1 | ethnicity:age) +
                    (1 | ethnicity:education) + (1 | age:education) +
                    (1 | state:education:age) + (1 | education:age:gender)

Independently, given someone voted, Gelman and friends setup a logistic
regression describing the probability someone voted for Clinton (or, if
you prefer, Trump):

    logit(Trump|voted) ~  1 + female + pre_election_poll_for_state +
                         (1 | state) + (1 | age) +
                         (1 | education) + (1 + pre_election_poll_for_state | ethnicity) +
                         (1 | married) + (1 | married:age) +
                         (1 | married:state) + (1 | married:ethnicity) +
                         (1 | married:gender) + (1 | married:education) +
                         (1 | state:gender) + (1 | age:gender) +
                         (1 | education:gender) + (1 | ethnicity:gender) +
                         (1 | state:ethnicity) + (1 | state:age) +
                         (1 | state:education) + (1 | ethnicity:age) +
                         (1 | ethnicity:education) + (1 | age:education) +
                         (1 | state:education:age) + (1 | education:age:gender)

The probability that someone voted for Trump may be computed as
`Pr(Trump|voted)Pr(voted)`, using the logistic regressions we just
assembled.

## Strategy

We can approximate the coefficients for the logistic regression using
exit poll data. Admittedly this is rather *ad hoc*, but we can estimate
the margin of error for the various coefficients using the Wilson
confidence interval as a proxy for the standard deviation of a Gaussian
variable centered at the polled percentages.

This will allow us to test certain hypotheses (“young people don’t
vote”, “there were more older white people than expected”, etc.)
without having to resort to other data sources, we hope.

## Data Caveat: Exit Polls noisier than CPS estimates

We are using exit polls from CNN and friends, whereas Gelman *et al.*
are using CPS estimates, which are much cleaner and much more accurate.
Therefore, we should not expect to perfectly reproduce their findings.
But it is a starting point for computation.

## Computational Details

So, for the logistic regression coefficients, we will compute the
coefficients using a strategy reflected in the following examples for
the regression on the probability someone will vote:

  - `female = log(((percent women in national exit poll)*(votes cast
    nationwide)/(exit poll size))/((number of women nationwide eligible
    to vote) - ((percent women in national exit poll)*(votes cast
    nationwide)/(exit poll size)))`
  - `(1|state) = log((votes cast in state)/(number of eligible voters
    who didn't turn out))`
  - `(1|age) = log((exit poll percentage for age)*(votes cast
    nationwide)/(sample size of exit poll)/((number of people in age
    bracket nationwide) - (exit poll percentage for age)*(votes cast
    nationwide)/(sample size of exit poll)))`
  - `(1 | state:age)` computed like `(1 | age)` but working with state
    specific exit polls and state specific population data
  - `pre_election_poll_for_state` is the source of the greatest
    variability, since there are various possible choices for a
    pre-election prior (e.g., we could use some estimates from 538 or
    Inside Elections or some other source, or we could use an actual
    poll).

The center of the Wilson confidence interval is “close enough” to the
exit poll percentages that we might as well use the exit poll
percentages. The usefulness of the Wilson interval is its width will
give us an estimate of a `sigma`, and we can transform the exit poll
percentage into the mean of a normally distributed random variable with
an estimated standard deviation `sigma`. The only caveat is, since exit
polls are noisy, we use `z=4` when computing the sigma (or `z=2` for
half the sigma).

We then test the hypotheses using this jury-rigged statistical model.
There is no shortage of
[explanations](https://fivethirtyeight.com/features/the-real-story-of-2016/)
for what happened, which we can examine.

# Loading Data

## Official Results

We load the official results.

``` r
load('../data/elections/presidential/county/countypres_2000-2016.RData')
states_factors <- factor(append(states(),c("District of Columbia", "nation")))
x$state <- factor(x$state, levels(states_factors))
election_2016 <- x[which(x$year == 2016),]
election_2016$totalvotes[is.na(election_2016$totalvotes)] <- 0
```

## Exit Poll Data

We load CNN’s exit poll data. Fortunately, it is stored as a CSV
file.

``` r
exit_poll_path <- "../data/elections/presidential/exit_polls/cnn_04022017.csv"
exit_poll_df <- read.csv(file=exit_poll_path, header=TRUE, sep=",", stringsAsFactors= FALSE)
exit_poll_df$state <- factor(exit_poll_df$state, levels(states_factors))
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

### Wilson Interval

Before moving on, we will require usage of Wilson confidence intervals
to estimate the noisiness of exit poll data.

``` r
wilson_center <- function(p, n, z) {
  assert_that(all(is.numeric(p)))
  assert_that(all(0 <= p && p <= 1))
  assert_that(all(is.numeric(n)))
  assert_that(all(n > 0))
  assert_that(all(is.numeric(z)))
  assert_that(all(z > 0))
  (p + 0.5*z*z/n)/(1.0 + z*z*1.0/n);
}

wilson_width <- function(p, n, z) {
  assert_that(all(is.numeric(p)))
  assert_that(all(0 <= p && p <= 1))
  assert_that(all(is.numeric(n)))
  assert_that(all(n > 0))
  assert_that(all(is.numeric(z)))
  assert_that(all(z > 0))
  (z/(1.0 + z*z*1.0/n))*sqrt(p*(1.0-p)/n  + (z*0.5/n)**2);
}
```

## Census Data

We load the census data for each state appearing in the exit poll.

``` r
state_census_data <- function() {
  df <- data.frame()
  for (state in unique(exit_poll_df$state)) {
    if (state != "nation") {
      state_data <- read.csv(file=paste0('../data/census/acs5/2016/', state, '.csv'), header=TRUE, sep=',')
      state_data$state = state;
      df <- rbind(df, state_data)
    }
  }
  df
}
```

We load the census data and renormalize the states, to match the exit
poll’s `state` factors.

``` r
census_data <- categorize(state_census_data())
census_data$state <- factor(census_data$state, levels(states_factors))
```

# Estimating Voter Turnout

## State Effects

We will compute the coefficient `(1 | state)`. This is
`log(Pr(turnout|state)/Pr(!turnout|state)) -
log(Pr(turnout)/Pr(!turnout))`.

``` r
turnout_state_log_coefs <- function(election_data, census_data) {
  census_data %>% 
    group_by(state) %>%
    summarize(vap = sum(voting_age)) %>%
    inner_join(election_data %>% group_by(state) %>% summarize(tv = sum(totalvotes)), by="state") %>%
    transmute(state,
              proportion = tv/vap) %>%
    transmute(state,
              beta_state = log(proportion/(1 - proportion))) %>%
    unique
}
```

``` r
kable(turnout_state_log_coefs(filter(election_2016,party=="democrat"), census_data))
```

<table>

<thead>

<tr>

<th style="text-align:left;">

state

</th>

<th style="text-align:right;">

beta\_state

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Arizona

</td>

<td style="text-align:right;">

0.0581703

</td>

</tr>

<tr>

<td style="text-align:left;">

California

</td>

<td style="text-align:right;">

\-0.0780185

</td>

</tr>

<tr>

<td style="text-align:left;">

Colorado

</td>

<td style="text-align:right;">

0.7351775

</td>

</tr>

<tr>

<td style="text-align:left;">

Florida

</td>

<td style="text-align:right;">

0.3790481

</td>

</tr>

<tr>

<td style="text-align:left;">

Georgia

</td>

<td style="text-align:right;">

0.1648291

</td>

</tr>

<tr>

<td style="text-align:left;">

Illinois

</td>

<td style="text-align:right;">

0.2562046

</td>

</tr>

<tr>

<td style="text-align:left;">

Indiana

</td>

<td style="text-align:right;">

0.1851792

</td>

</tr>

<tr>

<td style="text-align:left;">

Iowa

</td>

<td style="text-align:right;">

0.6555144

</td>

</tr>

<tr>

<td style="text-align:left;">

Kentucky

</td>

<td style="text-align:right;">

0.2667415

</td>

</tr>

<tr>

<td style="text-align:left;">

Maine

</td>

<td style="text-align:right;">

0.8235900

</td>

</tr>

<tr>

<td style="text-align:left;">

Michigan

</td>

<td style="text-align:right;">

0.5097904

</td>

</tr>

<tr>

<td style="text-align:left;">

Minnesota

</td>

<td style="text-align:right;">

0.8779563

</td>

</tr>

<tr>

<td style="text-align:left;">

Missouri

</td>

<td style="text-align:right;">

0.4132111

</td>

</tr>

<tr>

<td style="text-align:left;">

Nevada

</td>

<td style="text-align:right;">

0.0701401

</td>

</tr>

<tr>

<td style="text-align:left;">

New Hampshire

</td>

<td style="text-align:right;">

0.8559753

</td>

</tr>

<tr>

<td style="text-align:left;">

New Jersey

</td>

<td style="text-align:right;">

0.2452099

</td>

</tr>

<tr>

<td style="text-align:left;">

New Mexico

</td>

<td style="text-align:right;">

0.0198866

</td>

</tr>

<tr>

<td style="text-align:left;">

New York

</td>

<td style="text-align:right;">

\-0.0072810

</td>

</tr>

<tr>

<td style="text-align:left;">

North Carolina

</td>

<td style="text-align:right;">

0.4877199

</td>

</tr>

<tr>

<td style="text-align:left;">

Ohio

</td>

<td style="text-align:right;">

0.4655628

</td>

</tr>

<tr>

<td style="text-align:left;">

Oregon

</td>

<td style="text-align:right;">

0.5809006

</td>

</tr>

<tr>

<td style="text-align:left;">

Pennsylvania

</td>

<td style="text-align:right;">

0.4334794

</td>

</tr>

<tr>

<td style="text-align:left;">

South Carolina

</td>

<td style="text-align:right;">

0.2451518

</td>

</tr>

<tr>

<td style="text-align:left;">

Texas

</td>

<td style="text-align:right;">

\-0.1908018

</td>

</tr>

<tr>

<td style="text-align:left;">

Utah

</td>

<td style="text-align:right;">

0.2158158

</td>

</tr>

<tr>

<td style="text-align:left;">

Virginia

</td>

<td style="text-align:right;">

0.4822370

</td>

</tr>

<tr>

<td style="text-align:left;">

Washington

</td>

<td style="text-align:right;">

0.3509546

</td>

</tr>

<tr>

<td style="text-align:left;">

Wisconsin

</td>

<td style="text-align:right;">

0.7001138

</td>

</tr>

</tbody>

</table>

## Education

The coefficients of interest are:

``` 
                     (1 | education) + (1 | married:education) +
                     (1 | education:gender) + (1 | state:education) +
                     (1 | ethnicity:education) + (1 | age:education) +
                     (1 | state:education:age) + (1 | education:age:gender)
                     + (non education coefficients)
```

Similarly, education coefficients are determined thus using crude
estimates for logistic regression coefficients. We estimate the number
of voters with a level `ed` of education by taking the `options_perc
- 2z*wilson_interval_width` proportion of the total votes cast (this is
the assumption that the exit polls reflect the composition of voters).
This lower bound of the Wilson interval is stored in the `lower_bound`
which we use as the numerator of the logistic coefficient computation
(the logarithm of the ratio of `ed` voters to `ed` non-voters).

### Overall Factor

We compute the `(1|education)` coefficient
thus:

``` r
turnout_education_log_coefs <- function(exit_polls, election_data, census_data, z = 2) {
  assert_that(all(is.state(exit_polls$state) || exit_polls$state == "nation"))
  assert_that(all(is.numeric(exit_polls$num_respondents)))
  assert_that(all(is.numeric(exit_polls$options_perc)))
  assert_that(all(is.numeric(census_data$education_high_school)))
  assert_that(all(is.numeric(census_data$education_some_college)))
  assert_that(all(is.numeric(census_data$education_college_grad)))
  assert_that(all(is.numeric(census_data$education_postgrad)))
  assert_that(all(is.state(census_data$state)))
  assert_that(all(has.electoral_delegates(election_data$state)))
  assert_that(all(is.numeric(election_data$totalvotes)))
  exit_polls %>%
    filter(questions=="Education", state=="nation") %>%
    group_by(options) %>%
    mutate(p = sum(0.01*options_perc*num_respondents)/sum(num_respondents),
           count = sum(num_respondents),
           width = wilson_width(p, count, z),
           total_votes = sum(election_data$totalvotes),
           lower_bound = (p - 2*z*width)*total_votes,
           high_school = sum(census_data$education_high_school),
           some_college = sum(census_data$education_some_college) + 0.5*sum(census_data$education_assoc_degree),
           college_grad = sum(census_data$education_college_grad) + 0.5*sum(census_data$education_assoc_degree),
           postgrad = sum(census_data$education_postgrad),
           proportion = (ifelse(options == "High school or less",
                                   min(0.99, lower_bound/high_school),
                                   ifelse(options == "Some college",
                                          min(0.99, lower_bound/some_college),
                                          ifelse(options == "College graduate",
                                                 min(0.99, lower_bound/college_grad),
                                                 ifelse(options == "Postgraduate",
                                                        min(0.99, lower_bound/postgrad),
                                                        NaN)))))) %>%
    transmute(beta_education = log(proportion/(1.0-proportion)),
              tau_education = 2*(4*width/(0.01*proportion))**-2) %>%
    unique
}
```

``` r
kable(turnout_education_log_coefs(exit_poll_df, filter(election_2016, party=="democrat"), census_data))
```

<table>

<thead>

<tr>

<th style="text-align:left;">

options

</th>

<th style="text-align:right;">

beta\_education

</th>

<th style="text-align:right;">

tau\_education

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

High school or less

</td>

<td style="text-align:right;">

\-0.9967572

</td>

<td style="text-align:right;">

0.0377878

</td>

</tr>

<tr>

<td style="text-align:left;">

Some college

</td>

<td style="text-align:right;">

1.9105408

</td>

<td style="text-align:right;">

0.2676457

</td>

</tr>

<tr>

<td style="text-align:left;">

College graduate

</td>

<td style="text-align:right;">

3.4126658

</td>

<td style="text-align:right;">

0.3305849

</td>

</tr>

<tr>

<td style="text-align:left;">

Postgraduate

</td>

<td style="text-align:right;">

4.5951199

</td>

<td style="text-align:right;">

0.5096226

</td>

</tr>

</tbody>

</table>

The difficulty is there are more people in the exit polls claiming to be
educated than the census data corroborates. Although postgrads are
within the margin of error (roughly 21.9m people claim to be postgrads
in the exit poll, whereas the census claims there should be 20.1m
postgrads), the number of college graduates is off by 5m. I suspect
people may consider receiving an associate’s degree as being a “college
graduate”, which complicates the analysis. For simplicity, I suppose 50%
of associate degrees call themselves college graduates, the other 50%
say they have “some college” level of education.

### Interacting with State

We compute the `(1|education:state)` coefficient
thus:

``` r
turnout_education_state_log_coefs <- function(exit_polls, election_data, census_data, z = 2) {
  assert_that(all(is.state(exit_polls$state) || exit_polls$state == "nation"))
  assert_that(all(is.numeric(exit_polls$num_respondents)))
  assert_that(all(is.numeric(exit_polls$options_perc)))
  assert_that(all(is.numeric(census_data$education_high_school)))
  assert_that(all(is.numeric(census_data$education_some_college)))
  assert_that(all(is.numeric(census_data$education_college_grad)))
  assert_that(all(is.numeric(census_data$education_postgrad)))
  assert_that(all(is.state(census_data$state)))
  assert_that(all(has.electoral_delegates(election_data$state)))
  assert_that(all(is.numeric(election_data$totalvotes)))
  education_effects <- turnout_education_log_coefs(exit_polls, election_data, census_data, z)
  state_effects <- turnout_state_log_coefs(election_data, census_data)
  exit_polls %>%
    filter(questions=="Education", state!="nation") %>%
    mutate(options = factor(options)) %>%
    group_by(state) %>% # this group_by is responsible for the ":state" factor in `(1|education:state)`
    mutate(width = wilson_width(0.01*options_perc, num_respondents, z),
           center = wilson_center(0.01*options_perc, num_respondents, z),
           total_votes = sum(election_data[which(election_data$state %in% state & !is.na(election_data$totalvotes)),]$totalvotes),
           estimate_voters_by_education = (center + 2*z*width)*total_votes,
           high_school = sum(census_data[which(census_data$state %in% state),]$education_high_school),
           some_college = sum(census_data[which(census_data$state %in% state),]$education_some_college) + 0.5*sum(census_data[which(census_data$state %in% state),]$education_assoc_degree),
           college_grad = sum(census_data[which(census_data$state %in% state),]$education_college_grad) + 0.5*sum(census_data[which(census_data$state %in% state),]$education_assoc_degree),
           postgrad = sum(census_data[which(census_data$state %in% state),]$education_postgrad)
    ) %>%
    mutate(proportion = (ifelse(options == "High school or less",
                                min(0.99, estimate_voters_by_education/high_school),
                                ifelse(options == "Some college",
                                       min(0.99, estimate_voters_by_education/some_college),
                                       ifelse(options == "College graduate",
                                              min(0.99, estimate_voters_by_education/college_grad),
                                              ifelse(options == "Postgraduate",
                                                     min(0.99, estimate_voters_by_education/postgrad),
                                                     NaN)))))) %>%
    transmute(beta_education = log(proportion/(1.0-proportion)) - education_effects[which(education_effects$options==options),]$beta_education - state_effects[which(state_effects$state==state),]$beta_state,
              tau_education = 2*(4*width/(0.01*proportion))**-2,
              options) %>%
    arrange(state,options)
}
```

``` r
kable(turnout_education_state_log_coefs(exit_poll_df, filter(election_2016, party=="democrat"), census_data))
```

<table>

<thead>

<tr>

<th style="text-align:left;">

state

</th>

<th style="text-align:right;">

beta\_education

</th>

<th style="text-align:right;">

tau\_education

</th>

<th style="text-align:left;">

options

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Arizona

</td>

<td style="text-align:right;">

\-3.1415552

</td>

<td style="text-align:right;">

0.0079489

</td>

<td style="text-align:left;">

College graduate

</td>

</tr>

<tr>

<td style="text-align:left;">

Arizona

</td>

<td style="text-align:right;">

0.0275928

</td>

<td style="text-align:right;">

0.0028902

</td>

<td style="text-align:left;">

High school or less

</td>

</tr>

<tr>

<td style="text-align:left;">

Arizona

</td>

<td style="text-align:right;">

\-0.0581703

</td>

<td style="text-align:right;">

0.0415381

</td>

<td style="text-align:left;">

Postgraduate

</td>

</tr>

<tr>

<td style="text-align:left;">

Arizona

</td>

<td style="text-align:right;">

\-2.2712988

</td>

<td style="text-align:right;">

0.0046544

</td>

<td style="text-align:left;">

Some college

</td>

</tr>

<tr>

<td style="text-align:left;">

California

</td>

<td style="text-align:right;">

\-3.4878580

</td>

<td style="text-align:right;">

0.0072423

</td>

<td style="text-align:left;">

College graduate

</td>

</tr>

<tr>

<td style="text-align:left;">

California

</td>

<td style="text-align:right;">

0.0105853

</td>

<td style="text-align:right;">

0.0039820

</td>

<td style="text-align:left;">

High school or less

</td>

</tr>

<tr>

<td style="text-align:left;">

California

</td>

<td style="text-align:right;">

\-1.2859139

</td>

<td style="text-align:right;">

0.0464237

</td>

<td style="text-align:left;">

Postgraduate

</td>

</tr>

<tr>

<td style="text-align:left;">

California

</td>

<td style="text-align:right;">

\-2.1827746

</td>

<td style="text-align:right;">

0.0061705

</td>

<td style="text-align:left;">

Some college

</td>

</tr>

<tr>

<td style="text-align:left;">

Colorado

</td>

<td style="text-align:right;">

\-3.1720948

</td>

<td style="text-align:right;">

0.0098322

</td>

<td style="text-align:left;">

College graduate

</td>

</tr>

<tr>

<td style="text-align:left;">

Colorado

</td>

<td style="text-align:right;">

0.5866196

</td>

<td style="text-align:right;">

0.0091467

</td>

<td style="text-align:left;">

High school or less

</td>

</tr>

<tr>

<td style="text-align:left;">

Colorado

</td>

<td style="text-align:right;">

\-0.7351775

</td>

<td style="text-align:right;">

0.0255889

</td>

<td style="text-align:left;">

Postgraduate

</td>

</tr>

<tr>

<td style="text-align:left;">

Colorado

</td>

<td style="text-align:right;">

\-1.4262590

</td>

<td style="text-align:right;">

0.0126414

</td>

<td style="text-align:left;">

Some college

</td>

</tr>

<tr>

<td style="text-align:left;">

Florida

</td>

<td style="text-align:right;">

\-3.4054107

</td>

<td style="text-align:right;">

0.0192356

</td>

<td style="text-align:left;">

College graduate

</td>

</tr>

<tr>

<td style="text-align:left;">

Florida

</td>

<td style="text-align:right;">

\-0.2823325

</td>

<td style="text-align:right;">

0.0070722

</td>

<td style="text-align:left;">

High school or less

</td>

</tr>

<tr>

<td style="text-align:left;">

Florida

</td>

<td style="text-align:right;">

\-0.3790481

</td>

<td style="text-align:right;">

0.0910996

</td>

<td style="text-align:left;">

Postgraduate

</td>

</tr>

<tr>

<td style="text-align:left;">

Florida

</td>

<td style="text-align:right;">

\-2.1667263

</td>

<td style="text-align:right;">

0.0167640

</td>

<td style="text-align:left;">

Some college

</td>

</tr>

<tr>

<td style="text-align:left;">

Georgia

</td>

<td style="text-align:right;">

\-3.0551759

</td>

<td style="text-align:right;">

0.0152014

</td>

<td style="text-align:left;">

College graduate

</td>

</tr>

<tr>

<td style="text-align:left;">

Georgia

</td>

<td style="text-align:right;">

\-0.0809546

</td>

<td style="text-align:right;">

0.0046114

</td>

<td style="text-align:left;">

High school or less

</td>

</tr>

<tr>

<td style="text-align:left;">

Georgia

</td>

<td style="text-align:right;">

\-0.1648291

</td>

<td style="text-align:right;">

0.0600823

</td>

<td style="text-align:left;">

Postgraduate

</td>

</tr>

<tr>

<td style="text-align:left;">

Georgia

</td>

<td style="text-align:right;">

\-1.8726722

</td>

<td style="text-align:right;">

0.0122656

</td>

<td style="text-align:left;">

Some college

</td>

</tr>

<tr>

<td style="text-align:left;">

Illinois

</td>

<td style="text-align:right;">

\-2.7365448

</td>

<td style="text-align:right;">

0.0069153

</td>

<td style="text-align:left;">

College graduate

</td>

</tr>

<tr>

<td style="text-align:left;">

Illinois

</td>

<td style="text-align:right;">

0.3899782

</td>

<td style="text-align:right;">

0.0033163

</td>

<td style="text-align:left;">

High school or less

</td>

</tr>

<tr>

<td style="text-align:left;">

Illinois

</td>

<td style="text-align:right;">

\-0.2562046

</td>

<td style="text-align:right;">

0.0190333

</td>

<td style="text-align:left;">

Postgraduate

</td>

</tr>

<tr>

<td style="text-align:left;">

Illinois

</td>

<td style="text-align:right;">

\-1.3692142

</td>

<td style="text-align:right;">

0.0061774

</td>

<td style="text-align:left;">

Some college

</td>

</tr>

<tr>

<td style="text-align:left;">

Indiana

</td>

<td style="text-align:right;">

\-2.8873302

</td>

<td style="text-align:right;">

0.0117537

</td>

<td style="text-align:left;">

College graduate

</td>

</tr>

<tr>

<td style="text-align:left;">

Indiana

</td>

<td style="text-align:right;">

\-0.2509909

</td>

<td style="text-align:right;">

0.0023429

</td>

<td style="text-align:left;">

High school or less

</td>

</tr>

<tr>

<td style="text-align:left;">

Indiana

</td>

<td style="text-align:right;">

\-0.1851792

</td>

<td style="text-align:right;">

0.0436520

</td>

<td style="text-align:left;">

Postgraduate

</td>

</tr>

<tr>

<td style="text-align:left;">

Indiana

</td>

<td style="text-align:right;">

\-2.0045932

</td>

<td style="text-align:right;">

0.0070317

</td>

<td style="text-align:left;">

Some college

</td>

</tr>

<tr>

<td style="text-align:left;">

Iowa

</td>

<td style="text-align:right;">

\-3.7313277

</td>

<td style="text-align:right;">

0.0150704

</td>

<td style="text-align:left;">

College graduate

</td>

</tr>

<tr>

<td style="text-align:left;">

Iowa

</td>

<td style="text-align:right;">

\-0.4248224

</td>

<td style="text-align:right;">

0.0051318

</td>

<td style="text-align:left;">

High school or less

</td>

</tr>

<tr>

<td style="text-align:left;">

Iowa

</td>

<td style="text-align:right;">

\-0.6555144

</td>

<td style="text-align:right;">

0.0755960

</td>

<td style="text-align:left;">

Postgraduate

</td>

</tr>

<tr>

<td style="text-align:left;">

Iowa

</td>

<td style="text-align:right;">

\-2.5977181

</td>

<td style="text-align:right;">

0.0101835

</td>

<td style="text-align:left;">

Some college

</td>

</tr>

<tr>

<td style="text-align:left;">

Kentucky

</td>

<td style="text-align:right;">

\-1.7917464

</td>

<td style="text-align:right;">

0.0142346

</td>

<td style="text-align:left;">

College graduate

</td>

</tr>

<tr>

<td style="text-align:left;">

Kentucky

</td>

<td style="text-align:right;">

\-0.2301632

</td>

<td style="text-align:right;">

0.0014072

</td>

<td style="text-align:left;">

High school or less

</td>

</tr>

<tr>

<td style="text-align:left;">

Kentucky

</td>

<td style="text-align:right;">

\-0.2667415

</td>

<td style="text-align:right;">

0.0264043

</td>

<td style="text-align:left;">

Postgraduate

</td>

</tr>

<tr>

<td style="text-align:left;">

Kentucky

</td>

<td style="text-align:right;">

\-1.7749679

</td>

<td style="text-align:right;">

0.0053705

</td>

<td style="text-align:left;">

Some college

</td>

</tr>

<tr>

<td style="text-align:left;">

Maine

</td>

<td style="text-align:right;">

\-3.0802997

</td>

<td style="text-align:right;">

0.0183473

</td>

<td style="text-align:left;">

College graduate

</td>

</tr>

<tr>

<td style="text-align:left;">

Maine

</td>

<td style="text-align:right;">

\-0.2473596

</td>

<td style="text-align:right;">

0.0074085

</td>

<td style="text-align:left;">

High school or less

</td>

</tr>

<tr>

<td style="text-align:left;">

Maine

</td>

<td style="text-align:right;">

\-0.8235900

</td>

<td style="text-align:right;">

0.0407690

</td>

<td style="text-align:left;">

Postgraduate

</td>

</tr>

<tr>

<td style="text-align:left;">

Maine

</td>

<td style="text-align:right;">

\-1.8142094

</td>

<td style="text-align:right;">

0.0154023

</td>

<td style="text-align:left;">

Some college

</td>

</tr>

<tr>

<td style="text-align:left;">

Michigan

</td>

<td style="text-align:right;">

\-3.2339711

</td>

<td style="text-align:right;">

0.0193335

</td>

<td style="text-align:left;">

College graduate

</td>

</tr>

<tr>

<td style="text-align:left;">

Michigan

</td>

<td style="text-align:right;">

\-0.2695681

</td>

<td style="text-align:right;">

0.0056064

</td>

<td style="text-align:left;">

High school or less

</td>

</tr>

<tr>

<td style="text-align:left;">

Michigan

</td>

<td style="text-align:right;">

\-0.5097904

</td>

<td style="text-align:right;">

0.0675539

</td>

<td style="text-align:left;">

Postgraduate

</td>

</tr>

<tr>

<td style="text-align:left;">

Michigan

</td>

<td style="text-align:right;">

\-2.4637958

</td>

<td style="text-align:right;">

0.0089357

</td>

<td style="text-align:left;">

Some college

</td>

</tr>

<tr>

<td style="text-align:left;">

Minnesota

</td>

<td style="text-align:right;">

\-4.2494033

</td>

<td style="text-align:right;">

0.0057913

</td>

<td style="text-align:left;">

College graduate

</td>

</tr>

<tr>

<td style="text-align:left;">

Minnesota

</td>

<td style="text-align:right;">

\-0.3394692

</td>

<td style="text-align:right;">

0.0072593

</td>

<td style="text-align:left;">

High school or less

</td>

</tr>

<tr>

<td style="text-align:left;">

Minnesota

</td>

<td style="text-align:right;">

\-0.8779563

</td>

<td style="text-align:right;">

0.0313508

</td>

<td style="text-align:left;">

Postgraduate

</td>

</tr>

<tr>

<td style="text-align:left;">

Minnesota

</td>

<td style="text-align:right;">

\-2.6897048

</td>

<td style="text-align:right;">

0.0064813

</td>

<td style="text-align:left;">

Some college

</td>

</tr>

<tr>

<td style="text-align:left;">

Missouri

</td>

<td style="text-align:right;">

\-2.7132638

</td>

<td style="text-align:right;">

0.0167136

</td>

<td style="text-align:left;">

College graduate

</td>

</tr>

<tr>

<td style="text-align:left;">

Missouri

</td>

<td style="text-align:right;">

\-0.0850333

</td>

<td style="text-align:right;">

0.0043558

</td>

<td style="text-align:left;">

High school or less

</td>

</tr>

<tr>

<td style="text-align:left;">

Missouri

</td>

<td style="text-align:right;">

\-0.4132111

</td>

<td style="text-align:right;">

0.0421526

</td>

<td style="text-align:left;">

Postgraduate

</td>

</tr>

<tr>

<td style="text-align:left;">

Missouri

</td>

<td style="text-align:right;">

\-1.9507140

</td>

<td style="text-align:right;">

0.0094967

</td>

<td style="text-align:left;">

Some college

</td>

</tr>

<tr>

<td style="text-align:left;">

Nevada

</td>

<td style="text-align:right;">

\-2.9946169

</td>

<td style="text-align:right;">

0.0158929

</td>

<td style="text-align:left;">

College graduate

</td>

</tr>

<tr>

<td style="text-align:left;">

Nevada

</td>

<td style="text-align:right;">

\-0.2269524

</td>

<td style="text-align:right;">

0.0030122

</td>

<td style="text-align:left;">

High school or less

</td>

</tr>

<tr>

<td style="text-align:left;">

Nevada

</td>

<td style="text-align:right;">

\-0.0701401

</td>

<td style="text-align:right;">

0.0667372

</td>

<td style="text-align:left;">

Postgraduate

</td>

</tr>

<tr>

<td style="text-align:left;">

Nevada

</td>

<td style="text-align:right;">

\-2.3467749

</td>

<td style="text-align:right;">

0.0064952

</td>

<td style="text-align:left;">

Some college

</td>

</tr>

<tr>

<td style="text-align:left;">

New Hampshire

</td>

<td style="text-align:right;">

\-4.1971225

</td>

<td style="text-align:right;">

0.0107975

</td>

<td style="text-align:left;">

College graduate

</td>

</tr>

<tr>

<td style="text-align:left;">

New Hampshire

</td>

<td style="text-align:right;">

\-0.4788045

</td>

<td style="text-align:right;">

0.0094677

</td>

<td style="text-align:left;">

High school or less

</td>

</tr>

<tr>

<td style="text-align:left;">

New Hampshire

</td>

<td style="text-align:right;">

\-0.8559753

</td>

<td style="text-align:right;">

0.0484646

</td>

<td style="text-align:left;">

Postgraduate

</td>

</tr>

<tr>

<td style="text-align:left;">

New Hampshire

</td>

<td style="text-align:right;">

\-2.5165930

</td>

<td style="text-align:right;">

0.0129428

</td>

<td style="text-align:left;">

Some college

</td>

</tr>

<tr>

<td style="text-align:left;">

New Jersey

</td>

<td style="text-align:right;">

\-4.0819715

</td>

<td style="text-align:right;">

0.0036186

</td>

<td style="text-align:left;">

College graduate

</td>

</tr>

<tr>

<td style="text-align:left;">

New Jersey

</td>

<td style="text-align:right;">

\-0.3686516

</td>

<td style="text-align:right;">

0.0031496

</td>

<td style="text-align:left;">

High school or less

</td>

</tr>

<tr>

<td style="text-align:left;">

New Jersey

</td>

<td style="text-align:right;">

\-3.7145390

</td>

<td style="text-align:right;">

0.0155420

</td>

<td style="text-align:left;">

Postgraduate

</td>

</tr>

<tr>

<td style="text-align:left;">

New Jersey

</td>

<td style="text-align:right;">

\-2.1348069

</td>

<td style="text-align:right;">

0.0061023

</td>

<td style="text-align:left;">

Some college

</td>

</tr>

<tr>

<td style="text-align:left;">

New Mexico

</td>

<td style="text-align:right;">

\-2.5073519

</td>

<td style="text-align:right;">

0.0160333

</td>

<td style="text-align:left;">

College graduate

</td>

</tr>

<tr>

<td style="text-align:left;">

New Mexico

</td>

<td style="text-align:right;">

0.0436729

</td>

<td style="text-align:right;">

0.0035557

</td>

<td style="text-align:left;">

High school or less

</td>

</tr>

<tr>

<td style="text-align:left;">

New Mexico

</td>

<td style="text-align:right;">

\-0.0198866

</td>

<td style="text-align:right;">

0.0372185

</td>

<td style="text-align:left;">

Postgraduate

</td>

</tr>

<tr>

<td style="text-align:left;">

New Mexico

</td>

<td style="text-align:right;">

\-2.0428231

</td>

<td style="text-align:right;">

0.0062576

</td>

<td style="text-align:left;">

Some college

</td>

</tr>

<tr>

<td style="text-align:left;">

New York

</td>

<td style="text-align:right;">

\-3.2802108

</td>

<td style="text-align:right;">

0.0058617

</td>

<td style="text-align:left;">

College graduate

</td>

</tr>

<tr>

<td style="text-align:left;">

New York

</td>

<td style="text-align:right;">

0.1293945

</td>

<td style="text-align:right;">

0.0029533

</td>

<td style="text-align:left;">

High school or less

</td>

</tr>

<tr>

<td style="text-align:left;">

New York

</td>

<td style="text-align:right;">

\-2.5420632

</td>

<td style="text-align:right;">

0.0209656

</td>

<td style="text-align:left;">

Postgraduate

</td>

</tr>

<tr>

<td style="text-align:left;">

New York

</td>

<td style="text-align:right;">

\-1.5369690

</td>

<td style="text-align:right;">

0.0077766

</td>

<td style="text-align:left;">

Some college

</td>

</tr>

<tr>

<td style="text-align:left;">

North Carolina

</td>

<td style="text-align:right;">

\-3.3244549

</td>

<td style="text-align:right;">

0.0249066

</td>

<td style="text-align:left;">

College graduate

</td>

</tr>

<tr>

<td style="text-align:left;">

North Carolina

</td>

<td style="text-align:right;">

\-0.2032084

</td>

<td style="text-align:right;">

0.0098564

</td>

<td style="text-align:left;">

High school or less

</td>

</tr>

<tr>

<td style="text-align:left;">

North Carolina

</td>

<td style="text-align:right;">

\-0.4877199

</td>

<td style="text-align:right;">

0.0932935

</td>

<td style="text-align:left;">

Postgraduate

</td>

</tr>

<tr>

<td style="text-align:left;">

North Carolina

</td>

<td style="text-align:right;">

\-2.1559519

</td>

<td style="text-align:right;">

0.0193872

</td>

<td style="text-align:left;">

Some college

</td>

</tr>

<tr>

<td style="text-align:left;">

Ohio

</td>

<td style="text-align:right;">

\-3.0958338

</td>

<td style="text-align:right;">

0.0248165

</td>

<td style="text-align:left;">

College graduate

</td>

</tr>

<tr>

<td style="text-align:left;">

Ohio

</td>

<td style="text-align:right;">

\-0.3598185

</td>

<td style="text-align:right;">

0.0054180

</td>

<td style="text-align:left;">

High school or less

</td>

</tr>

<tr>

<td style="text-align:left;">

Ohio

</td>

<td style="text-align:right;">

\-0.4655628

</td>

<td style="text-align:right;">

0.0774263

</td>

<td style="text-align:left;">

Postgraduate

</td>

</tr>

<tr>

<td style="text-align:left;">

Ohio

</td>

<td style="text-align:right;">

\-2.1030040

</td>

<td style="text-align:right;">

0.0150625

</td>

<td style="text-align:left;">

Some college

</td>

</tr>

<tr>

<td style="text-align:left;">

Oregon

</td>

<td style="text-align:right;">

\-2.7443023

</td>

<td style="text-align:right;">

0.0103692

</td>

<td style="text-align:left;">

College graduate

</td>

</tr>

<tr>

<td style="text-align:left;">

Oregon

</td>

<td style="text-align:right;">

0.3269813

</td>

<td style="text-align:right;">

0.0057078

</td>

<td style="text-align:left;">

High school or less

</td>

</tr>

<tr>

<td style="text-align:left;">

Oregon

</td>

<td style="text-align:right;">

\-0.5809006

</td>

<td style="text-align:right;">

0.0216264

</td>

<td style="text-align:left;">

Postgraduate

</td>

</tr>

<tr>

<td style="text-align:left;">

Oregon

</td>

<td style="text-align:right;">

\-2.0574347

</td>

<td style="text-align:right;">

0.0058027

</td>

<td style="text-align:left;">

Some college

</td>

</tr>

<tr>

<td style="text-align:left;">

Pennsylvania

</td>

<td style="text-align:right;">

\-2.7395006

</td>

<td style="text-align:right;">

0.0251842

</td>

<td style="text-align:left;">

College graduate

</td>

</tr>

<tr>

<td style="text-align:left;">

Pennsylvania

</td>

<td style="text-align:right;">

\-0.1630623

</td>

<td style="text-align:right;">

0.0056845

</td>

<td style="text-align:left;">

High school or less

</td>

</tr>

<tr>

<td style="text-align:left;">

Pennsylvania

</td>

<td style="text-align:right;">

\-0.4334794

</td>

<td style="text-align:right;">

0.0584403

</td>

<td style="text-align:left;">

Postgraduate

</td>

</tr>

<tr>

<td style="text-align:left;">

Pennsylvania

</td>

<td style="text-align:right;">

\-1.1140067

</td>

<td style="text-align:right;">

0.0261818

</td>

<td style="text-align:left;">

Some college

</td>

</tr>

<tr>

<td style="text-align:left;">

South Carolina

</td>

<td style="text-align:right;">

\-2.5041533

</td>

<td style="text-align:right;">

0.0074558

</td>

<td style="text-align:left;">

College graduate

</td>

</tr>

<tr>

<td style="text-align:left;">

South Carolina

</td>

<td style="text-align:right;">

0.0576706

</td>

<td style="text-align:right;">

0.0022024

</td>

<td style="text-align:left;">

High school or less

</td>

</tr>

<tr>

<td style="text-align:left;">

South Carolina

</td>

<td style="text-align:right;">

\-0.2451518

</td>

<td style="text-align:right;">

0.0204090

</td>

<td style="text-align:left;">

Postgraduate

</td>

</tr>

<tr>

<td style="text-align:left;">

South Carolina

</td>

<td style="text-align:right;">

\-1.6721323

</td>

<td style="text-align:right;">

0.0046641

</td>

<td style="text-align:left;">

Some college

</td>

</tr>

<tr>

<td style="text-align:left;">

Texas

</td>

<td style="text-align:right;">

\-3.1159160

</td>

<td style="text-align:right;">

0.0109253

</td>

<td style="text-align:left;">

College graduate

</td>

</tr>

<tr>

<td style="text-align:left;">

Texas

</td>

<td style="text-align:right;">

0.0161257

</td>

<td style="text-align:right;">

0.0035059

</td>

<td style="text-align:left;">

High school or less

</td>

</tr>

<tr>

<td style="text-align:left;">

Texas

</td>

<td style="text-align:right;">

0.1908018

</td>

<td style="text-align:right;">

0.0613847

</td>

<td style="text-align:left;">

Postgraduate

</td>

</tr>

<tr>

<td style="text-align:left;">

Texas

</td>

<td style="text-align:right;">

\-1.9615298

</td>

<td style="text-align:right;">

0.0078639

</td>

<td style="text-align:left;">

Some college

</td>

</tr>

<tr>

<td style="text-align:left;">

Utah

</td>

<td style="text-align:right;">

\-3.6836140

</td>

<td style="text-align:right;">

0.0040313

</td>

<td style="text-align:left;">

College graduate

</td>

</tr>

<tr>

<td style="text-align:left;">

Utah

</td>

<td style="text-align:right;">

0.0546513

</td>

<td style="text-align:right;">

0.0037791

</td>

<td style="text-align:left;">

High school or less

</td>

</tr>

<tr>

<td style="text-align:left;">

Utah

</td>

<td style="text-align:right;">

\-0.2158158

</td>

<td style="text-align:right;">

0.0249887

</td>

<td style="text-align:left;">

Postgraduate

</td>

</tr>

<tr>

<td style="text-align:left;">

Utah

</td>

<td style="text-align:right;">

\-2.6198522

</td>

<td style="text-align:right;">

0.0023246

</td>

<td style="text-align:left;">

Some college

</td>

</tr>

<tr>

<td style="text-align:left;">

Virginia

</td>

<td style="text-align:right;">

\-3.8098734

</td>

<td style="text-align:right;">

0.0116911

</td>

<td style="text-align:left;">

College graduate

</td>

</tr>

<tr>

<td style="text-align:left;">

Virginia

</td>

<td style="text-align:right;">

\-0.2205189

</td>

<td style="text-align:right;">

0.0080196

</td>

<td style="text-align:left;">

High school or less

</td>

</tr>

<tr>

<td style="text-align:left;">

Virginia

</td>

<td style="text-align:right;">

\-3.2802748

</td>

<td style="text-align:right;">

0.0382285

</td>

<td style="text-align:left;">

Postgraduate

</td>

</tr>

<tr>

<td style="text-align:left;">

Virginia

</td>

<td style="text-align:right;">

\-2.2595532

</td>

<td style="text-align:right;">

0.0122362

</td>

<td style="text-align:left;">

Some college

</td>

</tr>

<tr>

<td style="text-align:left;">

Washington

</td>

<td style="text-align:right;">

\-3.1204453

</td>

<td style="text-align:right;">

0.0061480

</td>

<td style="text-align:left;">

College graduate

</td>

</tr>

<tr>

<td style="text-align:left;">

Washington

</td>

<td style="text-align:right;">

0.4799200

</td>

<td style="text-align:right;">

0.0047745

</td>

<td style="text-align:left;">

High school or less

</td>

</tr>

<tr>

<td style="text-align:left;">

Washington

</td>

<td style="text-align:right;">

\-0.3509546

</td>

<td style="text-align:right;">

0.0204090

</td>

<td style="text-align:left;">

Postgraduate

</td>

</tr>

<tr>

<td style="text-align:left;">

Washington

</td>

<td style="text-align:right;">

\-1.8505398

</td>

<td style="text-align:right;">

0.0056368

</td>

<td style="text-align:left;">

Some college

</td>

</tr>

<tr>

<td style="text-align:left;">

Wisconsin

</td>

<td style="text-align:right;">

\-3.5838905

</td>

<td style="text-align:right;">

0.0179711

</td>

<td style="text-align:left;">

College graduate

</td>

</tr>

<tr>

<td style="text-align:left;">

Wisconsin

</td>

<td style="text-align:right;">

\-0.4001897

</td>

<td style="text-align:right;">

0.0065838

</td>

<td style="text-align:left;">

High school or less

</td>

</tr>

<tr>

<td style="text-align:left;">

Wisconsin

</td>

<td style="text-align:right;">

\-0.7001138

</td>

<td style="text-align:right;">

0.0731991

</td>

<td style="text-align:left;">

Postgraduate

</td>

</tr>

<tr>

<td style="text-align:left;">

Wisconsin

</td>

<td style="text-align:right;">

\-2.3706769

</td>

<td style="text-align:right;">

0.0131274

</td>

<td style="text-align:left;">

Some college

</td>

</tr>

</tbody>

</table>

Of course, we need to subtract out the `(1|education)` and `(1|state)`
coefficients to avoid double counting effects. This ensures we capture
the *state specific* quirks due to education level.
