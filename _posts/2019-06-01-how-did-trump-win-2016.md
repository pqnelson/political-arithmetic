  - [How did Trump Win the White
    House?](#how-did-trump-win-the-white-house)
  - [Testing Gelman’s Model](#testing-gelmans-model)
      - [Strategy](#strategy)
      - [Data Caveat: Exit Polls noisier than CPS
        estimates](#data-caveat-exit-polls-noisier-than-cps-estimates)
      - [Data Caveat 2: Exit Polls Limited in
        Questions](#data-caveat-2-exit-polls-limited-in-questions)
      - [Computational Details](#computational-details)
  - [Loading Data](#loading-data)
      - [Official Results](#official-results)
      - [Exit Poll Data](#exit-poll-data)
          - [Wilson Interval](#wilson-interval)
      - [Census Data](#census-data)
  - [Estimating Voter Turnout](#estimating-voter-turnout)
      - [State Effects](#state-effects)
          - [State and Gender](#state-and-gender)
      - [Age](#age)
      - [Ethnicity](#ethnicity)
          - [Ethnicity and Gender](#ethnicity-and-gender)
          - [Ethnicity and Age](#ethnicity-and-age)
      - [Education](#education)
          - [Overall Factor](#overall-factor)
          - [Interacting with State](#interacting-with-state)
      - [Prediction for States](#prediction-for-states)
  - [Estimating votes for Trump](#estimating-votes-for-trump)
      - [Predicting Trump Votes](#predicting-trump-votes)

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

## Data Caveat 2: Exit Polls Limited in Questions

We cannot approximate every coefficient in the logistic regression from
Gelman and friends. We have to restrict it to:

    logit(voted) ~  1 + female +
                    (1 | state) + (1 | state:gender) +
                    (1 | age) + (1 | state:age) + 
                    (1 | married) + (1 | married:state) + (1 | married:gender) + 
                    (1 | state:ethnicity) + (1 | ethnicity:gender) + (1 | ethnicity:age) +
                    (1 | education) + (1 | state:education)

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

The constant for logistic regressions is the log odds of the desired
result. For us, that’s the number of people who turned out to vote, over
the number of people who abstained. Using the [election project’s
estimates](http://www.electproject.org/2016g):

``` r
votes_for_highest_office <- 136700729.0
eligible_voters <- 230931921.0
beta_0 <- log(votes_for_highest_office/(eligible_voters - votes_for_highest_office))
turnout_beta_0 <- function() {
  log(votes_for_highest_office/(eligible_voters - votes_for_highest_office))
}
```

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
              beta_state = log(proportion/(1 - proportion)) - beta_0) %>%
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

\-0.3138725

</td>

</tr>

<tr>

<td style="text-align:left;">

California

</td>

<td style="text-align:right;">

\-0.4500614

</td>

</tr>

<tr>

<td style="text-align:left;">

Colorado

</td>

<td style="text-align:right;">

0.3631347

</td>

</tr>

<tr>

<td style="text-align:left;">

Florida

</td>

<td style="text-align:right;">

0.0070053

</td>

</tr>

<tr>

<td style="text-align:left;">

Georgia

</td>

<td style="text-align:right;">

\-0.2072138

</td>

</tr>

<tr>

<td style="text-align:left;">

Illinois

</td>

<td style="text-align:right;">

\-0.1158382

</td>

</tr>

<tr>

<td style="text-align:left;">

Indiana

</td>

<td style="text-align:right;">

\-0.1868636

</td>

</tr>

<tr>

<td style="text-align:left;">

Iowa

</td>

<td style="text-align:right;">

0.2834716

</td>

</tr>

<tr>

<td style="text-align:left;">

Kentucky

</td>

<td style="text-align:right;">

\-0.1053013

</td>

</tr>

<tr>

<td style="text-align:left;">

Maine

</td>

<td style="text-align:right;">

0.4515472

</td>

</tr>

<tr>

<td style="text-align:left;">

Michigan

</td>

<td style="text-align:right;">

0.1377476

</td>

</tr>

<tr>

<td style="text-align:left;">

Minnesota

</td>

<td style="text-align:right;">

0.5059134

</td>

</tr>

<tr>

<td style="text-align:left;">

Missouri

</td>

<td style="text-align:right;">

0.0411683

</td>

</tr>

<tr>

<td style="text-align:left;">

Nevada

</td>

<td style="text-align:right;">

\-0.3019027

</td>

</tr>

<tr>

<td style="text-align:left;">

New Hampshire

</td>

<td style="text-align:right;">

0.4839325

</td>

</tr>

<tr>

<td style="text-align:left;">

New Jersey

</td>

<td style="text-align:right;">

\-0.1268329

</td>

</tr>

<tr>

<td style="text-align:left;">

New Mexico

</td>

<td style="text-align:right;">

\-0.3521563

</td>

</tr>

<tr>

<td style="text-align:left;">

New York

</td>

<td style="text-align:right;">

\-0.3793238

</td>

</tr>

<tr>

<td style="text-align:left;">

North Carolina

</td>

<td style="text-align:right;">

0.1156771

</td>

</tr>

<tr>

<td style="text-align:left;">

Ohio

</td>

<td style="text-align:right;">

0.0935199

</td>

</tr>

<tr>

<td style="text-align:left;">

Oregon

</td>

<td style="text-align:right;">

0.2088578

</td>

</tr>

<tr>

<td style="text-align:left;">

Pennsylvania

</td>

<td style="text-align:right;">

0.0614366

</td>

</tr>

<tr>

<td style="text-align:left;">

South
Carolina

</td>

<td style="text-align:right;">

\-0.1268911

</td>

</tr>

<tr>

<td style="text-align:left;">

Texas

</td>

<td style="text-align:right;">

\-0.5628446

</td>

</tr>

<tr>

<td style="text-align:left;">

Utah

</td>

<td style="text-align:right;">

\-0.1562270

</td>

</tr>

<tr>

<td style="text-align:left;">

Virginia

</td>

<td style="text-align:right;">

0.1101942

</td>

</tr>

<tr>

<td style="text-align:left;">

Washington

</td>

<td style="text-align:right;">

\-0.0210882

</td>

</tr>

<tr>

<td style="text-align:left;">

Wisconsin

</td>

<td style="text-align:right;">

0.3280710

</td>

</tr>

</tbody>

</table>

### State and Gender

``` r
turnout_state_gender_log_coefs <- function(exit_poll, election_data, census_data, z=2) {
  state_effect <- turnout_state_log_coefs(election_data, census_data)
  exit_poll %>%
    filter(questions=="Gender", state != "nation") %>%
    group_by(state,options) %>%
    transmute(pop = sum(census_data[which(census_data$state %in% state),c(tolower(options))]),
              center = wilson_center(0.01*options_perc, num_respondents, z),
              width = wilson_width(0.01*options_perc, num_respondents, z),
              voters = (center - 2*width)*sum(election_data[which(election_data$state %in% state),]$totalvotes),
              proportion = voters/pop) %>%
    transmute(beta_state_gender = log(proportion/(1 - proportion)) - (state_effect[which(state_effect$state %in% state),]$beta_state - beta_0) - beta_0,
              tau_state_gender = 2*(2*width/center)**-2
              )
}
```

``` r
kable(turnout_state_gender_log_coefs(exit_poll_df, filter(election_2016, party=="democrat"), census_data))
```

<table>

<thead>

<tr>

<th style="text-align:left;">

state

</th>

<th style="text-align:left;">

options

</th>

<th style="text-align:right;">

beta\_state\_gender

</th>

<th style="text-align:right;">

tau\_state\_gender

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Arizona

</td>

<td style="text-align:left;">

Male

</td>

<td style="text-align:right;">

0.1594664

</td>

<td style="text-align:right;">

208.14932

</td>

</tr>

<tr>

<td style="text-align:left;">

Arizona

</td>

<td style="text-align:left;">

Female

</td>

<td style="text-align:right;">

0.1882624

</td>

<td style="text-align:right;">

225.44622

</td>

</tr>

<tr>

<td style="text-align:left;">

California

</td>

<td style="text-align:left;">

Male

</td>

<td style="text-align:right;">

0.2046666

</td>

<td style="text-align:right;">

297.02187

</td>

</tr>

<tr>

<td style="text-align:left;">

California

</td>

<td style="text-align:left;">

Female

</td>

<td style="text-align:right;">

0.2278798

</td>

<td style="text-align:right;">

321.72173

</td>

</tr>

<tr>

<td style="text-align:left;">

Colorado

</td>

<td style="text-align:left;">

Male

</td>

<td style="text-align:right;">

0.0001559

</td>

<td style="text-align:right;">

160.83069

</td>

</tr>

<tr>

<td style="text-align:left;">

Colorado

</td>

<td style="text-align:left;">

Female

</td>

<td style="text-align:right;">

0.1020543

</td>

<td style="text-align:right;">

174.18602

</td>

</tr>

<tr>

<td style="text-align:left;">

Florida

</td>

<td style="text-align:left;">

Male

</td>

<td style="text-align:right;">

0.1489326

</td>

<td style="text-align:right;">

443.56208

</td>

</tr>

<tr>

<td style="text-align:left;">

Florida

</td>

<td style="text-align:left;">

Female

</td>

<td style="text-align:right;">

0.2842365

</td>

<td style="text-align:right;">

563.90488

</td>

</tr>

<tr>

<td style="text-align:left;">

Georgia

</td>

<td style="text-align:left;">

Male

</td>

<td style="text-align:right;">

0.0653316

</td>

<td style="text-align:right;">

283.48451

</td>

</tr>

<tr>

<td style="text-align:left;">

Georgia

</td>

<td style="text-align:left;">

Female

</td>

<td style="text-align:right;">

0.3398043

</td>

<td style="text-align:right;">

423.22995

</td>

</tr>

<tr>

<td style="text-align:left;">

Illinois

</td>

<td style="text-align:left;">

Male

</td>

<td style="text-align:right;">

0.1328766

</td>

<td style="text-align:right;">

115.00000

</td>

</tr>

<tr>

<td style="text-align:left;">

Illinois

</td>

<td style="text-align:left;">

Female

</td>

<td style="text-align:right;">

0.0170518

</td>

<td style="text-align:right;">

115.00000

</td>

</tr>

<tr>

<td style="text-align:left;">

Indiana

</td>

<td style="text-align:left;">

Male

</td>

<td style="text-align:right;">

0.1764676

</td>

<td style="text-align:right;">

218.71795

</td>

</tr>

<tr>

<td style="text-align:left;">

Indiana

</td>

<td style="text-align:left;">

Female

</td>

<td style="text-align:right;">

0.1575887

</td>

<td style="text-align:right;">

236.89520

</td>

</tr>

<tr>

<td style="text-align:left;">

Iowa

</td>

<td style="text-align:left;">

Male

</td>

<td style="text-align:right;">

0.0392679

</td>

<td style="text-align:right;">

329.94180

</td>

</tr>

<tr>

<td style="text-align:left;">

Iowa

</td>

<td style="text-align:left;">

Female

</td>

<td style="text-align:right;">

0.2884209

</td>

<td style="text-align:right;">

419.42350

</td>

</tr>

<tr>

<td style="text-align:left;">

Kentucky

</td>

<td style="text-align:left;">

Male

</td>

<td style="text-align:right;">

0.1069016

</td>

<td style="text-align:right;">

132.48755

</td>

</tr>

<tr>

<td style="text-align:left;">

Kentucky

</td>

<td style="text-align:left;">

Female

</td>

<td style="text-align:right;">

0.0890012

</td>

<td style="text-align:right;">

143.48194

</td>

</tr>

<tr>

<td style="text-align:left;">

Maine

</td>

<td style="text-align:left;">

Male

</td>

<td style="text-align:right;">

0.0687753

</td>

<td style="text-align:right;">

246.03772

</td>

</tr>

<tr>

<td style="text-align:left;">

Maine

</td>

<td style="text-align:left;">

Female

</td>

<td style="text-align:right;">

0.1324223

</td>

<td style="text-align:right;">

288.66580

</td>

</tr>

<tr>

<td style="text-align:left;">

Michigan

</td>

<td style="text-align:left;">

Male

</td>

<td style="text-align:right;">

0.1409450

</td>

<td style="text-align:right;">

324.96080

</td>

</tr>

<tr>

<td style="text-align:left;">

Michigan

</td>

<td style="text-align:left;">

Female

</td>

<td style="text-align:right;">

0.2084765

</td>

<td style="text-align:right;">

381.29080

</td>

</tr>

<tr>

<td style="text-align:left;">

Minnesota

</td>

<td style="text-align:left;">

Male

</td>

<td style="text-align:right;">

\-0.0877652

</td>

<td style="text-align:right;">

181.84746

</td>

</tr>

<tr>

<td style="text-align:left;">

Minnesota

</td>

<td style="text-align:left;">

Female

</td>

<td style="text-align:right;">

0.2000877

</td>

<td style="text-align:right;">

231.10435

</td>

</tr>

<tr>

<td style="text-align:left;">

Missouri

</td>

<td style="text-align:left;">

Male

</td>

<td style="text-align:right;">

0.0707141

</td>

<td style="text-align:right;">

215.65642

</td>

</tr>

<tr>

<td style="text-align:left;">

Missouri

</td>

<td style="text-align:left;">

Female

</td>

<td style="text-align:right;">

0.2227925

</td>

<td style="text-align:right;">

274.09637

</td>

</tr>

<tr>

<td style="text-align:left;">

Nevada

</td>

<td style="text-align:left;">

Male

</td>

<td style="text-align:right;">

0.1331367

</td>

<td style="text-align:right;">

321.03772

</td>

</tr>

<tr>

<td style="text-align:left;">

Nevada

</td>

<td style="text-align:left;">

Female

</td>

<td style="text-align:right;">

0.2966651

</td>

<td style="text-align:right;">

376.68663

</td>

</tr>

<tr>

<td style="text-align:left;">

New Hampshire

</td>

<td style="text-align:left;">

Male

</td>

<td style="text-align:right;">

0.0601177

</td>

<td style="text-align:right;">

323.57618

</td>

</tr>

<tr>

<td style="text-align:left;">

New Hampshire

</td>

<td style="text-align:left;">

Female

</td>

<td style="text-align:right;">

0.1987781

</td>

<td style="text-align:right;">

379.66580

</td>

</tr>

<tr>

<td style="text-align:left;">

New Jersey

</td>

<td style="text-align:left;">

Male

</td>

<td style="text-align:right;">

0.1340733

</td>

<td style="text-align:right;">

188.92234

</td>

</tr>

<tr>

<td style="text-align:left;">

New Jersey

</td>

<td style="text-align:left;">

Female

</td>

<td style="text-align:right;">

0.1631525

</td>

<td style="text-align:right;">

221.63455

</td>

</tr>

<tr>

<td style="text-align:left;">

New Mexico

</td>

<td style="text-align:left;">

Male

</td>

<td style="text-align:right;">

0.1043564

</td>

<td style="text-align:right;">

223.74840

</td>

</tr>

<tr>

<td style="text-align:left;">

New Mexico

</td>

<td style="text-align:left;">

Female

</td>

<td style="text-align:right;">

0.2760533

</td>

<td style="text-align:right;">

284.38626

</td>

</tr>

<tr>

<td style="text-align:left;">

New York

</td>

<td style="text-align:left;">

Male

</td>

<td style="text-align:right;">

\-0.0061579

</td>

<td style="text-align:right;">

144.47642

</td>

</tr>

<tr>

<td style="text-align:left;">

New York

</td>

<td style="text-align:left;">

Female

</td>

<td style="text-align:right;">

0.3186148

</td>

<td style="text-align:right;">

233.71800

</td>

</tr>

<tr>

<td style="text-align:left;">

North Carolina

</td>

<td style="text-align:left;">

Male

</td>

<td style="text-align:right;">

0.1070792

</td>

<td style="text-align:right;">

458.04818

</td>

</tr>

<tr>

<td style="text-align:left;">

North Carolina

</td>

<td style="text-align:left;">

Female

</td>

<td style="text-align:right;">

0.3158277

</td>

<td style="text-align:right;">

631.03427

</td>

</tr>

<tr>

<td style="text-align:left;">

Ohio

</td>

<td style="text-align:left;">

Male

</td>

<td style="text-align:right;">

0.1236893

</td>

<td style="text-align:right;">

377.05265

</td>

</tr>

<tr>

<td style="text-align:left;">

Ohio

</td>

<td style="text-align:left;">

Female

</td>

<td style="text-align:right;">

0.2674336

</td>

<td style="text-align:right;">

479.33041

</td>

</tr>

<tr>

<td style="text-align:left;">

Oregon

</td>

<td style="text-align:left;">

Male

</td>

<td style="text-align:right;">

0.0449271

</td>

<td style="text-align:right;">

135.97040

</td>

</tr>

<tr>

<td style="text-align:left;">

Oregon

</td>

<td style="text-align:left;">

Female

</td>

<td style="text-align:right;">

0.0604343

</td>

<td style="text-align:right;">

147.25489

</td>

</tr>

<tr>

<td style="text-align:left;">

Pennsylvania

</td>

<td style="text-align:left;">

Male

</td>

<td style="text-align:right;">

0.1150011

</td>

<td style="text-align:right;">

325.84038

</td>

</tr>

<tr>

<td style="text-align:left;">

Pennsylvania

</td>

<td style="text-align:left;">

Female

</td>

<td style="text-align:right;">

0.2566976

</td>

<td style="text-align:right;">

414.20807

</td>

</tr>

<tr>

<td style="text-align:left;">

South Carolina

</td>

<td style="text-align:left;">

Male

</td>

<td style="text-align:right;">

\-0.0298484

</td>

<td style="text-align:right;">

95.79819

</td>

</tr>

<tr>

<td style="text-align:left;">

South Carolina

</td>

<td style="text-align:left;">

Female

</td>

<td style="text-align:right;">

0.1662232

</td>

<td style="text-align:right;">

131.82776

</td>

</tr>

<tr>

<td style="text-align:left;">

Texas

</td>

<td style="text-align:left;">

Male

</td>

<td style="text-align:right;">

0.1502585

</td>

<td style="text-align:right;">

313.86868

</td>

</tr>

<tr>

<td style="text-align:left;">

Texas

</td>

<td style="text-align:left;">

Female

</td>

<td style="text-align:right;">

0.3135671

</td>

<td style="text-align:right;">

398.98467

</td>

</tr>

<tr>

<td style="text-align:left;">

Utah

</td>

<td style="text-align:left;">

Male

</td>

<td style="text-align:right;">

0.0342078

</td>

<td style="text-align:right;">

139.30696

</td>

</tr>

<tr>

<td style="text-align:left;">

Utah

</td>

<td style="text-align:left;">

Female

</td>

<td style="text-align:right;">

0.1978831

</td>

<td style="text-align:right;">

163.40538

</td>

</tr>

<tr>

<td style="text-align:left;">

Virginia

</td>

<td style="text-align:left;">

Male

</td>

<td style="text-align:right;">

0.0943759

</td>

<td style="text-align:right;">

326.61632

</td>

</tr>

<tr>

<td style="text-align:left;">

Virginia

</td>

<td style="text-align:left;">

Female

</td>

<td style="text-align:right;">

0.2680466

</td>

<td style="text-align:right;">

415.19477

</td>

</tr>

<tr>

<td style="text-align:left;">

Washington

</td>

<td style="text-align:left;">

Male

</td>

<td style="text-align:right;">

\-0.0929075

</td>

<td style="text-align:right;">

109.53430

</td>

</tr>

<tr>

<td style="text-align:left;">

Washington

</td>

<td style="text-align:left;">

Female

</td>

<td style="text-align:right;">

0.2436805

</td>

<td style="text-align:right;">

150.75710

</td>

</tr>

<tr>

<td style="text-align:left;">

Wisconsin

</td>

<td style="text-align:left;">

Male

</td>

<td style="text-align:right;">

0.1428081

</td>

<td style="text-align:right;">

366.43853

</td>

</tr>

<tr>

<td style="text-align:left;">

Wisconsin

</td>

<td style="text-align:left;">

Female

</td>

<td style="text-align:right;">

0.1782785

</td>

<td style="text-align:right;">

396.92071

</td>

</tr>

</tbody>

</table>

## Age

We have two coefficients for age `(1 | age)` and `(1 | state:age)`. We
try to keep our code DRY, since a lot of the logic is
reused.

``` r
turnout_age_log_coefs <- function(exit_poll, election_data, census_data, state_iter="nation", z = 2) {
  lookup <- list("18-24" = c("age_18_19", "age_20", "age_21", "age_22_24"),
                 "25-29" = c("age_25_29"),
                 "30-39" = c("age_30_34", "age_35_39"),
                 "40-49" = c("age_40_44", "age_45_49"),
                 "50-64" = c("age_50_54", "age_55_59", "age_60_61", "age_62_64"),
                 "65 and older" = c("age_retirees"))
  id <- sort(unique(filter(exit_poll, state == state_iter, questions=="Age")$questions_id))[2]
  
  state_filter <- state_iter;
  if (state_iter == "nation") {
    state_filter <- unique(exit_poll$state);
  }
  
  exit_poll %>%
    filter(state == state_iter, questions_id == id) %>%
    group_by(options) %>%
    transmute(k = lookup[options],
              center = wilson_center(0.01*options_perc, num_respondents, z),
              width =  wilson_width(0.01*options_perc, num_respondents, z),
              turnout = (center - 2*width)*sum(filter(election_data, state %in% state_filter)$totalvotes),
              pop = sum(filter(census_data, state %in% state_filter)[, unlist(k)]),
              proportion = turnout/pop
    ) %>%
    transmute(beta_age = log(proportion/(1 - proportion)) - beta_0,
              tau_age = 2*(2*width/center)**-2,
              state = state_iter) %>%
    as.data.frame
}
```

Now we iterate over all the states, accumulating all the
results.

``` r
turnout_state_age_log_coefs <- function(exit_poll, election_data, census_data, z = 2) {
  state_effects <- turnout_state_log_coefs(filter(election_2016,party=="democrat"), census_data)
  df <- data.frame();
  for (state_iter in unique(exit_poll$state)) {
    if (state_iter != "nation") {
      state_results <- turnout_age_log_coefs(exit_poll, election_data, census_data, state_iter, z);
      state_results$beta_age <- state_results$beta_age - (state_effects[which(state_effects$state == state_iter),]$beta_state - beta_0);
      df <- rbind(df, state_results);
    }
  }
  df
}
```

## Ethnicity

Computing `(1 | state:ethnicity)` requires doing this piece-wise, by
each
state.

``` r
ethnicity_by_state <- function(state_iter, exit_poll_by_state, election_data, census_data, z=2) {
  lookup <- list("White" = c("white_male", "white_female"),
                 "Black" = c("black_male", "black_female"),
                 "Latino" = c("hispanic_male", "hispanic_female"),
                 "Asian" = c("asian_male", "asian_female"),
                 "Other race" = c("others"))
  
  id <- min(filter(exit_poll_by_state,questions=="Race")$questions_id)
  state_effect <<- turnout_state_log_coefs(filter(election_data, state==state_iter), 
                                           filter(census_data, state == state_iter))

  exit_poll_by_state %>%
    filter(questions_id == id) %>%
    group_by(options) %>%
    transmute(k = lookup[options],
              estimates = (0.01*options_perc - wilson_width(0.01*options_perc, num_respondents, z))*sum(election_data[which(election_data$state %in% state_iter),]$totalvotes),
              pop = sum(census_data[which(census_data$state %in% state_iter), unlist(k)]),
              proportion = min(estimates/pop, 0.99)) %>% 
    transmute(state = state_iter,
              beta_ethnicity_by_state = log(proportion/(1 - proportion)) -
                (state_effect[which(state_effect$state %in% state_iter),]$beta_state - beta_0) -
                beta_0)%>%
    as.data.frame()
}
```

Alas, I could not get a slicker way to combine the results together to
work, at least not as
expected.

``` r
turnout_ethnicity_state_log_coefs <- function(exit_poll, election_data, census_data, z=2) {
  results = c()
  for (state_iter in unique(exit_poll$state)) {
    if (state_iter != "nation") {
      state_data <- ethnicity_by_state(state_iter, filter(exit_poll, state==state_iter), election_data, census_data, z)
      results <- rbind(results,state_data)
    }
  }
  results
}
```

``` r
kable(turnout_ethnicity_state_log_coefs(exit_poll_df, filter(election_2016, party=="democrat"), census_data))
```

<table>

<thead>

<tr>

<th style="text-align:left;">

options

</th>

<th style="text-align:left;">

state

</th>

<th style="text-align:right;">

beta\_ethnicity\_by\_state

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

White

</td>

<td style="text-align:left;">

Arizona

</td>

<td style="text-align:right;">

0.8294261

</td>

</tr>

<tr>

<td style="text-align:left;">

Black

</td>

<td style="text-align:left;">

Arizona

</td>

<td style="text-align:right;">

\-1.3034084

</td>

</tr>

<tr>

<td style="text-align:left;">

Latino

</td>

<td style="text-align:left;">

Arizona

</td>

<td style="text-align:right;">

\-0.7377429

</td>

</tr>

<tr>

<td style="text-align:left;">

Asian

</td>

<td style="text-align:left;">

Arizona

</td>

<td style="text-align:right;">

\-0.9908577

</td>

</tr>

<tr>

<td style="text-align:left;">

Other race

</td>

<td style="text-align:left;">

Arizona

</td>

<td style="text-align:right;">

\-0.6539098

</td>

</tr>

<tr>

<td style="text-align:left;">

White

</td>

<td style="text-align:left;">

California

</td>

<td style="text-align:right;">

0.5972545

</td>

</tr>

<tr>

<td style="text-align:left;">

Black

</td>

<td style="text-align:left;">

California

</td>

<td style="text-align:right;">

0.0807952

</td>

</tr>

<tr>

<td style="text-align:left;">

Latino

</td>

<td style="text-align:left;">

California

</td>

<td style="text-align:right;">

0.0715229

</td>

</tr>

<tr>

<td style="text-align:left;">

Asian

</td>

<td style="text-align:left;">

California

</td>

<td style="text-align:right;">

\-0.1724401

</td>

</tr>

<tr>

<td style="text-align:left;">

Other race

</td>

<td style="text-align:left;">

California

</td>

<td style="text-align:right;">

\-2.1355046

</td>

</tr>

<tr>

<td style="text-align:left;">

White

</td>

<td style="text-align:left;">

Colorado

</td>

<td style="text-align:right;">

0.5591674

</td>

</tr>

<tr>

<td style="text-align:left;">

Black

</td>

<td style="text-align:left;">

Colorado

</td>

<td style="text-align:right;">

\-0.3541654

</td>

</tr>

<tr>

<td style="text-align:left;">

Latino

</td>

<td style="text-align:left;">

Colorado

</td>

<td style="text-align:right;">

\-0.8473370

</td>

</tr>

<tr>

<td style="text-align:left;">

Asian

</td>

<td style="text-align:left;">

Colorado

</td>

<td style="text-align:right;">

\-0.4980359

</td>

</tr>

<tr>

<td style="text-align:left;">

Other race

</td>

<td style="text-align:left;">

Colorado

</td>

<td style="text-align:right;">

\-0.4074182

</td>

</tr>

<tr>

<td style="text-align:left;">

White

</td>

<td style="text-align:left;">

Florida

</td>

<td style="text-align:right;">

0.5167413

</td>

</tr>

<tr>

<td style="text-align:left;">

Black

</td>

<td style="text-align:left;">

Florida

</td>

<td style="text-align:right;">

0.0540924

</td>

</tr>

<tr>

<td style="text-align:left;">

Latino

</td>

<td style="text-align:left;">

Florida

</td>

<td style="text-align:right;">

\-0.2530922

</td>

</tr>

<tr>

<td style="text-align:left;">

Asian

</td>

<td style="text-align:left;">

Florida

</td>

<td style="text-align:right;">

\-0.6276510

</td>

</tr>

<tr>

<td style="text-align:left;">

Other race

</td>

<td style="text-align:left;">

Florida

</td>

<td style="text-align:right;">

0.7338685

</td>

</tr>

<tr>

<td style="text-align:left;">

White

</td>

<td style="text-align:left;">

Georgia

</td>

<td style="text-align:right;">

0.4460487

</td>

</tr>

<tr>

<td style="text-align:left;">

Black

</td>

<td style="text-align:left;">

Georgia

</td>

<td style="text-align:right;">

0.2314130

</td>

</tr>

<tr>

<td style="text-align:left;">

Latino

</td>

<td style="text-align:left;">

Georgia

</td>

<td style="text-align:right;">

\-1.0027545

</td>

</tr>

<tr>

<td style="text-align:left;">

Asian

</td>

<td style="text-align:left;">

Georgia

</td>

<td style="text-align:right;">

\-0.4693545

</td>

</tr>

<tr>

<td style="text-align:left;">

Other race

</td>

<td style="text-align:left;">

Georgia

</td>

<td style="text-align:right;">

\-1.2224203

</td>

</tr>

<tr>

<td style="text-align:left;">

White

</td>

<td style="text-align:left;">

Illinois

</td>

<td style="text-align:right;">

0.4079921

</td>

</tr>

<tr>

<td style="text-align:left;">

Black

</td>

<td style="text-align:left;">

Illinois

</td>

<td style="text-align:right;">

0.3287592

</td>

</tr>

<tr>

<td style="text-align:left;">

Latino

</td>

<td style="text-align:left;">

Illinois

</td>

<td style="text-align:right;">

\-0.4971198

</td>

</tr>

<tr>

<td style="text-align:left;">

Asian

</td>

<td style="text-align:left;">

Illinois

</td>

<td style="text-align:right;">

\-1.2759083

</td>

</tr>

<tr>

<td style="text-align:left;">

Other race

</td>

<td style="text-align:left;">

Illinois

</td>

<td style="text-align:right;">

\-1.9990198

</td>

</tr>

<tr>

<td style="text-align:left;">

White

</td>

<td style="text-align:left;">

Indiana

</td>

<td style="text-align:right;">

0.3742707

</td>

</tr>

<tr>

<td style="text-align:left;">

Black

</td>

<td style="text-align:left;">

Indiana

</td>

<td style="text-align:right;">

0.1194478

</td>

</tr>

<tr>

<td style="text-align:left;">

Latino

</td>

<td style="text-align:left;">

Indiana

</td>

<td style="text-align:right;">

\-0.1711357

</td>

</tr>

<tr>

<td style="text-align:left;">

Asian

</td>

<td style="text-align:left;">

Indiana

</td>

<td style="text-align:right;">

\-1.6084947

</td>

</tr>

<tr>

<td style="text-align:left;">

Other race

</td>

<td style="text-align:left;">

Indiana

</td>

<td style="text-align:right;">

\-0.5717536

</td>

</tr>

<tr>

<td style="text-align:left;">

White

</td>

<td style="text-align:left;">

Iowa

</td>

<td style="text-align:right;">

0.4296161

</td>

</tr>

<tr>

<td style="text-align:left;">

Black

</td>

<td style="text-align:left;">

Iowa

</td>

<td style="text-align:right;">

\-0.9591224

</td>

</tr>

<tr>

<td style="text-align:left;">

Latino

</td>

<td style="text-align:left;">

Iowa

</td>

<td style="text-align:right;">

0.2903177

</td>

</tr>

<tr>

<td style="text-align:left;">

Asian

</td>

<td style="text-align:left;">

Iowa

</td>

<td style="text-align:right;">

\-1.6988187

</td>

</tr>

<tr>

<td style="text-align:left;">

Other race

</td>

<td style="text-align:left;">

Iowa

</td>

<td style="text-align:right;">

\-0.7331453

</td>

</tr>

<tr>

<td style="text-align:left;">

White

</td>

<td style="text-align:left;">

Kentucky

</td>

<td style="text-align:right;">

0.3804439

</td>

</tr>

<tr>

<td style="text-align:left;">

Black

</td>

<td style="text-align:left;">

Kentucky

</td>

<td style="text-align:right;">

\-0.0118837

</td>

</tr>

<tr>

<td style="text-align:left;">

Latino

</td>

<td style="text-align:left;">

Kentucky

</td>

<td style="text-align:right;">

\-0.2276995

</td>

</tr>

<tr>

<td style="text-align:left;">

Asian

</td>

<td style="text-align:left;">

Kentucky

</td>

<td style="text-align:right;">

\-1.4817729

</td>

</tr>

<tr>

<td style="text-align:left;">

Other race

</td>

<td style="text-align:left;">

Kentucky

</td>

<td style="text-align:right;">

\-1.7058569

</td>

</tr>

<tr>

<td style="text-align:left;">

White

</td>

<td style="text-align:left;">

Maine

</td>

<td style="text-align:right;">

0.3327562

</td>

</tr>

<tr>

<td style="text-align:left;">

Black

</td>

<td style="text-align:left;">

Maine

</td>

<td style="text-align:right;">

\-0.8472464

</td>

</tr>

<tr>

<td style="text-align:left;">

Latino

</td>

<td style="text-align:left;">

Maine

</td>

<td style="text-align:right;">

0.8424335

</td>

</tr>

<tr>

<td style="text-align:left;">

Asian

</td>

<td style="text-align:left;">

Maine

</td>

<td style="text-align:right;">

\-0.9843119

</td>

</tr>

<tr>

<td style="text-align:left;">

Other race

</td>

<td style="text-align:left;">

Maine

</td>

<td style="text-align:right;">

\-0.0993408

</td>

</tr>

<tr>

<td style="text-align:left;">

White

</td>

<td style="text-align:left;">

Michigan

</td>

<td style="text-align:right;">

0.2640795

</td>

</tr>

<tr>

<td style="text-align:left;">

Black

</td>

<td style="text-align:left;">

Michigan

</td>

<td style="text-align:right;">

0.4492228

</td>

</tr>

<tr>

<td style="text-align:left;">

Latino

</td>

<td style="text-align:left;">

Michigan

</td>

<td style="text-align:right;">

0.5951546

</td>

</tr>

<tr>

<td style="text-align:left;">

Asian

</td>

<td style="text-align:left;">

Michigan

</td>

<td style="text-align:right;">

\-0.8375093

</td>

</tr>

<tr>

<td style="text-align:left;">

Other race

</td>

<td style="text-align:left;">

Michigan

</td>

<td style="text-align:right;">

\-0.3601128

</td>

</tr>

<tr>

<td style="text-align:left;">

White

</td>

<td style="text-align:left;">

Minnesota

</td>

<td style="text-align:right;">

0.4788521

</td>

</tr>

<tr>

<td style="text-align:left;">

Black

</td>

<td style="text-align:left;">

Minnesota

</td>

<td style="text-align:right;">

\-0.7639794

</td>

</tr>

<tr>

<td style="text-align:left;">

Latino

</td>

<td style="text-align:left;">

Minnesota

</td>

<td style="text-align:right;">

\-0.3665517

</td>

</tr>

<tr>

<td style="text-align:left;">

Asian

</td>

<td style="text-align:left;">

Minnesota

</td>

<td style="text-align:right;">

\-1.7824715

</td>

</tr>

<tr>

<td style="text-align:left;">

Other race

</td>

<td style="text-align:left;">

Minnesota

</td>

<td style="text-align:right;">

\-1.3821950

</td>

</tr>

<tr>

<td style="text-align:left;">

White

</td>

<td style="text-align:left;">

Missouri

</td>

<td style="text-align:right;">

0.2688492

</td>

</tr>

<tr>

<td style="text-align:left;">

Black

</td>

<td style="text-align:left;">

Missouri

</td>

<td style="text-align:right;">

0.7149143

</td>

</tr>

<tr>

<td style="text-align:left;">

Latino

</td>

<td style="text-align:left;">

Missouri

</td>

<td style="text-align:right;">

0.3030612

</td>

</tr>

<tr>

<td style="text-align:left;">

Asian

</td>

<td style="text-align:left;">

Missouri

</td>

<td style="text-align:right;">

\-1.5650174

</td>

</tr>

<tr>

<td style="text-align:left;">

Other race

</td>

<td style="text-align:left;">

Missouri

</td>

<td style="text-align:right;">

0.2431962

</td>

</tr>

<tr>

<td style="text-align:left;">

White

</td>

<td style="text-align:left;">

Nevada

</td>

<td style="text-align:right;">

0.5759662

</td>

</tr>

<tr>

<td style="text-align:left;">

Black

</td>

<td style="text-align:left;">

Nevada

</td>

<td style="text-align:right;">

0.2812411

</td>

</tr>

<tr>

<td style="text-align:left;">

Latino

</td>

<td style="text-align:left;">

Nevada

</td>

<td style="text-align:right;">

\-0.2852457

</td>

</tr>

<tr>

<td style="text-align:left;">

Asian

</td>

<td style="text-align:left;">

Nevada

</td>

<td style="text-align:right;">

\-0.4934692

</td>

</tr>

<tr>

<td style="text-align:left;">

Other race

</td>

<td style="text-align:left;">

Nevada

</td>

<td style="text-align:right;">

\-1.3205193

</td>

</tr>

<tr>

<td style="text-align:left;">

White

</td>

<td style="text-align:left;">

New Hampshire

</td>

<td style="text-align:right;">

0.3778848

</td>

</tr>

<tr>

<td style="text-align:left;">

Black

</td>

<td style="text-align:left;">

New Hampshire

</td>

<td style="text-align:right;">

\-1.0666135

</td>

</tr>

<tr>

<td style="text-align:left;">

Latino

</td>

<td style="text-align:left;">

New Hampshire

</td>

<td style="text-align:right;">

1.1827597

</td>

</tr>

<tr>

<td style="text-align:left;">

Asian

</td>

<td style="text-align:left;">

New Hampshire

</td>

<td style="text-align:right;">

\-1.9512133

</td>

</tr>

<tr>

<td style="text-align:left;">

Other race

</td>

<td style="text-align:left;">

New Hampshire

</td>

<td style="text-align:right;">

\-0.8356745

</td>

</tr>

<tr>

<td style="text-align:left;">

White

</td>

<td style="text-align:left;">

New Jersey

</td>

<td style="text-align:right;">

0.6611383

</td>

</tr>

<tr>

<td style="text-align:left;">

Black

</td>

<td style="text-align:left;">

New Jersey

</td>

<td style="text-align:right;">

0.2094820

</td>

</tr>

<tr>

<td style="text-align:left;">

Latino

</td>

<td style="text-align:left;">

New Jersey

</td>

<td style="text-align:right;">

\-0.3154899

</td>

</tr>

<tr>

<td style="text-align:left;">

Asian

</td>

<td style="text-align:left;">

New Jersey

</td>

<td style="text-align:right;">

\-1.3579703

</td>

</tr>

<tr>

<td style="text-align:left;">

Other race

</td>

<td style="text-align:left;">

New Jersey

</td>

<td style="text-align:right;">

\-2.3516358

</td>

</tr>

<tr>

<td style="text-align:left;">

White

</td>

<td style="text-align:left;">

New Mexico

</td>

<td style="text-align:right;">

0.5949394

</td>

</tr>

<tr>

<td style="text-align:left;">

Black

</td>

<td style="text-align:left;">

New Mexico

</td>

<td style="text-align:right;">

\-1.4731346

</td>

</tr>

<tr>

<td style="text-align:left;">

Latino

</td>

<td style="text-align:left;">

New Mexico

</td>

<td style="text-align:right;">

0.0827263

</td>

</tr>

<tr>

<td style="text-align:left;">

Asian

</td>

<td style="text-align:left;">

New Mexico

</td>

<td style="text-align:right;">

\-1.1416185

</td>

</tr>

<tr>

<td style="text-align:left;">

Other race

</td>

<td style="text-align:left;">

New Mexico

</td>

<td style="text-align:right;">

\-0.1302302

</td>

</tr>

<tr>

<td style="text-align:left;">

White

</td>

<td style="text-align:left;">

New York

</td>

<td style="text-align:right;">

0.5648156

</td>

</tr>

<tr>

<td style="text-align:left;">

Black

</td>

<td style="text-align:left;">

New York

</td>

<td style="text-align:right;">

0.8629137

</td>

</tr>

<tr>

<td style="text-align:left;">

Latino

</td>

<td style="text-align:left;">

New York

</td>

<td style="text-align:right;">

\-0.8969490

</td>

</tr>

<tr>

<td style="text-align:left;">

Asian

</td>

<td style="text-align:left;">

New York

</td>

<td style="text-align:right;">

\-2.1290674

</td>

</tr>

<tr>

<td style="text-align:left;">

Other race

</td>

<td style="text-align:left;">

New York

</td>

<td style="text-align:right;">

\-1.6147339

</td>

</tr>

<tr>

<td style="text-align:left;">

White

</td>

<td style="text-align:left;">

North Carolina

</td>

<td style="text-align:right;">

0.4771057

</td>

</tr>

<tr>

<td style="text-align:left;">

Black

</td>

<td style="text-align:left;">

North Carolina

</td>

<td style="text-align:right;">

0.1063050

</td>

</tr>

<tr>

<td style="text-align:left;">

Latino

</td>

<td style="text-align:left;">

North Carolina

</td>

<td style="text-align:right;">

\-0.5969138

</td>

</tr>

<tr>

<td style="text-align:left;">

Asian

</td>

<td style="text-align:left;">

North Carolina

</td>

<td style="text-align:right;">

\-1.6919428

</td>

</tr>

<tr>

<td style="text-align:left;">

Other race

</td>

<td style="text-align:left;">

North Carolina

</td>

<td style="text-align:right;">

\-0.4653057

</td>

</tr>

<tr>

<td style="text-align:left;">

White

</td>

<td style="text-align:left;">

Ohio

</td>

<td style="text-align:right;">

0.3156913

</td>

</tr>

<tr>

<td style="text-align:left;">

Black

</td>

<td style="text-align:left;">

Ohio

</td>

<td style="text-align:right;">

0.6546082

</td>

</tr>

<tr>

<td style="text-align:left;">

Latino

</td>

<td style="text-align:left;">

Ohio

</td>

<td style="text-align:right;">

0.0135409

</td>

</tr>

<tr>

<td style="text-align:left;">

Asian

</td>

<td style="text-align:left;">

Ohio

</td>

<td style="text-align:right;">

\-1.4465592

</td>

</tr>

<tr>

<td style="text-align:left;">

Other race

</td>

<td style="text-align:left;">

Ohio

</td>

<td style="text-align:right;">

\-0.3847516

</td>

</tr>

<tr>

<td style="text-align:left;">

White

</td>

<td style="text-align:left;">

Oregon

</td>

<td style="text-align:right;">

0.4057870

</td>

</tr>

<tr>

<td style="text-align:left;">

Black

</td>

<td style="text-align:left;">

Oregon

</td>

<td style="text-align:right;">

\-2.0214394

</td>

</tr>

<tr>

<td style="text-align:left;">

Latino

</td>

<td style="text-align:left;">

Oregon

</td>

<td style="text-align:right;">

\-0.3149128

</td>

</tr>

<tr>

<td style="text-align:left;">

Asian

</td>

<td style="text-align:left;">

Oregon

</td>

<td style="text-align:right;">

\-0.4591827

</td>

</tr>

<tr>

<td style="text-align:left;">

Other race

</td>

<td style="text-align:left;">

Oregon

</td>

<td style="text-align:right;">

\-0.9397544

</td>

</tr>

<tr>

<td style="text-align:left;">

White

</td>

<td style="text-align:left;">

Pennsylvania

</td>

<td style="text-align:right;">

0.4163404

</td>

</tr>

<tr>

<td style="text-align:left;">

Black

</td>

<td style="text-align:left;">

Pennsylvania

</td>

<td style="text-align:right;">

0.0262010

</td>

</tr>

<tr>

<td style="text-align:left;">

Latino

</td>

<td style="text-align:left;">

Pennsylvania

</td>

<td style="text-align:right;">

0.2292688

</td>

</tr>

<tr>

<td style="text-align:left;">

Asian

</td>

<td style="text-align:left;">

Pennsylvania

</td>

<td style="text-align:right;">

\-2.0276028

</td>

</tr>

<tr>

<td style="text-align:left;">

Other race

</td>

<td style="text-align:left;">

Pennsylvania

</td>

<td style="text-align:right;">

\-1.0543507

</td>

</tr>

<tr>

<td style="text-align:left;">

White

</td>

<td style="text-align:left;">

South Carolina

</td>

<td style="text-align:right;">

0.5768109

</td>

</tr>

<tr>

<td style="text-align:left;">

Black

</td>

<td style="text-align:left;">

South Carolina

</td>

<td style="text-align:right;">

\-0.4984077

</td>

</tr>

<tr>

<td style="text-align:left;">

Latino

</td>

<td style="text-align:left;">

South Carolina

</td>

<td style="text-align:right;">

\-0.5388754

</td>

</tr>

<tr>

<td style="text-align:left;">

Asian

</td>

<td style="text-align:left;">

South Carolina

</td>

<td style="text-align:right;">

\-1.8882016

</td>

</tr>

<tr>

<td style="text-align:left;">

Other race

</td>

<td style="text-align:left;">

South Carolina

</td>

<td style="text-align:right;">

\-0.5997317

</td>

</tr>

<tr>

<td style="text-align:left;">

White

</td>

<td style="text-align:left;">

Texas

</td>

<td style="text-align:right;">

0.7076489

</td>

</tr>

<tr>

<td style="text-align:left;">

Black

</td>

<td style="text-align:left;">

Texas

</td>

<td style="text-align:right;">

0.0462374

</td>

</tr>

<tr>

<td style="text-align:left;">

Latino

</td>

<td style="text-align:left;">

Texas

</td>

<td style="text-align:right;">

\-0.3296450

</td>

</tr>

<tr>

<td style="text-align:left;">

Asian

</td>

<td style="text-align:left;">

Texas

</td>

<td style="text-align:right;">

0.2247874

</td>

</tr>

<tr>

<td style="text-align:left;">

Other race

</td>

<td style="text-align:left;">

Texas

</td>

<td style="text-align:right;">

\-0.7689630

</td>

</tr>

<tr>

<td style="text-align:left;">

White

</td>

<td style="text-align:left;">

Utah

</td>

<td style="text-align:right;">

0.4704854

</td>

</tr>

<tr>

<td style="text-align:left;">

Black

</td>

<td style="text-align:left;">

Utah

</td>

<td style="text-align:right;">

\-1.1210158

</td>

</tr>

<tr>

<td style="text-align:left;">

Latino

</td>

<td style="text-align:left;">

Utah

</td>

<td style="text-align:right;">

\-0.4989875

</td>

</tr>

<tr>

<td style="text-align:left;">

Asian

</td>

<td style="text-align:left;">

Utah

</td>

<td style="text-align:right;">

\-2.1781905

</td>

</tr>

<tr>

<td style="text-align:left;">

Other race

</td>

<td style="text-align:left;">

Utah

</td>

<td style="text-align:right;">

\-0.9491003

</td>

</tr>

<tr>

<td style="text-align:left;">

White

</td>

<td style="text-align:left;">

Virginia

</td>

<td style="text-align:right;">

0.4104170

</td>

</tr>

<tr>

<td style="text-align:left;">

Black

</td>

<td style="text-align:left;">

Virginia

</td>

<td style="text-align:right;">

0.4645187

</td>

</tr>

<tr>

<td style="text-align:left;">

Latino

</td>

<td style="text-align:left;">

Virginia

</td>

<td style="text-align:right;">

\-0.4497964

</td>

</tr>

<tr>

<td style="text-align:left;">

Asian

</td>

<td style="text-align:left;">

Virginia

</td>

<td style="text-align:right;">

\-1.2574486

</td>

</tr>

<tr>

<td style="text-align:left;">

Other race

</td>

<td style="text-align:left;">

Virginia

</td>

<td style="text-align:right;">

\-1.3284723

</td>

</tr>

<tr>

<td style="text-align:left;">

White

</td>

<td style="text-align:left;">

Washington

</td>

<td style="text-align:right;">

0.5728826

</td>

</tr>

<tr>

<td style="text-align:left;">

Black

</td>

<td style="text-align:left;">

Washington

</td>

<td style="text-align:right;">

\-1.4604228

</td>

</tr>

<tr>

<td style="text-align:left;">

Latino

</td>

<td style="text-align:left;">

Washington

</td>

<td style="text-align:right;">

\-0.6882853

</td>

</tr>

<tr>

<td style="text-align:left;">

Asian

</td>

<td style="text-align:left;">

Washington

</td>

<td style="text-align:right;">

\-0.4275579

</td>

</tr>

<tr>

<td style="text-align:left;">

Other race

</td>

<td style="text-align:left;">

Washington

</td>

<td style="text-align:right;">

\-1.6162002

</td>

</tr>

<tr>

<td style="text-align:left;">

White

</td>

<td style="text-align:left;">

Wisconsin

</td>

<td style="text-align:right;">

0.4211253

</td>

</tr>

<tr>

<td style="text-align:left;">

Black

</td>

<td style="text-align:left;">

Wisconsin

</td>

<td style="text-align:right;">

0.6571685

</td>

</tr>

<tr>

<td style="text-align:left;">

Latino

</td>

<td style="text-align:left;">

Wisconsin

</td>

<td style="text-align:right;">

\-0.5877595

</td>

</tr>

<tr>

<td style="text-align:left;">

Asian

</td>

<td style="text-align:left;">

Wisconsin

</td>

<td style="text-align:right;">

\-1.8450388

</td>

</tr>

<tr>

<td style="text-align:left;">

Other race

</td>

<td style="text-align:left;">

Wisconsin

</td>

<td style="text-align:right;">

\-1.1264294

</td>

</tr>

</tbody>

</table>

### Ethnicity and Gender

We can compute`(1 |
ethnicity:gender)`

``` r
turnout_ethnicity_gender_log_coefs <- function(exit_poll, election_data, census_data, z=2) {
  lookup <- list(`White men` = "white_male",
                 `White women` = "white_female",
                 `Black men` = "black_male",
                 `Black women` = "black_female",
                 `Latino men` = "hispanic_male",
                 `Latino women` = "hispanic_female",
                 `Others` = "others")
  exit_poll %>%
    filter(questions=="Race and gender", state=="nation") %>%
    transmute(options,
              k = unlist(lookup[options]),
              estimated_voters = options_perc*0.01*sum(election_data$totalvotes),
              proportion = estimated_voters/sum(census_data[k])
              ) %>%
    transmute(options,
              beta_ethnicity_gender = log(proportion/(1 - proportion)) - beta_0)
}
```

``` r
kable(turnout_ethnicity_gender_log_coefs(exit_poll_df, election_2016, census_data))
```

<table>

<thead>

<tr>

<th style="text-align:left;">

options

</th>

<th style="text-align:right;">

beta\_ethnicity\_gender

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

White men

</td>

<td style="text-align:right;">

0.5525753

</td>

</tr>

<tr>

<td style="text-align:left;">

White women

</td>

<td style="text-align:right;">

0.8887179

</td>

</tr>

<tr>

<td style="text-align:left;">

Black men

</td>

<td style="text-align:right;">

\-2.5118082

</td>

</tr>

<tr>

<td style="text-align:left;">

Black women

</td>

<td style="text-align:right;">

\-2.1271190

</td>

</tr>

<tr>

<td style="text-align:left;">

Latino men

</td>

<td style="text-align:right;">

\-2.5118082

</td>

</tr>

<tr>

<td style="text-align:left;">

Latino women

</td>

<td style="text-align:right;">

\-2.3056687

</td>

</tr>

<tr>

<td style="text-align:left;">

Others

</td>

<td style="text-align:right;">

\-2.3056687

</td>

</tr>

</tbody>

</table>

### Ethnicity and Age

We compute `(1 |
ethnicity:age)`

``` r
turnout_ethnicity_age_log_coefs <- function(exit_poll, election_data, census_data, z=2) {
  lookup <- list("Whites 18-29" = c("white_male_18_29", "white_female_18_29"),
                 "Whites 30-44" = c("white_male_30_44", "white_female_30_44"),
                 "Whites 45-64" = c("white_male_45_64", "white_female_45_64"),
                 "Whites 65 and older" = c("white_male_retirees", "white_female_retirees"),
                 "Blacks 18-29" = c("black_male_18_29", "black_female_18_29"),
                 "Blacks 30-44" = c("black_male_30_44", "black_female_30_44"),
                 "Blacks 45-64" = c("black_male_45_64", "black_female_45_64"),
                 "Blacks 65 and older" = c("black_male_retirees", "black_female_retirees"),
                 "Latinos 18-29" = c("hispanic_male_18_29", "hispanic_female_18_29"),
                 "Latinos 30-44" = c("hispanic_male_30_44", "hispanic_female_30_44"),
                 "Latinos 45-64" = c("hispanic_male_45_64", "hispanic_female_45_64"),
                 "Latinos 65 and older" = c("hispanic_male_retirees", "hispanic_female_retirees"),
                 "All others" = c("others")
                 )
  exit_poll %>%
    filter(questions=="Age by race", state=="nation") %>%
    transmute(options,
              k = lookup[options],
              estimated_voters = options_perc*0.01*sum(election_data$totalvotes),
              proportion = estimated_voters/sum(census_data[unlist(k)])
    ) %>%
    transmute(options,
              beta_ethnicity_age = log(proportion/(1 - proportion)) - beta_0)
}
```

``` r
kable(turnout_ethnicity_age_log_coefs(exit_poll_df, election_2016, census_data))
```

<table>

<thead>

<tr>

<th style="text-align:left;">

options

</th>

<th style="text-align:right;">

beta\_ethnicity\_age

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Whites 18-29

</td>

<td style="text-align:right;">

\-1.4563087

</td>

</tr>

<tr>

<td style="text-align:left;">

Latinos 30-44

</td>

<td style="text-align:right;">

\-2.7582155

</td>

</tr>

<tr>

<td style="text-align:left;">

Latinos 45-64

</td>

<td style="text-align:right;">

\-2.7582155

</td>

</tr>

<tr>

<td style="text-align:left;">

Latinos 65 and older

</td>

<td style="text-align:right;">

\-4.2112202

</td>

</tr>

<tr>

<td style="text-align:left;">

All others

</td>

<td style="text-align:right;">

\-2.3056687

</td>

</tr>

<tr>

<td style="text-align:left;">

Whites 30-44

</td>

<td style="text-align:right;">

\-1.0490358

</td>

</tr>

<tr>

<td style="text-align:left;">

Whites 45-64

</td>

<td style="text-align:right;">

0.1676844

</td>

</tr>

<tr>

<td style="text-align:left;">

Whites 65 and older

</td>

<td style="text-align:right;">

\-1.3476822

</td>

</tr>

<tr>

<td style="text-align:left;">

Blacks 18-29

</td>

<td style="text-align:right;">

\-3.0686324

</td>

</tr>

<tr>

<td style="text-align:left;">

Blacks 30-44

</td>

<td style="text-align:right;">

\-2.7582155

</td>

</tr>

<tr>

<td style="text-align:left;">

Blacks 45-64

</td>

<td style="text-align:right;">

\-2.5118082

</td>

</tr>

<tr>

<td style="text-align:left;">

Blacks 65 and older

</td>

<td style="text-align:right;">

\-4.2112202

</td>

</tr>

<tr>

<td style="text-align:left;">

Latinos 18-29

</td>

<td style="text-align:right;">

\-3.0686324

</td>

</tr>

</tbody>

</table>

## Education

The coefficients of interest are:

    logit(x) ~ (1 | education) + (1 | state:education) + 
               (non-education coefficients)

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
    transmute(beta_education = log(proportion/(1.0-proportion)) - beta_0,
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

\-1.3688000

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

0.6622764

</td>

<td style="text-align:right;">

0.1919843

</td>

</tr>

<tr>

<td style="text-align:left;">

College graduate

</td>

<td style="text-align:right;">

2.2753560

</td>

<td style="text-align:right;">

0.3076090

</td>

</tr>

<tr>

<td style="text-align:left;">

Postgraduate

</td>

<td style="text-align:right;">

4.2230770

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
    transmute(beta_education = log(proportion/(1.0-proportion)) - education_effects[which(education_effects$options==options),]$beta_education - state_effects[which(state_effects$state==state),]$beta_state - 3*beta_0,
              tau_education = 2*(4*width/proportion)**-2,
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

\-2.8349808

</td>

<td style="text-align:right;">

73.79351

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

\-0.3444501

</td>

<td style="text-align:right;">

28.90228

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

\-0.4302131

</td>

<td style="text-align:right;">

415.38112

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

\-2.0373214

</td>

<td style="text-align:right;">

33.51770

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

\-3.1485672

</td>

<td style="text-align:right;">

68.28909

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

\-0.3614575

</td>

<td style="text-align:right;">

39.81955

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

\-1.6579568

</td>

<td style="text-align:right;">

464.23651

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

\-1.9352648

</td>

<td style="text-align:right;">

44.94775

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

\-2.8835674

</td>

<td style="text-align:right;">

92.63909

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

0.2145768

</td>

<td style="text-align:right;">

91.46710

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

\-1.1072204

</td>

<td style="text-align:right;">

255.88914

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

\-1.5073440

</td>

<td style="text-align:right;">

90.57485

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

\-3.1105638

</td>

<td style="text-align:right;">

177.22046

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

\-0.6543754

</td>

<td style="text-align:right;">

70.72173

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

\-0.7510910

</td>

<td style="text-align:right;">

910.99586

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

\-1.9729481

</td>

<td style="text-align:right;">

122.29168

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

\-2.7593332

</td>

<td style="text-align:right;">

141.06544

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

\-0.4529975

</td>

<td style="text-align:right;">

46.11412

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

\-0.5368719

</td>

<td style="text-align:right;">

600.82272

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

\-1.7446144

</td>

<td style="text-align:right;">

84.43682

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

\-2.4534385

</td>

<td style="text-align:right;">

64.82001

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

0.0179353

</td>

<td style="text-align:right;">

33.16307

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

\-0.6282474

</td>

<td style="text-align:right;">

190.33290

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

\-1.3212699

</td>

<td style="text-align:right;">

44.39965

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

\-2.6357149

</td>

<td style="text-align:right;">

106.58484

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

\-0.6230337

</td>

<td style="text-align:right;">

23.42920

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

\-0.5572220

</td>

<td style="text-align:right;">

436.52043

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

\-1.8483879

</td>

<td style="text-align:right;">

48.94366

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

\-3.4525422

</td>

<td style="text-align:right;">

136.56071

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

\-0.7968652

</td>

<td style="text-align:right;">

51.31763

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

\-1.0275573

</td>

<td style="text-align:right;">

755.96027

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

\-2.3839914

</td>

<td style="text-align:right;">

74.24175

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

\-1.7335983

</td>

<td style="text-align:right;">

128.53670

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

\-0.6022060

</td>

<td style="text-align:right;">

14.07229

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

\-0.6387843

</td>

<td style="text-align:right;">

264.04331

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

\-1.6747387

</td>

<td style="text-align:right;">

37.32834

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

\-2.8454335

</td>

<td style="text-align:right;">

169.27996

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

\-0.6194024

</td>

<td style="text-align:right;">

74.08473

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

\-1.1956328

</td>

<td style="text-align:right;">

407.68951

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

\-1.7800495

</td>

<td style="text-align:right;">

112.32552

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

\-2.9575443

</td>

<td style="text-align:right;">

178.25967

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

\-0.6416110

</td>

<td style="text-align:right;">

56.06386

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

\-0.8818332

</td>

<td style="text-align:right;">

675.53923

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

\-2.2543481

</td>

<td style="text-align:right;">

64.71034

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

\-3.9367221

</td>

<td style="text-align:right;">

53.43369

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

\-0.7115120

</td>

<td style="text-align:right;">

72.59301

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

\-1.2499991

</td>

<td style="text-align:right;">

313.50816

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

\-2.5052879

</td>

<td style="text-align:right;">

46.61877

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

\-2.4615538

</td>

<td style="text-align:right;">

155.23995

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

\-0.4570761

</td>

<td style="text-align:right;">

43.55849

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

\-0.7852539

</td>

<td style="text-align:right;">

421.52586

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

\-1.8068547

</td>

<td style="text-align:right;">

68.56317

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

\-2.7072987

</td>

<td style="text-align:right;">

146.23828

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

\-0.5989952

</td>

<td style="text-align:right;">

30.12168

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

\-0.4421829

</td>

<td style="text-align:right;">

667.37175

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

\-2.1305132

</td>

<td style="text-align:right;">

45.32985

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

\-3.8713804

</td>

<td style="text-align:right;">

101.05743

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

\-0.8508473

</td>

<td style="text-align:right;">

94.67674

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

\-1.2280182

</td>

<td style="text-align:right;">

484.64631

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

\-2.3301839

</td>

<td style="text-align:right;">

95.56253

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

\-3.7274337

</td>

<td style="text-align:right;">

34.52067

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

\-0.7406945

</td>

<td style="text-align:right;">

31.49562

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

\-4.0865819

</td>

<td style="text-align:right;">

155.42047

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

\-1.9360802

</td>

<td style="text-align:right;">

44.06863

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

\-2.2585345

</td>

<td style="text-align:right;">

147.07328

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

\-0.3283700

</td>

<td style="text-align:right;">

35.55735

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

\-0.3919294

</td>

<td style="text-align:right;">

372.18513

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

\-1.8222807

</td>

<td style="text-align:right;">

45.46497

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

\-2.9594623

</td>

<td style="text-align:right;">

54.69521

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

\-0.2426483

</td>

<td style="text-align:right;">

29.53254

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

\-2.9141060

</td>

<td style="text-align:right;">

209.65601

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

\-1.3743016

</td>

<td style="text-align:right;">

57.13378

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

\-3.0404668

</td>

<td style="text-align:right;">

229.59620

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

\-0.5752512

</td>

<td style="text-align:right;">

98.56352

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

\-0.8597627

</td>

<td style="text-align:right;">

932.93465

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

\-2.0146589

</td>

<td style="text-align:right;">

136.36314

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

\-2.8302967

</td>

<td style="text-align:right;">

228.23826

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

\-0.7318613

</td>

<td style="text-align:right;">

54.18029

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

\-0.8376056

</td>

<td style="text-align:right;">

774.26328

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

\-1.9545963

</td>

<td style="text-align:right;">

107.32951

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

\-2.4982226

</td>

<td style="text-align:right;">

96.73746

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

\-0.0450616

</td>

<td style="text-align:right;">

57.07833

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

\-0.9529435

</td>

<td style="text-align:right;">

216.26413

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

\-1.9116136

</td>

<td style="text-align:right;">

42.42984

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

\-2.4859586

</td>

<td style="text-align:right;">

234.07759

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

\-0.5351051

</td>

<td style="text-align:right;">

56.84513

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

\-0.8055223

</td>

<td style="text-align:right;">

584.40301

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

\-1.1718421

</td>

<td style="text-align:right;">

191.07022

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

\-2.2793183

</td>

<td style="text-align:right;">

68.40837

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

\-0.3143722

</td>

<td style="text-align:right;">

22.02395

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

\-0.6171946

</td>

<td style="text-align:right;">

204.09011

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

\-1.5636715

</td>

<td style="text-align:right;">

33.20587

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

\-2.7931877

</td>

<td style="text-align:right;">

102.06980

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

\-0.3559171

</td>

<td style="text-align:right;">

35.05939

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

\-0.1812410

</td>

<td style="text-align:right;">

613.84671

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

\-1.7641303

</td>

<td style="text-align:right;">

54.50890

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

\-3.3752515

</td>

<td style="text-align:right;">

36.88041

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

\-0.3173915

</td>

<td style="text-align:right;">

37.79113

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

\-0.5878587

</td>

<td style="text-align:right;">

249.88717

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

\-2.3925931

</td>

<td style="text-align:right;">

16.19401

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

\-3.4786526

</td>

<td style="text-align:right;">

110.06647

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

\-0.5925617

</td>

<td style="text-align:right;">

80.19606

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

\-3.6523176

</td>

<td style="text-align:right;">

382.28469

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

\-2.0791508

</td>

<td style="text-align:right;">

88.10906

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

\-2.8319288

</td>

<td style="text-align:right;">

57.05760

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

0.1078772

</td>

<td style="text-align:right;">

47.74499

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

\-0.7229975

</td>

<td style="text-align:right;">

204.08965

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

\-1.7207863

</td>

<td style="text-align:right;">

40.40965

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

\-3.3074749

</td>

<td style="text-align:right;">

164.27100

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

\-0.7722326

</td>

<td style="text-align:right;">

65.83759

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

\-1.0721567

</td>

<td style="text-align:right;">

731.99098

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

\-2.2050533

</td>

<td style="text-align:right;">

94.69527

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

## Prediction for States

Now we have assembled all the coefficients necessary, we will try to
predict the fraction of the voting age population turns out to vote in
each state.

TODO: add a `sigma` parameter to allow some multiple of standard
deviation to appear in the logistic regression coefficients, so we can
see how turnout changes “within reason”.

``` r
mrp_weights <- function(state, coefs, beta_0, sigma_turnout=0) {
  list("male" = c(beta_0,
                  coefs$state[[which(coefs$state$state==state),c("beta_state")]],
                  coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Male"),c("beta_state_gender")]] + sigma_turnout*(coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Male"),c("tau_state_gender")]])**-0.5),
       "female" = c(beta_0,
                    coefs$state[[which(coefs$state$state==state),c("beta_state")]],
                    coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Female"),c("beta_state_gender")]] + 
                      sigma_turnout*(coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Female"),c("tau_state_gender")]])**-0.5),
       "age_18_24" = c(beta_0,
                       coefs$state[[which(coefs$state$state==state),c("beta_state")]],
                       coefs$age[[which(coefs$age=="18-24"),c("beta_age")]],
                       coefs$state_age[[which(coefs$state_age=="18-24" & coefs$state_age$state==state),c("beta_age")]] + sigma_turnout*(coefs$state_age[[which(coefs$state_age=="18-24" & coefs$state_age$state==state),c("tau_age")]]**-0.5)),
       "age_25_29" = c(beta_0,
                       coefs$state[[which(coefs$state$state==state),c("beta_state")]],
                       coefs$age[[which(coefs$age=="25-29"),c("beta_age")]],
                       coefs$state_age[[which(coefs$state_age=="25-29" & coefs$state_age$state==state),c("beta_age")]] + sigma_turnout*(coefs$state_age[[which(coefs$state_age=="25-29" & coefs$state_age$state==state),c("tau_age")]])),
       "age_30_39" = c(beta_0,
                       coefs$state[[which(coefs$state$state==state),c("beta_state")]],
                       coefs$age[[which(coefs$age=="30-39"),c("beta_age")]],
                       coefs$state_age[[which(coefs$state_age=="30-39" & coefs$state_age$state==state),c("beta_age")]] + sigma_turnout*(coefs$state_age[[which(coefs$state_age=="30-39" & coefs$state_age$state==state),c("tau_age")]]**-0.5)),
       "age_40_49" = c(beta_0,
                       coefs$state[[which(coefs$state$state==state),c("beta_state")]],
                       coefs$age[[which(coefs$age=="40-49"),c("beta_age")]],
                       coefs$state_age[[which(coefs$state_age=="40-49" & coefs$state_age$state==state),c("beta_age")]] + sigma_turnout*(coefs$state_age[[which(coefs$state_age=="40-49" & coefs$state_age$state==state),c("tau_age")]]**-0.5)),
       "age_50_64" = c(beta_0,
                       coefs$state[[which(coefs$state$state==state),c("beta_state")]],
                       coefs$age[[which(coefs$age=="50-64"),c("beta_age")]],
                       coefs$state_age[[which(coefs$state_age=="50-64" & coefs$state_age$state==state),c("beta_age")]] + sigma_turnout*(coefs$state_age[[which(coefs$state_age=="50-64" & coefs$state_age$state==state),c("tau_age")]]**-0.5)),
       "age_retirees" = c(beta_0,
                          coefs$state[[which(coefs$state$state==state),c("beta_state")]],
                          coefs$age[[which(coefs$age=="65 and older"),c("beta_age")]],
                          coefs$state_age[[which(coefs$state_age=="65 and older" & coefs$state_age$state==state),c("beta_age")]] + sigma_turnout*(coefs$state_age[[which(coefs$state_age=="65 and older" & coefs$state_age$state==state),c("tau_age")]]**-0.5)
       ),
       "education_high_school_male" = c(beta_0,
                                        coefs$state[[which(coefs$state$state==state),c("beta_state")]],
                                        coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Male"),c("beta_state_gender")]] + 
                                          sigma_turnout*(coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Male"),c("tau_state_gender")]]**-0.5),
                                        coefs$state_ed[[which(coefs$state_ed$state==state & coefs$state_ed$options=="High school or less"),c("beta_education")]] + sigma_turnout*(coefs$state_ed[[which(coefs$state_ed$state==state & coefs$state_ed$options=="High school or less"),c("tau_education")]]**-0.5),
                                        coefs$ed[[which(coefs$ed$options=="High school or less"),c("beta_education")]] + sigma_turnout*(coefs$ed[[which(coefs$ed$options=="High school or less"),c("tau_education")]]**-0.5)), 
       "education_high_school_female" = c(beta_0,
                                          coefs$state[[which(coefs$state$state==state),c("beta_state")]],
                                          coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Female"),c("beta_state_gender")]] + sigma_turnout*(coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Female"),c("tau_state_gender")]]**-0.5),
                                          coefs$state_ed[[which(coefs$state_ed$state==state & coefs$state_ed$options=="High school or less"),c("beta_education")]] + sigma_turnout*(coefs$state_ed[[which(coefs$state_ed$state==state & coefs$state_ed$options=="High school or less"),c("tau_education")]]**-0.5),
                                          coefs$ed[[which(coefs$ed$options=="High school or less"),c("beta_education")]] + sigma_turnout*(coefs$ed[[which(coefs$ed$options=="High school or less"),c("tau_education")]]**-0.5)),
       "education_some_college_male" = c(beta_0,
                                         coefs$state[[which(coefs$state$state==state),c("beta_state")]],
                                         coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Male"),c("beta_state_gender")]] + sigma_turnout*(coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Male"),c("tau_state_gender")]]**-0.5),
                                         coefs$state_ed[[which(coefs$state_ed$state==state & coefs$state_ed$options=="Some college"),c("beta_education")]] + sigma_turnout*(coefs$state_ed[[which(coefs$state_ed$state==state & coefs$state_ed$options=="Some college"),c("tau_education")]]**-0.5),
                                         coefs$ed[[which(coefs$ed$options=="Some college"),c("beta_education")]] + sigma_turnout*(coefs$ed[[which(coefs$ed$options=="Some college"),c("tau_education")]]**-0.5)), 
       "education_some_college_female" = c(beta_0,
                                           coefs$state[[which(coefs$state$state==state),c("beta_state")]],
                                           coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Female"),c("beta_state_gender")]] + sigma_turnout*(coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Female"),c("tau_state_gender")]]**-0.5),
                                           coefs$state_ed[[which(coefs$state_ed$state==state & coefs$state_ed$options=="Some college"),c("beta_education")]] + sigma_turnout*(coefs$state_ed[[which(coefs$state_ed$state==state & coefs$state_ed$options=="Some college"),c("tau_education")]]**-0.5),
                                           coefs$ed[[which(coefs$ed$options=="Some college"),c("beta_education")]] + sigma_turnout*(coefs$ed[[which(coefs$ed$options=="Some college"),c("tau_education")]]**-0.5)),
       "education_assoc_degree_male" = c(beta_0,
                                         coefs$state[[which(coefs$state$state==state),c("beta_state")]],
                                         coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Male"),c("beta_state_gender")]] + sigma_turnout*(coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Male"),c("tau_state_gender")]]**-0.5),
                                         0.5*(coefs$state_ed[[which(coefs$state_ed$state==state & coefs$state_ed$options=="Some college"),c("beta_education")]] + coefs$state_ed[[which(coefs$state_ed$state==state & coefs$state_ed$options=="College graduate"),c("beta_education")]]) + sigma_turnout*(0.5*(coefs$state_ed[[which(coefs$state_ed$state==state & coefs$state_ed$options=="Some college"),c("tau_education")]] + coefs$state_ed[[which(coefs$state_ed$state==state & coefs$state_ed$options=="College graduate"),c("tau_education")]]))**-0.5,
                                         0.5*(coefs$ed[[which(coefs$ed$options=="Some college"),c("beta_education")]]+coefs$ed[[which(coefs$ed$options=="Some college"),c("beta_education")]]) + sigma_turnout*((0.5*(coefs$ed[[which(coefs$ed$options=="Some college"),c("tau_education")]]+coefs$ed[[which(coefs$ed$options=="Some college"),c("tau_education")]]))**-0.5)),
       "education_assoc_degree_female"  = c(beta_0,
                                            coefs$state[[which(coefs$state$state==state),c("beta_state")]],
                                            coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Female"),c("beta_state_gender")]] + sigma_turnout*(coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Female"),c("tau_state_gender")]]**-0.5),
                                            0.5*(coefs$state_ed[[which(coefs$state_ed$state==state & coefs$state_ed$options=="Some college"),c("beta_education")]] + coefs$state_ed[[which(coefs$state_ed$state==state & coefs$state_ed$options=="College graduate"),c("beta_education")]])  + sigma_turnout*(0.5*(coefs$state_ed[[which(coefs$state_ed$state==state & coefs$state_ed$options=="Some college"),c("tau_education")]] + coefs$state_ed[[which(coefs$state_ed$state==state & coefs$state_ed$options=="College graduate"),c("tau_education")]]))**-0.5,
                                            0.5*(coefs$ed[[which(coefs$ed$options=="Some college"),c("beta_education")]]+coefs$ed[[which(coefs$ed$options=="Some college"),c("beta_education")]]) + sigma_turnout*((0.5*(coefs$ed[[which(coefs$ed$options=="Some college"),c("tau_education")]]+coefs$ed[[which(coefs$ed$options=="Some college"),c("tau_education")]]))**-0.5)),
       
       "education_college_grad_male" = c(beta_0,
                                         coefs$state[[which(coefs$state$state==state),c("beta_state")]],
                                         coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Male"),c("beta_state_gender")]] + sigma_turnout*(coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Male"),c("tau_state_gender")]]**-0.5),
                                         coefs$state_ed[[which(coefs$state_ed$state==state & coefs$state_ed$options=="College graduate"),c("beta_education")]] + sigma_turnout*(coefs$state_ed[[which(coefs$state_ed$state==state & coefs$state_ed$options=="College graduate"),c("tau_education")]]**-0.5),
                                         coefs$ed[[which(coefs$ed$options=="College graduate"),c("beta_education")]] + sigma_turnout*(coefs$ed[[which(coefs$ed$options=="College graduate"),c("tau_education")]]**-0.5)), 
       "education_college_grad_female" = c(beta_0,
                                           coefs$state[[which(coefs$state$state==state),c("beta_state")]],
                                           coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Female"),c("beta_state_gender")]] + sigma_turnout*(coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Female"),c("tau_state_gender")]]**-0.5),
                                           coefs$state_ed[[which(coefs$state_ed$state==state & coefs$state_ed$options=="College graduate"),c("beta_education")]] + sigma_turnout*(coefs$state_ed[[which(coefs$state_ed$state==state & coefs$state_ed$options=="College graduate"),c("tau_education")]]**-0.5),
                                           coefs$ed[[which(coefs$ed$options=="College graduate"),c("beta_education")]] + sigma_turnout*(coefs$ed[[which(coefs$ed$options=="College graduate"),c("tau_education")]]**-0.5)), 
       "education_postgrad_male" = c(beta_0,
                                     coefs$state[[which(coefs$state$state==state),c("beta_state")]],
                                     coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Male"),c("beta_state_gender")]] + sigma_turnout*(coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Male"),c("tau_state_gender")]]**-0.5),
                                     coefs$state_ed[[which(coefs$state_ed$state==state & coefs$state_ed$options=="Postgraduate"),c("beta_education")]] + sigma_turnout*(coefs$state_ed[[which(coefs$state_ed$state==state & coefs$state_ed$options=="Postgraduate"),c("tau_education")]]**-0.5),
                                     coefs$ed[[which(coefs$ed$options=="Postgraduate"),c("beta_education")]] + sigma_turnout*(coefs$ed[[which(coefs$ed$options=="Postgraduate"),c("tau_education")]]**-0.5)), 
       "education_postgrad_female" = c(beta_0,
                                       coefs$state[[which(coefs$state$state==state),c("beta_state")]],
                                       coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Female"),c("beta_state_gender")]] + sigma_turnout*(coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Female"),c("tau_state_gender")]]**-0.5),
                                       coefs$state_ed[[which(coefs$state_ed$state==state & coefs$state_ed$options=="Postgraduate"),c("beta_education")]] + sigma_turnout*(coefs$state_ed[[which(coefs$state_ed$state==state & coefs$state_ed$options=="Postgraduate"),c("tau_education")]]**-0.5),
                                       coefs$ed[[which(coefs$ed$options=="Postgraduate"),c("beta_education")]]  + sigma_turnout*(coefs$ed[[which(coefs$ed$options=="Postgraduate"),c("tau_education")]]**-0.5)), 
       "white_male_18_29" = c(beta_0,
                              coefs$state[[which(coefs$state$state==state), c("beta_state")]],
                              coefs$race_age[[which(coefs$race_age=="Whites 18-29"), c("beta_ethnicity_age")]],
                              coefs$race_gender[[which(coefs$race_gender=="White men"), c("beta_ethnicity_gender")]],
                              coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Male"),c("beta_state_gender")]] + sigma_turnout*(coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Male"),c("tau_state_gender")]]**-0.5),
                              coefs$state_race[[which(coefs$state_race$state==state & coefs$state_race$options=="White"), c("beta_ethnicity_by_state")]]
       ),
       "white_male_30_44" = c(beta_0,
                              coefs$state[[which(coefs$state$state==state), c("beta_state")]],
                              coefs$race_age[[which(coefs$race_age=="Whites 30-44"), c("beta_ethnicity_age")]],
                              coefs$race_gender[[which(coefs$race_gender=="White men"), c("beta_ethnicity_gender")]],
                              coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Male"),c("beta_state_gender")]] + sigma_turnout*(coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Male"),c("tau_state_gender")]]**-0.5),
                              coefs$state_race[[which(coefs$state_race$state==state & coefs$state_race$options=="White"), c("beta_ethnicity_by_state")]]
       ), 
       "white_male_45_64" = c(beta_0,
                              coefs$state[[which(coefs$state$state==state), c("beta_state")]],
                              coefs$race_age[[which(coefs$race_age=="Whites 45-64"), c("beta_ethnicity_age")]],
                              coefs$race_gender[[which(coefs$race_gender=="White men"), c("beta_ethnicity_gender")]],
                              coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Male"),c("beta_state_gender")]] + sigma_turnout*(coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Male"),c("tau_state_gender")]]**-0.5),
                              coefs$state_race[[which(coefs$state_race$state==state & coefs$state_race$options=="White"), c("beta_ethnicity_by_state")]]
       ), 
       "white_male_retirees" = c(beta_0,
                                 coefs$state[[which(coefs$state$state==state), c("beta_state")]],
                                 coefs$race_age[[which(coefs$race_age=="Whites 65 and older"), c("beta_ethnicity_age")]],
                                 coefs$race_gender[[which(coefs$race_gender=="White men"), c("beta_ethnicity_gender")]],
                                 coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Male"),c("beta_state_gender")]] + sigma_turnout*(coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Male"),c("tau_state_gender")]]**-0.5),
                                 coefs$state_race[[which(coefs$state_race$state==state & coefs$state_race$options=="White"), c("beta_ethnicity_by_state")]]
       ), 
       "white_female_18_29" = c(beta_0,
                                coefs$state[[which(coefs$state$state==state), c("beta_state")]],
                                coefs$race_age[[which(coefs$race_age=="Whites 18-29"), c("beta_ethnicity_age")]],
                                coefs$race_gender[[which(coefs$race_gender=="White women"), c("beta_ethnicity_gender")]],
                                coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Female"),c("beta_state_gender")]] + sigma_turnout*(coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Female"),c("tau_state_gender")]]**-0.5),
                                coefs$state_race[[which(coefs$state_race$state==state & coefs$state_race$options=="White"), c("beta_ethnicity_by_state")]]
       ),
       "white_female_30_44" = c(beta_0,
                                coefs$state[[which(coefs$state$state==state), c("beta_state")]],
                                coefs$race_age[[which(coefs$race_age=="Whites 30-44"), c("beta_ethnicity_age")]],
                                coefs$race_gender[[which(coefs$race_gender=="White women"), c("beta_ethnicity_gender")]],
                                coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Female"),c("beta_state_gender")]] + sigma_turnout*(coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Female"),c("tau_state_gender")]]**-0.5),
                                coefs$state_race[[which(coefs$state_race$state==state & coefs$state_race$options=="White"), c("beta_ethnicity_by_state")]]
       ),
       "white_female_45_64" = c(beta_0,
                                coefs$state[[which(coefs$state$state==state), c("beta_state")]],
                                coefs$race_age[[which(coefs$race_age=="Whites 45-64"), c("beta_ethnicity_age")]],
                                coefs$race_gender[[which(coefs$race_gender=="White women"), c("beta_ethnicity_gender")]],
                                coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Female"),c("beta_state_gender")]] + sigma_turnout*(coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Female"),c("tau_state_gender")]]**-0.5),
                                coefs$state_race[[which(coefs$state_race$state==state & coefs$state_race$options=="White"), c("beta_ethnicity_by_state")]]
       ),
       "white_female_retirees" = c(beta_0,
                                   coefs$state[[which(coefs$state$state==state), c("beta_state")]],
                                   coefs$race_age[[which(coefs$race_age=="Whites 65 and older"), c("beta_ethnicity_age")]],
                                   coefs$race_gender[[which(coefs$race_gender=="White women"), c("beta_ethnicity_gender")]],
                                   coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Female"),c("beta_state_gender")]] + sigma_turnout*(coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Female"),c("tau_state_gender")]]**-0.5),
                                   coefs$state_race[[which(coefs$state_race$state==state & coefs$state_race$options=="White"), c("beta_ethnicity_by_state")]]
       ),
       "black_male_18_29" = c(beta_0,
                              coefs$state[[which(coefs$state$state==state), c("beta_state")]],
                              coefs$race_age[[which(coefs$race_age=="Blacks 18-29"), c("beta_ethnicity_age")]],
                              coefs$race_gender[[which(coefs$race_gender=="Black men"), c("beta_ethnicity_gender")]],
                              coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Male"), c("beta_state_gender")]] + sigma_turnout*(coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Male"),c("tau_state_gender")]]**-0.5),
                              coefs$state_race[[which(coefs$state_race$state==state & coefs$state_race$options=="Black"), c("beta_ethnicity_by_state")]]
       ),
       "black_male_30_44" = c(beta_0,
                              coefs$state[[which(coefs$state$state==state), c("beta_state")]],
                              coefs$race_age[[which(coefs$race_age=="Blacks 30-44"), c("beta_ethnicity_age")]],
                              coefs$race_gender[[which(coefs$race_gender=="Black men"), c("beta_ethnicity_gender")]],
                              coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Male"), c("beta_state_gender")]] + sigma_turnout*(coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Male"),c("tau_state_gender")]]**-0.5),
                              coefs$state_race[[which(coefs$state_race$state==state & coefs$state_race$options=="Black"), c("beta_ethnicity_by_state")]]
       ), 
       "black_male_45_64" = c(beta_0,
                              coefs$state[[which(coefs$state$state==state), c("beta_state")]],
                              coefs$race_age[[which(coefs$race_age=="Blacks 45-64"), c("beta_ethnicity_age")]],
                              coefs$race_gender[[which(coefs$race_gender=="Black men"), c("beta_ethnicity_gender")]],
                              coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Male"), c("beta_state_gender")]] + sigma_turnout*(coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Male"),c("tau_state_gender")]]**-0.5),
                              coefs$state_race[[which(coefs$state_race$state==state & coefs$state_race$options=="Black"), c("beta_ethnicity_by_state")]]
       ), 
       "black_male_retirees" = c(beta_0,
                                 coefs$state[[which(coefs$state$state==state), c("beta_state")]],
                                 coefs$race_age[[which(coefs$race_age=="Blacks 65 and older"), c("beta_ethnicity_age")]],
                                 coefs$race_gender[[which(coefs$race_gender=="Black men"), c("beta_ethnicity_gender")]],
                                 coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Male"), c("beta_state_gender")]] + sigma_turnout*(coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Male"),c("tau_state_gender")]]**-0.5),
                                 coefs$state_race[[which(coefs$state_race$state==state & coefs$state_race$options=="Black"), c("beta_ethnicity_by_state")]]
       ), 
       "black_female_18_29" = c(beta_0,
                                coefs$state[[which(coefs$state$state==state), c("beta_state")]],
                                coefs$race_age[[which(coefs$race_age=="Blacks 18-29"), c("beta_ethnicity_age")]],
                                coefs$race_gender[[which(coefs$race_gender=="Black women"), c("beta_ethnicity_gender")]],
                                coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Female"), c("beta_state_gender")]] + sigma_turnout*(coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Female"),c("tau_state_gender")]]**-0.5),
                                coefs$state_race[[which(coefs$state_race$state==state & coefs$state_race$options=="Black"), c("beta_ethnicity_by_state")]]
       ),
       "black_female_30_44" = c(beta_0,
                                coefs$state[[which(coefs$state$state==state), c("beta_state")]],
                                coefs$race_age[[which(coefs$race_age=="Blacks 30-44"), c("beta_ethnicity_age")]],
                                coefs$race_gender[[which(coefs$race_gender=="Black women"), c("beta_ethnicity_gender")]],
                                coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Female"), c("beta_state_gender")]] + sigma_turnout*(coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Female"),c("tau_state_gender")]]**-0.5),
                                coefs$state_race[[which(coefs$state_race$state==state & coefs$state_race$options=="Black"), c("beta_ethnicity_by_state")]]
       ),
       "black_female_45_64" = c(beta_0,
                                coefs$state[[which(coefs$state$state==state), c("beta_state")]],
                                coefs$race_age[[which(coefs$race_age=="Blacks 45-64"), c("beta_ethnicity_age")]],
                                coefs$race_gender[[which(coefs$race_gender=="Black women"), c("beta_ethnicity_gender")]],
                                coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Female"), c("beta_state_gender")]] + sigma_turnout*(coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Female"),c("tau_state_gender")]]**-0.5),
                                coefs$state_race[[which(coefs$state_race$state==state & coefs$state_race$options=="Black"), c("beta_ethnicity_by_state")]]
       ), 
       "black_female_retirees" = c(beta_0,
                                   coefs$state[[which(coefs$state$state==state), c("beta_state")]],
                                   coefs$race_age[[which(coefs$race_age=="Blacks 65 and older"), c("beta_ethnicity_age")]],
                                   coefs$race_gender[[which(coefs$race_gender=="Black women"), c("beta_ethnicity_gender")]],
                                   coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Female"), c("beta_state_gender")]] + sigma_turnout*(coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Female"),c("tau_state_gender")]]**-0.5),
                                   coefs$state_race[[which(coefs$state_race$state==state & coefs$state_race$options=="Black"), c("beta_ethnicity_by_state")]]
       ),
       "hispanic_male_18_29" = c(beta_0,
                                 coefs$state[[which(coefs$state$state==state), c("beta_state")]],
                                 coefs$race_age[[which(coefs$race_age=="Latinos 18-29"), c("beta_ethnicity_age")]],
                                 coefs$race_gender[[which(coefs$race_gender=="Latino men"), c("beta_ethnicity_gender")]],
                                 coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Male"), c("beta_state_gender")]] + sigma_turnout*(coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Male"),c("tau_state_gender")]]**-0.5),
                                 coefs$state_race[[which(coefs$state_race$state==state & coefs$state_race$options=="Latino"), c("beta_ethnicity_by_state")]]
       ), 
       "hispanic_male_30_44" = c(beta_0,
                                 coefs$state[[which(coefs$state$state==state), c("beta_state")]],
                                 coefs$race_age[[which(coefs$race_age=="Latinos 30-44"), c("beta_ethnicity_age")]],
                                 coefs$race_gender[[which(coefs$race_gender=="Latino men"), c("beta_ethnicity_gender")]],
                                 coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Male"), c("beta_state_gender")]] + sigma_turnout*(coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Male"),c("tau_state_gender")]]**-0.5),
                                 coefs$state_race[[which(coefs$state_race$state==state & coefs$state_race$options=="Latino"), c("beta_ethnicity_by_state")]]
       ),
       "hispanic_male_45_64" = c(beta_0,
                                 coefs$state[[which(coefs$state$state==state), c("beta_state")]],
                                 coefs$race_age[[which(coefs$race_age=="Latinos 45-64"), c("beta_ethnicity_age")]],
                                 coefs$race_gender[[which(coefs$race_gender=="Latino men"), c("beta_ethnicity_gender")]],
                                 coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Male"), c("beta_state_gender")]] + sigma_turnout*(coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Male"),c("tau_state_gender")]]**-0.5),
                                 coefs$state_race[[which(coefs$state_race$state==state & coefs$state_race$options=="Latino"), c("beta_ethnicity_by_state")]]
       ),
       "hispanic_male_retirees" = c(beta_0,
                                    coefs$state[[which(coefs$state$state==state), c("beta_state")]],
                                    coefs$race_age[[which(coefs$race_age=="Latinos 65 and older"), c("beta_ethnicity_age")]],
                                    coefs$race_gender[[which(coefs$race_gender=="Latino men"), c("beta_ethnicity_gender")]],
                                    coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Male"), c("beta_state_gender")]] + sigma_turnout*(coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Male"),c("tau_state_gender")]]**-0.5),
                                    coefs$state_race[[which(coefs$state_race$state==state & coefs$state_race$options=="Latino"), c("beta_ethnicity_by_state")]]
       ),
       "hispanic_female_18_29" = c(beta_0,
                                   coefs$state[[which(coefs$state$state==state), c("beta_state")]],
                                   coefs$race_age[[which(coefs$race_age=="Latinos 18-29"), c("beta_ethnicity_age")]],
                                   coefs$race_gender[[which(coefs$race_gender=="Latino women"), c("beta_ethnicity_gender")]],
                                   coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Female"), c("beta_state_gender")]] + sigma_turnout*(coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Female"),c("tau_state_gender")]]**-0.5),
                                   coefs$state_race[[which(coefs$state_race$state==state & coefs$state_race$options=="Latino"), c("beta_ethnicity_by_state")]]
       ), 
       "hispanic_female_30_44" = c(beta_0,
                                   coefs$state[[which(coefs$state$state==state), c("beta_state")]],
                                   coefs$race_age[[which(coefs$race_age=="Latinos 30-44"), c("beta_ethnicity_age")]],
                                   coefs$race_gender[[which(coefs$race_gender=="Latino women"), c("beta_ethnicity_gender")]],
                                   coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Female"), c("beta_state_gender")]] + sigma_turnout*(coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Female"),c("tau_state_gender")]]**-0.5),
                                   coefs$state_race[[which(coefs$state_race$state==state & coefs$state_race$options=="Latino"), c("beta_ethnicity_by_state")]]
       ),
       "hispanic_female_45_64" = c(beta_0,
                                   coefs$state[[which(coefs$state$state==state), c("beta_state")]],
                                   coefs$race_age[[which(coefs$race_age=="Latinos 45-64"), c("beta_ethnicity_age")]],
                                   coefs$race_gender[[which(coefs$race_gender=="Latino women"), c("beta_ethnicity_gender")]],
                                   coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Female"), c("beta_state_gender")]] + sigma_turnout*(coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Female"),c("tau_state_gender")]]**-0.5),
                                   coefs$state_race[[which(coefs$state_race$state==state & coefs$state_race$options=="Latino"), c("beta_ethnicity_by_state")]]
       ), 
       "hispanic_female_retirees" = c(beta_0,
                                      coefs$state[[which(coefs$state$state==state), c("beta_state")]],
                                      coefs$race_age[[which(coefs$race_age=="Latinos 65 and older"), c("beta_ethnicity_age")]],
                                      coefs$race_gender[[which(coefs$race_gender=="Latino women"), c("beta_ethnicity_gender")]],
                                      coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Female"), c("beta_state_gender")]] + sigma_turnout*(coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Female"),c("tau_state_gender")]]**-0.5),
                                      coefs$state_race[[which(coefs$state_race$state==state & coefs$state_race$options=="Latino"), c("beta_ethnicity_by_state")]]
       ),
       "others_male_18_29" = c(beta_0,
                               coefs$state[[which(coefs$state$state==state), c("beta_state")]],
                               coefs$race_age[[which(coefs$race_age=="All others"), c("beta_ethnicity_age")]],
                               coefs$race_gender[[which(coefs$race_gender=="Others"), c("beta_ethnicity_gender")]],
                               coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Male"), c("beta_state_gender")]] + sigma_turnout*(coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Male"),c("tau_state_gender")]]**-0.5),
                               coefs$state_race[[which(coefs$state_race$state==state & coefs$state_race$options=="Other race"), c("beta_ethnicity_by_state")]],
                               coefs$state_race[[which(coefs$state_race$state==state & coefs$state_race$options=="Asian"), c("beta_ethnicity_by_state")]]
       ),
       "others_male_30_44" = c(beta_0,
                               coefs$state[[which(coefs$state$state==state), c("beta_state")]],
                               coefs$race_age[[which(coefs$race_age=="All others"), c("beta_ethnicity_age")]],
                               coefs$race_gender[[which(coefs$race_gender=="Others"), c("beta_ethnicity_gender")]],
                               coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Male"), c("beta_state_gender")]] + sigma_turnout*(coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Male"),c("tau_state_gender")]]**-0.5),
                               coefs$state_race[[which(coefs$state_race$state==state & coefs$state_race$options=="Other race"), c("beta_ethnicity_by_state")]],
                               coefs$state_race[[which(coefs$state_race$state==state & coefs$state_race$options=="Asian"), c("beta_ethnicity_by_state")]]
       ), 
       "others_male_45_64" = c(beta_0,
                               coefs$state[[which(coefs$state$state==state), c("beta_state")]],
                               coefs$race_age[[which(coefs$race_age=="All others"), c("beta_ethnicity_age")]],
                               coefs$race_gender[[which(coefs$race_gender=="Others"), c("beta_ethnicity_gender")]],
                               coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Male"), c("beta_state_gender")]] + sigma_turnout*(coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Male"),c("tau_state_gender")]]**-0.5),
                               coefs$state_race[[which(coefs$state_race$state==state & coefs$state_race$options=="Other race"), c("beta_ethnicity_by_state")]],
                               coefs$state_race[[which(coefs$state_race$state==state & coefs$state_race$options=="Asian"), c("beta_ethnicity_by_state")]]
       ),
       "others_male_retirees" = c(beta_0,
                                  coefs$state[[which(coefs$state$state==state), c("beta_state")]],
                                  coefs$race_age[[which(coefs$race_age=="All others"), c("beta_ethnicity_age")]],
                                  coefs$race_gender[[which(coefs$race_gender=="Others"), c("beta_ethnicity_gender")]],
                                  coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Male"), c("beta_state_gender")]] + sigma_turnout*(coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Male"),c("tau_state_gender")]]**-0.5),
                                  coefs$state_race[[which(coefs$state_race$state==state & coefs$state_race$options=="Other race"), c("beta_ethnicity_by_state")]],
                                  coefs$state_race[[which(coefs$state_race$state==state & coefs$state_race$options=="Asian"), c("beta_ethnicity_by_state")]]
       ), 
       "others_female_18_29" = c(beta_0,
                                 coefs$state[[which(coefs$state$state==state), c("beta_state")]],
                                 coefs$race_age[[which(coefs$race_age=="All others"), c("beta_ethnicity_age")]],
                                 coefs$race_gender[[which(coefs$race_gender=="Others"), c("beta_ethnicity_gender")]],
                                 coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Female"), c("beta_state_gender")]] + sigma_turnout*(coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Female"),c("tau_state_gender")]]**-0.5),
                                 coefs$state_race[[which(coefs$state_race$state==state & coefs$state_race$options=="Other race"), c("beta_ethnicity_by_state")]],
                                 coefs$state_race[[which(coefs$state_race$state==state & coefs$state_race$options=="Asian"), c("beta_ethnicity_by_state")]]
       ),
       "others_female_30_44" = c(beta_0,
                                 coefs$state[[which(coefs$state$state==state), c("beta_state")]],
                                 coefs$race_age[[which(coefs$race_age=="All others"), c("beta_ethnicity_age")]],
                                 coefs$race_gender[[which(coefs$race_gender=="Others"), c("beta_ethnicity_gender")]],
                                 coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Female"), c("beta_state_gender")]] + sigma_turnout*(coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Female"),c("tau_state_gender")]]**-0.5),
                                 coefs$state_race[[which(coefs$state_race$state==state & coefs$state_race$options=="Other race"), c("beta_ethnicity_by_state")]],
                                 coefs$state_race[[which(coefs$state_race$state==state & coefs$state_race$options=="Asian"), c("beta_ethnicity_by_state")]]
       ), 
       "others_female_45_64" = c(beta_0,
                                 coefs$state[[which(coefs$state$state==state), c("beta_state")]],
                                 coefs$race_age[[which(coefs$race_age=="All others"), c("beta_ethnicity_age")]],
                                 coefs$race_gender[[which(coefs$race_gender=="Others"), c("beta_ethnicity_gender")]],
                                 coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Female"), c("beta_state_gender")]] + sigma_turnout*(coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Female"),c("tau_state_gender")]]**-0.5),
                                 coefs$state_race[[which(coefs$state_race$state==state & coefs$state_race$options=="Other race"), c("beta_ethnicity_by_state")]],
                                 coefs$state_race[[which(coefs$state_race$state==state & coefs$state_race$options=="Asian"), c("beta_ethnicity_by_state")]]
       ), 
       "others_female_retirees" = c(beta_0,
                                    coefs$state[[which(coefs$state$state==state), c("beta_state")]],
                                    coefs$race_age[[which(coefs$race_age=="All others"), c("beta_ethnicity_age")]],
                                    coefs$race_gender[[which(coefs$race_gender=="Others"), c("beta_ethnicity_gender")]],
                                    coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Female"), c("beta_state_gender")]] + sigma_turnout*(coefs$state_gender[[which(coefs$state_gender$state == state & coefs$state_gender$options == "Female"),c("tau_state_gender")]]**-0.5),
                                    coefs$state_race[[which(coefs$state_race$state==state & coefs$state_race$options=="Other race"), c("beta_ethnicity_by_state")]],
                                    coefs$state_race[[which(coefs$state_race$state==state & coefs$state_race$options=="Asian"), c("beta_ethnicity_by_state")]]
       ))
}
```

``` r
# $age$tau_age
predict_turnout_for_state <- function(state, coefs, census_data, election_2016, beta0, sigma_turnout=0) {
  numerator <- 0.0;
  denominator <- 0.0;
  categories <- list("male", "female", "age_18_24", "age_25_29", "age_30_39", "age_40_49", "age_50_64", 
                     "age_retirees", "education_high_school_male", "education_high_school_female",
                     "education_some_college_male", "education_some_college_female",
                     "education_assoc_degree_male", "education_assoc_degree_female",
                     "education_college_grad_male", "education_college_grad_female", 
                     "education_postgrad_male", "education_postgrad_female", "white_male_18_29",
                     "white_male_30_44", "white_male_45_64", "white_male_retirees", "white_female_18_29",
                     "white_female_30_44", "white_female_45_64", "white_female_retirees", "black_male_18_29",
                     "black_male_30_44", "black_male_45_64", "black_male_retirees", "black_female_18_29",
                     "black_female_30_44", "black_female_45_64", "black_female_retirees",
                     "hispanic_male_18_29", "hispanic_male_30_44", "hispanic_male_45_64",
                     "hispanic_male_retirees", "hispanic_female_18_29", "hispanic_female_30_44",
                     "hispanic_female_45_64", "hispanic_female_retirees", "others_male_18_29",
                     "others_female_18_29", "others_male_30_44", "others_female_30_44", "others_male_45_64",
                     "others_female_45_64", "others_male_retirees", "others_female_retirees")
  relevant_coefs <- mrp_weights(state, coefs, beta0, sigma_turnout);
  for (cat in categories) {
    numerator <- numerator + sum(census_data[which(census_data$state == state), c(cat)])*invlogit(sum(relevant_coefs[[cat]]))
    denominator <- denominator + sum(census_data[which(census_data$state == state), c(cat)])
  }
  data.frame(state=state, 
             predicted_turnout = numerator/denominator, 
             actual_turnout = sum(election_2016[which(election_2016$state==state),]$totalvotes)/sum(census_data[which(census_data$state==state),]$voting_age))
}
```

``` r
predict_turnout <- function(exit_poll, election_2016, census_data, sigma_turnout) {
  election_data <- filter(election_2016, party=="democrat")
  coefs <- list(
    age = turnout_age_log_coefs(exit_poll, election_data, census_data),
    ed = turnout_education_log_coefs(exit_poll, election_data, census_data),
    race_age = turnout_ethnicity_age_log_coefs(exit_poll, election_data, census_data),
    race_gender = turnout_ethnicity_gender_log_coefs(exit_poll, election_data, census_data),
    state_race = turnout_ethnicity_state_log_coefs(exit_poll_df, election_data, census_data),
    state_age = turnout_state_age_log_coefs(exit_poll, election_data, census_data),
    state_gender = turnout_state_gender_log_coefs(exit_poll, election_data, census_data),
    state = turnout_state_log_coefs(election_data, census_data),
    state_ed = turnout_education_state_log_coefs(exit_poll, election_data, census_data)
  )
  results = data.frame();
  for(state in unique(exit_poll$state)) {
    if (state != "nation") {
      results <- rbind(results, predict_turnout_for_state(state, coefs, census_data, election_data, turnout_beta_0(), sigma_turnout))
    }
  }
  results
}
```

We can observe the RMSE for this, since

``` r
rmse <- function(data) {
  deltas <- (data$predicted_turnout - data$actual_turnout)**2
  sqrt(mean(deltas))
}
```

``` r
rmse(predict_turnout(exit_poll_df, election_2016, census_data, sigma_turnout=0))
```

    ## [1] 0.2038147

``` r
rmse(predict_turnout(exit_poll_df, election_2016, census_data, sigma_turnout=1))
```

    ## [1] 0.05272848

``` r
rmse(predict_turnout(exit_poll_df, election_2016, census_data, sigma_turnout=2))
```

    ## [1] 0.03785781

We can do a t-test to see if the predicted turnout differs significantly
from the observed turnout; since we have a “small sample” (`n = 28
< 30`), we need to also test for
normality.

``` r
turnout_data <- predict_turnout(exit_poll_df, election_2016, census_data, sigma_turnout=2)
shapiro.test(turnout_data$predicted_turnout - turnout_data$actual_turnout)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  turnout_data$predicted_turnout - turnout_data$actual_turnout
    ## W = 0.98437, p-value = 0.9388

We fail to reject the null hypothesis (the differences is “normal
enough”). Now we can perform the paired t-tests to see if there is any
significant difference between `predicted_turnout` and
`actual_turnout`:

``` r
t.test(turnout_data$predicted_turnout, turnout_data$actual_turnout, paired=TRUE)
```

    ## 
    ##  Paired t-test
    ## 
    ## data:  turnout_data$predicted_turnout and turnout_data$actual_turnout
    ## t = -0.96018, df = 27, p-value = 0.3455
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.02157941  0.00782105
    ## sample estimates:
    ## mean of the differences 
    ##            -0.006879179

We fail to reject the null hypothesis, since the p-value is greater than
`alpha = 0.01`. This seems to adequately describe the voter turnout.

``` r
kable(turnout_data)
```

<table>

<thead>

<tr>

<th style="text-align:left;">

state

</th>

<th style="text-align:right;">

predicted\_turnout

</th>

<th style="text-align:right;">

actual\_turnout

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Arizona

</td>

<td style="text-align:right;">

0.5541460

</td>

<td style="text-align:right;">

0.5145385

</td>

</tr>

<tr>

<td style="text-align:left;">

California

</td>

<td style="text-align:right;">

0.5331639

</td>

<td style="text-align:right;">

0.4805053

</td>

</tr>

<tr>

<td style="text-align:left;">

Colorado

</td>

<td style="text-align:right;">

0.6202840

</td>

<td style="text-align:right;">

0.6759404

</td>

</tr>

<tr>

<td style="text-align:left;">

Florida

</td>

<td style="text-align:right;">

0.5838546

</td>

<td style="text-align:right;">

0.5936435

</td>

</tr>

<tr>

<td style="text-align:left;">

Georgia

</td>

<td style="text-align:right;">

0.5607462

</td>

<td style="text-align:right;">

0.5411142

</td>

</tr>

<tr>

<td style="text-align:left;">

Illinois

</td>

<td style="text-align:right;">

0.5627644

</td>

<td style="text-align:right;">

0.5637031

</td>

</tr>

<tr>

<td style="text-align:left;">

Indiana

</td>

<td style="text-align:right;">

0.5638125

</td>

<td style="text-align:right;">

0.5461630

</td>

</tr>

<tr>

<td style="text-align:left;">

Iowa

</td>

<td style="text-align:right;">

0.6180999

</td>

<td style="text-align:right;">

0.6582521

</td>

</tr>

<tr>

<td style="text-align:left;">

Kentucky

</td>

<td style="text-align:right;">

0.5695295

</td>

<td style="text-align:right;">

0.5662928

</td>

</tr>

<tr>

<td style="text-align:left;">

Maine

</td>

<td style="text-align:right;">

0.6308730

</td>

<td style="text-align:right;">

0.6949979

</td>

</tr>

<tr>

<td style="text-align:left;">

Michigan

</td>

<td style="text-align:right;">

0.5956189

</td>

<td style="text-align:right;">

0.6247573

</td>

</tr>

<tr>

<td style="text-align:left;">

Minnesota

</td>

<td style="text-align:right;">

0.6313961

</td>

<td style="text-align:right;">

0.7063985

</td>

</tr>

<tr>

<td style="text-align:left;">

Missouri

</td>

<td style="text-align:right;">

0.5875649

</td>

<td style="text-align:right;">

0.6018576

</td>

</tr>

<tr>

<td style="text-align:left;">

Nevada

</td>

<td style="text-align:right;">

0.5516280

</td>

<td style="text-align:right;">

0.5175278

</td>

</tr>

<tr>

<td style="text-align:left;">

New Hampshire

</td>

<td style="text-align:right;">

0.6319065

</td>

<td style="text-align:right;">

0.7018191

</td>

</tr>

<tr>

<td style="text-align:left;">

New Jersey

</td>

<td style="text-align:right;">

0.5667018

</td>

<td style="text-align:right;">

0.5609971

</td>

</tr>

<tr>

<td style="text-align:left;">

New Mexico

</td>

<td style="text-align:right;">

0.5418639

</td>

<td style="text-align:right;">

0.5049715

</td>

</tr>

<tr>

<td style="text-align:left;">

New York

</td>

<td style="text-align:right;">

0.5407655

</td>

<td style="text-align:right;">

0.4981798

</td>

</tr>

<tr>

<td style="text-align:left;">

North Carolina

</td>

<td style="text-align:right;">

0.5980241

</td>

<td style="text-align:right;">

0.6195692

</td>

</tr>

<tr>

<td style="text-align:left;">

Ohio

</td>

<td style="text-align:right;">

0.5964281

</td>

<td style="text-align:right;">

0.6143330

</td>

</tr>

<tr>

<td style="text-align:left;">

Oregon

</td>

<td style="text-align:right;">

0.6031471

</td>

<td style="text-align:right;">

0.6412746

</td>

</tr>

<tr>

<td style="text-align:left;">

Pennsylvania

</td>

<td style="text-align:right;">

0.5945111

</td>

<td style="text-align:right;">

0.6067042

</td>

</tr>

<tr>

<td style="text-align:left;">

South Carolina

</td>

<td style="text-align:right;">

0.5601741

</td>

<td style="text-align:right;">

0.5609828

</td>

</tr>

<tr>

<td style="text-align:left;">

Texas

</td>

<td style="text-align:right;">

0.5233081

</td>

<td style="text-align:right;">

0.4524437

</td>

</tr>

<tr>

<td style="text-align:left;">

Utah

</td>

<td style="text-align:right;">

0.5638476

</td>

<td style="text-align:right;">

0.5537455

</td>

</tr>

<tr>

<td style="text-align:left;">

Virginia

</td>

<td style="text-align:right;">

0.5953478

</td>

<td style="text-align:right;">

0.6182760

</td>

</tr>

<tr>

<td style="text-align:left;">

Washington

</td>

<td style="text-align:right;">

0.5799902

</td>

<td style="text-align:right;">

0.5868491

</td>

</tr>

<tr>

<td style="text-align:left;">

Wisconsin

</td>

<td style="text-align:right;">

0.6219358

</td>

<td style="text-align:right;">

0.6682130

</td>

</tr>

</tbody>

</table>

# Estimating votes for Trump

We can use a similar scheme to compute the expected number of votes per
state for Trump. Explicitly: compute the approximate coefficients for
the logistic regression for people who turned out to vote, ended up
voting for Trump. We use a similar scheme as before. The expected number
of votes is computed similarly, with the numerator of the
post-stratified quotient changed from
`number_of_people_in_cat*probability_to_vote(category)` to multiply by
an additional factor of `probability_voted_for_trump(category)` using
the logistic regression we just constructed.

Fortunately, we can automate away most of the odious calculations.

``` r
total_votes <- 138846571
trump_votes <- 62984828
trump_beta_0 <- log(trump_votes/(total_votes - trump_votes))

log_coefs <- function(data, var, z = 2) {
  beta = as.symbol(paste0("beta_", as.character(var)))
  tau = as.symbol(paste0("tau_", as.character(var)))
  data %>% 
    group_by(options) %>%
    transmute(
      questions,state,
      sigma = wilson_width(0.01*Trump_perc, num_respondents, z),
      trump_numerator = 0.01*Trump_perc + 2*sigma,
      odds_ratio = ifelse(0==Clinton_perc, 0, trump_numerator/(max(1e-7, (0.01*Clinton_perc)))),
      # odds_ratio = trump_numerator/(1 - trump_numerator),
      !!beta := ifelse(odds_ratio==0 || !is.finite(odds_ratio), 0, log(odds_ratio) - trump_beta_0),
      !!tau := (trump_numerator/sigma)**2 + ((1 - trump_numerator)/(wilson_width(1 - 0.01*Trump_perc, num_respondents, z)))**2
    )
}
```

Now we can compute all the same coefficients as before, reusing quite a
bit of code. Nifty. We just need to go through computing:

``` r
trump_state_gender <- function(exit_poll) {
  log_coefs(filter(exit_poll, questions=="Gender", state != "nation"), "gender")
}

trump_education <- function(exit_poll) {
  log_coefs(filter(exit_poll, questions=="Education", state == "nation"), "education")
}

trump_age_helper <- function(exit_poll) {
  poll <- filter(exit_poll, questions=="Age")
  id <- sort(unique(poll$questions_id))[2]
  log_coefs(filter(exit_poll, questions_id == id), "age")
}

trump_age <- function(exit_poll) {
  trump_age_helper(filter(exit_poll, state=="nation"))
}

trump_race_age <- function(exit_poll) {
  log_coefs(filter(exit_poll, questions=="Age by race", state=="nation"), "ethnicity_age")
}

trump_race_gender <- function(exit_poll) {
  log_coefs(filter(exit_poll, questions=="Race and gender", state=="nation"), "ethnicity_gender")
}

trump_state_race <- function(exit_poll) {
  poll <- filter(exit_poll, questions=="Race", state!="nation")
  poll[is.na(poll)] <- 0
  results <- as.data.frame(unique(log_coefs(poll, "ethnicity_by_state")))
  results[is.na(results)] <- 0
  results$odds_ratio[which(!is.finite(results$odds_ratio))] <- 0
  results
}

trump_state_education <- function(exit_poll) {
  log_coefs(filter(exit_poll, questions=="Education", state != "nation"), "state_education")
}

trump_state_age <- function(exit_poll) {
  results <- data.frame()
  exit_poll[is.na(exit_poll)] <- 0
  for (state_iter in unique(exit_poll$state)) {
    if (state_iter != "nation") {
      state_results <- as.data.frame(trump_age_helper(filter(exit_poll, state==state_iter)))
      results <- rbind(results, state_results)
    }
  }
  results
}
```

We last need to compute the state effect (just the log of the ratio of
Trump votes to non-Trump votes per state), but also we need to subtract
out national effects and state effects from each coefficient computed
above.

``` r
subtract_national_effects <- function(state_coefs, national_coefs) {
  state_beta <- colnames(state_coefs)[grep("beta", colnames(state_coefs))]
  nat_beta <- colnames(national_coefs)[grep("beta", colnames(national_coefs))]
  for (option in unique(state_coefs$options)) {
    state_coefs[which(state_coefs$options==option), state_beta] <- state_coefs[which(state_coefs$options==option), state_beta] - national_coefs[[which(national_coefs$options==option), nat_beta]] - trump_beta_0
  }
  state_coefs
}
```

``` r
subtract_state_effects <- function(state_coefs, state_beta0) {
  beta <- colnames(state_coefs)[grep("beta", colnames(state_coefs))]
  for (state in unique(state_beta0$state)) {
    b0 <- state_beta0[[which(state_beta0$state==state), c("beta_state")]]
    state_coefs[which(state_coefs$state == state), beta] <- state_coefs[which(state_coefs$state == state), beta] - b0
  }
  state_coefs
}
```

``` r
state_beta <- function(election_2016) {
  election_2016 %>%
    filter(party=="republican", is.finite(candidatevotes)) %>%
    group_by(state) %>%
    transmute(beta_state = log(sum(candidatevotes)/sum(totalvotes)) - trump_beta_0) %>%
    unique %>%
    as.data.frame
}
```

## Predicting Trump Votes

Now, we are in a position to finally answer the question: how did Trump
win 2016? We compute the turnout:

``` r
# $age$tau_age
predict_trump_votes_for_state <- function(state, turnout_coefs, trump_coefs, census_data, election_2016, beta0, sigma_turnout, sigma_vote) {
  numerator <- 0.0;
  denominator <- 0.0;
  categories <- list("male", "female", "age_18_24", "age_25_29", "age_30_39", "age_40_49", "age_50_64", 
                     "age_retirees", "education_high_school_male", "education_high_school_female",
                     "education_some_college_male", "education_some_college_female",
                     "education_assoc_degree_male", "education_assoc_degree_female",
                     "education_college_grad_male", "education_college_grad_female", 
                     "education_postgrad_male", "education_postgrad_female", "white_male_18_29",
                     "white_male_30_44", "white_male_45_64", "white_male_retirees", "white_female_18_29",
                     "white_female_30_44", "white_female_45_64", "white_female_retirees", "black_male_18_29",
                     "black_male_30_44", "black_male_45_64", "black_male_retirees", "black_female_18_29",
                     "black_female_30_44", "black_female_45_64", "black_female_retirees",
                     "hispanic_male_18_29", "hispanic_male_30_44", "hispanic_male_45_64",
                     "hispanic_male_retirees", "hispanic_female_18_29", "hispanic_female_30_44",
                     "hispanic_female_45_64", "hispanic_female_retirees", "others_male_18_29",
                     "others_female_18_29", "others_male_30_44", "others_female_30_44", "others_male_45_64",
                     "others_female_45_64", "others_male_retirees", "others_female_retirees")
  mrp_turnout <- mrp_weights(state, turnout_coefs, beta_0, sigma_turnout);
  mrp_trump <- mrp_weights(state, trump_coefs, trump_beta_0, sigma_vote);
  for (cat in categories) {
    numerator <- numerator + sum(census_data[which(census_data$state == state), c(cat)])*invlogit(sum(mrp_turnout[[cat]]))*invlogit(sum(mrp_trump[[cat]]))
    denominator <- denominator + sum(census_data[which(census_data$state == state), c(cat)])
  }
  data.frame(state=state, 
             predicted_votes = numerator/denominator, 
             actual_votes = sum(election_2016[which(election_2016$state==state & election_2016$party=="republican"),]$candidatevotes)/sum(election_2016[which(election_2016$state==state & election_2016$party=="republican"),]$totalvotes))
}
```

``` r
predict_trump_votes <- function(exit_poll, election_2016, census_data, sigma_turnout = 0, sigma_vote = 0) {
  election_data <- filter(election_2016, party=="republican")
  state_beta0 <- state_beta(election_data)
  trump_coefs <- list(
    age = trump_age(exit_poll),
    ed = trump_education(exit_poll),
    race_age = trump_race_age(exit_poll),
    race_gender = trump_race_gender(exit_poll),
    state = state_beta0,
    state_race = subtract_state_effects(trump_state_race(exit_poll), state_beta0),
    state_age = subtract_state_effects(subtract_national_effects(trump_state_age(exit_poll), trump_age(exit_poll)), state_beta0),
    state_gender = subtract_state_effects(trump_state_gender(exit_poll), state_beta0),
    state_ed = subtract_state_effects(subtract_national_effects(trump_state_education(exit_poll), trump_education(exit_poll)), state_beta0)
  )
  turnout_coefs <- list(
    age = turnout_age_log_coefs(exit_poll, election_data, census_data),
    ed = turnout_education_log_coefs(exit_poll, election_data, census_data),
    race_age = turnout_ethnicity_age_log_coefs(exit_poll, election_data, census_data),
    race_gender = turnout_ethnicity_gender_log_coefs(exit_poll, election_data, census_data),
    state_race = turnout_ethnicity_state_log_coefs(exit_poll_df, election_data, census_data),
    state_age = turnout_state_age_log_coefs(exit_poll, election_data, census_data),
    state_gender = turnout_state_gender_log_coefs(exit_poll, election_data, census_data),
    state = turnout_state_log_coefs(election_data, census_data),
    state_ed = turnout_education_state_log_coefs(exit_poll, election_data, census_data)
  )
  results = data.frame();
  for(state in unique(exit_poll$state)) {
    if (state != "nation") {
      results <- rbind(results, predict_trump_votes_for_state(state, turnout_coefs, trump_coefs, census_data, election_data, turnout_beta_0(), sigma_turnout, sigma_vote))
    }
  }
  results
}
```

We have the RMSE:

``` r
votes_rmse <- function(data) {
  deltas <- (data$predicted_votes - data$actual_votes)**2
  sqrt(mean(deltas))
}

predict_trump_votes(exit_poll_df, election_2016, census_data, 2, 500)
```

    ##             state predicted_votes actual_votes
    ## 1         Arizona       0.4435226    0.4764234
    ## 2      California       0.4172556    0.3161711
    ## 3        Colorado       0.4805971    0.4325140
    ## 4         Florida       0.4647590    0.4902194
    ## 5         Georgia       0.4516357    0.5077159
    ## 6        Illinois       0.4412401    0.3860637
    ## 7         Indiana       0.4584066    0.5694003
    ## 8            Iowa       0.4905753    0.5114733
    ## 9        Kentucky       0.4658317    0.6251964
    ## 10          Maine       0.4930058    0.4511016
    ## 11       Michigan       0.4706608    0.4749756
    ## 12      Minnesota       0.4922720    0.4492479
    ## 13       Missouri       0.4740337    0.5679710
    ## 14         Nevada       0.4418486    0.4550070
    ## 15  New Hampshire       0.4935703    0.4645867
    ## 16     New Jersey       0.4466476    0.4135039
    ## 17     New Mexico       0.4303389    0.4004251
    ## 18       New York       0.4268075    0.3651818
    ## 19 North Carolina       0.4753240    0.4982809
    ## 20           Ohio       0.4760668    0.5168765
    ## 21         Oregon       0.4666134    0.3909404
    ## 22   Pennsylvania       0.4727110    0.4857789
    ## 23 South Carolina       0.4507137    0.5493933
    ## 24          Texas       0.4291216    0.5223469
    ## 25           Utah       0.4492502    0.4553804
    ## 26       Virginia       0.4683357           NA
    ## 27     Washington       0.4444020    0.3806998
    ## 28      Wisconsin       0.4895676    0.4719612

This actually quantifies how surprising an event this was — a 500-sigma
event. But it also gives us some idea of how far off the exit polls
were, which is unsurprising since it is a noisy and useless source. At
the risk of sounding immodest, the amazing thing is we Macguyvered this
thing together.
