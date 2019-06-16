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
    transmute(beta_state_age = log(proportion/(1 - proportion)) - (state_effect[which(state_effect$state %in% state),]$beta_state - beta_0) - beta_0,
              tau_state_age = 2*(2*width/center)**-2
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

beta\_state\_age

</th>

<th style="text-align:right;">

tau\_state\_age

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

\-2.8349808

</td>

<td style="text-align:right;">

0.0073794

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

\-0.4302131

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

\-2.0373214

</td>

<td style="text-align:right;">

0.0033518

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

0.0068289

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

\-1.6579568

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

\-1.9352648

</td>

<td style="text-align:right;">

0.0044948

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

0.0092639

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

\-1.1072204

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

\-1.5073440

</td>

<td style="text-align:right;">

0.0090575

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

0.0177220

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

\-0.7510910

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

\-1.9729481

</td>

<td style="text-align:right;">

0.0122292

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

0.0141065

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

\-0.5368719

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

\-1.7446144

</td>

<td style="text-align:right;">

0.0084437

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

0.0064820

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

\-0.6282474

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

\-1.3212699

</td>

<td style="text-align:right;">

0.0044400

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

0.0106585

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

\-0.5572220

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

\-1.8483879

</td>

<td style="text-align:right;">

0.0048944

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

0.0136561

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

\-1.0275573

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

\-2.3839914

</td>

<td style="text-align:right;">

0.0074242

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

0.0128537

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

\-0.6387843

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

\-1.6747387

</td>

<td style="text-align:right;">

0.0037328

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

0.0169280

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

\-1.1956328

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

\-1.7800495

</td>

<td style="text-align:right;">

0.0112326

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

0.0178260

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

\-0.8818332

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

\-2.2543481

</td>

<td style="text-align:right;">

0.0064710

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

0.0053434

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

\-1.2499991

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

\-2.5052879

</td>

<td style="text-align:right;">

0.0046619

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

0.0155240

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

\-0.7852539

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

\-1.8068547

</td>

<td style="text-align:right;">

0.0068563

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

0.0146238

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

\-0.4421829

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

\-2.1305132

</td>

<td style="text-align:right;">

0.0045330

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

0.0101057

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

\-1.2280182

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

\-2.3301839

</td>

<td style="text-align:right;">

0.0095563

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

0.0034521

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

\-4.0865819

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

\-1.9360802

</td>

<td style="text-align:right;">

0.0044069

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

0.0147073

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

\-0.3919294

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

\-1.8222807

</td>

<td style="text-align:right;">

0.0045465

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

0.0054695

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

\-2.9141060

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

\-1.3743016

</td>

<td style="text-align:right;">

0.0057134

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

0.0229596

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

\-0.8597627

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

\-2.0146589

</td>

<td style="text-align:right;">

0.0136363

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

0.0228238

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

\-0.8376056

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

\-1.9545963

</td>

<td style="text-align:right;">

0.0107330

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

0.0096737

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

\-0.9529435

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

\-1.9116136

</td>

<td style="text-align:right;">

0.0042430

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

0.0234078

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

\-0.8055223

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

\-1.1718421

</td>

<td style="text-align:right;">

0.0191070

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

0.0068408

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

\-0.6171946

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

\-1.5636715

</td>

<td style="text-align:right;">

0.0033206

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

0.0102070

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

\-0.1812410

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

\-1.7641303

</td>

<td style="text-align:right;">

0.0054509

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

0.0036880

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

\-0.5878587

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

\-2.3925931

</td>

<td style="text-align:right;">

0.0016194

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

0.0110066

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

\-3.6523176

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

\-2.0791508

</td>

<td style="text-align:right;">

0.0088109

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

0.0057058

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

\-0.7229975

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

\-1.7207863

</td>

<td style="text-align:right;">

0.0040410

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

0.0164271

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

\-1.0721567

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

\-2.2050533

</td>

<td style="text-align:right;">

0.0094695

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
