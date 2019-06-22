  - [Vanilla moving average](#vanilla-moving-average)
  - [Weighing the score of the
    pollsters](#weighing-the-score-of-the-pollsters)

The first thing we do is load the 538 collected poll data.

``` r
poll_df <- read_csv("../data/primary/2020/president_primary_polls.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character(),
    ##   question_id = col_double(),
    ##   poll_id = col_double(),
    ##   cycle = col_double(),
    ##   pollster_id = col_double(),
    ##   sponsor_ids = col_number(),
    ##   pollster_rating_id = col_double(),
    ##   sample_size = col_double(),
    ##   sponsor_candidate = col_logical(),
    ##   internal = col_logical(),
    ##   tracking = col_logical(),
    ##   nationwide_batch = col_logical(),
    ##   pct = col_double()
    ## )

    ## See spec(...) for full column specifications.

``` r
poll_df$start_date <- as.Date(poll_df$start_date, "%m/%d/%y")
poll_df$end_date <- as.Date(poll_df$end_date, "%m/%d/%y")
poll_2020_df <- filter(poll_df, cycle==2020, party=="DEM")
```

Now we can arrange by the `end_date` for the poll, which places the
earlier polls first and the more recent polls at the end of the data
frame. This sets us up nicely to use a running average for the poll for
each candidate.

# Vanilla moving average

We implement the “naive” moving
average.

``` r
alpha <- function(prev_date, next_date, avg_interval, normalizer = 30, ...) {
  1 - exp(-abs(as.numeric(next_date - prev_date))/(normalizer * avg_interval))
}
```

We will have a utility function for computing the average interval
between poll releases.

``` r
average_interval <- function(poll_data) {
  abs(mean(as.numeric(diff(poll_data$end_date))))
}
```

The moving average is a little sad, it’s just what we’d
expect.

``` r
moving_average <- function(poll_data, weighting_interval, alpha_fn = alpha) {
  assert_that(length(unique(poll_data$cycle)) == 1)
  poll_data < arrange(poll_data, end_date)
  avg_interval <- average_interval(poll_data)
  avg <- poll_data$pct[1]
  prev_dt <- poll_data$end_date[1]
  for (i in 2:nrow(poll_data)) {
    a <- alpha_fn(prev_dt, poll_data$end_date[i], avg_interval, weighting_interval, poll_data$fte_grade[i])
    avg <- a*poll_data$pct[i] + (1 - a)*avg
    prev_dt <- poll_data$end_date[i]
  }
  avg
}

compute_moving_avgs <- function(poll_data, weighting_interval = 30) {
  assert_that(length(unique(poll_data$cycle)) == 1)
  
  poll_data %>%
    group_by(candidate_name) %>%
    filter(n() > 2, !is.na(fte_grade), !is.na(sponsors), state=="Iowa") %>%
    nest() %>%
    transmute(candidate_name,
              moving_avg = map(data, moving_average, weighting_interval)
              ) %>%
    unnest() %>%
    arrange(-moving_avg)
}
```

We can now present a table of running averages by month:

``` r
kable(compute_moving_avgs(poll_2020_df))
```

<table>

<thead>

<tr>

<th style="text-align:left;">

candidate\_name

</th>

<th style="text-align:right;">

moving\_avg

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Joseph R. Biden Jr. 

</td>

<td style="text-align:right;">

29.9573542

</td>

</tr>

<tr>

<td style="text-align:left;">

Bernard Sanders

</td>

<td style="text-align:right;">

21.4225938

</td>

</tr>

<tr>

<td style="text-align:left;">

Elizabeth Warren

</td>

<td style="text-align:right;">

11.4856540

</td>

</tr>

<tr>

<td style="text-align:left;">

Pete Buttigieg

</td>

<td style="text-align:right;">

10.3400012

</td>

</tr>

<tr>

<td style="text-align:left;">

Kamala D. Harris

</td>

<td style="text-align:right;">

5.0934454

</td>

</tr>

<tr>

<td style="text-align:left;">

Beto O’Rourke

</td>

<td style="text-align:right;">

4.6853307

</td>

</tr>

<tr>

<td style="text-align:left;">

Amy Klobuchar

</td>

<td style="text-align:right;">

3.9877000

</td>

</tr>

<tr>

<td style="text-align:left;">

Cory A. Booker

</td>

<td style="text-align:right;">

3.0891274

</td>

</tr>

<tr>

<td style="text-align:left;">

John K. Delaney

</td>

<td style="text-align:right;">

1.8751733

</td>

</tr>

<tr>

<td style="text-align:left;">

Jay Robert Inslee

</td>

<td style="text-align:right;">

0.9406898

</td>

</tr>

<tr>

<td style="text-align:left;">

Tim Ryan

</td>

<td style="text-align:right;">

0.9355070

</td>

</tr>

<tr>

<td style="text-align:left;">

Tulsi Gabbard

</td>

<td style="text-align:right;">

0.9272703

</td>

</tr>

<tr>

<td style="text-align:left;">

Steve Bullock

</td>

<td style="text-align:right;">

0.9250796

</td>

</tr>

<tr>

<td style="text-align:left;">

Kirsten E. Gillibrand

</td>

<td style="text-align:right;">

0.8862591

</td>

</tr>

<tr>

<td style="text-align:left;">

Julián Castro

</td>

<td style="text-align:right;">

0.1137409

</td>

</tr>

<tr>

<td style="text-align:left;">

Michael Bloomberg

</td>

<td style="text-align:right;">

0.0983517

</td>

</tr>

<tr>

<td style="text-align:left;">

Michael F. Bennet

</td>

<td style="text-align:right;">

0.0792160

</td>

</tr>

<tr>

<td style="text-align:left;">

John Hickenlooper

</td>

<td style="text-align:right;">

0.0703959

</td>

</tr>

<tr>

<td style="text-align:left;">

Andrew Yang

</td>

<td style="text-align:right;">

0.0266960

</td>

</tr>

<tr>

<td style="text-align:left;">

Eric Swalwell

</td>

<td style="text-align:right;">

0.0110857

</td>

</tr>

<tr>

<td style="text-align:left;">

Marianne Williamson

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Bill de Blasio

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Seth Moulton

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Wayne Messam

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Eric H. Holder

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Mike Gravel

</td>

<td style="text-align:right;">

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

Stacey Yvonne Abrams

</td>

<td style="text-align:right;">

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

Sherrod Brown

</td>

<td style="text-align:right;">

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

Eric Garcetti

</td>

<td style="text-align:right;">

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

Tom Steyer

</td>

<td style="text-align:right;">

NA

</td>

</tr>

</tbody>

</table>

And if we choose instead a week:

``` r
kable(compute_moving_avgs(poll_2020_df, 7))
```

<table>

<thead>

<tr>

<th style="text-align:left;">

candidate\_name

</th>

<th style="text-align:right;">

moving\_avg

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Joseph R. Biden Jr. 

</td>

<td style="text-align:right;">

30.0340144

</td>

</tr>

<tr>

<td style="text-align:left;">

Bernard Sanders

</td>

<td style="text-align:right;">

20.1234993

</td>

</tr>

<tr>

<td style="text-align:left;">

Elizabeth Warren

</td>

<td style="text-align:right;">

10.2189779

</td>

</tr>

<tr>

<td style="text-align:left;">

Pete Buttigieg

</td>

<td style="text-align:right;">

8.4515550

</td>

</tr>

<tr>

<td style="text-align:left;">

Beto O’Rourke

</td>

<td style="text-align:right;">

6.4599181

</td>

</tr>

<tr>

<td style="text-align:left;">

Kamala D. Harris

</td>

<td style="text-align:right;">

5.2523202

</td>

</tr>

<tr>

<td style="text-align:left;">

Amy Klobuchar

</td>

<td style="text-align:right;">

3.9130175

</td>

</tr>

<tr>

<td style="text-align:left;">

Cory A. Booker

</td>

<td style="text-align:right;">

3.3394145

</td>

</tr>

<tr>

<td style="text-align:left;">

John K. Delaney

</td>

<td style="text-align:right;">

1.5647181

</td>

</tr>

<tr>

<td style="text-align:left;">

Jay Robert Inslee

</td>

<td style="text-align:right;">

0.7694831

</td>

</tr>

<tr>

<td style="text-align:left;">

Tim Ryan

</td>

<td style="text-align:right;">

0.7514773

</td>

</tr>

<tr>

<td style="text-align:left;">

Steve Bullock

</td>

<td style="text-align:right;">

0.7250319

</td>

</tr>

<tr>

<td style="text-align:left;">

Tulsi Gabbard

</td>

<td style="text-align:right;">

0.7235286

</td>

</tr>

<tr>

<td style="text-align:left;">

Kirsten E. Gillibrand

</td>

<td style="text-align:right;">

0.5965504

</td>

</tr>

<tr>

<td style="text-align:left;">

Julián Castro

</td>

<td style="text-align:right;">

0.4034496

</td>

</tr>

<tr>

<td style="text-align:left;">

Michael Bloomberg

</td>

<td style="text-align:right;">

0.3993663

</td>

</tr>

<tr>

<td style="text-align:left;">

Michael F. Bennet

</td>

<td style="text-align:right;">

0.2967218

</td>

</tr>

<tr>

<td style="text-align:left;">

John Hickenlooper

</td>

<td style="text-align:right;">

0.2623492

</td>

</tr>

<tr>

<td style="text-align:left;">

Andrew Yang

</td>

<td style="text-align:right;">

0.0762835

</td>

</tr>

<tr>

<td style="text-align:left;">

Eric Swalwell

</td>

<td style="text-align:right;">

0.0318323

</td>

</tr>

<tr>

<td style="text-align:left;">

Marianne Williamson

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Bill de Blasio

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Seth Moulton

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Wayne Messam

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Eric H. Holder

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Mike Gravel

</td>

<td style="text-align:right;">

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

Stacey Yvonne Abrams

</td>

<td style="text-align:right;">

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

Sherrod Brown

</td>

<td style="text-align:right;">

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

Eric Garcetti

</td>

<td style="text-align:right;">

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

Tom Steyer

</td>

<td style="text-align:right;">

NA

</td>

</tr>

</tbody>

</table>

``` r
kable(compute_moving_avgs(poll_2020_df, 1))
```

<table>

<thead>

<tr>

<th style="text-align:left;">

candidate\_name

</th>

<th style="text-align:right;">

moving\_avg

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Joseph R. Biden Jr. 

</td>

<td style="text-align:right;">

31.6276490

</td>

</tr>

<tr>

<td style="text-align:left;">

Bernard Sanders

</td>

<td style="text-align:right;">

18.9110613

</td>

</tr>

<tr>

<td style="text-align:left;">

Beto O’Rourke

</td>

<td style="text-align:right;">

10.5144161

</td>

</tr>

<tr>

<td style="text-align:left;">

Elizabeth Warren

</td>

<td style="text-align:right;">

8.0788425

</td>

</tr>

<tr>

<td style="text-align:left;">

Kamala D. Harris

</td>

<td style="text-align:right;">

5.0718321

</td>

</tr>

<tr>

<td style="text-align:left;">

Cory A. Booker

</td>

<td style="text-align:right;">

3.9520211

</td>

</tr>

<tr>

<td style="text-align:left;">

Amy Klobuchar

</td>

<td style="text-align:right;">

3.1974336

</td>

</tr>

<tr>

<td style="text-align:left;">

Pete Buttigieg

</td>

<td style="text-align:right;">

2.2000161

</td>

</tr>

<tr>

<td style="text-align:left;">

Michael Bloomberg

</td>

<td style="text-align:right;">

1.8963617

</td>

</tr>

<tr>

<td style="text-align:left;">

John K. Delaney

</td>

<td style="text-align:right;">

1.0183156

</td>

</tr>

<tr>

<td style="text-align:left;">

Julián Castro

</td>

<td style="text-align:right;">

0.9719352

</td>

</tr>

<tr>

<td style="text-align:left;">

Michael F. Bennet

</td>

<td style="text-align:right;">

0.9080997

</td>

</tr>

<tr>

<td style="text-align:left;">

John Hickenlooper

</td>

<td style="text-align:right;">

0.8500166

</td>

</tr>

<tr>

<td style="text-align:left;">

Jay Robert Inslee

</td>

<td style="text-align:right;">

0.1597326

</td>

</tr>

<tr>

<td style="text-align:left;">

Steve Bullock

</td>

<td style="text-align:right;">

0.1469192

</td>

</tr>

<tr>

<td style="text-align:left;">

Tim Ryan

</td>

<td style="text-align:right;">

0.1353353

</td>

</tr>

<tr>

<td style="text-align:left;">

Tulsi Gabbard

</td>

<td style="text-align:right;">

0.1037983

</td>

</tr>

<tr>

<td style="text-align:left;">

Kirsten E. Gillibrand

</td>

<td style="text-align:right;">

0.0280648

</td>

</tr>

<tr>

<td style="text-align:left;">

Andrew Yang

</td>

<td style="text-align:right;">

0.0225626

</td>

</tr>

<tr>

<td style="text-align:left;">

Eric Swalwell

</td>

<td style="text-align:right;">

0.0097492

</td>

</tr>

<tr>

<td style="text-align:left;">

Marianne Williamson

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Bill de Blasio

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Seth Moulton

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Wayne Messam

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Eric H. Holder

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Mike Gravel

</td>

<td style="text-align:right;">

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

Stacey Yvonne Abrams

</td>

<td style="text-align:right;">

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

Sherrod Brown

</td>

<td style="text-align:right;">

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

Eric Garcetti

</td>

<td style="text-align:right;">

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

Tom Steyer

</td>

<td style="text-align:right;">

NA

</td>

</tr>

</tbody>

</table>

``` r
kable(compute_moving_avgs(poll_2020_df, 45))
```

<table>

<thead>

<tr>

<th style="text-align:left;">

candidate\_name

</th>

<th style="text-align:right;">

moving\_avg

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Joseph R. Biden Jr. 

</td>

<td style="text-align:right;">

29.9669870

</td>

</tr>

<tr>

<td style="text-align:left;">

Bernard Sanders

</td>

<td style="text-align:right;">

21.6036386

</td>

</tr>

<tr>

<td style="text-align:left;">

Elizabeth Warren

</td>

<td style="text-align:right;">

11.6492786

</td>

</tr>

<tr>

<td style="text-align:left;">

Pete Buttigieg

</td>

<td style="text-align:right;">

10.5552562

</td>

</tr>

<tr>

<td style="text-align:left;">

Kamala D. Harris

</td>

<td style="text-align:right;">

5.0653175

</td>

</tr>

<tr>

<td style="text-align:left;">

Beto O’Rourke

</td>

<td style="text-align:right;">

4.4655882

</td>

</tr>

<tr>

<td style="text-align:left;">

Amy Klobuchar

</td>

<td style="text-align:right;">

3.9924629

</td>

</tr>

<tr>

<td style="text-align:left;">

Cory A. Booker

</td>

<td style="text-align:right;">

3.0600811

</td>

</tr>

<tr>

<td style="text-align:left;">

John K. Delaney

</td>

<td style="text-align:right;">

1.9149472

</td>

</tr>

<tr>

<td style="text-align:left;">

Jay Robert Inslee

</td>

<td style="text-align:right;">

0.9600584

</td>

</tr>

<tr>

<td style="text-align:left;">

Tim Ryan

</td>

<td style="text-align:right;">

0.9565287

</td>

</tr>

<tr>

<td style="text-align:left;">

Tulsi Gabbard

</td>

<td style="text-align:right;">

0.9509059

</td>

</tr>

<tr>

<td style="text-align:left;">

Steve Bullock

</td>

<td style="text-align:right;">

0.9492107

</td>

</tr>

<tr>

<td style="text-align:left;">

Kirsten E. Gillibrand

</td>

<td style="text-align:right;">

0.9226442

</td>

</tr>

<tr>

<td style="text-align:left;">

Julián Castro

</td>

<td style="text-align:right;">

0.0773558

</td>

</tr>

<tr>

<td style="text-align:left;">

Michael Bloomberg

</td>

<td style="text-align:right;">

0.0659314

</td>

</tr>

<tr>

<td style="text-align:left;">

Michael F. Bennet

</td>

<td style="text-align:right;">

0.0535595

</td>

</tr>

<tr>

<td style="text-align:left;">

John Hickenlooper

</td>

<td style="text-align:right;">

0.0476386

</td>

</tr>

<tr>

<td style="text-align:left;">

Andrew Yang

</td>

<td style="text-align:right;">

0.0185447

</td>

</tr>

<tr>

<td style="text-align:left;">

Eric Swalwell

</td>

<td style="text-align:right;">

0.0076970

</td>

</tr>

<tr>

<td style="text-align:left;">

Marianne Williamson

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Bill de Blasio

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Seth Moulton

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Wayne Messam

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Eric H. Holder

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Mike Gravel

</td>

<td style="text-align:right;">

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

Stacey Yvonne Abrams

</td>

<td style="text-align:right;">

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

Sherrod Brown

</td>

<td style="text-align:right;">

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

Eric Garcetti

</td>

<td style="text-align:right;">

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

Tom Steyer

</td>

<td style="text-align:right;">

NA

</td>

</tr>

</tbody>

</table>

# Weighing the score of the pollsters

There are a lot of garbage polls, so we need to apply a weighting scheme
to them.

``` r
grade_pollster <- function(fte_grade) {
  lookup <- list("A+" = 13/13,
                 "A"  = 12/13,
                 "A-" = 11/13,
                 "B+" = 10/13,
                 "B"  =  9/39,
                 "B-" =  8/39,
                 "C+" =  6/39,
                 "C"  =  5/39,
                 "C-" =  4/39,
                 "D+" =  3/39,
                 "D"  =  2/39,
                 "D-" =  1/39)
  ifelse(is.null(lookup[[fte_grade]]), 0, lookup[[fte_grade]])
}
```

Now we apply this to our new alpha
function.

``` r
weighted_alpha <- function(prev_date, next_date, avg_interval, normalizer, fte_grade, ...) {
  grade_pollster(fte_grade)*alpha(prev_date, next_date, avg_interval, normalizer)
}

compute_weighted_moving_avgs <- function(poll_data, weighting_interval = 30) {
  assert_that(length(unique(poll_data$cycle)) == 1)
  
  poll_data %>%
    group_by(candidate_name) %>%
    filter(n() > 2, !is.na(fte_grade), !is.na(sponsors), state=="Iowa") %>%
    nest() %>%
    transmute(candidate_name,
              moving_avg = map(data, moving_average, weighting_interval, weighted_alpha)
              ) %>%
    unnest() %>%
    arrange(-moving_avg)
}

kable(compute_weighted_moving_avgs(poll_2020_df, 7))
```

<table>

<thead>

<tr>

<th style="text-align:left;">

candidate\_name

</th>

<th style="text-align:right;">

moving\_avg

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Joseph R. Biden Jr. 

</td>

<td style="text-align:right;">

30.3006826

</td>

</tr>

<tr>

<td style="text-align:left;">

Bernard Sanders

</td>

<td style="text-align:right;">

20.8897710

</td>

</tr>

<tr>

<td style="text-align:left;">

Elizabeth Warren

</td>

<td style="text-align:right;">

10.7154023

</td>

</tr>

<tr>

<td style="text-align:left;">

Pete Buttigieg

</td>

<td style="text-align:right;">

8.3239305

</td>

</tr>

<tr>

<td style="text-align:left;">

Beto O’Rourke

</td>

<td style="text-align:right;">

6.0602779

</td>

</tr>

<tr>

<td style="text-align:left;">

Kamala D. Harris

</td>

<td style="text-align:right;">

5.1187633

</td>

</tr>

<tr>

<td style="text-align:left;">

Amy Klobuchar

</td>

<td style="text-align:right;">

3.7194492

</td>

</tr>

<tr>

<td style="text-align:left;">

Cory A. Booker

</td>

<td style="text-align:right;">

3.2668995

</td>

</tr>

<tr>

<td style="text-align:left;">

John K. Delaney

</td>

<td style="text-align:right;">

1.5910644

</td>

</tr>

<tr>

<td style="text-align:left;">

Tim Ryan

</td>

<td style="text-align:right;">

0.8941110

</td>

</tr>

<tr>

<td style="text-align:left;">

Jay Robert Inslee

</td>

<td style="text-align:right;">

0.7694831

</td>

</tr>

<tr>

<td style="text-align:left;">

Steve Bullock

</td>

<td style="text-align:right;">

0.7513782

</td>

</tr>

<tr>

<td style="text-align:left;">

Tulsi Gabbard

</td>

<td style="text-align:right;">

0.7235286

</td>

</tr>

<tr>

<td style="text-align:left;">

Kirsten E. Gillibrand

</td>

<td style="text-align:right;">

0.5959617

</td>

</tr>

<tr>

<td style="text-align:left;">

Julián Castro

</td>

<td style="text-align:right;">

0.4040383

</td>

</tr>

<tr>

<td style="text-align:left;">

Michael Bloomberg

</td>

<td style="text-align:right;">

0.3993663

</td>

</tr>

<tr>

<td style="text-align:left;">

Michael F. Bennet

</td>

<td style="text-align:right;">

0.2980442

</td>

</tr>

<tr>

<td style="text-align:left;">

John Hickenlooper

</td>

<td style="text-align:right;">

0.2354142

</td>

</tr>

<tr>

<td style="text-align:left;">

Andrew Yang

</td>

<td style="text-align:right;">

0.0230022

</td>

</tr>

<tr>

<td style="text-align:left;">

Eric Swalwell

</td>

<td style="text-align:right;">

0.0048973

</td>

</tr>

<tr>

<td style="text-align:left;">

Marianne Williamson

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Bill de Blasio

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Seth Moulton

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Wayne Messam

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Eric H. Holder

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Mike Gravel

</td>

<td style="text-align:right;">

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

Stacey Yvonne Abrams

</td>

<td style="text-align:right;">

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

Sherrod Brown

</td>

<td style="text-align:right;">

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

Eric Garcetti

</td>

<td style="text-align:right;">

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

Tom Steyer

</td>

<td style="text-align:right;">

NA

</td>

</tr>

</tbody>

</table>

It seems the larger the window for remembering events, the smaller the
lead Biden has, until we weigh the pollster ratings.

``` r
kable(compute_weighted_moving_avgs(poll_2020_df))
```

<table>

<thead>

<tr>

<th style="text-align:left;">

candidate\_name

</th>

<th style="text-align:right;">

moving\_avg

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Joseph R. Biden Jr. 

</td>

<td style="text-align:right;">

30.0640480

</td>

</tr>

<tr>

<td style="text-align:left;">

Bernard Sanders

</td>

<td style="text-align:right;">

21.6854446

</td>

</tr>

<tr>

<td style="text-align:left;">

Elizabeth Warren

</td>

<td style="text-align:right;">

11.6509021

</td>

</tr>

<tr>

<td style="text-align:left;">

Pete Buttigieg

</td>

<td style="text-align:right;">

10.2998095

</td>

</tr>

<tr>

<td style="text-align:left;">

Kamala D. Harris

</td>

<td style="text-align:right;">

5.0377416

</td>

</tr>

<tr>

<td style="text-align:left;">

Beto O’Rourke

</td>

<td style="text-align:right;">

4.5502035

</td>

</tr>

<tr>

<td style="text-align:left;">

Amy Klobuchar

</td>

<td style="text-align:right;">

3.9274584

</td>

</tr>

<tr>

<td style="text-align:left;">

Cory A. Booker

</td>

<td style="text-align:right;">

3.0699456

</td>

</tr>

<tr>

<td style="text-align:left;">

John K. Delaney

</td>

<td style="text-align:right;">

1.8845053

</td>

</tr>

<tr>

<td style="text-align:left;">

Tim Ryan

</td>

<td style="text-align:right;">

0.9737839

</td>

</tr>

<tr>

<td style="text-align:left;">

Jay Robert Inslee

</td>

<td style="text-align:right;">

0.9406898

</td>

</tr>

<tr>

<td style="text-align:left;">

Steve Bullock

</td>

<td style="text-align:right;">

0.9344116

</td>

</tr>

<tr>

<td style="text-align:left;">

Tulsi Gabbard

</td>

<td style="text-align:right;">

0.9272703

</td>

</tr>

<tr>

<td style="text-align:left;">

Kirsten E. Gillibrand

</td>

<td style="text-align:right;">

0.8862108

</td>

</tr>

<tr>

<td style="text-align:left;">

Julián Castro

</td>

<td style="text-align:right;">

0.1137892

</td>

</tr>

<tr>

<td style="text-align:left;">

Michael Bloomberg

</td>

<td style="text-align:right;">

0.0983517

</td>

</tr>

<tr>

<td style="text-align:left;">

Michael F. Bennet

</td>

<td style="text-align:right;">

0.0793120

</td>

</tr>

<tr>

<td style="text-align:left;">

John Hickenlooper

</td>

<td style="text-align:right;">

0.0610157

</td>

</tr>

<tr>

<td style="text-align:left;">

Andrew Yang

</td>

<td style="text-align:right;">

0.0079838

</td>

</tr>

<tr>

<td style="text-align:left;">

Eric Swalwell

</td>

<td style="text-align:right;">

0.0017055

</td>

</tr>

<tr>

<td style="text-align:left;">

Marianne Williamson

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Bill de Blasio

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Seth Moulton

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Wayne Messam

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Eric H. Holder

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Mike Gravel

</td>

<td style="text-align:right;">

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

Stacey Yvonne Abrams

</td>

<td style="text-align:right;">

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

Sherrod Brown

</td>

<td style="text-align:right;">

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

Eric Garcetti

</td>

<td style="text-align:right;">

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

Tom Steyer

</td>

<td style="text-align:right;">

NA

</td>

</tr>

</tbody>

</table>
