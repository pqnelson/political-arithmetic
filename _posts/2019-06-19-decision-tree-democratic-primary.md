  - [Overview](#overview)

# Overview

I want to create a decision tree to guide the user to determine who is
likely to win the nomination.

``` r
.data <- as.data.frame(tribble(
  ~name,    ~year, ~iowa, ~new_hampshire, ~nevada, ~south_carolina, ~won_nomination,
  "Clinton", 2016,     1,              0,       1,               1,               1,
  "Sanders", 2016,     0,              1,       0,               0,               0,
  "Obama",   2008,     1,              0,       1,               1,               1,
  "Edwards", 2008,     0,              0,       0,               0,               0,
  "Clinton", 2008,     0,              0,       0,               0,               0,
  "Kerry",   2004,     1,              1,       1,               0,               1,
  "Edwards", 2004,     1,              0,       0,               1,               0,
  "Harkin",  1992,     1,              0,       0,               0,               0,
  "Tsongas", 1992,     0,              1,       1,               0,               0,
  "Clinton", 1992,     0,              1,       1,               1,               1
))
```

``` r
tree_fit <- ctree(won_nomination ~ iowa + new_hampshire + nevada + south_carolina, data = .data)
```

``` r
rtree_fit <- rpart(won_nomination ~ iowa + new_hampshire + nevada + south_carolina, .data, method = "class", model = T, control = rpart.control(minsplit = 2))
rpart.plot(rtree_fit)
```

![](2019-06-19-decision-tree-democratic-primary_files/figure-gfm/unnamed-chunk-66-1.png)<!-- -->

We should read this tree as stating three rows in each outcome bubble:

1.  Did the candidate become the nominee? `0` for “No”, `1` for “Yes”
2.  The probability of this outcome (0.00 for “impossible to become
    nominee” to 1.00 for “almost certainly the nominee”)
3.  The percentage of data points which fall in the outcome.
