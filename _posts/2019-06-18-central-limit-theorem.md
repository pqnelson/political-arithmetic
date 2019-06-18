  - [Plotting](#plotting)
  - [Testing](#testing)
  - [Conclusion](#conclusion)

This is a quick attempt to reproduce the results from:

  - Zachary R. Smith and Craig S. Wells’s [Central Limit Theorem and
    Sample
    Size](https://www.umass.edu/remp/Papers/Smith&Wells_NERA06.pdf)

We begin with three probability distributions: Normal (`dnorm(mean = 50,
sd = 10)`)), Uniform (`dunif(10,30)`), Bimodal, and then we will
consider a family of skewed distributions.

> From each of the three populations, 10,000 samples of size 5, 10, 15,
> 20, 25, and 30 were randomly drawn, rounding to the nearest integer,
> as is often observed in educational and psychological measures. The
> mean for each sample was then computed, using four decimal places.
> Twenty replications were performed for each condition (i.e., the
> process was repeated 20 times for each sample size).
> 
> After the sampling distribution for the mean was constructed for a
> replication, a one sample Kolmogorov-Smirnov (KS) test was used to
> test whether the sample mean followed a normal distribution. This test
> was used on each sample size taken from all distributions. The
> proportion of replications in which the distribution of the sample
> means were concluded to depart normality according to the KS-test was
> recorded.

So lets start sampling in the manner described.

``` r
sample_normal <- function(sample_size, N = 20, mean = 50, sd = 10) {
  replicate(N, mean(round(rnorm(sample_size, mean, sd))));
}

sample_unif <- function(sample_size, N=20, lower_bound = 10, upper_bound = 30) {
  replicate(N, mean(round(runif(sample_size, lower_bound, upper_bound))))
}

sample_bimodal <- function(sample_size, N=20, theta = 0.75) {
  samples = c();
  for (i in 1:N) {
    w = rbinom(sample_size, 1, theta);
    samples <- append(samples, mean(round(w*rnorm(sample_size, 50, 10) + (1 - w)*rnorm(sample_size, 85, 10))));
  }
  samples
}

fleisman <- function(n, a, b, c, d, mean = 50, sd = 10) {
  x <- rnorm(n, mean, sd)
  a + x*(b + x*(c + d*x))
}
coefs <- list(list(-0.399, 0.930, 0.399, -0.036),
              list(-0.221, 0.866, 0.221, 0.027),
              list(-0.161, 0.819, 0.161, 0.049),
              list(-0.119, 0.789, 0.119, 0.062));


sample_fleisman <- function(sample_size, k, N=20, theta = 0.75) {
  a <- coefs[[k]][[1]]
  b <- coefs[[k]][[2]]
  c <- coefs[[k]][[3]]
  d <- coefs[[k]][[4]]
  replicate(N, mean(round(fleisman(sample_size, a, b, c, d))));
}
```

# Plotting

Before continuing on, we can plot some histograms to see what the data
looks
like.

``` r
qplot(sample_bimodal(5, N=200), geom="histogram", fill=I("#008FD5"), ylab="Bimodal(5, N=200)") +
  scale_color_fivethirtyeight() +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) + xlab("Bimodal(5, N=200)") + ylab("density")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](2019-06-18-central-limit-theorem_files/figure-gfm/unnamed-chunk-53-1.png)<!-- -->

``` r
qplot(sample_bimodal(10, N=200), geom="histogram", fill=I("#008FD5")) +
  scale_color_fivethirtyeight() +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) + xlab("Bimodal(10, N=200)") + ylab("density")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](2019-06-18-central-limit-theorem_files/figure-gfm/unnamed-chunk-54-1.png)<!-- -->

``` r
qplot(sample_bimodal(30, N=200), geom="histogram", fill=I("#008FD5")) +
  scale_color_fivethirtyeight() +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) + xlab("Bimodal(30, N=200)") + ylab("density")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](2019-06-18-central-limit-theorem_files/figure-gfm/unnamed-chunk-55-1.png)<!-- -->

``` r
qplot(sample_fleisman(5, 4, N=200), geom="histogram", fill=I("#FF2700")) +
  scale_color_fivethirtyeight() +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) + xlab("Fleisman4(5, N=200)") + ylab("density")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](2019-06-18-central-limit-theorem_files/figure-gfm/unnamed-chunk-56-1.png)<!-- -->

# Testing

Perfect, now we are in a state to imitate the testing procedure:

> Samples of 5 through 300 at intervals of 5 were drawn with no rounding
> from each of the four skewed distributions over 10,000 repetitions.
> After the transformation, the sampling means were rounded to the
> fourth decimal place. Once the sampling distribution for the mean is
> complete, the one sample Kolmogorov-Smirnov (KS) test was applied.

We implement this thus:

``` r
results <- data.frame(uniform = rep(NA,6),
                      normal = rep(NA,6),
                      bimodal = rep(NA,6),
                      fleisman1 = rep(NA,6),
                      fleisman2 = rep(NA,6),
                      fleisman3 = rep(NA,6),
                      fleisman4 = rep(NA,6))
# sidak significance = 1 - (1 - 0.05)**(1e-4);
significance_level <- 0.05;
for (i in 1:6) {
  print(i)
  results$uniform[i] <- length(Filter(function(p) {p < significance_level}, replicate(10000, shapiro.test(sample_unif(5*i))$p)))
  results$normal[i] <- length(Filter(function(p) {p < significance_level}, replicate(10000, shapiro.test(sample_normal(5*i))$p)))
  results$bimodal[i] <- length(Filter(function(p) {p < significance_level}, replicate(10000, shapiro.test(sample_bimodal(5*i))$p)))
  results$fleisman1[i] <- length(Filter(function(p) {p < significance_level}, replicate(10000, shapiro.test(sample_fleisman(5*i, 1))$p)))
  results$fleisman2[i] <- length(Filter(function(p) {p < significance_level}, replicate(10000, shapiro.test(sample_fleisman(5*i, 2))$p)))
  results$fleisman3[i] <- length(Filter(function(p) {p < significance_level}, replicate(10000, shapiro.test(sample_fleisman(5*i, 3))$p)))
  results$fleisman4[i] <- length(Filter(function(p) {p < significance_level}, replicate(10000, shapiro.test(sample_fleisman(5*i, 4))$p)))
}
```

    ## [1] 1
    ## [1] 2
    ## [1] 3
    ## [1] 4
    ## [1] 5
    ## [1] 6

``` r
kable(results)
```

<table>

<thead>

<tr>

<th style="text-align:right;">

uniform

</th>

<th style="text-align:right;">

normal

</th>

<th style="text-align:right;">

bimodal

</th>

<th style="text-align:right;">

fleisman1

</th>

<th style="text-align:right;">

fleisman2

</th>

<th style="text-align:right;">

fleisman3

</th>

<th style="text-align:right;">

fleisman4

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:right;">

419

</td>

<td style="text-align:right;">

552

</td>

<td style="text-align:right;">

630

</td>

<td style="text-align:right;">

1248

</td>

<td style="text-align:right;">

1074

</td>

<td style="text-align:right;">

1123

</td>

<td style="text-align:right;">

1084

</td>

</tr>

<tr>

<td style="text-align:right;">

430

</td>

<td style="text-align:right;">

525

</td>

<td style="text-align:right;">

567

</td>

<td style="text-align:right;">

903

</td>

<td style="text-align:right;">

805

</td>

<td style="text-align:right;">

818

</td>

<td style="text-align:right;">

772

</td>

</tr>

<tr>

<td style="text-align:right;">

455

</td>

<td style="text-align:right;">

499

</td>

<td style="text-align:right;">

547

</td>

<td style="text-align:right;">

805

</td>

<td style="text-align:right;">

677

</td>

<td style="text-align:right;">

714

</td>

<td style="text-align:right;">

734

</td>

</tr>

<tr>

<td style="text-align:right;">

498

</td>

<td style="text-align:right;">

439

</td>

<td style="text-align:right;">

537

</td>

<td style="text-align:right;">

663

</td>

<td style="text-align:right;">

635

</td>

<td style="text-align:right;">

653

</td>

<td style="text-align:right;">

675

</td>

</tr>

<tr>

<td style="text-align:right;">

469

</td>

<td style="text-align:right;">

527

</td>

<td style="text-align:right;">

526

</td>

<td style="text-align:right;">

647

</td>

<td style="text-align:right;">

619

</td>

<td style="text-align:right;">

594

</td>

<td style="text-align:right;">

655

</td>

</tr>

<tr>

<td style="text-align:right;">

466

</td>

<td style="text-align:right;">

461

</td>

<td style="text-align:right;">

496

</td>

<td style="text-align:right;">

594

</td>

<td style="text-align:right;">

550

</td>

<td style="text-align:right;">

587

</td>

<td style="text-align:right;">

628

</td>

</tr>

</tbody>

</table>

This is nothing at all what Smith and Wells report…

# Conclusion

The central limit theorem works, the world is safe, and we can all sleep
well again. AND DO NOT CITE THIS DAMN PAPER\!
