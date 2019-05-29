We are really just interested in computing the hypergeometric
distribution given `K=22`, `k=5`, and `n=12`. Well, using the
conventions chosen by R, we have

``` r
prob_dist <- function(N) {
  return(dhyper(5, m=22, n=N-22, k=12));
}
```

![](2019-05-28-news-stories_files/figure-gfm/pressure-1.png)<!-- -->

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
