  - [Introduction](#introduction)
      - [Grouping Pair-wise
        Correlations](#grouping-pair-wise-correlations)

# Introduction

The goal is to try to cluster states based on geographic proximity and
similar voting tendency. The first thing we do is load the data.

``` r
load("../data/elections/senate/1976_2018_senate.RData")
senate_data <- x %>%
  filter(stage == "gen") %>%
  mutate(party = ifelse((party == "democrat" | party == "democratic-farmer-labor") & writein == F, "democrat",
                        ifelse(party == "republican" & writein == F, party, "$third")))

load("../data/elections/house/1976_2018_house.RData")
house_data <- x %>%
  filter(stage == "gen") %>%
  mutate(party = ifelse((party == "democrat" | party == "democratic-farmer-labor") & writein == F, "democrat",
                        ifelse(party == "republican" & writein == F, party, "$third")))

load("../data/elections/presidential/state/1976_2016_president.RData")
president_data <- x %>%
  mutate(party = ifelse((party == "democrat" | party == "democratic-farmer-labor") & writein == F, "democrat",
                        ifelse(party == "republican" & writein == F, party, "$third"))) %>%
  group_by(state, year) %>%
  mutate(totalvotes = sum(candidatevotes)) %>%
  ungroup
```

We now need to work our way to produce a triple for each state.

``` r
house <- house_data %>%
  filter(special == F) %>%
  group_by(state, party, district, year) %>%
  summarize(candidatevotes = sum(candidatevotes),
            totalvotes = totalvotes[[1]]) %>%
  spread(party, candidatevotes, fill = 0) %>%
  select(-`<NA>`) %>%
  mutate(third = `$third`/totalvotes,
         dem = democrat/totalvotes,
         rep = republican/totalvotes)

senate <- senate_data %>%
  filter(special == F) %>%
  group_by(state, year, party, totalvotes) %>%
  summarize(candidatevotes = sum(candidatevotes)) %>%
  ungroup() %>%
  spread(party, candidatevotes, fill = 0) %>%
  select(-`<NA>`) %>%
  mutate(third = `$third`/totalvotes,
         dem = democrat/totalvotes,
         rep = republican/totalvotes)

president <- president_data %>% 
  group_by(state, year, party, totalvotes) %>%
  summarize(candidatevotes = sum(candidatevotes)) %>%
  ungroup() %>%
  spread(party, candidatevotes, fill = 0) %>%
  select(-`<NA>`) %>%
  mutate(third = `$third`/totalvotes,
         dem = democrat/totalvotes,
         rep = republican/totalvotes)
```

We create a vector for a given state representing the `third`, `dem`,
`rep` percents ordered in time. Then these vectors are used to compute
the correlations between states, pair-wise.

``` r
state_to_vec <- function(data, state_name) {
  (data %>%
    filter(state == state_name) %>%
    gather("party", "percent", third, dem, rep) %>%
    select(state, year, party, percent) %>%
    arrange(year, party))$percent
}
```

``` r
states <- factor(sort(unique(president$state)))
state_vecs <- list()
for (state in states) {
  # state_vecs <- append(state_vecs, list(state = state_to_vec(president, state)))
  state_vecs[[as.character(state)]] <- state_to_vec(president, state)
}
```

``` r
cor_mat <- data.frame(from = c(),
                      to = c(),
                      val = c())
cor_mat_spearman <- data.frame(from = c(),
                      to = c(),
                      val = c())
for (i in 1:50) {
  from <- states[i]
  for (j in (1 + i):51) {
    to <- states[j]
    
    val <- cor(state_vecs[[i]], state_vecs[[j]], method = "spearman")
    cor_mat_spearman <- rbind(cor_mat_spearman, data.frame(from = from,
                                         to = to,
                                         val = val))
    
    val <- cor(state_vecs[[i]], state_vecs[[j]])
    cor_mat <- rbind(cor_mat, data.frame(from = from,
                                         to = to,
                                         val = val))
  }
}
```

``` r
ggplot(cor_mat_spearman, aes(x=val)) + 
  geom_histogram(color="black", fill="white")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](2019-07-25-political-geography_files/figure-gfm/spearman_histogram-1.png)<!-- -->

``` r
ggplot(cor_mat, aes(x=val)) + 
  geom_histogram(color="black", fill="white")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](2019-07-25-political-geography_files/figure-gfm/pearson_histogram-1.png)<!-- -->

## Grouping Pair-wise Correlations

``` r
mat <- matrix(0, 51, 51)
edges <- filter(cor_mat, val > quantile(cor_mat$val, probs = c(0.925))[1])
for (i in 1:51) {
  relevant_edges <- rbind(filter(edges, from == states[i], to %in% neighboring_states[[states[i]]]),
                          filter(edges, to == states[i], from %in% neighboring_states[[states[i]]]))
  
  for (j in relevant_edges$from) {
    to <- factor(j, states)
    mat[i, as.integer(to)] <- 1
    mat[as.integer(to), i] <- 1
  }
  for (j in relevant_edges$to) {
    to <- factor(j, states)
    mat[i, as.integer(to)] <- 1
    mat[as.integer(to), i] <- 1
  }
}
rownames(mat) <- states
colnames(mat) <- states

tr_ad <- transitiveClosure(mat)
rownames(tr_ad) <- states
colnames(tr_ad) <- states
tr_ad0 <- tr_ad

for (i in 1:51) {
  if (all(tr_ad[i,] == 0) & length(neighboring_states[[states[i]]]) > 0) {
    print(i)
    choices <- rbind(filter(cor_mat, from == states[i], to %in% neighboring_states[[states[i]]]),
                     filter(cor_mat, to == states[i], from %in% neighboring_states[[states[i]]]))
    to <- (choices %>% arrange(-val))$to[1]
    print(head((choices %>% arrange(-val))))
    if (to == states[i]) {
      to <- factor((choices %>% arrange(-val))$from[1], states)
    } else {
      to <- factor(to, states)
    }
    tr_ad[i, as.integer(to)] <- 1
    tr_ad[as.integer(to), i] <- 1    
  }
}
```

    ## [1] 3
    ##      from         to       val
    ## 1 Arizona   Colorado 0.9826450
    ## 2 Arizona     Nevada 0.9819895
    ## 3 Arizona New Mexico 0.9560578
    ## 4 Arizona California 0.9066506
    ## 5 Arizona       Utah 0.8905851
    ## [1] 5
    ##         from         to       val
    ## 1 California     Oregon 0.9841654
    ## 2 California     Nevada 0.9486876
    ## 3    Arizona California 0.9066506
    ## [1] 7
    ##          from            to       val
    ## 1 Connecticut      New York 0.9652274
    ## 2 Connecticut  Rhode Island 0.9650341
    ## 3 Connecticut Massachusetts 0.9650027
    ## [1] 9
    ##                   from       to       val
    ## 1 District of Columbia Maryland 0.7364861
    ## 2 District of Columbia Virginia 0.4567931
    ## [1] 10
    ##      from      to       val
    ## 1 Florida Georgia 0.9606005
    ## 2 Alabama Florida 0.9503022
    ## [1] 15
    ##       from       to       val
    ## 1  Indiana     Ohio 0.9793351
    ## 2  Indiana Kentucky 0.9774857
    ## 3  Indiana Michigan 0.9523052
    ## 4 Illinois  Indiana 0.9027231
    ## [1] 20
    ##    from            to       val
    ## 1 Maine New Hampshire 0.9654459
    ## [1] 26
    ##       from        to       val
    ## 1     Iowa  Missouri 0.9816815
    ## 2 Missouri Tennessee 0.9812899
    ## 3 Kentucky  Missouri 0.9810118
    ## 4 Arkansas  Missouri 0.9684993
    ## 5   Kansas  Missouri 0.9543109
    ## 6 Illinois  Missouri 0.9415298
    ## [1] 28
    ##       from           to       val
    ## 1   Kansas     Nebraska 0.9592088
    ## 2 Nebraska South Dakota 0.9520224
    ## 3 Colorado     Nebraska 0.9331155
    ## 4 Nebraska      Wyoming 0.9299419
    ## 5 Missouri     Nebraska 0.9015656
    ## 6     Iowa     Nebraska 0.8801388
    ## [1] 29
    ##         from     to       val
    ## 1    Arizona Nevada 0.9819895
    ## 2     Nevada Oregon 0.9605642
    ## 3 California Nevada 0.9486876
    ## 4      Idaho Nevada 0.8792567
    ## 5     Nevada   Utah 0.8282175
    ## [1] 44
    ##         from    to       val
    ## 1  Louisiana Texas 0.9821465
    ## 2   Oklahoma Texas 0.9741975
    ## 3   Arkansas Texas 0.9519246
    ## 4 New Mexico Texas 0.9446500
    ## [1] 45
    ##         from      to       val
    ## 1      Idaho    Utah 0.9815920
    ## 2       Utah Wyoming 0.9634379
    ## 3    Arizona    Utah 0.8905851
    ## 4     Nevada    Utah 0.8282175
    ## 5   Colorado    Utah 0.8271037
    ## 6 New Mexico    Utah 0.7631080
    ## [1] 46
    ##            from      to       val
    ## 1      New York Vermont 0.9743776
    ## 2 Massachusetts Vermont 0.9638466
    ## 3 New Hampshire Vermont 0.9144187
    ## [1] 49
    ##           from            to       val
    ## 1     Kentucky West Virginia 0.9827426
    ## 2         Ohio West Virginia 0.9432166
    ## 3 Pennsylvania West Virginia 0.9344479
    ## 4     Virginia West Virginia 0.9094865
    ## 5     Maryland West Virginia 0.8375146

``` r
tr_ad <- transitiveClosure(tr_ad)
rownames(tr_ad) <- states
colnames(tr_ad) <- states

ci <- 1
clusters <- data.frame(region = c(),
                       cluster = c())
for (i in 1:51) {
  others <- rownames(as.matrix(Filter(function(x) {x > 0}, tr_ad[i, ])))
  if (length(others) > 0 & !(tolower(as.character(states[i])) %in% clusters$region)) {
    for(other in sort(append(others, as.character(states[i])))) {
      clusters <- rbind(clusters, data.frame(region = c(tolower(as.character(other))),
                                             cluster = c(ci)))
    }
    ci <- ci + 1
  }
}
clusters$cluster <- factor(clusters$cluster)

us_states <- map_data("state")
clustered_states <- us_states %>%
  inner_join(clusters, by = "region")
```

    ## Warning: Column `region` joining character vector and factor, coercing into
    ## character vector

``` r
red <- "#FF2700"
orange <- "#ff7400"
green <- "#77AB43"
blue <- "#008FD5"
party_colors <- c(blue, # 1
                  orange, # 2
                  green, # 3
                  blue, # 4
                  green, # 5
                  red, # 6
                  blue, # 7 
                  orange, # 8
                  green, # 9
                  red, # 10
                  red, # 11
                  blue) # 12

ggplot(data = clustered_states,
            mapping = aes(x = long, y = lat, 
                          fill = cluster, group = group)) +
  geom_polygon(color = "gray90", size = 0.1) + 
  coord_fixed(1.3) +
  scale_fill_manual(values = party_colors) +
  guides(fill = FALSE) + # no legend!
  labs(caption="http://political-arithmetic.blogspot.com",
       title= "State Clusters", 
       subtitle="Clustering states based on Presidential voting behavior 1976-2016")
```

![](2019-07-25-political-geography_files/figure-gfm/clustered_map-1.png)<!-- -->
