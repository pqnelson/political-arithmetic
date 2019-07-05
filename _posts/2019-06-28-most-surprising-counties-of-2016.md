  - [Statistically Significant
    Surprises](#statistically-significant-surprises)
  - [Picking Out the Relevant States](#picking-out-the-relevant-states)
  - [Surprising Counties](#surprising-counties)
  - [Overperformed…compared to what?](#overperformedcompared-to-what)
  - [Third Party Performance: 2016 vs
    2012](#third-party-performance-2016-vs-2012)
      - [Johnson’s Performance](#johnsons-performance)
      - [Stein’s Performance](#steins-performance)

We can measure surprise using the Kullback–Leibler divergence, niftily
provided by the `LaplacesDemon::KLD()` function. This quantifies how
surprising the 2016 election results were, given we were assuming a
result more closely resembling 2012.

``` r
county_results <- make_party_into_factor(load_obj(county_path))
election_2004 <- filter(county_results, year == 2004)
election_2008 <- filter(county_results, year == 2008)
election_2012 <- filter(county_results, year == 2012)
election_2016 <- filter(county_results, year == 2016)
```

Now, we treat each county like a three-sided coin, with the proportion
of the vote as a proxy for probability.

``` r
conj_prob <- function(election) {
  prop <- election %>%
    mutate(party = ifelse(party == "democrat", party,
                          ifelse(party == "republican", party,
                                 "$third-party"))) %>%
    group_by(year,state,county,party,FIPS) %>%
    transmute(probability = candidatevotes/totalvotes) %>%
    ungroup() %>%
    unique
  prop$probability[is.na(prop$probability)] <- 0
  prop
}

prop_2004 <- conj_prob(election_2004)
prop_2008 <- conj_prob(election_2008)
prop_2012 <- conj_prob(election_2012)
prop_2016 <- conj_prob(election_2016)

probabilities2008 <- inner_join(select(prop_2004, -year),
                                select(prop_2008, -year),
                                by = c("state", "county", "party"))

probabilities2012 <- inner_join(select(prop_2008, -year),
                                select(prop_2012, -year),
                                by = c("state", "county", "party"))

probabilities2016 <- inner_join(select(prop_2012, -year),
                                select(prop_2016, -year),
                                by = c("state", "county", "party"))
```

We will now try to assign to each county the measure of surprise.

``` r
compute_surprise <- function(data) {
  KLD(data$probability.x, data$probability.y, base=2)$sum.KLD.py.px
}

election_surprise <- function(probs) {
  probs %>%
    filter(party != "NA") %>%
    group_by(state,county) %>%
    nest() %>%
    mutate(surprise = map(data, compute_surprise)) %>%
    unnest(surprise, .drop = TRUE) %>%
    arrange(-surprise)
}

results08 <- election_surprise(probabilities2008)
results12 <- election_surprise(probabilities2012)
results16 <- election_surprise(probabilities2016)
```

## Statistically Significant Surprises

Note that Blair County, PA has a surprise of 0.01; did it vote
differently than 2012 with any statistical significance (with alpha =
0.05, say):

``` r
blair_pa_2012 <- election_2012 %>%
  filter(state == "Pennsylvania", county == "Blair", party == "republican")  %>%
  transmute(p = candidatevotes/totalvotes)

blair_pa_2016 <- election_2016 %>%
  filter(state == "Pennsylvania", county == "Blair", party == "republican")  %>%
  transmute(x = candidatevotes,
            n = totalvotes)

prop.test(blair_pa_2016$x, blair_pa_2016$n, blair_pa_2012$p)
```

    ## 
    ##  1-sample proportions test with continuity correction
    ## 
    ## data:  blair_pa_2016$x out of blair_pa_2016$n, null probability blair_pa_2012$p
    ## X-squared = 603.1, df = 1, p-value < 2.2e-16
    ## alternative hypothesis: true p is not equal to 0.6631835
    ## 95 percent confidence interval:
    ##  0.7089160 0.7165035
    ## sample estimates:
    ##         p 
    ## 0.7127247

The `p-value < 2.2e-16`, which tells us, yes, Blair county behaved
differently in 2016 with statistical significance.

We could try generalizing this. Going county-by-county will be no good.
We will instead work with state-level results, then examine each state
for statistically different voting patterns among specifically
Republicans.

``` r
state_results <- make_party_into_factor(load_obj(state_path))
election_2012 <- filter(state_results, year == 2012)
election_2016 <- filter(state_results, year == 2016)
prop_2012 <- election_2012 %>%
  group_by(year,state,party) %>%
  transmute(probability = candidatevotes/totalvotes) %>%
  ungroup()
# prop_2012$probability[is.na(prop_2012$probability)] <- 0
```

We are interested in the situation where Trump did better than
Republicans historically perform, as compared to the past race in 2012.
This can be approximated using the p-value from the Binomial
test.

``` r
data <- inner_join(filter(election_2016, party == "republican", writein == F),
                   filter(prop_2012, party == "republican"),
                   by = c("state","party")) %>%
  filter(!is.na(totalvotes), is.state(state)) %>%
  select(state,candidatevotes,totalvotes,probability) %>%
  group_by(state) %>%
  mutate(trump_2016 = candidatevotes/totalvotes,
         p.value = binom.test(x = candidatevotes, n = totalvotes, p = probability, alternative = "greater")$p.value) %>%
  ungroup() %>%
  rename(republican_2012 = probability) %>%
  arrange(p.value)

kable(data)
```

<table>

<thead>

<tr>

<th style="text-align:left;">

state

</th>

<th style="text-align:right;">

candidatevotes

</th>

<th style="text-align:right;">

totalvotes

</th>

<th style="text-align:right;">

republican\_2012

</th>

<th style="text-align:right;">

trump\_2016

</th>

<th style="text-align:right;">

p.value

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Alabama

</td>

<td style="text-align:right;">

1318255

</td>

<td style="text-align:right;">

2123372

</td>

<td style="text-align:right;">

0.6054582

</td>

<td style="text-align:right;">

0.6208309

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Indiana

</td>

<td style="text-align:right;">

1557286

</td>

<td style="text-align:right;">

2734958

</td>

<td style="text-align:right;">

0.5412553

</td>

<td style="text-align:right;">

0.5694003

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Iowa

</td>

<td style="text-align:right;">

800983

</td>

<td style="text-align:right;">

1565580

</td>

<td style="text-align:right;">

0.4617787

</td>

<td style="text-align:right;">

0.5116206

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Kentucky

</td>

<td style="text-align:right;">

1202971

</td>

<td style="text-align:right;">

1924149

</td>

<td style="text-align:right;">

0.6049314

</td>

<td style="text-align:right;">

0.6251964

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Maine

</td>

<td style="text-align:right;">

335593

</td>

<td style="text-align:right;">

771892

</td>

<td style="text-align:right;">

0.4032739

</td>

<td style="text-align:right;">

0.4347668

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Michigan

</td>

<td style="text-align:right;">

2279543

</td>

<td style="text-align:right;">

4799284

</td>

<td style="text-align:right;">

0.4471092

</td>

<td style="text-align:right;">

0.4749756

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Mississippi

</td>

<td style="text-align:right;">

700714

</td>

<td style="text-align:right;">

1209357

</td>

<td style="text-align:right;">

0.5528585

</td>

<td style="text-align:right;">

0.5794104

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Missouri

</td>

<td style="text-align:right;">

1594511

</td>

<td style="text-align:right;">

2808605

</td>

<td style="text-align:right;">

0.5376374

</td>

<td style="text-align:right;">

0.5677235

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Nebraska

</td>

<td style="text-align:right;">

495961

</td>

<td style="text-align:right;">

844227

</td>

<td style="text-align:right;">

0.3802731

</td>

<td style="text-align:right;">

0.5874735

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

New York

</td>

<td style="text-align:right;">

2527142

</td>

<td style="text-align:right;">

7802084

</td>

<td style="text-align:right;">

0.3124160

</td>

<td style="text-align:right;">

0.3239060

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

North Dakota

</td>

<td style="text-align:right;">

216794

</td>

<td style="text-align:right;">

344360

</td>

<td style="text-align:right;">

0.5831568

</td>

<td style="text-align:right;">

0.6295563

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Ohio

</td>

<td style="text-align:right;">

2841005

</td>

<td style="text-align:right;">

5496487

</td>

<td style="text-align:right;">

0.4768844

</td>

<td style="text-align:right;">

0.5168765

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Pennsylvania

</td>

<td style="text-align:right;">

2970733

</td>

<td style="text-align:right;">

6115402

</td>

<td style="text-align:right;">

0.4668087

</td>

<td style="text-align:right;">

0.4857789

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Rhode Island

</td>

<td style="text-align:right;">

180543

</td>

<td style="text-align:right;">

464144

</td>

<td style="text-align:right;">

0.3524366

</td>

<td style="text-align:right;">

0.3889806

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

South Dakota

</td>

<td style="text-align:right;">

227721

</td>

<td style="text-align:right;">

370093

</td>

<td style="text-align:right;">

0.5788931

</td>

<td style="text-align:right;">

0.6153075

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Tennessee

</td>

<td style="text-align:right;">

1522925

</td>

<td style="text-align:right;">

2508027

</td>

<td style="text-align:right;">

0.5947871

</td>

<td style="text-align:right;">

0.6072203

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

West Virginia

</td>

<td style="text-align:right;">

489371

</td>

<td style="text-align:right;">

713051

</td>

<td style="text-align:right;">

0.6229584

</td>

<td style="text-align:right;">

0.6863057

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Wisconsin

</td>

<td style="text-align:right;">

1405284

</td>

<td style="text-align:right;">

2976150

</td>

<td style="text-align:right;">

0.4593835

</td>

<td style="text-align:right;">

0.4721818

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

New Jersey

</td>

<td style="text-align:right;">

1601933

</td>

<td style="text-align:right;">

3874046

</td>

<td style="text-align:right;">

0.4062356

</td>

<td style="text-align:right;">

0.4135039

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Delaware

</td>

<td style="text-align:right;">

185127

</td>

<td style="text-align:right;">

441590

</td>

<td style="text-align:right;">

0.3998260

</td>

<td style="text-align:right;">

0.4192282

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Hawaii

</td>

<td style="text-align:right;">

128847

</td>

<td style="text-align:right;">

437664

</td>

<td style="text-align:right;">

0.2768215

</td>

<td style="text-align:right;">

0.2943971

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Montana

</td>

<td style="text-align:right;">

279240

</td>

<td style="text-align:right;">

494526

</td>

<td style="text-align:right;">

0.5535154

</td>

<td style="text-align:right;">

0.5646619

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

South Carolina

</td>

<td style="text-align:right;">

1155389

</td>

<td style="text-align:right;">

2103027

</td>

<td style="text-align:right;">

0.5456113

</td>

<td style="text-align:right;">

0.5493933

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Louisiana

</td>

<td style="text-align:right;">

1178638

</td>

<td style="text-align:right;">

2029032

</td>

<td style="text-align:right;">

0.5778458

</td>

<td style="text-align:right;">

0.5808868

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Connecticut

</td>

<td style="text-align:right;">

673215

</td>

<td style="text-align:right;">

1644920

</td>

<td style="text-align:right;">

0.4074511

</td>

<td style="text-align:right;">

0.4092691

</td>

<td style="text-align:right;">

0.0000011

</td>

</tr>

<tr>

<td style="text-align:left;">

New Hampshire

</td>

<td style="text-align:right;">

345790

</td>

<td style="text-align:right;">

744296

</td>

<td style="text-align:right;">

0.4640380

</td>

<td style="text-align:right;">

0.4645867

</td>

<td style="text-align:right;">

0.1715487

</td>

</tr>

<tr>

<td style="text-align:left;">

Arkansas

</td>

<td style="text-align:right;">

684872

</td>

<td style="text-align:right;">

1130635

</td>

<td style="text-align:right;">

0.6056694

</td>

<td style="text-align:right;">

0.6057410

</td>

<td style="text-align:right;">

0.4384544

</td>

</tr>

<tr>

<td style="text-align:left;">

Minnesota

</td>

<td style="text-align:right;">

1322949

</td>

<td style="text-align:right;">

2944782

</td>

<td style="text-align:right;">

0.4495820

</td>

<td style="text-align:right;">

0.4492519

</td>

<td style="text-align:right;">

0.8727014

</td>

</tr>

<tr>

<td style="text-align:left;">

Nevada

</td>

<td style="text-align:right;">

512058

</td>

<td style="text-align:right;">

1125385

</td>

<td style="text-align:right;">

0.4567532

</td>

<td style="text-align:right;">

0.4550070

</td>

<td style="text-align:right;">

0.9999004

</td>

</tr>

<tr>

<td style="text-align:left;">

Florida

</td>

<td style="text-align:right;">

4617886

</td>

<td style="text-align:right;">

9420039

</td>

<td style="text-align:right;">

0.4913098

</td>

<td style="text-align:right;">

0.4902194

</td>

<td style="text-align:right;">

1.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Alaska

</td>

<td style="text-align:right;">

163387

</td>

<td style="text-align:right;">

318608

</td>

<td style="text-align:right;">

0.5480158

</td>

<td style="text-align:right;">

0.5128151

</td>

<td style="text-align:right;">

1.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Arizona

</td>

<td style="text-align:right;">

1252401

</td>

<td style="text-align:right;">

2573165

</td>

<td style="text-align:right;">

0.5365453

</td>

<td style="text-align:right;">

0.4867162

</td>

<td style="text-align:right;">

1.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

California

</td>

<td style="text-align:right;">

4483810

</td>

<td style="text-align:right;">

14181595

</td>

<td style="text-align:right;">

0.3712038

</td>

<td style="text-align:right;">

0.3161711

</td>

<td style="text-align:right;">

1.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Colorado

</td>

<td style="text-align:right;">

1202484

</td>

<td style="text-align:right;">

2780220

</td>

<td style="text-align:right;">

0.4612709

</td>

<td style="text-align:right;">

0.4325140

</td>

<td style="text-align:right;">

1.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Georgia

</td>

<td style="text-align:right;">

2089104

</td>

<td style="text-align:right;">

4114732

</td>

<td style="text-align:right;">

0.5332924

</td>

<td style="text-align:right;">

0.5077133

</td>

<td style="text-align:right;">

1.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Idaho

</td>

<td style="text-align:right;">

409055

</td>

<td style="text-align:right;">

690255

</td>

<td style="text-align:right;">

0.6452978

</td>

<td style="text-align:right;">

0.5926143

</td>

<td style="text-align:right;">

1.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Illinois

</td>

<td style="text-align:right;">

2146015

</td>

<td style="text-align:right;">

5536424

</td>

<td style="text-align:right;">

0.4073274

</td>

<td style="text-align:right;">

0.3876175

</td>

<td style="text-align:right;">

1.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Kansas

</td>

<td style="text-align:right;">

671018

</td>

<td style="text-align:right;">

1184402

</td>

<td style="text-align:right;">

0.5971132

</td>

<td style="text-align:right;">

0.5665458

</td>

<td style="text-align:right;">

1.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Maryland

</td>

<td style="text-align:right;">

943169

</td>

<td style="text-align:right;">

2781446

</td>

<td style="text-align:right;">

0.3589773

</td>

<td style="text-align:right;">

0.3390930

</td>

<td style="text-align:right;">

1.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Massachusetts

</td>

<td style="text-align:right;">

1090893

</td>

<td style="text-align:right;">

3378821

</td>

<td style="text-align:right;">

0.3731912

</td>

<td style="text-align:right;">

0.3228620

</td>

<td style="text-align:right;">

1.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

New Mexico

</td>

<td style="text-align:right;">

319667

</td>

<td style="text-align:right;">

798319

</td>

<td style="text-align:right;">

0.4284333

</td>

<td style="text-align:right;">

0.4004251

</td>

<td style="text-align:right;">

1.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

North Carolina

</td>

<td style="text-align:right;">

2362631

</td>

<td style="text-align:right;">

4741564

</td>

<td style="text-align:right;">

0.5039306

</td>

<td style="text-align:right;">

0.4982809

</td>

<td style="text-align:right;">

1.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Oklahoma

</td>

<td style="text-align:right;">

949136

</td>

<td style="text-align:right;">

1452992

</td>

<td style="text-align:right;">

0.6677232

</td>

<td style="text-align:right;">

0.6532286

</td>

<td style="text-align:right;">

1.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Oregon

</td>

<td style="text-align:right;">

782403

</td>

<td style="text-align:right;">

2001336

</td>

<td style="text-align:right;">

0.4214987

</td>

<td style="text-align:right;">

0.3909404

</td>

<td style="text-align:right;">

1.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Texas

</td>

<td style="text-align:right;">

4685047

</td>

<td style="text-align:right;">

8969226

</td>

<td style="text-align:right;">

0.5716698

</td>

<td style="text-align:right;">

0.5223469

</td>

<td style="text-align:right;">

1.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Utah

</td>

<td style="text-align:right;">

515211

</td>

<td style="text-align:right;">

1131317

</td>

<td style="text-align:right;">

0.7279053

</td>

<td style="text-align:right;">

0.4554082

</td>

<td style="text-align:right;">

1.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Vermont

</td>

<td style="text-align:right;">

95369

</td>

<td style="text-align:right;">

320467

</td>

<td style="text-align:right;">

0.3097264

</td>

<td style="text-align:right;">

0.2975938

</td>

<td style="text-align:right;">

1.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Virginia

</td>

<td style="text-align:right;">

1769443

</td>

<td style="text-align:right;">

3982752

</td>

<td style="text-align:right;">

0.4728310

</td>

<td style="text-align:right;">

0.4442765

</td>

<td style="text-align:right;">

1.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Washington

</td>

<td style="text-align:right;">

1221747

</td>

<td style="text-align:right;">

3317019

</td>

<td style="text-align:right;">

0.4129462

</td>

<td style="text-align:right;">

0.3683268

</td>

<td style="text-align:right;">

1.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Wyoming

</td>

<td style="text-align:right;">

174419

</td>

<td style="text-align:right;">

258788

</td>

<td style="text-align:right;">

0.6819359

</td>

<td style="text-align:right;">

0.6739841

</td>

<td style="text-align:right;">

1.0000000

</td>

</tr>

</tbody>

</table>

There are onl 28 states where this p-value is smaller than units (of
which 24 are really the interesting states). So we’ve narrowed down our
attention to 24 states where Trump over-performed (compared to 2012).
Really, there are only about a half dozen states worth investigating
since they were won by Trump:

``` r
kable(filter(data, trump_2016 > republican_2012, trump_2016 > 0.45))
```

<table>

<thead>

<tr>

<th style="text-align:left;">

state

</th>

<th style="text-align:right;">

candidatevotes

</th>

<th style="text-align:right;">

totalvotes

</th>

<th style="text-align:right;">

republican\_2012

</th>

<th style="text-align:right;">

trump\_2016

</th>

<th style="text-align:right;">

p.value

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Alabama

</td>

<td style="text-align:right;">

1318255

</td>

<td style="text-align:right;">

2123372

</td>

<td style="text-align:right;">

0.6054582

</td>

<td style="text-align:right;">

0.6208309

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Indiana

</td>

<td style="text-align:right;">

1557286

</td>

<td style="text-align:right;">

2734958

</td>

<td style="text-align:right;">

0.5412553

</td>

<td style="text-align:right;">

0.5694003

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Iowa

</td>

<td style="text-align:right;">

800983

</td>

<td style="text-align:right;">

1565580

</td>

<td style="text-align:right;">

0.4617787

</td>

<td style="text-align:right;">

0.5116206

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Kentucky

</td>

<td style="text-align:right;">

1202971

</td>

<td style="text-align:right;">

1924149

</td>

<td style="text-align:right;">

0.6049314

</td>

<td style="text-align:right;">

0.6251964

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Michigan

</td>

<td style="text-align:right;">

2279543

</td>

<td style="text-align:right;">

4799284

</td>

<td style="text-align:right;">

0.4471092

</td>

<td style="text-align:right;">

0.4749756

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Mississippi

</td>

<td style="text-align:right;">

700714

</td>

<td style="text-align:right;">

1209357

</td>

<td style="text-align:right;">

0.5528585

</td>

<td style="text-align:right;">

0.5794104

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Missouri

</td>

<td style="text-align:right;">

1594511

</td>

<td style="text-align:right;">

2808605

</td>

<td style="text-align:right;">

0.5376374

</td>

<td style="text-align:right;">

0.5677235

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Nebraska

</td>

<td style="text-align:right;">

495961

</td>

<td style="text-align:right;">

844227

</td>

<td style="text-align:right;">

0.3802731

</td>

<td style="text-align:right;">

0.5874735

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

North Dakota

</td>

<td style="text-align:right;">

216794

</td>

<td style="text-align:right;">

344360

</td>

<td style="text-align:right;">

0.5831568

</td>

<td style="text-align:right;">

0.6295563

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Ohio

</td>

<td style="text-align:right;">

2841005

</td>

<td style="text-align:right;">

5496487

</td>

<td style="text-align:right;">

0.4768844

</td>

<td style="text-align:right;">

0.5168765

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Pennsylvania

</td>

<td style="text-align:right;">

2970733

</td>

<td style="text-align:right;">

6115402

</td>

<td style="text-align:right;">

0.4668087

</td>

<td style="text-align:right;">

0.4857789

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

South Dakota

</td>

<td style="text-align:right;">

227721

</td>

<td style="text-align:right;">

370093

</td>

<td style="text-align:right;">

0.5788931

</td>

<td style="text-align:right;">

0.6153075

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Tennessee

</td>

<td style="text-align:right;">

1522925

</td>

<td style="text-align:right;">

2508027

</td>

<td style="text-align:right;">

0.5947871

</td>

<td style="text-align:right;">

0.6072203

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

West Virginia

</td>

<td style="text-align:right;">

489371

</td>

<td style="text-align:right;">

713051

</td>

<td style="text-align:right;">

0.6229584

</td>

<td style="text-align:right;">

0.6863057

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Wisconsin

</td>

<td style="text-align:right;">

1405284

</td>

<td style="text-align:right;">

2976150

</td>

<td style="text-align:right;">

0.4593835

</td>

<td style="text-align:right;">

0.4721818

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Montana

</td>

<td style="text-align:right;">

279240

</td>

<td style="text-align:right;">

494526

</td>

<td style="text-align:right;">

0.5535154

</td>

<td style="text-align:right;">

0.5646619

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

South Carolina

</td>

<td style="text-align:right;">

1155389

</td>

<td style="text-align:right;">

2103027

</td>

<td style="text-align:right;">

0.5456113

</td>

<td style="text-align:right;">

0.5493933

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

Louisiana

</td>

<td style="text-align:right;">

1178638

</td>

<td style="text-align:right;">

2029032

</td>

<td style="text-align:right;">

0.5778458

</td>

<td style="text-align:right;">

0.5808868

</td>

<td style="text-align:right;">

0.0000000

</td>

</tr>

<tr>

<td style="text-align:left;">

New Hampshire

</td>

<td style="text-align:right;">

345790

</td>

<td style="text-align:right;">

744296

</td>

<td style="text-align:right;">

0.4640380

</td>

<td style="text-align:right;">

0.4645867

</td>

<td style="text-align:right;">

0.1715487

</td>

</tr>

<tr>

<td style="text-align:left;">

Arkansas

</td>

<td style="text-align:right;">

684872

</td>

<td style="text-align:right;">

1130635

</td>

<td style="text-align:right;">

0.6056694

</td>

<td style="text-align:right;">

0.6057410

</td>

<td style="text-align:right;">

0.4384544

</td>

</tr>

</tbody>

</table>

## Picking Out the Relevant States

We can pick out the relevant states, but the criteria we have is
exceedingly generous. We end up with 25 states, with genuine surprises
like Michigan, but with false-positives like Alabama.

``` r
relevant_states <- data %>%
  filter(p.value < 0.1) %>% 
  select(state) %>%
  `[[`("state") %>%
  sort
```

We could then compute the surprise for each of these states:

``` r
relevant_surprise <- results16 %>%
  filter(state %in% relevant_states) %>%
  group_by(state) %>%
  summarize(total_surprise = sum(surprise)) %>%
  arrange(-total_surprise)
relevant_surprise
```

    ## # A tibble: 25 x 2
    ##    state        total_surprise
    ##    <I<chr>>              <dbl>
    ##  1 Iowa                   9.83
    ##  2 Michigan               7.20
    ##  3 Nebraska               6.45
    ##  4 Wisconsin              6.36
    ##  5 Missouri               6.15
    ##  6 Ohio                   5.95
    ##  7 North Dakota           5.74
    ##  8 Kentucky               5.31
    ##  9 Indiana                4.84
    ## 10 South Dakota           4.31
    ## # ... with 15 more rows

Now we can filter out the states less surprising than Alabama:

``` r
relevant_surprise2 <- filter(relevant_surprise,
                            total_surprise > relevant_surprise[which(relevant_surprise$state=="Alabama"),]$total_surprise) %>%
  arrange(-total_surprise)
```

This eliminates 10 false-positives. Lets try also weighing these states
by electoral delegates:

``` r
relevant_surprise2 %>%
  group_by(state) %>%
  mutate(ec = electoral_delegates[[state]],
         weighted_surprise = ec*total_surprise) %>%
  arrange(-weighted_surprise)
```

    ## # A tibble: 15 x 4
    ## # Groups:   state [15]
    ##    state         total_surprise    ec weighted_surprise
    ##    <I<chr>>               <dbl> <dbl>             <dbl>
    ##  1 Michigan                7.20    16            115.  
    ##  2 Ohio                    5.95    18            107.  
    ##  3 New York                2.39    29             69.2 
    ##  4 Wisconsin               6.36    10             63.6 
    ##  5 Missouri                6.15    10             61.5 
    ##  6 Iowa                    9.83     6             59.0 
    ##  7 Pennsylvania            2.68    20             53.5 
    ##  8 Indiana                 4.84    11             53.3 
    ##  9 Tennessee               4.06    11             44.7 
    ## 10 Kentucky                5.31     8             42.5 
    ## 11 Nebraska                6.45     5             32.3 
    ## 12 North Dakota            5.74     3             17.2 
    ## 13 West Virginia           2.96     5             14.8 
    ## 14 South Dakota            4.31     3             12.9 
    ## 15 Montana                 2.78     3              8.33

We shouldn’t be surprised Nebraska was won by Trump, it’s a red state…
as are all the others with a small `weighted_surprise` value. So we can
drop these as false-positives as well:

``` r
true_surprises <- relevant_surprise2 %>%
  group_by(state) %>%
  mutate(electoral_delegates = electoral_delegates[[state]],
         weighted_surprise = electoral_delegates*total_surprise) %>%
  arrange(-weighted_surprise) %>%
  filter(weighted_surprise > 10)
```

The true surprises, where Trump overperformed, appears to be:

``` r
kable(true_surprises)
```

<table>

<thead>

<tr>

<th style="text-align:left;">

state

</th>

<th style="text-align:right;">

total\_surprise

</th>

<th style="text-align:right;">

electoral\_delegates

</th>

<th style="text-align:right;">

weighted\_surprise

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Michigan

</td>

<td style="text-align:right;">

7.201144

</td>

<td style="text-align:right;">

16

</td>

<td style="text-align:right;">

115.21831

</td>

</tr>

<tr>

<td style="text-align:left;">

Ohio

</td>

<td style="text-align:right;">

5.954073

</td>

<td style="text-align:right;">

18

</td>

<td style="text-align:right;">

107.17332

</td>

</tr>

<tr>

<td style="text-align:left;">

New York

</td>

<td style="text-align:right;">

2.385851

</td>

<td style="text-align:right;">

29

</td>

<td style="text-align:right;">

69.18968

</td>

</tr>

<tr>

<td style="text-align:left;">

Wisconsin

</td>

<td style="text-align:right;">

6.357924

</td>

<td style="text-align:right;">

10

</td>

<td style="text-align:right;">

63.57924

</td>

</tr>

<tr>

<td style="text-align:left;">

Missouri

</td>

<td style="text-align:right;">

6.152647

</td>

<td style="text-align:right;">

10

</td>

<td style="text-align:right;">

61.52647

</td>

</tr>

<tr>

<td style="text-align:left;">

Iowa

</td>

<td style="text-align:right;">

9.825524

</td>

<td style="text-align:right;">

6

</td>

<td style="text-align:right;">

58.95314

</td>

</tr>

<tr>

<td style="text-align:left;">

Pennsylvania

</td>

<td style="text-align:right;">

2.675596

</td>

<td style="text-align:right;">

20

</td>

<td style="text-align:right;">

53.51192

</td>

</tr>

<tr>

<td style="text-align:left;">

Indiana

</td>

<td style="text-align:right;">

4.843167

</td>

<td style="text-align:right;">

11

</td>

<td style="text-align:right;">

53.27483

</td>

</tr>

<tr>

<td style="text-align:left;">

Tennessee

</td>

<td style="text-align:right;">

4.059553

</td>

<td style="text-align:right;">

11

</td>

<td style="text-align:right;">

44.65508

</td>

</tr>

<tr>

<td style="text-align:left;">

Kentucky

</td>

<td style="text-align:right;">

5.310330

</td>

<td style="text-align:right;">

8

</td>

<td style="text-align:right;">

42.48264

</td>

</tr>

<tr>

<td style="text-align:left;">

Nebraska

</td>

<td style="text-align:right;">

6.453432

</td>

<td style="text-align:right;">

5

</td>

<td style="text-align:right;">

32.26716

</td>

</tr>

<tr>

<td style="text-align:left;">

North Dakota

</td>

<td style="text-align:right;">

5.744110

</td>

<td style="text-align:right;">

3

</td>

<td style="text-align:right;">

17.23233

</td>

</tr>

<tr>

<td style="text-align:left;">

West Virginia

</td>

<td style="text-align:right;">

2.956753

</td>

<td style="text-align:right;">

5

</td>

<td style="text-align:right;">

14.78376

</td>

</tr>

<tr>

<td style="text-align:left;">

South Dakota

</td>

<td style="text-align:right;">

4.313175

</td>

<td style="text-align:right;">

3

</td>

<td style="text-align:right;">

12.93952

</td>

</tr>

</tbody>

</table>

## Surprising Counties

Lets examine the counties in these genuinely surprising states.

``` r
surprising_counties <- results16 %>%
  filter(state %in% true_surprises$state) %>%
  arrange(-surprise)
surprising_counties$state <- as.factor(surprising_counties$state)
```

Lets draw a box-plot for the
data.

``` r
ggplot(filter(surprising_counties, state!="Nebraska", state!="North Dakota"), aes(x=fct_reorder(state, -surprise), y=surprise)) +
  geom_boxplot() +
  theme_fivethirtyeight(base_size = 8)
```

![](2019-06-28-most-surprising-counties-of-2016_files/figure-gfm/surprise-boxplots-1.png)<!-- -->

``` r
ggplot(filter(surprising_counties, state %in% c("Iowa", "Ohio", "Michigan", "Kentucky", "Wisconsin")), aes(x = surprise, fill = state)) +
  geom_histogram() +
  geom_density(alpha=.2, fill="#FF6666") +
  theme_fivethirtyeight(base_size = 8)
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](2019-06-28-most-surprising-counties-of-2016_files/figure-gfm/random-rustbelt-states-1.png)<!-- -->

``` r
ggplot(filter(surprising_counties, !(state %in% c("Iowa", "Ohio", "Michigan", "Kentucky", "Wisconsin"))), aes(x = surprise, fill = state)) +
  geom_histogram() +
  geom_density(alpha=.2, fill="#FF6666") +
  theme_fivethirtyeight(base_size = 8)
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](2019-06-28-most-surprising-counties-of-2016_files/figure-gfm/non-random-rustbelt-states-1.png)<!-- -->

This looks like a log-normal distribution, lets see what happens if we
exponentiate the surprise:

``` r
ggplot(surprising_counties %>%
         filter(surprise > exp(-6)) %>%
         mutate(exp_surprise = log(surprise)), aes(x = exp_surprise, fill = state)) +
  geom_histogram() +
  geom_density(alpha=.2, fill="#FF6666") +
  theme_fivethirtyeight(base_size = 8)
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](2019-06-28-most-surprising-counties-of-2016_files/figure-gfm/log-normal-guess-1.png)<!-- -->

``` r
Mode <- function(x, na.rm = FALSE) {
  if (na.rm) {
    x = x[!is.na(x)]
  }

  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

surprising_counties %>%
  group_by(state) %>%
  summarize(mode = Mode(surprise, na.rm = T),
            avg = mean(surprise),
            median = median(surprise),
            sd = sd(surprise),
            sum = sum(surprise)) %>%
  arrange(-mode)
```

    ## # A tibble: 14 x 6
    ##    state           mode    avg median     sd   sum
    ##    <fct>          <dbl>  <dbl>  <dbl>  <dbl> <dbl>
    ##  1 Nebraska      1.20   0.0694 0.0569 0.121   6.45
    ##  2 North Dakota  0.392  0.108  0.0995 0.0544  5.74
    ##  3 Iowa          0.173  0.0992 0.0980 0.0253  9.83
    ##  4 Kentucky      0.169  0.0443 0.0443 0.0241  5.31
    ##  5 Missouri      0.162  0.0530 0.0484 0.0212  6.15
    ##  6 West Virginia 0.135  0.0538 0.0501 0.0242  2.96
    ##  7 South Dakota  0.134  0.0654 0.0623 0.0259  4.31
    ##  8 Wisconsin     0.131  0.0883 0.0870 0.0192  6.36
    ##  9 Michigan      0.129  0.0868 0.0886 0.0179  7.20
    ## 10 Ohio          0.128  0.0677 0.0667 0.0257  5.95
    ## 11 Tennessee     0.116  0.0427 0.0381 0.0195  4.06
    ## 12 Indiana       0.103  0.0526 0.0485 0.0169  4.84
    ## 13 New York      0.0950 0.0385 0.0375 0.0221  2.39
    ## 14 Pennsylvania  0.0837 0.0399 0.0352 0.0150  2.68

Considering we are effectively looking at a 3-sided die (which would
have for a given state’s results), which has its entropy be maximized
for a “fair die” with entropy 1.5849625. There are 17 such states, but
only 12 were really surprising:

``` r
results16 %>%
  group_by(state) %>%
  summarize(sum = sum(surprise)) %>%
  filter(sum > log(3, base = 2)) %>%
  arrange(-sum)
```

    ## # A tibble: 36 x 2
    ##    state       sum
    ##    <I<chr>>  <dbl>
    ##  1 Oklahoma  23.2 
    ##  2 Utah      13.0 
    ##  3 Iowa       9.83
    ##  4 Minnesota  8.70
    ##  5 Idaho      8.55
    ##  6 Virginia   7.61
    ##  7 Illinois   7.44
    ##  8 Michigan   7.20
    ##  9 Nebraska   6.45
    ## 10 Wisconsin  6.36
    ## # ... with 26 more rows

In biological systems, the “sum” column would measure how far we are
from equilibrium (c.f.,
[arXiv:1512.02742](https://arxiv.org/abs/1512.02742)). Intuitively, it’s
a measure of “surprise” — how far off we’d be supposing 2016 would
unfold like 2012. These states in particular are where we were just dead
wrong in our suppositions.

``` r
surprise_for_state <- function(p_states, y) {
  inner_join(filter(p_states, year == y - 4),
             filter(p_states, year == y),
             by = c("state", "party")) %>%
    group_by(state) %>%
    nest() %>%
    mutate(surprise = map(data, compute_surprise)) %>%
    unnest(surprise, .drop = TRUE) %>%
    arrange(-surprise)
}
```

``` r
prop_states <- state_results %>%
  mutate(party = ifelse(party == "democrat", "democrat",
                        ifelse(party == "republican", "republican",
                               "$third-party"))) %>%
  group_by(year,state,party) %>%
  transmute(probability = sum(candidatevotes)/totalvotes) %>%
  ungroup() %>%
  unique
prop_states$party <- as.factor(prop_states$party)

surprise_1988 <- surprise_for_state(prop_states, 1988)
surprise_1992 <- surprise_for_state(prop_states, 1992)
surprise_1996 <- surprise_for_state(prop_states, 1996)
surprise_2000 <- surprise_for_state(prop_states, 2000)
surprise_2004 <- surprise_for_state(prop_states, 2004)
surprise_2008 <- surprise_for_state(prop_states, 2008)
surprise_2012 <- surprise_for_state(prop_states, 2012)
surprise_2016 <- surprise_for_state(prop_states, 2016)
```

How much surprise was there, overall, compared to 2012? We plot a
histogram of the states’s “surprise value” by year:

``` r
surprise_1988$year <- 1988
surprise_1992$year <- 1992
surprise_1996$year <- 1996
surprise_2000$year <- 2000
surprise_2004$year <- 2004
surprise_2008$year <- 2008
surprise_2016$year <- 2016
surprise_2012$year <- 2012
surprise_elections <- rbind(surprise_1988, rbind(surprise_1992, rbind(surprise_1996, rbind(surprise_2000, rbind(surprise_2004, rbind(surprise_2008, rbind(surprise_2012, surprise_2016)))))))
surprise_elections$year <- as.factor(surprise_elections$year)

ggplot(surprise_elections, aes(x=surprise, fill=year)) +
  geom_histogram() +
  theme_fivethirtyeight(base_size = 8)
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](2019-06-28-most-surprising-counties-of-2016_files/figure-gfm/historic-surprises-1.png)<!-- -->

Why is the 1992 election so surprising? Well, Ross Perot did
**surprisingly** well in the popular vote, receiving about 20% of the
popular vote. He *literally* did **surprisingly** well.

``` r
surprise_elections %>%
  group_by(year) %>%
  summarize(surprise = sum(surprise))
```

    ## # A tibble: 8 x 2
    ##   year  surprise
    ##   <fct>    <dbl>
    ## 1 1988     0.850
    ## 2 1992    33.3  
    ## 3 1996     2.81 
    ## 4 2000     2.88 
    ## 5 2004     1.40 
    ## 6 2008     0.725
    ## 7 2012     0.341
    ## 8 2016     3.52

## Overperformed…compared to what?

We should take the time here to note we should be a little careful upon
further analysis to take, e.g., the running mean of the percentages for
Republican candidates rather than just the last election’s results.
Well, we would take the running average for the Republican candidate,
the Democratic candidate, and “Third Party candidate”, then renormalize
to make certain the weighted means are transformed into proportions.

``` r
prop_states %>%
  arrange(state,party,year) %>%
  group_by(state,party) %>%
  summarize(moving_prob = last(rollmean(probability, k = 4))) %>%
  group_by(state) %>%
  mutate(moving_prob = moving_prob/sum(moving_prob)) %>%
  ungroup()
```

    ## # A tibble: 153 x 3
    ##    state    party        moving_prob
    ##    <I<chr>> <fct>              <dbl>
    ##  1 Alabama  $third-party      0.0157
    ##  2 Alabama  democrat          0.371 
    ##  3 Alabama  republican        0.614 
    ##  4 Alaska   $third-party      0.0566
    ##  5 Alaska   democrat          0.377 
    ##  6 Alaska   republican        0.566 
    ##  7 Arizona  $third-party      0.0248
    ##  8 Arizona  democrat          0.448 
    ##  9 Arizona  republican        0.527 
    ## 10 Arkansas $third-party      0.0297
    ## # ... with 143 more rows

Now we can answer the question more directly, how well did Trump do
compared to an “average Republican”?

``` r
priors_2016 <- prop_states %>%
  arrange(state,party,year) %>%
  group_by(state,party) %>%
  summarize(moving_prob = last(rollmean(probability, k = 4))) %>%
  ungroup() %>%
  mutate(year = 2012) %>%
  group_by(state) %>%
  mutate(probability = moving_prob/sum(moving_prob)) %>%
  ungroup()

comp_2016 <- inner_join(priors_2016,
                        filter(prop_states, year==2016),
                        by=c("state", "party")) %>%
  group_by(state) %>%
  nest() %>%
  mutate(surprise = map(data, compute_surprise)) %>%
  unnest(surprise, .drop = TRUE) %>%
  arrange(-surprise) %>%
  mutate(year = 2016)
```

Lets see everything more surprising than
Alabama:

``` r
kable(select(filter(comp_2016, surprise > comp_2016[which(comp_2016$state=="Alabama"),]$surprise), -year))
```

<table>

<thead>

<tr>

<th style="text-align:left;">

state

</th>

<th style="text-align:right;">

surprise

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Utah

</td>

<td style="text-align:right;">

0.2153058

</td>

</tr>

<tr>

<td style="text-align:left;">

Vermont

</td>

<td style="text-align:right;">

0.0839644

</td>

</tr>

<tr>

<td style="text-align:left;">

Idaho

</td>

<td style="text-align:right;">

0.0743184

</td>

</tr>

<tr>

<td style="text-align:left;">

North Dakota

</td>

<td style="text-align:right;">

0.0605664

</td>

</tr>

<tr>

<td style="text-align:left;">

New Mexico

</td>

<td style="text-align:right;">

0.0597367

</td>

</tr>

<tr>

<td style="text-align:left;">

Washington

</td>

<td style="text-align:right;">

0.0547859

</td>

</tr>

<tr>

<td style="text-align:left;">

Hawaii

</td>

<td style="text-align:right;">

0.0508001

</td>

</tr>

<tr>

<td style="text-align:left;">

Wyoming

</td>

<td style="text-align:right;">

0.0479741

</td>

</tr>

<tr>

<td style="text-align:left;">

West Virginia

</td>

<td style="text-align:right;">

0.0457694

</td>

</tr>

<tr>

<td style="text-align:left;">

Oregon

</td>

<td style="text-align:right;">

0.0452301

</td>

</tr>

<tr>

<td style="text-align:left;">

Alaska

</td>

<td style="text-align:right;">

0.0444291

</td>

</tr>

<tr>

<td style="text-align:left;">

Nebraska

</td>

<td style="text-align:right;">

0.0443129

</td>

</tr>

<tr>

<td style="text-align:left;">

Maine

</td>

<td style="text-align:right;">

0.0438791

</td>

</tr>

<tr>

<td style="text-align:left;">

Iowa

</td>

<td style="text-align:right;">

0.0418378

</td>

</tr>

<tr>

<td style="text-align:left;">

Colorado

</td>

<td style="text-align:right;">

0.0407879

</td>

</tr>

<tr>

<td style="text-align:left;">

Minnesota

</td>

<td style="text-align:right;">

0.0392525

</td>

</tr>

<tr>

<td style="text-align:left;">

New Hampshire

</td>

<td style="text-align:right;">

0.0334553

</td>

</tr>

<tr>

<td style="text-align:left;">

Massachusetts

</td>

<td style="text-align:right;">

0.0330140

</td>

</tr>

<tr>

<td style="text-align:left;">

Wisconsin

</td>

<td style="text-align:right;">

0.0327694

</td>

</tr>

<tr>

<td style="text-align:left;">

South Dakota

</td>

<td style="text-align:right;">

0.0326571

</td>

</tr>

<tr>

<td style="text-align:left;">

Arizona

</td>

<td style="text-align:right;">

0.0304998

</td>

</tr>

<tr>

<td style="text-align:left;">

Kansas

</td>

<td style="text-align:right;">

0.0294447

</td>

</tr>

<tr>

<td style="text-align:left;">

Virginia

</td>

<td style="text-align:right;">

0.0292913

</td>

</tr>

<tr>

<td style="text-align:left;">

District of Columbia

</td>

<td style="text-align:right;">

0.0290764

</td>

</tr>

<tr>

<td style="text-align:left;">

California

</td>

<td style="text-align:right;">

0.0287558

</td>

</tr>

<tr>

<td style="text-align:left;">

Missouri

</td>

<td style="text-align:right;">

0.0282343

</td>

</tr>

<tr>

<td style="text-align:left;">

Michigan

</td>

<td style="text-align:right;">

0.0259703

</td>

</tr>

<tr>

<td style="text-align:left;">

Rhode Island

</td>

<td style="text-align:right;">

0.0255431

</td>

</tr>

<tr>

<td style="text-align:left;">

Indiana

</td>

<td style="text-align:right;">

0.0253461

</td>

</tr>

<tr>

<td style="text-align:left;">

Illinois

</td>

<td style="text-align:right;">

0.0232764

</td>

</tr>

<tr>

<td style="text-align:left;">

Kentucky

</td>

<td style="text-align:right;">

0.0229925

</td>

</tr>

<tr>

<td style="text-align:left;">

Montana

</td>

<td style="text-align:right;">

0.0227734

</td>

</tr>

<tr>

<td style="text-align:left;">

Tennessee

</td>

<td style="text-align:right;">

0.0225430

</td>

</tr>

<tr>

<td style="text-align:left;">

Nevada

</td>

<td style="text-align:right;">

0.0225262

</td>

</tr>

<tr>

<td style="text-align:left;">

Ohio

</td>

<td style="text-align:right;">

0.0222270

</td>

</tr>

<tr>

<td style="text-align:left;">

Maryland

</td>

<td style="text-align:right;">

0.0221489

</td>

</tr>

<tr>

<td style="text-align:left;">

Texas

</td>

<td style="text-align:right;">

0.0219803

</td>

</tr>

<tr>

<td style="text-align:left;">

Delaware

</td>

<td style="text-align:right;">

0.0202498

</td>

</tr>

<tr>

<td style="text-align:left;">

Arkansas

</td>

<td style="text-align:right;">

0.0199481

</td>

</tr>

<tr>

<td style="text-align:left;">

North Carolina

</td>

<td style="text-align:right;">

0.0177541

</td>

</tr>

<tr>

<td style="text-align:left;">

Oklahoma

</td>

<td style="text-align:right;">

0.0158876

</td>

</tr>

<tr>

<td style="text-align:left;">

South Carolina

</td>

<td style="text-align:right;">

0.0156075

</td>

</tr>

<tr>

<td style="text-align:left;">

Georgia

</td>

<td style="text-align:right;">

0.0151864

</td>

</tr>

</tbody>

</table>

The low surprise for Michigan and Ohio suggests that…we shouldn’t be
surprised by the outcome. The counterfactual situation where we consider
a “replacement candidate” in both parties may be worth plotting, against
the real competitors’s results.

``` r
surprise_2016$counterfactual <- F
comp_2016$counterfactual <- T
conterfact_2016 <- select(rbind(surprise_2016, comp_2016), -year)

ggplot(conterfact_2016, aes(x=surprise, fill=counterfactual)) +
  geom_histogram(binwidth = 0.005) +
  theme_fivethirtyeight(base_size = 8)
```

![](2019-06-28-most-surprising-counties-of-2016_files/figure-gfm/counterfactual-1.png)<!-- -->

Did Trump overperform or Clinton underperform? Or did third party
candidates surge unexpectedly? The histogram can’t tell, because that’s
not being measured. All that’s measured is the “surprise distribution”
across the states, comparing “replacement candidates” to the actual
ones. The actual candidates produced more surprising results.

We can sum the differences between *expected* vote proportions and
*actual* vote
proportions.

``` r
diffs_2016 <- inner_join(select(filter(prop_states, year==2016), -year), select(priors_2016, -year), by=c("state", "party")) %>%
  group_by(state,party) %>%
  transmute(delta = probability.x - probability.y) %>%
  ungroup()
```

Now we should weigh this by the electoral delegates in each state
(honestly, it should be the difference in voters in each state, but this
is approximated by the electoral delegate count)

``` r
kable(diffs_2016 %>%
        group_by(state,party) %>%
        mutate(expected_ed = delta*electoral_delegates[[first(state)]]) %>%
        ungroup() %>%
        group_by(party) %>%
        summarize(actual_minus_expected = sum(expected_ed)))
```

<table>

<thead>

<tr>

<th style="text-align:left;">

party

</th>

<th style="text-align:right;">

actual\_minus\_expected

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

$third-party

</td>

<td style="text-align:right;">

17.953961

</td>

</tr>

<tr>

<td style="text-align:left;">

democrat

</td>

<td style="text-align:right;">

\-10.924031

</td>

</tr>

<tr>

<td style="text-align:left;">

republican

</td>

<td style="text-align:right;">

\-7.029929

</td>

</tr>

</tbody>

</table>

So, yes, the third party candidates did better than expected. We could
even plot the expected outcome:

``` r
plot_usmap(data = priors_2016 %>%
             select(-moving_prob) %>%
             spread(party, probability) %>%
             mutate(winning_margin = republican - democrat - `$third-party`,
                    winning_party = ifelse(winning_margin < 0, -1, 1)),
           values = "winning_party") + 
  scale_fill_continuous(
    low = "blue", high = "red", name = "Margin of Victory"
  ) + theme(legend.position = "right")
```

![](2019-06-28-most-surprising-counties-of-2016_files/figure-gfm/expected-map-1.png)<!-- -->

This would have produced the following results:

``` r
priors_2016 %>%
  select(-moving_prob) %>%
  spread(party, probability) %>%
  mutate(winning_margin = republican - democrat - `$third-party`,
         winning_party = ifelse(winning_margin < 0, -1, 1)) %>%
  ungroup() %>%
  group_by(state) %>%
  mutate(dem_ed = electoral_delegates[[first(state)]]*ifelse(winning_party==-1, 1, 0),
         rep_ed = electoral_delegates[[first(state)]]*ifelse(winning_party==1, 1, 0)) %>%
  ungroup() %>%
  summarize(democratic_electoral_delegates = sum(dem_ed),
            republican_electoral_delegates = sum(rep_ed)
            )
```

    ## # A tibble: 1 x 2
    ##   democratic_electoral_delegates republican_electoral_delegates
    ##                            <dbl>                          <dbl>
    ## 1                            332                            206

# Third Party Performance: 2016 vs 2012

It just so happens Jill Stein ran in 2012 on the Green party ticket, and
Gary Johnson ran in 2012 on the Libertarian ticket. We can compare their
respective performances to see if either one improved noticeably. What’s
the game plan? Well, we should measure how surprising the results were
in 2016 compared to 2012, as usual. We partition the election results
into “Voted for Johnson” and “Voted for Someone Else” (we repeat this
analysis for “Stein” instead of Johnson later), and measure how
surprising the result is.

## Johnson’s Performance

We first will look at how well Johnson did in each state, for both
elections. Unfortunately, our data source’s information at the county
level is insufficient. So we will be forced to work at the state
level.

``` r
johnson_2012 <- filter(state_results, year == 2012, candidate == "Johnson, Gary")
johnson_2016 <- filter(state_results, year == 2016, candidate == "Johnson, Gary")
```

Lets see…in 2012, Johnson ran in 46 states, but in 2016 he ran in all 50
states. Lets see what states were missing in 2012.

``` r
johnson_2016 %>%
  filter(!(state %in% johnson_2012$state)) %>%
  select(state) %>%
  unique
```

    ##       state
    ## 1   Alabama
    ## 2  Michigan
    ## 3  Oklahoma
    ## 4 Wisconsin

Some of these states (Michigan, Wisconsin) are critical to the 2016
election outcome.

``` r
johnson_2012 %>%
  group_by(state) %>%
  transmute(p = sum(candidatevotes)/totalvotes,
            p_other = 1 - p) %>%
  ungroup()
```

    ## # A tibble: 46 x 3
    ##    state                      p p_other
    ##    <I<chr>>               <dbl>   <dbl>
    ##  1 Alaska               0.0246    0.975
    ##  2 Arizona              0.0140    0.986
    ##  3 Arkansas             0.0152    0.985
    ##  4 California           0.0110    0.989
    ##  5 Colorado             0.0138    0.986
    ##  6 Connecticut          0.00792   0.992
    ##  7 Delaware             0.00938   0.991
    ##  8 District of Columbia 0.00709   0.993
    ##  9 Florida              0.00528   0.995
    ## 10 Georgia              0.0116    0.988
    ## # ... with 36 more rows

We can now examine the (electoral delegate weighted) surprise for each
state, to see how well Johnson performed compared to our 2012
expectations. The weighted surprise is:

``` r
johnson_surprise <- inner_join(johnson_2012 %>%
             group_by(state) %>%
             transmute(p = sum(candidatevotes)/totalvotes,
                       p_other = 1 - p) %>%
             ungroup(),
           johnson_2016 %>%
             group_by(state) %>%
             transmute(p = sum(candidatevotes)/totalvotes,
                       p_other = 1 - p) %>%
             ungroup(),
           by = "state") %>%
  group_by(state) %>%
  transmute(surprise = KLD(px = c(p.x, p_other.x), py = c(p.y, p_other.y), base = 2)$sum.KLD.py.px*electoral_delegates[[first(state)]]) %>%
  arrange(-surprise)

johnson_surprise
```

    ## # A tibble: 46 x 2
    ## # Groups:   state [45]
    ##    state         surprise
    ##    <I<chr>>         <dbl>
    ##  1 California       1.22 
    ##  2 Texas            0.703
    ##  3 Florida          0.619
    ##  4 Illinois         0.606
    ##  5 New York         0.500
    ##  6 New York         0.500
    ##  7 Washington       0.479
    ##  8 Massachusetts    0.445
    ##  9 Colorado         0.405
    ## 10 Virginia         0.323
    ## # ... with 36 more rows

Johnson did surprisingly well compared to his 2012 performance. At this
rate, he could be president in a
century.

``` r
johnson_surprise$state <- factor(johnson_surprise$state, levels=states_and_dc())

ggplot(johnson_surprise, aes(x=fct_reorder(state, surprise), y=surprise)) +
  geom_point() +
  coord_flip() +
  theme_fivethirtyeight(base_size = 8)
```

![](2019-06-28-most-surprising-counties-of-2016_files/figure-gfm/johnson-scatterplot-1.png)<!-- -->

## Stein’s Performance

We can do likewise for Stein, finding the state-level
data.

``` r
Stein_2012 <- filter(state_results, year == 2012, candidate == "Stein, Jill")
Stein_2016 <- filter(state_results, year == 2016, candidate == "Stein, Jill")
```

We find the (electoral delegate weighted) surprise for Stein:

``` r
stein_surprise <- inner_join(Stein_2012 %>%
             group_by(state) %>%
             transmute(p = sum(candidatevotes)/totalvotes,
                       p_other = 1 - p) %>%
             ungroup(),
           Stein_2016 %>%
             group_by(state) %>%
             transmute(p = sum(candidatevotes)/totalvotes,
                       p_other = 1 - p) %>%
             ungroup(),
           by = "state") %>%
  group_by(state) %>%
  transmute(surprise = KLD(px = c(p.x, p_other.x), py = c(p.y, p_other.y), base = 2)$sum.KLD.py.px*electoral_delegates[[first(state)]]) %>%
  arrange(-surprise)

stein_surprise
```

    ## # A tibble: 32 x 2
    ## # Groups:   state [32]
    ##    state      surprise
    ##    <I<chr>>      <dbl>
    ##  1 California    0.677
    ##  2 Florida       0.293
    ##  3 New York      0.180
    ##  4 Texas         0.148
    ##  5 Colorado      0.138
    ##  6 Arizona       0.133
    ##  7 Illinois      0.118
    ##  8 New Jersey    0.110
    ##  9 Hawaii        0.108
    ## 10 Washington    0.107
    ## # ... with 22 more rows

This is seemingly smaller than Johnson’s improvement. But that’s
misleading, because of the use of scientific notation. If we plot out
Stein’s performance, things clear up
quickly:

``` r
stein_surprise$state <- factor(stein_surprise$state, levels=states_and_dc())

ggplot(stein_surprise, aes(x=fct_reorder(state, surprise), y=surprise)) +
  geom_point() +
  coord_flip() +
  theme_fivethirtyeight(base_size = 8)
```

![](2019-06-28-most-surprising-counties-of-2016_files/figure-gfm/stein-scatterplot-1.png)<!-- -->

Lets try to plot these together.

``` r
stein_surprise$candidate <- "stein"
johnson_surprise$candidate <- "johnson"

third_party_surprise <- rbind(stein_surprise,
                              johnson_surprise)
# third_party_surprise$candidate <- factor(third_party_surprise$candidate, levels=states)

ggplot(third_party_surprise, aes(x=fct_reorder(state, surprise), y=surprise, shape=candidate, color=candidate)) +
  geom_point() +
  coord_flip() +
  theme_fivethirtyeight(base_size = 8)
```

![](2019-06-28-most-surprising-counties-of-2016_files/figure-gfm/joint-scatterplot-1.png)<!-- -->

Just curious, what was the average ratio of Johnson’s improvement to
Stein’s
improvement?

``` r
efficiency <- inner_join(stein_surprise, johnson_surprise, by="state") %>% group_by(state) %>% transmute(rat = surprise.y/surprise.x) %>% arrange(-rat)

mean(efficiency$rat)
```

    ## [1] 22.88781

**Observe:** Johnson’s improvement from 2012 to 2016 was 22.8878053
times that of Stein’s improvement (an order of magnitude better\!).

Lets restrict focus to states we’re really interested in, the surprising
ones.

``` r
swing_states <- c("Florida", "Michigan", "Pennsylvania", "Wisconsin", "Iowa", "Indiana", "North Carolina", "Ohio", "Nebraska")
ggplot(filter(third_party_surprise, state %in% swing_states), 
       aes(x=fct_reorder(state, -surprise), 
           y=surprise, 
           shape=candidate, 
           color=candidate)) +
  geom_point() +
  theme_fivethirtyeight(base_size = 8)
```

![](2019-06-28-most-surprising-counties-of-2016_files/figure-gfm/joint-swing-scatterplot-1.png)<!-- -->

With the exception of Flordia, Jill Stein really didn’t improve that
much compared to Johnson.

``` r
johnson_2012 %>%
  filter(state %in% swing_states) %>%
  select(state, candidatevotes, totalvotes) %>%
  group_by(state) %>%
  mutate(p = candidatevotes/totalvotes) %>%
  ungroup()
```

    ## # A tibble: 7 x 4
    ##   state          candidatevotes totalvotes       p
    ##   <I<chr>>                <int>      <int>   <dbl>
    ## 1 Florida                 44726    8474179 0.00528
    ## 2 Indiana                 50111    2624534 0.0191 
    ## 3 Iowa                    12926    1582180 0.00817
    ## 4 Nebraska                11109     794379 0.0140 
    ## 5 North Carolina          44515    4505372 0.00988
    ## 6 Ohio                    49493    5580822 0.00887
    ## 7 Pennsylvania            49991    5742040 0.00871

``` r
johnson_2016 %>%
  filter(state %in% swing_states) %>%
  select(state, candidatevotes, totalvotes) %>%
  group_by(state) %>%
  mutate(p = candidatevotes/totalvotes) %>%
  ungroup()
```

    ## # A tibble: 8 x 4
    ##   state          candidatevotes totalvotes      p
    ##   <I<chr>>                <int>      <int>  <dbl>
    ## 1 Florida                207043    9420039 0.0220
    ## 2 Indiana                133993    2734958 0.0490
    ## 3 Iowa                    59186    1565580 0.0378
    ## 4 Michigan               172136    4799284 0.0359
    ## 5 Nebraska                38946     844227 0.0461
    ## 6 North Carolina         130126    4741564 0.0274
    ## 7 Pennsylvania           146715    6115402 0.0240
    ## 8 Wisconsin              106674    2976150 0.0358

``` r
right_join(johnson_2012 %>%
             filter(state %in% swing_states) %>%
             select(state, candidatevotes, totalvotes) %>%
             group_by(state) %>%
             transmute(p_2012 = candidatevotes/totalvotes) %>%
             ungroup(),
           johnson_2016 %>%
             filter(state %in% swing_states) %>%
             select(state, candidatevotes, totalvotes) %>%
             group_by(state) %>%
             transmute(p_2016 = candidatevotes/totalvotes) %>%
             ungroup(),
           by = "state") %>%
  group_by(state) %>%
  mutate(improvement = ifelse(is.na(p_2012), NA, p_2016/p_2012)) %>%
  ungroup()
```

    ## # A tibble: 8 x 4
    ##   state            p_2012 p_2016 improvement
    ##   <I<chr>>          <dbl>  <dbl>       <dbl>
    ## 1 Florida         0.00528 0.0220        4.16
    ## 2 Indiana         0.0191  0.0490        2.57
    ## 3 Iowa            0.00817 0.0378        4.63
    ## 4 Michigan       NA       0.0359       NA   
    ## 5 Nebraska        0.0140  0.0461        3.30
    ## 6 North Carolina  0.00988 0.0274        2.78
    ## 7 Pennsylvania    0.00871 0.0240        2.76
    ## 8 Wisconsin      NA       0.0358       NA
