  - [Statistically Significant
    Surprises](#statistically-significant-surprises)
  - [Picking Out the Relevant States](#picking-out-the-relevant-states)
  - [Overperformed…compared to what?](#overperformedcompared-to-what)

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
  group_by(year,state,county,party,FIPS) %>%
  transmute(probability = candidatevotes/totalvotes) %>%
  ungroup()
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
  KLD(data$probability.x, data$probability.y)$sum.KLD.py.px
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
  summarize(total_surprise = sum(surprise))
```

Now we can filter out the states less surprising than Alabama:

``` r
relevant_surprise <- filter(relevant_surprise,
                            total_surprise > relevant_surprise[which(relevant_surprise$state=="Alabama"),]$total_surprise) %>%
  arrange(-total_surprise)
```

This eliminates 10 false-positives. Lets try also weighing these states
by electoral delegates:

``` r
relevant_surprise %>%
  group_by(state) %>%
  mutate(ec = electoral_delegates[[state]],
         weighted_surprise = ec*total_surprise) %>%
  arrange(-weighted_surprise)
```

    ## # A tibble: 15 x 4
    ## # Groups:   state [15]
    ##    state         total_surprise    ec weighted_surprise
    ##    <I<chr>>               <dbl> <dbl>             <dbl>
    ##  1 Ohio                   2.89     18             52.1 
    ##  2 New York               1.28     29             37.0 
    ##  3 Missouri               3.31     10             33.1 
    ##  4 Michigan               1.70     16             27.3 
    ##  5 Indiana                2.14     11             23.5 
    ##  6 Iowa                   3.58      6             21.5 
    ##  7 Pennsylvania           1.06     20             21.3 
    ##  8 Tennessee              1.85     11             20.4 
    ##  9 Kentucky               2.34      8             18.7 
    ## 10 Wisconsin              1.47     10             14.7 
    ## 11 Nebraska               1.85      5              9.26
    ## 12 West Virginia          1.58      5              7.92
    ## 13 North Dakota           1.83      3              5.50
    ## 14 South Dakota           1.45      3              4.34
    ## 15 Montana                0.775     3              2.33

We shouldn’t be surprised Nebraska was won by Trump, it’s a red state…
as are all the others with a small `weighted_surprise` value. So we can
drop these as false-positives as well:

``` r
true_surprises <- relevant_surprise %>%
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

Ohio

</td>

<td style="text-align:right;">

2.892666

</td>

<td style="text-align:right;">

18

</td>

<td style="text-align:right;">

52.06799

</td>

</tr>

<tr>

<td style="text-align:left;">

New York

</td>

<td style="text-align:right;">

1.275305

</td>

<td style="text-align:right;">

29

</td>

<td style="text-align:right;">

36.98386

</td>

</tr>

<tr>

<td style="text-align:left;">

Missouri

</td>

<td style="text-align:right;">

3.305370

</td>

<td style="text-align:right;">

10

</td>

<td style="text-align:right;">

33.05370

</td>

</tr>

<tr>

<td style="text-align:left;">

Michigan

</td>

<td style="text-align:right;">

1.704539

</td>

<td style="text-align:right;">

16

</td>

<td style="text-align:right;">

27.27262

</td>

</tr>

<tr>

<td style="text-align:left;">

Indiana

</td>

<td style="text-align:right;">

2.138280

</td>

<td style="text-align:right;">

11

</td>

<td style="text-align:right;">

23.52108

</td>

</tr>

<tr>

<td style="text-align:left;">

Iowa

</td>

<td style="text-align:right;">

3.578930

</td>

<td style="text-align:right;">

6

</td>

<td style="text-align:right;">

21.47358

</td>

</tr>

<tr>

<td style="text-align:left;">

Pennsylvania

</td>

<td style="text-align:right;">

1.064128

</td>

<td style="text-align:right;">

20

</td>

<td style="text-align:right;">

21.28255

</td>

</tr>

<tr>

<td style="text-align:left;">

Tennessee

</td>

<td style="text-align:right;">

1.853572

</td>

<td style="text-align:right;">

11

</td>

<td style="text-align:right;">

20.38929

</td>

</tr>

<tr>

<td style="text-align:left;">

Kentucky

</td>

<td style="text-align:right;">

2.339400

</td>

<td style="text-align:right;">

8

</td>

<td style="text-align:right;">

18.71520

</td>

</tr>

<tr>

<td style="text-align:left;">

Wisconsin

</td>

<td style="text-align:right;">

1.470905

</td>

<td style="text-align:right;">

10

</td>

<td style="text-align:right;">

14.70905

</td>

</tr>

</tbody>

</table>

## Overperformed…compared to what?

We should take the time here to note we should be a little careful upon
further analysis to take, e.g., the running mean of the percentages for
Republican candidates rather than just the last election’s results.
