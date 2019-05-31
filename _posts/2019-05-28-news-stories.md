  - [Raw Data](#raw-data)
      - [May 28, 2019](#may-28-2019)
          - [International](#international)
          - [National](#national)
      - [May 29, 2019](#may-29-2019)
          - [International](#international-1)
          - [National](#national-1)

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

# Raw Data

## May 28, 2019

### International

<!--html_preserve-->

<table class="viz" style>

<thead>

<tr>

<th>

New York <cite class="newspaper">Times</cite>

</th>

<th>

Los Angeles
<cite class="newspaper">Times</cite>

</th>

</tr>

</thead>

<tr>

<td>

<a href="https://www.nytimes.com/2019/05/27/world/asia/pakistan-china-trafficking.html">She
Thought Sheâ€™d Married a Rich Chinese Farmer. She Hadnâ€™t.</a>
(A4)

</td>

<td>

</td>

</tr>

<tr>

<td>

<a href="https://www.nytimes.com/2019/05/27/world/asia/afghanistan-attacks-schools-unicef.html">Attacks
by Extremists on Afghan Schools Triple, Report Says</a>
(A4)

</td>

<td>

</td>

</tr>

<tr>

<td>

<a href="https://www.nytimes.com/2019/05/27/world/europe/liviu-dragnea-romania-corruption.html">Romaniaâ€™s
Most Powerful Man Is Sent to Prison for Corruption</a>
(A6)

</td>

<td>

</td>

</tr>

<tr>

<td>

<a href="https://www.nytimes.com/2019/05/27/world/asia/japan-empress-masako.html">With
Trumpâ€™s Visit to Japan, Empress Masako Finds a Spotlight</a>
(A8)

</td>

<td>

</td>

</tr>

<tr>

<td>

<a href="https://www.nytimes.com/2019/05/27/world/asia/trump-japan.html">Trump
and Abeâ€™s â€˜Unshakable Bondâ€™ Shows Some Cracks in Tokyo</a>
(A8)

</td>

<td>

<a href="https://www.latimes.com/politics/la-na-pol-trump-iran-north-korea-japan-20190527-story.html">Trump
pushes off war talk on Iran, says â€˜regime changeâ€™ is not U.S.
goal</a>
(A1)

</td>

</tr>

<tr>

<td>

<a href="https://www.nytimes.com/2019/05/27/world/europe/europe-election-results-populism.html">Election
Puts Europe on the Front Line of the Battle With Populism</a>
(A10)

</td>

<td>

<a href="https://www.latimes.com/world/la-fg-europe-elections-results-nationalists-green-party-20190528-story.html">In
European vote, far-right surge fails to materialize, but mainstream
parties lose support</a>
(A2)

</td>

</tr>

<tr>

<td>

<a href="https://www.nytimes.com/2019/05/27/world/europe/eu-election-takeaways.html">European
Parliament Elections: 5 Biggest Takeaways</a>
(A10)

</td>

<td>

</td>

</tr>

<tr>

<td>

<a href="https://www.nytimes.com/2019/05/27/world/europe/european-vote-france.html">European
Vote Reveals an Ever More Divided France</a>
(A11)

</td>

<td>

</td>

</tr>

<tr>

<td>

<a href="https://www.nytimes.com/2019/05/27/world/asia/japan-stabbing-bus-stop.html">18
Schoolchildren Stabbed, and Girl and Man Killed, in Attack in Japan</a>
(A11)

</td>

<td>

<a href="https://www.latimes.com/world/la-fg-japan-knife-attack-20190528-story.html">Knife-wielding
man attacks schoolgirls in Japan, killing 2</a> (blurb of story on
A2)

</td>

</tr>

<tr>

<td>

<a href="https://www.nytimes.com/2019/05/27/world/europe/sebastian-kurz-austria.html">Sebastian
Kurz, Austrian Leader, Is Ousted in No-Confidence Vote</a>
(A12)

</td>

<td>

<a href="https://www.latimes.com/world/la-fg-austria-kurz-chancellor-no-confidence-vote-20190527-story.html">Ousted
by parliament, Austriaâ€™s Kurz vows to win back job</a>
(A4)

</td>

</tr>

<tr>

<td>

<a href="https://www.nytimes.com/2019/05/27/world/middleeast/netanyahu-israel-lieberman-coalition.html">Israelâ€™s
Netanyahu Struggles to Form a Government, as Time Runs Short</a>
(A12)

</td>

<td>

<a href="https://www.latimes.com/world/la-fg-israel-netanyahu-coalition-knesset-20190527-story.html">Netanyahu
running out of time to form government; Israel may face new
elections</a>
(A2)

</td>

</tr>

<tr>

<td>

<a href="https://www.nytimes.com/2019/05/27/world/asia/china-white-panda.html">White
Panda Is Spotted in China for the First Time</a>
(A12)

</td>

<td>

</td>

</tr>

<tr>

<td>

<a href="https://www.nytimes.com/2019/05/27/world/africa/congo-boat-sinking.html">30
Dead and 200 Missing in Congo After Boat Sinks</a>
(A12)

</td>

<td>

</td>

</tr>

<tr>

<td>

</td>

<td>

<a href="https://www.latimes.com/world/la-fg-thailand-dissidents-20190528-story.html">Arrests,
killings strike fear in Thailandâ€™s dissidents: â€˜The hunting has been
acceleratedâ€™</a> (A3)

</td>

</tr>

</table>

<footer class="viz">

<p class="notes">

Matches are based on substantially overlapping subject matters. The only
debatable story match is “Trump pushes off war talk on Iran”, which is a
proper subset of the corresponding New York
<cite class="newspaper">Times</cite> article.

</p>

<p class="notes">

Also note, in the Los Angeles <cite class="newspaper">Times</cite>,
there was a 1000 word blurb about the knife attacks in Japan. Later, on
their website, they posted a longer and more detailed article. I decided
to count that as a match, which may be debatable.

</p>

<p class="source">

Sources: Los Angeles Times, New York Times

</p>

</footer>

<!--html_preserve-->

### National

<!--html_preserve-->

<table class="viz">

<thead>

<tr>

<th>

New York <cite class="newspaper">Times</cite>

</th>

<th>

Los Angeles
<cite class="newspaper">Times</cite>

</th>

</tr>

</thead>

<tr>

<td>

<a href="https://www.nytimes.com/2019/05/27/us/politics/trump-climate-science.html">Trump
Administration Hardens Its Attack on Climate Science</a>
(A1)

</td>

<td>

</td>

</tr>

<tr>

<td>

<a href="https://www.nytimes.com/2019/05/28/technology/google-temp-workers.html">Googleâ€™s
Shadow Work Force: Temps Who Outnumber Full-Time Employees</a>
(A1)

</td>

<td>

</td>

</tr>

<tr>

<td>

<a href="https://www.nytimes.com/2019/05/27/us/politics/us-huawei-berlin-wall.html">Trump
Wants to Wall Off Huawei, but the Digital World Bridles at Barriers</a>
(A1)

</td>

<td>

</td>

</tr>

<tr>

<td>

<a href="https://www.nytimes.com/2019/05/27/us/auto-worker-jobs-lost.html">With
His Job Gone, an Autoworker Wonders, â€˜What Am I as a Man?â€™</a>
(A1)

</td>

<td>

</td>

</tr>

<tr>

<td>

<a href="https://www.nytimes.com/2019/05/27/us/politics/2020-candidates-iowa.html">With
the 2020 Democratic Field Set, Candidates Begin the Races Within the
Race</a>
(A1)

</td>

<td>

</td>

</tr>

<tr>

<td>

<a href="https://www.nytimes.com/2019/05/27/us/oklahoma-floods-pets-rescue.html">Saving
Charlie: A Rush to Rescue Stranded Cats and Dogs from Oklahoma
Floods</a>
(A17)

</td>

<td>

</td>

</tr>

<tr>

<td>

<a href="https://www.nytimes.com/2019/05/27/us/politics/supreme-court-gun-control.html">Fearing
Supreme Court Loss, New York Tries to Make Gun Case Vanish</a>
(A17)

</td>

<td>

</td>

</tr>

<tr>

<td>

<a href="https://www.nytimes.com/2019/05/27/upshot/malpractice-health-care-missed-opportunity.html">A
Missed Opportunity for the Malpractice System to Improve Health Care</a>
(A19)

</td>

<td>

</td>

</tr>

<tr>

<td>

<a href="https://www.nytimes.com/2019/05/27/nyregion/hamptons-shinnecock-billboards.html">Why
a Hamptons Highway Is a Battleground Over Native American Rights</a>
(A22)

</td>

<td>

</td>

</tr>

<tr>

<td>

</td>

<td>

<a href="https://www.latimes.com/science/environment/la-me-marshall-islands-dome-is-leaking-radiation-20190528-story.html">High
radiation levels found in giant clams of Marshall Islands near U.S.
nuclear dump</a>
(A1)

</td>

</tr>

<tr>

<td>

</td>

<td>

<a href="https://www.latimes.com/politics/la-na-pol-joe-sanberg-presidential-run-20190528-story.html">He
made millions as an L.A. investor. Now, he may run for president to
fight poverty</a>
(A1)

</td>

</tr>

<tr>

<td>

</td>

<td>

<a href="https://www.latimes.com/local/lanow/la-me-ln-koreatown-parking-ktown-black-car-silver-car-20190528-story.html">Want
to park in Koreatown? Get ready for a â€˜blood sportâ€™</a>
(A1)

</td>

</tr>

<tr>

<td>

</td>

<td>

<a href="https://www.latimes.com/nation/la-na-world-series-poker-anniversary-20190528-story.html">Put
your hands together for the World Series of Poker, turning 50 this
year</a>
(A4)

</td>

</tr>

<tr>

<td>

</td>

<td>

<a href="https://www.latimes.com/nation/la-na-texas-gun-safety-bill-nra-20190527-story.html">Texas
lawmakers approve safe gun storage program, quietly going around the
NRA</a>
(A4)

</td>

</tr>

<tr>

<td>

</td>

<td>

<a href="https://www.latimes.com/nation/la-na-oklahoma-opioid-trial-pharmaceutical-20190527-story.html">Oklahomaâ€™s
opiod lawsuit targeting drugmaker goes to trial Tuesday</a> (A7)

</td>

</tr>

</table>

<footer class="viz">

<p class="notes">

Matches are based on substantially overlapping subject matters.

</p>

<p class="source">

Sources: Los Angeles Times, New York Times

</p>

</footer>

<!--html_preserve-->

## May 29, 2019

### International

<!--html_preserve-->

<table class="viz">

<thead>

<th>

New York Times

</th>

<th>

Los Angeles
Times

</th>

</thead>

<tr>

<td>

<a href="/2019/05/28/world/asia/japan-stabbing-schoolchildren.html">Stabbing
of 17 Children Shocks Japan, Where Parents Trust Streets Are Safe</a>
A4

</td>

<td>

</td>

</tr>

<tr>

<td>

<a href="/2019/05/28/world/asia/afghanistan-taliban-attacks-moscow-talks.html">Afghan
Forces Hit With Wave of Attacks on Eve of Taliban Talks</a>
A4

</td>

<td>

</td>

</tr>

<tr>

<td>

<a href="/2019/05/28/world/americas/246-cocaine-packs-flight-death.html">After
Death on Airliner, Autopsyâ€™s Grim Discovery: 246 Packs of Cocaine</a>
A4

</td>

<td>

</td>

</tr>

<tr>

<td>

<a href="/2019/05/28/world/asia/pakistan-pashtun-dissent.html">â€˜Time
Is Upâ€™: Pakistanâ€™s Army Targets Protest Movement, Stifling
Dissent</a>
A6

</td>

<td>

</td>

</tr>

<tr>

<td>

<a href="/2019/05/28/us/politics/christian-reismeier-guantanamo-military-court.html">Former
Navy Judge Named to Oversee GuantÃ¡namo Military Court</a> A6

</td>

<td>

</td>

</tr>

<tr>

<td>

<a href="/2019/05/28/world/asia/military-patch-trump.html">More Than
Patriotism on Their Sleeves: Military Patches Sport Trump Likeness</a>
A6

</td>

<td>

</td>

</tr>

<tr>

<td>

<a href="/2019/05/28/climate/united-nations-climate-pledges.html">United
Nations Says 80 Countries May Ramp Up Climate Pledges</a>
A7

</td>

<td>

</td>

</tr>

<tr>

<td>

<a href="/2019/05/28/world/europe/eu-elections-germany-merkel.html">Germanyâ€™s
Political Establishment Looks Fragile After E.U. Vote</a> A8

</td>

<td>

</td>

</tr>

<tr>

<td>

<a href="/2019/05/28/world/europe/pope-francis-mccarrick.html">Pope
Francis Denies He Knew of Abuse by McCarrick</a>
A8

</td>

<td>

</td>

</tr>

<tr>

<td>

<a href="/2019/05/28/world/americas/mexico-corruption-prosecution-oil-company.html">Mexico
Charges Former Oil Official With Bribery in Anticorruption Drive</a>
A9

</td>

<td>

</td>

</tr>

<tr>

<td>

<a href="/2019/05/28/world/asia/south-korea-jeju-massacres.html">Memories
of Massacres Were Long Suppressed Here. Tourists Now Retrace the
Atrocities.</a> A10

</td>

<td>

</td>

</tr>

<tr>

<td>

<a href="/2019/05/28/world/asia/trump-japan.html">Japan Rolled Out the
Red Carpet. Trump Veered Off Into Personal Fixations.</a> A10

</td>

<td>

</td>

</tr>

</table>

<!--html_preserve-->

### National

<!--html_preserve-->

<table class="viz">

<thead>

<th>

New York Times

</th>

<th>

Los Angeles
Times

</th>

</thead>

<tr>

<td>

<a href="https://www.nytimes.com/2019/05/28/us/politics/elizabeth-warren-2020.html">Elizabeth
Warren Gains Ground in 2020 Field, One Plan at a Time</a>
A11

</td>

<td>

</td>

</tr>

<tr>

<td>

<a href="https://www.nytimes.com/2019/05/28/us/politics/disaster-relief-bill.html">For
Second Time, One Republicanâ€™s Objection Delays Disaster Aid</a>
A11

</td>

<td>

</td>

</tr>

<tr>

<td>

<a href="https://www.nytimes.com/2019/05/28/us/politics/supreme-court-mexican-border-shooting.html">Justices
to Hear Case of U.S. Agentâ€™s Shooting of Teenager Across the Mexican
Border</a>
A12

</td>

<td>

</td>

</tr>

<tr>

<td>

<a href="https://www.nytimes.com/2019/05/28/us/politics/kamala-harris-abortion.html">Kamala
Harris, in MSNBC Town Hall, Lays Out New Abortion Proposal</a>
A12

</td>

<td>

</td>

</tr>

<tr>

<td>

<a href="https://www.nytimes.com/2019/05/28/us/missouri-abortion-clinic.html">Missouriâ€™s
Last Abortion Clinic Could Stop Providing the Procedure This Week</a>
A12

</td>

<td>

</td>

</tr>

<tr>

<td>

<a href="https://www.nytimes.com/2019/05/28/us/politics/trump-biden-north-korea.html">Trump
and Biden Campaign Trade Jabs Over North Koreaâ€™s Remarks</a>
A13

</td>

<td>

</td>

</tr>

<tr>

<td>

<a href="https://www.nytimes.com/2019/05/28/us/border-wall-private-new-mexico.html">Border
Wall on Private Land in New Mexico Fuels Backlash</a>
A14

</td>

<td>

</td>

</tr>

<tr>

<td>

<a href="https://www.nytimes.com/2019/05/28/us/tornadoes-usa.html">Kansas
City-Area Tornadoes Add to 12 Straight Days of Destruction</a>
A15

</td>

<td>

</td>

</tr>

<tr>

<td>

<a href="https://www.nytimes.com/2019/05/28/us/politics/justin-amash-trump.html">Justin
Amash, Under Attack for Impeachment Talk, Finds Mixed Support at
Home</a>
A16

</td>

<td>

</td>

</tr>

<tr>

<td>

<a href="https://www.nytimes.com/2019/05/28/us/politics/elaine-chao-stock-divest.html">Transportation
Secretary Failed to Sever Financial Ties to Construction Company</a>
A16

</td>

<td>

</td>

</tr>

<tr>

<td>

<a href="https://www.nytimes.com/2019/05/28/us/politics/michael-wolff-siege-trump.html">White
House Insider Account Has Feel of an Outside View, and Prompts a Mueller
Denial</a> A16

</td>

<td>

</td>

</tr>

</table>

<!--/html_preserve-->
