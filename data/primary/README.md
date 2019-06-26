This is a tidy data set for the Democratic primaries, from 1976 (when it started) until 2008.

The data provided falls into the following schema:

- `date` in the usual "YYYY-mm-dd" format
- `state` which includes DC, where the primary or caucus was held
- `event_type` which is either `primary` or `caucus`. NOTE some states (I'm looking at you, Washington state) had BOTH a primary AND a caucus, which is why we need this column
- `vote_perc` the vote percentage the candidate received (numeric between 0 and 100)
- `candidate` the standardized name for the candidate (may not be his legal name, e.g., "Bill Clinton" instead of "William Jefferson Clinton")
- `won_state` is either `TRUE` or `FALSE`
- `is_nominee` is either `TRUE` or `FALSE` 

The sources for this data is from Wikipedia: 

- [1976](https://en.wikipedia.org/w/index.php?title=1976_Democratic_Party_presidential_primaries&oldid=901844194)
- [1980](https://en.wikipedia.org/w/index.php?title=1980_Democratic_Party_presidential_primaries&oldid=902993161)
- [1984](https://en.wikipedia.org/w/index.php?title=1984_Democratic_Party_presidential_primaries&oldid=901845264)
- [1988](https://en.wikipedia.org/w/index.php?title=1988_Democratic_Party_presidential_primaries&oldid=901846358)
- [1992](https://en.wikipedia.org/w/index.php?title=1992_Democratic_Party_presidential_primaries&oldid=901860036)
- [2004](https://en.wikipedia.org/w/index.php?title=2004_Democratic_Party_presidential_primaries&oldid=901894472)
- [2008](https://en.wikipedia.org/w/index.php?title=Results_of_the_2008_Democratic_Party_presidential_primaries&oldid=902101925)