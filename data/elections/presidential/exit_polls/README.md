This contains the exit polls for the 2016 US Presidenial election, as
CNN posted on their [website](http://www.cnn.com/election/results/exit-polls/national/president).

This includes data from 28 states, as well as the nation-wide data.
There are 90 questions in the survey. The states included in the survey:
1. Arizona
2. California 
3. Colorado
4. Florida
5. Georgia
6. Illinois
7. Indiana
8. Iowa
9. Kentucky
10. Maine
11. Michigan
12. Minnesota
13. Missouri
14. Nevada
15. New Hampshire
16. New Jersey
17. New Mexico
18. New York
19. North Carolina
20. Ohio
21. Oregon
22. Pennsylvania
23. South Carolina
24. Texas
25. Utah
26. Virginia
27. Washington
28. Wisconsin

So we're missing: Alabama, Alaska, Arkansas, Connecticut, Delaware,
Hawaii, Idaho, Kansas, Maryland, Massachusets, Mississippi, Montana,
Nebraska, North Dakota, Oklahoma, Rhode Island, South Dakota, Tennessee,
Vermont, West Virginia, Wyoming. These are all considered "safe states",
though.

I did not do the dirty work to accumulate the exit poll data, this was
done by Vincent Zhang on `data.world`: https://data.world/mrapple/2016-exit-polling-data/

# Columns
- `quesions_id` the unique id for the question asked
- `questions` the string text of the question
- `num_respondents` number of respondents in given state to the question
- `options` string text of response
- `Clinton_perc` an integer indicating, of the respondents who gave this
  answer, the percent of whom voted for Clinton
- `Trump_perc` an integer indicating, of the respondents who gave this
  answer, the percent of whom voted for Trump
- `Others_Unknown_perc`  an integer indicating, of the respondents who
  gave this answer, the percent of whom voted for someone else (or forgot?)
- `options_perc` for the given question, what percent (as an integer
  between 0 and 100) gave this answer
- `state` either "nation" or the name of the state (e.g., "Alabama")
  where the respondents voted
- `url` the URL where data was scrapped from (the state specific answers
  are taken from a different url, something like `http://cnn/base-url/STATE`,
  at least schematically)
