states <- function() c("Alabama",
                       "Alaska",
                       "Arizona",
                       "Arkansas",
                       "California",
                       "Colorado",
                       "Connecticut",
                       "Delaware",
                       "Florida",
                       "Georgia",
                       "Hawaii",
                       "Idaho",
                       "Illinois",
                       "Indiana",
                       "Iowa",
                       "Kansas",
                       "Kentucky",
                       "Louisiana",
                       "Maine",
                       "Maryland",
                       "Massachusetts",
                       "Michigan",
                       "Minnesota",
                       "Mississippi",
                       "Missouri",
                       "Montana",
                       "Nebraska",
                       "Nevada",
                       "New Hampshire",
                       "New Jersey",
                       "New Mexico",
                       "New York",
                       "North Carolina",
                       "North Dakota",
                       "Ohio",
                       "Oklahoma",
                       "Oregon",
                       "Pennsylvania",
                       "Rhode Island",
                       "South Carolina",
                       "South Dakota",
                       "Tennessee",
                       "Texas",
                       "Utah",
                       "Vermont",
                       "Virginia",
                       "Washington",
                       "West Virginia",
                       "Wisconsin",
                       "Wyoming")

is.state <- function(s) {
  s %in% states()
}

is.dc <- function(s) {
  s %in% c("DC", "D.C.", "District of Columbia")
}

has.electoral_delegates <- function(s) {
  is.state(s) || is.dc(s)
}