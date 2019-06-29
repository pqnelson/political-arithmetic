make_party_into_factor <- function(results) {
  results$party[results$party == 'democratic-farmer-labor'] <- 'democrat';
  results$party[is.na(results$party)] <- '$third-party';
  results$party <- as.factor(results$party);
  return(results);
}

state_path <- "../data/elections/presidential/state/1976_2016_president.RData"
county_path <- "../data/elections/presidential/county/countypres_2000-2016.RData"

load_obj <- function(path) {
  env <- new.env();
  nm <- load(path,env)[1];
  env[[nm]];
}