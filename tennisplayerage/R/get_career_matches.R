# name format (ex. "serena williams" is "SerenaWilliams")
format_player_name <- function(player_name) {
  words <- strsplit(trimws(player_name), " +")[[1]]
  first_letters <- toupper(substr(words, 1, 1))
  paste(paste0(first_letters, substring(words, 2)), collapse = "")
}


# check name and give error if name not found
check_career_js <- function(career_js, player_name) {
  if (!grepl("matchmx", career_js)) {
    stop("Couldn't find", player_name,
         "'. Are you sure that's a tennis player?", call. = FALSE)
  }
}


get_opponent_age <- function(values, match_dates) {
  birth_date <- !is.na(values) & grepl("^[0-9]{8}$", values)
  ages <- suppressWarnings(as.numeric(values))
  ages[birth_date] <- as.numeric(
    match_dates[birth_date] - as.Date(values[birth_date], format = "%Y%m%d")
  ) / 365.25
  ages[!is.finite(ages) | ages < 0 | ages > 100] <- NA_real_
  ages
}

clean_career_js <- function(career_js, player_dob) {
  rows <- regmatches(career_js,
                     gregexpr("\\[\"[0-9]{8}\"[^]]*\\]", career_js))[[1]]
  rows <- gsub("\"|^\\[|\\]$", "", unique(rows))
  pieces <- strsplit(rows, ",")
  get_field <- function(k) trimws(sapply(pieces, function(p) p[k]))

  match_date <- as.Date(get_field(1), format = "%Y%m%d")
  matches <- data.frame(
    date = match_date,
    tournament = get_field(2),
    surface = get_field(3),
    result = get_field(5),
    opponent = get_field(12),
    player_age = as.numeric(match_date - player_dob) / 365.25,
    opponent_age = get_opponent_age(get_field(17), match_date)
  )
  matches$age_diff <- matches$player_age - matches$opponent_age

  matches <- matches[matches$result %in% c("W", "L"), ]
  matches <- matches[!is.na(matches$opponent_age), ]
  matches[order(matches$date), ]
}
