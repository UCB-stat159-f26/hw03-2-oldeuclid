read_mock_js <- function(file_name) {
  lines <- readLines(test_path("mockdata", file_name), warn = FALSE)
  paste(lines, collapse = " ")
}

test_that("player names have same format", {
  expect_equal(format_player_name("serena williams"), "SerenaWilliams")
  expect_equal(format_player_name("Serena Williams"), "SerenaWilliams")
  expect_equal(format_player_name("SerenaWilliams"), "SerenaWilliams")
})

test_that("false name gives error", {
  female_js <- read_mock_js("FemalePlayerCareer.JS")
  expect_error(check_career_js("", "Gaston Sanchez"), "Gaston Sanchez")
  expect_no_error(check_career_js(female_js, "Serena Williams"))
})

test_that("female file: ages are calculated from birth dates", {
  serena_dob <- as.Date("1981-09-26")
  female_js <- read_mock_js("FemalePlayerCareer.JS")
  matches <- clean_career_js(female_js, serena_dob)

  expect_equal(nrow(matches), 3)
  expect_equal(matches$opponent[1], "Anne Miller")
  expect_equal(matches$result[1], "L")

  player_age <- as.numeric(as.Date("1995-10-30") - serena_dob) / 365.25
  opp_age <- as.numeric(as.Date("1995-10-30") - as.Date("1977-01-19")) / 365.25
  expect_equal(matches$player_age[1], player_age)
  expect_equal(matches$opponent_age[1], opp_age)
  expect_equal(matches$age_diff[1], player_age - opp_age)
})

test_that("male file: opponent age is used as given", {
  rafa_dob <- as.Date("1986-06-03")
  male_js <- read_mock_js("MalePlayerCareer.JS")
  matches <- clean_career_js(male_js, rafa_dob)

  expect_equal(nrow(matches), 3)
  expect_equal(matches$result, c("L", "L", "W"))
  expect_equal(matches$opponent_age[1], 22.264202601)

  player_age <- as.numeric(as.Date("2001-09-10") - rafa_dob) / 365.25
  expect_equal(matches$age_diff[1], player_age - 22.264202601)
})

test_that("matches with no results/age are removed", {
  female_js <- read_mock_js("FemalePlayerCareer.JS")
  no_result_js <- sub('"W", "L"', '"W", ""', female_js)
  no_birth_date_js <- sub('"19770119"', '""', female_js)
  serena_dob <- as.Date("1981-09-26")

  expect_equal(nrow(clean_career_js(no_result_js, serena_dob)), 2)
  expect_equal(nrow(clean_career_js(no_birth_date_js, serena_dob)), 2)
})
