test_that("Me and My feelings score", {

  input_data_a <-  tibble(
    mm1  = c("Never",     "Never",     "Never",     "Never"),
    mm2  = c("Never",     "Always",    "Sometimes", "Always"),
    mm3  = c("Never",     "Never",     "Never",     "Always"),
    mm4  = c("Sometimes", "Never",     "Sometimes", "Never"),
    mm5  = c("Never",     "Never",     "Never",     "Never"),
    mm6  = c("Sometimes", "Sometimes", "Always",    "Sometimes"),
    mm7  = c("Always",    "Never",     "Sometimes", "Always"),
    mm8  = c("Never",     "Never",     "Never",     "Never"),
    mm9  = c("Never",     "Never",     "Sometimes", "Never"),
    mm10 = c("Always",    "Always",    "Sometimes", "Never"),

    mm11 = c("Never",     "Always",    "Always",    "Sometimes"),
    mm12 = c("Never",     "Always",    "Never",     "Sometimes"),
    mm13 = c("Sometimes", "Always",    "Never",     "Never"),
    mm14 = c("Never",     "Always",    "Never",     "Never"),
    mm15 = c("Sometimes", "Never",     "Never",     "Never"),
    mm16 = c("Never",     "Never",     "Always",    "Sometimes")
  )

  expected_a <-bind_cols(input_data_a,
     tribble(
      ~mme_score, ~mmb_score,   ~mme_cat,   ~mmb_cat,
               6,          2, "Expected", "Expected",
               5,         10, "Expected", "Elevated",
               7,          6, "Expected", "Elevated",
               7,          5, "Expected", "Expected"
    )
  )

  expect_equal(mm_score(input_data_a), expected_a)

  input_data_b <-  tibble(
    mm1  = c(NA_character_,     "Never",     "Never",     "Never"),
    mm2  = c(NA_character_,     "Always",    "Sometimes", "Always"),
    mm3  = c(NA_character_,     "Never",     "Never",     "Always"),
    mm4  = c("Sometimes", "Never",     "Sometimes", "Never"),
    mm5  = c("Never",     "Never",     "Never",     "Never"),
    mm6  = c("Sometimes", NA_character_, "Always",    "Sometimes"),
    mm7  = c("Always",    NA_character_,     "Sometimes", "Always"),
    mm8  = c("Never",     NA_character_,     "Never",     "Never"),
    mm9  = c("Never",     NA_character_,     "Sometimes", "Never"),
    mm10 = c("Always",    NA_character_,    "Sometimes", "Never"),

    mm11 = c("Never",     "Always",    NA_character_,    "Sometimes"),
    mm12 = c("Never",     "Always",    NA_character_,     "Sometimes"),
    mm13 = c("Sometimes", "Always",    "Never",     "Never"),
    mm14 = c("Never",     "Always",    "Never",     NA_character_),
    mm15 = c("Sometimes", "Never",     "Never",     NA_character_),
    mm16 = c("Never",     "Never",     "Always",    NA_character_)
  )

  expected_b <-bind_cols(input_data_b,
      tribble(
      ~mme_score, ~mmb_score,   ~mme_cat,   ~mmb_cat,
               6 * 10/7,          2, "Expected", "Expected",
               NA_real_,         10, NA_character_, "Elevated",
               7,          4 * 6 / 4, "Expected", "Elevated",
               7,          NA_real_, "Expected", NA_character_
    )
  )


  expect_identical(mm_score(input_data_b), expected_b)

  })

test_that("WHO score", {




  input_data_a <- tibble(
    Who1 = c("Most of the time",           "At no time",  NA_character_),
    Who2 = c("More than half of the time", "Most of the time", "All of the time"),
    Who3 = c("Less than half of the time", "Some of the time", "Some of the time"),
    Who4 = c("Most of the time",           "Most of the time", "More than half of the time"),
    Who5 = c("Some of the time",           "At no time",       "Most of the time")
  )

  expected_a <- bind_cols(input_data_a, tribble(
      ~who_score, ~who_cat,
      (4+3+2+4+1) * 4, "good",
      (0+4+1+4+0) * 4, "low",
      NA_real_, NA_character_
    )
  )

  expect_identical(who_score(input_data_a), expected_a)

  })

test_that("Secondary SEHS scoring variables calculating", {

  input_data_c <- map(1:30, \(sehs_n) {
    tibble("SEHSS{sehs_n}" := 1:3)
  }) |>
    reduce(bind_cols)

  sehs_responses <- c(
    "Not at all true of me",
    "A little true of me",
    "Pretty much true of me",
    "Very much true of me"
  )

  input_data_c <- input_data_c |>
    mutate(across(everything(), ~sehs_responses[.x]))

  out_dat <- sehs_secondary(input_data_c)

  expected <-
    tibble(
      efficacy_score = 1:3 * 3,
      aware_score = 1:3 * 3,
      persist_score = 1:3 * 3,
      sch_support_score = 1:3 * 3,
      fam_support_score = 1:3 * 3,
      peer_support_score = 1:3 * 3,
      emt_regulation_score = 1:3 * 3,
      empathy_score = 1:3 * 3,
      control_score = 1:3 * 3,
      optimism_score = 1:3 * 3,
      belief_self_score = 1:3 * 3,
      belief_others_score = 1:3 * 3,
      emotional_competence_score = 1:3 * 3
    ) |>
    bind_cols(input_data_c, x = _)

  expect_identical(out_dat, expected)

  })

test_that("ASW score calculations", {

  asw_responses <- c(
    "Never",
    "Once in a while",
    "Sometimes",
    "Quite often",
    "Frequently, if not always",
    "Always"
  )

  input_data_d <- map(1:10, \(asw_n) {
    tibble("ASW{asw_n}" := c(1, 6))
  }) |>
    reduce(bind_cols) |>
    mutate(across(c(ASW1, ASW3, ASW4, ASW5, ASW6, ASW7, ASW8), ~ 7 - .x), across(everything(), ~
                                                                                   asw_responses[.x]))

  expected <-
    bind_cols(input_data_d, tibble(asw_score = c(10, 60))) # high/low scores

  expect_identical(asw_score(input_data_d), expected)

})

test_that("SDQ score output", {

  sdq_responses <- c(
    "Not true",
    "Somewhat true",
    "Certainly true"
  )

  input_data_e <- map(1:25, \(sdq_n) {
    tibble("SDQ{sdq_n}" := sample(sdq_responses, 3, replace = TRUE))
  }) |>
    reduce(bind_cols)

  expected_shape <- map(c("ep", "cp", "ha", "pp", "ps", "sdq_total"), \(varname) {
    tibble(
      "{varname}_score" := rep(NA_real_, 3),
      "{varname}_cat" := rep(NA_character_, 3),
      )
  }) |>
    reduce(bind_cols) |>
    bind_cols(input_data_e, x = _)

  test_output <- sdq_score(input_data_e)

  expect_identical(names(test_output), names(expected_shape))
  expect_identical(dim(test_output), dim(expected_shape))

})
