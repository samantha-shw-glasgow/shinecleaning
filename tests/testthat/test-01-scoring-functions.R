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


  expect_equal(mm_score(input_data_b), expected_b)

})
