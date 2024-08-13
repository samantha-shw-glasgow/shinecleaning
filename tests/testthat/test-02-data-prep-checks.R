test_that("`data_prep` rejects wrong data", {

  expect_error(data_prep(pri_valid_responses, "secondary"))
  expect_error(data_prep(sec_valid_responses, "primary"))
  expect_error(data_prep(pri_valid_responses, "fake_response"))

})
