describe("check_duplicate_dob", {
  it("correctly identifies duplicates", {
    input <- tibble::tribble(
      ~id, ~dob,
      1, 101,
      2, 102,
      3, 102,
      4, 103,
    )
    expected <- tibble::tribble(
      ~message,
      "",
      "Duplicate DOB",
      "Duplicate DOB",
      "",
    )
    expect_equal(
      check_duplicate_dob(input),
      expected
    )
  })
})
