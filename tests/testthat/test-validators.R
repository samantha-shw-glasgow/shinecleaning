describe("duration_too_short", {
  it("succeeds when duration is 61", {
    input <- tibble::tibble("Duration (in seconds)" = 61)
    expected <- tibble::tibble(include = TRUE, message = "")
    expect_equal(duration_too_short(input), expected)
  })

  it("succeeds when duration is exactly 60", {
    input <- tibble::tibble("Duration (in seconds)" = 60)
    expected <- tibble::tibble(include = TRUE, message = "")
    expect_equal(duration_too_short(input), expected)
  })

  it("fails when duration is 59", {
    input <- tibble::tibble("Duration (in seconds)" = 59)
    expected <- tibble::tibble(include = FALSE, message = "Duration too short")
    expect_equal(duration_too_short(input), expected)
  })
})
