test_that("jd transformation works", {
  expect_equal(date_to_jd(lubridate::ymd_hm("1986-01-01 20:20"), 2446432))
  expect_equal(date_to_jd(lubridate::ymd_hm("2000-05-30 12:00"), 2451694.7))
  expect_equal(date_to_jd(lubridate::ymd_hm("2030-09-16 16:20"), 2462760.85))
})
