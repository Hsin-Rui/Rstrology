test_that("jd transformation works", {
  expect_equal(date_to_jd("1986-01-01", 20, 20, "Asia/Taipei") ,2446432)
  expect_equal(date_to_jd("2000-05-30", 12, 00, "Asia/Taipei") ,2451694.7)
  expect_equal(date_to_jd("2030-09-16", 16, 20, "Asia/Taipei") ,2462760.85)
})
