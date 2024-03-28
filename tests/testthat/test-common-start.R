test_that("Find a common start of strings", {
  strings <- c('hello world', 'hellothere', 'hellohowareyou')
  expect_equal(.find_common_start(strings), 'hello')

  strings <- c('3hello world', '2hellothere', '1hellohowareyou')
  expect_equal(.find_common_start(strings), '')
})
