source(here::here("R", "Functions.R"))

table1_long_sum_fixture <- function() {
  table1 <- expand.grid(
    variable = c("Variable A", "Variable B"),
    tf45 = c("Yes", "No"),
    label = c("Level 1", "Level 2"),
    year = "2023",
    stringsAsFactors = FALSE
  )
  table1$N <- rep(c(30, 30, 70, 70, 20, 20, 80, 80), 1)
  table1$perc <- ave(
    table1$N,
    table1$variable,
    FUN = function(N) 100 * N / sum(N)
  )
  table1
}

table1_sum_fixture <- function() {
  data.frame(
    variable = rep("Example variable", 6),
    label = rep(c("Level A", "Level B"), each = 3),
    year = rep("2023", 6),
    status = rep(c("Yes", "No", "Total"), 2),
    N = c(30, 70, 100, 20, 80, 100),
    total_N = c(50, 150, 200, 50, 150, 200),
    perc = c(30, 70, 50, 20, 80, 50)
  )
}

test_that("long Table 1 variable and year/status sums are consistent", {
  expect_true(check_table1_long_sums(table1_long_sum_fixture()))
})

test_that("long Table 1 allows suppressed counts when percentages remain", {
  table1 <- table1_long_sum_fixture()
  table1$N[1] <- NA_real_

  expect_true(check_table1_long_sums(table1))
})

test_that("long Table 1 count mismatches identify the problem row", {
  table1 <- table1_long_sum_fixture()
  table1$N[1] <- table1$N[1] + 1

  expect_error(
    check_table1_long_sums(table1),
    "percentages: Variable A/2023/Yes/Level 1",
    fixed = TRUE
  )
})

test_that("long Table 1 percentage mismatches identify the problem row", {
  table1 <- table1_long_sum_fixture()
  table1$perc[1] <- table1$perc[1] + 1

  expect_error(
    check_table1_long_sums(table1),
    "percentages: Variable A/2023/Yes/Level 1",
    fixed = TRUE
  )
})

test_that("Table 1 row and column sums are accepted when consistent", {
  expect_true(check_table1_sums(table1_sum_fixture()))
})

test_that("Table 1 row total mismatches identify the problem group", {
  table1 <- table1_sum_fixture()
  table1$N[table1$label == "Level A" & table1$status == "Total"] <- 99

  expect_error(
    check_table1_sums(table1),
    "row totals: Example variable/Level A/2023",
    fixed = TRUE
  )
})

test_that("Table 1 column total mismatches identify the problem group", {
  table1 <- table1_sum_fixture()
  table1$total_N[table1$status == "Yes"] <- 51

  expect_error(
    check_table1_sums(table1),
    "column totals: Example variable/2023/Yes",
    fixed = TRUE
  )
})

test_that("Table 1 checks require the columns used in the calculations", {
  table1 <- table1_sum_fixture()
  table1$perc <- NULL

  expect_error(
    check_table1_sums(table1),
    "missing columns: perc",
    fixed = TRUE
  )
})
