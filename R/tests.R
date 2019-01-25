library(dplyr)
fbi = read.table('data/TestResponses.txt', header = TRUE)
fbi.test = fbi %>%
  distinct(Mating, Compare_Value, Inconclusive_Reason, .keep_all = TRUE)

library(testthat)

### ADD NEGATIVE TESTS

test_that("score_bb_data(inconclusive_mcar) correctly scores true/false positives and negatives",{
  expect_equal(score_bb_data(fbi.test, "inconclusive_mcar")[which(
    fbi.test$Mating == "Mates" & fbi.test$Compare_Value == "Individualization")], 1)
  expect_equal(score_bb_data(fbi.test, "inconclusive_mcar")[which(
    fbi.test$Mating == "Non-mates" & fbi.test$Compare_Value == "Individualization")], 0)
})
test_that("score_bb_data(inconclusive_incorrect) correctly scores true positives",{
  expect_equal(score_bb_data(fbi.test, "inconclusive_incorrect")[which(
    fbi.test$Mating == "Mates" & fbi.test$Compare_Value == "Individualization")], 1)
  expect_equal(score_bb_data(fbi.test, "inconclusive_incorrect")[which(
    fbi.test$Mating == "Non-mates" & fbi.test$Compare_Value == "Individualization")], 0)
})
test_that("score_bb_data(partial_credit) correctly scores true positives",{
  expect_equal(score_bb_data(fbi.test, "partial_credit")[which(
    fbi.test$Mating == "Mates" & fbi.test$Compare_Value == "Individualization")], 2)
  expect_equal(score_bb_data(fbi.test, "partial_credit")[which(
    fbi.test$Mating == "Non-mates" & fbi.test$Compare_Value == "Individualization")], 0)
})
test_that("score_bb_data(no_consensus_mcar) correctly scores true positives",{
  expect_equal(score_bb_data(fbi.test, "no_consensus_mcar")[which(
    fbi.test$Mating == "Mates" & fbi.test$Compare_Value == "Individualization")], 1)
  expect_equal(score_bb_data(fbi.test, "no_consensus_mcar")[which(
    fbi.test$Mating == "Non-mates" & fbi.test$Compare_Value == "Individualization")], 0)
})
test_that("score_bb_data(no_consensus_incorrect) correctly scores true positives",{
  expect_equal(score_bb_data(fbi.test, "no_consensus_incorrect")[which(
    fbi.test$Mating == "Mates" & fbi.test$Compare_Value == "Individualization")], 1)
  expect_equal(score_bb_data(fbi.test, "no_consensus_incorrect")[which(
    fbi.test$Mating == "Non-mates" & fbi.test$Compare_Value == "Individualization")], 0)
})


