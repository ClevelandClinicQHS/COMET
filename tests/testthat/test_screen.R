## Test 1

test_that("screening", {
  test_cans <- tibble(c_id = as.integer(c(1, 2, 3, 4, 5, 6)),
                      hgt_in = as.numeric(c(65.5, 70, 75, 61, 68.5, 63)),
                      dx_grp = c('A', 'D', 'C', 'D', 'D', 'D'),
                      surg_type = factor(c('D', 'E', 'D', 'D', 'E', 'S'), levels = c('D', 'E', 'S')),
                       abo = c('O', 'A', 'B', 'O', 'O', 'O'))

  test_dons <-  tibble(d_id = as.integer(c(11, 12, 13, 14, 15, 16)),
                       hgt_in = as.numeric(c(66.5, 70.5, 71.5, 66.5, 66, 69.5)),
                       don_org = factor(c('DLU', 'DLU', 'LUL', 'DLU', 'DLU', 'DLU'), levels = c('DLU', 'LUL')),
                       abo = factor(c('A', 'O', 'A', 'AB', 'A', 'O'), levels = c('A', 'AB', 'B', 'O')))

  expect_equal(height_screen(test_cans, test_dons),
               tibble(c_id = as.integer(c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 5, 5, 5, 5, 5)),
                      d_id = as.integer(c(11, 12, 13, 14, 15, 16, 11, 12, 13, 14, 15, 16, 11, 12, 14, 15, 16)),
                      match_single = as.logical(c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)),
                      match_double = as.logical(c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE)))
               )

  expect_equal(abo_screen(test_cans, test_dons),
               tibble(c_id = as.integer(c(1, 1, 2, 2, 2, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6)),
                      d_id = as.integer(c(12, 16, 11, 12, 13, 15, 16, 12, 16, 12, 16, 12, 16, 12, 16)),
                      abo_exact = as.logical(c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)))
               )

  expect_equal(count_screen(test_cans, test_dons),
               tibble(c_id = as.integer(c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6)),
                      d_id = as.integer(c(11, 12, 14, 15, 16, 11, 12, 13, 14, 15, 16, 11, 12, 14, 15, 16, 11, 12, 14, 15, 16, 11, 12, 13, 14, 15, 16, 11, 12, 13, 14, 15, 16)))
  )
  })


