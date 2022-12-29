test_that("compute_split works", {

    n_col <- 19
    n_row <- 31
    col_size <- 7
    row_size <- 5
    col_overlap <- 1
    row_overlap <- 3

    split <- compute_split(n_col = n_col, n_row = n_row, col_size = col_size,
                           row_size = row_size, col_overlap = col_overlap,
                           row_overlap = row_overlap)

    expect_equal(split[["ocol_from"]][1], 1)
    expect_equal(split[["orow_from"]][1], 1)
    expect_equal(split[["ocol_to"]][nrow(split)], n_col)
    expect_equal(split[["orow_to"]][nrow(split)], n_row)

    id_first_x <- split$x_id %in% head(unique(split$x_id), -1)
    expect_true(all(split[id_first_x, "col_to"] -
                    split[id_first_x, "col_from"] == col_size - 1))

    id_first_y <- split$y_id %in% head(unique(split$y_id), - 1)
    expect_true(all(split[id_first_y, "row_to"] -
                    split[id_first_y, "row_from"] == row_size - 1))

})


test_that("compute_split works with no overlap", {

    split <- compute_split(n_col = 19, n_row = 31, col_size = 11,
                           row_size = 7, col_overlap = 0,
                           row_overlap = 0)

    expect_equal(split[["ocol_from"]][1], 1)
    expect_equal(split[["orow_from"]][1], 1)
    expect_equal(split[["ocol_to"]][nrow(split)], 19)
    expect_equal(split[["orow_to"]][nrow(split)], 31)

    expect_equal(split[["col_from"]], split[["ocol_from"]])
    expect_equal(split[["row_from"]], split[["orow_from"]])
    expect_equal(split[["col_to"]], split[["ocol_to"]])
    expect_equal(split[["row_to"]], split[["orow_to"]])

})
