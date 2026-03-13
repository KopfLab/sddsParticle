test_that("parse_json()", {
  # errors
  parse_json() |> expect_error("must be provided as a single string")
  parse_json(42) |> expect_error("must be provided as a single string")
  parse_json("NA") |> expect_error("invalid JSON")
})

test_that("split_event_data()", {
  # errors
  split_event_data("{\"a\":5}") |>
    expect_error("cannot be named")
})

test_that("sdds_split_events_data()", {
  # errors
  sdds_split_events_data() |>
    expect_error("must be a data frame")
})

test_that("sdds_parse_tree()", {
  # errors
  sdds_parse_tree("{\"s\":5}") |> expect_error("not a complete tree")

  # values
  sdds_parse_tree(NA_character_) |>
    expect_equal(tibble(enums = list(NULL), tree = list(NULL)))
})

test_that("sdds_parse_treedata()", {
  # errors
  sdds_parse_values("{\"d\":5}") |>
    expect_error("not a complete set of tree values")

  # values
  sdds_parse_values(NA_character_) |>
    expect_equal(tibble(values = list(NULL)))
})
