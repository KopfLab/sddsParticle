# These tests exercise the pure value -> text converters and require no
# network / Particle cloud connection.

test_that("add_units() only appends units when both are present", {
  expect_equal(add_units("5", "V"), "5 V")
  expect_equal(add_units("5", NA_character_), "5")
  expect_equal(add_units(NA_character_, "V"), NA_character_)
})

test_that("basic value converters", {
  expect_equal(null_value_to_text(), "<no value>")
  expect_equal(text_value_to_text("abc", NA_character_), "abc")
  expect_equal(integer_value_to_text(5L, "counts"), "5 counts")
  expect_equal(enum_value_to_text("on"), "on")
  # doubles are rounded to 4 significant digits
  expect_equal(double_value_to_text(3.14159, "V"), "3.142 V")
  expect_equal(double_value_to_text(3.14159, NA_character_), "3.142")
})

test_that("version_value_to_text() decodes the packed version integer", {
  expect_equal(version_value_to_text(10203), "1.2.3")
  expect_equal(version_value_to_text(10000), "1.0.0")
  expect_equal(version_value_to_text(c(10203L, NA_integer_)), c("1.2.3", NA))
})

test_that("datetime_value_to_text() formats valid and passes through invalid", {
  expect_match(
    datetime_value_to_text("2024-06-15 10:30:00", "UTC"),
    "10:30:00 UTC"
  )
  # unparseable input is returned unchanged
  expect_equal(datetime_value_to_text("not a date", "UTC"), "not a date")
})

test_that("HHMM_value_to_text() renders the time of day", {
  expect_equal(HHMM_value_to_text(830, "UTC"), "08:30 UTC")
  expect_equal(HHMM_value_to_text(1445, "UTC"), "14:45 UTC")
})

test_that("duration_value_to_seconds() converts to seconds", {
  expect_equal(duration_value_to_seconds(5, "sec"), 5)
  expect_equal(duration_value_to_seconds(2, "min"), 120)
  expect_equal(duration_value_to_seconds(1, "hour"), 3600)
  expect_equal(duration_value_to_seconds(1, "day"), 86400)
  expect_error(duration_value_to_seconds(1, "fortnight"), "Unknown units")
})

test_that("duration_value_to_text() renders human readable durations", {
  expect_equal(duration_value_to_text(90, "sec"), "1 min 30 secs")
  expect_equal(duration_value_to_text(1, "hour"), "1 hour")
  expect_equal(duration_value_to_text(2, "day"), "2 days")
  expect_equal(duration_value_to_text(NA_real_, "sec"), NA_character_)
})

test_that("var_intervals_value_to_text() maps special values and falls back to duration", {
  expect_equal(
    var_intervals_value_to_text(-1L, "ms"),
    "average over global interval (if recording)"
  )
  expect_equal(var_intervals_value_to_text(0L, "ms"), "never send")
  expect_equal(
    var_intervals_value_to_text(1L, "ms"),
    "send each change (if recording)"
  )
  expect_equal(
    var_intervals_value_to_text(2L, "ms"),
    "send each change (always)"
  )
  # any other value is interpreted as a duration
  expect_equal(var_intervals_value_to_text(5000L, "ms"), "5 secs")
})
