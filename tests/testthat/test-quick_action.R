# Offline tests for the sdds_ui_quick_action() definition helper (no network required).

test_that("sdds_ui_quick_action() builds a complete definition", {
  qa <- sdds_ui_quick_action(
    "restart",
    "Restart",
    icon = shiny::icon("gears"),
    path = "SYSTEM.action",
    value = "restart"
  )
  expect_type(qa, "list")
  expect_named(
    qa,
    c("id", "label", "icon", "path", "value", "flag_as_changed")
  )
  expect_equal(qa$id, "restart")
  expect_equal(qa$path, "SYSTEM.action")
  expect_equal(qa$value, "restart")
})

test_that("sdds_ui_quick_action() flag_as_changed defaults from value", {
  # a value implies the field should be flagged as changed
  expect_true(
    sdds_ui_quick_action("a", "A", path = "p", value = "v")$flag_as_changed
  )
  # no value -> just selects the path, not flagged as changed
  expect_false(sdds_ui_quick_action("a", "A", path = "p")$flag_as_changed)
})

test_that("sdds_ui_quick_action() validates id and path", {
  expect_error(sdds_ui_quick_action(42, "A", path = "p"), "must be a string")
  expect_error(sdds_ui_quick_action("a", "A"), "must be a string")
})
