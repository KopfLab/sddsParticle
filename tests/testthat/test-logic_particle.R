# Offline tests for the pure helpers in logic_particle.R (no network required).

test_that("check_commands_success() decodes the return value bit mask", {
  # no commands -> empty logical
  expect_equal(check_commands_success(0L, 0L), logical(0))

  # single command: 0 means success, anything else failure
  expect_true(check_commands_success(0L, 1L))
  expect_false(check_commands_success(-5L, 1L))
  expect_false(check_commands_success(3L, 1L))

  # multiple commands: each set bit marks a failed command
  expect_equal(check_commands_success(0L, 3L), c(TRUE, TRUE, TRUE))
  # 2 == 0b010 -> second command failed
  expect_equal(check_commands_success(2L, 3L), c(TRUE, FALSE, TRUE))
  # 1 == 0b01 -> first command failed
  expect_equal(check_commands_success(1L, 2L), c(FALSE, TRUE))
})
