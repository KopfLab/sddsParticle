# logic functions for the app ======

# convert the simplified tree with values for use in a selector table
convert_simplified_tree_w_values_to_table <- function(
  simplified_tree_w_valus,
  timezone = Sys.timezone()
) {
  simplified_tree_w_valus |>
    dplyr::left_join(
      devices |> dplyr::select("id", "corename" = "name"),
      by = c("coreid" = "id")
    ) |>
    dplyr::mutate(
      device_info = sprintf(
        "%s\n%s",
        corename,
        .data$published_at |>
          lubridate::with_tz(!!timezone) |>
          format("%b %d %H:%M:%S")
      )
    ) |>
    dplyr::select(
      "device_info",
      "path",
      "parent",
      "label",
      "value_w_units"
    ) |>
    tidyr::pivot_wider(
      names_from = "device_info",
      values_from = "value_w_units"
    )
}
