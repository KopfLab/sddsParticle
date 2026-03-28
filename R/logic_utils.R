# format duration as approximate time (2.3 weeks, 1.5 hrs)
format_duration <- function(x) {
  x <- lubridate::as.duration(x)
  out <- x |> as.character() |> stringr::str_extract("(?<=~)\\d[^)]+")
  out[is.na(out)] <- signif(as.numeric(x[is.na(out)]), 3) |> paste("seconds")
  return(out)
}

# format duration in long format (5 hrs 2 minutes 30 sections)
format_duration_long <- function(x) {
  sec <- lubridate::as.duration(x) |> as.numeric("sec")
  d <- sec %/% 86400
  h <- sec %/% 3600 %% 24
  m <- sec %/% 60 %% 60
  s <- sec %% 60

  s_fmt <- "{s} sec{?s}"
  m_fmt <- "{m} min{?s}" |> paste(s_fmt)
  h_fmt <- "{h} hour{?s}" |> paste(m_fmt)
  d_fmt <- "{d} day{?s}" |> paste(h_fmt)

  fmts <- dplyr::case_when(
    d > 0 ~ d_fmt,
    h > 0 ~ h_fmt,
    m > 0 ~ m_fmt,
    TRUE ~ s_fmt,
  )
  out <- purrr::pmap_chr(list(fmts, s, m, h, d), function(fmt, s, m, h, d) {
    format_inline(fmt)
  })
  out[is.na(sec)] <- NA_character_
  return(out)
}
