library(tidyverse)

# Fill missing DIC with the closest-date measurement at the same site.
# dic_na = measured DIC only (NA where not measured); dic = gap-filled DIC used downstream.
# If two measured dates are equally close (one before, one after), their mean is used.
# Rewrites the `dic` column (gap-filled rows only) of data/temp_dic_ph.csv in place.

dat <- read_csv("data/temp_dic_ph.csv", col_types = cols(flag = col_character()))

measured <- dat |>
  filter(!is.na(dic_na)) |>
  select(site, dic_date = sampledate, dic_meas = dic_na)

fills <- dat |>
  filter(is.na(dic_na)) |>
  select(sampledate, site) |>
  inner_join(measured, by = "site", relationship = "many-to-many") |>
  mutate(gap_days = abs(as.numeric(dic_date - sampledate))) |>
  group_by(sampledate, site) |>
  filter(gap_days == min(gap_days)) |>
  summarise(dic_fill = mean(dic_meas), gap_days = first(gap_days), n_tied = n(), .groups = "drop")

dat <- dat |>
  left_join(fills, by = c("sampledate", "site")) |>
  # only replace gap-filled rows; measured rows (incl. flagged corrections) are left as-is
  mutate(dic = if_else(is.na(dic_na), dic_fill, dic)) |>
  select(-dic_fill, -gap_days, -n_tied) |>
  mutate(flag = replace_na(flag, ""))

stopifnot(!anyNA(dat$dic))
write_csv(dat, "data/temp_dic_ph.csv", na = "NA", eol = "\r\n")  # original file uses CRLF
