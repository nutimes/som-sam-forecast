################################################################################
#                                WRANGLE DATA                                  #
################################################################################


## ---- Tidy the data ----------------------------------------------------------

admissions <- input_data |>
  pivot_longer(
    cols = !c(region, district, lsystems),
    names_to = "time",
    values_to = "sam_admissions"
  ) |>
  mutate(
    time = gsub(pattern = "^X", "", x = time),
    time = gsub(pattern = "\\.", "/", x = time),
    Monthly = ymd(as.Date(time, format = "%d/%m/%Y")) |>
      yearmonth(time)
  ) |>
  relocate(Monthly, .before = sam_admissions) |>
  select(-time)


## ---- Remove districts with zero admissions ----------------------------------

### --------------------------------------- List of district to be excluded ----
list <- c(
  "Ceel_Dheere", "Jalalaqsi", "Jamaame", "Kurtunwaarey", "Sablaale",
  "Adan Yabaal", "Bu'aale", "Jilib", "Saakow/Salagle", "Sheik", "Cadale",
  "Xarardheere"
)

### --------------------------------------------------- Apply the exclusion ----

admissions <- admissions |>
  filter(!(district %in% list))

## ---- Summarise admissions ---------------------------------------------------

grouped_admissions <- admissions |>
  summarise_admissions(
    .group = TRUE,
    time = "M"
  )

ungrouped_admissions <- admissions |>
    summarise_admissions(
      .group = FALSE,
      time = "M"
    )

############################## End of workflow #################################
