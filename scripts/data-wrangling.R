################################################################################
#                                WRANGLE DATA                                  #
################################################################################


## ---- Tidy the data ----------------------------------------------------------

month_admit <- admissions |>
  pivot_longer(
    cols = !c(region, district, lsystems),
    names_to = "time",
    values_to = "admissions"
  ) |>
  mutate(
    time = gsub(pattern = "^X", "", x = time),
    time = gsub(pattern = "\\.", "/", x = time),
    Monthly = ymd(as.Date(time, format = "%d/%m/%Y")) |>
      yearmonth(time)
  ) |>
  relocate(Monthly, .before = admissions) |>
  select(-time)


## ---- Remove districts with zero admissions ----------------------------------

### --------------------------------------- List of district to be excluded ----
list <- c(
  "Ceel_Dheere", "Jalalaqsi", "Jamaame", "Kurtunwaarey", "Sablaale",
  "Adan Yabaal", "Bu'aale", "Jilib", "Saakow/Salagle", "Sheik", "Cadale",
  "Xarardheere"
)

### --------------------------------------------------- Apply the exclusion ----

month_admit <- month_admit |>
  filter(!(district %in% list))

## ---- Create a hierarchical TS of admission by livelihood systems ------------

hts <- month_admit |>
  summarise_admissions(
    .group = TRUE,
    time = "M"
  ) |>
  aggregate_key(
    .spec = lsystems,
    admissions = sum(admissions)
  )
############################## End of workflow #################################
