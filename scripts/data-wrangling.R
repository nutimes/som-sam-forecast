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


## ---- Box-Cox transformation to stabilized variance  -------------------------

### -------------------------------------------------------------- National ----

#### Get lambda ----
lambda_national <- ungrouped_admissions |>
  features(
    .var = sam_admissions,
    features = guerrero
  ) |>
  pull(lambda_guerrero)

### Transform ----
ungrouped_admissions <- admissions |>
  summarise_admissions(
    .group = FALSE,
    time = "M"
  ) |>
  mutate(
    .admissions = do.call(
      what = box_cox,
      args = list(x = sam_admissions, lambda = lambda_national)
    )
  )

### ---------------------------------------------------- livelihood systems ----

####  Lambda Pastoral ----
lambda_pastoral <- grouped_admissions |>
  filter(lsystems == "Pastoral") |>
  features(
    .var = sam_admissions,
    features = guerrero
  ) |>
  pull(lambda_guerrero)

#### Lambda Agropastoral ----
lambda_agropastoral <- grouped_admissions |>
  filter(lsystems == "Agropastoral") |>
  features(
    .var = sam_admissions,
    features = guerrero
  ) |>
  pull(lambda_guerrero)

#### Lambda Riverine ----
lambda_riverine <- grouped_admissions |>
  filter(lsystems == "Riverine") |>
  features(
    .var = sam_admissions,
    features = guerrero
  ) |>
  pull(lambda_guerrero)

#### Lambda Urban/IDP's ----
lambda_urban_idps <- grouped_admissions |>
  filter(lsystems == "Urban/IDPs") |>
  features(
    .var = sam_admissions,
    features = guerrero
  ) |>
  pull(lambda_guerrero)


### ----------------------------------------- Apply row-wise transformation ----

grouped_admissions <- grouped_admissions |>
  mutate(
    .admissions = do.call(
      what = row_wise_box_cox,
      args = list(admissions = sam_admissions, lsystems = lsystems)
    )
  )

############################## End of workflow #################################
