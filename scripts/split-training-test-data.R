################################################################################
#            SPLIT TRAINING AND TEST DATA SET BY LIVELIHOOD SYSTEM             #

#                    Split rules: Training 84%%; Test: 16%                     #
################################################################################

## ---- Pastoral livelihood systems' sets --------------------------------------

pasto_split <- grouped_admissions |>
  mutate(
    .admissions = do.call(
      what = row_wise_box_cox,
      args = list(admissions = sam_admissions, lsystems = lsystems)
    )
  ) |>
  filter(lsystems == "Pastoral") |>
  initial_time_split(prop = 0.839)


### ------------------------------------------ Extract training and testing ----

pasto_train_data <- training(pasto_split)
pasto_test_data <- testing(pasto_split)


## ---- Agropastoral livelihood systems' sets ----------------------------------

agropasto_split <- grouped_admissions |>
  filter(lsystems == "Agropastoral") |>
  initial_time_split(prop = 0.839)


### ------------------------------------------ Extract training and testing ----

agropasto_train_data <- training(agropasto_split)
agropasto_test_data <- testing(agropasto_split)

## ---- Riverine livelihood systems' sets --------------------------------------

riverine_split <- grouped_admissions |>
  filter(lsystems == "Riverine") |>
  initial_time_split(prop = 0.839)


### ------------------------------------------ Extract training and testing ----

riverine_train_data <- training(riverine_split)
riverine_test_data <- testing(riverine_split)

## ---- Urban/IDPs livelihood systems' sets ------------------------------------

urbanidps_split <- grouped_admissions |>
  filter(lsystems == "Urban/IDPs") |>
  initial_time_split(prop = 0.839)


### ------------------------------------------ Extract training and testing ----

urbanidps_train_data <- training(urbanidps_split)
urbanidps_test_data <- testing(urbanidps_split)

############################## End of workflow #################################