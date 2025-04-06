################################################################################
#            SPLIT TRAINING AND TEST DATA SET BY LIVELIHOOD SYSTEM             #

#                    Split rules: Training 84%%; Test: 16%                     #
################################################################################

## ---- Pastoral livelihood systems' sets --------------------------------------

split_pasto <- grouped_admissions |>
  mutate(
    .admissions = do.call(
      what = row_wise_box_cox,
      args = list(admissions = sam_admissions, lsystems = lsystems)
    )
  ) |>
  filter(lsystems == "Pastoral") |>
  initial_time_split(prop = 0.839)


### ------------------------------------------ Extract training and testing ----

train_data_pasto <- training(split_pasto)
test_data_pasto <- testing(split_pasto)


## ---- Agropastoral livelihood systems' sets ----------------------------------

split_agropasto <- grouped_admissions |>
  filter(lsystems == "Agropastoral") |>
  initial_time_split(prop = 0.839)


### ------------------------------------------ Extract training and testing ----

train_data_agropasto <- training(split_agropasto)
test_data_agropasto <- testing(split_agropasto)

## ---- Riverine livelihood systems' sets --------------------------------------

split_riverine <- grouped_admissions |>
  filter(lsystems == "Riverine") |>
  initial_time_split(prop = 0.839)


### ------------------------------------------ Extract training and testing ----

train_data_riverine <- training(split_riverine)
test_data_riverine <- testing(split_riverine)

## ---- Urban/IDPs livelihood systems' sets ------------------------------------

split_urbanidps <- grouped_admissions |>
  filter(lsystems == "Urban/IDPs") |>
  initial_time_split(prop = 0.839)


### ------------------------------------------ Extract training and testing ----

train_data_urbanidps <- training(split_urbanidps)
test_data_urbanidps <- testing(split_urbanidps)

############################## End of workflow #################################