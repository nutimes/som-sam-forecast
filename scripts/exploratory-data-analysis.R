################################################################################
#                           EXPLORATORY DATA ANALYIS                           #
################################################################################

## ---- Time plot --------------------------------------------------------------
grouped_admissions |> 
  autoplot(.vars = sam_admissions) + 
  facet_grid(vars(lsystems), scales = "free_y") +
  theme(legend.position = "none")

## ---- Seasonal plot ----------------------------------------------------------
gg_season(
  data = grouped_admissions,
  y = sam_admissions,
  period = 12
)

############################## End of workflow #################################
