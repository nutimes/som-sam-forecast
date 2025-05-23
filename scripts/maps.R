################################################################################
#                     CREATE MAP OF THE LIVELIHOOD SYSTEM                      #
################################################################################


## ---- Transform Somalia CRS to UTM -------------------------------------------

somalia <- st_transform(
  x = som2,
  crs = 20538
)

## ---- Plot a map of the excluded districts -----------------------------------

### -------------------------------------------- List of excluded districts ----

excluded <- c(
  "Ceel Dheer", "Jalalaqsi", "Jamaame", "Kurtunwaarey", "Sablaale",
  "Adan Yabaal", "Bu'aale", "Jilib", "Saakow", "Sheikh", "Cadale",
  "Xarardheere"
)

### ------------------------------------------------------------ Plot a map ----

map_incl_excl <- somalia |>
  mutate(
    inc_exc = ifelse(ADM2_EN %in% excluded, 1, 0),
    excl = ifelse(inc_exc == 1, ADM2_EN, NA)
  ) |>
  ggplot() +
  geom_sf(
    aes(fill = factor(inc_exc)),
    show.legend = TRUE,
    color = "grey",
  ) +
  geom_sf_text(
    aes(label = excl),
    na.rm = TRUE,
    size = 3
  ) +
  scale_fill_manual(
    values = c("0" = "#7AA691", "1" = "#FDFEFD"),
    labels = c("Included", "Excluded"),
    name = "District status"
  ) +
  theme_void() +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 14, face = "bold")
  )

## ---- Plot a map of livelihood systems ---------------------------------------

### --------------------------- List of districts in each livelihood system ----

#### Pastoral ----
wrong_p <- c("Laas_Caanood", "Ceel_Afweyn", "Ceel_Waaq", "Belet_Xaawo")
correct_p <- c("Laas Caanood", "Ceel Afweyn", "Ceel Waaq", "Belet Xaawo")
pastoral <- input_data |>
  select(district, lsystems) |>
  filter(lsystems == "Pastoral") |>
  mutate(district = recode(district, !!!setNames(correct_p, wrong_p))) |>
  pull(district)

#### Agropastoral ----
wrong_ap <- c("Belet_Weyne", "Ceel_Buur", "Bulo_Burto", "Ceel_Dheere", "Buur_Hakaba", "Saakow/Salagle")
correct_ap <- c("Belet Weyne", "Ceel Buur", "Bulo Burto", "Ceel Dheere", "Buur Hakaba", "Saakow")
agropastoral <- input_data |>
  select(district, lsystems) |>
  filter(lsystems == "Agropastoral") |>
  mutate(district = recode(district, !!!setNames(correct_ap, wrong_ap))) |>
  pull(district)

#### Riverine ----
riverine <- input_data |>
  select(district, lsystems) |>
  filter(lsystems == "Riverine") |>
  pull(district)

#### Urban/IDPs ----
urban_idps <- input_data |>
  select(district, lsystems) |>
  filter(lsystems == "Urban/IDPs") |>
  pull(district)


### ----------------------- Create a vector of livelihood system categories ----
somalia <- somalia |>
  mutate(
    lsystem = case_when(
      ADM2_EN %in% pastoral ~ 1,
      ADM2_EN %in% agropastoral ~ 2,
      ADM2_EN %in% riverine ~ 3,
      .default = 4
    )
  )

### -------------------------------------------- Create regional boundaries ----
regions <- somalia |>
  group_by(ADM1_EN) |>
  summarise(geometry = st_union(geometry))

### ------------------------------------------------- Livelihood system map ----
map_lsystems <- somalia |>
  ggplot() +
  geom_sf(
    mapping = aes(
      fill = factor(lsystem),
    ),
    show.legend = TRUE,
    linewidth = 0.2,
    color = "grey"
  ) +
  scale_fill_manual(
    values = c("1" = "#A67C52", "2" = "#4CAF50", "3" = "#2196F3", "4" = "#616161"),
    labels = c("Pastoreio", "Agropastoreio", "Riverinho", "Urbano/IDPs"),
    name = "Sistemas de subsistência"
  ) +
  theme_void() +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 14, face = "bold")
  )
