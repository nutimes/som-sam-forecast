################################################################################
#                                  READ DATA                                   #
################################################################################


## ---- Read data --------------------------------------------------------------

### --------------------------------------- Decrypt and read admission data ----
input_data <- decrypt(
  expr = read.csv("data-raw/admissions.csv"),
  key = secret_key
)

### ------------------------------------------------------- Read shapefiles ----

#### Download zipfile of Somalia shapefiles from Humanitarian Data Exchange ----
download_som_shp(overwrite = FALSE)

#### Read zipped Somalia shapefile ----
read_somalia_shp(
  path_to_zip = "data-raw/somalia-shp.zip",
  overwrite = TRUE,
  layer = 2
)

#### Somalia's admin 2 ----
som2 <- st_read(
  dsn = "data-raw/som-shp",
  layer = "som_admbnda_adm2_ocha_20250108",
  quiet = TRUE
)

################################ End of workflow ###############################
