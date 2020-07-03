devtools::load_all()

library(dplyr)
library(sf)

stl_holc_1937 <- st_read("inst/extdata/stl_holc_1937.geojson", crs = 4326) %>%
  select(area, area2) %>%
  rename(
    holc_id = area,
    holc_grade = area2
  )

save(stl_holc_1937, file = "data/stl_holc_1937.rda")

stl_holc_1940 <- st_read("inst/extdata/stl_holc_1940.geojson", crs = 4326) %>%
  select(area, area2) %>%
  rename(
    holc_id = area,
    holc_grade = area2
  )

save(stl_holc_1940, file = "data/stl_holc_1940.rda")
