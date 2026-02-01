
# This script is used to calculate several estimates of landmine and minefield prevalence throughout
# Bosnia and Herzegovina in the immediate aftermath of the Bosnian War. All estimates assume all
# landmines within Bosnia and Herzegovina originated during the war and all clearing of mines
# occurred after the conclusion of the war.


# Load Libraries -----------------------------------------------------------------------------------
library(readxl)
library(units)
library(sf)
library(dplyr)


# Load Data  ---------------------------------------------------------------------------------------

# Minefield Prevalence by Canton
landmines <- readxl::read_xlsx("Data/unmac_minefield_canton_data_1997.xlsx", skip = 1)


# Minefield 2004 Map Shapefile
minefield_2004_shapefile <- sf::read_sf(
  "Shape Files/bih_landmine_areas.gpkg",
  quiet = TRUE
)

# Postwar Municipalities Shapefile
bih_postwar_municipalities_shapefile <- sf::read_sf(
  "Shape Files/bih_postwar_municipalities_shapefile_formatted.shp",
  quiet = TRUE
)

# Prewar Municipalities Metadata
postwar_metadata <- readxl::read_xlsx("Data/bih_postwar_municipality_metadata.xlsx")


# Cantonal 1997 Estimates -------------------------------------------------------------------------

# Calculate estimates for minefield prevalence at the cantonal level in 1997.

## Calculate Canton Areas
## Calculate areas of the 10 cantons in the Federation, Brčko, and Republika Srpska.
canton_area <- bih_postwar_municipalities_shapefile %>%
  # create a new column containing either (1) the canton name for the 10 entities in the Federation,
  # (2) Brčko, or (3) Republika Srpska
  dplyr::mutate(
    canton = dplyr::case_when(
      entity == "Republika Srpska" ~ "Republika Srpska",
      entity == "Brčko" ~ "Brčko",
      .default = canton
    )) %>%
  dplyr::group_by(canton) %>%
  dplyr::summarise(.groups = 'drop') %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    area = units::set_units(sf::st_area(geometry), km^2)
  )

## Format Minefield Cantonal Data
landmines1 <- landmines %>%
  dplyr::select(canton = `Federation:`, minefields = Minefields) %>%
  replace(is.na(.), 0) %>%
  dplyr::mutate(canton = ifelse(canton == "Republika Srpska:", "Republika Srpska", canton)) %>%
  dplyr::filter(canton != "Total:") %>%
  dplyr::full_join(canton_area, by = "canton") %>%
  dplyr::mutate(
    minefields_per_sq_km = minefields / area
  )

## Replace Republika Srpska estimates with estimates for individual cantons.

# pull RS minefield density estimate
rs_minefield_density_est <- landmines1$minefields_per_sq_km[landmines1$canton == "Republika Srpska"]

landmines_rs_cantons <- data.frame(
  canton = c("Banja Luka", "Bijeljina", "Doboj", "Foča", "Istočno Sarajevo",
             "Trebinje", "Vlasenica"),
  minefields_per_sq_km = rep(rs_minefield_density_est, 7)
)

# append data
landmines1 <- landmines1 %>%
  dplyr::select(canton, minefields_per_sq_km) %>%
  dplyr::filter(!canton %in% c("Republika Srpska", "Brčko")) %>%
  rbind(landmines_rs_cantons)

# estimate Brčko
# 59.6 sq. km in Brčko are suspected areas - 12.04% of territory.
# 6.36% estimate in FBiH and 1.71% in Republika Srpska.

# Bosnia-wide estimate of 3 million landmines; 152 mines per square mile.

# Divide 3 million landmines from number of minefields in data to get estimate of mines per
# minefield.
total_minefields <- sum(landmines$Minefields, na.rm = TRUE)
average_mines_per_minefield = 3000000 / total_minefields

# estimated 152 mines per square mile
# convert to sq km
mines_per_sq_km_est <- 152 / 2.59

# pull Brčko size in sq km
brcko_area <- canton_area$area[canton_area$canton == "Brčko"]

brcko_mine_estimate <- mines_per_sq_km_est * brcko_area
brcko_minefield_estimate <- brcko_mine_estimate / average_mines_per_minefield
brcko_minefield_density_estimate <- units::set_units(as.numeric(brcko_minefield_estimate / brcko_area), 1/km^2)

# add Brčko estimate
landmines1 <- landmines1 %>%
  rbind(data.frame(
    canton = "Brčko",
    minefields_per_sq_km = brcko_minefield_density_estimate
  )) %>%
  dplyr::arrange(canton)

# write formatted data
write.csv(landmines1, "Formatted Data/landmines_1997.csv", row.names = FALSE)


# Minefield Map 2004 Estimates ---------------------------------------------------------------------

# Using a 2004 map showing approximate minefield areas, cleared minefield areas, post-war mine
# explosions, and frontlines of the battlefield, use the active and cleared minefield areas and
# post-war mine explosion location data to approximate minefield areas. Use this geocoded data
# to approximate proportion of post-war municipal areas at risk for landmine infection.

bih_postwar_municipalities_shapefile <- bih_postwar_municipalities_shapefile %>%
  sf::st_make_valid()

minefield_2004_shapefile <- minefield_2004_shapefile %>%
  sf::st_make_valid()


ggplot() +
  # Bottom layer (Base map)
  geom_sf(data = bih_postwar_municipalities_shapefile, fill = "gray80", color = "white") + 
  # Top layer (Overlay with transparency)
  geom_sf(data = minefield_2004_shapefile, fill = "blue", alpha = 0.5, color = "black") + 
  theme_minimal() +
  labs(title = "sh1 overlaid on sh2")

# confirm CRSs match
sf::st_crs(minefield_2004_shapefile)$proj4string
sf::st_crs(bih_postwar_municipalities_shapefile)$proj4string
# both WGS84

# spatial join
# bih_minefields_2004 <- sf::st_join(
#   minefield_2004_shapefile,
#   bih_postwar_municipalities_shapefile,
#   join = st_within,
#   left = TRUE
# )

bih_minefields_2004 <- sf::st_intersection(
  minefield_2004_shapefile,
  bih_postwar_municipalities_shapefile
  )

# collapse by municipality
bih_minefields_2004_perc_area <- bih_minefields_2004 %>%
  sf::st_make_valid() %>%
  dplyr::rename(municipality = mncplty) %>%
  dplyr::mutate(
    # calculate updated geometry area
    area = units::set_units(sf::st_area(geom), km^2)
    ) %>%
  dplyr::group_by(municipality) %>%
  dplyr::summarise(
    minefield_area = sum(area, na.rm = TRUE),
    municipality_area = min(mun_are, na.rm = TRUE)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    minefield_perc = units::drop_units(minefield_area / municipality_area),
  ) %>%
  dplyr::arrange(municipality) %>%
  as.data.frame() %>%
  dplyr::full_join(
    bih_postwar_municipalities_shapefile,
    dplyr::join_by("municipality" == "mncplty")
    ) %>%
  dplyr::select(municipality, minefield_area, minefield_perc) %>%
  replace(is.na(.), 0)

# write formatted data
write.csv(bih_minefields_2004_perc_area, "Formatted Data/landmines_2004.csv", row.names = FALSE)

## By Canton ---------------------------------------------------------------------------------------

bih_postwar_cantons_shapefile <- bih_postwar_municipalities_shapefile %>%
  dplyr::group_by(canton) %>%
  dplyr::summarise(canton_geometry = sf::st_union(geometry)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    canton_area = units::set_units(sf::st_area(canton_geometry), km^2)
  )

bih_minefields_2004_canton <- sf::st_intersection(
  minefield_2004_shapefile,
  bih_postwar_cantons_shapefile
  )

# collapse by canton
bih_minefields_2004_perc_canton_area <- bih_minefields_2004_canton %>%
  sf::st_make_valid() %>%
  dplyr::mutate(
    # calculate updated geometry area
    minefield_area = units::set_units(sf::st_area(geom), km^2)
  ) %>%
  dplyr::group_by(canton) %>%
  dplyr::summarise(
    minefield_area = sum(minefield_area, na.rm = TRUE),
    canton_area = min(canton_area, na.rm = TRUE)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    minefield_perc = units::drop_units(minefield_area / canton_area),
  ) %>%
  dplyr::arrange(canton) %>%
  as.data.frame() %>%
  dplyr::full_join(
    bih_postwar_cantons_shapefile,
    by = "canton"
  ) %>%
  dplyr::select(canton, minefield_area, minefield_perc) %>%
  replace(is.na(.), 0) %>%
  dplyr::mutate(
    canton = stringr::str_squish(canton)
  )

# write formatted data
write.csv(bih_minefields_2004_perc_canton_area, "Formatted Data/landmines_2004_canton.csv", row.names = FALSE)


