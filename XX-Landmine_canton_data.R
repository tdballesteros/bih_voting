
# calculate estimates for minefield prevelance in 1997

### load libraries ---------------------------------------------------------------------------------
library(readxl)
library(units)
library(dplyr)

### load data --------------------------------------------------------------------------------------
# load canton minefield data
landmines <- readxl::read_xlsx("Data/unmac_minefield_canton_data_1997.xlsx", skip = 1)

# load formatted postwar shapefile
bih_postwar_municipalities_shapefile <- sf::read_sf(
  "Shape Files/bih_postwar_municipalities_shapefile_formatted.shp",
  quiet = TRUE
)

# post-war municipality metadata
postwar_metadata <- readxl::read_xlsx("Data/bih_postwar_municipality_metadata.xlsx")

### format data ------------------------------------------------------------------------------------
### canton area ------------------------------------------------------------------------------------
canton_area <- bih_postwar_municipalities_shapefile %>%
  dplyr::group_by(canton) %>%
  dplyr::summarise(
    .groups = 'drop'
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    canton_area = units::set_units(sf::st_area(geometry), km^2)
  ) %>%
  as.data.frame() %>%
  dplyr::mutate(
    # Brčko is grouped in Republika Srpska as it is not included in the canton-level data
    entity = ifelse(
      canton %in% c("Banja Luka", "Bijeljina", "Brčko", "Doboj", "Foča", "Istočno Sarajevo",
                    "Trebinje", "Vlasenica"),
      "Republika Srpska",
      "FBIH"
      )
    ) %>%
  dplyr::mutate(canton = ifelse(entity == "Republika Srpska", "Republika Srpska", canton)) %>%
  dplyr::group_by(canton) %>%
  dplyr::summarise(
    canton_area = sum(canton_area),
    .groups = 'drop'
  ) %>%
  dplyr::ungroup()

### federation municipality area -------------------------------------------------------------------
canton_area_formatted <- canton_area %>%
  dplyr::mutate(canton = dplyr::case_match(
    canton,
    "Banja Luka" ~ "Banja Luka",
    "Istočno Sarajevo" ~ "Istočno Sarajevo",
    .default = canton
  ))

fbih_area <- bih_postwar_municipalities_shapefile %>%
  dplyr::select(municipality = mncplty, geometry) %>%
  dplyr::full_join(postwar_metadata, by = "municipality") %>%
  # dplyr::filter(entity == "Federacija Bosna i Hercegovina") %>%
  dplyr::mutate(
    mun_area = units::set_units(sf::st_area(geometry), km^2)
  ) %>%
  as.data.frame() %>%
  dplyr::full_join(canton_area_formatted, by = "canton")

### minefields -------------------------------------------------------------------------------
# landmines
landmines1 <- landmines %>%
  dplyr::select(canton = `Federation:`, minefields = Minefields) %>%
  replace(is.na(.), 0) %>%
  dplyr::mutate(canton = ifelse(canton == "Republika Srpska:", "Republika Srpska", canton)) %>%
  dplyr::filter(canton != "Total:") %>%
  dplyr::full_join(canton_area, by = "canton") %>%
  dplyr::mutate(
    minefields_per_sq_km = minefields / canton_area
  )

# replace Republika Srpska-wide estimate with canton entries

# pull rs density estimate
rs_minefield_density_est <- landmines1$minefields_per_sq_km[landmines1$canton == "Republika Srpska"]

landmines_rs_cantons <- data.frame(
  canton = c("Banja Luka", "Bijeljina", "Brčko", "Doboj", "Foča", "Istočno Sarajevo",
             "Trebinje", "Vlasenica"),
  minefields_per_sq_km = rep(rs_minefield_density_est, 8)
)

# append data
landmines1 <- landmines1 %>%
  dplyr::select(canton, minefields_per_sq_km) %>%
  dplyr::filter(canton != "Republika Srpska") %>%
  rbind(landmines_rs_cantons) %>%
  dplyr::arrange(canton)

# write formatted data
write.csv(landmines1, "Formatted Data/landmines_1997.csv", row.names = FALSE)

### minefields -------------------------------------------------------------------------------
# landmines
landmines2 <- landmines %>%
  dplyr::select(canton = `Federation:`, minefields = Minefields) %>%
  replace(is.na(.), 0) %>%
  dplyr::mutate(canton = ifelse(canton == "Republika Srpska:", "Republika Srpska", canton)) %>%
  dplyr::filter(canton != "Total:") %>%
  dplyr::full_join(canton_area, by = "canton") %>%
  dplyr::mutate(
    minefields_per_sq_km = minefields / canton_area
  )

# replace Republika Srpska-wide estimate with canton entries

# pull rs density estimate
rs_minefield_density_est <- landmines$minefields_per_sq_km[landmines$canton == "Republika Srpska"]

landmines_rs_cantons <- data.frame(
  canton = c("Banja Luka", "Bijeljina", "Brčko", "Doboj", "Foča", "Istočno Sarajevo",
             "Trebinje", "Vlasenica"),
  minefields_per_sq_km = rep(rs_minefield_density_est, 8)
)

# append data
landmines <- landmines %>%
  dplyr::select(canton, minefields_per_sq_km) %>%
  dplyr::filter(canton != "Republika Srpska") %>%
  rbind(landmines_rs_cantons) %>%
  dplyr::arrange(canton)

# write formatted data
write.csv(landmines, "Formatted Data/landmines_1997.csv", row.names = FALSE)

