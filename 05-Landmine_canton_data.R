
# calculate estimates for minefield prevalence in 1997

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
    # add entity information
    entity = dplyr::case_when(
      canton %in% c("Banja Luka", "Bijeljina", "Doboj", "Foča", "Istočno Sarajevo", "Trebinje",
                    "Vlasenica") ~ "Republika Srpska",
      canton == "Brčko" ~ "Brćko",
      .default = "FBIH"
      ),
    # replace canton names in Republika Srpska with "Republika Srpska" to facilitate collapsing
    canton = ifelse(entity == "Republika Srpska", "Republika Srpska", canton)) %>%
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

### minefields 1 -----------------------------------------------------------------------------------
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
# 59.6 sq. km in Brčko are suspected areas - 12.04% of territory
# 6.36% estimate in FBiH and 1.71% in Republika Srpska

# Bosnia-wide estimate of 3 million landmines; 152 mines per square mile

# divide 3 million landmines from number of minefields in data to get estimate of mines per
# minefield
total_minefields <- sum(landmines$Minefields, na.rm = TRUE)
average_mines_per_minefield = 3000000 / total_minefields

# estimated 152 mines per square mile
# convert to sq km
mines_per_sq_km_est <- 152 / 2.59

# pull Brčko size in sq km
brcko_area <- canton_area$canton_area[canton_area$canton == "Brčko"]

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

