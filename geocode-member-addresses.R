library(tidyverse)
library(rstudioapi)
library(airtabler) # remotes::install_github("bergant/airtabler")
library(geoclient) # remotes::install_github("austensen/geoclient")
library(sf)
library(dotenv)

# Edit ".env_sample" to set variables and save as ".env"
load_dot_env(".env")

geoclient_api_keys(Sys.getenv("GEOCLIENT_ID"), Sys.getenv("GEOCLIENT_KEY"))


# Members from Airtable ---------------------------------------------------

# Get all the members and their info from the RTU Airtable

rtu_at <- airtable(base = "apphxizlDnDKtObOk", "Members")

at_members_all <- rtu_at$Members$select_all() %>% 
  as_tibble() %>% 
  transmute(
    uid = id,
    name = Name,
    pronouns = Pronouns,
    phone = `Phone Number`,
    email = `E-Mail`,
    address = str_c(Address, ", ", coalesce(`Zip code`, "QN"))
  ) %>% 
  filter(!is.na(name))

# There are a few records without any address, and for now these are just
# dropped completely.
at_no_address <- at_members_all %>% filter(is.na(address))

# Geocoding ---------------------------------------------------------------

# There are some addresses that can't be geocoded with the API, so these
# records are manually located and added to this file. Further down there is a
# point to check for new records that need to be added here.
manual_geocode_file <- "data/manual_geocode_results.csv"

if (file.exists(manual_geocode_file)) {
  manual_geocode_results <- manual_geocode_file %>% 
    read_csv(col_types = "c_dd") %>% 
    distinct(uid, .keep_all = TRUE)
} else {
  manual_geocode_results <- tibble(
    uid = character(), 
    latitude = numeric(), 
    longitude = numeric()
  )
}

# After dropping records already manually geocoded or missing addresses, we do a
# first attempt to geocode the addresses using NYC's Geoclient API. The
# "geo_search_data" function returns a new dataframe with the results with the
# same length and in the same order as the input data, so we can just
# concatenate it to the input. Then we drop any records that couldn't be
# geocoded (no coordinates).
first_geocode <- at_members_all %>% 
  anti_join(at_no_address, by = "uid") %>% 
  anti_join(manual_geocode_results, by = "uid") %>% 
  bind_cols(
    geo_search_data(., address) %>% 
      transmute(
        clean_address = str_c(houseNumber, " ", str_to_title(firstStreetNameNormalized), ", ", zipCode),
        latitude, 
        longitude
      )
  ) %>% 
  filter(!is.na(latitude))

# Most of the addresses are in Queens (Ridgewood), so we add on the borough name
# to help out the geocoder and repeat the same process as before for the
# remaining records.
second_geocode <- at_members_all %>% 
  anti_join(at_no_address, by = "uid") %>% 
  anti_join(manual_geocode_results, by = "uid") %>% 
  anti_join(first_geocode, by = "uid") %>% 
  mutate(address_boro = str_c(address, ", queens")) %>% 
  bind_cols(
    geo_search_data(., address_boro) %>% 
      transmute(
        clean_address = str_c(houseNumber, " ", str_to_title(firstStreetNameNormalized), ", ", zipCode),
        latitude, 
        longitude
      )
  ) %>% 
  filter(!is.na(latitude)) %>% 
  select(-address_boro)

# If there are still any records that have address that can't be geocoded using
# the API, we can look them up manually from this list and add the information
# to a the CSV file that's loaded at the top of this script.
to_manual_geocode <- at_members_all %>%
  anti_join(at_no_address, by = "uid") %>%
  anti_join(manual_geocode_results, by = "uid") %>% 
  anti_join(first_geocode, by = "uid") %>%
  anti_join(second_geocode, by = "uid") %>% 
  select(uid, address)

if (nrow(to_manual_geocode) != 0) {
  to_manual_geocode %>% 
    add_column(latitude = "", longitude = "") %>% 
    write_csv(manual_geocode_file, append = file.exists(manual_geocode_file))
  
  navigateToFile(manual_geocode_file)
  
  stop(
    str_glue("There are {nrow(to_manual_geocode)} records to manually geocode!
             Please fill in the coordinates and re-run this geocoding section."), 
    call. = FALSE
  )
}


# Now that all the records are geocoded we can put the final dataset back
# together from all the parts, and use the normalized address where possible.
all_geocoded <- at_members_all %>% 
  inner_join(manual_geocode_results, by = "uid") %>% 
  bind_rows(first_geocode, second_geocode) %>% 
  filter(!is.na(latitude)) %>% 
  mutate(address = coalesce(clean_address, address)) %>% 
  select(-clean_address) %>% 
  mutate_if(is.character, replace_na, "-") %>%
  st_as_sf(coords = c("longitude", "latitude"))


write_rds(all_geocoded, "app/data/members.rds")
