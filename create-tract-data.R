library(tidyverse) # general data manipulation
library(glue) # glue_sql()
library(DBI) # DB connection
library(dotenv) # get env variables
library(fs) # consistent file system operations
library(tidycensus) # census API
library(sf) # spatial data manipulation

# Edit ".env_sample" to set variables and save as ".env"
load_dot_env(".env")

# Free API can be requested here: http://api.census.gov/data/key_signup.html
census_api_key(Sys.getenv("ACS_API_KEY"))

# Connection to NYCDB SQL database.
con <- dbConnect(
  drv = RPostgres::Postgres(),
  host = Sys.getenv("HDC_NYCDB_HOST"),
  user = Sys.getenv("HDC_NYCDB_USER"),
  password = Sys.getenv("HDC_NYCDB_PASSWORD"),
  port = Sys.getenv("HDC_NYCDB_PORT"),
  dbname = Sys.getenv("HDC_NYCDB_DBNAME")
)


# Get list of tracts to include in the map
# To easily add/remove tracts and get list see: 
# https://popfactfinder.planning.nyc.gov/profile/23604/housing
rtu_tracts <- path("data", "ridgewood-tracts.csv") %>% 
  read_csv(col_types = "c") %>% 
  pull(geoid)

# Get ACS tract-level data, drop MOE columns, clean up column names, and
# calculate extra indicator for renter poverty rate
acs_tracts <- get_acs(
  "tract",
  state = "36",
  county = "081",
  variables = c(
    "pop_num" = "B01001_001", # total population
    "occ_units" = "B25003_001", # total occupied units
    "occ_renter_units" = "B25003_003", # renter occupied units
    "occ_renter_pop" = "B25008_003", # renter population
    "gross_rent_med" = "B25064_001", # median gross rent
    "renter_hh_inc_med" = "B25119_003", # median renter household income
    # renter families below poverty
    "renter_pov_fam5" = "B17019_005", 
    "renter_pov_fam9" = "B17019_009", 
    "renter_pov_fam12" = "B17019_012",
    # renter families above poverty
    "renter_nonpov_fam16" = "B17019_016", 
    "renter_nonpov_fam20" = "B17019_020", 
    "renter_nonpov_fam23" = "B17019_023", 
    # renter households by race/ethnicity of head-of-household
    "renter_white_num" = "B25003H_003",
    "renter_black_num" = "B25003B_003",
    "renter_asian_num" = "B25003D_003",
    "renter_latin_num" = "B25003I_003",
    # Renter households with children
    "renter_child_num18" = "B25115_018",
    "renter_child_num22" = "B25115_022",
    "renter_child_num25" = "B25115_025",
    # rent burden
    "rent_burden_num1" = "B25070_001",
    "rent_burden_num7" = "B25070_007",
    "rent_burden_num8" = "B25070_008",
    "rent_burden_num9" = "B25070_009",
    "rent_burden_num10" = "B25070_010",
    "rent_burden_num11" = "B25070_011"
  ),
  survey = "acs5",
  year = 2018,
  output = "wide",
  geometry = TRUE
) %>% 
  rename(geoid = GEOID) %>% 
  filter(geoid %in% rtu_tracts) %>% 
  select(-NAME, -ends_with("M")) %>% 
  rename_with(~str_remove(., "E$")) %>% 
  mutate(
    rent_burden_denom = na_if(rent_burden_num1, 0) - rent_burden_num11, # Total minus (not computed)
    rent_burden_30p_num = rent_burden_num7+rent_burden_num8+rent_burden_num9+rent_burden_num10,
    rent_burden_30p_pct = rent_burden_30p_num / rent_burden_denom,
    rent_burden_50p_pct = rent_burden_num10 / rent_burden_denom,
    
    renter_fam_pov_pct = (renter_pov_fam5+renter_pov_fam9+renter_pov_fam12)/
      (renter_pov_fam5+renter_pov_fam9+renter_pov_fam12+renter_nonpov_fam16+renter_nonpov_fam20+renter_nonpov_fam23),
    
    renter_pct = occ_renter_units / occ_units,
    
    unit_occ_rent_child_num = renter_child_num18+renter_child_num22+renter_child_num25,
    unit_occ_rent_child_pct = unit_occ_rent_child_num/occ_renter_units,
    
    renter_white_pct = renter_white_num/occ_renter_units,
    renter_black_pct = renter_black_num/occ_renter_units,
    renter_asian_pct = renter_asian_num/occ_renter_units,
    renter_latin_pct = renter_latin_num/occ_renter_units,
    
  ) %>% 
  select(
    geoid,
    occ_units,
    pop_num,
    occ_renter_units,
    occ_renter_pop,
    gross_rent_med,
    renter_hh_inc_med,
    rent_burden_30p_pct,
    rent_burden_50p_pct,
    renter_fam_pov_pct,
    renter_pct,
    unit_occ_rent_child_pct,
    renter_white_pct,
    renter_black_pct,
    renter_asian_pct,
    renter_latin_pct,
  )

# Use glue_sql() to insert our list of tracts into the WHERE statement: 
# "where geoid in ({rtu_tracts*})"

# Reformat tract geoid column in pluto, create a crosswalk between NYC
# boro-block and census tract because most other datasets don't have a tract
# column.

# Then for each data set aggregate by tract to get various indicators relevant
# to displacement risk, and finally join them all together by tract
nycdb_tracts <- dbGetQuery(con, glue_sql("
with pluto_w_geoid as (
  select 
  	borocode::text,
  	lpad(block::text, 5, '0') as block,
  	'36' ||
  	case 
  		when borocode = '1' then '061'
  		when borocode = '2' then '005'
  		when borocode = '3' then '047'
  		when borocode = '4' then '081'
  		when borocode = '5' then '085'
  	end || 
  	rpad(tract2010, 6, '0') geoid 
  from pluto_19v2 
  where borough = 'QN'
), block_tract_xwalk as (
  select distinct on (borocode, block)
  	borocode,
  	block,
  	geoid 
  from pluto_w_geoid 
  where geoid in ({rtu_tracts*})
  group by borocode, block, geoid
), complaints as (
  select
  	geoid,
  	sum((type = 'NON EMERGENCY')::int) as comp_nonemerg_num,
  	sum((type = 'EMERGENCY')::int) as comp_emerg_num,
  	sum((type = 'IMMEDIATE EMERGENCY')::int) as comp_immed_num,
  	sum((type = 'HAZARDOUS')::int) as comp_haz_num
  from hpd_complaints as c
  left join hpd_complaint_problems as p
  	using(complaintid)
  inner join block_tract_xwalk as x
  	on x.borocode::int = c.boroughid::int 
  	  and x.block::int = c.block::int
  where c.receiveddate >= '2019-01-01'
  group by geoid
), violations as (
  select
  	geoid,
  	sum((class = 'A')::int) as viol_a_num,
  	sum((class = 'B')::int) as viol_b_num,
  	sum((class = 'C')::int) as viol_c_num
  from hpd_violations as v
  inner join block_tract_xwalk as x
  	on x.borocode::int = v.boroid::int 
  	  and x.block::int = v.block::int
  where v.inspectiondate >= '2019-01-01'
  group by geoid
), evictions as (
  select
  	geoid,
  	count(*) as marsh_evic_num
  from marshal_evictions_19 as e
  inner join block_tract_xwalk as x
  	on (x.borocode || x.block) = substr(e.bbl, 1, 6)
  where residentialcommercialind = 'RESIDENTIAL'
  group by geoid
), deregulation as (
  select 
    geoid,
  	sum(unitsstab2007 - uc2018)::numeric as dereg_units,
  	sum(unitsstab2007 - uc2018)::numeric/sum(unitsstab2007)::numeric as dereg_pct
  from rentstab_summary as r1
  left join rentstab_v2 as r2
  	using(ucbbl)
  inner join block_tract_xwalk as x
    	on (x.borocode || x.block) = substr(r1.ucbbl, 1, 6)
  where unitsstab2007 > 0
  	and uc2018 <= unitsstab2007 -- don't want to include gains in RS units
  group by geoid
)
select *
from complaints 
full join violations using(geoid)
full join evictions using(geoid)
full join deregulation using(geoid)
", .con = con))

# Join our ACS tract data (with geometries) with the NYCDB indicators and where
# appropriate calculate versions of the indicators as a rate per 100 rental
# units. For example, code violations per 100 rental units. Tracts are all
# different sizes and the renter/owner composition differs, and our indicators
# are only relevant to rental units we need to use those to normalize.

# There are no NYCHA properties in the area so we don't have to worry about
# removing those from the denominator where they aren't relevant, like HPD
# complaints since NYCHA isn't included in that system.
ridgewood_tracts_all <- acs_tracts %>% 
  left_join(nycdb_tracts, by = "geoid") %>% 
  mutate(across(ends_with("_num"), ~ . / occ_renter_units*100, .names = "{col}_rt")) %>% 
  mutate(across(-c(geoid, geometry), as.numeric)) %>%
  rename_with(~str_replace(., "_num_rt", "_rt"))

# Export the data for use in Shiny app
ridgewood_tracts_all %>% 
  st_transform(4326) %>% 
  write_rds(path("app", "data", "ridgewood-tracts-indicators.rds"))

# NOTE: In the app data directory there is a manually created csv file of
# indicator names and formatting instructions for use in the app, so if new
# indicators are added here you need to update that file as well
