library(dotenv)
library(sodium)
library(fs)

# Edit ".env_sample" to set variables and save as ".env"
load_dot_env(".env")

# Create a user base then hash passwords with {sodium} then save to an rds file
# in app directory. These are then used by {shinyauthr} to created a login
# screen in front of the app.

user_base <- data.frame(
  user = "rtu",
  password = sodium::password_store(Sys.getenv("ADMIN_PASSWORD")),
  permissions = "admin",
  name = "RTU Admin",
  stringsAsFactors = FALSE,
  row.names = NULL
)

saveRDS(user_base, "app/data/user_base.rds")

# Save Mapbox token for shiny app 
# Free to sign up for API key here: https://account.mapbox.com/auth/signup/
saveRDS(Sys.getenv("MAPBOX_TOKEN"), path("app", "data", "mapbox_token.rds"))