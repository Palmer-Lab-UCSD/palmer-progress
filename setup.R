# application set-up

set_labels(
  language = "en",
  "Please authenticate" =
    "Welcome to the Palmer Lab Shiny app! Please log in below.",
  "Username:" = "Enter your username:", "Password:" = "Enter your password:"
)

# basically authenticates the login, matches up username and password
res_auth <- secure_server(check_credentials = check_credentials(credentials),
                          timeout = 480)

# setup clean disconnect when the app closes
session$onSessionEnded(function() { dbDisconnect(get_con()) })

# remove any projects lacking either .gwas_phenotypes or .descriptions
projects <- projects[sapply(projects, table_exists, "gwas_phenotypes") |
                       sapply(projects, table_exists, "descriptions")]

# non-admin users may only access their projects
user_projects <- reactive({
  if(res_auth$user != "PalmerAdmin")
    projects[startsWith(projects, res_auth$user)]
  else projects
})
