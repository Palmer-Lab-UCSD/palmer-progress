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

# establish database link ----

# uses the list generated from JSON file to enter in credentials
con <- dbConnect(RPostgres::Postgres(), dbname = AMA_CREDS$database,
            host = AMA_CREDS$host , port = 5432, user = AMA_CREDS$user, 
            password = AMA_CREDS$password) 

# name the application being connected to database
dbSendQuery(con, "set application_name = 'Palmer Shiny App'")

# load tables ----

# credentials to log in to the app, not database
credentials <- dbGetQuery(con, "SELECT * FROM sample_tracking.credentials")

# progress tables
individual_progress <- 
  dbGetQuery(con, "SELECT * FROM sample_tracking.progress_checklist")
project_progress_active <- 
  dbGetQuery(con, "SELECT * FROM sample_tracking.db_progress")
project_progress_complete <-
  dbGetQuery(con, "SELECT * FROM sample_tracking.db_complete")
# project metadata
project_meta <- 
  dbGetQuery(con, "SELECT * FROM sample_tracking.project_metadata")
sample_meta <-
  dbGetQuery(con, "SELECT * FROM sample_tracking.sample_metadata")

# prepare meta-info for later use ----

catalog <- dbGetQuery(con, "SELECT * FROM pg_catalog.pg_tables")
# remove the ones that aren't for a specific project
catalog <- catalog[!(catalog$schemaname %in% NON_PROJECT_SCHEMAS),]
# group tables by their schema
tables_by_schema <- aggregate(tablename ~ schemaname, catalog, unique)

# all the different project names in the data
projects <- sort(unique(c(catalog$schemaname, individual_progress$project_name, 
                          project_progress_complete$project_name,
                          project_meta$project_name, sample_meta$project_name)),
                 decreasing = TRUE)

# turn sample meta table into one that matches project names to organism/strain
sample_meta <- aggregate(cbind(organism,strain) ~ project_name, 
                         sample_meta, unique)
# deal with projects missing in sample_metadata
sample_meta <- rbind(sample_meta, data.frame(
  project_name = projects[!(projects %in% sample_meta$project_name)],
  organism = "missing", strain = "missing"))
sample_meta$animal_type <- paste(sample_meta$organism, "-", sample_meta$strain)

# helper functions for using meta-info ----

# helper function to check if a certain table exists for a certain project
table_exists <- function(project, table) {
  # look up the schema for this project and check through its table catalog
  project %in% tables_by_schema$schemaname &&
    table %in% tables_by_schema[tables_by_schema$schemaname == project,2][[1]]
}

# remove any projects lacking either .gwas_phenotypes or .descriptions
projects <- projects[sapply(projects, table_exists, "gwas_phenotypes") |
                       sapply(projects, table_exists, "descriptions")]

# helper function to check if a data table exists before actually loading it
safely_load_df <- function(project, table, msg) {
  # display an understandable message if the data table is not found
  validate(need(table_exists(project, table), 
                paste0(msg, " (.", table, " table missing)")))
  # grab table if it exists
  dbGetQuery(con, paste("select * from ", project, ".", table))
}

# table visualizations of said meta-info ----

# make a table of which projects are in which tables

# table to store information about whether other tables have each project
projects_in_tables <- data.frame(project_name = projects)

# manually fill in information about the projects
projects_in_tables$progress_checklist <- 
  sapply(projects_in_tables$project_name,
         function(x) x %in% individual_progress$project_name)
projects_in_tables$db_progress <- 
  sapply(projects_in_tables$project_name, 
         function(x) x %in% project_progress_active$project_name)
projects_in_tables$db_complete <- sapply(
  projects_in_tables$project_name, 
  function(x) x %in% project_progress_complete$project_name)
projects_in_tables$progress_metadata <- 
  sapply(projects_in_tables$project_name, 
         function(x) x %in% project_meta$project_name)

# strings for Javascript interpretation
projects_in_tables[] <- lapply(projects_in_tables, as.character)

# make a table of which schemas have which tables
tables_in_schemas <- function(show_empty) {
  # determine tables which have more than one occurrence in project schemas
  common_tables <- table(catalog$tablename)
  common_tables <- common_tables[common_tables > 1]
  
  # determine whether each project has the common tables or not
  has_table <- data.frame(project_name = projects)
  # for each common table name, add a column to global has_table data frame
  sapply(names(common_tables), function(table) has_table[[table]] <<- 
           # for each project_name (row), check if table_name is in its schema
           sapply(has_table$project_name,
                  function(project) {
                    ifelse(table_exists(project, table), 
                           # query database to check if tables empty if asked
                           ifelse(show_empty && 
                                    nrow(dbGetQuery(con, 
                                                    paste0("select * from ", 
                                                           project, ".", table))
                                    ) == 0, 
                                  "EMPTY", "TRUE"), "FALSE")
                  })
  )
  
  has_table
}

# setup clean disconnect when the app closes
session$onSessionEnded(function() { dbDisconnect(con) })

# non-admin users may only access their projects
user_projects <- reactive({
  if(res_auth$user != "PalmerAdmin")
    projects[startsWith(projects, res_auth$user)]
  else projects
})
