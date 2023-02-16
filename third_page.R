# third page items

accessible_projects <- function(data)
  data[data$project_name %in% user_projects(),]

# explain what the tables_in_schemas table is doing
output$t_in_s_explanation <- renderUI({
  mainPanel(
    tags$p(paste("This table has information about which projects have which",
                 "common subtables. In other words, it looks up the list of",
                 "all schemas in the database, filters out those that aren't",
                 "for specific projects (e.g. the schema with a table that",
                 "lists all schemas) or which have no data, then collects",
                 "data on all of the names for tables inside those schemas.",
                 "Table names which are unique to one schema are thrown out.",
                 "Then, this table is constructed, with project names for",
                 "rows and table names for columns. Not all of these tables",
                 "are used by the Shiny App. Below is how two are used:")),
    tags$ul(
      tags$li(tags$code(".descriptions"), 
              paste(" is used on the 'Interactive Phenotypes' page to pull",
                    "information about what each phenotype is as well as",
                    "the list of available traits to graph.")),
      tags$li(tags$code(".gwas_phenotypes"), 
              paste(" is used on the 'Interactive Phenotypes' page to pull",
                    "information about measured/known values for plotting",
                    "phenotypes/traits on graphs."))
    )
  )
})

# output the tables_in_schemas data frame
output$tables_in_schemas <- DT::renderDataTable({
  DT::datatable(
    accessible_projects(tables_in_schemas(input$show_empty)),
    extensions = c("Buttons","FixedColumns"),
    rownames = F, options = table_options(F, "tables_in_schemas")
  ) %>% color_tf()
})

# explain what the projects_in_tables table is doing
output$p_in_t_explanation <- renderUI({
  mainPanel(
    tags$p("This table has information about which projects are in which ",
           tags$code("sample_tracking"),
           paste(" tables. This schema tracks progress for all projects.",
                 "Below is how these tables are used:")),
    tags$ul(
      tags$li(tags$code(".progress_checklist"), 
              paste(" is used on the 'Progress Log and Tables' page as the",
                    "third/lower table displayed under 'Progress Tables'.")),
      tags$li(tags$code(".db_progress"), "and", tags$code("db_complete"),
              paste(" are used on the 'Progress Log and Tables' page as the",
                    "first and second tables, respectively, and their union",
                    "is the source of data for the bar charts under the page",
                    "'Progress Barplots'.")),
      tags$li(tags$code(".project_meta"), 
              paste(" is used on the 'Interactive Phenotypes' page to pull",
                    "meta-information about the selected project, such as",
                    "who the PI is."))
    )
  )
})

# output the projects_in_tables data frame
output$projects_in_tables <- DT::renderDataTable({ 
  DT::datatable(
    accessible_projects(projects_in_tables), rownames = F, 
    extensions = c("Buttons","FixedColumns"),
    options = table_options(F, "projects_in_tables")
  ) %>% color_tf()
})
