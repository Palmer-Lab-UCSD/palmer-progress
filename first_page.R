# default page items

# master dataframes ----

# these will hold the active versions of the progress tables
progress_tables <- reactiveValues(individual = individual_progress,
                                  active = project_progress_active,
                                  complete = project_progress_complete)

# update the active versions of the datatables when project selections change
observeEvent(input$progress_projects, {
  filter_table <- function(x) x[x$project_name %in% input$progress_projects,]
  
  progress_tables$individual <- filter_table(individual_progress)
  progress_tables$active <- filter_table(project_progress_active)
  progress_tables$complete <- filter_table(project_progress_complete)
})

# first tab items ----

# generate a list of projects user has access to, by default all selected
output$projects_selector <- renderUI({
  allowed <- sample_meta[sample_meta$animal_type %in% input$animal_types,
                         ]$project_name
  allowed <- allowed[allowed %in% user_projects()]
  pickerInput("progress_projects", "Select project(s):", 
              choices = allowed, selected = allowed, 
              options = list(`actions-box` = TRUE), multiple = T)
})

# generate a type-of-animal filter
output$animal_selector <- renderUI({
  allowed <- unique(sample_meta[sample_meta$project_name %in% user_projects(),
                                ]$animal_type)
  pickerInput("animal_types", "Select animal type(s) to filter projects by:", 
              choices = allowed, selected = allowed, 
              options = list(`actions-box` = TRUE), multiple = T)
})

# progress tables, split up by active and complete projects

output$active_caption <- renderText(project_caption("ACTIVE"))

output$by_project_active <- DT::renderDataTable({ 
  DT::datatable(
    process_progress_df(progress_tables$active), rownames = F, 
    extensions = c("Buttons", "FixedColumns"),
    options = table_options(F, "active_progress")
  ) %>% bold_asterisk()
})

output$complete_caption <- renderText(project_caption("COMPLETE"))

output$by_project_complete <- DT::renderDataTable({ 
  DT::datatable(
    process_progress_df(progress_tables$complete), rownames = F, 
    extensions = c("Buttons","FixedColumns"),
    options = table_options(F, "complete_progress")
    ) %>% bold_asterisk()
})

output$animal_caption <- renderText("Animals' progress through a project")

# low-level stats for each animal
output$by_animal <- DT::renderDataTable({ 
  DT::datatable(
    progress_tables$individual, rownames = F, 
    filter = list(position = "top", clear = F), 
    extensions = c("Buttons","FixedColumns"), 
    options = table_options(T, "individual_progress")
    )
})

# second tab items ----

# output a barplot with relative progress for each project
output$progress_bars <- renderPlot({ 
  # calculate what data to plot and then feed it into plotting function
  progress_bars(rbind(progress_tables$active, progress_tables$complete))
})

# download with custom file name and image size
output$download <- downloadHandler(
  filename = function() paste0(input$out_file, ".png"),
  content = function(file)
    ggsave(file, plot = progress_bars(), device = "png",
           width = input$width, height = input$height, units = "px")
)
