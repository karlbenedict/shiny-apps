library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)

# fields for captured user input
snafu_fields <- c("documentation",
	"org_structure",
	"org_naming",
	"formats",
	"prod_reports",
	"prod_publications",
	"prod_data",
	"other")

# field labels
snafu_field_labels <- c(
  "Documentation",
  "Organization - File Structure",
  "Organization - File Naming",
  "File Formats",
  "Products - Reports",
  "Products - Publications",
  "Products - Data/Metadata (documentation)",
  "Other")

outputDir <- "../../../data/surveys/snafu_issues"

saveData <- function(data) {
	data <- t(data)
	fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
	write.csv(
    	x = data,
    	file = file.path(outputDir, fileName), 
    	row.names = FALSE, 
		quote = TRUE
	)
}
	
loadData <- function() {
  files <- list.files(outputDir, full.names = TRUE)
  if (length(files) > 0){
  	data <- lapply(files, read.csv, stringsAsFactors = FALSE) 
  	# Concatenate all data together into one data.frame
  	data <- do.call(rbind, data)
  	data
  } else {
	data <- tibble(
      documentation = NA,
      org_structure = NA,
      org_naming = NA,
      formats = NA,
      prod_reports = NA,
      prod_publications = NA,
      prod_data = NA,
      other = NA
    )
	data
  }
}
	
shinyApp(
	ui = fluidPage(
	  titlePanel("Role of Different Issues in the SNAFU Video"),
		tags$hr(),
		    radioButtons("documentation", "Documentation", c("No Response"=NA, "No Role"=1, "Some Role" =2, "Significant Role"=3), inline=TRUE),
		    radioButtons("org_structure", "Organization - File Structure",  c("No Response"=NA, "No Role"=1, "Some Role" =2, "Significant Role"=3), inline=TRUE),
		    radioButtons("org_naming", "Organization - File Naming", c("No Response"=NA, "No Role"=1, "Some Role" =2, "Significant Role"=3), inline=TRUE),
		    radioButtons("formats", "File Formats", c("No Response"=NA, "No Role"=1, "Some Role" =2, "Significant Role"=3), inline=TRUE),
		    radioButtons("understanding", "Understanding the Data",  c("No Response"=NA, "No Role"=1, "Some Role" =2, "Significant Role"=3), inline=TRUE),
		    radioButtons("prod_reports", "Products - Reports", c("No Response"=NA, "No Role"=1, "Some Role" =2, "Significant Role"=3), inline=TRUE),
		    radioButtons("prod_publications", "Products - Publications",  c("No Response"=NA, "No Role"=1, "Some Role" =2, "Significant Role"=3), inline=TRUE),
		    radioButtons("prod_data", "Products - Data/Metadata (documentation)", c("No Response"=NA, "No Role"=1, "Some Role" =2, "Significant Role"=3), inline=TRUE),
		    radioButtons("other", "Other", c("No Response"=NA, "No Role"=1, "Some Role" =2, "Significant Role"=3), inline=TRUE),
		    actionButton("submit","Submit"),
		#tags$hr(),
		#plotOutput("meanScores_plot", inline = TRUE)
	),
	
	server = function(input, output, session) {
		
		formData <- reactive({
			data <- sapply(snafu_fields, function(x) input[[x]])
			data
		})
		
		observeEvent(input$submit, {
			saveData(formData())
		})
				
		output$meanScores_plot <- renderPlot({
		  input$submit
		  if (nrow(loadData()) == 0) {
		    ggplot() + annotate("text", x=1, y=1, size=8, label="Please submit a survey response...") + theme_void()
		  } else {
  		  summaries <- loadData() %>% 
  		    pivot_longer(cols=all_of(snafu_fields)) %>% 
  		    group_by(name) %>% 
  		    summarize(Average=mean(as.numeric(value), na.rm=TRUE))
  		  summaries$name <- factor(summaries$name, levels=rev(snafu_fields))
  		  ggplot(summaries, aes(x=name, y=Average)) + 
  		    geom_point(stat="identity", size = 5) +
  		    coord_flip()+
  		    scale_y_discrete(labels = c("Low", "Medium", "High")) +
  		    scale_x_discrete(labels = rev(snafu_field_labels)) +
  		    xlab("Data Management Issue Areas") +
  		    ylab("Representation in Video (Low --> High)") +
  		    #ylim(1,3) +
  		    theme_light() +
			theme(text = element_text(size=15))
		  }
		},
		  width = 600,
		  height = 250
		)
	}
)