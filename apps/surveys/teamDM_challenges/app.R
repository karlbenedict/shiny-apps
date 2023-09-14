library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)

# fields for captured user input
fields <- c("obtaining",
	"storage",
	"finding",
	"sharing_internal",
	"understanding",
	"using",
	"documenting",
	"reporting",
	"preserving",
	"sharing_external")

# field labels
field_labels <- c(
  "Obtaining/\nCollecting",
  "Storing",
  "Finding\nin Storage",
  "Sharing with\nTeammates",
  "Understanding",
  "Using",
  "Documenting",
  "Reporting",
  "Preserving",
  "Sharing Outside\nof Team"
  )

outputDir <- "../../../data/surveys/teamDM_challenges"

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
      obtaining = NA,
      storage = NA,
      finding = NA,
      sharing_internal = NA,
      understanding = NA,
      using = NA,
      documenting = NA,
      reporting = NA,
      preserving = NA,
      sharing_external = NA
    )
	data
  }
}
	
shinyApp(
	ui = fluidPage(
	  titlePanel("Rate Team Research Activity Difficulty"),
		tags$hr(),
#		sidebarLayout(
#		  sidebarPanel(
		    radioButtons("obtaining", "Obtaining/Collecting Data", c("No Response"=NA, "Very Easy"=1, "Easy" =2, "neutral"=3, "Difficult"=4, "Very Difficult"=5), inline=TRUE),
		    radioButtons("storage", "Data Storage",  c("No Response"=NA, "Very Easy"=1, "Easy" =2, "neutral"=3, "Difficult"=4, "Very Difficult"=5), inline=TRUE),
		    radioButtons("finding", "Finding Project Data", c("No Response"=NA, "Very Easy"=1, "Easy" =2, "neutral"=3, "Difficult"=4, "Very Difficult"=5), inline=TRUE),
		    radioButtons("sharing_internal", "Data Sharing Within Team", c("No Response"=NA, "Very Easy"=1, "Easy" =2, "neutral"=3, "Difficult"=4, "Very Difficult"=5), inline=TRUE),
		    radioButtons("understanding", "Understanding the Data",  c("No Response"=NA, "Very Easy"=1, "Easy" =2, "neutral"=3, "Difficult"=4, "Very Difficult"=5), inline=TRUE),
		    radioButtons("using", "Using Data", c("No Response"=NA, "Very Easy"=1, "Easy" =2, "neutral"=3, "Difficult"=4, "Very Difficult"=5), inline=TRUE),
		    radioButtons("documenting", "Documenting Data",  c("No Response"=NA, "Very Easy"=1, "Easy" =2, "neutral"=3, "Difficult"=4, "Very Difficult"=5), inline=TRUE),
		    radioButtons("reporting", "Reporting Data Analysis Results", c("No Response"=NA, "Very Easy"=1, "Easy" =2, "neutral"=3, "Difficult"=4, "Very Difficult"=5), inline=TRUE),
		    radioButtons("preserving", "Preserving Data", c("No Response"=NA, "Very Easy"=1, "Easy" =2, "neutral"=3, "Difficult"=4, "Very Difficult"=5), inline=TRUE),
		    radioButtons("sharing_external", "Data Sharing Within Team", c("No Response"=NA, "Very Easy"=1, "Easy" =2, "neutral"=3, "Difficult"=4, "Very Difficult"=5), inline=TRUE),
		    actionButton("submit","Submit")
#		  ),
#		  mainPanel(
#		    plotOutput("meanScores_plot", inline = TRUE),
#			p(),
#		    plotOutput("meanScores_spider", inline = TRUE),
#		    tags$hr(),
#		    p("Summary of Responses"),
#		    tableOutput("meanScores"),
#		    tags$hr(),
#		    p("Individual Responses"),
#		    tableOutput("inputData")
#		  )
#		)
	),
	
	server = function(input, output, session) {
		
		formData <- reactive({
			data <- sapply(fields, function(x) input[[x]])
			data
		})
		
		observeEvent(input$submit, {
			saveData(formData())
		})
		
		#output$inputData <- renderTable({
		#  input$submit
		#	loadData()
		#})
		
		output$meanScores_plot <- renderPlot({
		  input$submit
		  if (nrow(loadData()) == 0) {
		    ggplot() + annotate("text", x=1, y=1, size=8, label="Please submit a survey response...") + theme_void()
		  } else {
  		  summaries <- loadData() %>% 
  		    pivot_longer(cols=all_of(fields)) %>% 
  		    group_by(name) %>% 
  		    summarize(Average=mean(as.numeric(value), na.rm=TRUE))
  		  summaries$name <- factor(summaries$name, levels=rev(fields))
  		  ggplot(summaries, aes(x=name, y=Average)) + 
  		    geom_point(stat="identity", size = 5) +
  		    coord_flip()+
  		    #scale_y_continuous(labels = NULL) +
  		    scale_x_discrete(labels = rev(field_labels)) +
  		    xlab("Challenge Areas") +
  		    ylab("Average Degree of Difficulty (Very Easy --> Very Difficult)") +
  		    ylim(1,5) +
  		    theme_light()
		  }
		},
		  width = 500,
		  height = 300
		)
		
		output$meanScores_spider <- renderPlot({
		  input$submit
		  if (nrow(loadData()) == 0) {
		    print("empty data")
		    ggplot() + annotate("text", x=1, y=1, size=8, label="Please submit a survey response...") + theme_void()
		  } else {
		    print("non-empty data")
  		  summaries <- loadData() %>% 
  		  pivot_longer(cols=all_of(fields)) %>% 
  		  group_by(name) %>% 
  		  summarize(Average=mean(as.numeric(value), na.rm=TRUE))
  		  summaries$name <- factor(summaries$name, levels=fields)
  		  ggplot(summaries, aes(x=name, y=Average)) + 
  		    geom_col() +
  		    coord_polar()+
  		    scale_y_continuous(labels = NULL) +
  		    scale_x_discrete(labels = field_labels) +
  		    xlab(NULL) +
  		    ylab(NULL) +
  		    #ylim(1,5) +
  		    theme_light() +
  		    theme(panel.border = element_rect(
  		            linetype = 0
  		          ))
		  }
		},
		  width = 300,
		  height = 300
		)
		
		
		#output$meanScores <- renderTable({
		#  input$submit
		#  summaries <- loadData() %>% 
		#    pivot_longer(cols=all_of(fields)) %>% 
		#    group_by(name) %>% 
		#    summarize(Average=mean(as.numeric(value), na.rm=FALSE)) %>%
		#	arrange(factor(name, levels=fields))
		#  #summaries$name <- factor(summaries$name, levels=rev(fields))
		#  #summaries <- summaries %>% 
		#  #  arrange(name)
		#  summaries
		#})
		
	}
)