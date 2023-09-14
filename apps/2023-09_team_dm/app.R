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
  "Obtain/\nCollect",
  "Store",
  "Find",
  "Share with\nTeammates",
  "Understand",
  "Use",
  "Document",
  "Report",
  "Preserve",
  "Share Outside\nof Team"
  )
  
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


values <- reactiveValues()
values$responses = 0
values$snafuResponses = 0
outputDir <- "../../data/surveys/teamDM_challenges"
snafuOutputDir <- "../../data/surveys/snafu_issues"

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

snafuLoadData <- function() {
  files <- list.files(snafuOutputDir, full.names = TRUE)
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
	


ui = fluidPage(
	tags$head(
		tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
	),

	tabsetPanel(
		id = 'slides',
		
		tabPanel("01",
			class = 'slide title_slide',
			htmlTemplate("01_title.html", name = "titlePage")
		),
		
		tabPanel("02",
			class = 'slide body_slide text_graphic', 
			htmlTemplate("02_overview.html", name = "overview")
		),
		
		tabPanel("03",
			class = 'slide body_slide graphic_graphic',
			htmlTemplate("03_concepts_lifecycles01.html", name = "concepts_lifecycle01")
		),
		
		tabPanel("04",
			class = 'slide body_slide text_graphic',
			htmlTemplate("04_concepts_lifecycles02.html", name = 'concepts_lifecycle02')	
		),
		
		tabPanel("05",
			class = "slide body_slide graphic_graphic",
			tags$h2("Challenges You Foresee / Have Experienced"),
			tags$div(
				plotOutput("meanScores_spider", inline = TRUE),
				tags$div(
					tags$p(
						class="large_link",
						tags$img(src="teamDM_challenges.png"),
						tags$br(),
						tags$a(href="http://shiny.kbene.com:3838/apps/surveys/teamDM_challenges/", "http://shiny.kbene.com:3838/apps/surveys/teamDM_challenges/", target="_blank"
						)
					)
				)
				
			)
		),
		
		tabPanel("06", 
			class = 'slide body_slide iframe_media',
			htmlTemplate("06_snafu.html", name = 'snafu')	
		),
		
		tabPanel("07", 
			class = 'slide body_slide text_graphic',
			tags$h2("Issues Highlighted in the Video /", 
				tags$br(),
				"Actions Throughout the Lifecycle(s)"),
			tags$div(
				style = "width:900px;float:right",
				plotOutput("meanSNAFUScores_plot"),
				tags$div(
					style="float:none;width:600;margin-left:auto;margin-right:auto",
					tags$img(src="snafu_issues.png", style= "display:block;")
					
				),
			),
			tags$div(class="text_content_half",
				tags$ul(
					tags$li("Documentation"),
					tags$li("Organization of Files"),
						tags$ul(
							tags$li("Directory Structure"),
							tags$li("File Naming")
						),
					tags$li("File Formats"),
					tags$li("Research Products"),
						tags$ul(
							tags$li("Reports"),
							tags$li("Publications"),
							tags$li("Datasets/Metadata (Documentation)")
						)
				),
				tags$p(tags$a(href="http://shiny.kbene.com:3838/apps/surveys/snafu_issues/", "http://shiny.kbene.com:3838/apps/surveys/snafu_issues/", target="_blank"))
			)
		),
		
		tabPanel("08",
			class = 'slide body_slide text_graphic',
			htmlTemplate("08_documentation.html", name = 'documentation')	
		),
	
		tabPanel("09", 
			class = 'slide body_slide text_graphic',
			htmlTemplate("09_organization_fileStructure.html", name = 'organization_fileStructure')	
		),
		
		tabPanel("10", 
			class = 'slide body_slide text_graphic',
			htmlTemplate("10_organization_fileStructure.html", name = 'organization_fileStructure')	
		),
		
		tabPanel("11", 
			class = 'slide body_slide text_graphic',
			htmlTemplate("11_fileFormats.html", name = 'fileFormats')	
		),
		
		tabPanel("12", 
			class = 'slide body_slide text_graphic',
			htmlTemplate("12_productsReportsPublications.html", name = 'productsReportsPublications')	
		),
		
		tabPanel("13", 
			class = 'slide body_slide text_graphic',
			htmlTemplate("13_productsDatasetsMetadata.html", name = 'productsDatasetsMetadata')	
		),
		
		tabPanel("14",
			class = 'slide body_slide text_graphic',
			htmlTemplate("14_tools.html", name = 'tools')	
		),
		
		tabPanel("15", 
			class = 'slide body_slide text_graphic',
			htmlTemplate("15_nextSteps.html", name = 'nextSteps')	
		),

	)
)
	
	
server = function(input, output, session) {
	data <- reactivePoll(1000,
		session,
		checkFunc = function () {
			files <- list.files(outputDir, full.names = TRUE)
			length(files)
		},
		valueFunc = function () {
			files <- list.files(outputDir, full.names = TRUE)
			values$responses <- length(files)
			loadData()
		}
	)

	snafuData <- reactivePoll(1000,
		session,
		checkFunc = function () {
			files <- list.files(snafuOutputDir, full.names = TRUE)
			length(files)
		},
		valueFunc = function () {
			files <- list.files(snafuOutputDir, full.names = TRUE)
			values$snafuResponses <- length(files)
			snafuLoadData()
		}
	)
	
	output$meanScores_spider <- renderPlot({
	  if (nrow(data()) == 0) {
		print("empty data")
		ggplot() + annotate("text", x=1, y=1, size=8, label="Please submit a survey response...") + theme_void()
	  } else {
		print("non-empty data")
		summaries <- data() %>% 
		pivot_longer(cols=all_of(fields)) %>% 
		group_by(name) %>% 
		summarize(Average=mean(as.numeric(value), na.rm=TRUE))
		summaries$name <- factor(summaries$name, levels=fields)
		ggplot(summaries, aes(x=name, y=Average)) + 
		  geom_col(fill="lightgray") +
		  geom_hline(yintercept = 3, color = "blue") +
		  geom_hline(yintercept = 5, color = "red") +
		  geom_hline(yintercept = 1, color = "green") +
		  annotate("text", x = 0, y = 0, label = values$responses, size = 20) +
		  coord_polar()+
		  scale_y_continuous(labels = NULL) +
		  scale_x_discrete(labels = field_labels) +
		  xlab(NULL) +
		  ylab(NULL) +
		  theme_light() +
		  theme(
		  	panel.border = element_rect(linetype = 0),
			text = element_text(size=30),
			panel.background = element_blank(),
			panel.ontop = TRUE
		  )
	  }
	},
	  width = 900,
	  height = 900
	)	
	
	output$meanSNAFUScores_plot <- renderPlot({
		  if (nrow(snafuData()) == 0) {
			ggplot() + annotate("text", x=1, y=1, size=8, label="Please submit a survey response...") + theme_void()
		  } else {
			summaries <- snafuData() %>% 
			  pivot_longer(cols=all_of(snafu_fields)) %>% 
			  group_by(name) %>% 
			  summarize(Average=mean(as.numeric(value), na.rm=TRUE))
			summaries$name <- factor(summaries$name, levels=rev(snafu_fields))
			ggplot(summaries, aes(x=name, y=Average)) + 
			  geom_point(stat="identity", size = 5) +
		  	  annotate("text", x = 1, y = 1, label = values$snafuResponses, size = 10) +
			  coord_flip()+
			  #scale_y_discrete(labels = c("Low", "Medium", "High")) +
			  scale_x_discrete(labels = rev(snafu_field_labels)) +
			  ggtitle("Data Management Issue Areas") +
			  ylab("Representation in Video (Low --> High)") +
			  ylim(1,3) +
			  theme_light() +
			  theme(text = element_text(size=24),
			  	axis.title.y = element_blank(),
				axis.text.x = element_blank())
		  }
		},
		  width = 900,
		  height = 350
		)

}

shinyApp(ui, server)