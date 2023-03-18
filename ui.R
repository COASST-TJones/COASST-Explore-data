##### Need
library(shinyjs)
library(shinyalert)
library(leaflet)
library(DT)
library(sp)
library(plotly)
library(shiny)

shinyUI(
	tagList(useShinyjs(),
		tags$head(
			tags$style(HTML("@import url('https://fonts.googleapis.com/css?family=Raleway&display=swap');
			input[type=\"number\"] {
				max-width: 80%;
			}
			div.outer {
				position: fixed;
				top: 42px;
				left: 0;
				right: 0;
				bottom: 0;
				overflow: hidden;
				padding: 0;
			}
			/* Customize fonts */
			body { 
				font-family: 'Raleway', sans-serif;
				font-weight: 250;
				font-size: 15px;
			} 
			p {
				margin: 0px;
				padding: 0px;
			}
			label { 
				font-family: 'Raleway', sans-serif;
				font-weight: 200;
				font-size: 16px;
			} 
			input { 
				font-family: 'Raleway', sans-serif;
				font-weight: 200;
				font-size: 16px;
			}
			navbar-header {
				font-family: 'Raleway', sans-serif;
				font-weight: 250;
				font-size: 36px;
			}
			button { 
				font-family: 'Raleway', sans-serif;
				font-weight: 250;
				font-size: 22px;
			} 
			select { 
				font-family: 'Raleway', sans-serif;
				font-weight: 200;
				font-size: 16px;
			}
			h1 { 
				font-family: 'Raleway', sans-serif;
				font-weight: 400; 
				font-size: 32px;
				} 
			h2 { 
				font-family: 'Raleway', sans-serif;
				font-weight: 300;
				font-size: 20px;						
				} 
			h3 { 
				font-family: 'Raleway', sans-serif;
				font-weight: 250;
				font-size: 18px;						
				} 
			h4 { 
				font-family: 'Raleway', sans-serif;
				font-weight: 250; 
				font-size: 16px;
				margin: 0px 0px 0px 0px;
				padding: 8px 0px 6px 0px;
				}
			.hovertext text {
				font-size: 14px !important;
				font-family: 'Raleway', sans-serif;
			}
			#controls {
				/* Appearance */
				background-color: white;
				padding: 10px 20px 20px 20px;
				cursor: move;
				/* Fade out while not hovering */
				opacity: 0.95;
				zoom: 0.9;
				transition: opacity 500ms 0.2s;
			}
			#controls:hover {
				/* Fade in while hovering */
				opacity: 0.95;
				transition-delay: 0;
			}
			/* Position and style citation */
			#cite {
				position: absolute;
				bottom: 1px;
				left: 1px;
				font-size: 1px;
			}
			/* If not using map tiles, show a white background */
			.leaflet-container {
				background-color: white !important;
			}
			.navbar {
				background-color: white;
			}
			.shiny-output-error {
				visibility: hidden;
			}
			.shiny-output-error:before {
				visibility: hidden;
			}
			"))
		), useShinyalert(),
		navbarPage(id = "nav", title=div(tags$a(img(src='COASST_logo.png',style="margin-top: -10px; padding-right:10px;padding-bottom:15px;padding-left:10px", height = 55), href="https://coasst.org/")), 	
			position="fixed-top",collapsible=TRUE, windowTitle="COASST: Explore data",
			################################################
			tabPanel("Welcome",
				# Draw map and other elements
				h2("Welcome to COASSTs explore data App!"),
				sidebarLayout(      
					sidebarPanel(
						helpText("This App allows you to view/explore different representations of COASSTs data."),
						helpText("At the top of the screen you should see four navigation tabs;", strong("Welcome, Graphs Explained, Data Visuals, and Tables"), "that can be used to navigate among the interactive visuals and tables we have available"),
						helpText("Drop-down menus and other interactive controls are located in the grey box on the left-hand side of each page, allowing you to select how/what data is viewed."),
						h3("COASST regions"),
						helpText("The map to the right shows the COASST regions. Colored polygons on the map show the extent of each COASST region. By clicking on a polygon, summary information for that region will be displayed in the table attached to the map."),
						actionLink("help1", label="More Info",icon=icon("info-circle"), style="color: #1faad7; font-size: 14px"),
						width=3),
					mainPanel(
						selectInput("Map_L48_AK", h4("View Lower-48 (CA-WA) OR Alaska:"), c("Lower-48", "Alaska"), selected="Lower-48", width='80%', selectize=FALSE),
						leafletOutput("map", width="92%", height='600px'),
						absolutePanel(id = "minitabcontrols", class = "panel panel-default", fixed = FALSE,
							draggable = FALSE, top = "90px", left = "20px", right = "auto", bottom = "auto",
							width = "42%", height = "auto",
							h4(strong("Region Summary"), align = "center"),
							p("Click colored polygons to show region info", align="center"),
							div(actionLink("toggleMiniTable", label="More/Less info", href = "#", style="color: #1faad7; font-size: 12px;"), style="text-align: center;"),
								div(id = "MiniTable",
									DT::dataTableOutput('MiniTab', width="100%"), style="font-size:80%"
									),
							style = "opacity: 1; z-index: 2;"
						),	
						width=9
					)
				)
			),
			tabPanel("Graphs Explained",
				h2("What's in a Graph?"),
				# Draw map and other elements
				sidebarLayout(      
					sidebarPanel(
						helpText("The example figure below highlights the major components shown on many of COASSTs data visuals."),
						helpText("COASST takes beached bird abundance and calculates an", strong("Encounter Rate"), " (ER), which is the number of beached birds per km surveyed. For each of the",strong("Data Visuals"), "ER from individual surveys are averaged monthly (", strong("Trends-in-Time"), ") or seasonally (", strong("Regional differences"), ") by COASST region (see map) for ", em("Species"), " of interest, which can be selected within data views from drop-down menus."),
						helpText("In this example figure we show month-averaged ER plotted through time (see ", strong("Trends in Time"), "), as well as the ", em("Baseline"), " that represents the long-term average, or expected ER for a specific species, location and time of year."),
						hr(),
						actionLink("help2", label="More Info",icon=icon("info-circle"), style="color: #1faad7; font-size: 14px"),
						width=3),
					mainPanel(
						plotlyOutput(outputId = "explan_figure", height="550px", width="100%"),
						width=9
					)
				)
			),
			#############################################################################################
			navbarMenu("Data Visuals",
				tabPanel("Regional differences", 
					h2("Explore regional differences in COASST Beached Bird Data"),
					sidebarLayout(      
						sidebarPanel(
							helpText("The information presented here is the regional baselines of beached bird encounter rates* expressed for Summer (May-Oct) and Winter (Nov-Apr) months."),
							helpText("Each region has different patterns of species abundance, and some regions are naturally more 'birdy' than others due to environmental and/or food patterns extant in that area. There are also broad differences between summer (breeding birds plus arctic migrants in the North) and winter (winterkill and arctic migrants in the South) months in terms of dead bird abundance"),
							helpText("Data for different species (as well as All species) can be viewed by selecting from the dropdown menu below."),
							selectInput("SpeciesInRegion", "Species:", Full.Species.List, selected="All", selectize=FALSE),
							hr(),
							helpText("* Encounter rate is the number of carcasses counted per km of beach walked."),
							actionLink("help4", label="More Info",icon=icon("info-circle"), style="color: #1faad7; font-size: 14px"),
							width=3),
						mainPanel(plotlyOutput(outputId = "regional_plot", height="600px", width="100%"), width=9)
					)
				),
				tabPanel("Trends in time", 
					h2("Explore trends in time of COASST Beached Bird Data"),
					sidebarLayout(      
						sidebarPanel(
							helpText("The information presented here is the regional average encounter rates* for each month that COASST has been collecting data. By interacting with the plot you can see how beached bird carcass numbers collected by COASST varied through time."),
							helpText("The natural history patterns of beached birds create seasonal patterns in carcass abundance. We illustrate this by calculating a baseline, which represents what we would normally expect at a given time."),
							helpText("Data for different regions/species can be viewed by selecting from the dropdown menus below."),
							selectInput("dataRegion", "Region:", c("Chukchi Sea","Bering Sea","Aleutian Islands","Gulf of AK","Strait of Juan de Fuca", "Northern WA", "Southern WA", "Northern OR", "Southern OR", "Northern CA: Humboldt", "Northern CA: Mendocino"), selected="Northern WA", selectize=FALSE),
							uiOutput("SpecControls"),
							helpText("A specific period of time can be viewed by moving the sliders below"),
							uiOutput("SlideControls"),
							helpText("* Encounter rate is the number of carcasses counted per km of beach walked."),
							actionLink("help3", label="More Info",icon=icon("info-circle"), style="color: #1faad7; font-size: 14px"),
							width=3),
						mainPanel(plotlyOutput(outputId = "timeseries_plot", height="600px", width="100%"), width=9)
					)
				),
				tabPanel("Species composition", 
					h2("Explore species composition characteristics of COASST Beached Bird Data"),
					sidebarLayout(      
						sidebarPanel(
							helpText("The information presented here is the regional baselines of beached bird encounter rates* throughout the year."),
							helpText("At different times of year the baseline consists of different species depending on the natural history patterns of the birds in that region. These patterns are created by natural events such as migration, breeding and molt cycles."),
							helpText("Data for different regions can be viewed by selecting from the dropdown menu below."),
							selectInput("dataRegion2", "Region:", rev(c("Northern CA: Mendocino","Northern CA: Humboldt", "Southern OR", "Northern OR", "Southern WA", "Northern WA")), selected="Northern WA", selectize=FALSE),
							helpText("Up to four species-specific baselines can be added so that you can see what species contribute to the seasonal pattern in that region."),
							uiOutput("SpecControls2"),
							helpText("Species specific baselines are plotted seperately, but to get a better idea of the cumulative composition, baselines can be stacked on top of each other. This can be turned on and off below"),
							checkboxInput(inputId="StackorNo", label="Stack baselines", value=FALSE),
							hr(),
							helpText("* Encounter rate is the number of carcasses counted per km of beach walked."),
							actionLink("help5", label="More Info",icon=icon("info-circle"), style="color: #1faad7; font-size: 14px"),
							width=3),
						mainPanel(plotlyOutput(outputId = "composition_plot", height="600px", width="100%"), width=9)
					)
				)
			),
			################################################
			navbarMenu("Tables",
				tabPanel('COASST Top 10',
					h2("COASST Top-10 birds by abundance"),
					tags$style(type="text/css", "body {padding-top: 70px;}"),
					sidebarLayout(      
						sidebarPanel(
							helpText("This is a list of the top-10 species by total number of carcasses counted"),
							helpText("Choose a Year and Region, or select All to see data across all years/all regions"),
							selectInput(inputId = "top10Reg", label = "Region:", choices=Region.MAPLS, selectize=FALSE),
							uiOutput("YearControlTop10"),
							hr(),
							helpText("* indicates individuals could not be identified to species and were identified to the highest taxonomic level possible"),
							actionLink("help6", label="More Info",icon=icon("info-circle"), style="color: #1faad7; font-size: 14px"),
							width=3
						),
						mainPanel(DT::dataTableOutput('ex1'), width=9)
					) 
				),
				################################################
				tabPanel('Species List',
					h2("COASST List of all species encountered"),
					sidebarLayout(      
						sidebarPanel(
							helpText(textOutput(outputId="NSpeciestotal")),
							helpText("Region specific total abundances can be viewed by selecting regions from the dropdown below. Note that some species are only found in certain regions"),
							selectInput(inputId = "Speclist.Reg", label = "Region", choices=Region.MAPLS, selectize=FALSE),
							uiOutput("YearControlSpeclist"),
							hr(),
							helpText("* indicates individuals could not be identified to species and were identified to the highest taxonomic level possible"),
							actionLink("help7", label="More Info",icon=icon("info-circle"), style="color: #1faad7; font-size: 14px"),
							width=3
						),
						mainPanel(DT::dataTableOutput('ex2'), width=9)
					)
				),
				################################################
				tabPanel('Oiled Birds',
					h2("COASST observations of oiled birds and locations"),
					sidebarLayout(      
						sidebarPanel(
							helpText("Occasionally, COASST finds dead oiled birds on the beach. Very rarely, these birds are associated with a known spill in the area. In most cases, these oiling events are the only evidence of a mystery release of oil somewhere in the coastal environment. By documenting incidents of oiled birds, COASST is able to track the rate of chronic oiling. Fortunately, chronic oiling rates are low in all areas that COASST currently monitors."),
							helpText("Data on oiled birds are presented in two ways:"),
							helpText("First, by beach, including the total number of oiled birds found on each beach and as a percentage of the total number of oiled birds found across all beaches."),
							helpText("Second, by species, including the total number of birds of each species that was found oiled and the percent of all oiled birds represented by each species."),
							selectInput(inputId = "Beach_Species", label = "Beach|Species:", choices=c("Beach", "Species"), selectize=FALSE),
							helpText("For a breakdown of oiling data by year, please select a year from the drop-down menu"),
							selectInput(inputId = "YearOil",label = "Year:",choices=Full.Year.list, selectize=FALSE),
							hr(),
							helpText("* indicates individuals could not be identified to species and were identified to the highest taxonomic level possible"),
							actionLink("help9", label="More Info",icon=icon("info-circle"), style="color: #1faad7; font-size: 14px"),
							width=3),
						mainPanel(DT::dataTableOutput('ex3'), width=9)
					)
				),
				################################################
				tabPanel('Entanglements',
					h2("COASST observations of entangled birds and locations"),
					sidebarLayout(      
						sidebarPanel(
							helpText("Occasionally, COASST finds dead birds that are entangled in fishing line, hooks, net, or some other man-made substance. Fortunately, these are rare occurrences, but it is important to document these incidents of human disturbance."),
							helpText("Data on entangled birds are presented by entanglement type, which can be viewed in two ways:"),
							helpText("First, by beach, including the number of entangled birds found on each beach by entanglement type as well as the total for each beach."),
							helpText("Second, by species, including the number of birds of each species that was found entangled, split by entanglement type, as well as the total for each species."),
							selectInput(inputId = "Beach_Species_entangle", label = "Beach|Species:", choices=c("Beach", "Species"), selectize=FALSE),
							helpText("For a breakdown of entanglement data by year, please select a year from the drop-down menu"),
							selectInput(inputId = "YearEntangle",label = "Year:",choices=Full.Year.list, selectize=FALSE),
							hr(),
							helpText("* indicates individuals could not be identified to species and were identified to the highest taxonomic level possible"),
							actionLink("help8", label="More Info",icon=icon("info-circle"), style="color: #1faad7; font-size: 14px"),
							width=3),
						mainPanel(DT::dataTableOutput('ex4'), width=9)
					)
				)
				################################################
			)
		)
	)
)



