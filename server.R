###### Need
library(shiny)
library(sp)
library(plotly)
library(lubridate)
library(shinyalert)
library(shinyjs)

shinyServer(function(input, output, session) {
	
	###############################################################################################
	# General and interactive features
	
	# Collapses menu after item has been selected
	observeEvent(input$nav,{
		runjs('
			var elem=document.getElementsByClassName("navbar-collapse")[0]
			elem.setAttribute("aria-expanded", "false");
			elem.setAttribute("class", "navbar-collapse collapse");
		')
	})
	
	# Format of hover info
	hover.format <- list(
			font = list(family="Raleway", size=14))
	
	# Interactive help	
	observeEvent(input$help1, {
		# Show a modal when the button is pressed
		shinyalert(title=NULL, 
		text=HTML("<b>Explore data</b> <br> <br> INFORMATION ON HOW TO OPERATE APPS AND THE DIFFERENT OPTIONS AVAILABLE <br>
		<br> If you would like more information about the COASST program, please visit our <a href='https://coasst.org/'> homepage</a> 
		to look for upcoming trainings, or to contact us to see how you can get involved."), 
		closeOnClickOutside = TRUE, 
		html=TRUE, 
		imageUrl='COASST_logo.png', imageWidth=160, 
		animation=FALSE)
	})
	
	# Help 2 - graphs explained
	observeEvent(input$help2, {
		# Show a modal when the button is pressed
		shinyalert(title=NULL, 
		text=HTML("<b>Explore data</b> <br> <br> INFORMATION ON HOW TO OPERATE APPS AND THE DIFFERENT OPTIONS AVAILABLE <br>
		<br> If you would like more information about the COASST program, please visit our <a href='https://coasst.org/'> homepage</a> 
		to look for upcoming trainings, or to contact us to see how you can get involved."), 
		closeOnClickOutside = TRUE, 
		html=TRUE, 
		imageUrl='COASST_logo.png', imageWidth=160, 
		animation=FALSE)
	})
	
	# Help 3 - trends in time
	observeEvent(input$help3, {
		# Show a modal when the button is pressed
		shinyalert(title=NULL, 
		text=HTML("<b>Explore data</b> <br> <br> INFORMATION ON HOW TO OPERATE APPS AND THE DIFFERENT OPTIONS AVAILABLE <br>
		<br> If you would like more information about the COASST program, please visit our <a href='https://coasst.org/'> homepage</a> 
		to look for upcoming trainings, or to contact us to see how you can get involved."), 
		closeOnClickOutside = TRUE, 
		html=TRUE, 
		imageUrl='COASST_logo.png', imageWidth=160, 
		animation=FALSE)
	})
	
	# Help 4 - regional differences explained
	observeEvent(input$help4, {
		# Show a modal when the button is pressed
		shinyalert(title=NULL, 
		text=HTML("<b>Explore data</b> <br> <br> INFORMATION ON HOW TO OPERATE APPS AND THE DIFFERENT OPTIONS AVAILABLE <br>
		<br> If you would like more information about the COASST program, please visit our <a href='https://coasst.org/'> homepage</a> 
		to look for upcoming trainings, or to contact us to see how you can get involved."), 
		closeOnClickOutside = TRUE, 
		html=TRUE, 
		imageUrl='COASST_logo.png', imageWidth=160, 
		animation=FALSE)
	})
	
	# Help 5 - species baselines
	observeEvent(input$help5, {
		# Show a modal when the button is pressed
		shinyalert(title=NULL, 
		text=HTML("<b>Explore data</b> <br> <br> INFORMATION ON HOW TO OPERATE APPS AND THE DIFFERENT OPTIONS AVAILABLE <br>
		<br> If you would like more information about the COASST program, please visit our <a href='https://coasst.org/'> homepage</a> 
		to look for upcoming trainings, or to contact us to see how you can get involved."), 
		closeOnClickOutside = TRUE, 
		html=TRUE, 
		imageUrl='COASST_logo.png', imageWidth=160, 
		animation=FALSE)
	})
	
	# Help 6 - top 10
	observeEvent(input$help6, {
		# Show a modal when the button is pressed
		shinyalert(title=NULL, 
		text=HTML("<b>Explore data</b> <br> <br> INFORMATION ON HOW TO OPERATE APPS AND THE DIFFERENT OPTIONS AVAILABLE <br>
		<br> If you would like more information about the COASST program, please visit our <a href='https://coasst.org/'> homepage</a> 
		to look for upcoming trainings, or to contact us to see how you can get involved."), 
		closeOnClickOutside = TRUE, 
		html=TRUE, 
		imageUrl='COASST_logo.png', imageWidth=160, 
		animation=FALSE)
	})
	
	# Help 7 - species list
	observeEvent(input$help7, {
		# Show a modal when the button is pressed
		shinyalert(title=NULL, 
		text=HTML("<b>Explore data</b> <br> <br> INFORMATION ON HOW TO OPERATE APPS AND THE DIFFERENT OPTIONS AVAILABLE <br>
		<br> If you would like more information about the COASST program, please visit our <a href='https://coasst.org/'> homepage</a> 
		to look for upcoming trainings, or to contact us to see how you can get involved."), 
		closeOnClickOutside = TRUE, 
		html=TRUE, 
		imageUrl='COASST_logo.png', imageWidth=160, 
		animation=FALSE)
	})
	
	# Help 8 - entanglements
	observeEvent(input$help8, {
		# Show a modal when the button is pressed
		shinyalert(title=NULL, 
		text=HTML("<b>Explore data</b> <br> <br> INFORMATION ON HOW TO OPERATE APPS AND THE DIFFERENT OPTIONS AVAILABLE <br>
		<br> If you would like more information about the COASST program, please visit our <a href='https://coasst.org/'> homepage</a> 
		to look for upcoming trainings, or to contact us to see how you can get involved."), 
		closeOnClickOutside = TRUE, 
		html=TRUE, 
		imageUrl='COASST_logo.png', imageWidth=160, 
		animation=FALSE)
	})
	
	# Help 9 - oiling
	observeEvent(input$help9, {
		# Show a modal when the button is pressed
		shinyalert(title=NULL, 
		text=HTML("<b>Explore data</b> <br> <br> INFORMATION ON HOW TO OPERATE APPS AND THE DIFFERENT OPTIONS AVAILABLE <br>
		<br> If you would like more information about the COASST program, please visit our <a href='https://coasst.org/'> homepage</a> 
		to look for upcoming trainings, or to contact us to see how you can get involved."), 
		closeOnClickOutside = TRUE, 
		html=TRUE, 
		imageUrl='COASST_logo.png', imageWidth=160, 
		animation=FALSE)
	})
	
	#*********************************************************************************************#
	
	###############################################################################################
	# Welcome page
	
	# Little map
	output$map <- renderLeaflet({

		if(input$Map_L48_AK=="Lower-48"){
			pal <- rainbow(n=9, end=0.85)
			leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 5, maxZoom = 5, dragging = TRUE)) %>%
			addProviderTiles("Esri.WorldTopoMap") %>%
			addPolygons(data=SpDf.L48, label=as.character(SpDf.L48$ID), layerId =~ID, fillColor=pal, weight=0.5, color="#000000", smoothFactor=1.2, fillOpacity=0.3, labelOptions = labelOptions(direction = "auto", textOnly = T, style = list("font-family" = "Raleway","font-style" = "italic", "font-size" = "14px","color" = "rgba(0,0,0,0.8)" ))) %>%
			setView(lng = -127, lat = 43.8, zoom = 5)
		} else {
			pal <- rainbow(n=5, end=0.85)
			leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 3, maxZoom = 3, dragging = TRUE)) %>%
			addProviderTiles("Esri.WorldTopoMap") %>%
			addPolygons(data=SpDf.AK, label=as.character(SpDf.AK$ID), layerId =~ID, fillColor=pal, weight=0.5, color="#000000", smoothFactor=1.0, fillOpacity=0.3,labelOptions = labelOptions(direction = "auto", textOnly = T, style = list("font-family" = "Raleway","font-style" = "italic", "font-size" = "14px","color" = "rgba(0,0,0,0.8)" ))) %>%
			setView(lng = -159, lat = 62.5, zoom = 3) 
		}
	})
	
	data <- reactiveValues(clickedShape=NULL)
	
	observeEvent(input$map_shape_click,
                {data$clickedShape <- input$map_shape_click$id})
	
	observeEvent(input$Map_L48_AK,
                {data$clickedShape <- NULL})
			   
	observeEvent(input$toggleMiniTable,
				{data$clickedShape <- NULL})

	# Mini table
	output$MiniTab <- DT::renderDataTable({
		inp <- data$clickedShape 
		if(is.null(inp)){
			if(input$Map_L48_AK == "Lower-48"){
				tmp2 <- TempTab[TempTab$reg.vis=="All L48",c("Year", "Nsurv", "B.active", "Nbird", "Nspecies", "Bird1", "Bird2", "Bird3")] 
			} else {
				tmp2 <- TempTab[TempTab$reg.vis=="All AK",c("Year", "Nsurv", "B.active", "Nbird", "Nspecies", "Bird1", "Bird2", "Bird3")]
			}
		} else {	
			tmp2 <- TempTab[TempTab$reg.vis==inp,c("Year", "Nsurv", "B.active", "Nbird", "Nspecies", "Bird1", "Bird2", "Bird3")] 
		}
		tmp2 <- t(tmp2)
		rownames(tmp2) <- c("Year Est.","Surveys","Active Beaches", "Birds", "Species", "Bird #1", "Bird #2", "Bird #3")
		
		datatable(tmp2, options=list(searching=FALSE, paging=FALSE, info=FALSE, ordering=FALSE))
	})
	
	# Toggles menu 
	onclick("toggleMiniTable",						#######
        toggle(id = "MiniTable", anim = TRUE))    #########
	
	
	observeEvent(data$clickedShape, {
		showElement(id = "MiniTable", anim = TRUE) 
	})
	
	#*********************************************************************************************#
	
	###############################################################################################
	# Graphs explained
	
	# Explanatory figure
	output$explan_figure <- renderPlotly({
    
		roll_calendar <- data.frame(Month = 1:12,month_abbr=(c("Jan", "Feb", "Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")))
		
		# Use Oregon North all species 2013-2016 as a template
		expl.Base.df <- BaseDF[BaseDF$Species=="All" & BaseDF$Region=="Oregon North",]
		expl.Obs.dat <- R.df[R.df$Region == "Oregon North" & R.df$Species == "All" & R.df$Year %in% 2013:2016,]
		
		# Replicate Baseline data
		YearsFill <- 2013:2016
		MonFill <- 1:12
		DateGrid <- expand.grid(MonFill, YearsFill)
		names(DateGrid) <- c("Month", "Year")
		Date.DG <- as.Date(paste("15-", DateGrid$Month, "-", DateGrid$Year, sep=""), format="%d-%m-%Y")
	
		Mean.ER.DG <- rep(expl.Base.df$Mean.ER, length(YearsFill))
		Low.ER.DG <- rep(expl.Base.df$Low.ER, length(YearsFill))
		High.ER.DG <- rep(expl.Base.df$High.ER, length(YearsFill))
		Median.ER.DG <- rep(expl.Base.df$Median.ER, length(YearsFill))

		expl.Base.All <- data.frame(Date=Date.DG,Mean.ER=Mean.ER.DG,Low.ER=Low.ER.DG, High.ER=High.ER.DG, Median.ER=Median.ER.DG)  
		
		expl.Obs.dat$MonNum <- 1
		expl.Obs.dat$MonNum[expl.Obs.dat$Month=="Jan"] <- 1
		expl.Obs.dat$MonNum[expl.Obs.dat$Month=="Feb"] <- 2
		expl.Obs.dat$MonNum[expl.Obs.dat$Month=="Mar"] <- 3
		expl.Obs.dat$MonNum[expl.Obs.dat$Month=="Apr"] <- 4
		expl.Obs.dat$MonNum[expl.Obs.dat$Month=="May"] <- 5
		expl.Obs.dat$MonNum[expl.Obs.dat$Month=="Jun"] <- 6
		expl.Obs.dat$MonNum[expl.Obs.dat$Month=="Jul"] <- 7
		expl.Obs.dat$MonNum[expl.Obs.dat$Month=="Aug"] <- 8
		expl.Obs.dat$MonNum[expl.Obs.dat$Month=="Sep"] <- 9
		expl.Obs.dat$MonNum[expl.Obs.dat$Month=="Oct"] <- 10
		expl.Obs.dat$MonNum[expl.Obs.dat$Month=="Nov"] <- 11
		expl.Obs.dat$MonNum[expl.Obs.dat$Month=="Dec"] <- 12
		
		expl.Obs.dat$Date <- as.Date(paste("15-", expl.Obs.dat$MonNum, "-", expl.Obs.dat$Year, sep=""), format="%d-%m-%Y")
		
		# Order both by date
		expl.Obs.plot <- expl.Obs.dat[order(expl.Obs.dat$Date),]
		expl.Base.plot <- expl.Base.All[order(expl.Base.All$Date),]
		
		# define some limits
		
		maxY <- max(max(expl.Base.plot$High.ER, na.rm=T), max(expl.Obs.plot$ER, na.rm=T), na.rm=T)*1.05 + 0.01
		
		###############################
		# Create annotations
		
		anno <- list()
		anno[[1]] <- list(
			x = as.Date("2015-02-01"), y = 20, text = "<i><b>Red bars:</b> 'wrecks'<br>classified as<br>ER > 5<span>&#215;</span><b>Baseline</b></i>",
			showarrow = FALSE, 
			#arrowhead=1, 
			#ax=40,
			#ay=0, 
			font = list(color = '#1F2935', 
				family = 'Raleway',
				size=12), 
			align="left",
			xanchor = "left"
		)
		#anno[[2]] <- list(
		#	x = as.Date("2017-01-01"), 
		#	y = 29, 
		#	text = "<i>Clicking legend<br>labels will turn<br>items on/off</i>",
		#	showarrow = TRUE, 
		#	arrowhead=1, 
		#	xanchor = "right",
		#	ax=-15,
		#	ay=0, 
		#	align="right",
		#	font = list(color = '#1F2935', 
		#		family = 'Raleway',
		#		size=12)
		#)
		anno[[2]] <- list(
			x = as.Date("2014-02-01"), y = 0.94, text = "<i><b>Black line:</b> long-term <br> average or <b>Baseline</b><br>for that month</i>",
			showarrow = TRUE, 
			arrowhead=1, 
			ax=0,
			ay=-140, 
			font = list(color = '#1F2935', 
				family = 'Raleway',
				size=12)
		)
		anno[[3]] <- list(
			x = as.Date("2013-08-01"), y = 1.83, text = "<i><b>Grey bars:</b><br>observed ER</i>",
			showarrow = TRUE, 
			arrowhead=1, 
			ax=-10,
			ay=-80, 
			font = list(color = '#1F2935', 
				family = 'Raleway',
				size=12)
		)
		anno[[4]] <- list(
			x = as.Date("2015-11-01"), y = 5.18, text = "<i><b>Yellow:</b> range<br>in <b>Baseline</b><br>average</i>",
			showarrow = TRUE, 
			arrowhead=1, 
			ax=10,
			ay=-48, 
			font = list(color = '#1F2935', 
				family = 'Raleway',
				size=12), 
			align="left", 
			xanchor = "left"
		)
			
		###############################
				
		#### Make the Plot
		
		spec.write <- "Species name"
			
		expl.Base.plot$High.ER[is.na(expl.Base.plot$High.ER)] <- 0
		expl.Base.plot$Low.ER[is.na(expl.Base.plot$Low.ER)] <- 0
		
		day(expl.Base.plot$Date) <- 1
		day(expl.Obs.plot$Date) <- 1
		
		Xribbon <- c(expl.Base.plot$Date, rev(expl.Base.plot$Date))
		Yribbon <- c(round(expl.Base.plot$Low.ER,2), rev(round(expl.Base.plot$High.ER,2)))
		
		expl.Base.plot$Mean.ER <- round(expl.Base.plot$Mean.ER,2)
		expl.Base.plot$High.ER <- round(expl.Base.plot$High.ER,2)
		expl.Base.plot$Low.ER <- round(expl.Base.plot$Low.ER,2)
		expl.Obs.plot$ER <- round(expl.Obs.plot$ER,2)
		
		plottest <- plot_ly(data=expl.Obs.plot[expl.Obs.plot$Wreck==0,], x=~Date,y=~ER,type="bar", marker = list(color = 'rgb(190,190,190)'), name="Obs. normal", hoverlabel=hover.format) %>%
			config(displayModeBar = F) %>%
			add_trace(data=expl.Obs.plot[expl.Obs.plot$Wreck==1,], x=~Date,y=~ER,type="bar", marker = list(color = 'rgb(200,0,0)'), name="Obs. elevated", hoverlabel=hover.format) %>%
			add_polygons(x=Xribbon, y=Yribbon,fillcolor='rgba(255,255,40,0.5)', name="Base. range",line = list(color = 'transparent'), marker=list(color='rgba(0,0,0,0)'), hoverlabel=hover.format) %>%
			add_trace(data=expl.Base.plot,x=~Date,y=~Mean.ER,type='scatter', mode='lines', name="Baseline",line = list(color = 'rgb(0,0,0)'), marker=list(color='rgba(0,0,0,0)'), hoverlabel=hover.format) %>%
			layout(margin=list(r=40, t=50, b=50, l=50),
			xaxis = list(title = "", titlefont=list(family="Raleway", size=16),tickfont=list(family="Raleway", size=16), fixedrange=TRUE), 
			yaxis = list(title = "Encounter Rate (birds/km)",titlefont=list(family="Raleway", size=16),tickfont=list(family="Raleway", size=16), fixedrange=TRUE),
			legend = list(x=1,y=1,font=list(family="Raleway", size=13), xanchor="right", yanchor="top"),
			font=list(family="Raleway", size=16),
			title = NULL,annotations = anno)
		
		plottest

	})
  
	#*********************************************************************************************#
	
	###############################################################################################
	# Time series plot
		
	output$SpecControls <- renderUI({
		if(input$dataRegion %in% c("Northern WA", "Southern WA", "Northern OR", "Southern OR", "Northern CA: Humboldt", "Northern CA: Mendocino")){
			SpecCont.ls <- SPECTAB[[which(names(SPECTAB)==input$dataRegion)]]
			selectInput("dataSpecies", "Species:", SpecCont.ls, selectize=FALSE)
		} 
	})
	
	output$SlideControls <- renderUI({
		
		today <- as.Date(Sys.Date())
		if(mday(today) > 28){
			today <- today
			today_2m <- today  
		} else {
			today_2m <- today  
		}
		DateMin <- YBegin.df$DateBegin[YBegin.df$Region==input$dataRegion]
		DateMax <- today_2m
		
		sliderInput(inputId="sliderdate",label="Date range:", min=DateMin, max=DateMax, value=c(DateMax-months(12), DateMax),timeFormat="%Y-%m")
	 
	})

	output$timeseries_plot <- renderPlotly({
    
		roll_calendar <- data.frame(Month = 1:12,month_abbr=(c("Jan", "Feb", "Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")))
		# Identify region
		Region.Select <- input$dataRegion 
		# Identify Species - replace with dynamic dropdown
		
		
		if(Region.Select %in% c("Northern WA", "Southern WA", "Northern OR", "Southern OR", "Northern CA: Humboldt", "Northern CA: Mendocino")){
			if(is.null(input$dataSpecies)){
				Species.Select <- "All"
			} else {
				Species.Select <- input$dataSpecies
			}
		} else {
			Species.Select <- "All"
		}
		# load the correct baseline and observed data
		Base.df <- BaseDF[BaseDF$Species==Species.Select & BaseDF$reg.vis==Region.Select,]
		Obs.dat <- R.df[R.df$reg.vis == Region.Select & R.df$Species == Species.Select,]
		
		# Handle Date and Month Issues
		today <- as.Date(Sys.Date())
		# Get date for two months from today and 14 months from today
		# changed so that it handles February and month weirdness
		if(mday(today) > 28){
			today <- today-5
			today_2m <- today  
			today_14m <- today - months(12)
		} else {
			today_2m <- today 
			today_14m <- today - months(12)
		}
		
		# Replicate baseline data
		YearsFill <- 1999:year(today_2m)
		MonFill <- 1:12
		DateGrid <- expand.grid(MonFill, YearsFill)
		names(DateGrid) <- c("Month", "Year")
		Date.DG <- as.Date(paste("15-", DateGrid$Month, "-", DateGrid$Year, sep=""), format="%d-%m-%Y")
	
		Mean.ER.DG <- rep(Base.df$Mean.ER, length(YearsFill))
		Low.ER.DG <- rep(Base.df$Low.ER, length(YearsFill))
		High.ER.DG <- rep(Base.df$High.ER, length(YearsFill))
		Median.ER.DG <- rep(Base.df$Median.ER, length(YearsFill))

		Base.All <- data.frame(Date=Date.DG,Mean.ER=Mean.ER.DG,Low.ER=Low.ER.DG, High.ER=High.ER.DG, Median.ER=Median.ER.DG)  
		
		Obs.dat$MonNum <- 1
		Obs.dat$MonNum[Obs.dat$Month=="Jan"] <- 1
		Obs.dat$MonNum[Obs.dat$Month=="Feb"] <- 2
		Obs.dat$MonNum[Obs.dat$Month=="Mar"] <- 3
		Obs.dat$MonNum[Obs.dat$Month=="Apr"] <- 4
		Obs.dat$MonNum[Obs.dat$Month=="May"] <- 5
		Obs.dat$MonNum[Obs.dat$Month=="Jun"] <- 6
		Obs.dat$MonNum[Obs.dat$Month=="Jul"] <- 7
		Obs.dat$MonNum[Obs.dat$Month=="Aug"] <- 8
		Obs.dat$MonNum[Obs.dat$Month=="Sep"] <- 9
		Obs.dat$MonNum[Obs.dat$Month=="Oct"] <- 10
		Obs.dat$MonNum[Obs.dat$Month=="Nov"] <- 11
		Obs.dat$MonNum[Obs.dat$Month=="Dec"] <- 12
		
		Obs.dat$Date <- as.Date(paste("15-", Obs.dat$MonNum, "-", Obs.dat$Year, sep=""), format="%d-%m-%Y")
		
		# Now limit the data so that nothing beyond two months back gets plotted
		
		Obs.plot <- Obs.dat[Obs.dat$Date < today_2m,]
		Base.plot <- Base.All[Base.All$Date < today_2m,]
		
		# Order both by date
		Obs.plot <- Obs.plot[order(Obs.plot$Date),]
		Base.plot <- Base.plot[order(Base.plot$Date),]
		
		day(Obs.plot$Date) <- 15
		day(Base.plot$Date) <- 15
		
		# define some limits
		
		maxY <- max(max(Base.plot$High.ER, na.rm=T), max(Obs.plot$ER[Obs.plot$Date >= today_14m], na.rm=T), na.rm=T)*1.05 + 0.01
		XRan <- as.numeric(c(today_14m,today_2m))*86400000
			
		#### Make the Plot
		
		if(Species.Select == "All"){
			spec.write <- "All Species"
		} else {
			spec.write <- Species.Select
		}
		
		Base.plot$High.ER[is.na(Base.plot$High.ER)] <- 0
		Base.plot$Low.ER[is.na(Base.plot$Low.ER)] <- 0
		
		day(Base.plot$Date) <- 1
		day(Obs.plot$Date) <- 1
		
		MINDate <- as.Date(as.character(input$sliderdate[1]), format="%Y-%m-%d")
		MAXDate <- as.Date(as.character(input$sliderdate[2]), format="%Y-%m-%d")
		
		try(day(MINDate) <- 1, silent=TRUE)
		try(day(MAXDate) <- 1, silent=TRUE)
		
		Obs.plot <- Obs.plot[Obs.plot$Date >= MINDate & Obs.plot$Date <= MAXDate,]
		Base.plot <- Base.plot[Base.plot$Date >= MINDate & Base.plot$Date <= MAXDate,]
		
		Xribbon <- c(Base.plot$Date, rev(Base.plot$Date))
		Yribbon <- c(round(Base.plot$Low.ER,2), rev(round(Base.plot$High.ER,2)))
		
		Base.plot$Mean.ER <- round(Base.plot$Mean.ER,2)
		Base.plot$High.ER <- round(Base.plot$High.ER,2)
		Base.plot$Low.ER <- round(Base.plot$Low.ER,2)
		Obs.plot$ER <- round(Obs.plot$ER,2)
		
		plottest <- plot_ly(data=Obs.plot[Obs.plot$Wreck==0,], x=~Date,y=~ER,type="bar", marker = list(color = 'rgb(190,190,190)'), name="Obs. normal", hoverlabel=hover.format) %>%
			config(displayModeBar = F) %>%
			add_trace(data=Obs.plot[Obs.plot$Wreck==1,], x=~Date,y=~ER,type="bar", marker = list(color = 'rgb(200,0,0)'), name="Obs. elevated", hoverlabel=hover.format) %>%
			add_polygons(x=Xribbon, y=Yribbon,fillcolor='rgba(255,255,40,0.5)', name="Base. range",line = list(color = 'transparent'), marker=list(color='rgba(0,0,0,0)'), hoverlabel=hover.format) %>%
			add_trace(data=Base.plot,x=~Date,y=~Mean.ER,type='scatter', mode='lines', name="Baseline",line = list(color = 'rgb(0,0,0)'), marker=list(color='rgba(0,0,0,0)'), hoverlabel=hover.format) %>%
			layout(margin=list(r=40, t=50, b=50, l=50),
			xaxis = list(title = "", titlefont=list(family="Raleway", size=16),tickfont=list(family="Raleway", size=16), fixedrange=TRUE), 
			yaxis = list(title = "Encounter Rate (birds/km)",titlefont=list(family="Raleway", size=16),tickfont=list(family="Raleway", size=16), fixedrange=TRUE),
			legend = list(x=1,y=1,font=list(family="Raleway", size=13), xanchor="right", yanchor="top"),
			font=list(family="Raleway", size=16),
			title = paste(Region.Select, ": ", spec.write, sep=""))
		
		plottest
		
	})
	
	#*********************************************************************************************#
	
	###############################################################################################
	# Composition plot
		
	output$SpecControls2 <- renderUI({
		SpecCont.ls2 <- SPECTAB[[which(names(SPECTAB)==input$dataRegion2)]]
		selectInput("dataSpecies2", "Species:", choices=SpecCont.ls2[2:5], multiple=TRUE, selectize=TRUE)
	})
		
	output$composition_plot <- renderPlotly({
    
		# Identify region
		Region.Select2 <- input$dataRegion2 
		# Identify Species - replace with dynamic dropdown
		if(is.null(input$dataSpecies2)){
			Species.Select2 <- "All"
		} else {
			Species.Select2 <- input$dataSpecies2
		}
		# load the correct baseline and observed data
		Base.comp <- BaseDF[BaseDF$Species %in% c("All",Species.Select2) & BaseDF$reg.vis==Region.Select2,]
		
		# Handle Date and Month Issues
		today.month <- month(as.Date(Sys.Date()))
		
		Base.comp$monthnum <- match(Base.comp$Month, month.abb)
		colorvec <- c("rgb(60,60,60)", "rgb(230,0,0)", "rgb(0, 230,0)", "rgb(0,0,230)", "rgb(0,230,230)")
		filcolvec <- c("rgba(60,60,60,0.3)", "rgba(230,0,0,0.3)", "rgba(0, 230,0,0.3)", "rgba(0,0,230,0.3)", "rgba(0,230,230,0.3)")
		
		Base.comp$Mean.ER <- round(Base.comp$Mean.ER, 2)
		# build graph
		# create plain graph with all species irrespective of species choice
		if(input$StackorNo==FALSE){
			
			p <- plot_ly(data=Base.comp, y=~Mean.ER,x=~monthnum,split=~Species, fill="tozeroy", type="scatter", mode="lines", line=list(width=3),showlegend=TRUE, hoverlabel=hover.format) %>% config(displayModeBar = F)
			layout(p,margin=list(r=40, t=50, b=50, l=50),
			xaxis = list(title = "", titlefont=list(family="Raleway", size=16),tickfont=list(family="Raleway", size=16), tickvals=1:12, ticktext=month.abb, fixedrange=TRUE), 
			yaxis = list(title = "Encounter Rate (birds/km)",titlefont=list(family="Raleway", size=16),tickfont=list(family="Raleway", size=16), fixedrange=TRUE),
			legend = list(x=1,y=1,font=list(family="Raleway", size=13), xanchor="right", yanchor="top"),
			font=list(family="Raleway", size=18))
		
		} else {
			Base.cumu <- Base.comp
			Species.Select2 <- Species.Select2[rev(order(Species.Select2))]
			for(i in 1:length(Species.Select2)){
				if(i==1){
					Base.cumu$Perc.ER[Base.cumu$Species==Species.Select2[i]] <- 100*Base.cumu$Mean.ER[Base.cumu$Species==Species.Select2[i]]/Base.cumu$Mean.ER[Base.cumu$Species=="All"]
					Base.cumu$Mean.ER[Base.cumu$Species==Species.Select2[i]] <- Base.cumu$Mean.ER[Base.cumu$Species==Species.Select2[i]]
				} else {
					Base.cumu$Perc.ER[Base.cumu$Species==Species.Select2[i]] <- 100*Base.cumu$Mean.ER[Base.cumu$Species==Species.Select2[i]]/Base.cumu$Mean.ER[Base.cumu$Species=="All"]
					Base.cumu$Mean.ER[Base.cumu$Species==Species.Select2[i]] <- Base.cumu$Mean.ER[Base.cumu$Species==Species.Select2[i-1]] + Base.cumu$Mean.ER[Base.cumu$Species==Species.Select2[i]]
				}
			}
			Base.cumu$Perc.ER[Base.cumu$Species=="All"] <- 100
			Base.cumu$Perc.ER[Base.cumu$Perc.ER > 100] <- 100
			Base.cumu$Perc.ER[is.na(Base.cumu$Perc.ER)] <- 0
			
			Base.cumu$Mean.ER <- round(Base.cumu$Mean.ER, 2)
			
			p <- plot_ly(data=Base.cumu, y=~Mean.ER,x=~monthnum, fill="tozeroy", type="scatter", split=~Species, mode="lines", showlegend=TRUE,line=list(width=3), hoverlabel=hover.format) %>% 
			config(displayModeBar = F)
			
			layout(p,margin=list(r=40, t=50, b=50, l=50),
			xaxis = list(title = "", titlefont=list(family="Raleway", size=16),tickfont=list(family="Raleway", size=16), tickvals=1:12, ticktext=month.abb, fixedrange=TRUE), 
			yaxis = list(title = "Encounter Rate (birds/km)",titlefont=list(family="Raleway", size=16),tickfont=list(family="Raleway", size=16), fixedrange=TRUE),
			legend = list(x=1,y=1,font=list(family="Raleway", size=13), xanchor="right", yanchor="top"),
			font=list(family="Raleway", size=16))
			
		}
	})
	
	#*********************************************************************************************#
	
	###############################################################################################
	# Regional plot
		
	output$regional_plot <- renderPlotly({
    
		TaxaSelect <- input$SpeciesInRegion
		ORDREG <- c("Chukchi Sea","Bering Sea","Aleutian Islands","Gulf of AK","Southeast AK","San Juan Islands","Puget Sound","Strait of Juan de Fuca", "Northern WA", "Southern WA", "Northern OR", "Southern OR", "Northern CA: Humboldt", "Northern CA: Mendocino")
		p <- plot_ly(
			x = BarPlotDATA$reg.vis[BarPlotDATA$Season=="Summer" & BarPlotDATA$Species==TaxaSelect],
			y = BarPlotDATA$ER[BarPlotDATA$Season=="Summer" & BarPlotDATA$Species==TaxaSelect],
			name = "Summer",
			type = "bar", marker = list(color = 'rgb(255,181,0)'), hoverlabel=hover.format
			) %>% config(displayModeBar = F)

		p2 <- add_trace(
			p,
			x = BarPlotDATA$reg.vis[BarPlotDATA$Season=="Winter" & BarPlotDATA$Species==TaxaSelect],
			y = BarPlotDATA$ER[BarPlotDATA$Season=="Winter" & BarPlotDATA$Species==TaxaSelect],
			name = "Winter",
			type = "bar", marker = list(color = 'rgb(0,178,255)'), hoverlabel=hover.format)

		p_final <- layout(p2, margin=list(r=50, t=50, b=120, l=50),
			xaxis = list(title = "", titlefont=list(family="Raleway", size=16),tickfont=list(family="Raleway", size=16), fixedrange=TRUE, categoryorder="array", categoryarray = ORDREG), 
			yaxis = list(title = "Encounter Rate (birds/km)",titlefont=list(family="Raleway", size=16),tickfont=list(family="Raleway", size=16), fixedrange=TRUE),
			legend = list(x=1,y=1,font=list(family="Raleway", size=13), xanchor="right", yanchor="top"),
			font=list(family="Raleway", size=16))
		p_final

	})
	
	#*********************************************************************************************#
	
	###############################################################################################
	# Top ten panel
	
	output$YearControlTop10 <- renderUI({

		Y.MINIMUM <- YBegin.df$YearBegin[YBegin.df$Region==input$top10Reg]
		
		Y.MAXIMUM <- year(Sys.Date())
		year.cho <- Y.MINIMUM:Y.MAXIMUM
		year.cho2 <- c("All", as.character(year.cho))
		selectInput(inputId = "top10Year", label = "Year:", choices=year.cho2, selectize=FALSE)
	})
	
	output$ex1 <- DT::renderDataTable({
		# Function that calculates top-ten species
		process.table <- function(yearselec, regselec){
			if(yearselec=="All"){			# all years selected
				if(regselec=="All"){
					alldat <- ctot[ctot$is_survey & !is.na(ctot$is_survey) & ctot$is_bird & !is.na(ctot$is_bird) & ctot$refound==FALSE & !is.na(ctot$refound),]
					if(nrow(alldat) > 0){
						spec.nums.all <- tapply(X=alldat$year, INDEX=alldat$Species, FUN=length)
						spec.nums.all <- spec.nums.all[order(-spec.nums.all)]
						outdat <- data.frame(Species=names(spec.nums.all), Count=spec.nums.all, stringsAsFactors=FALSE)
						outdat$Percent <- round(100*outdat$Count/sum(outdat$Count),2)
						outdat <- outdat[outdat$Species %in% c("*", "Unknown*")==FALSE,]
						outdat$Group <- species.index$group_lab[match(outdat$Species, species.index$species_name)]
						outdat <- outdat[, c(1,4,2,3)]
						# adds total
						add.data <- outdat[1,]
						add.data$Species <- "ALL BIRDS"
						add.data$Group <- "ALL"
						add.data$Count <- dim(alldat)[1]
						add.data$Percent <- 100
						rownames(add.data) <- ""
						tmp.df <- outdat[1:10,]
						row.names(tmp.df) <- as.character(1:10)
						out <- rbind(add.data,tmp.df)
					} else {
						out <- data.frame(Species = "NO BIRDS", Group="ALL", Count=0, Percent="")
						rownames(out) <- ""
					}
				} else {
					alldat <- ctot[ctot$reg.vis == regselec & ctot$is_survey & !is.na(ctot$is_survey) & ctot$is_bird & !is.na(ctot$is_bird) & ctot$refound==FALSE & !is.na(ctot$refound),]
					if(nrow(alldat) > 0){
						spec.nums.all <- tapply(X=alldat$year, INDEX=alldat$Species, FUN=length)
						spec.nums.all <- spec.nums.all[order(-spec.nums.all)]
						outdat <- data.frame(Species=names(spec.nums.all), Count=spec.nums.all, stringsAsFactors=FALSE)
						outdat$Percent <- round(100*outdat$Count/sum(outdat$Count),2)
						outdat <- outdat[outdat$Species %in% c("*", "Unknown*")==FALSE,]
						outdat$Group <- species.index$group_lab[match(outdat$Species, species.index$species_name)]
						outdat <- outdat[, c(1,4,2,3)]
						# adds total
						add.data <- outdat[1,]
						add.data$Species <- "ALL BIRDS"
						add.data$Group <- "ALL"
						add.data$Count <- dim(alldat)[1]
						add.data$Percent <- 100
						rownames(add.data) <- ""
						tmp.df <- outdat[1:10,]
						row.names(tmp.df) <- as.character(1:10)
						out <- rbind(add.data,tmp.df)
					} else {
						out <- data.frame(Species = "NO BIRDS", Group="ALL", Count=0, Percent="")
						rownames(out) <- ""
					}
				}
			} else {				# specific year requested
				if(regselec=="All"){
					Yearnumer <- as.numeric(yearselec)
					alldat <- ctot[ctot$is_survey & !is.na(ctot$is_survey) & ctot$is_bird & !is.na(ctot$is_bird) & ctot$refound==FALSE & !is.na(ctot$refound) & ctot$year==Yearnumer,]
					if(nrow(alldat) > 0){
						spec.nums.all <- tapply(X=alldat$year, INDEX=alldat$Species, FUN=length)
						spec.nums.all <- spec.nums.all[order(-spec.nums.all)]
						outdat <- data.frame(Species=names(spec.nums.all), Count=spec.nums.all, stringsAsFactors=FALSE)
						outdat$Percent <- round(100*outdat$Count/sum(outdat$Count),2)
						outdat <- outdat[outdat$Species %in% c("*", "Unknown*")==FALSE,]
						outdat$Group <- species.index$group_lab[match(outdat$Species, species.index$species_name)]
						outdat <- outdat[, c(1,4,2,3)]
						add.data <- outdat[1,]
						add.data$Species <- "ALL BIRDS"
						add.data$Group <- "ALL"
						add.data$Count <- dim(alldat)[1]
						add.data$Percent <- 100
						rownames(add.data) <- ""
						tmp.df <- outdat[1:10,]
						row.names(tmp.df) <- as.character(1:10)
						out <- rbind(add.data,tmp.df)
					} else {
						out <- data.frame(Species = "NO BIRDS", Group="", Count=0, Percent="")
						rownames(out) <- ""
					}
				} else {
					Yearnumer <- as.numeric(yearselec)
					alldat <- ctot[ctot$reg.vis == regselec & ctot$is_survey & !is.na(ctot$is_survey) & ctot$is_bird & !is.na(ctot$is_bird) & ctot$refound==FALSE & !is.na(ctot$refound) & ctot$year==Yearnumer,]
					if(nrow(alldat) > 0){
						spec.nums.all <- tapply(X=alldat$year, INDEX=alldat$Species, FUN=length)
						spec.nums.all <- spec.nums.all[order(-spec.nums.all)]
						outdat <- data.frame(Species=names(spec.nums.all), Count=spec.nums.all, stringsAsFactors=FALSE)
						outdat$Percent <- round(100*outdat$Count/sum(outdat$Count),2)
						outdat <- outdat[outdat$Species %in% c("*", "Unknown*")==FALSE,]
						outdat$Group <- species.index$group_lab[match(outdat$Species, species.index$species_name)]
						outdat <- outdat[, c(1,4,2,3)]
						add.data <- outdat[1,]
						add.data$Species <- "ALL BIRDS"
						add.data$Group <- "ALL"
						add.data$Count <- dim(alldat)[1]
						add.data$Percent <- 100
						rownames(add.data) <- ""
						tmp.df <- outdat[1:10,]
						row.names(tmp.df) <- as.character(1:10)
						out <- rbind(add.data,tmp.df)
					} else {
						out <- data.frame(Species = "NO BIRDS", Group="", Count=0, Percent="")
						rownames(out) <- ""
					}
				}
			}
			out <- out[!is.na(out$Count),]
			return(out)
		}
		datatable(process.table(input$top10Year, input$top10Reg),options=list(searching=FALSE, paging=FALSE, info=FALSE), rownames=FALSE)
	})

	#*********************************************************************************************#
	
	###############################################################################################
	# Species List
	
	output$YearControlSpeclist <- renderUI({

		Y.MINIMUM <- YBegin.df$YearBegin[YBegin.df$Region==input$Speclist.Reg]
		
		Y.MAXIMUM <- year(Sys.Date())
		year.cho <- Y.MINIMUM:Y.MAXIMUM
		year.cho2 <- c("All", as.character(year.cho))
		selectInput(inputId = "Speclist.Year", label = "Year:", choices=year.cho2, selectize=FALSE)
	})
	
	
	output$ex2 <- DT::renderDataTable({
		if(input$Speclist.Reg=="All"){
			if(input$Speclist.Year=="All"){
				alldat <- ctot[ctot$is_survey & !is.na(ctot$is_survey) & ctot$is_bird & !is.na(ctot$is_bird) & ctot$refound==FALSE & !is.na(ctot$refound),]
			} else {
				alldat <- ctot[ctot$year==input$Speclist.Year & ctot$is_survey & !is.na(ctot$is_survey) & ctot$is_bird & !is.na(ctot$is_bird) & ctot$refound==FALSE & !is.na(ctot$refound),]
			}
		} else {
			if(input$Speclist.Year=="All"){
				alldat <- ctot[ctot$is_survey & !is.na(ctot$is_survey) & ctot$is_bird & !is.na(ctot$is_bird) & ctot$refound==FALSE & !is.na(ctot$refound) & ctot$reg.vis==input$Speclist.Reg,]
			} else {
				alldat <- ctot[ctot$year==input$Speclist.Year & ctot$is_survey & !is.na(ctot$is_survey) & ctot$is_bird & !is.na(ctot$is_bird) & ctot$refound==FALSE & !is.na(ctot$refound) & ctot$reg.vis==input$Speclist.Reg,]
			}
		}
		
		if(nrow(alldat) > 0) {
			spec.nums.all <- tapply(X=alldat$year, INDEX=alldat$Species, FUN=length)
			spec.nums.all <- spec.nums.all[order(-spec.nums.all)]
			outdat <- data.frame(Species=names(spec.nums.all), Count=spec.nums.all, stringsAsFactors=FALSE)
			outdat$Percent <- round(100*outdat$Count/sum(outdat$Count),2)
			outdat <- outdat[outdat$Species %in% c("*", "Unknown*")==FALSE,]
			outdat$Group <- species.index$group_lab[match(outdat$Species, species.index$species_name)]
			outdat <- outdat[, c(1,4,2,3)]
			add.data <- outdat[1,]
			add.data$Species <- "ALL BIRDS"
			add.data$Group <- "ALL"
			add.data$Count <- dim(alldat)[1]
			add.data$Percent <- 100
			out <- rbind(add.data,outdat)
			row.names(out) <- c("",as.character(1:(dim(out)[1]-1)))
		} else {
			out <- data.frame(Species = "NO BIRDS", Group="", Count=0, Percent="")
			rownames(out) <- ""
		}
		datatable(out, rownames=FALSE)
	})
	
	# Calculates total number of species for species list helptext
	output$NSpeciestotal <- renderText(paste("This is a list of the total number of individuals counted of ", length(unique(ctot$species_name))-2, " species encountered in COASST surveys", sep=""))
	
	#*********************************************************************************************#
	
	###############################################################################################
	# Oiled birds
	output$ex3 <- DT::renderDataTable({
		
		spec_beach <- input$Beach_Species
		if(spec_beach=="Species"){
			yearselect <- input$YearOil
			if(yearselect=="All"){
				
				alldat2 <- ctot[ctot$is_survey & !is.na(ctot$is_survey) & ctot$is_bird & !is.na(ctot$is_bird) & ctot$refound==FALSE & !is.na(ctot$refound) & ctot$oil & !is.na(ctot$oil),]
				alldat2$Species[alldat2$Species=="*"] <- "Unknown*"
				spec.nums.oil <- tapply(X=alldat2$year, INDEX=alldat2$Species, FUN=length)
				spec.nums.oil <- spec.nums.oil[order(-spec.nums.oil)]
				outdat <- data.frame(Species=names(spec.nums.oil), Count=spec.nums.oil, stringsAsFactors=FALSE)
				outdat$Percent <- round(100*outdat$Count/sum(outdat$Count),2)
				
				outdat$Group <- species.index$group_lab[match(outdat$Species, species.index$species_name)]
				outdat <- outdat[, c(1,4,2,3)]
				
				out <- outdat
				ad.df <- out[1,]
				ad.df$Species[1] <- "ALL BIRDS"
				ad.df$Group[1] <- "ALL"
				ad.df$Count[1] <- sum(out$Count)
				ad.df$Percent[1] <- 100
					
				out <- rbind(ad.df,out)
				row.names(out) <- c("",as.character(1:(dim(out)[1]-1)))
			
			} else {
				Yearnumer <- as.numeric(yearselect)
				alldat2 <- ctot[ctot$is_survey & !is.na(ctot$is_survey) & ctot$is_bird & !is.na(ctot$is_bird) & ctot$refound==FALSE & !is.na(ctot$refound) & ctot$oil & !is.na(ctot$oil) & ctot$year==Yearnumer,]
				alldat2$Species[alldat2$Species=="*"] <- "Unknown*"
				
				if(dim(alldat2)[1]==0){
					Species <- "ALL BIRDS"
					Group <- "ALL"
					Count <- 0
					Percent <- 0
					out <- data.frame(Species,Group, Count, Percent, stringsAsFactors=FALSE)
					row.names(out) <- ""
				} else {
				
					spec.nums.oil <- tapply(X=alldat2$year, INDEX=alldat2$Species, FUN=length)
					spec.nums.oil <- spec.nums.oil[order(-spec.nums.oil)]
					outdat <- data.frame(Species=names(spec.nums.oil), Count=spec.nums.oil, stringsAsFactors=FALSE)
					outdat$Percent <- round(100*outdat$Count/sum(outdat$Count),2)
				
					outdat$Group <- species.index$group_lab[match(outdat$Species, species.index$species_name)]
					outdat <- outdat[, c(1,4,2,3)]
				
					out <- outdat
				
					ad.df <- out[1,]
					ad.df$Species[1] <- "ALL BIRDS"
					ad.df$Group[1] <- "ALL"
					ad.df$Count[1] <- sum(out$Count)
					ad.df$Percent[1] <- 100
					
					out <- rbind(ad.df,out)
					row.names(out) <- c("",as.character(1:(dim(out)[1]-1)))
				}
			}
		} else {
			yearselect <- input$YearOil
			if(yearselect=="All"){
				
				alldat2 <- ctot[ctot$is_survey & !is.na(ctot$is_survey) & ctot$is_bird & !is.na(ctot$is_bird) & ctot$refound==FALSE & !is.na(ctot$refound) & ctot$oil & !is.na(ctot$oil),]
				alldat2$Species[alldat2$Species=="*"] <- "Unknown*"
				spec.nums.oil <- tapply(X=alldat2$year, INDEX=alldat2$beach_name, FUN=length)
				spec.nums.oil <- spec.nums.oil[order(-spec.nums.oil)]
				outdat <- data.frame(Beach=names(spec.nums.oil), Count=spec.nums.oil, stringsAsFactors=FALSE)
				outdat$Percent <- round(100*outdat$Count/sum(outdat$Count),2)
				
				outdat$Region <- ctot$reg.vis[match(outdat$Beach, ctot$beach_name)]
				outdat <- outdat[, c(1,4,2,3)]
				
				out <- outdat
				ad.df <- out[1,]
				ad.df$Beach[1] <- "ALL BEACHES"
				ad.df$Region[1] <- "ALL"
				ad.df$Count[1] <- sum(out$Count)
				ad.df$Percent[1] <- 100
					
				out <- rbind(ad.df,out)
				row.names(out) <- c("",as.character(1:(dim(out)[1]-1)))
			
			} else {
				
				Yearnumer <- as.numeric(yearselect)
				alldat2 <- ctot[ctot$is_survey & !is.na(ctot$is_survey) & ctot$is_bird & !is.na(ctot$is_bird) & ctot$refound==FALSE & !is.na(ctot$refound) & ctot$oil & !is.na(ctot$oil) & ctot$year==Yearnumer,]
				alldat2$Species[alldat2$Species=="*"] <- "Unknown*"
				
				if(dim(alldat2)[1]==0){
					Beach <- "ALL BEACHES"
					Region <- "ALL"
					Count <- 0
					Percent <- 0
					out <- data.frame(Beach,Region, Count, Percent, stringsAsFactors=FALSE)
					row.names(out) <- ""
				} else {
				
					spec.nums.oil <- tapply(X=alldat2$year, INDEX=alldat2$beach_name, FUN=length)
					spec.nums.oil <- spec.nums.oil[order(-spec.nums.oil)]
					outdat <- data.frame(Beach=names(spec.nums.oil), Count=spec.nums.oil, stringsAsFactors=FALSE)
					outdat$Percent <- round(100*outdat$Count/sum(outdat$Count),2)
				
					outdat$Region <- ctot$reg.vis[match(outdat$Beach, ctot$beach_name)]
					outdat <- outdat[, c(1,4,2,3)]
				
					out <- outdat
				
					ad.df <- out[1,]
					ad.df$Beach[1] <- "ALL BEACHES"
					ad.df$Region[1] <- "ALL"
					ad.df$Count[1] <- sum(out$Count)
					ad.df$Percent[1] <- 100
					
					out <- rbind(ad.df,out)
					row.names(out) <- c("",as.character(1:(dim(out)[1]-1)))
				}
			}
		}
		datatable(out, rownames=FALSE)
	})
	
	#*********************************************************************************************#
	
	###############################################################################################
	# Entangled birds
	
	output$ex4 <- DT::renderDataTable({
		
		spec_beach <- input$Beach_Species_entangle
		ctot$Entanglebin <- array(0, dim=c(dim(ctot)[1]))
		ctot$Entanglebin[ctot$entangled %in% c("Not", "", NA)==FALSE] <- 1
		endat <- ctot[ctot$is_survey & !is.na(ctot$is_survey) & ctot$is_bird & !is.na(ctot$is_bird) & ctot$refound==FALSE & !is.na(ctot$refound) & ctot$Entanglebin == 1,]
		endat$Species[endat$Species=="*"] <- "Unknown*"
		
		if(spec_beach=="Species"){
			yearselect <- input$YearEntangle
			if(yearselect=="All"){
				unispec <- unique(endat$Species)
				fl.vec <- array(0, dim=c(length(unispec)))
				ne.vec <- array(0, dim=c(length(unispec)))
				ho.vec <- array(0, dim=c(length(unispec)))
				pl.vec <- array(0, dim=c(length(unispec)))
				ot.vec <- array(0, dim=c(length(unispec)))
				to.vec <- array(0, dim=c(length(unispec)))
				
				for(i in 1:length(unispec)){
					fl.vec[i] <- length(endat$year[endat$Species==unispec[i] & endat$entangled=="Fishing Line"])
					ne.vec[i] <- length(endat$year[endat$Species==unispec[i] & endat$entangled=="Net"])
					ho.vec[i] <- length(endat$year[endat$Species==unispec[i] & endat$entangled=="Hook"])
					pl.vec[i] <- length(endat$year[endat$Species==unispec[i] & endat$entangled=="Plastic"])
					ot.vec[i] <- length(endat$year[endat$Species==unispec[i] & endat$entangled=="Other Man-Made Substance"])
					to.vec[i] <- length(endat$year[endat$Species==unispec[i]])
				}
				
				odat <- data.frame(Species=unispec, Line=fl.vec, Net=ne.vec, Hook=ho.vec, Plastic=pl.vec, Other=ot.vec, Total=to.vec, stringsAsFactors=FALSE)
				odat <- odat[order(-odat$Total),]
				
				odat$Group <- species.index$group_lab[match(odat$Species, species.index$species_name)]
				odat <- odat[, c(1,8,2:7)]
				
				ad.df <- odat[1,]
				ad.df$Species[1] <- "ALL BIRDS"
				ad.df$Group[1] <- "ALL"
				ad.df$Line[1] <- sum(odat$Line)
				ad.df$Net[1] <- sum(odat$Net)
				ad.df$Hook[1] <- sum(odat$Hook)
				ad.df$Plastic[1] <- sum(odat$Plastic)
				ad.df$Other[1] <- sum(odat$Other)
				ad.df$Total[1] <- sum(odat$Total)
				odat <- rbind(ad.df,odat)
				row.names(odat) <- c("",as.character(1:(dim(odat)[1]-1)))
			
			} else {
				Yearnumer <- as.numeric(yearselect)
				endat <- endat[endat$year==Yearnumer,]
				if(dim(endat)[1]==0){
					Species <- "ALL BIRDS"
					Group <- "ALL"
					odat <- data.frame(Species, Group, Line=0, Net=0, Hook=0, Plastic=0, Other=0, Total=0, stringsAsFactors=FALSE)
					row.names(odat) <- ""
				} else {
				
					unispec <- unique(endat$Species)
					fl.vec <- array(0, dim=c(length(unispec)))
					ne.vec <- array(0, dim=c(length(unispec)))
					ho.vec <- array(0, dim=c(length(unispec)))
					pl.vec <- array(0, dim=c(length(unispec)))
					ot.vec <- array(0, dim=c(length(unispec)))
					to.vec <- array(0, dim=c(length(unispec)))
				
					for(i in 1:length(unispec)){
						fl.vec[i] <- length(endat$year[endat$Species==unispec[i] & endat$entangled=="Fishing Line"])
						ne.vec[i] <- length(endat$year[endat$Species==unispec[i] & endat$entangled=="Net"])
						ho.vec[i] <- length(endat$year[endat$Species==unispec[i] & endat$entangled=="Hook"])
						pl.vec[i] <- length(endat$year[endat$Species==unispec[i] & endat$entangled=="Plastic"])
						ot.vec[i] <- length(endat$year[endat$Species==unispec[i] & endat$entangled=="Other Man-Made Substance"])
						to.vec[i] <- length(endat$year[endat$Species==unispec[i]])
					}
				
					odat <- data.frame(Species=unispec, Line=fl.vec, Net=ne.vec, Hook=ho.vec, Plastic=pl.vec, Other=ot.vec, Total=to.vec, stringsAsFactors=FALSE)
					odat <- odat[order(-odat$Total),]
					
					odat$Group <- species.index$group_lab[match(odat$Species, species.index$species_name)]
					odat <- odat[, c(1,8,2:7)]
				
					ad.df <- odat[1,]
					ad.df$Species[1] <- "ALL BIRDS"
					ad.df$Group[1] <- "ALL"
					ad.df$Line[1] <- sum(odat$Line)
					ad.df$Net[1] <- sum(odat$Net)
					ad.df$Hook[1] <- sum(odat$Hook)
					ad.df$Plastic[1] <- sum(odat$Plastic)
					ad.df$Other[1] <- sum(odat$Other)
					ad.df$Total[1] <- sum(odat$Total)
					odat <- rbind(ad.df,odat)
					row.names(odat) <- c("",as.character(1:(dim(odat)[1]-1)))
				}
			}
		} else {
			yearselect <- input$YearEntangle
			if(yearselect=="All"){
				unispec <- unique(endat$beach_name)
				fl.vec <- array(0, dim=c(length(unispec)))
				ne.vec <- array(0, dim=c(length(unispec)))
				ho.vec <- array(0, dim=c(length(unispec)))
				pl.vec <- array(0, dim=c(length(unispec)))
				ot.vec <- array(0, dim=c(length(unispec)))
				to.vec <- array(0, dim=c(length(unispec)))
				
				for(i in 1:length(unispec)){
					fl.vec[i] <- length(endat$year[endat$beach_name==unispec[i] & endat$entangled=="Fishing Line"])
					ne.vec[i] <- length(endat$year[endat$beach_name==unispec[i] & endat$entangled=="Net"])
					ho.vec[i] <- length(endat$year[endat$beach_name==unispec[i] & endat$entangled=="Hook"])
					pl.vec[i] <- length(endat$year[endat$beach_name==unispec[i] & endat$entangled=="Plastic"])
					ot.vec[i] <- length(endat$year[endat$beach_name==unispec[i] & endat$entangled=="Other Man-Made Substance"])
					to.vec[i] <- length(endat$year[endat$beach_name==unispec[i]])
				}
				
				odat <- data.frame(Beach=unispec, Line=fl.vec, Net=ne.vec, Hook=ho.vec, Plastic=pl.vec, Other=ot.vec, Total=to.vec, stringsAsFactors=FALSE)
				odat <- odat[order(-odat$Total),]
				
				odat$Region <- ctot$reg.vis[match(odat$Beach, ctot$beach_name)]
				odat <- odat[, c(1,8,2:7)]
				
				ad.df <- odat[1,]
				ad.df$Beach[1] <- "ALL BEACHES"
				ad.df$Region[1] <- "ALL"
				ad.df$Line[1] <- sum(odat$Line)
				ad.df$Net[1] <- sum(odat$Net)
				ad.df$Hook[1] <- sum(odat$Hook)
				ad.df$Plastic[1] <- sum(odat$Plastic)
				ad.df$Other[1] <- sum(odat$Other)
				ad.df$Total[1] <- sum(odat$Total)
				odat <- rbind(ad.df,odat)
				row.names(odat) <- c("",as.character(1:(dim(odat)[1]-1)))
			
			} else {
			
				Yearnumer <- as.numeric(yearselect)
				endat <- endat[endat$year==Yearnumer,]
				if(dim(endat)[1]==0){
					Beach <- "ALL BEACHES"
					Region <- "ALL"
					odat <- data.frame(Beach, Region,Line=0, Net=0, Hook=0, Plastic=0, Other=0, Total=0, stringsAsFactors=FALSE)
					row.names(odat) <- ""
				} else {
				
					unispec <- unique(endat$beach_name)
					fl.vec <- array(0, dim=c(length(unispec)))
					ne.vec <- array(0, dim=c(length(unispec)))
					ho.vec <- array(0, dim=c(length(unispec)))
					pl.vec <- array(0, dim=c(length(unispec)))
					ot.vec <- array(0, dim=c(length(unispec)))
					to.vec <- array(0, dim=c(length(unispec)))
				
					for(i in 1:length(unispec)){
						fl.vec[i] <- length(endat$year[endat$beach_name==unispec[i] & endat$entangled=="Fishing Line"])
						ne.vec[i] <- length(endat$year[endat$beach_name==unispec[i] & endat$entangled=="Net"])
						ho.vec[i] <- length(endat$year[endat$beach_name==unispec[i] & endat$entangled=="Hook"])
						pl.vec[i] <- length(endat$year[endat$beach_name==unispec[i] & endat$entangled=="Plastic"])
						ot.vec[i] <- length(endat$year[endat$beach_name==unispec[i] & endat$entangled=="Other Man-Made Substance"])
						to.vec[i] <- length(endat$year[endat$beach_name==unispec[i]])
					}
				
					odat <- data.frame(Beach=unispec, Line=fl.vec, Net=ne.vec, Hook=ho.vec, Plastic=pl.vec, Other=ot.vec, Total=to.vec, stringsAsFactors=FALSE)
					odat <- odat[order(-odat$Total),]
					
					odat$Region <- ctot$reg.vis[match(odat$Beach, ctot$beach_name)]
					odat <- odat[, c(1,8,2:7)]
					
					ad.df <- odat[1,]
					ad.df$Beach[1] <- "ALL BEACHES"
					ad.df$Region[1] <- "ALL"
					ad.df$Line[1] <- sum(odat$Line)
					ad.df$Net[1] <- sum(odat$Net)
					ad.df$Hook[1] <- sum(odat$Hook)
					ad.df$Plastic[1] <- sum(odat$Plastic)
					ad.df$Other[1] <- sum(odat$Other)
					ad.df$Total[1] <- sum(odat$Total)
					odat <- rbind(ad.df,odat)
					row.names(odat) <- c("",as.character(1:(dim(odat)[1]-1)))
				}
			}
		}
		datatable(odat, rownames=FALSE)
	})
	
	#*********************************************************************************************#

})





 
