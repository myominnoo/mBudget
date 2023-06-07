#' home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_home_ui <- function(id){
  ns <- NS(id)
  tagList(


  	shiny::fluidRow(
  		bs4Dash::box(
  			width = 3, status = "lightblue", collapsible = FALSE, solidHeader = TRUE,
  			title = shiny::h3(tagList(shiny::icon("money-check-dollar"), "Balance"),
  												style = "text-align:center;"),
  			shiny::h2(shiny::uiOutput(ns("txt_balance")), style = "text-align:center;")
  		),

  		bs4Dash::box(
  			width = 3, status = "danger", collapsible = FALSE, solidHeader = TRUE,
  			title = shiny::h3(tagList(shiny::icon("credit-card"), "Expense"),
  												style = "text-align:center;"),
  			shiny::h2(shiny::uiOutput(ns("txt_expense")), style = "text-align:center;")
  		),

  		bs4Dash::box(
  			width = 3, status = "warning", collapsible = FALSE, solidHeader = TRUE,
  			title = shiny::h3(tagList(shiny::icon("cash-register"), "Income"),
  												style = "text-align:center;"),
  			shiny::h2(shiny::uiOutput(ns("txt_income")), style = "text-align:center;")
  		),

  		bs4Dash::box(
  			width = 3, status = "success", collapsible = FALSE, solidHeader = TRUE,
  			title = shiny::h3(tagList(shiny::icon("piggy-bank"), "Savings"),
  												style = "text-align:center;"),
  			shiny::h2(shiny::uiOutput(ns("txt_savings")), style = "text-align:center;")
  		)
  	),

  	bs4Dash::box(
  		title = "Filter",
  		width = 12,
  		status = "primary",
  		solidHeader = TRUE,
  		collapsible = TRUE,
  		collapsed = FALSE,


  		shiny::fluidRow(
  			shiny::column(
  				8,
  				shiny::dateRangeInput(
  					ns("date"), "Date:",
  					start = lubridate::floor_date(Sys.Date(), unit = "month"),
  					end = lubridate::ceiling_date(Sys.Date(), unit = "month") -1
  				)
  			),
  			shiny::column(
  				2, offset = 2, br(),
  				shiny::actionButton(ns("btn_filter"), "Filter", shiny::icon("filter"),
  														width = "100%", class = "btn-primary")
  			)
  		)
  	),

  	bs4Dash::box(
  		title = "Summary",
  		width = 12,
  		status = "primary",
  		solidHeader = TRUE,
  		collapsible = TRUE,
  		plotOutput(ns("fig_summary"))
  	),

  	bs4Dash::box(
  		title = "Expense",
  		width = 12,
  		status = "primary",
  		solidHeader = TRUE,
  		collapsible = TRUE,

  		shiny::fluidRow(
  			shiny::column(
  				6,
  				plotOutput(ns("fig_exp_category"))
  			),
  			shiny::column(
  				6,
  				plotOutput(ns("fig_exp_category_by_pay_account"))
  			)
  		)
  	),

  	bs4Dash::box(
  		title = "Income",
  		width = 12,
  		status = "primary",
  		solidHeader = TRUE,
  		collapsible = TRUE,

  		shiny::fluidRow(
  			shiny::column(
  				6,
  				plotOutput(ns("fig_inc_category"))
  			),
  			shiny::column(
  				6,
  				plotOutput(ns("fig_inc_category_by_pay_account"))
  			)
  		)
  	),

  	bs4Dash::box(
  		title = "Savings",
  		width = 12,
  		status = "primary",
  		solidHeader = TRUE,
  		collapsible = TRUE,

  		shiny::fluidRow(
  			shiny::column(
  				6,
  				plotOutput(ns("fig_sav_category"))
  			),
  			shiny::column(
  				6,
  				plotOutput(ns("fig_sav_category_by_pay_account"))
  			)
  		)
  	)

  )
}

#' home Server Functions
#'
#' @noRd
mod_home_server <- function(id, env){

  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observe({

    	## calculation
    	df_expense <- env$tbl_expense |> dplyr::filter(username == env$user$username)
    	df_income <- env$tbl_income |> dplyr::filter(username == env$user$username)
    	df_savings <- env$tbl_savings |> dplyr::filter(username == env$user$username)

    	ttl_exp_amount <- df_expense$exp_amount |> sum(na.rm = TRUE)
    	ttl_inc_amount <- df_income$inc_amount |> sum(na.rm = TRUE)
    	ttl_sav_amount <- df_savings$sav_amount |> sum(na.rm = TRUE)

    	balance <- ttl_inc_amount + ttl_sav_amount - ttl_exp_amount

    	output$txt_balance <- shiny::renderUI(scales::label_comma(prefix = "$ ")(balance))
    	output$txt_expense <- shiny::renderUI(scales::label_comma(prefix = "$ ")(ttl_exp_amount))
    	output$txt_income <- shiny::renderUI(scales::label_comma(prefix = "$ ")(ttl_inc_amount))
    	output$txt_savings <- shiny::renderUI(scales::label_comma(prefix = "$ ")(ttl_sav_amount))

    	output$fig_summary <- shiny::renderPlot({
    		tryCatch(
    			get_summary_by_type(env$tbl_expense, env$tbl_income, env$tbl_savings,
    													shiny::isolate(input$date[1]),
    													shiny::isolate(input$date[2])),
    			error = function(err) ggplot2::ggplot()
    		)
    	})
    	output$fig_exp_category <- shiny::renderPlot({
    		tryCatch(
    			get_exp_summary(env$tbl_expense, shiny::isolate(input$date[1]),
    											shiny::isolate(input$date[2])),
    			error = function(err) ggplot2::ggplot()
    		)
    	})
    	output$fig_exp_category_by_pay_account <- shiny::renderPlot({
    		tryCatch(
    			get_exp_summary_by_pay_account(env$tbl_expense,
    																		 shiny::isolate(input$date[1]),
    																		 shiny::isolate(input$date[2])),
    			error = function(err) ggplot2::ggplot()
    		)
    	})


    	output$fig_inc_category <- shiny::renderPlot({
    		tryCatch(
    			get_inc_summary(env$tbl_income, shiny::isolate(input$date[1]),
    											shiny::isolate(input$date[2])),
    			error = function(err) ggplot2::ggplot()
    		)
    	})
    	output$fig_inc_category_by_pay_account <- shiny::renderPlot({
    		tryCatch(
    			get_inc_summary_by_pay_account(env$tbl_income,
    																		 shiny::isolate(input$date[1]),
    																		 shiny::isolate(input$date[2])),
    			error = function(err) ggplot2::ggplot()
    		)
    	})


    	output$fig_sav_category <- shiny::renderPlot({
    		tryCatch(
    			get_sav_summary(env$tbl_savings, shiny::isolate(input$date[1]),
    											shiny::isolate(input$date[2])),
    			error = function(err) ggplot2::ggplot()
    		)
    	})
    	output$fig_sav_category_by_pay_account <- shiny::renderPlot({
    		tryCatch(
    			get_sav_summary_by_pay_account(env$tbl_savings,
    																		 shiny::isolate(input$date[1]),
    																		 shiny::isolate(input$date[2])),
    			error = function(err) ggplot2::ggplot()
    		)
    	})
    })

    ## filter event
    observeEvent(input$btn_filter, {

    	output$fig_summary <- shiny::renderPlot({
    		tryCatch(
    			get_summary_by_type(env$tbl_expense, env$tbl_income, env$tbl_savings,
    													shiny::isolate(input$date[1]),
    													shiny::isolate(input$date[2])),
    			error = function(err) ggplot2::ggplot()
    		)
    	})
    	output$fig_exp_category <- shiny::renderPlot({
    		tryCatch(
    			get_exp_summary(env$tbl_expense, shiny::isolate(input$date[1]),
    											shiny::isolate(input$date[2])),
    			error = function(err) ggplot2::ggplot()
    		)
    	})
    	output$fig_exp_category_by_pay_account <- shiny::renderPlot({
    		tryCatch(
    			get_exp_summary_by_pay_account(env$tbl_expense,
    																		 shiny::isolate(input$date[1]),
    																		 shiny::isolate(input$date[2])),
    			error = function(err) ggplot2::ggplot()
    		)
    	})

    	output$fig_inc_category <- shiny::renderPlot({
    		tryCatch(
    			get_inc_summary(env$tbl_income, shiny::isolate(input$date[1]),
    											shiny::isolate(input$date[2])),
    			error = function(err) ggplot2::ggplot()
    		)
    	})
    	output$fig_inc_category_by_pay_account <- shiny::renderPlot({
    		tryCatch(
    			get_inc_summary_by_pay_account(env$tbl_income,
    																		 shiny::isolate(input$date[1]),
    																		 shiny::isolate(input$date[2])),
    			error = function(err) ggplot2::ggplot()
    		)
    	})


    	output$fig_sav_category <- shiny::renderPlot({
    		tryCatch(
    			get_sav_summary(env$tbl_savings, shiny::isolate(input$date[1]),
    											shiny::isolate(input$date[2])),
    			error = function(err) ggplot2::ggplot()
    		)
    	})
    	output$fig_sav_category_by_pay_account <- shiny::renderPlot({
    		tryCatch(
    			get_sav_summary_by_pay_account(env$tbl_savings,
    																		 shiny::isolate(input$date[1]),
    																		 shiny::isolate(input$date[2])),
    			error = function(err) ggplot2::ggplot()
    		)
    	})
    })


  })
}

## To be copied in the UI
# mod_home_ui("home_1")

## To be copied in the server
# mod_home_server("home_1")
