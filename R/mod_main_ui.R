#' main UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_main_ui <- function(id){
  ns <- NS(id)
  ## main dashboard
  bs4Dash::dashboardPage(
  	dark = FALSE,
  	help = TRUE,
  	fullscreen = TRUE,
  	scrollToTop = TRUE,
  	skin = "light",
  	header = bs4Dash::dashboardHeader(
  		skin = "light",
  		fixed = TRUE,
  		title = bs4Dash::dashboardBrand(
  			title = "mBudget",
  			href = "https://myominnoo.github.io/",
  			image = "www/logo.png",
  			opacity = 0.8
  		)
  	),

  	sidebar = bs4Dash::dashboardSidebar(
  		fixed = TRUE,
  		skin = "light",
  		id = "sidebar",

  		# bs4Dash::sidebarUserPanel(
  		# 	name = "Welcome Onboard!"
  		# ),
  		bs4Dash::sidebarMenu(
  			id = "current_tab",
  			flat = FALSE,
  			compact = FALSE,
  			childIndent = TRUE,
  			bs4Dash::menuItem(
  				"Home",
  				tabName = ns("tab_home"),
  				icon = icon("home")
  			),
  			bs4Dash::sidebarHeader("Transactions"),
  			bs4Dash::menuItem(
  				"Expense",
  				tabName = ns("tab_expense"),
  				icon = icon("money-bill")
  			),
  			bs4Dash::menuItem(
  				"Income",
  				tabName = ns("tab_income"),
  				icon = icon("wallet")
  			),
  			bs4Dash::menuItem(
  				"Savings",
  				tabName = ns("tab_savings"),
  				icon = icon("piggy-bank")
  			),

  			bs4Dash::sidebarHeader(""),

  			bs4Dash::menuItem(
  				"Settings",
  				tabName = ns("tab_settings"),
  				icon = icon("sliders")
  			),
  			bs4Dash::menuItem(
  				"Admin",
  				tabName = ns("tab_admin"),
  				icon = icon("screwdriver-wrench")
  			)
  		)

  	),
  	body = bs4Dash::dashboardBody(

  		bs4Dash::tabItems(
  			bs4Dash::tabItem(
  				tabName = ns("tab_home"), mod_home_ui(ns("home_1"))
  			),
  			bs4Dash::tabItem(
  				tabName = ns("tab_expense"), mod_expense_ui(ns("expense_1"))
  			),
  			bs4Dash::tabItem(
  				tabName = ns("tab_income"), mod_income_ui(ns("income_1"))
  			),
  			bs4Dash::tabItem(
  				tabName = ns("tab_savings"), mod_savings_ui(ns("savings_1"))
  			),
  			bs4Dash::tabItem(
  				tabName = ns("tab_settings"), mod_settings_ui(ns("settings_1"))
  			)
  		)
  	),
  	# controlbar = bs4Dash::dashboardControlbar(
  	# 	id = "controlbar",
  	# 	skin = "light",
  	# 	pinned = TRUE,
  	# 	overlay = FALSE,
  	# ),
  	footer = bs4Dash::dashboardFooter(
  		fixed = FALSE,
  		left = a(
  			href = "https://twitter.com/dr_myominnoo",
  			target = "_blank", "@dr_myominnoo"
  		),
  		right = "2023"
  	),
  	title = "bs4Dash Showcase"
  )
}



# ui helpers --------------------------------------------------------------




#' main Server Functions
#'
#' @noRd
mod_main_server <- function(id){

	## mongodb connection
	url <- mongodb_link
	con_tbl_expense <- mongolite::mongo("tbl_expense", "budgetApp", url)
	con_tbl_category <- mongolite::mongo("tbl_category", "budgetApp", url)
	con_tbl_pay_account <- mongolite::mongo("tbl_pay_account", "budgetApp", url)
	con_tbl_income <- mongolite::mongo("tbl_income", "budgetApp", url)
	con_tbl_savings <- mongolite::mongo("tbl_savings", "budgetApp", url)

	tbl_category <- get_all_fields(con_tbl_category, username = "admin")
	tbl_pay_account <- get_all_fields(con_tbl_pay_account, username = "admin")
	tbl_expense <- get_all_fields(con_tbl_expense, username = "admin") |>
		format_tbl_expense(tbl_category$cat_description, tbl_pay_account$pay_account)
	tbl_income <- get_all_fields(con_tbl_income, username = "admin") |>
		format_tbl_income(tbl_pay_account$pay_account)
	tbl_savings <- get_all_fields(con_tbl_savings, username = "admin") |>
		format_tbl_savings(tbl_pay_account$pay_account)

	moduleServer( id, function(input, output, session){
		ns <- session$ns
		bs4Dash::useAutoColor()

		env <- shiny::reactiveValues(
			user = shinyAuthX::create_dummy_users() |>
				dplyr::filter(username == "admin"),
			con_tbl_expense = con_tbl_expense,
			tbl_expense = tbl_expense,
			con_tbl_category = con_tbl_category,
			tbl_category = tbl_category,
			con_tbl_pay_account = con_tbl_pay_account,
			tbl_pay_account = tbl_pay_account,
			con_tbl_income = con_tbl_income,
			tbl_income = tbl_income,
			con_tbl_savings = con_tbl_savings,
			tbl_savings = tbl_savings
		)
		mod_home_server("home_1", env)
		mod_expense_server("expense_1", env)
		mod_income_server("income_1", env)
		mod_settings_server("settings_1", env)
		mod_savings_server("savings_1", env)
	})
}

## To be copied in the UI
# mod_main_ui("main_1")

## To be copied in the server
# mod_main_ui_server("main_1")
