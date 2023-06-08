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

			shinyjs::hidden(
				shiny::div(
					id = ns("dash-sidebar"),
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
						shinyjs::hidden(
							shiny::div(
								id = ns("admin-panel"),
								bs4Dash::menuItem(
									"Admin",
									tabName = ns("tab_admin"),
									icon = icon("screwdriver-wrench")
								)
							)
						)
					)
				)
			)

  	),
  	body = bs4Dash::dashboardBody(

  		# add signin panel UI function with signup panel
  		shinyAuthX::signinUI(id = ns("signin"),
  												 .add_forgotpw = TRUE,
  												 .add_btn_signup = TRUE),
  		# add signup panel
  		shinyAuthX::signupUI(id = ns("signup")),
  		# add password-reset panel
  		shinyAuthX::forgotpwUI(id = ns("pw-reset")),

  		shinyjs::hidden(
  			shiny::div(
  				id = ns("dash-body"),
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
  				),

  				shinymanager::fab_button(
  					position = "bottom-right",
  					shinyAuthX::signoutUI(id = ns("signout"), label = "Sign out",
  																shiny::icon("sign-out"))
  				)

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
  		left = "Version 0.1.0",
  		right = "Myo Minn Oo @ 2023"
  	),
  	title = "mBudget App: shinyAuthX Showcase"
  )
}




#' main Server Functions
#'
#' @noRd
mod_main_server <- function(id){

	## mongodb connection
	url <- mongodb_link
	con_tbl_user <- mongolite::mongo("tbl_user", "budgetApp", url)

	moduleServer( id, function(input, output, session){
		ns <- session$ns
		bs4Dash::useAutoColor()

		# call signin module supplying data frame,
		credentials <- shinyAuthX::signinServer(
			id = "signin",
			users_db = get_all_fields(con_tbl_user),
			sodium_hashed = TRUE,
			reload_on_signout = TRUE,
			signout = reactive(signout_init())
		)

		# call the signout module with reactive trigger to hide/show
		signout_init <- shinyAuthX::signoutServer(
			id = "signout",
			active = reactive(credentials()$user_auth)
		)

		# call signup module supplying credentials() reactive and mongodb
		shinyAuthX::signupServer(
			id = "signup", credentials = credentials, mongodb = con_tbl_user,
			email = template
		)
		# call password-reset module supplying credentials() reactive and mongodb
		shinyAuthX::forgotpwServer(
			id = "pw-reset", credentials = credentials, mongodb = con_tbl_user,
			email = template
		)

		observe({
			shinyjs::toggle("dash-header", condition = credentials()$user_auth)
			shinyjs::toggle("dash-sidebar", condition = credentials()$user_auth)
			shinyjs::toggle("dash-body", condition = credentials()$user_auth)
			shinyjs::toggle("admin-panel", condition = credentials()$info$permissions == "admin")


			## if credential is true, show UI
			if (credentials()$user_auth) {
				shinyjs::removeClass(selector = "body", class = "sidebar-collapse")

				## load mongodb databases
				con_tbl_expense <- mongolite::mongo("tbl_expense", "budgetApp", url)
				con_tbl_category <- mongolite::mongo("tbl_category", "budgetApp", url)
				con_tbl_pay_account <- mongolite::mongo("tbl_pay_account", "budgetApp", url)
				con_tbl_income <- mongolite::mongo("tbl_income", "budgetApp", url)
				con_tbl_savings <- mongolite::mongo("tbl_savings", "budgetApp", url)

				## retrieve data
				tbl_category <- get_all_fields(
					con_tbl_category, username = credentials()$info$username
				)
				tbl_pay_account <- get_all_fields(
					con_tbl_pay_account, username = credentials()$info$username
				)
				tbl_expense <- get_all_fields(
					con_tbl_expense, username = credentials()$info$username
				) |>
					format_tbl_expense(tbl_category$cat_description, tbl_pay_account$pay_account)
				tbl_income <- get_all_fields(
					con_tbl_income, username = credentials()$info$username
				) |>
					format_tbl_income(tbl_pay_account$pay_account)
				tbl_savings <- get_all_fields(
					con_tbl_savings, username = credentials()$info$username
				) |>
					format_tbl_savings(tbl_pay_account$pay_account)

				## create global reactive values
				env <- shiny::reactiveValues(
					user = credentials()$info,
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

				## call modules
				mod_home_server("home_1", env)
				mod_expense_server("expense_1", env)
				mod_income_server("income_1", env)
				mod_settings_server("settings_1", env)
				mod_savings_server("savings_1", env)

			} else {
				shinyjs::addClass(selector = "body", class = "sidebar-collapse")
			}
		})

	})
}

## To be copied in the UI
# mod_main_ui("main_1")

## To be copied in the server
# mod_main_ui_server("main_1")
