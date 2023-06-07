#' pay_account UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_pay_account_ui <- function(id){
  ns <- NS(id)
  tagList(

  	bs4Dash::box(
  		title = "Payment Account",
  		width = 12,
  		status = "primary",
  		solidHeader = TRUE,
  		collapsible = TRUE,
  		collapsed = TRUE,

  		dropdownMenu = bs4Dash::boxDropdown(
  			icon = shiny::icon("gear"),
  			bs4Dash::boxDropdownItem(
  				"Add New", id = ns("btn_show_add_new"),
  				icon = shiny::icon("plus")
  			),
  			bs4Dash::boxDropdownItem(
  				"Delete", id = ns("btn_delete"),
  				icon =  shiny::icon("xmark")
  			),
  			bs4Dash::boxDropdownItem(
  				"Reset", id = ns("btn_reset"),
  				icon =  shiny::icon("rotate-left")
  			)
  		),

  		shinyjs::hidden(
  			shiny::div(
  				id = ns("div-add-new"),
  				shiny::fluidRow(
  					shiny::column(
  						3,
  						shiny::textInput(
  							ns("account"), "Account", width = "100%"
  						)
  					),

  					shiny::column(
  						3,
  						shiny::textInput(
  							ns("type"), "Type", width = "100%"
  						)
  					),

  					shiny::column(
  						6,
  						shiny::textInput(
  							ns("note"), "Note", width = "100%"
  						)
  					)
  				),

  				shiny::fluidRow(
  					shiny::column(
  						2, offset = 8,
  						shiny::actionButton(ns("btn_cancel_add_new"), "Cancel", width = "100%")
  					),
  					shiny::column(
  						2, shiny::actionButton(ns("btn_add_new"), "Add",
  																	 width = "100%", class = "btn-primary")
  					)
  				),
  				hr()
  			)
  		),

  		DT::dataTableOutput(ns("show_tbl")),
  		shiny::div(style="text-align:center;",
  							 shiny::p("To update, double-click a cell and edit."))

  	)
  )
}

#' pay_account Server Functions
#'
#' @noRd
mod_pay_account_server <- function(id, env){
	moduleServer( id, function(input, output, session){
		ns <- session$ns

		## show table
		output$show_tbl <- DT::renderDT({
			if (nrow(env$tbl_pay_account) > 0) {
				env$tbl_pay_account <- env$tbl_pay_account |>
					dplyr::arrange(pay_account)
				env$tbl_pay_account |>
					dplyr::select(pay_account, pay_type, pay_note) |>
					DT::datatable(
						rownames = FALSE,
						colnames = c("Account" = "pay_account",
												 "Type" = "pay_type", "Note" = "pay_note"),
						editable = TRUE,
						selection = "multiple",
						filter = list(position = "top", clear = FALSE),
						options = list(scrollX = TRUE)
					)|>
					DT::formatStyle(columns = c(1, 2), width='200px') |>
					DT::formatStyle(columns = c(3), width='800px')
			} else {
				NULL
			}
		})


		## delete event
		observeEvent(input$btn_delete, {
			rows <- input$show_tbl_rows_selected
			ids <- env$tbl_pay_account[rows, "_id"]

			tryCatch({
				remove_rows_by_id(env$con_tbl_pay_account, ids)
				env$tbl_pay_account <- get_all_fields(env$con_tbl_pay_account,
																							username = env$user$username)
				shinyWidgets::show_toast(sprintf("Deleted %s Records!", length(ids)),
																 type = "success")
			}, error = function(err) {
				shinyWidgets::show_toast("Failed to delete records!", type = "error")
			})
		})

		## reset event
		observeEvent(input$btn_reset, {
			ids <- env$tbl_pay_account$`_id`
			tryCatch({
				if (!is.null(ids)) remove_rows_by_id(env$con_tbl_pay_account, ids)

				dummy_tbl_pay_account |>
					dplyr::mutate(username = env$user$username) |>
					env$con_tbl_pay_account$insert()

				env$tbl_pay_account <- get_all_fields(env$con_tbl_pay_account,
																							username = env$user$username)
				env$tbl_expense <- get_all_fields(env$con_tbl_expense,
																					username = env$user$username)
				env$tbl_income <- get_all_fields(env$con_tbl_income,
																				 username = env$user$username)

				shinyWidgets::show_toast("Reset Success", type = "success")
			}, error = function(err) {
				shinyWidgets::show_toast("Failed to reset!", type = "error")
			})
		})

		## add new event
		shiny::observeEvent(input$btn_show_add_new, {
			shinyjs::toggle(id = "div-add-new")
		})
		shiny::observeEvent(input$btn_cancel_add_new, {
			shinyjs::toggle(id = "div-add-new")
			shiny::updateTextInput(session, "account", value = "")
			shiny::updateTextInput(session, "type", value = "")
			shiny::updateTextInput(session, "note", value = "")
		})


		## add new event
		shiny::observeEvent(input$btn_add_new, {
			if (input$account == "" | input$type == "") {
				shinyWidgets::show_toast("Empty values!", type = "error")
			} else {
				tryCatch({
					tibble::tibble(
						username = env$user$username,
						pay_account = input$account,
						pay_type = input$type,
						pay_note = input$note
					) |> env$con_tbl_pay_account$insert()

					env$tbl_pay_account <- get_all_fields(env$con_tbl_pay_account,
																								username = env$user$username)

					## reset form
					shinyjs::toggle(id = "div-add-new")
					shiny::updateTextInput(session, "account", value = "")
					shiny::updateTextInput(session, "type", value = "")
					shiny::updateTextInput(session, "note", value = "")

					shinyWidgets::show_toast("Added a new payment account!", type = "success")
				}, error = function(err) {
					shinyWidgets::show_toast("Failed to add new account!", type = "error")
				})
			}
		})

		## edit event
		observeEvent(input$show_tbl_cell_edit, {
			edit <- input$show_tbl_cell_edit
			df <- env$tbl_pay_account[edit$row, ]
			tryCatch({
				remove_rows_by_id(env$con_tbl_pay_account, df[["_id"]])

				col_name <- names(df)[edit$col+3]
				## syn change between pay account and expense tables
				if (col_name == "pay_account" & nrow(env$tbl_pay_account) > 0) {
					update_by_ids(
						con = con_tbl_expense,
						username = env$user$username,
						var_query = "exp_pay_account",
						val_query = df[[col_name]],
						var_update = "exp_pay_account",
						val_update = edit$value
					)
					update_by_ids(
						con = con_tbl_income,
						username = env$user$username,
						var_query = "inc_pay_account",
						val_query = df[[col_name]],
						var_update = "inc_pay_account",
						val_update = edit$value
					)
					update_by_ids(
						con = con_tbl_savings,
						username = env$user$username,
						var_query = "sav_pay_account",
						val_query = df[[col_name]],
						var_update = "sav_pay_account",
						val_update = edit$value
					)
				}

				df[[col_name]] <- edit$value

				df |>
					dplyr::select(-`_id`) |>
					env$con_tbl_pay_account$insert()

				env$tbl_pay_account <- get_all_fields(env$con_tbl_pay_account,
																							username = env$user$username)
				env$tbl_expense <- get_all_fields(env$con_tbl_expense,
																					username = env$user$username)
				env$tbl_savings <- get_all_fields(env$con_tbl_savings,
																					username = env$user$username)
				env$tbl_income <- get_all_fields(env$con_tbl_income,
																				 username = env$user$username)

				shinyWidgets::show_toast("Updated success", type = "success")
			}, error = function(err) {
				shinyWidgets::show_toast("Failed to update!", type = "error")
			})
		})

	})
}

## To be copied in the UI
# mod_pay_account_ui("pay_account_1")

## To be copied in the server
# mod_pay_account_server("pay_account_1")
