#' income UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_income_ui <- function(id){
  ns <- NS(id)
  tagList(

  	bs4Dash::box(
  		title = "Income",
  		maximizable = TRUE,
  		width = 12,
  		status = "warning",
  		solidHeader = TRUE,
  		collapsible = FALSE,

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
  				"Import", id = ns("btn_show_import"),
  				icon =  shiny::icon("file-import")
  			)
  		),


  		shinyjs::hidden(
  			shiny::div(
  				id = ns("div-add-new"),
  				shiny::fluidRow(
  					shiny::column(
  						3,
  						shiny::dateInput(ns("date"), "Date", width = "100%")
  					),
  					shiny::column(
  						3,
  						shiny::selectInput(
  							ns("pay_account"), "Account", "", width = "100%"
  						)
  					),
  					shiny::column(
  						4,
  						shiny::textInput(
  							ns("description"), "Description", width = "100%"
  						)
  					),
  					shiny::column(
  						2,
  						shiny::numericInput(
  							ns("amount"), "Amount", 0, 0, step = 5, width = "100%"
  						)
  					)
  				),
  				shiny::fluidRow(
  					shiny::column(
  						8,
  						shiny::textInput(
  							ns("note"), "Note:", width = "100%"
  						)
  					),
  					shiny::column(
  						2, br(),
  						shiny::actionButton(ns("btn_cancel_add_new"), "Cancel", width = "100%")
  					),
  					shiny::column(
  						2, br(),
  						shiny::actionButton(ns("btn_add_new"), "Add",
  																width = "100%", class = "btn-primary")
  					)
  				),
  				hr()
  			)
  		),

  		shinyjs::hidden(
  			shiny::div(
  				id = ns("div-import"),
  				shiny::fileInput(ns("file_import"), "Choose a file:",
  												 accept = c(".xlsx", ".xls")),
  				shiny::fluidRow(
  					shiny::column(
  						4, offset = 4,
  						shiny::downloadButton(ns("btn_download_dummy"), "Download template",
  																	style = "width:100%", class = "btn-success")
  					),
  					shiny::column(
  						2,
  						shiny::actionButton(ns("btn_cancel_import"), "Cancel", width = "100%")
  					),
  					shiny::column(
  						2, shiny::actionButton(ns("btn_import"), "Import",
  																	 width = "100%", class = "btn-primary")
  					)
  				),
  				hr()
  			)
  		),

  		DT::dataTableOutput(ns("show_tbl")),
  		shiny::div(style="text-align:center;", shiny::uiOutput(ns("tbl_instruct")))

  	)

  )
}

#' income Server Functions
#'
#' @noRd
mod_income_server <- function(id, env){
	moduleServer( id, function(input, output, session){
		ns <- session$ns

		output$tbl_instruct <- shiny::renderUI({
			if (nrow(env$tbl_income) > 0) {
				shiny::p("To update, double-click a cell and edit.")
			} else {
				tagList(
					shiny::p("To add a new record, choose 'Add New' from the dropdown menu."),
					shiny::p("To import existing records, choose 'Import' from the dropdown menu."),
				)
			}
		})

		## show table
		output$show_tbl <- DT::renderDT({
			if (nrow(env$tbl_income) > 0) {
				env$tbl_income <- env$tbl_income |>
					format_tbl_income(env$tbl_pay_account$pay_account) |>
					dplyr::filter(!is.na(inc_date)) |>
					dplyr::arrange(dplyr::desc(inc_date))
				env$tbl_income |>
					dplyr::select(inc_date:inc_note) |>
					DT::datatable(
						rownames = FALSE,
						colnames = c(
							"Date" = "inc_date", "Account" = "inc_pay_account",
							"Description" = "inc_description",
							"Amount" = "inc_amount", "Note" = "inc_note"
						),
						editable = TRUE,
						selection = "multiple",
						filter = list(position = "top", clear = FALSE),
						options = list(scrollX = TRUE)
					) |>
					DT::formatStyle(columns = c(1,2,4), width='200px') |>
					DT::formatStyle(columns = c(3,5), width='700px')
			} else {
				NULL
			}
		})


		## delete event
		observeEvent(input$btn_delete, {
			rows <- input$show_tbl_rows_selected
			ids <- env$tbl_income[rows, "_id"]
			tryCatch({
				remove_rows_by_id(env$con_tbl_income, ids)
				env$tbl_income <- get_all_fields(env$con_tbl_income,
																				 username = env$user$username)
				shinyWidgets::show_toast(sprintf("Deleted %s Records!", length(ids)),
																 type = "success")
			}, error = function(err) {
				shinyWidgets::show_toast("Failed to delete records!", type = "error")
			})
		})

		## download event
		output$btn_download_dummy <- shiny::downloadHandler(
			filename = function() {
				sprintf(
					"template_income_%s.xlsx", format(Sys.time(), "%Y%m%d_%H%M%S")
				)
			},
			content = function(file) rio::export(dummy_tbl_income, file)
		)

		## import event
		shiny::observeEvent(input$btn_show_import, {
			shinyjs::toggle(id = "div-import")
		})
		shiny::observeEvent(input$btn_cancel_import, {
			shinyjs::toggle(id = "div-import")
		})

		shiny::observeEvent(input$btn_import, {
			shiny::req(input$file_import)
			tryCatch({
				df <- readxl::read_excel(input$file_import$datapath)

				## stop if colnames are wrong
				stopifnot("Wrong column names!" = all(names(df) %in% c(
					"username", "inc_date", "inc_pay_account", "inc_description",
					"inc_amount", "inc_note"
				)))

				df <- df |>
					format_tbl_income(env$tbl_pay_account$pay_account) |>
					dplyr::mutate(username = env$user$username)
				df |>
					env$con_tbl_income$insert()

				env$tbl_income <- get_all_fields(env$con_tbl_income,
																				 username = env$user$username)

				shinyWidgets::show_toast(sprintf("%s Records Imported!", nrow(df)),
																 type = "success")
			}, error = function(err) {
				shinyWidgets::show_toast("Failed to import!", type = "error")
			})
		})

		## add new event
		shiny::observeEvent(input$btn_show_add_new, {
			shinyjs::toggle(id = "div-add-new")
			shiny::updateSelectInput(
				session, "pay_account", choices = env$tbl_pay_account$pay_account
			)
		})

		shiny::observeEvent(input$btn_cancel_add_new, {
			shiny::updateDateInput(session, "date", value = Sys.Date())
			shiny::updateTextInput(session, "description", value = "")
			shiny::updateSelectInput(
				session, "pay_account", choices = env$tbl_pay_account$pay_account
			)
			shiny::updateNumericInput(session, "amount", value = 0)
			shiny::updateTextInput(session, "note", value = "")

			shinyjs::toggle(id = "div-add-new")
		})

		shiny::observeEvent(input$btn_add_new, {
			tryCatch({
				## write new record to mongodb
				dplyr::tibble(
					username = env$user$username,
					inc_date = input$date,
					inc_pay_account = input$pay_account,
					inc_description = input$description,
					inc_amount = input$amount,
					inc_note = input$note
				) |>
					format_tbl_income(env$tbl_pay_account$pay_account) |>
					env$con_tbl_income$insert()

				env$tbl_income <- get_all_fields(env$con_tbl_income,
																				 username = env$user$username)

				## reset form
				shiny::updateDateInput(session, "date", value = Sys.Date())
				shiny::updateTextInput(session, "description", value = "")
				shiny::updateSelectInput(
					session, "pay_account", choices = env$tbl_pay_account$pay_account
				)
				shiny::updateNumericInput(session, "amount", value = 0)
				shiny::updateTextInput(session, "note", value = "")

				shinyjs::toggle(id = "div-add-new")

				shinyWidgets::show_toast("Added a new record!", type = "success")
			}, error = function(err) {
				shinyWidgets::show_toast("Failed to add a new record!", type = "error")
			})
		})


		## edit event
		observeEvent(input$show_tbl_cell_edit, {
			edit <- input$show_tbl_cell_edit
			df <- env$tbl_income[edit$row, ]
			tryCatch({
				remove_rows_by_id(env$con_tbl_income, df[["_id"]])

				col_name <- names(df)[edit$col+3]
				df[[col_name]] <- edit$value
				df |>
					dplyr::select(-`_id`) |>
					env$con_tbl_income$insert()

				env$tbl_income <- get_all_fields(env$con_tbl_income,
																				 username = env$user$username)

				shinyWidgets::show_toast("Updated record!", type = "success")
			}, error = function(err) {
				shinyWidgets::show_toast("Failed to update!", type = "error")
			})
		})

	})
}

## To be copied in the UI
# mod_income_ui("income_1")

## To be copied in the server
# mod_income_server("income_1")
