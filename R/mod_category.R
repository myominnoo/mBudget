#' category UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_category_ui <- function(id){
  ns <- NS(id)
  tagList(

  	bs4Dash::box(
  		title = "Expense Category",
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
  						5,
  						shiny::textInput(
  							ns("description"), "Description", width = "100%"
  						)
  					),

  					shiny::column(
  						3,
  						shiny::numericInput(
  							ns("allocated_amount"), "Allocated Amount", 0, 0,
  							step = 5, width = "100%"
  						)
  					),

  					shiny::column(
  						4,
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
  		shiny::div(style="text-align:center;", shiny::uiOutput(ns("tbl_instruct")))

  	)
  )
}

#' category Server Functions
#'
#' @noRd
mod_category_server <- function(id, env){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$tbl_instruct <- shiny::renderUI({
    	if (nrow(env$tbl_category) > 0) {
    		shiny::p("To update, double-click a cell and edit.")
    	} else {
    		tagList(
    			shiny::p("To add a new record, choose 'Add New' from the dropdown menu."),
    			shiny::p("To reset to default, choose 'Reset' from the dropdown menu."),
    		)
    	}
    })

    ## show table
    output$show_tbl <- DT::renderDT({
    	if (nrow(env$tbl_category) > 0) {
    		env$tbl_category <- env$tbl_category |>
    			dplyr::arrange(cat_description)
    		env$tbl_category |>
    			dplyr::select(cat_description:cat_note) |>
    			DT::datatable(
    				rownames = FALSE,
    				colnames = c("Description" = "cat_description",
    										 "Allocated Amount" = "cat_allocated_amount",
    										 "Note" = "cat_note"),
    				editable = TRUE,
    				selection = "multiple",
    				filter = list(position = "top", clear = FALSE),
    				options = list(scrollX = TRUE)
    			)|>
    			DT::formatStyle(columns = c(1), width='200px') |>
    			DT::formatStyle(columns = c(2), width='100px') |>
    			DT::formatStyle(columns = c(3), width='800px')
    	} else {
    		NULL
    	}
    })


    ## delete event
    observeEvent(input$btn_delete, {
    	rows <- input$show_tbl_rows_selected
    	ids <- env$tbl_category[rows, "_id"]

    	tryCatch({
    		remove_rows_by_id(env$con_tbl_category, ids)
    		env$tbl_category <- get_all_fields(env$con_tbl_category,
    																			 username = env$user$username)
    		shinyWidgets::show_toast(sprintf("Deleted %s Records!", length(ids)),
    														 type = "success")
    	}, error = function(err) {
    		shinyWidgets::show_toast("Failed to delete records!", type = "error")
    	})
    })


    ## reset event
    observeEvent(input$btn_reset, {
    	ids <- env$tbl_category$`_id`
    	tryCatch({
    		if (!is.null(ids)) remove_rows_by_id(env$con_tbl_category, ids)

    		dummy_tbl_cat |>
    			dplyr::mutate(username = env$user$username) |>
    			env$con_tbl_category$insert()

    		env$tbl_category <- get_all_fields(env$con_tbl_category,
    																			 username = env$user$username)
    		env$tbl_expense <- get_all_fields(env$con_tbl_expense,
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
    	shiny::updateTextInput(session, "description", value = "")
    	shiny::updateTextInput(session, "note", value = "")
    	shiny::updateNumericInput(session, "allocated_amount", value = 0)
    })

    shiny::observeEvent(input$btn_add_new, {
    	if (input$description == "" | input$allocated_amount == "") {
    		shinyWidgets::show_toast("Empty values!", type = "error")
    	} else {
    		tryCatch({
    			tibble::tibble(
    				username = env$user$username,
    				cat_description = input$description,
    				cat_allocated_amount = input$allocated_amount,
    				cat_note = input$note
    			) |> env$con_tbl_category$insert()

    			env$tbl_category <- get_all_fields(env$con_tbl_category,
    																				 username = env$user$username)

    			## reset form
    			shinyjs::toggle(id = "div-add-new")
    			shiny::updateTextInput(session, "description", value = "")
    			shiny::updateTextInput(session, "note", value = "")
    			shiny::updateNumericInput(session, "allocated_amount", value = 0)

    			shinyWidgets::show_toast("Added a new category!", type = "success")
    		}, error = function(err) {
    			shinyWidgets::show_toast("Failed to add new category!", type = "error")
    		})
    	}
    })

    ## edit event
    observeEvent(input$show_tbl_cell_edit, {
    	edit <- input$show_tbl_cell_edit
    	df <- env$tbl_category[edit$row, ]
    	tryCatch({
    		remove_rows_by_id(env$con_tbl_category, df[["_id"]])

    		col_name <- names(df)[edit$col+3]
    		## syn change between category and expense tables
    		if (col_name == "cat_description" & nrow(env$tbl_expense) > 0) {
    			update_by_ids(
    				con = con_tbl_expense,
    				username = env$user$username,
    				var_query = "exp_category",
    				val_query = df[[col_name]],
    				var_update = "exp_category",
    				val_update = edit$value
    			)
    		}

    		df[[col_name]] <- edit$value

    		df |>
    			dplyr::select(-`_id`) |>
    			env$con_tbl_category$insert()

    		env$tbl_category <- get_all_fields(env$con_tbl_category,
    																			 username = env$user$username)
    		env$tbl_expense <- get_all_fields(env$con_tbl_expense,
    																			username = env$user$username)

    		shinyWidgets::show_toast("Updated success", type = "success")
    	}, error = function(err) {
    		shinyWidgets::show_toast("Failed to update!", type = "error")
    	})
    })

  })
}

## To be copied in the UI
# mod_category_ui("category_1")

## To be copied in the server
# mod_category_server("category_1", env)
