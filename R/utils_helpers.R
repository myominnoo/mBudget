#' helpers
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
#'
#'
#'
#'
format_tbl_expense <- function(df, lvl_exp_category = NULL,
															 lvl_exp_pay_account = NULL)
{
	if (!nrow(df) > 0) {
		df <- dummy_tbl_exp[0, ]
	}

	df <- df |>
		dplyr::mutate(
			# exp_id = as.character(exp_id),
			username = as.character(username),
			exp_date = as.Date(exp_date),
			exp_category = as.character(exp_category),
			exp_description = as.character(exp_description),
			exp_pay_account = as.character(exp_pay_account),
			exp_amount = as.numeric(exp_amount),
			exp_note = as.character(exp_note))

	if (!is.null(lvl_exp_category)) {
		df <- df |>
			dplyr::mutate(exp_category = factor(
				exp_category, levels = lvl_exp_category
			))
	}

	if (!is.null(lvl_exp_pay_account)) {
		df <- df |>
			dplyr::mutate(exp_pay_account = factor(
				exp_pay_account, levels = lvl_exp_pay_account
			))
	}

	return(df)
}


format_tbl_income <- function(df, lvl_exp_pay_account = NULL)
{
	if (!nrow(df) > 0) {
		df <- dummy_tbl_income[0, ]
	}

	df <- df |>
		dplyr::mutate(
			inc_date = as.Date(inc_date),
			inc_pay_account = as.character(inc_pay_account),
			inc_description = as.character(inc_description),
			inc_amount = as.numeric(inc_amount),
			inc_note = as.character(inc_note)
		)
	if (!is.null(lvl_exp_pay_account)) {
		df <- df |>
			dplyr::mutate(
				inc_pay_account = factor(
					inc_pay_account, levels = lvl_exp_pay_account
				)
			)
	}
	return(df)
}


get_exp_summary <- function(df, start_date, end_date)
{
	df_summ <- df |>
		dplyr::filter(
			exp_date %within% lubridate::interval(start_date, end_date)
		) |>
		dplyr::group_by(exp_category) |>
		dplyr::reframe(
			sum = sum(exp_amount, na.rm = TRUE)
		) |>
		dplyr::ungroup() |>
		dplyr::mutate(
			exp_category = forcats::fct_infreq(exp_category, sum),
			exp_category = forcats::fct_rev(exp_category)
		)

	df_summ |>
		ggplot2::ggplot(ggplot2::aes(x = sum, y = exp_category, fill = exp_category)) +
		ggplot2::geom_col() +
		ggplot2::scale_x_continuous(
			labels = scales::label_comma(prefix = "$ ")
		) +
		ggplot2::labs(
			title = sprintf("Total $ %s spent",
											scales::label_comma()(sum(df_summ$sum, na.rm = TRUE))),
			x = NULL,
			y = NULL
		) +
		ggplot2::theme_minimal(base_size = 12) +
		ggplot2::theme(
			legend.position = "none",
			panel.grid.major.y = ggplot2::element_blank(),
			panel.grid.minor.x = ggplot2::element_blank(),
			plot.title.position = "plot"
		)
}

get_inc_summary <- function(df, start_date, end_date)
{
	df_summ <- df |>
		dplyr::filter(
			inc_date %within% lubridate::interval(start_date, end_date)
		) |>
		dplyr::group_by(inc_description) |>
		dplyr::reframe(
			sum = sum(inc_amount, na.rm = TRUE)
		) |>
		dplyr::ungroup() |>
		dplyr::mutate(
			inc_description = factor(inc_description),
			inc_description = forcats::fct_infreq(inc_description, sum),
			inc_description = forcats::fct_rev(inc_description)
		)
	df_summ |>
		ggplot2::ggplot(ggplot2::aes(x = sum, y = inc_description,
																 fill = inc_description)) +
		ggplot2::geom_col() +
		ggplot2::scale_x_continuous(
			labels = scales::label_comma(prefix = "$ ")
		) +
		ggplot2::labs(
			title = sprintf("Total $ %s income received",
											scales::label_comma()(sum(df_summ$sum, na.rm = TRUE))),
			x = NULL,
			y = NULL
		) +
		ggplot2::theme_minimal(base_size = 12) +
		ggplot2::theme(
			legend.position = "none",
			panel.grid.major.y = ggplot2::element_blank(),
			panel.grid.minor.x = ggplot2::element_blank(),
			plot.title.position = "plot"
		)
}

get_sav_summary <- function(df, start_date, end_date)
{
	df_summ <- df |>
		dplyr::filter(
			sav_date %within% lubridate::interval(start_date, end_date)
		) |>
		dplyr::group_by(sav_description) |>
		dplyr::reframe(
			sum = sum(sav_amount, na.rm = TRUE)
		) |>
		dplyr::ungroup() |>
		dplyr::mutate(
			sav_description = factor(sav_description),
			sav_description = forcats::fct_infreq(sav_description, sum),
			sav_description = forcats::fct_rev(sav_description)
		)
	df_summ |>
		ggplot2::ggplot(ggplot2::aes(x = sum, y = sav_description,
																 fill = sav_description)) +
		ggplot2::geom_col() +
		ggplot2::scale_x_continuous(
			labels = scales::label_comma(prefix = "$ ")
		) +
		ggplot2::labs(
			title = sprintf("Total $ %s saved",
											scales::label_comma()(sum(df_summ$sum, na.rm = TRUE))),
			x = NULL,
			y = NULL
		) +
		ggplot2::theme_minimal(base_size = 12) +
		ggplot2::theme(
			legend.position = "none",
			panel.grid.major.y = ggplot2::element_blank(),
			panel.grid.minor.x = ggplot2::element_blank(),
			plot.title.position = "plot"
		)
}

get_exp_summary_by_pay_account <- function(df, start_date, end_date)
{
	df_summ <- df |>
		dplyr::filter(
			exp_date %within% lubridate::interval(start_date, end_date)
		) |>
		dplyr::group_by(exp_pay_account, exp_category) |>
		dplyr::reframe(
			sum = sum(exp_amount, na.rm = TRUE)
		) |>
		dplyr::ungroup() |>
		dplyr::mutate(
			exp_category = forcats::fct_infreq(exp_category, sum),
			exp_category = forcats::fct_rev(exp_category)
		)

	df_summ |>
		ggplot2::ggplot(ggplot2::aes(x = sum, y = exp_category, fill = exp_pay_account)) +
		ggplot2::geom_col(position = "fill") +
		ggplot2::scale_x_continuous(
			labels = scales::label_percent()
		) +
		ggplot2::labs(
			title = "",
			x = NULL,
			y = NULL,
			fill = "Account"
		) +
		ggplot2::theme_minimal(base_size = 12) +
		ggplot2::theme(
			legend.position = "right",
			panel.grid.major.y = ggplot2::element_blank(),
			panel.grid.minor.x = ggplot2::element_blank(),
			plot.title.position = "plot"
		)
}

get_inc_summary_by_pay_account <- function(df, start_date, end_date)
{
	df_summ <- df |>
		dplyr::filter(
			inc_date %within% lubridate::interval(start_date, end_date)
		) |>
		dplyr::group_by(inc_pay_account, inc_description) |>
		dplyr::reframe(
			sum = sum(inc_amount, na.rm = TRUE)
		) |>
		dplyr::ungroup() |>
		dplyr::mutate(
			inc_description = factor(inc_description),
			inc_description = forcats::fct_infreq(inc_description, sum),
			inc_description = forcats::fct_rev(inc_description)
		)

	df_summ |>
		ggplot2::ggplot(ggplot2::aes(x = sum, y = inc_description,
																 fill = inc_pay_account)) +
		ggplot2::geom_col(position = "fill") +
		ggplot2::scale_x_continuous(
			labels = scales::label_percent()
		) +
		ggplot2::labs(
			title = "",
			x = NULL,
			y = NULL,
			fill = "Account"
		) +
		ggplot2::theme_minimal(base_size = 12) +
		ggplot2::theme(
			legend.position = "right",
			panel.grid.major.y = ggplot2::element_blank(),
			panel.grid.minor.x = ggplot2::element_blank(),
			plot.title.position = "plot"
		)
}
get_sav_summary_by_pay_account <- function(df, start_date, end_date)
{
	df_summ <- df |>
		dplyr::filter(
			sav_date %within% lubridate::interval(start_date, end_date)
		) |>
		dplyr::group_by(sav_pay_account, sav_description) |>
		dplyr::reframe(
			sum = sum(sav_amount, na.rm = TRUE)
		) |>
		dplyr::ungroup() |>
		dplyr::mutate(
			sav_description = factor(sav_description),
			sav_description = forcats::fct_infreq(sav_description, sum),
			sav_description = forcats::fct_rev(sav_description)
		)

	df_summ |>
		ggplot2::ggplot(ggplot2::aes(x = sum, y = sav_description,
																 fill = sav_pay_account)) +
		ggplot2::geom_col(position = "fill") +
		ggplot2::scale_x_continuous(
			labels = scales::label_percent()
		) +
		ggplot2::labs(
			title = "",
			x = NULL,
			y = NULL,
			fill = "Account"
		) +
		ggplot2::theme_minimal(base_size = 12) +
		ggplot2::theme(
			legend.position = "right",
			panel.grid.major.y = ggplot2::element_blank(),
			panel.grid.minor.x = ggplot2::element_blank(),
			plot.title.position = "plot"
		)
}


get_summary_by_type <- function(exp, inc, sav, start_date, end_date)
{

	dplyr::bind_rows(
		dplyr::select(exp, date = exp_date, amount = exp_amount) |>
			dplyr::mutate(type = "Expense"),
		dplyr::select(inc, date = inc_date, amount = inc_amount) |>
			dplyr::mutate(type = "Income"),
		dplyr::select(sav, date = sav_date, amount = sav_amount) |>
			dplyr::mutate(type = "Savings")
	) |>
		dplyr::filter(
			date %within% lubridate::interval(start_date, end_date)
		) |>
		ggplot2::ggplot(ggplot2::aes(x = date, y = amount, color = type)) +
		ggplot2::geom_line(alpha = .5) +
		ggplot2::geom_point(size = 3) +
		ggplot2::scale_y_continuous(
			labels = scales::label_comma(prefix = "$ ")
		) +
		ggplot2::labs(
			title = sprintf("Between %s and %s:", start_date, end_date),
			x = NULL,
			y = NULL,
			color = NULL
		) +
		ggplot2::theme_minimal(base_size = 12) +
		ggplot2::theme(
			legend.position = "top",
			panel.grid.major.y = ggplot2::element_blank(),
			panel.grid.minor.x = ggplot2::element_blank(),
			plot.title.position = "plot"
		)
}




format_tbl_savings <- function(df, lvl_exp_pay_account = NULL)
{
	if (!nrow(df) > 0) {
		df <- dummy_tbl_savings[0, ]
	}

	df <- df |>
		dplyr::mutate(
			sav_date = as.Date(sav_date),
			sav_pay_account = as.character(sav_pay_account),
			sav_description = as.character(sav_description),
			sav_amount = as.numeric(sav_amount),
			sav_note = as.character(sav_note)
		)
	if (!is.null(lvl_exp_pay_account)) {
		df <- df |>
			dplyr::mutate(
				sav_pay_account = factor(
					sav_pay_account, levels = lvl_exp_pay_account
				)
			)
	}
	return(df)
}



update_by_ids <- function(con, username, var_query, val_query,
													var_update, val_update)
{
	tryCatch({
		con$update(
			query = sprintf('{"username" : "%s", "%s": "%s"}',
											username, var_query, val_query),
			update = sprintf('{"$set":{"%s": "%s"}}', var_update, val_update),
			multiple = TRUE,
			upsert = TRUE
		)
		message("update success!")
	}, error = function(err) {
		message("failed to update! ", err)
	})
}

get_all_fields <- function(con, username = NULL)
{
	if (is.null(username)) {
		con$find(fields = '{}')
	} else {
		con$find(query = sprintf('{"username" : "%s"}', username),
						 fields = '{}')
	}
}




remove_rows_by_id <- function(con, ids) {
	qry <- paste0('{"_id" :{"$in": [',
								paste0('{"$oid":"', ids, '"}', collapse = ", "),
								']}}')
	con$remove(qry)
}


`%within%` <- lubridate::`%within%`
