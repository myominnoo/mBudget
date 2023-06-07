


# dummy tables ------------------------------------------------------------

dummy_tbl_cat <- readxl::read_excel("dev/dummy_tables.xlsx",
																		sheet = "tbl_category")
dplyr::glimpse(dummy_tbl_cat)

dummy_tbl_pay_account <- readxl::read_excel("dev/dummy_tables.xlsx",
																						sheet = "tbl_pay_account")
dplyr::glimpse(dummy_tbl_pay_account)


dummy_tbl_exp <- readxl::read_excel("dev/dummy_tables.xlsx",
																		sheet = "tbl_expense")
dplyr::glimpse(dummy_tbl_exp)


dummy_tbl_income <- readxl::read_excel("dev/dummy_tables.xlsx",
																			 sheet = "tbl_income")
dplyr::glimpse(dummy_tbl_income)



dummy_tbl_savings <- readxl::read_excel("dev/dummy_tables.xlsx",
																			 sheet = "tbl_savings")
dplyr::glimpse(dummy_tbl_savings)


usethis::use_data(
	dummy_tbl_exp,
	dummy_tbl_cat,
	dummy_tbl_pay_account,
	dummy_tbl_income,
	dummy_tbl_savings,
	internal = TRUE, overwrite = TRUE
)

user <- "admin"

df_expense <- dummy_tbl_exp |> dplyr::filter(username == user)
df_income <- dummy_tbl_income |> dplyr::filter(username == user)
df_savings <- dummy_tbl_savings |> dplyr::filter(username == user)

ttl_exp_amount <- df_expense$exp_amount |> sum(na.rm = TRUE)
ttl_inc_amount <- df_income$inc_amount |> sum(na.rm = TRUE)
ttl_sav_amount <- df_savings$sav_amount |> sum(na.rm = TRUE)

balance <- ttl_inc_amount + ttl_sav_amount - ttl_exp_amount

df_summ <- dummy_tbl_income |>
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
	ggplot2::ggplot(ggplot2::aes(x = sum, y = inc_description, fill = inc_description)) +
	ggplot2::geom_col(position = "fill") +
	ggplot2::scale_x_continuous(
		labels = scales::label_percent()
	) +
	ggplot2::labs(
		title = sprintf("$ %s spent between %s and %s",
										scales::label_comma()(sum(df$sum, na.rm = TRUE)),
										"start_date", "end_date"),
		x = "$",
		y = NULL,
		fill = "Account"
	) +
	ggplot2::theme_minimal() +
	ggplot2::theme(
		legend.position = "right",
		panel.grid.major.y = ggplot2::element_blank(),
		panel.grid.minor.x = ggplot2::element_blank(),
		plot.title.position = "plot"
	)

a
# calculations ------------------------------------------------------------






# mongodb -----------------------------------------------------------------

## mongodb connection
url <- "mongodb+srv://myself:letmein2022@budgetapp.whtgpcl.mongodb.net"


# expense table -----------------------------------------------------------

con_tbl_expense <- mongolite::mongo("tbl_expense", "budgetApp", url)
con_tbl_expense$count()
con_tbl_expense$remove('{}')

dummy_tbl_exp |>
	dplyr::bind_rows(
		dummy_tbl_exp |>
			dplyr::mutate(username = "test")
	) |>
	format_tbl_expense() |>
	con_tbl_expense$insert()


# remove_rows_by_id <- function(con, ids) {
# 	qry <- paste0('{"_id" :{"$in": [',
# 								paste0('{"$oid":"', ids, '"}', collapse = ", "),
# 								']}}')
# 	print(qry)
# 	con$remove(qry, just_one = TRUE)
# }
#
# remove_rows_by_id(con_tbl_expense, "647f6b0367718338e70bb231")
#
# con_tbl_expense$update(
# 	query = sprintf('{"username" : "%s", "exp_category": "%s"}',
# 									"admin", "Health Care"),
# 	update = sprintf('{"$set":{"exp_category": "%s"}}', "healthcare"),
# 	upsert = TRUE
# )
get_all_fields(con_tbl_expense) |>
	str()



# income ------------------------------------------------------------------

con_tbl_income <- mongolite::mongo("tbl_income", "budgetApp", url)
con_tbl_income$count()
con_tbl_income$remove('{}')

dummy_tbl_income |>
	dplyr::bind_rows(
		dummy_tbl_income |>
			dplyr::mutate(username = "test")
	) |>
	format_tbl_income() |>
	con_tbl_income$insert()
get_all_fields(con_tbl_income) |>
	str()


# savings -----------------------------------------------------------------


con_tbl_savings <- mongolite::mongo("tbl_savings", "budgetApp", url)
con_tbl_savings$count()
con_tbl_savings$remove('{}')

dummy_tbl_savings |>
	dplyr::bind_rows(
		dummy_tbl_savings |>
			dplyr::mutate(username = "test")
	) |>
	format_tbl_savings() |>
	con_tbl_savings$insert()
get_all_fields(con_tbl_savings) |>
	str()



# category ----------------------------------------------------------------

con_tbl_category <- mongolite::mongo("tbl_category", "budgetApp", url)
con_tbl_category$count()
con_tbl_category$remove('{}')

dummy_tbl_cat |>
	dplyr::bind_rows(
		dummy_tbl_cat |>
			dplyr::mutate(username = "test")
	) |>
	con_tbl_category$insert()
get_all_fields(con_tbl_category) |>
	str()



# pay account -------------------------------------------------------------

con_tbl_pay_account <- mongolite::mongo("tbl_pay_account", "budgetApp", url)
con_tbl_pay_account$count()
con_tbl_pay_account$remove('{}')

dummy_tbl_pay_account |>
	dplyr::bind_rows(
		dummy_tbl_pay_account |>
			dplyr::mutate(username = "test")
	) |>
	con_tbl_pay_account$insert()
get_all_fields(con_tbl_pay_account) |>
	str()






