


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

# temp_dir <- tempdir()
# filenames <- sapply(c("expense", "income", "savings"),
# 			 function(x) sprintf("%s/%s_%s_%s.xlsx", temp_dir,
# 			 										"username", x,
# 			 										format(Sys.time(), "%Y%m%d_%H%M%S")))
#
# mapply(rio::export,
# 			 list(dummy_tbl_exp, dummy_tbl_income, dummy_tbl_savings),
# 			 filenames)
#
# utils::zip("~/zip.zip", filenames)
# utils::tar("~/zip.tar", filenames, compression = "gzip")

# mongodb -----------------------------------------------------------------

## mongodb connection


# users -------------------------------------------------------------------

con_tbl_user <- mongolite::mongo("tbl_user", "budgetApp", url)
con_tbl_user$count()
con_tbl_user$remove('{}')

shinyAuthX::create_dummy_users() |>
	con_tbl_user$insert()

get_all_fields(con_tbl_user) |>
	str()

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






