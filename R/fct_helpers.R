#' helpers
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd



mongodb_link <- Sys.getenv("mongodb_link")
email_address <- Sys.getenv("email_address")
email_password <- Sys.getenv("email_password")

outlook_creds <- sprintf(
	'{"type":"list","attributes":{"names":{"type":"character","attributes":{},"value":["version","host","port","use_ssl","user","password"]}},"value":[{"type":"integer","attributes":{},"value":[1]},{"type":"character","attributes":{},"value":["smtp-mail.outlook.com"]},{"type":"double","attributes":{},"value":[587]},{"type":"logical","attributes":{},"value":[true]},{"type":"character","attributes":{},"value":["%s"]},{"type":"character","attributes":{},"value":["%s"]}]}',
	email_address,
	email_password
)


# create email template with outlook credential
template <- shinyAuthX::email_template(
	creds_file = blastula::creds_file(textConnection(outlook_creds)),
	from = email_address,
	subject_welcome = "Welcome to mBudget, The Personal Budget App!",
	body_welcome = '
Hi {name},

<div style="color:green; font-size:40px;"><strong>Thanks for signing up for the mBudget App</strong>!</div>

We\\\'re happy you\\\'re here. Don\\\'t forget your username, {username}!

If you have any questions, please don\\\'t hesitate to contact us!

Happy Budgeting!!!
Regards,

Team

<strong>The mBudget App</strong>
',
	footer = "sent via [mBudget](https://github.com/myominnoo/mBudget) on {blastula::add_readable_time()}."
)
