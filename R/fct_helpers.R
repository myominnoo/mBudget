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
	body_welcome = "\n## Hi {name}!\n\nI hope this email finds you well! I wanted to personally extend a warm welcome to **mBudget**, the personal budget app. We're thrilled to have you join us. \n\nIf you have any questions, please don't hesitate to contact us!\n\nBest regards and cheers,\n\nMyo\n*Creator of mBudget*\n",
	body_getcode = "\nHi {name},\n\n<div>Your username is {username}.</div>\n\nPlease use the following code to verify your email:\n\n<div style=\"color:navy; font-size:40px;\"><strong>{code}</strong></div>\n\nIf this wasn\\'t you, please reset your email password to secure your account.\n\nRegards,\n\nThe `mBudget` App\n",
	body_pw_reset = "\nHi {name},\n\nWe have reset the password for the account {username}.\n\nRegards,\n\nThe `mBudget` App\n",
	footer = "sent via [mBudget](https://github.com/myominnoo/mBudget) on {blastula::add_readable_time()}."
)
