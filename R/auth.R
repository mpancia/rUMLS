#' @importFrom httr POST status_code
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_attr html_text

umls_env <- new.env(parent = emptyenv())

#' Get a TGT token from the UMLS API.
#'
#' TGT tokens are used to generate service tickets that allow for requests to be ran against the UMLS API.
#'
#' @param name The UMLS username.
#' @param pass The UMLS password.
#'
#' @return Returns nothing, adding the TGT token to the environment.
#' @export
get_TGT <- function(name = NULL, pass = NULL)
{
  TGT <- umls_env$TGT
  if (is.null(TGT) && !is.null(name) && !is.null(pass))
  {
    params <- list(username = name, password = pass)
    authURL <- paste0(authBaseURL, authEndpoint)
    r <- POST(url = authURL, body = params, encode = "form")
    if(status_code(r) == 201)
    {
      htmlResponse <- read_html(r)
      TGT <- html_attr(html_nodes(htmlResponse, "form"), "action")
      set_TGT(TGT)
      message("Authenticated.")
    } else
    {
      stop("Error authenticating.")
    }
  } else if (is.null(TGT) && (is.null(name) | !is.null(pass)))
  {
    stop("No username/password provided.")
  }

  TGT
}

#' @rdname get_TGT
#' @export
set_TGT <- function(value)
{
  umls_env$TGT <- value
}

#' @rdname get_TGT
#' @export
reset_TGT <- function()
{
  set_TGT(NULL)
}

#' Get a service ticket.
#'
#' @param TGT The TGT token for the current session.
#'
#' @return Returns nothing, adding the service token to the environment.
#' @export
#'
get_service_ticket <- function(TGT)
{
  r <- POST(url = TGT, body = list(service = "http://umlsks.nlm.nih.gov"),
    encode = "form")
  htmlResponse <- read_html(r)
  ticketValue <- html_text(html_nodes(htmlResponse, "body"))
  set_service_ticket(ticketValue)
}

#' @rdname get_service_ticket
#' @export
set_service_ticket <- function(value)
{
  umls_env$service_ticket <- value
}

#' Authenticate against UMLS.
#'
#' @return Returns nothing, sets user credentials for UMLS.
#' @export
#'
#'
auth_UMLS <- function()
{
  user <- readline("Enter username:")
  pass <- readline("Enter password:")
  if(!is.null(user) && !is.null(pass))
  {
    invisible(get_TGT(user, pass))
  } else
  {
    stop("You did not enter a username and password.")
  }
}
