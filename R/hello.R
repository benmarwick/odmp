
#' Details of US law enforcement officers killed while on duty
#'
#' A dataset containing basic information of police killed
#' while on duty in US during 1791-2018. Collected on
#' March 2018.
#'
#' @format A data frame with 23,257 rows and 18 variables:
#' \describe{
#'   \item{Age}{Officer's age at death}
#'   \item{Tour}{Officer's duration of service}
#'   \item{Badge}{Officer's badge}
#'   \item{Cause}{Cause of death}
#'   \item{Weapon}{Type of weapon involved, if known}
#'   \item{Offender}{Fate of offender, if known}
#'   \item{lat}{Location latitude}
#'   \item{long}{Location longitude}
#'   \item{nam_and_rank}{Officer name and rank}
#'   \item{eow}{Date of end of watch}
#'   \item{posix_date}{POSIX formatted date suitable for computing on}
#'   \item{year}{year of event}
#'   \item{month}{month of event}
#'   \item{day}{day of event}
#'   \item{gender}{Gender of officer}
#'   \item{rank}{Rank of officer}
#'   \item{name}{Name of officer}
#'   \item{State}{US State where officer died}
#' }
#' @source \url{https://www.odmp.org}
"odmp_1791_2018"
