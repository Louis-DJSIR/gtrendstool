#' Choosing the correct timeframe of analysis for google trends data
#' 
#' @return A tibble
#' @export
#'
#' @examples

time_selector <- function (...) {
	answer <- readline("Does your period of analysis end at the most current time period? [Yes/No] ")

	if(tolower(substr(answer, 1, 1)) == "n") {
		end <- readline("What is the last day in your desired window of analysis? [YYYY-MM-DD] ")

		if(is.na(suppressWarnings(lubridate::as_date(end)))) {
			stop(paste0("'", start, "' does not correspond to a unique recognisable date, please try again using the format YYYY-MM-DD, \n such as ", Sys.Date()))
		}

		if(lubridate::as_date(end) > Sys.Date()) {
			stop(paste0("'", end, "' has not yet occurred, the most recent date is ", Sys.Date()))
		}

		start <- readline("What is the first day in your desired window of analysis? [YYYY-MM-DD] ")

		if(is.na(suppressWarnings(lubridate::as_date(start)))) {
			stop(paste0("'", start, "' does not correspond to a unique recognisable date, please try again using the format YYYY-MM-DD, \n such as ", Sys.Date()))
		}

		start <- lubridate::as_date(start)

		end <- lubridate::as_date(end)

		if(start > end) {
			answer2 <- readline(paste0("'", start, "' occurs later than '", end, "'. Did you mean to assess the time period between ", end, start, "instead? [Yes/No]"))
			if(tolower(substr(answer2, 1, 1)) == "n") {
				stop("Please start again with the desired start and end dates of your window of analysis.")
			} else {
				timeframe <- paste(end, start)
			}
		} else {
			timeframe <- paste(start, end)
		}
	} else {
		times <- cbind(c("Last hour"
				         , "Last 4 hours"
				         , "Last day"
				         , "Last 7 days"
				         , "Past 30 days"
				         , "Past 90 days"
				         , "Past 12 months"
				         , "Last 5 years"
				         , "Since the start of Google Trends")
					   , c("now 1-H"
					   	   , "now 4-H"
					   	   , "now 1-d"
					   	   , "now 7-d"
					   	   , "today 1-m"
					   	   , "today 3-m"
					   	   , "today 12-m"
					   	   , "today+5-y"
					   	   , "all")) %>%
			dplyr::as_tibble() %>%
			dplyr::rename(option = V1
				   		  , option_code = V2)

		times %>%
			dplyr::select(option) %>%
			print()

		answer3 <- readline("please specify the row number corresponding to the time period option that you would like to select: ")
		
		if(!(suppressWarnings(as.numeric(answer3)) %in% 1:nrow(times))) {
			stop(paste0("'", answer3, "' does not correspond to one of the options. Please start again."))
		}

		timeframe <- times %>%
			dplyr::slice(as.numeric(answer3)) %>%
			dplyr::select(option_code) %>%
			as.character()
	}
	message(paste0("'", dplyr::slice(times, as.numeric(answer3))$option , "' chosen, thank you."))
	
	assign("timeframe", timeframe, envir=globalenv())
	timeframe <<- timeframe
	return(timeframe)
}