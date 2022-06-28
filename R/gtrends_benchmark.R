#' Finds the benchmark term that other terms in the selected list will be calibrated against for consistency.
#'
#' @param terms Google Trends search terms of interest.
#' @param regions Relevant regions to compare for search interest. Please use "region, country" syntax in the list of terms if looking at sub-national geographical areas.
#' @param source Relevant search engine. Choose from c("web", "news", "images", "froogle", "youtube")
#' @param category Search category to be applied. Search category applies to all search terms.
#' @param timeframe Window of time that google trends is searching for hits.
#'
#' @return Country/Term pair
#' @export
#'
#' @examples

benchmark <- function(terms, regions, source, category, timeframe = NA) {
	selected_regions <- region_list(regions)

	if(!("selected_category" %in% ls())) {
		categories_list(category)
	} else {
		selected_category <- selected_category
	}

	if(!("timeframe" %in% ls())) {
		timeframe <- time_selector()
	} else {
		timeframe <- timeframe
	}

	terms <- terms %>%
		dplyr::as_tibble()

	names(terms) <- category
	
	df_combined <- selected_regions %>%
		dplyr::full_join(terms, by = character())

	pb <- progress::progress_bar$new(
		format = "Benchmarking [:bar] :percent eta: :eta,  update: "
		, total = nrow(selected_regions)*(nrow(terms)-1)+1
		, clear = FALSE
		, width = 60
		)

	pb$tick()

	num <- 1

	benchmark_mat <- matrix(nrow = nrow(selected_regions), ncol = 2)
	
	for (i in unique(df_combined$region)) {
		df_combined_region <- df_combined %>%
			dplyr::filter(region == i)

		j <- df_combined_region %>%
			head(1) %>%
			dplyr::transmute(comparator = sports)
		
		query_total <- df_combined_region %>%
			tibble::add_column(j) %>%
			dplyr::filter(sports != comparator)

		query_list <- query_total

		# loops <- nrow(query_list)*nrow(selected_regions)

		for (k in 1:nrow(query_list)) {
			gtrends_one <- gtrendsR::gtrends(keyword = c(query_list$sports[1], query_list$comparator[1])
										 , geo = c(query_list$sub_code[1], query_list$sub_code[1])
										 , time = timeframe
										 , gprop = source
										 , category = selected_category$id)

			gtrends_leader <- gtrends_one$interest_over_time %>%
				dplyr::as_tibble() %>%
				dplyr::filter(hits == "100") %>%
				dplyr::group_by(keyword) %>%
				dplyr::summarise(n = dplyr::n()) %>%
				dplyr::ungroup() %>%
				dplyr::arrange(desc(n)) %>%
				head(1) %>%
				dplyr::select(keyword)

			pb$tick()

			if (gtrends_leader == j) {
				query_list <- query_list %>%
					tail(-1)
			} else {
				j <- query_list %>%
					head(1) %>%
					dplyr::select(comparator)
				query_list <- query_list %>%
						tail(-1) %>%
						dplyr::select(-comparator) %>%
						tibble::add_column(gtrends_leader) %>%
						dplyr::rename(comparator = keyword)
			}
			if (k == nrow(query_total)) {
				message(paste0("'", gtrends_leader, "' is the most popular search term in ", i))
			} else {
				message(paste0("'", gtrends_leader, "' is the leading search term in ", i))
			}

			# message(paste0("[", num*k, "/", loops, "] completed."))
		}

		benchmark_mat[num, 1] <- as.matrix(i)

		benchmark_mat[num, 2] <- as.matrix(gtrends_leader)

		num <- num + 1
	}

	benchmark_mat <- benchmark_mat %>%
		dplyr::as_tibble() %>%
		dplyr::mutate(category = selected_category$name) %>%
		dplyr::rename(region = V1
					  , benchmark = V2) %>%
		dplyr::left_join(selected_category, by = c("category" = "name")) %>%
		dplyr::rename(category_id = id)

	assign("benchmark_out", benchmark_mat, envir=globalenv())

	return(benchmark_mat)
}

# DIAGNOSTICS PLEASE DELETE AFTER THIS LINE

# terms <- c("tennis", "rugby", "football", "cricket", "formula 1")
# regions <- c("Australia", "New South Wales, Australia", "Victoria, Australia", "New Zealand")
# source <- "youtube"
# category <- "sports"

# benchmark(
# 	terms = c("tennis", "rugby", "football", "cricket", "formula 1")
# 	, regions = c("Australia", "New South Wales, Australia", "Victoria, Australia", "New Zealand")
# 	, source = "youtube"
# 	, category = "sports"
# 	)