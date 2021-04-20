# helpers

parse_comment_thread <- function(res) {
  res %>%
    # fields that may not be available:
    # live streaming details
    conditional_unnest_wider(var = "topLevelComment") %>%
    conditional_unnest_wider(var = "topLevelComment_snippet") %>%
    conditional_unnest_wider(var = "topLevelComment_snippet_authorChannelId") %>%
    conditional_unnest_wider(var = "pageInfo") %>%
    dplyr::select(-c(id)) %>%
    # rename to make compatible with other comments later
    dplyr::rename_at(
      dplyr::vars(tidyselect::starts_with("topLevelComment_")),
      ~stringr::str_remove(.x, "topLevelComment_")
    ) %>%
    dplyr::mutate(is_reply = FALSE)
}

parse_replies <- function(comment_thread) {
  replies <- comment_thread %>%
    dplyr::select(replies, totalReplyCount) %>%
    tidyr::unnest_wider(replies) %>%
    dplyr::filter(totalReplyCount > 0)

  if (nrow(replies) >= 0) {
    replies <- replies %>%
      tidyr::unnest(comments) %>%
      conditional_unnest_wider("comments") %>%
      # rename to make compatible with other comments
      dplyr::rename_at(
        dplyr::vars(tidyselect::starts_with("comments_")),
        ~ stringr::str_remove(.x, "comments_")
      ) %>%
      conditional_unnest_wider("snippet") %>%
      conditional_unnest_wider("snippet_authorChannelId") %>%
      dplyr::mutate(is_reply = TRUE)
  }
}

#' Get Most Comments
#'
#' @param filter string; Required.
#' named vector of length 1
#' potential names of the entry in the vector:
#' \code{video_id}: video ID.
#' \code{channel_id}: channel ID.
#' \code{thread_id}: comma-separated list of comment thread IDs
#' \code{threads_related_to_channel}: channel ID.
#'
#' @param part  Comment resource requested. Required. Comma separated list
#' of one or more of the
#' following: \code{id, snippet}. e.g., \code{"id, snippet"},
#' \code{"id"}, etc. Default: \code{snippet}.
#' @param max_results  Maximum number of items that should be returned.
#'  Integer. Optional. Default is 100.
#' If the value is greater than 100 then the function fetches all the
#' results. The outcome is a simplified \code{data.frame}.
#' @param page_token  Specific page in the result set that should be
#' returned. Optional.
#' @param text_format Data Type: Character. Default is \code{"html"}.
#' Only takes \code{"html"} or \code{"plainText"}. Optional.
#' @param \dots Additional arguments passed to \code{\link{tuber_GET}}.
#'
#' @return
#' Nested named list. The entry \code{items} is a list of comments
#' along with meta information.
#' Within each of the \code{items} is an item \code{snippet} which
#' has an item \code{topLevelComment$snippet$textDisplay}
#' that contains the actual comment.
#'
#' If simplify is \code{TRUE}, a \code{data.frame} with the following columns:
#' \code{authorDisplayName, authorProfileImageUrl, authorChannelUrl,
#' authorChannelId.value, videoId, textDisplay,
#' canRate, viewerRating, likeCount, publishedAt, updatedAt}
#'
#' @export get_most_comments
#'
#' @references \url{https://developers.google.com/youtube/v3/docs/commentThreads/list}
#'
#' @examples
#' \dontrun{
#'
#' # Set API token via yt_oauth() first
#'
#' get_most_comments(filter = c(video_id = "N708P-A45D0"))
#' get_most_comments(filter = c(video_id = "N708P-A45D0"), max_results = 101)
#' }
#'
#'

get_most_comments <- function(filter = NULL, part = "snippet,replies,id",
                              text_format = "html", max_results = 100, page_token = NULL, ...) {
  if (max_results < 20) {
    stop("max_results only takes a value over 20.
            Above 100, it outputs all the results.")
  }

  if (text_format != "html" & text_format != "plainText") {
    stop("Provide a legitimate value of textFormat.")
  }

  if (!(names(filter) %in%
    c("video_id", "channel_id", "thread_id", "threads_related_to_channel"))) {
    stop("filter can only take one of values: channel_id, video_id, parent_id,
        threads_related_to_channel.")
  }

  if (length(filter) != 1) stop("filter must be a vector of length 1.")

  orig_filter <- filter
  translate_filter <- c(
    video_id = "videoId", thread_id = "id",
    threads_related_to_channel = "allThreadsRelatedToChannelId",
    channel_id = "channelId", page_token = "pageToken"
  )

  yt_filter_name <- as.vector(translate_filter[match(
    names(filter),
    names(translate_filter)
  )])
  names(filter) <- yt_filter_name

  querylist <- list(
    part = part, maxResults =
      ifelse(max_results > 100, 100, max_results),
    textFormat = text_format,
    pageToken = page_token
  )

  querylist <- c(querylist, filter)
  print(querylist)

  ## get first page of results of a comment thread and
  ## initialize objects with content of first page before
  ## proceeding to next pages of API response
  res <- tuber:::tuber_GET("commentThreads", querylist, ...)
  # parse results
  snippet <- parse_snippet(res)
  comment_thread <- parse_comment_thread(snippet)
  replies <- parse_replies(comment_thread)
  # get columns names of columns that will be NA upon binding the two dataframes
  na_cols_1 <- setdiff(
    colnames(comment_thread),
    colnames(replies)
  )
  # setdiff(
  #   colnames(replies),
  #   colnames(comment_thread)
  # )

  comments <- dplyr::bind_rows(
    comment_thread, replies
  ) %>%
    dplyr::select(-c(replies)) %>%
    # dplyr::filter(totalReplyCount > 1) %>%
    # make columns complete if missing to avoid NAs
    tidyr::fill(tidyselect::any_of(na_cols_1), .direction = "down")

  # get all following pages of comment thread
  # agg_res <- comments

  #   # shouldn't this be `unique()`?
  # next_page_token <- unique(res$nextPageToken)
  print("erstes Mal")
  print(next_page_token)

  print("finished")

  comments
}

library(magrittr)
tuber::yt_oauth(
  app_id = Sys.getenv("YOUTUBE_API_APP_ID"),
  app_secret = Sys.getenv("YOUTUBE_API_CLIENT_SECRET")
)

all_data <- get_most_comments(filter = c(video_id = "Hop_MfkXl7c"), page_token = NULL)
counter_while <- 0
# next_page_token <- NULL
next_page_token <- unique(all_data$nextPageToken)

while (counter_while == 0 | !is.null(next_page_token)) {
  next_data <- get_most_comments(
    filter = c(video_id = "Hop_MfkXl7c"),
    page_token = next_page_token
  )
  next_page_token <- unique(next_data$nextPageToken)
  print(next_data)
  counter_while <- counter_while + 1
  message(paste(counter_while, ":", "counter_while"))
  message(paste(next_page_token, ":", "next_page_token"))
  all_data <- dplyr::bind_rows(next_data, all_data)
}
return(all_data)

retrieve_data_from_paginated_api <- function(video_id_input) {

  # initialize objects for loop
  all_data <- get_most_comments(filter = c(video_id = video_id_input), page_token = NULL)
  counter_while <- 0
  next_page_token <- unique(all_data$nextPageToken)

  # loop over results until last nextPageToken
  while (counter_while == 0 | !is.null(next_page_token)) {
    next_data <- get_most_comments(
      filter = c(video_id = video_id_input),
      page_token = next_page_token
    )

    # overwrite `next_page_token` that was initialized outside loop
    # with new content that was just retrieved in the data
    next_page_token <- unique(next_data$nextPageToken)
    counter_while <- counter_while + 1

    # overwrite `all_data` that was initialized outside loop
    # using `all_data` from outside of loop in first iteration
    # and then using itself from previous iteration plus
    # new `next_data`.
    all_data <- dplyr::bind_rows(next_data, all_data)
  }
  return(all_data)
}

# new_example_with_function <- retrieve_data_from_paginated_api(video_id_input = "Hop_MfkXl7c")
# new_example_with_function %>%
#   dplyr::filter(totalReplyCount > 1 & is_reply)
# length(unique(new_example_with_function$snippet_textOriginal))

# example <- get_most_comments(filter = c(video_id = "Hop_MfkXl7c"), page_token = NULL)
# example %>%
#   tidyr::unnest(nextPageToken)
#
# get_most_comments(filter = c(video_id = "Hop_MfkXl7c"), page_token = "QURTSl9pMjVDT2V3WGF5Nm5ha3ZYam1HbWVPMFVybTJuWk96R2UyOTZwNnVwSXFJdTJfUFVpSVI3VUxqbU1TSGpKWVpCcFpITEl4cm83dw==")
# tuber::get_comment_threads(filter = c(video_id = "Hop_MfkXl7c")) %>%
#   tibble::as_tibble()
#
# res_2 <- get_most_comments(filter = c(video_id = "Hop_MfkXl7c"))
#
# res_2$nextPageToken
#
# t1 <- "QURTSl9pMnM3TFpxY2FQUTVDNGVfTTBDRUF0Nm52R0RXNGRuM1R3a21fMDZhemR0aUtDeHRTWnV3UXpmREs0cnI0TmYzOXh2VTlzRXdOYw=="
# t2 <- "QURTSl9pMHdOWXBXWlRBakdlQnk1VXBUNEljSXM0QTU0WkNuOEp2VFBiX0RMQlhMeDVGdEo1UTlpWm5BaEFTRGZZZWU4UmV0LVFMTmFydw=="
# t3 <- "QURTSl9pMlY5M1Q1VkxsZW9RbXR0VG1acXkzRjRMaDYxcEc4c05UenVUc0VIR0tXbmZmVHU2V3RVdC1WUmlHaW5wa19WUlJfS19vSGEzNA=="
#
# get_most_comments(filter = c(video_id = "Hop_MfkXl7c"), page_token = t1)
# get_most_comments(filter = c(video_id = "Hop_MfkXl7c"), page_token = t2)
# get_most_comments(filter = c(video_id = "Hop_MfkXl7c"), page_token = t3)
#
