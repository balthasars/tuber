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

get_parse_bind_comments <- function(filter = NULL, page_token = NULL,
                                    part = part, text_format = text_format, max_results = 100) {
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
  # print(querylist)

  ## get first page of results of a comment thread and
  ## initialize objects with content of first page before
  ## proceeding to next pages of API response
  res <- tuber_GET("commentThreads", querylist)
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
  # print("erstes Mal")
  # print(next_page_token)

  # print("finished")

  comments
}


#' Get Most Comments
#'
#' Retrieves all top level comments and replies to them.
#' Replies to replies are not included.
#' @param video_id ID of video, required.
#' @return
#' Data frame with all comments and replies.
#'
#' @export get_most_comments
#'
#' @examples
#' \dontrun{
#'
#' # Set API token via yt_oauth() first
#'
#' get_most_comments(video_id = "Hop_MfkXl7c")
#' }
#'
#'
#'
get_most_comments <- function(video_id) {
  video_id_arg <- video_id
  part_arg <- "snippet,replies,id"
  filter_arg <- c(video_id = video_id_arg)
  text_format_arg <- "html"

  # initialize objects for loop
  all_data <- get_parse_bind_comments(
    filter = filter_arg,
    page_token = NULL,
    part = part_arg,
    text_format = text_format_arg,
    max_results = 100
    )
  counter_while <- 0
  suppressWarnings(next_page_token <- unique(all_data$nextPageToken))

  # loop over results until last nextPageToken
  while (counter_while == 0 | !is.null(next_page_token)) {
    next_data <- get_parse_bind_comments(
      filter = filter_arg,
      page_token = next_page_token,
      part = part_arg,
      text_format = text_format_arg,
      max_results = 100
    )
    counter_while <- counter_while + 1
    # cli::cli_alert_success("Page {counter_while} packages.")

    # overwrite `next_page_token` that was initialized outside loop
    # with new content that was just retrieved in the data
    suppressWarnings(next_page_token <- unique(all_data$nextPageToken))

    # overwrite `all_data` that was initialized outside loop
    # using `all_data` from outside of loop in first iteration
    # and then using itself from previous iteration plus
    # new `next_data`.
    all_data <- dplyr::bind_rows(next_data, all_data)
  }
  return(all_data)
}

# tuber::yt_oauth(
#   app_id = Sys.getenv("YOUTUBE_API_APP_ID"),
#   app_secret = Sys.getenv("YOUTUBE_API_CLIENT_SECRET")
# )
# get_most_comments(video_id = "Hop_MfkXl7c")
