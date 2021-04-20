# fix warning about no visible bindings due to tidyverse functions
# https://community.rstudio.com/t/how-to-solve-no-visible-binding-for-global-variable-note/28887
utils::globalVariables(c("totalReplyCount", "id", "comments", "kind", "etag", "items", "snippet", "video_id_arg"))
