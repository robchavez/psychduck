# Future developments:
#rep_perceiver_target_counts <- function(data, perceiver_id, target_id,
#                                        target_role, p1_role, p2_role,
#                                        t_meta_targets, p1_meta_targets,
#                                        p2_meta_targets, accuracy_role){
#  if (is.data.frame(data) == TRUE |
#      is.tibble(data) == TRUE) {
#
#    perceiver_id <- noquote(perceiver_id)
#    target_id <- noquote(target_id)
#    data_tmp <- data %>% select(perceiver_id, target_id)
#    # counts
#    # number of targets per group
#    n_t <- data_tmp %>%
#      filter(noquote(print(target_id)) %in% target_role) %>%
#      distinct(noquote(print(target_id))) %>%
#      count()
#    # is their target self-report
#    n_t_self <- filter(noquote(print(target_id)) %in% target_role) %>%
#      filyer(noquote(print(perceiver_id)) == noquote(print(target_id))) %>%
#      distinct(noquote(print(target_id))) %>%
#      count()
#    # number of P1s per group
#    n_p1 <- data_tmp %>%
#      filter(noquote(print(perceiver_id)) %in% p1_role) %>%
#      distinct(noquote(print(perceiver_id))) %>%
#      count()
#    # number of P2s per group
#    n_p2 <- data_tmp %>%
#      filter(noquote(print(perceiver_id)) %in% p2_role) %>%
#      distinct(noquote(print(perceiver_id))) %>%
#      count()
#    # number of target first-person meta ratings if relevant
#    if(!is.null(t_meta_targets)){
#      n_t_meta <- data_tmp %>%
#        filter(noquote(print(target_id)) %in% t_meta_targets) %>%
#        distinct(noquote(print(target_id))) %>%
#        count()}
#    # number of p1 3rd person meta ratings if relevant
#    if(!is.null(p1_meta_targets)){
#      n_p1_meta <- data_tmp %>%
#        filter(noquote(print(target_id)) %in% p1_meta_targets) %>%
#        distinct(noquote(print(target_id))) %>%
#        count()}
#    # number of p2 3rd person meta ratings if relevant
#    if(!is.null(p2_meta_targets)){
#      n_p2_meta <- data_tmp %>%
#        filter(noquote(print(target_id)) %in% p2_meta_targets) %>%
#        distinct(noquote(print(target_id))) %>%
#        count()}
#    # number of p1s per target
#    n_p1s_per_t <- data_tmp %>%
#      filter(noquote(print(perceiver_id)) %in% p1_role &
#               noquote(print(target_id)) %in% target_role &
#               noquote(print(perceiver_id)) != noquote(print(target_id))) %>%
#      distinct(noquote(print(target_id)), noquote(print(perceiver_id))) %>%
#      count()
#    # number of p2s per target
#    n_p2s_per_t <- data_tmp %>%
#      filter(noquote(print(perceiver_id)) %in% p2_role &
#               noquote(print(target_id)) %in% target_role &
#               noquote(print(perceiver_id)) != noquote(print(target_id))) %>%
#      distinct(noquote(print(target_id)), noquote(print(perceiver_id))) %>%
#      count()
#    return(n_t, n_p1, n_p2,
#           t_meta_targets, p1_meta_targets, p2_meta_targets,
#           n_p1s_per_t, n_p2s_per_t)
#  }
#  else{print("Error: expecting a dataframe or tibble for the first argument")}
#}
#
#rep_analyses_auto <- function(data, perceiver_id, target_id, ratings,
#                              target_role, p1_role, p2_role, t_meta_targets = NULL,
#                              p1_meta_targets = NULL, p2_meta_targets = NULL,
#                              accuracy_role = NULL){
#
#  # get counts to determine which model to use
#  if (is.data.frame(data) == TRUE |
#      is.tibble(data) == TRUE) {
#
#    data_cnt <- data %>% select(noquote(perceiver_id), noquote(target_id))
#    # counts
#    # number of targets per group
#    n_t <- data_cnt %>%
#      filter(noquote(print(target_id)) %in% target_role) %>%
#      distinct(noquote(print(target_id))) %>%
#      count()
#    # is their target self-report
#    n_t_self <- data_cnt %>%
#      filter(noquote(print(target_id)) %in% target_role) %>%
#      filter(noquote(print(perceiver_id)) == noquote(print(target_id))) %>%
#      distinct(noquote(print(target_id))) %>%
#      count()
#    # number of P1s per group
#    n_p1 <- data_cnt %>%
#      filter(noquote(print(perceiver_id)) %in% p1_role) %>%
#      distinct(noquote(print(perceiver_id))) %>%
#      count()
#    # number of P2s per group
#    n_p2 <- data_cnt %>%
#      filter(noquote(print(perceiver_id)) %in% p2_role) %>%
#      distinct(noquote(print(perceiver_id))) %>%
#      count()
#    # number of target first-person meta ratings if relevant
#    if(!is.null(t_meta_targets)){
#      n_t_meta <- data_cnt %>%
#        filter(noquote(print(target_id)) %in% t_meta_targets) %>%
#        distinct(noquote(print(target_id))) %>%
#        count()}
#    # number of p1 3rd person meta ratings if relevant
#    if(!is.null(p1_meta_targets)){
#      n_p1_meta <- data_cnt %>%
#        filter(noquote(print(target_id)) %in% p1_meta_targets) %>%
#        distinct(noquote(print(target_id))) %>%
#        count()}
#    # number of p2 3rd person meta ratings if relevant
#    if(!is.null(p2_meta_targets)){
#      n_p2_meta <- data_cnt %>%
#        filter(noquote(print(target_id)) %in% p2_meta_targets) %>%
#        distinct(noquote(print(target_id))) %>%
#        count()}
#    # number of p1s per target
#    n_p1s_per_t <- data_cnt %>%
#      filter(noquote(print(perceiver_id)) %in% p1_role &
#               noquote(print(target_id)) %in% target_role &
#               noquote(print(perceiver_id)) != noquote(print(target_id))) %>%
#      distinct(noquote(print(target_id)), noquote(print(perceiver_id))) %>%
#      count()
#    # number of p2s per target
#    n_p2s_per_t <- data_cnt %>%
#      filter(noquote(print(perceiver_id)) %in% p2_role &
#               noquote(print(target_id)) %in% target_role &
#               noquote(print(perceiver_id)) != noquote(print(target_id))) %>%
#      distinct(noquote(print(target_id)), noquote(print(perceiver_id))) %>%
#      count()
#  }
#  else{print("Error: expecting a dataframe or tibble for the first argument")}
#
#  if(n_p1$n == 1 & n_p2$n == 1 &
#     is.null(t_meta_targets)& is.null(p1_meta_targets) &
#     is.null(p2_meta_targets)& is.null(accuracy_role)) {
#    perceiver_id <- noquote(perceiver_id)
#    target_id <- noquote(target_id)
#    data_tmp <- data %>% select(perceiver_id, target_id, sapply(ratings, noquote))
#    # remove targets we don't need
#    data_tmp <- data_tmp %>%
#      filter(target_id %in% target_role)
#
#    # widen the data
#    data_tmp_wide <- data_tmp %>%
#      gather(rate_vars, value, -sess_id, -perceiver_id, -target_id) %>%
#      unite(perceiver_target_rating, perceiver_id, target_id, rate_vars) %>%
#      spread(perceiver_target_rating, value)
#
#    p1_ratings <- colnames(select(data_tmp, starts_with(p1_role)))
#    p2_ratings <- colnames(select(data_tmp, starts_with(p2_role)))
#
#    model_results<- rep_consensus(data_tmp_wide, p1_ratings, p2_ratings)
#    return(model_results)
#  }
#}
# handling individual diff mods
# handling group-level mods
