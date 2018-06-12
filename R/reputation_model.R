# rep_consensus documentation

#' Reputation Consensus Model
#'
#' This takes a vector of P1 and P2 reports and fits
#' a model estimating the possible hearsay reputation parameters.
#' Those parameters are:
#' hc - hearsay consensus; the correlation between P1(T) & P2(T)
#' int_p1 - Intercept for P1(T)
#' int_p2 - Intercept for P2(T)
#' v_p1 - variance for P1(T)
#' v_p2 - variance for P2(T)
#' p1_p2_rel_el - P1-P2 Relative Elevation (i.e., Mean P1(T) - Mean P2(T))
#'
#' If n exchangeable triads > 1:
#' rec - direct reciprocity; the correlation between opposit P1(T)s (e.g., A(C) <-> C(A))
#' h - hearsay reciprocity; the correlation between exchangeable P2(T)s (e.g., B(C) <-> D(A))
#' m - unnamed parameter; The correlation between P2(T) and the opposite P1(T) in a group. (e.g., B(C) <-> C(A))
#'
#' The function can handle up to n exchangeable triads. It returns a fitted lavaan
#' object. See lavaan documentation for more.
#' @param data The dataframe that contains P1 and P2 ratings.
#' Data should wide, with a row for every group of participants.
#' At a minimum, it must contain two columns: one for P1 reports and one for P2 reports.
#' @param  p1_reports The column(s) that contain P1 reports,
#' or ratings made by the person that knows the target directly.
#' If more than one is supplied, the order must match the other
#' rating types.
#' @param p2_reports The column(s) that contain P1 reports,
#' or ratings made by the person that knows the target indirectly through the corresponding P1.
#' If more than one is supplied, the order must match the other
#' rating types.
#' @param n_triads The number of exchangeable triads in each group. By default, this is determined by
#' counting the number of P1 reports. This parameter rarely needs to be changed.
#' @param n_p1s_per_p2s The number of P1s for every P2. This defaults to 1.
#' Currently, only values of 1 are supported.
#' @param n_p2s_per_p1s The number of P2s for every P1;. This defaults to 1.
#' Currently, only values of 1 are supported.
#' @import lavaan
#' @export
#' @examples data("rep_sim_data")
#'           rep_consensus(data = rep_sim_data,
#'                         p1_reports = c("A_C_agreeableness", "C_A_agreeableness"),
#'                         p2_reports = c("B_C_agreeableness", "D_A_agreeableness"))

rep_consensus <- function(data, p1_reports, p2_reports, n_triads = length(p1_reports),
                          n_p1s_per_p2s = 1, n_p2s_per_p1s = 1){
  if(n_triads > 0 &
     n_p1s_per_p2s == 1 &
     n_p2s_per_p1s == 1){

    # code for 1 triad - much simpler
    if(n_triads == 1){

      model <-
        # hearsay (P1-P2) consensus
        paste(paste(p1_reports, "~~ hc*", p2_reports),
              # intercepts
              paste(p1_reports, "~ int_p1*1"),
              paste(p2_reports, "~ int_p2*1"),

              # variances
              paste(p1_reports, "~~ v_p1*", p1_reports),

              paste(p2_reports, " ~~ v_p2*", p2_reports),

              # P1-P2 relative elevation
              "p1_p2_rel_el := 1*int_p1 - (1*int_p2)", sep = "\n")}

    if(n_triads > 1 &
       n_p1s_per_p2s == 1 &
       n_p2s_per_p1s == 1){
      # create empty model
      model <- ""
      for(i in 1:n_triads){
        # cross-target correlations
        if(i < n_triads){
          prev_i <- i:1
          m   <- paste(p1_reports[i], "~~ m*", p2_reports[-prev_i]) %>% stringr::str_flatten(collapse = "\n")
          rec <- paste(p1_reports[i], "~~ rec*", p1_reports[-prev_i]) %>% stringr::str_flatten(collapse = "\n")
          h   <-  paste(p2_reports[i], "~~ h*", p2_reports[-prev_i], "\n") %>% stringr::str_flatten(collapse = "\n")
          xtrs <- paste(m, rec, h, sep = "\n")
          model <- paste(model, xtrs)}}
      hc <- paste(p1_reports, "~~ hc*", p2_reports) %>% stringr::str_flatten(collapse = "\n")
      int_p1 <- paste(p1_reports, "~ int_p1*1") %>% stringr::str_flatten(collapse = "\n")
      int_p2 <- paste(p2_reports, "~ int_p2*1") %>% stringr::str_flatten(collapse = "\n")
      v_p1 <- paste(p1_reports, "~~ v_p1*", p1_reports) %>% stringr::str_flatten(collapse = "\n")
      v_p2 <- paste(p2_reports, " ~~ v_p2*", p2_reports) %>% stringr::str_flatten(collapse = "\n")
      #  P1-P2 relative elevation
      p1p2_el <- "p1_p2_rel_el := 1*int_p1 - (1*int_p2)"

      model <- paste(hc, model,  int_p1, int_p2, v_p1, v_p2, p1p2_el, sep = "\n")}

  # fit model and print some informaiton about it
  fitted_model <- lavaan::sem(model, data = data, missing = "FIML")
  model_type <- "Simple Hearsay Consensus (P1-P2)"
  print(paste("Returning Results for", model_type, ". If this is not the model you intended to run, please check the data and variables you supplied."))
  cat(paste("design:\nnumber of exchangeable triads =", n_triads, "\n",
              "number of P1s per P2s", n_p1s_per_p2s, "\n",
              "number of P2s per P1s", n_p2s_per_p1s))

  return(fitted_model) }
  if(n_p1s_per_p2s > 1){print("I'm sorry, this function can only handle designs with 1 P1 per P2; check back for changes")}
  if(n_p2s_per_p1s > 1){print("I'm sorry, this function can only handle designs with 1 P1 per P2; check back for changes")}
}

# rep_consensus documentation

#' Reputation Consensus & Accuracy Model
#'
#' This takes a vector of P1-, P2-, and target self-reports and fits
#' a model estimating the possible hearsay reputation parameters.
#' Those parameters are:
#' hc - hearsay consensus; the correlation between P1(T) & P2(T)
#' ha - hearsay accuracy; the correation between P2(T) & T(T)
#' da - direct accuracy; the correlation between P1(T) & T(T)
#' int_p1 - Intercept for P1(T)
#' int_p2 - Intercept for P2(T)
#' int_self - Intercept for T(T)
#' v_p1 - variance for P1(T)
#' v_p2 - variance for P2(T)
#' v_self - variance for T(T)
#' p1_p2_rel_el - P1-P2 Relative Elevation (i.e., Mean P1(T) - Mean P2(T))
#' self_p2_rel_el - Self-P2 Relative Elevation (i.e., Mean T(T) - Mean P2(T))
#' self_p1_rel_el - Self-P1 Relative Elevation (i.e., Mean T(T) - Mean P1(T))
#'
#' If n exchangeable triads > 1:
#' rec - direct reciprocity; the correlation between opposit P1(T)s (e.g., A(C) <-> C(A))
#' h - hearsay reciprocity; the correlation between exchangeable P2(T)s (e.g., B(C) <-> D(A))
#' m - unnamed parameter; The correlation between P2(T) and the opposite P1(T) in a group. (e.g., B(C) <-> C(A))
#' tru_sim - True Similarity; the correlation between targets' self-reports. (e.g., A(A) <-> C(C))
#' as_sim_3p - Third-person assumed similarity; correlation between P2(T) and P1's self-report (e.g., B(C) <- A(A))
#' as_sim_1p - First-person assumed similarity (i.e., interpersonal assumed similarity); correlation between
#'             P1(T) and P1's self-report (e.g., A(C) <-> A(A))
#'
#' The function can handle up to n exchangeable triads. It returns a fitted lavaan
#' object. See lavaan documentation for more.
#' @param data The dataframe that contains P1 and P2 ratings.
#' Data should wide, with a row for every group of participants.
#' At a minimum, it must contain two columns: one for P1 reports and one for P2 reports.
#' @param  p1_reports The column(s) that contain P1 reports,
#' or ratings made by the person that knows the target directly.
#' If more than one is supplied, the order must match the other
#' rating types.
#' @param p2_reports The column(s) that contain P1 reports,
#' or ratings made by the person that knows the target indirectly through the corresponding P1.
#' If more than one is supplied, the order must match the other
#' rating types.
#' @param target_self The column(s) that contain target self-reports.
#' If more than one is supplied, the order must match the other
#' rating types.
#' @param n_triads The number of exchangeable triads in each group. By default, this is determined by
#' counting the number of P1 reports. It is rare that this parameter would need to be changed.
#' @param n_p1s_per_p2s The number of P1s for every P2. This defaults to 1.
#' Currently, only values of 1 are supported.
#' @param n_p2s_per_p1s The number of P2s for every P1;. This defaults to 1.
#' Currently, only values of 1 are supported.
#' @param n_p1s_per_ts The number of P1s for every target;. This defaults to 1.
#' Currently, only values of 1 are supported.
#' @param n_p2s_per_ts The number of P2s for every target;. This defaults to 1.
#' Currently, only values of 1 are supported.
#' @param n_ts_per_p1s The number of targets for every P1;. This defaults to 1.
#' Currently, only values of 1 are supported.
#' @param n_ts_per_p2s The number of targets for every P2;. This defaults to 1.
#' Currently, only values of 1 are supported.
#'
#'
#' @export
#' @examples data("rep_sim_data")
#'           rep_consensus_accuracy(data = rep_sim_data,
#'                                  p1_reports = c("A_C_agreeableness", "C_A_agreeableness"),
#'                                  p2_reports = c("B_C_agreeableness", "D_A_agreeableness"),
#'                                  target_self = c("C_C_agreeableness", "A_A_agreeableness"))
rep_consensus_accuracy <- function(data, p1_reports, p2_reports, target_self, n_triads = length(p1_reports),
                          n_p1s_per_p2s = 1, n_p2s_per_p1s = 1, n_p1s_per_ts = 1,
                          n_p2s_per_ts = 1, n_ts_per_p1s = 1, n_ts_per_p2s = 1){
  if(n_triads > 0 &
     n_p1s_per_p2s == 1 &
     n_p2s_per_p1s == 1 &
     n_p1s_per_ts == 1 &
     n_p2s_per_ts == 1 &
     n_ts_per_p1s == 1 &
     n_ts_per_p2s == 1 ){

    # code for 1 triad - much simpler
    if(n_triads == 1){

      model <-
        # hearsay (P1-P2) consensus
        paste(paste(p1_reports, "~~ hc*", p2_reports),
              paste(target_self, "~~ ha*", p2_reports),
              paste(target_self, "~~ da*", p1_reports),
              # intercepts
              paste(p1_reports, "~ int_p1*1"),
              paste(p2_reports, "~ int_p2*1"),
              paste(target_self, "~ int_self*1"),

              # variances
              paste(p1_reports, "~~ v_p1*", p1_reports),
              paste(p2_reports, " ~~ v_p2*", p2_reports),
              paste(target_self, " ~~ v_self*", target_self),

              # P1-P2 relative elevation
              "p1_p2_rel_el := 1*int_p1 - (1*int_p2)",
              "self_p2_rel_el := 1*int_self - (1*int_p2)",
              "self_p1_rel_el := 1*int_self - (1*int_p1)", sep = "\n")}

    if(n_triads > 1 &
       n_p1s_per_p2s == 1 &
       n_p2s_per_p1s == 1){
      # create empty model
      model <- ""
      for(i in 1:n_triads){
        # cross-target correlations
        if(i < n_triads){
          prev_i <- i:1

          m   <- paste(p1_reports[i], "~~ m*", p2_reports[-prev_i]) %>% stringr::str_flatten(collapse = "\n")
          rec <- paste(p1_reports[i], "~~ rec*", p1_reports[-prev_i]) %>% stringr::str_flatten(collapse = "\n")
          h   <-  paste(p2_reports[i], "~~ h*", p2_reports[-prev_i], "\n") %>% stringr::str_flatten(collapse = "\n")
          tru_sim <- paste(target_self[i], "~~ tru_sim*", target_self[-prev_i], "\n") %>% stringr::str_flatten(collapse = "\n")
          as_sim_3p <- paste(p2_reports[i], "~~ as_sim_3p*", target_self[-prev_i], "\n") %>% stringr::str_flatten(collapse = "\n")
          as_sim_1p <- paste(p1_reports[i], "~~ as_sim_1p*", target_self[-prev_i], "\n") %>% stringr::str_flatten(collapse = "\n")
          xtrs <- paste(m, rec, h, tru_sim, as_sim_3p, as_sim_1p, sep = "\n")
          model <- paste(model, xtrs)}}

      hc <- paste(p1_reports, "~~ hc*", p2_reports) %>% stringr::str_flatten(collapse = "\n")
      ha <- paste(target_self, "~~ ha*", p2_reports) %>% stringr::str_flatten(collapse = "\n")
      da <- paste(target_self, "~~ da*", p1_reports) %>% stringr::str_flatten(collapse = "\n")

      int_p1 <- paste(p1_reports, "~ int_p1*1") %>% stringr::str_flatten(collapse = "\n")
      int_p2 <- paste(p2_reports, "~ int_p2*1") %>% stringr::str_flatten(collapse = "\n")
      int_self <- paste(target_self, "~ int_self*1") %>% stringr::str_flatten(collapse = "\n")
      v_p1 <- paste(p1_reports, "~~ v_p1*", p1_reports) %>% stringr::str_flatten(collapse = "\n")
      v_p2 <- paste(p2_reports, " ~~ v_p2*", p2_reports) %>% stringr::str_flatten(collapse = "\n")
      v_self <- paste(target_self, " ~~ v_self*", target_self) %>% stringr::str_flatten(collapse = "\n")
      #  P1-P2 relative elevation
      p1p2_el <- "p1_p2_rel_el := 1*int_p1 - (1*int_p2)"
      selfp2_el <- "self_p2_rel_el := 1*int_self - (1*int_p2)"
      selfp1_el <- "self_p1_rel_el := 1*int_self - (1*int_p1)"

      model <- paste(hc, ha, da, model,  int_self, int_p1,
                     int_p2, v_self, v_p1, v_p2, p1p2_el, selfp2_el, selfp1_el, sep = "\n")}
    # fit model and print some informaiton about it
    fitted_model <- lavaan::sem(model, data = data, missing = "FIML")
    model_type <- "Full Triadic Model: Hearsay Consensus and Accuracy"
    print(paste("Returning Results for", model_type, ". If this is not the model you intended to run, please check the data and variables you supplied."))
    cat(paste("design:\nnumber of exchangeable triads =", n_triads, "\n",
              "number of P1s per P2s", n_p1s_per_p2s, "\n",
              "number of P2s per P1s", n_p2s_per_p1s, "\n",
              "number of p1s per targets", n_p1s_per_ts, "\n",
              "number of p2s per targets", n_p2s_per_ts, "\n",
              "number of targets per p1s", n_ts_per_p1s, "\n",
              "number of targets per p2s", n_ts_per_p2s, "\n"))

    return(fitted_model) }
  if(n_p1s_per_p2s > 1){print("I'm sorry, this function can only handle designs with 1 P1 per P2; check back for changes")}
  if(n_p2s_per_p1s > 1){print("I'm sorry, this function can only handle designs with 1 P1 per P2; check back for changes")}
  if(n_ts_per_p1s  > 1){print("I'm sorry, this function can only handle designs with 1 target per P1; check back for changes")}
  if(n_ts_per_p2s  > 1){print("I'm sorry, this function can only handle designs with 1 target per P2; check back for changes")}
  if(n_p1s_per_ts > 1){print("I'm sorry, this function can only handle designs with 1 P1 per target; check back for changes")}
  if(n_p2s_per_ts > 1){print("I'm sorry, this function can only handle designs with 1 P2 per target; check back for changes")}
}

#' Reputation Consensus, Accuracy, and 3rd-Person Meta-Perception Model
#'
#' This takes a vector of P1-, P2-, target self-reports, and
#' third-person meta-perceptions (for P1 and P2) and fits
#' a model estimating the possible hearsay reputation parameters.
#' Those parameters are:
#' hc - hearsay consensus; the correlation between P1(T) & P2(T)
#' ha - hearsay accuracy; the correation between P2(T) & T(T)
#' da - direct accuracy; the correlation between P1(T) & T(T)
#' p1ma - P1 Meta-Accuracy; the correlation between P1(P2(T)) & P2(T)
#' p2ma - P2 Meta-Accuracy; the correlation between P2(P1(T)) & P1(T)
#' as_ac1 -  P1 Assumed Accuracy; the correlation between P1(P2(T)) & T(T)
#' as_con1 - P1 Assumed Consensus; the correlation between P1(P2(T)) & P1(T)
#' mp_rec -  Meta-Perception Reciprocity; the correlation between P1(P2(T)) & P2(P1(T))
#' as_ac2  - P2 Assumed Accuracy; the correlation between P2(P1(T)) & T(T)
#' as_con2 - P2 Assumed Consensus; the correlation between P2(P1(T)) & P2(T)
#' int_p1 - Intercept for P1(T)
#' int_p2 - Intercept for P2(T)
#' int_self - Intercept for T(T)
#' int_mp1 - Intercept for P1(P2(T))
#' int_mp2 - Intercept for P2(P1(T))
#' v_p1 - variance for P1(T)
#' v_p2 - variance for P2(T)
#' v_self - variance for T(T)
#' v_mp1 - variance for P1(P2(T))
#' v_mp2 - variance for P2(P1(T))
#' p1_p2_rel_el - P1-P2 Relative Elevation (i.e., Mean P1(T) - Mean P2(T))
#' self_p2_rel_el - Self-P2 Relative Elevation (i.e., Mean T(T) - Mean P2(T))
#' self_p1_rel_el - Self-P1 Relative Elevation (i.e., Mean T(T) - Mean P1(T))
#' p1_meta_rel_el - P1 Meta Relative Elevation (i.e., mean P2(T) - Mean P1(P2(T)))
#' p2_meta_rel_el - P2 Meta Relative Elevation (i.e., mean P1(T) - Mean P2(P1(T)))
#' If n exchangeable triads > 1:
#' rec - direct reciprocity; the correlation between opposit P1(T)s (e.g., A(C) <-> C(A))
#' h - hearsay reciprocity; the correlation between exchangeable P2(T)s (e.g., B(C) <-> D(A))
#' m - unnamed parameter; The correlation between P2(T) and the opposite P1(T) in a group. (e.g., B(C) <-> C(A))
#' tru_sim - True Similarity; the correlation between targets' self-reports. (e.g., A(A) <-> C(C))
#' as_sim_3p - Third-person assumed similarity; correlation between P2(T) and P1's self-report (e.g., B(C) <- A(A))
#' as_sim_1p - First-person assumed similarity (i.e., interpersonal assumed similarity); correlation between
#'             P1(T) and P1's self-report (e.g., A(C) <-> A(A))
#' as_sim_p1m - P1 Meta-assumed similarity (e.g., A(B(C)) <-> A(A))
#' ukp1m1 - unknown p1-meta 1 - P1 meta-perception with opposite P1-report (e.g., A(B(C)) <-> C(A))).
#' p1meta_sim - P1 Meta-Similarity - correlation between
#'              exchangeable P1 meta-perceptions (e.g., A(B(C)) <-> C(D(A))).
#' ukp2m1 - unknown P2-meta 1 - P2 Meta-perception with exchangeable P2-reports (e.g., B(A(C)) <-> D(A))
#' ukp2m2 - unknown P2-meta 2 - P2 Meta-perception with exchangeable target self-report (e.g., B(A(C) <-> A(A)))
#' ukp2m3 - unknown P2-meta 3 - P2 Meta-perception with exchangeable P1-reports (e.g., B(A(C)) <-> C(A))
#' p2meta_sim - P2 Meta-Similarity - correlation between
#'              exchangeable P2 meta-perceptions (e.g., B(A(C)) <-> D(C(A))).
#' ukm1 - unknown Meta-perception - P1 Meta-perception with exchangeable P2 Meta-Perception
#'                                  (e.g., A(B(C)) <-> D(C(A)))
#'
#' The function can handle up to n exchangeable triads. It returns a fitted lavaan
#' object. See lavaan documentation for more.
#' @param data The dataframe that contains ratings (P1, P2, target self-report, P1, and P2 meta-perceptions).
#' Data should wide, with a row for every group of participants.
#' At a minimum, it must contain two columns: one for P1 reports and one for P2 reports.
#' @param  p1_reports The column(s) that contain P1 reports,
#' or ratings made by the person that knows the target directly.
#' If more than one is supplied, the order must match the other
#' rating types.
#' @param p2_reports The column(s) that contain P1 reports,
#' or ratings made by the person that knows the target indirectly through the corresponding P1.
#' If more than one is supplied, the order must match the other
#' rating types.
#' @param target_self The column(s) that contain target self-reports.
#' If more than one is supplied, the order must match the other
#' rating types.
#' @param  p1_meta The column(s) that contain P1 3rd person Meta-perceptions,
#' or P1's ratings of how they think P2 sees the target.
#' If more than one is supplied, the order must match the other
#' rating types.
#' @param p2_meta The column(s) that contain P2 3rd person Meta-perceptions,
#' or P2's ratings of how they think P1 sees the target.
#' If more than one is supplied, the order must match the other
#' rating types.
#' @param n_triads The number of exchangeable triads in each group. By default, this is determined by
#' counting the number of P1 reports. It is rare that this parameter would need to be changed.
#' @param n_p1s_per_p2s The number of P1s for every P2. This defaults to 1.
#' Currently, only values of 1 are supported.
#' @param n_p2s_per_p1s The number of P2s for every P1;. This defaults to 1.
#' Currently, only values of 1 are supported.
#' @param n_p1s_per_ts The number of P1s for every target;. This defaults to 1.
#' Currently, only values of 1 are supported.
#' @param n_p2s_per_ts The number of P2s for every target;. This defaults to 1.
#' Currently, only values of 1 are supported.
#' @param n_ts_per_p1s The number of targets for every P1;. This defaults to 1.
#' Currently, only values of 1 are supported.
#' @param n_ts_per_p2s The number of targets for every P2;. This defaults to 1.
#' Currently, only values of 1 are supported.
#'
#'
#' @export
#' @examples data("rep_sim_data")
#'           rep_full_w_3pmeta(data = rep_sim_data,
#'                             p1_reports = c("A_C_agreeableness", "C_A_agreeableness"),
#'                             p2_reports = c("B_C_agreeableness", "D_A_agreeableness"),
#'                             target_self = c("C_C_agreeableness", "A_A_agreeableness"),
#'                             p1_meta = c("A_B_C_agreeableness", "C_D_A_agreeableness"),
#'                             p2_meta = c("B_A_C_agreeableness", "D_C_A_agreeableness"))

rep_full_w_3pmeta <- function(data, p1_reports, p2_reports, target_self, p1_meta, p2_meta,
                              n_triads = length(p1_reports), n_p1s_per_p2s = 1,
                              n_p2s_per_p1s = 1, n_p1s_per_ts = 1,
                              n_p2s_per_ts = 1, n_ts_per_p1s = 1, n_ts_per_p2s = 1){
  if(n_triads > 0 &
     n_p1s_per_p2s == 1 &
     n_p2s_per_p1s == 1 &
     n_p1s_per_ts == 1 &
     n_p2s_per_ts == 1 &
     n_ts_per_p1s == 1 &
     n_ts_per_p2s == 1 ){

    # code for 1 triad - much simpler
    if(n_triads == 1){

      model <-
        # hearsay (P1-P2) consensus
        paste(paste(p1_reports, "~~ hc*", p2_reports),
              paste(target_self, "~~ ha*", p2_reports),
              paste(target_self, "~~ da*", p1_reports),
              paste(p1_meta, "~~p1ma*", p2_reports),
              paste(p2_meta, "~~p2ma*", p1_reports),
              paste(p1_meta, "~~as_ac1*", target_self),
              paste(p1_meta, "~~as_con1*", p1_reports),
              paste(p1_meta, "~~mp_rec*", p2_meta),
              paste(p2_meta, "~~as_ac2*", target_self),
              paste(p2_meta, "~~as_con2*", p2_reports),

              # intercepts
              paste(p1_reports, "~ int_p1*1"),
              paste(p2_reports, "~ int_p2*1"),
              paste(target_self, "~ int_self*1"),
              paste(p1_meta, "~ int_mp1*1"),
              paste(p2_meta, "~ int_mp2*1"),

              # variances
              paste(p1_reports, "~~ v_p1*", p1_reports),
              paste(p2_reports, "~~ v_p2*", p2_reports),
              paste(target_self, "~~ v_self*", target_self),
              paste(p1_meta, "~~ v_mp1*", p1_meta),
              paste(p2_meta, "~~ v_mp2*", p2_meta),

              # P1-P2 relative elevation
              "p1_p2_rel_el := 1*int_p1 - (1*int_p2)",
              "self_p2_rel_el := 1*int_self - (1*int_p2)",
              "self_p1_rel_el := 1*int_self - (1*int_p1)",
              "p1_meta_rel_el := 1*int_p2 - (1*int_mp1)",
              "p2_meta_rel_el := 1*int_p1 - (1*int_mp2)", sep = "\n")}

    if(n_triads > 1 &
       n_p1s_per_p2s == 1 &
       n_p2s_per_p1s == 1){
      # create empty model
      model <- ""
      for(i in 1:n_triads){
        # cross-target correlations
        if(i < n_triads){
          prev_i <- i:1

          m   <- paste(p1_reports[i], "~~ m*", p2_reports[-prev_i]) %>% stringr::str_flatten(collapse = "\n")
          rec <- paste(p1_reports[i], "~~ rec*", p1_reports[-prev_i]) %>% stringr::str_flatten(collapse = "\n")
          h   <-  paste(p2_reports[i], "~~ h*", p2_reports[-prev_i], "\n") %>% stringr::str_flatten(collapse = "\n")
          tru_sim <- paste(target_self[i], "~~ tru_sim*", target_self[-prev_i], "\n") %>% stringr::str_flatten(collapse = "\n")
          as_sim_3p <- paste(p2_reports[i], "~~ as_sim_3p*", target_self[-prev_i], "\n") %>% stringr::str_flatten(collapse = "\n")
          as_sim_1p <- paste(p1_reports[i], "~~ as_sim_1p*", target_self[-prev_i], "\n") %>% stringr::str_flatten(collapse = "\n")
          as_sim_p1m <- paste(p1_meta[i], "~~ as_sim_p1m*", target_self[-prev_i], "\n") %>% stringr::str_flatten(collapse = "\n")
          ukp1m1 <- paste(p1_meta[i], "~~ ukp1m1*", p1_reports[-prev_i], "\n") %>% stringr::str_flatten(collapse = "\n")
          p1meta_sim <- paste(p1_meta[i], "~~ p1meta_sim*", p1_meta[-prev_i], "\n") %>% stringr::str_flatten(collapse = "\n")
          ukp2m1 <- paste(p2_meta[i], "~~ ukp2m1*", p2_reports[-prev_i], "\n") %>% stringr::str_flatten(collapse = "\n")
          ukp2m2 <- paste(p2_meta[i], "~~ ukp2m2*", target_self[-prev_i], "\n") %>% stringr::str_flatten(collapse = "\n")
          ukp2m3 <- paste(p2_meta[i], "~~ ukp2m3*", p1_reports[-prev_i], "\n") %>% stringr::str_flatten(collapse = "\n")
          p2meta_sim <- paste(p2_meta[i], "~~ p2meta_sim*", p2_meta[-prev_i], "\n") %>% stringr::str_flatten(collapse = "\n")
          ukm1 <- paste(p1_meta[i], "~~ ukm1*", p2_meta[-prev_i], "\n") %>% stringr::str_flatten(collapse = "\n")
          xtrs <- paste(m, rec, h, tru_sim, as_sim_3p, as_sim_1p,
                        ukp1m1, p1meta_sim, ukp2m1, ukp2m2, ukp2m3, p2meta_sim, ukm1, sep = "\n")
          model <- paste(model, xtrs)}}

      hc <- paste(p1_reports, "~~ hc*", p2_reports) %>% stringr::str_flatten(collapse = "\n")
      ha <- paste(target_self, "~~ ha*", p2_reports) %>% stringr::str_flatten(collapse = "\n")
      da <- paste(target_self, "~~ da*", p1_reports) %>% stringr::str_flatten(collapse = "\n")
      p1ma <- paste(p1_meta, "~~p1ma*", p2_reports) %>% stringr::str_flatten(collapse = "\n")
      p2ma <- paste(p2_meta, "~~p2ma*", p1_reports) %>% stringr::str_flatten(collapse = "\n")
      as_ac1 <- paste(p1_meta, "~~as_ac1*", target_self) %>% stringr::str_flatten(collapse = "\n")
      as_con1 <- paste(p1_meta, "~~as_con1*", p1_reports) %>% stringr::str_flatten(collapse = "\n")
      mp_rec <- paste(p1_meta, "~~mp_rec*", p2_meta) %>% stringr::str_flatten(collapse = "\n")
      as_ac2  <- paste(p2_meta, "~~as_ac2*", target_self) %>% stringr::str_flatten(collapse = "\n")
      as_con2 <- paste(p2_meta, "~~as_con2*", p2_reports) %>% stringr::str_flatten(collapse = "\n")

      int_p1 <- paste(p1_reports, "~ int_p1*1") %>% stringr::str_flatten(collapse = "\n")
      int_p2 <- paste(p2_reports, "~ int_p2*1") %>% stringr::str_flatten(collapse = "\n")
      int_self <- paste(target_self, "~ int_self*1") %>% stringr::str_flatten(collapse = "\n")
      int_mp1 <- paste(p1_meta, "~ int_mp1*1") %>% stringr::str_flatten(collapse = "\n")
      int_mp2 <- paste(p2_meta, "~ int_mp2*1") %>% stringr::str_flatten(collapse = "\n")

      v_p1 <- paste(p1_reports, "~~ v_p1*", p1_reports) %>% stringr::str_flatten(collapse = "\n")
      v_p2 <- paste(p2_reports, " ~~ v_p2*", p2_reports) %>% stringr::str_flatten(collapse = "\n")
      v_mp1 <- paste(p1_meta, "~~ v_mp1*", p1_meta) %>% stringr::str_flatten(collapse = "\n")
      v_mp2 <- paste(p2_meta, "~~ v_mp2*", p2_meta) %>% stringr::str_flatten(collapse = "\n")
      v_self <- paste(target_self, " ~~ v_self*", target_self) %>% stringr::str_flatten(collapse = "\n")
      #  P1-P2 relative elevation
      p1p2_el <- "p1_p2_rel_el := 1*int_p1 - (1*int_p2)"
      selfp2_el <- "self_p2_rel_el := 1*int_self - (1*int_p2)"
      selfp1_el <- "self_p1_rel_el := 1*int_self - (1*int_p1)"
      p1_meta_el <- "p1_meta_rel_el := 1*int_p2 - (1*int_mp1)"
      p2_meta_el <- "p2_meta_rel_el := 1*int_p1 - (1*int_mp2)"
      model <- paste(hc, ha, da, p1ma, p2ma, as_ac1, as_con1,
                     mp_rec, as_ac2, as_con2, model, int_self, int_p1,
                     int_p2, int_mp1, int_mp2, v_self, v_p1, v_p2, v_mp1, v_mp2,
                     p1p2_el, selfp2_el, selfp1_el, p1_meta_el, p2_meta_el,
                     sep = "\n")}
    # fit model and print some informaiton about it
    fitted_model <- lavaan::sem(model, data = data, missing = "FIML")
    model_type <- "Full Triadic Model: Hearsay Consensus and Accuracy"
    print(paste("Returning Results for", model_type, ". If this is not the model you intended to run, please check the data and variables you supplied."))
    cat(paste("design:\nnumber of exchangeable triads =", n_triads, "\n",
              "number of P1s per P2s", n_p1s_per_p2s, "\n",
              "number of P2s per P1s", n_p2s_per_p1s, "\n",
              "number of p1s per targets", n_p1s_per_ts, "\n",
              "number of p2s per targets", n_p2s_per_ts, "\n",
              "number of targets per p1s", n_ts_per_p1s, "\n",
              "number of targets per p2s", n_ts_per_p2s, "\n"))

    return(fitted_model) }
  if(n_p1s_per_p2s > 1){print("I'm sorry, this function can only handle designs with 1 P1 per P2; check back for changes")}
  if(n_p2s_per_p1s > 1){print("I'm sorry, this function can only handle designs with 1 P1 per P2; check back for changes")}
  if(n_ts_per_p1s  > 1){print("I'm sorry, this function can only handle designs with 1 target per P1; check back for changes")}
  if(n_ts_per_p2s  > 1){print("I'm sorry, this function can only handle designs with 1 target per P2; check back for changes")}
  if(n_p1s_per_ts > 1){print("I'm sorry, this function can only handle designs with 1 P1 per target; check back for changes")}
  if(n_p2s_per_ts > 1){print("I'm sorry, this function can only handle designs with 1 P2 per target; check back for changes")}
}

#' Reputation Analyses (automatic)
#'
#' This is a wrapper function around the reputation model functions.
#'
#' This chooses a function depending on which
#' vectors sre supplied. At a minimum, it requires a P1- & P2-ratings. It can
#' optionally take target self-reports and third-person meta-perceptions (for P1 and P2).
#' It fits a model estimating the possible hearsay reputation parameters.
#'
#' The function can handle up to n exchangeable triads. It returns a fitted lavaan
#' object. See lavaan documentation for more.
#' @param data The dataframe that contains ratings (P1, P2, target self-report, P1, and P2 meta-perceptions).
#' Data should wide, with a row for every group of participants.
#' At a minimum, it must contain two columns: one for P1 reports and one for P2 reports.
#' @param  p1_reports The column(s) that contain P1 reports,
#' or ratings made by the person that knows the target directly.
#' If more than one is supplied, the order must match the other
#' rating types.
#' @param p2_reports The column(s) that contain P1 reports,
#' or ratings made by the person that knows the target indirectly through the corresponding P1.
#' If more than one is supplied, the order must match the other
#' rating types.
#' @param target_self The column(s) that contain target self-reports.
#' If more than one is supplied, the order must match the other
#' rating types. The default is NULL.
#' @param  p1_meta The column(s) that contain P1 3rd person Meta-perceptions,
#' or P1's ratings of how they think P2 sees the target.
#' If more than one is supplied, the order must match the other
#' rating types. The default is NULL
#' @param p2_meta The column(s) that contain P2 3rd person Meta-perceptions,
#' or P2's ratings of how they think P1 sees the target.
#' If more than one is supplied, the order must match the other
#' rating types. The default is NULL.
#' @param n_triads The number of exhangeable triads in each group. By default, this is determined by
#' counting the number of P1 reports. It is rare that this parameter would need to be changed.
#' @param n_p1s_per_p2s The number of P1s for every P2. This defaults to 1.
#' Currently, only values of 1 are supported.
#' @param n_p2s_per_p1s The number of P2s for every P1;. This defaults to 1.
#' Currently, only values of 1 are supported.
#' @param n_p1s_per_ts The number of P1s for every target;. This defaults to 1.
#' Currently, only values of 1 are supported.
#' @param n_p2s_per_ts The number of P2s for every target;. This defaults to 1.
#' Currently, only values of 1 are supported.
#' @param n_ts_per_p1s The number of targets for every P1;. This defaults to 1.
#' Currently, only values of 1 are supported.
#' @param n_ts_per_p2s The number of targets for every P2;. This defaults to 1.
#' Currently, only values of 1 are supported.
#'
#'
#'
#' @export
#' @examples data("rep_sim_data")
#'           rep_analyses_auto(data = rep_sim_data,
#'                             p1_reports = c("A_C_agreeableness", "C_A_agreeableness"),
#'                             p2_reports = c("B_C_agreeableness", "D_A_agreeableness"))
#'
#'           rep_analyses_auto(data = rep_sim_data,
#'                             p1_reports = c("A_C_agreeableness", "C_A_agreeableness"),
#'                             p2_reports = c("B_C_agreeableness", "D_A_agreeableness"),
#'                             target_self = c("C_C_agreeableness", "A_A_agreeableness"))
#'
#'           rep_analyses_auto(data = rep_sim_data,
#'                             p1_reports = c("A_C_agreeableness", "C_A_agreeableness"),
#'                             p2_reports = c("B_C_agreeableness", "D_A_agreeableness"),
#'                             target_self = c("C_C_agreeableness", "A_A_agreeableness"),
#'                             p1_meta = c("A_B_C_agree_meta", "C_D_A_agree_meta"),
#'                             p2_meta = c("B_A_C_agree_meta", "D_C_A_agree_meta"))
#'
rep_analyses_auto <- function(data, p1_reports, p2_reports, target_self = NULL, p1_meta = NULL, p2_meta = NULL,
                    n_triads = length(p1_reports), n_p1s_per_p2s = 1,
                    n_p2s_per_p1s = 1, n_p1s_per_ts = 1,
                    n_p2s_per_ts = 1, n_ts_per_p1s = 1, n_ts_per_p2s = 1){
  if(is.null(target_self) &
     is.null(p1_meta) &
     is.null(p2_meta)){
    rep_consensus(data = data, p1_reports = p1_reports, p2_reports = p2_reports)
  }
  if(is.null(p1_meta) &
     is.null(p2_meta)){
    rep_consensus_accuracy(data = data, p1_reports = p1_reports, p2_reports = p2_reports, target_self = target_self)
  }
  if(!is.null(target_self) &
     !is.null(p1_meta) &
     !is.null(p2_meta)){
    rep_full_w_3pmeta(data = data, p1_reports = p1_reports,  p2_reports = p2_reports, target_self = target_self,
                      p1_meta = p1_meta, p2_meta = p2_meta)
  }
}

