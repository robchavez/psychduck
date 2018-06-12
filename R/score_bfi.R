#' A Function to calculate bfi trait scores
#'
#' This function takes a dataframe with Big Five Inventory 2 items labelled bfi_1, bfi_2, etc. and reverses necessary items and scores the five trait domains.
#' @param data The dataframe. Note that the bfi items must be labelled correctly
#' @keywords bfi, scores, reliability
#' @examples bfi_alpha(data)
#' score_bfi()
#' @export
score_bfi <- function(data1) {
  require(psych)
  data <- data1
  scores <- data.frame(matrix(NA, nrow = nrow(data), ncol = 5))

  #Agreeableness
  mitems <- cbind(data$bfi_2, data$bfi_7, data$bfi_12, data$bfi_17,
                  data$bfi_22, data$bfi_27, data$bfi_32,
                  data$bfi_37, data$bfi_42, data$bfi_47,
                  data$bfi_52, data$bfi_57)
  mkey <- c(1 , 1, -1, -1, -1, 1, 1, -1, -1, -1, 1, 1)
  agree_self <- psych::scoreItems(mkey, mitems, totals = FALSE, ilabels = NULL,missing=TRUE, impute="mean", delete=TRUE, min = NULL, max = NULL, digits = 4)
  scores$X1 <- agree_self$scores


  #Extraversion
  # Ext = 1, 6, 11R, 16R, 21, 26R, 31R, 36R, 41, 46, 51R, 56
  mitemse <- cbind(data$bfi_1, data$bfi_6, data$bfi_11, data$bfi_16,
                   data$bfi_21, data$bfi_26, data$bfi_31, data$bfi_36,
                   data$bfi_41, data$bfi_46, data$bfi_51,
                   data$bfi_56)
  mkeye <- c(1 , 1, -1, -1, 1, -1, -1, -1, 1, 1, -1, 1)
  extra_self <- psych::scoreItems(mkeye, mitemse, totals = FALSE, ilabels = NULL,missing=TRUE, impute="mean", delete=TRUE, min = NULL, max = NULL, digits = 4)
  scores$X2 <- extra_self$scores

  #openness
  # Open = 5R, 10, 15, 20, 25R, 30R, 35, 40, 45R, 50R, 55R, 60
  mitemso <- cbind(data$bfi_5, data$bfi_10, data$bfi_15, data$bfi_20,
                   data$bfi_25, data$bfi_30, data$bfi_35, data$bfi_40,
                   data$bfi_45, data$bfi_50, data$bfi_55, data$bfi_60)
  mkeyo <- c(-1 , 1, 1, 1, -1, -1, 1, 1, -1, -1, -1, 1)
  open_self <- psych::scoreItems(mkeyo, mitemso, totals = FALSE, ilabels = NULL,missing=TRUE, impute="mean", delete=TRUE, min = NULL, max = NULL, digits = 4)
  scores$X3 <- open_self$scores

  #Conscientiousness
  # Consc = 3R, 8R, 13, 18, 23R, 28R, 33, 38, 43, 48R, 53, 58R
  mitemsc <- cbind(data$bfi_3, data$bfi_8, data$bfi_13, data$bfi_18,
                   data$bfi_23, data$bfi_28, data$bfi_33, data$bfi_38,
                   data$bfi_43, data$bfi_48, data$bfi_53,
                   data$bfi_58)
  mkeyc <- c(-1 , -1, 1, 1, -1, -1, 1, 1, 1, -1, 1, -1)
  consc_self <- psych::scoreItems(mkeyc, mitemsc, totals = FALSE, ilabels = NULL,missing=TRUE, impute="mean", delete=TRUE, min = NULL, max = NULL, digits = 4)
  scores$X4 <- consc_self$scores

  #Neuroticism
  # Neur = 4R, 9R, 14, 19, 24R, 29R, 34, 39, 44R, 49R, 54, 59
  mitemsn <- cbind(data$bfi_4, data$bfi_9, data$bfi_14, data$bfi_19,
                   data$bfi_24, data$bfi_29, data$bfi_34, data$bfi_39,
                   data$bfi_44, data$bfi_49, data$bfi_54, data$bfi_59)
  mkeyn <- c(1 , -1, 1, 1, -1, -1, 1, 1, -1, -1, 1, 1)
  neuro_self <- psych::scoreItems(mkeyn, mitemsn, totals = FALSE, ilabels = NULL,missing=TRUE, impute="mean", delete=TRUE, min = NULL, max = NULL, digits = 4)
  scores$X5 <- neuro_self$scores

  scores <- as.data.frame(scores)
  colnames(scores) <- c("agree", "extra", "open", "consc", "neuro")

  return(scores)
}
