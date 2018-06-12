#' Find the 3-item short scale (criterion = part-whole relationship)
#'
#' This function tests all 3-item combinations possible in your questionnaire and returns the combo that explains the highest variance in mean score
#' @param x The vector with all the items in the questionnaire. All columns must be numeric.
#' @keywords questionnaire brief scale 3-item
#' @export
#' @examples 
#' library(psych) #load the psych package
#' data("bfi") #read in the bfi dataset from the psych package
#' test <- as.data.frame(cbind(bfi$A1,bfi$A2,bfi$A3,bfi$A4,bfi$A5)) #save the 5 agreeableness items as a dataframe
#' var_3item_combo(test) #run the function on the dataframe


var_3item_combo <- function(x) {
  
  item_list <- colnames(x)
  comb3_list <- combn(item_list, 3)
  record3 <- matrix((rep(NA,3*ncol(comb3_list))),ncol = 3)
  
  x$mean_score <- rowMeans(x, na.rm = TRUE) #calculate rowmeans before the loop
  
  for (i in 1:ncol(comb3_list)){
    a <- comb3_list[1,i]
    b <- comb3_list[2,i]
    c <- comb3_list[3,i]
    
    
    combo <- x[,c(a,b,c)]
    combo$combo_avg <- rowMeans(combo, na.rm = TRUE)
    combo$whole_avg <- x$mean_score #again, mean score must be calculated before this loop
    
    record3[i,1] <- paste(a,b,c)
    
    record3[i,2] <- round((cor.test(combo$combo_avg, combo$whole_avg)$estimate)^2, digits = 5)*100
  }
  
  df_3items <- as.data.frame(cbind(record3[,1],as.numeric(record3[,2])))
  colnames(df_3items) <- c("items","var_explained")
  
  test <- df_3items[which.max(df_3items$var_explained),]
  
  out <- paste0("Combination based on items ",test$items," explain the most variance in total score (",test$var_explained,"% variance explained). View(var_list) for the full list." )
  
  assign("var_list",df_3items,envir=.GlobalEnv)
  
  return(out)  
}



#' Find the 3-item short scale (criterion = Cronbach's alpha)
#'
#' This function tests all 3-item combinations possible in your questionnaire and returns the combo that has the highest internal consistency (based on Cronbach's alpha)
#' @param x The vector with all the items in the questionnaire. All columns must be numeric.
#' @keywords questionnaire brief scale 3-item cronbach alpha
#' @export
#' @examples 
#' library(psych) #load the psych package
#' data("bfi") #read in the bfi dataset from the psych package
#' test <- as.data.frame(cbind(bfi$A1,bfi$A2,bfi$A3,bfi$A4,bfi$A5)) #save the 5 agreeableness items as a dataframe
#' alpha_3item_combo(test) #run the function on the dataframe


alpha_3item_combo <- function(x) {
  
  item_list <- colnames(x)
  comb3_list <- combn(item_list, 3)
  record3 <- matrix((rep(NA,3*ncol(comb3_list))),ncol = 3)
  
  for (i in 1:ncol(comb3_list)){
    a <- comb3_list[1,i]
    b <- comb3_list[2,i]
    c <- comb3_list[3,i]
  
    
    combo <- x[,c(a,b,c)]
    record3[i,2] <- round(as.numeric(suppressWarnings((psych::alpha(combo, check.keys = TRUE)$total$std.alpha))), digits = 3) #calculate alpha
    record3[i,1] <- paste(a,b,c)
    
  }
  
  df_3items <- as.data.frame(cbind(record3[,1],as.numeric(record3[,2])))
  colnames(df_3items) <- c("items","alpha")
  
  test <- df_3items[which.max(df_3items$alpha),]
  
  out <- paste0("Combination based on items ",test$items," has the highest internal consistency (Cronbach's alpha = ",test$alpha,"). View(alpha_list) for the full list." )
  
  assign("alpha_list",df_3items,envir=.GlobalEnv)
  
  return(out)  
}

