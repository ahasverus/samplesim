#' Internal function
#' @noRd

credintt <- function(data, interval) {
  
  out <- round((length(data) - (interval / 100) * length(data)) / 2)
  
  c(sort(data)[out + 1], sort(data)[length(data) - out - 1])
}



#' Internal function
#' @noRd

credint <- function(data, interval) {
  
  apply(data, 2, credintt, interval = interval)
}



#' Internal function
#' @noRd

meansd_nd <- function(data, n, input) {
  
  if (input == "means") {
    
    mn <- grep("mean", tolower(colnames(data)))
    sd <- grep("sd", tolower(colnames(data)))
    
    data_s <- data
    
    for (i in 1:length(mn)) {
      
      tmp <- stats::rnorm(n, data[1, mn[i]], data[1, sd[i]])
      data_s[1, mn[i]] <- mean(tmp)
      data_s[1, sd[i]] <- stats::sd(tmp)
    }
    
    return(data_s)
    
  } else {
    
    mn <- grep("mean", tolower(colnames(data)))
    sd <- grep("sd", tolower(colnames(data)))
    
    data_s <- data
    
    tmp <- data.frame()
    
    for (i in 1:length(mn)) {
      
      if (i == 1) {
        
        tmp <- stats::rnorm(n, data[1, mn[i]], data[1, sd[i]])
        
      } else {
        
        tmp <- cbind(tmp, stats::rnorm(n, data[1, mn[i]], data[1, sd[i]]))
      }
    }
    colnames(tmp) <- gsub("Mean", "", colnames(data[mn]))
    
    return(tmp)
  }
}
