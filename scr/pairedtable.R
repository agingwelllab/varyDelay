pairedttable <- function(data, var.names){
  # data is a data frame in wide format with only an ID column in front of columns of interest
  # var.names is a vector with the names of the columns you want to compare
  dt <- matrix(nrow = length(var.names), ncol = length(var.names)-1)
  d1 <- matrix(nrow = length(var.names), ncol = length(var.names)-1)
  d2 <- matrix(nrow = length(var.names), ncol = length(var.names)-1)
  d3 <- matrix(nrow = length(var.names), ncol = length(var.names)-1)
  count = 0
  for (l1 in 1:(length(var.names)-1)){
    print(l1)
    var1 = var.names[l1]
    print(var1)
    for (l2 in (l1+1):length(var.names)) {
      print(l2)
      var2 = var.names[l2]
      print(var2)
      t <- t.test(unlist(data[,l1+1], use.names = FALSE), unlist(data[,l2+1], use.names = FALSE), paired = TRUE, na.action=na.omit)
      print(t)
      dt[l1, l2-1] <- round(t$statistic,2)
      d1[l1, l2-1] <- paste0('[', round(t$conf.int[1],2), ',', round(t$conf.int[2],2), ']')
      d2[l1, l2-1] <- round(t$p.value, 4)
      d3[l1, l2-1] <- t$parameter
      count = count+1
    }
  }
  #criticalp = 0.05/count
  row.names(dt) <- var.names
  colnames(dt) <- var.names[2:length(var.names)]
  row.names(d1) <- var.names
  colnames(d1) <- var.names[2:length(var.names)]
  row.names(d2) <- var.names
  colnames(d2) <- var.names[2:length(var.names)]
  row.names(d3) <- var.names
  colnames(d3) <- var.names[2:length(var.names)]
  output <- list('tval' = dt, 'CI' = d1, 'p values' = d2, 'df' = d3)
  return(output)
}

