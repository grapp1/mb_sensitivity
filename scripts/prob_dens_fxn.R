# 20190625 probability density function


pdfxn <- function(df_exited,max_time,bin_length,column = "age"){
  library(ggplot2)

  time_bins <- matrix(seq(0,max_time,bin_length))
  df_exited_pdf <- df_exited
  col_num <- which(colnames(df_exited_pdf)==column)
  df_exited_pdf$bin <- cut(df_exited_pdf[,col_num], c(time_bins), include.lowest = TRUE)
  
  df_exited_pdf <- aggregate(df_exited_pdf$mass, by = list(bin = df_exited_pdf$bin), FUN = sum)
  df_exited_pdf$age <- as.integer(df_exited_pdf$bin)*bin_length
  
  colnames(df_exited_pdf) <- c("bin","Density",column)
  
  max_density <- max(df_exited_pdf$Density)
  df_exited_pdf$Density_norm <- df_exited_pdf$Density/max_density
  
  sum_norm <- sum(df_exited_pdf$Density_norm)
  df_exited_pdf$Density_pdf <- df_exited_pdf$Density_norm/sum_norm
  
  return(df_exited_pdf)
  
}
