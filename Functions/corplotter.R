corplotter <- function(df, yaxis, xaxis, title, xaxisname = "xaxis", yaxisname= "yaxis"){
  
  dfcor <- signif(cor(eval(parse(text = paste0("df$", yaxis)
  )), 
  eval(parse(text = paste0("df$", xaxis))), 
  use = 'complete.obs'),2)
  
  ypos <- mean(eval(parse(text = paste0("df$", yaxis))), na.rm = TRUE)/1.5
  xpos <-range(eval(parse(text = paste0("df$", xaxis))), na.rm = TRUE) 
  xpos <- (xpos[2]-xpos[1])*.8+xpos[1]
  
  
  df %>% ggplot(., aes_string(y=yaxis, x= xaxis )) + geom_point() +
    labs(title = title,
         x = xaxisname,
         y = yaxisname) +
    theme(plot.title = element_text(hjust = 0.5)) +
    annotate("text",
             label = paste0("Correlation = ",
                            signif(dfcor,2)),
             x = xpos , y = ypos, size = 5) +
    geom_smooth(method = "lm", se = FALSE)
}
