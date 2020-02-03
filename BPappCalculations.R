BPappCalculations <- function(age, height, sex, BP, SBP.DBP){
  source(paste0("splineKnots_",sex,".R"))
  table <- percentileCalculator(age, height, sex, SBP.DBP)
  pcnt <- which.min(abs(BP-table$fxsys))
  median <- table$fxsys[50]
  elevated <- table$fxsys[90]
  stage1 <- table$fxsys[95]
  stage2 <- stage1+12
  plot <- ggplot(table, aes(x = fxsys, y = percentile*100)) +
    geom_area(data=table[which(table$fxsys<=elevated),], fill = "green", alpha = 0.25)+
    geom_area(data=table[which(table$fxsys>=elevated & table$fxsys<=stage1),], fill = "yellow", alpha = 0.25)+
    geom_area(data=table[which(table$fxsys>=stage1),], fill = "red", alpha = 0.25)+
    geom_line(size=1) +
    geom_point(aes(x = BP, y = pcnt), shape = 21, color = "black", fill= "red", size = 3, stroke = 2) +
    theme_classic() +
    labs(x="mmHg", y="Percentile") +
    theme(plot.title = element_text(hjust = 0.5,
                                    size = 16,
                                    face = "bold"))
  
  return(list(
    "table" = table,
    "pcnt" = pcnt,
    "median" = median,
    "elevated" = elevated,
    "stage1" = stage1,
    "stage2" = stage2,
    "plot" = plot
  ))
}  
