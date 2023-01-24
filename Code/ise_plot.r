ise_plot <- function(roc, observed, yhat, savepath="ise.png") {
    fnrs <- 1 - roc$sensitivities
    n_dropped <- rep(NA, length(fnrs))
    for (i in 1:length(n_dropped)) {
        n_dropped[i] <- sum(yhat < roc$thresholds[i], na.rm=T)
    }
    pct_drop <- n_dropped / length(yhat[!is.na(yhat)])
    fnr_df <- data.frame(FNR=fnrs, Pct_dropped=pct_drop)
    best_idx <- which.min(fnrs - pct_drop * 0.6)
    best_pct <- round(pct_drop[best_idx], 2) * 100
    g2 <- ggplot(data=fnr_df, aes(x=Pct_dropped, y=FNR)) +           
        geom_line(aes(color="ISE Curve")) +   
    geom_segment(aes(x=0, xend=1, y=0, 
        yend=0.6, color="Managerial Threshold"),               
        linetype="dotted") + xlab("Screening Rate") +  
        ylab("False Negative Rate")
        g2 <- g2 + annotate("text", x=0.85, y=0.05, 
            label= paste0("Best at:", best_pct, "%"))
        g2 <- g2 + scale_colour_manual("", 
            breaks = c("ISE Curve", "Managerial Threshold"),
            values = c("blue", "green")) + 
            theme(legend.position = "top")
ggsave(savepath, g2, width=4, height=4, units="in")                                          
        sacrifice = round(fnrs[best_idx], 2) * 100
        return(list(best_pct=best_pct, best_idx=best_idx,
  sacrifice=sacrifice))
    }
ise_object = ise_plot(roc, observed, yhat)   
