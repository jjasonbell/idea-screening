source('lasso_fun.R')
main_data_path <- "master_data.csv"
vars <- c("wo_goog", "wc", "peer_dev_lda", "peer_dev_jacc", "ks", "min_nf")
data_object <- data_make(input_file=main_data_path, 
               varnames =vars, filter_on_X='impute', use_winner=T)

X <- data_object$X
Xso <- X
X = X[, c(5, 7)]
X_orig <- data_object$X_object
idea_df <- data_object$idea_df
contest <- idea_df$contest
shortlist <- idea_df$shortlist
winner <- idea_df$winner
pen_vec <- c(rep(1, ncol(X) - 1), 0)

oos_yhat <- rep(NA, length=0)
oos_y_test <- rep(NA, length=0)
oos_doomed <- rep(NA, length=0)
oos_winner = rep(NA, length=0)
total_dropped = 0
winners_dropped = 0
for (i in 1:length(levels(contest))) {
    contest_i <- levels(contest)[i]
    train_ind <- contest != contest_i
    test_ind <- contest == contest_i
    setwd(paste0(contest_i))
    df_so_i <- data.frame(y=shortlist[train_ind], Xso[train_ind,])
    so_i1 <- glm(y ~ peer_dev_lda, data=df_so_i, family="binomial")
    so_i2 <- glm(y ~ wc, data=df_so_i, family="binomial")
    quantile_i = 0.25
    X_test_so_i <- data.frame(Xso[test_ind, ])
    yhat_so_i1 <- predict(so_i1, X_test_so_i, type="response")
    yhat_so_i1 <- ifelse(yhat_so_i1 < quantile(yhat_so_i1, quantile_i), 1, 0)
    yhat_so_i2 <- predict(so_i2, X_test_so_i, type="response")
    yhat_so_i2 <- ifelse(yhat_so_i2 < quantile(yhat_so_i2, quantile_i), 1, 0)
    
    yhat_so_i <-  yhat_so_i1 * yhat_so_i2
    total_dropped <- sum(yhat_so_i) + total_dropped
    df_contest_i <- idea_df[test_ind, ]
    target_bin_i <- ifelse(df_contest_i$winner[yhat_so_i == 1] == 1, 1, 0)
    if (sum(target_bin_i) > 0) {
        cat("WINNER DROPPED FOR ", contest_i, "\n")
        winners_dropped = winners_dropped + sum(target_bin_i)
    }
    lasso <- get_model(X[train_ind, ], X[test_ind,], 
        contest[train_ind], shortlist[train_ind],
        display=T, save_coefs=F, pen_vec=pen_vec, winner[train_ind])
     oos_doomed <- c(oos_doomed, yhat_so_i)
     oos_yhat <- c(oos_yhat, lasso$yhat)
     oos_y_test <- c(oos_y_test, shortlist[test_ind])
     oos_winner = c(oos_winner, winner[test_ind])
    setwd('..')
    cat('\n\n')
}
winner2 <- oos_winner[oos_doomed==0]
yhat2 <- oos_yhat[oos_doomed==0]
widx2 = which(winner2[order(yhat2)] == 1)
widx = which(oos_winner[order(oos_yhat)] == 1)
garbage_dropped_n = total_dropped + widx2[1]
pct_garbage_dropped = round(garbage_dropped_n / nrow(X), 2) * 100
cat("Can screen out ", garbage_dropped_n, " ideas w/o losing winners.
This is ", pct_garbage_dropped, "% of ideas.")
   oos_yhat = ifelse(oos_doomed == 1, 0, oos_yhat) 

oos_yhat = ifelse(oos_doomed == 1, 0, oos_yhat)
cat("AUC: ", roc(oos_y_test, oos_yhat)$auc)
setwd('../Output')
results <- list(observed=oos_y_test, yhat=oos_yhat, X=X, X_orig=X_orig, idea_df=idea_df)
res_path = paste0("../Output/twostage_res_", vars[1], "_", length(vars), ".RData")
save(results, file=res_path)
