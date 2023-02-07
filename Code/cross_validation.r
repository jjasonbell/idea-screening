# Cross validation over contests 
source('Code/lasso_fun.R')
main_data_path <- "Data/example_cv_data.csv"
vars <- c("peer_dev_jacc", "peer_dev_lda", "cc", "degree", "burt", "*_goog")
#vars = c("peer_dev_lda", "cc_orig", "ks_goog")
data_object <- data_make(input_file=main_data_path, 
    varnames =vars, filter_on_X='impute', use_winner = F)

X <- data_object$X
orig_X_names = colnames(X)
X_orig <- data_object$X_orig
idea_df <- data_object$idea_df
contest <- idea_df$contest
shortlist <- idea_df$shortlist
winner <- idea_df$winner 
pen_vec <- c(rep(1, ncol(X) - 1), 0)

lasso_is <- get_model(X, X, contest, shortlist, T, T, pen_vec)
oos_yhat <- rep(NA, length=0)
oos_y_test <- rep(NA, length=0)
oos_winner = rep(NA, length=0)
oos_idea_id = rep("", length=0)
for (i in 1:length(levels(contest))) {
    contest_i <- levels(contest)[i]
    train_ind <- contest != contest_i
    test_ind <- contest == contest_i
      
    lasso <- get_model(X[train_ind, ], X[test_ind, ], 
        contest[train_ind], shortlist[train_ind],
        display=T, save_coefs=F, pen_vec=pen_vec, winner[train_ind])
    oos_yhat <- c(oos_yhat, lasso$yhat)
    oos_winner = c(oos_winner, winner[test_ind]) 
    oos_idea_id = c(oos_idea_id, as.character(idea_df$idea_id[test_ind]))
    oos_y_test <- c(oos_y_test, shortlist[test_ind])
}

results <- list(observed=oos_y_test, yhat=oos_yhat, X=X, X_orig=X_orig, idea_df=idea_df)
res_path = paste0("Output/lasso_res_", vars[1], "_", length(vars), ".RData")
save(results, file=res_path)
source("Code/ise_plot.r")
