library(glmnet)
library(pROC)
library(foreach)
library(DMwR2)
library(MLmetrics)
library(doParallel)
library(yardstick)
numCores <- detectCores()
registerDoParallel(numCores)  

data_make <- function(input_file, varnames=c("*nf*", "*jm*", "ks*", "tc", "cc", "cc_orig", "degree", "peer_dev*", "burt"),
    filter_on_X='impute', use_winner=F) {
    idea_df <- read.csv(input_file, header=T,stringsAsFactors=T)
    contest <- idea_df[, names(idea_df)=="contest"]
    col_list <- list()
    for (i in 1:length(varnames)) {
        var_i <- varnames[i]
        grep_target <- glob2rx(var_i)
        col_list[[i]] <- grep(grep_target, names(idea_df))
    }
    X <- as.matrix(idea_df[, unlist(col_list)])
    if (filter_on_X == 'impute') {
        getmode <- function(v) {
            uniqv <- unique(v)
            uniqv[which.max(tabulate(match(v, uniqv)))]
        }
        if ('cc' %in% colnames(X)) {
            cc_idx <- which(colnames(X) == 'cc')
            X_cc <- X[, cc_idx]
            X_cc[is.na(X_cc)] <- getmode(X_cc)
            X[, cc_idx] <- X_cc 
        }
        if ('cc_orig' %in% colnames(X)) {
            cc_idx <- which(colnames(X) == 'cc_orig')
            X_cc <- X[, cc_idx]
            X_cc[is.na(X_cc)] <- getmode(X_cc)
            X[, cc_idx] <- X_cc 
        }
        if (dim(X)[2] == 1) {
            non_cc <- 1
        } else {
            non_cc <- which(!(colnames(X) %in% c('cc', 'cc_orig')))
        }
        for (k in 1:length(non_cc)) {
            spot_k <- non_cc[k]
            X_spot_k <- X[, spot_k]
            X_spot_k[is.na(X_spot_k)] <- median(X_spot_k, na.rm=T)
            X[, spot_k] <- X_spot_k
        }
    } else if (filter_on_X == 'drop') {
        X_filter <- rowSums(is.na(X)) == 0
        idea_df <- idea_df[X_filter, ]
        X <- X[X_filter, ]
    }
    if (ncol(X) > 1) {
        nan_cols <- colSums(is.nan(X)) > 0.1 * nrow(X)
        X <- X[, !nan_cols]
    } else if (ncol(X) == 1) {
        colnames(X) <- varnames
    }
    X_orig <- X
    X <- scale(X)
    X <- add_contest_short_prob(X, idea_df, use_winner = use_winner)
    return(list(idea_df=idea_df, X=X, X_orig=X_orig))
}


add_contest_short_prob <- function(X, idea_df, use_winner=F) {
    contest_names <- levels(idea_df$contest)
    prob_vec <- rep(0, nrow(X))
    for (i in 1:length(contest_names)) {
        contest_i <- contest_names[i]
        contest_idx <- idea_df$contest == contest_i
        prob_vec[contest_idx] <- sum(idea_df$shortlist[contest_idx]) / 
            	sum(contest_idx)
        if (use_winner) {
           prob_vec[contest_idx] = sum(idea_df$winner[contest_idx] ==1) /
               sum(contest_idx)
        }
    }
    colnames0 <- colnames(X)
    X <- cbind(X, prob_vec)
    colnames(X) <- c(colnames0, "short_prob")
    return(X)
}


get_model <- function(X, newx, contest_vec, y_train, 
        display=F, save_coefs=F, pen_vec=rep(1, ncol(X)), winner) {
    X_drop <- ifelse(is.na(rowSums(X)), T, F)
    X <- X[!X_drop, ]
    contest_vec <- contest_vec[!X_drop]
    y_train <- y_train[!X_drop]
    NC <- length(unique(contest_vec))
    contests <- unique(contest_vec)
    
    lasso <- glmnet(X, as.factor(y_train), alpha=1, family="binomial", penalty.factor = pen_vec)
    grid <- lasso$lambda
   
    cv_obj <- foreach(l=1:length(grid), .combine=c) %dopar% {
        pmetric_l <- rep(NA, NC)
        for (i in 1:NC) {
            holdout <- contest_vec == contests[i]
            X_i <- X[!holdout, ]
            X_holdout <- X[holdout, ]
            y_holdout <- y_train[holdout]
            y_i <- y_train[!holdout]
            fraction_0 = rep(1 - sum(y_i) / length(y_i), sum(y_i == 0))
            fraction_1 <- rep(1 - sum(y_i == 1) / length(y_i), sum(y_i == 1))
            glm_wts = numeric(length(y_i))
            glm_wts[y_i == 0] = fraction_0
            glm_wts[y_i == 1] = fraction_1
            model_i <- glmnet(X_i, as.factor(y_i), alpha=1, family="binomial",
                penalty.factor = pen_vec, weights=glm_wts)
            yhat_li <- predict(model_i, newx=X_holdout, s=grid[l], type="response")
            coefs_li <- predict(model_i, s=grid[l], type="coef")
            nvars_li <- sum(coefs_li != 0)
            #roc_obj <- roc(y_holdout, c(yhat_li), quiet=T)
            #pmetric_li <-roc_obj$auc
            y_holdout_temp <- y_holdout[order(c(yhat_li))]
            pmetric_li <- sum(which(y_holdout_temp == 1))
            #y_holdout_temp <- winner[order(c(yhat_li))]
            #pmetric_li = which(y_holdout_temp == 1)[1]
            y_bin_pred <- ifelse(yhat_li > quantile(yhat_li, 0.25), 1, 0)
            y_bin_pred <- factor(y_bin_pred, levels=c("0", "1"))
            pmetric_l[i] <- ifelse(nvars_li > 2, pmetric_li, 1)
        }
        mean_pmetric_l <- mean(pmetric_l)
        mean_pmetric_l
    }
    
    pmetric <- cv_obj
    dg_flag <- rep(F, length(grid))
    for (i in 1:length(grid)) {
        coefs_temp <- predict(lasso, s=grid[i], type="coef")
        nvars_temp <- sum(coefs_temp != 0)
        dg_flag[i] <- ifelse(nvars_temp > 2, F, T)
    }
    grid_ndg <- grid[!dg_flag]
    pmetric_ndg <- pmetric[!dg_flag]
    best.lambda <- grid_ndg[which.max(pmetric_ndg)]
    if (display) print(predict(lasso, s=best.lambda, type="coef"))
    if (save_coefs) {
        a <- predict(lasso, s=best.lambda, type="coef")
        a <- as.data.frame(round(as.matrix(a), 2))
        rownames(a) <- change_names(rownames(a))
        a$vars <- rownames(a)
        names(a)[1] <- "coefs"
        a <- a[a$coefs != 0, ]
        a <- a[order(abs(a$coefs), decreasing=T), ]
        write.csv(a, "lasso_coefficients.csv")
    }
    yhat <- predict(lasso, newx=newx, s=best.lambda, type="response")
    return(list(yhat=yhat, lasso=lasso,best.lambda=best.lambda))
}

