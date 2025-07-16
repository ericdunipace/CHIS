
# with dummy data

#### Variable Selection SMI ####
novary_cols <- grepl("ti7_11", colnames(chis))
missing_cols <- sapply(chis, function(x) x %>% is.na %>% any)
cols <- !(missing_cols | novary_cols |grepl("raked|fnwgt|region|p_tf|orthog_tf|geometry", colnames(chis)) | 
            grepl(paste(colnames(california_shapefile), collapse = "|"), colnames(chis)))

xform <- as.formula(~. -depPCA - s_depPCA - max_K6 - K6 - worst_K6 - K6_ge_13 -  worst_K6_ge_13- baseid - srcnty - tg11 - tg12 - tg13 - tg14 - tg15 - tg16 - tf31 - tf32 - tf33 - tf34 - tf35 - tf36
                    -dstrs30 - dstrsyr - dstrs12 - distress 
                    - dstrstn_p1
                    - tf30 # if using depPCA
                    - tk1 - tk2 - tk3 -tk4 - tk5 # suicide vars, if using dep PCA
                    - fips_cnt - te24a - te24 - tsvrunit - tsvarstr - bmi_p - povgwd_p - povll- povll2_p -  povgwd_p1 - povll2_p - povll2_p1 -   povll2_p1v2 - srage - bmi_p - intv_mode2 - wghtk_p - wghtp_p - ta1yr - CNTY_ID - wghtp - survey_dates 
                    - acmdmt_p1 - tadate_mm # date and year of survey
                    # - ccpreg19 #covered cal price regions
                    - ta2 - ma7 - ma7_p
                    - ti2h_a - ti2h_b - ti2h_c - ti2h_d - ti2h_e - ti2h_f # all white vars
                    - asian10 - asian8 - asian9 - asnhp2_p - asnhp2 # all asian vars
                    - alcohol # collinear with te22 (ever had a few sips of alcohol)
                    - acmdnum #duplicate of TF16
                    - usual5tp
                    + I(as.numeric(ti3 == "United States")) - ti3
                    + I(as.factor(survey_months)) - survey_years - survey_months
                    + I(as.numeric(as.character(povgwd_p)))
                    + I(as.numeric(as.character(povll2_p)))
                    + I(as.numeric(as.character(povll2_p1v2)))
                    + I(as.numeric(as.character(povgwd_p)))
                    + I(as.numeric(tc38=='Yes')* te81) - tc38 - te81
                    # + I(as.numeric(tc38=='Yes')):te19 - te19 
                    + te19
                    # tc38 = EVER SMOKED CIGARETTES
                    # te19 = # OF DAYS SMOKED CIGARETTES IN PAST 30 DAYS
                    + I(as.numeric(te79=='Yes')* te82) - te79 - te82 - te82_p1
                    + I(as.numeric(te79=='Yes')* te80) - te80
                    + I(as.numeric(tf28=='Yes')* tf29v2) - tf29v2 - tf28
                    + I(as.numeric(te19 != 'Inapplicable' & te19 != 'None') * te20) - te20 - te19 
                    # te19 = # OF DAYS SMOKED CIGARETTES IN PAST 30 DAYS
                    # te20 = # OF CIGARETTES SMOKED PER DAY IN PAST 30 DAYS
                    + I(as.numeric(ti6 > -1) * ti6) - ti6
                    + as.factor(year) - year
)
Xmf <- model.frame(xform, chis[,cols])
X <- model.matrix(terms(Xmf), Xmf)
# if (any(matrixStats::colSds(X) == 0)) {
#   stop("There are columns with zero variance in the model matrix.")
# }
# Y <- scale(chis$max_K6)
Y <- scale(chis$depPCA,
           scale = matrixStats::weightedSd(chis$depPCA,chis_design$pweights),
           center = weighted.mean(chis$depPCA, chis_design$pweights))

# lambs <- glmnet::glmnet(x = X, y = Y, weights = chis$fngwt0,
#                 alpha = 1, family = "gaussian", standardize = TRUE, intercept = TRUE, lambda.min.ratio = 0.001)$lambda

k <- 10
n <- nrow(chis)
p <- ncol(X)
w <- chis$fnwgt0
penalty <- "grp.lasso"
groups <- attributes(X)$assign 
group_list <- split(seq_along(groups), groups)
set.seed(1231243)
folds <- sample(rep(1:k, length.out = n))

# cont_vars <- which(apply(X,2,function(j) length(unique(j))>2))
# i_vars <- grepl("I(", names(cont_vars), fixed = TRUE)
# cont_vars <- cont_vars[!i_vars]
# 
# std_cont <- sapply(cont_vars, function(i)
#   scale(X[,i], center = weighted.mean(X[,i],w),
#         scale = matrixStats::weightedSd(X[,i],w))
# )
# X[,cont_vars] <- std_cont

all_vars <- colnames(X)
i_vars <- grepl("I(", (all_vars), fixed = TRUE)
all_vars <- all_vars[!i_vars][-1]

std_cont <- sapply(all_vars, function(i)
  scale(X[,i], center = weighted.mean(X[,i],w),
        scale = matrixStats::weightedSd(X[,i],w))
)
X[,all_vars] <- std_cont


for(i in which(grepl("I(", colnames(X), fixed = TRUE))) {
  if (length(unique(X[,i])) > 2) {
    temp <-  X[,i]
    tnonzero <- temp != 0
    temp_nonzero <- temp[tnonzero]
    X[tnonzero,i] <- (temp_nonzero - weighted.mean(temp_nonzero, w[tnonzero]))/matrixStats::weightedSd(X[,i], w)
  } else if (length(unique(X[,i])) %in% 2) {
    X[,i] <- (X[,i] - weighted.mean(X[,i], w)) / matrixStats::weightedSd(X[,i], w)
  }
}

# matrixStats::colWeightedSds(X[,all_vars],w) 
# matrixStats::colWeightedMeans(X[,all_vars],w)

X_s <- Matrix::Matrix(X, sparse = TRUE)

xtx <- crossprod(X_s, X_s * w/sum(w) )
xty <- crossprod(X_s, Y * w/sum(w) )

time1 <- proc.time()[3]
full <- oem::oem.xtx(as.matrix(xtx), as.matrix(xty), family = "gaussian",
                     penalty = penalty,
                     lambda.min.ratio = 0.001,
                     nlambda = 100,
                     tau = 0.5, alpha = 0.5,
                     groups = groups,
                     group.weights = c(0, rep(1, length(unique(groups)) - 1))
)
print(proc.time()[3] - time1)

lambdas <- full$lambda[[1]]

# 4. Cross-validation function
cv_loss <- function(lambda) {
  library(CVXR)
  library(Matrix)
  fold_mse <- numeric(k)
  
  for (i in 1:k) {
    test_idx <- which(folds == i)
    train_idx <- setdiff(1:n, test_idx)
    
    X_train <- X[train_idx, , drop = FALSE]
    y_train <- Y[train_idx]
    X_test  <- X[test_idx, , drop = FALSE]
    y_test  <- Y[test_idx]
    
    w_test  <- w[test_idx]
    w_train <- w[train_idx]
    
    # xtx <- crossprod(X_train, X_train * n*w_train)/sum(w_train)
    # xty <- crossprod(X_train, y_train * n * w_train)/sum(w_train)
    
    # Fit lasso using CVXR
    beta <- Variable(p)
    loss <- sum_entries( w_train*(X_train %*% beta - y_train)^2) *0.5/n
    group_penalty <- Reduce("+", sapply(group_list, function(g) norm2(beta[g])))
    problem <- Problem(Minimize(loss + lambda * group_penalty))
    result <- solve(problem)
    
    beta_hat <- result$getValue(beta)
    preds <- X_test %*% beta_hat
    fold_mse[i] <- mean(w_test * (y_test - preds)^2)
  }
  
  c(mean(fold_mse), var(fold_mse))
}


cv_loss_oem <- function(i, X, Y, groups, folds) {
  require(oem)
  
  test_idx <- which(folds == i)
  train_idx <- setdiff(1:n, test_idx)
  
  X_train <- X[train_idx, , drop = FALSE]
  y_train <- Y[train_idx]
  X_test  <- X[test_idx , , drop = FALSE]
  y_test  <- Y[test_idx]
  
  w_test  <- w[test_idx]
  w_train <- w[train_idx]
  xtx <- Matrix::crossprod(X_train, X_train * w_train/sum(w_train))
  xty <- Matrix::crossprod(X_train, y_train * w_train/sum(w_train))
  
  result <- oem::oem.xtx(xtx = as.matrix(xtx), 
                         xty = as.matrix(xty), 
                         family = "gaussian",
                         penalty = penalty,
                         lambda = lambdas,
                         alpha = 0.5,
                         tau = 0.5,
                         groups = groups,
                         group.weights = c(0, rep(1, length(unique(groups)) - 1))
  )
  betas  <- result$beta[[1]]
  preds  <- as.matrix( X_test %*% betas )
  fold_mse <- matrixStats::colWeightedMeans( (y_test - preds)^2, w = w_test)
  
  fold_mse
}

cv.grplasso <- function(x, y, index, weights = rep(1, length(y)),
                        model = LinReg(), nfolds=5, foldid=NULL, nlambda=20,parallel = FALSE, standardize) {
  if(is.null(foldid)) {
    foldid = sample(rep(seq(nfolds), length = length(y)))
    
  }
  lambda.max <- lambdamax(x, y, index = index, 
                          weights = weights,
                          model = model,
                          standardize = standardize)
  lambda.min = 0.0001 * lambda.max 
  
  lambda=exp(seq(log(lambda.max), log(lambda.min), length.out = nlambda))
  
  ## Fit the solution path on the lambda grid
  if (parallel) {
    library(doParallel)
    cl = parallel::makeCluster(nfolds, outfile="")
    doParallel::registerDoParallel(cl)
    on.exit(parallel::stopCluster(cl))
    
    outlist = foreach(i = seq(nfolds),.combine = list,.multicombine = TRUE, .packages = c("grplasso")) %dopar% 
      {
        print(paste(i,"th fold",sep=""))
        which <- foldid == i
        x_sub <- x[!which, , drop = FALSE]
        y_sub <- y[!which]
        w_sub <- weights[!which]
        
        fit <- grplasso(x = x_sub, 
                        y = y_sub, index = index, 
                        w = w_sub/sum(w_sub),
                        lambda = lambda, model = model,
                        standardize = standardize,
                        control = grpl.control(trace = 0L))
        pred <- predict(fit, x[which,,drop=FALSE])
        w_test <- weights[which]
        w_test <- w_test/sum(w_test)
        y_test <- y[which]
        sapply(1:length(lambda), function(j) 
          model@nloglik(y_test, pred[,j], w_test))
      }
    # doParallel::stopImplicitCluster()
  } else {   
    outlist <- vector("list", nfolds)
    for (i in seq(nfolds)) {
      print(paste(i,"th fold",sep=""))
      which <- foldid == i
      x_sub <- x[!which, , drop = FALSE]
      y_sub <- y[!which]
      w_sub <- weights[!which]
      
      fit <- grplasso(x = x_sub, 
                      y = y_sub, index = index, 
                      weights = w_sub,
                      lambda = lambda, model = model,
                      standardize = standardize,
                      control = grpl.control(trace = 0L))
      pred <- predict(fit, x[which,,drop=FALSE])
      w_test <- weights[which]
      w_test <- w_test/sum(w_test)
      y_test <- y[which]
      outlist[[i]] <- sapply(1:length(lambda), function(j) 
        model@nloglik(y_test, pred[,j], w_test))
    }  
  }
  
  mse.mat <- do.call(rbind, outlist)
  mse <- data.frame(cvm = colMeans(mse.mat),
                    cvse = matrixStats::colSds(mse.mat) / sqrt(nfolds),
                    lambda = lambda)
  
  id<-which.min(mse$cvm)
  lambda.min <- mse$lambda[id]
  lambda.1se <- max(mse$lambda[mse$cvm <= mse$cvm[id] + mse$cvse[id]])
  
  test <- grplasso(x =x, y=y, index=index, 
                   weights = weights, standardize = standardize,
                   lambda=c(lambda.1se,lambda.min), model=model)
  list(lambda=lambda, cvm=mse$cvm, cvse=mse$cvse, grplasso.fit=test, lambda.min=lambda.min,lambda.1se=lambda.1se,foldid=foldid)  
}

# 5. Run CV for all lambdas
cv_results <- tibble(
  i = rep(1:k, each = length(lambdas)),
  lambda = rep(lambdas, k),
  cv_mse = map(1:k, cv_loss_oem, X = X_s, Y = Y, groups = groups, folds = folds, .progress = TRUE) %>% unlist()
)

cv_sum <- cv_results %>% group_by(lambda) %>% 
  summarize(mse = mean(cv_mse),
            var = var(cv_mse)/k)

mins <- cv_sum %>% summarize(
  min = min(mse),
  lambda.min = lambda[which.min(mse)],
  min.1se = min(mse[mse>=(min + sqrt(var[which.min(mse)])) & lambda >= lambda.min]),
  lambda.1se = lambda[which(mse == min.1se & lambda >= lambda.min)],
)

plot(x = cv_sum$lambda %>% log(), y = cv_sum$mse, pch=19, ylab="MSE", xlab="log lambda", col = "red")
arrows(x0 = cv_sum$lambda %>% log(), y0 = cv_sum$mse - 1* sqrt(cv_sum$var), 
       x1 = cv_sum$lambda %>% log(), y1 = cv_sum$mse + 1* sqrt(cv_sum$var),
       angle = 90, code = 3, length = 0.05)
abline(v = mins$lambda.min %>% log, lty = 3)
abline(v = mins$lambda.1se %>% log, lty = 3)

group_beta <- full$beta[[1]][,which(lambdas == mins$lambda.1se)]
nonzero_beta_group <- as.numeric(group_beta) != 0
group_beta[nonzero_beta_group]

1 - weighted.mean(as.numeric((Y - (X %*% group_beta))^2), chis$fnwgt0/sum(chis$fnwgt0))/1.0 #(divide by 1 b/c standardized)

group_beta_min <- full$beta[[1]][,which(lambdas == mins$lambda.min)]
nonzero_beta_min <- as.numeric(group_beta_min) != 0
group_beta_min[nonzero_beta_min]

nms <- names(group_beta[nonzero_beta_group])

whichcols <- attributes(X)$assign[nonzero_beta_group]
sapply(unique(attr(terms(Xmf), "term.labels")[whichcols]), function(i) attr(chis[[i]], "label"))

set.seed(103248108)
lasso.cv <- glmnet::cv.glmnet(x = X[,-1], y = Y, 
                              # alpha = 0.5,
                              weights = chis$fnwgt0,
                              family = "gaussian",
                              lambda.min.ratio = 0.001,
                              intercept = TRUE,
                              standardize = TRUE
)
print(lasso.cv)
plot(lasso.cv)

beta_lasso <- glmnet::coef.glmnet(lasso.cv, s = lasso.cv$lambda.1se)
nonzero_beta <- as.numeric(beta_lasso) != 0

plot( y = Y, x = X %*% beta_lasso)
1 - weighted.mean(as.numeric((Y - (X %*% beta_lasso))^2), chis$fnwgt0/sum(chis$fnwgt0))/1.0 #(divide by 1 b/c standardized)

rownames(beta_lasso)[nonzero_beta]
beta_lasso[nonzero_beta,]

nms <- rownames(beta_lasso)[nonzero_beta]

whichcols <- attributes(X)$assign[sapply(nms, function(n) grep(n, colnames(X), fixed = TRUE))]
sapply(attr(terms(Xmf), "term.labels")[whichcols], function(i) attr(chis[[i]], "label"))

set.seed(103248108)
tf45col <- grep("tf45No", colnames(X))
Y_tf45 <- 1-X[,tf45col]
lasso.cv.tf <- glmnet::cv.glmnet(x = X[,-c(1,tf45col)], y = Y_tf45, 
                                 weights = chis$fnwgt0/sum(chis$fnwgt0),
                                 family = "binomial",
                                 lambda.min.ratio = 0.001,
                                 intercept = TRUE,
                                 standardize = TRUE
)
print(lasso.cv.tf)
plot(lasso.cv.tf)

s_Y_tf45 <- scale(Y_tf45, center = weighted.mean(Y_tf45,w),
                  scale = matrixStats::weightedSd(Y_tf45, w))
xty_tf45 <- Matrix::crossprod(X_s[,-c(tf45col)], s_Y_tf45 * w)
xts_tf45 <- Matrix::crossprod(X_s[,-c(tf45col)], X_s[,-c(tf45col)] * w)
groups.gglasso <- as.integer(factor(groups[-c(tf45col)]))

full.tf <- oem::oem.xtx(as.matrix(xts_tf45), as.matrix(xty_tf45), family = "gaussian",
                        penalty = penalty,
                        lambda.min.ratio = 0.001,
                        nlambda = 100,
                        tau = 0.5, alpha = 0.5,
                        groups = groups.gglasso
)
set.seed(234232)
folds.tf <- sample(rep(1:k, length.out = nrow(X_s)))
cv_results.tf <- tibble(
  i = rep(1:k, each = length(full.tf$lambda[[1]])),
  lambda = rep(full.tf$lambda[[1]], k),
  cv_mse = map(1:k, cv_loss_oem, X = X_s[,-c(tf45col)], Y = s_Y_tf45, groups = groups.gglasso, folds.tf, .progress = TRUE) %>% unlist()
)

cv_sum.tf <- cv_results.tf %>% group_by(lambda) %>% 
  summarize(mse = mean(cv_mse),
            var = var(cv_mse)/k)

mins.tf <- cv_sum.tf %>% summarize(
  min = min(mse),
  lambda.min = lambda[which.min(mse)],
  min.1se = min(mse[mse>=(min + sqrt(var[which.min(mse)])) & lambda >= lambda.min]),
  lambda.1se = lambda[which(mse == min.1se & lambda >= lambda.min)],
)

plot(x = cv_sum.tf$lambda %>% log(), y = cv_sum.tf$mse, pch=19, ylab="MSE", xlab="log lambda", col = "red")
arrows(x0 = cv_sum.tf$lambda %>% log(), y0 = cv_sum.tf$mse - 1* sqrt(cv_sum.tf$var), 
       x1 = cv_sum.tf$lambda %>% log(), y1 = cv_sum.tf$mse + 1* sqrt(cv_sum.tf$var),
       angle = 90, code = 3, length = 0.05)
abline(v = mins.tf$lambda.min %>% log, lty = 3)
abline(v = mins.tf$lambda.1se %>% log, lty = 3)

group_beta.tf <- full.tf$beta[[1]][,which(full.tf$lambda[[1]] == mins.tf$lambda.1se)]
nonzero_group_beta.tf <- as.numeric(group_beta.tf) != 0
group_beta.tf[nonzero_group_beta.tf]

beta_lasso.tf <- glmnet::coef.glmnet(lasso.cv.tf, s = lasso.cv.tf$lambda.1se)
nonzero_beta.tf <- as.numeric(beta_lasso.tf) != 0
rownames(beta_lasso.tf)[nonzero_beta.tf]
beta_lasso.tf[nonzero_beta.tf,]

nms.tf <- rownames(beta_lasso.tf)[nonzero_beta.tf]
whichcols.tf <- attributes(X)$assign[sapply(nms.tf, function(n) grep(n, colnames(X), fixed = TRUE))]
sapply(attr(terms(Xmf), "term.labels")[whichcols.tf], function(i) attr(chis[[i]], "label"))

# expand_idx <- mapply(function(w,i) {rep(i,w)}, w = w*(1/min(w)), i = 1:nrow(X_s)) %>% unlist()

library(grplasso)
groups.grplasso <- as.integer(factor(groups[-c(tf45col)]))
groups.grplasso[1] <- NA
set.seed(123123)
grp_tf <- cv.grplasso(x = X[,-c(tf45col)], y = as.numeric(Xmf$tf45 == "Yes"),
                      index = groups.grplasso, weights = w/sum(w),
                      model = LogReg(), standardize = FALSE,
                      nfolds = 10, nlambda = 100, parallel = TRUE)

plot(x = grp_tf$lambda %>% log(), y = grp_tf$cvm, pch=19, ylab="MSE", xlab="log lambda", col = "red")
arrows(x0 = grp_tf$lambda %>% log(), y0 = grp_tf$cvm - 1* (grp_tf$cvse), 
       x1 = grp_tf$lambda %>% log(), y1 = grp_tf$cvm + 1* (grp_tf$cvse),
       angle = 90, code = 3, length = 0.05)
abline(v = grp_tf$lambda.min %>% log, lty = 3)
abline(v = grp_tf$lambda.1se %>% log, lty = 3)

gl_beta_1se <- grp_tf$grplasso.fit$coefficients[,1]
nonzero_gl_beta_1se <- as.numeric(gl_beta_1se) != 0
gl_beta_1se[nonzero_gl_beta_1se]

gl_beta_min <- grp_tf$grplasso.fit$coefficients[,2]
nonzero_gl_beta_min <- as.numeric(gl_beta_min) != 0
gl_beta_min[nonzero_gl_beta_min]

expand_idx <- mapply(function(w,i) {rep(i,w)}, w = ceiling(w), i = 1:nrow(X_s)) %>% unlist()

X_expand <- X[expand_idx,-1]
Y_tf45_expand <- Y_tf45[expand_idx]

grp_tf <- gglasso::cv.gglasso(x = X_expand, 
                              y = (2L * Y_tf45_expand - 1L),
                              group = groups.gglasso,
                              pred.loss = "loss", 
                              loss = "logit",
                              nlambda = 100, 
                              lambda.factor = 1e-8,
                              nfolds = 10,
                              intercept = TRUE)

plot(grp_tf)

#### Variable Selection climate ####
novary_cols <- grepl("ti7_11", colnames(chis))
missing_cols <- sapply(chis, function(x) x %>% is.na %>% any)
shape_cols <- colnames(california_shapefile)
shape_cols <- shape_cols["county" != shape_cols]
cols <- !(missing_cols | novary_cols |grepl("raked|fnwgt|region|p_tf|orthog_tf|geometry", colnames(chis)) | 
            grepl(paste(shape_cols, collapse = "|"), colnames(chis)))

xform <- as.formula(~. - baseid - srcnty - tf45
                    - depPCA - s_depPCA
                    - worst_K6 - K6 - worst_K6_ge_13
                    - K6_ge_13
                    - tg11 - tg12 - tg13 - tg14 - tg15 - tg16 - tf31 - tf32 - tf33 - tf34 - tf35 - tf36
                    -dstrs30 - dstrsyr - dstrs12 - distress
                    - dstrstn_p1
                    - ppt_20yr_normal
                    # - tf30 # if using depPCA
                    # - tk1 - tk2 - tk3 -tk4 - tk5 # suicide vars, if using dep PCA
                    - bmi
                    - fips_cnt 
                    - te24a - te24 - tsvrunit - tsvarstr - bmi_p - povgwd_p - povll- povll2_p -  povgwd_p1 - povll2_p - povll2_p1 -   povll2_p1v2 - srage - bmi_p - intv_mode2 - wghtk_p - wghtp_p - ta1yr - CNTY_ID - wghtp - survey_dates 
                    - acmdmt_p1 - tadate_mm # date and year of survey
                    # - ccpreg19 #covered cal price regions
                    - ta2 - ma7 - ma7_p
                    - ti2h_a - ti2h_b - ti2h_c - ti2h_d - ti2h_e - ti2h_f # all white vars
                    - asian10 - asian8 - asian9 - asnhp2_p - asnhp2 # all asian vars
                    - alcohol # collinear with te22 (ever had a few sips of alcohol)
                    - acmdnum #duplicate of TF16
                    - usual5tp
                    + as.factor(bestzip) + as.factor(tract10)
                    - bestzip - tract10
                    - srstrata
                    + I(as.numeric(ti3 == "United States")) - ti3
                    + I(as.factor(survey_months)) - survey_years - survey_months
                    + I(as.numeric(as.character(povgwd_p)))
                    + I(as.numeric(as.character(povll2_p)))
                    + I(as.numeric(as.character(povll2_p1v2)))
                    + I(as.numeric(as.character(povgwd_p)))
                    + I(as.numeric(tc38=='Yes')* te81) - tc38 - te81
                    # + I(as.numeric(tc38=='Yes')):te19 - te19 
                    + te19
                    # tc38 = EVER SMOKED CIGARETTES
                    # te19 = # OF DAYS SMOKED CIGARETTES IN PAST 30 DAYS
                    + I(as.numeric(te79=='Yes')* te82) - te79 - te82 - te82_p1
                    + I(as.numeric(te79=='Yes')* te80) - te80
                    + I(as.numeric(tf28=='Yes')* tf29v2) - tf29v2 - tf28
                    + I(as.numeric(te19 != 'Inapplicable' & te19 != 'None') * te20) - te20 - te19 
                    # te19 = # OF DAYS SMOKED CIGARETTES IN PAST 30 DAYS
                    # te20 = # OF CIGARETTES SMOKED PER DAY IN PAST 30 DAYS
                    + I(as.numeric(ti6 > -1) * ti6) - ti6
                    + as.factor(year) - year
)
Xmf <- model.frame(xform, chis[,cols])
X <- model.matrix(terms(Xmf), Xmf)
Y_tf45 <- as.numeric(chis$tf45 == "Yes")
k <- 10
n <- nrow(chis)
p <- ncol(X)
w <- chis$fnwgt0
groups <- attributes(X)$assign 

# if (any(matrixStats::colSds(X) == 0)) {
#   stop("There are columns with zero variance in the model matrix.")
# }
# Y <- scale(chis$max_K6)
cv.grplasso <- function(x, y, index, weights = rep(1, length(y)),
                        model = LinReg(), nfolds=5, foldid=NULL, nlambda=20,parallel = FALSE, standardize) {
  if(is.null(foldid)) {
    foldid = sample(rep(seq(nfolds), length = length(y)))
    
  }
  lambda.max <- lambdamax(x, y, index = index, 
                          weights = weights,
                          model = model,
                          standardize = standardize)
  lambda.min = 0.0001 * lambda.max 
  
  lambda=exp(seq(log(lambda.max), log(lambda.min), length.out = nlambda))
  
  ## Fit the solution path on the lambda grid
  if (parallel) {
    library(doParallel)
    cl = parallel::makeCluster(nfolds, outfile="")
    doParallel::registerDoParallel(cl)
    on.exit(parallel::stopCluster(cl))
    
    outlist = foreach(i = seq(nfolds),.combine = list,.multicombine = TRUE, .packages = c("grplasso")) %dopar% 
      {
        print(paste(i,"th fold",sep=""))
        which <- foldid == i
        x_sub <- x[!which, , drop = FALSE]
        y_sub <- y[!which]
        w_sub <- weights[!which]
        
        fit <- grplasso(x = x_sub, 
                        y = y_sub, index = index, 
                        w = w_sub/sum(w_sub),
                        lambda = lambda, model = model,
                        standardize = standardize,
                        control = grpl.control(trace = 0L))
        pred <- predict(fit, x[which,,drop=FALSE])
        w_test <- weights[which]
        w_test <- w_test/sum(w_test)
        y_test <- y[which]
        sapply(1:length(lambda), function(j) 
          model@nloglik(y_test, pred[,j], w_test))
      }
    # doParallel::stopImplicitCluster()
  } else {   
    outlist <- vector("list", nfolds)
    for (i in seq(nfolds)) {
      print(paste(i,"th fold",sep=""))
      which <- foldid == i
      x_sub <- x[!which, , drop = FALSE]
      y_sub <- y[!which]
      w_sub <- weights[!which]
      
      fit <- grplasso(x = x_sub, 
                      y = y_sub, index = index, 
                      weights = w_sub,
                      lambda = lambda, model = model,
                      standardize = standardize,
                      control = grpl.control(trace = 0L))
      pred <- predict(fit, x[which,,drop=FALSE])
      w_test <- weights[which]
      w_test <- w_test/sum(w_test)
      y_test <- y[which]
      outlist[[i]] <- sapply(1:length(lambda), function(j) 
        model@nloglik(y_test, pred[,j], w_test))
    }  
  }
  
  mse.mat <- do.call(rbind, outlist)
  mse <- data.frame(cvm = colMeans(mse.mat),
                    cvse = matrixStats::colSds(mse.mat) / sqrt(nfolds),
                    lambda = lambda)
  
  id<-which.min(mse$cvm)
  lambda.min <- mse$lambda[id]
  lambda.1se <- max(mse$lambda[mse$cvm <= mse$cvm[id] + mse$cvse[id]])
  
  test <- grplasso(x =x, y=y, index=index, 
                   weights = weights, standardize = standardize,
                   lambda=c(lambda.1se,lambda.min), model=model)
  list(lambda=lambda, cvm=mse$cvm, cvse=mse$cvse, grplasso.fit=test, lambda.min=lambda.min,lambda.1se=lambda.1se,foldid=foldid)  
}

# 5. Run CV for all lambdas


set.seed(103248108)
lasso.cv.tf <- glmnet::cv.glmnet(x = X[,-c(1)], y = Y_tf45, 
                                 weights = chis$fnwgt0/sum(chis$fnwgt0),
                                 family = "binomial",
                                 lambda.min.ratio = 0.001,
                                 intercept = TRUE,
                                 standardize = TRUE
)
print(lasso.cv.tf)
plot(lasso.cv.tf)

beta_lasso.tf <- glmnet::coef.glmnet(lasso.cv.tf, s = lasso.cv.tf$lambda.1se)
nonzero_beta.tf <- as.numeric(beta_lasso.tf) != 0
rownames(beta_lasso.tf)[nonzero_beta.tf]
beta_lasso.tf[nonzero_beta.tf,]

nms.tf <- rownames(beta_lasso.tf)[nonzero_beta.tf]
whichcols.tf <- attributes(X)$assign[sapply(nms.tf, function(n) grep(n, colnames(X), fixed = TRUE))]
sapply(attr(terms(Xmf), "term.labels")[whichcols.tf], function(i) attr(chis[[i]], "label"))


library(grplasso)
groups.grplasso <- as.integer(factor(groups))
groups.grplasso[1] <- NA
set.seed(123123)
grp_tf <- cv.grplasso(x = X, y = Y_tf45,
                      index = groups.grplasso, weights = w/sum(w),
                      model = LogReg(), standardize = FALSE,
                      nfolds = k, nlambda = 100, parallel = TRUE)

plot(x = grp_tf$lambda %>% log(), y = grp_tf$cvm, pch=19, ylab="Negative Log-Likelihood", xlab="log lambda", col = "red")
arrows(x0 = grp_tf$lambda %>% log(), y0 = grp_tf$cvm - 1* (grp_tf$cvse), 
       x1 = grp_tf$lambda %>% log(), y1 = grp_tf$cvm + 1* (grp_tf$cvse),
       angle = 90, code = 3, length = 0.05)
abline(v = grp_tf$lambda.min %>% log, lty = 3)
abline(v = grp_tf$lambda.1se %>% log, lty = 3)

gl_beta_1se <- grp_tf$grplasso.fit$coefficients[,1]
nonzero_gl_beta_1se <- as.numeric(gl_beta_1se) != 0
gl_beta_1se[nonzero_gl_beta_1se]

gl_beta_min <- grp_tf$grplasso.fit$coefficients[,2]
nonzero_gl_beta_min <- as.numeric(gl_beta_min) != 0
gl_beta_min[nonzero_gl_beta_min]

expand_idx <- mapply(function(w,i) {rep(i,w)}, w = ceiling(w), i = 1:nrow(X_s)) %>% unlist()

X_expand <- X[expand_idx,-1]
Y_tf45_expand <- Y_tf45[expand_idx]

grp_tf <- gglasso::cv.gglasso(x = X_expand, 
                              y = (2L * Y_tf45_expand - 1L),
                              group = groups.gglasso,
                              pred.loss = "loss", 
                              loss = "logit",
                              nlambda = 100, 
                              lambda.factor = 1e-8,
                              nfolds = 10,
                              intercept = TRUE)

plot(grp_tf)