

#### Variable Selection SMI ####
novary_cols <- NULL
missing_cols <- sapply(chis, function(x) x %>% is.na %>% any)
cols <- !(missing_cols |grepl("raked|fnwgt|region|p_tf|orthog_tf", colnames(chis)) )

xform <- as.formula(~. -tf45 - puf1y_id - year
                    + I(as.factor(year)) 
                    + I(as.numeric(as.character(dstrstn_p1))) - dstrstn_p1
                    - srage_p
                    - dstrs12
                    - dstrs30
                    - raceh2_s
                    # - povll2_p1 -   povll2_p1v2 - bmi_p - intv_mode2 - wghtk_p - wghtp_p - ta1yr - survey_dates 
                    # - tf45
                    # - tadate_mm # date and year of survey
                    # # - ccpreg19 #covered cal price regions
                    # + I(as.factor(survey_months)) - survey_years - survey_months
                    # + I(as.numeric(as.character(povll2_p)))
                    # + I(as.numeric(as.character(povll2_p1v2)))
                    # # + I(as.numeric(tc38=='Yes')* te81) - tc38 - te81
                    # # + I(as.numeric(tc38=='Yes')):te19 - te19 
                    # # + te19
                    # # tc38 = EVER SMOKED CIGARETTES
                    # # te19 = # OF DAYS SMOKED CIGARETTES IN PAST 30 DAYS
                    # + I(as.numeric(te79=='Yes')* te82) - te79 - te82 - te82_p1
                    # # + I(as.numeric(te79=='Yes')* te80) - te80
                    # + I(as.numeric(tf28=='Yes')* tf29v2) - tf29v2 - tf28
                    # # + I(as.numeric(te19 != 'Inapplicable' & te19 != 'None') * te20) - te20 - te19 
                    # # te19 = # OF DAYS SMOKED CIGARETTES IN PAST 30 DAYS
                    # # te20 = # OF CIGARETTES SMOKED PER DAY IN PAST 30 DAYS
                    # # + I(as.numeric(ti6 > -1) * ti6) - ti6
                    # + as.factor(year) - year
)
Xmf <- model.frame(xform, chis[,cols] )
X <- model.matrix(terms(Xmf), Xmf)
all_vars <- colnames(X)
i_vars <- grepl("I(", (all_vars), fixed = TRUE)
all_vars <- all_vars[!i_vars][-1]

std_cont <- sapply(all_vars, function(i)
  scale(X[,i], center = weighted.mean(X[,i],w),
        scale = matrixStats::weightedSd(X[,i],w))
)
X[,all_vars] <- std_cont


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
names(gl_beta_1se)[nonzero_gl_beta_1se]

nms.tf <- names(gl_beta_1se)[nonzero_gl_beta_1se]
whichcols.tf <- attributes(X)$assign[sapply(nms.tf, function(n) grep(n, colnames(X), fixed = TRUE)) %>% unlist()]
var.names.tf <- sapply(attr(terms(Xmf), "term.labels")[whichcols.tf] %>% unique(), function(i) attr(chis[[i]], "label"))
if(!is.null(var.names.tf$age_groups)) {
  var.names.tf$age_group <- "Age group split 12-14 and 15-17"
}
if(!is.null(var.names.tf$`I(as.numeric(as.character(dstrstn_p1)))`)) {
  var.names.tf$`I(as.numeric(as.character(dstrstn_p1)))` <- attr(chis$dstrstn_p1,"label")
}
print(var.names.tf)
writeLines(var.names.tf, "Outputs/lasso_seletion.txt")


gl_beta_min <- grp_tf$grplasso.fit$coefficients[,2]
nonzero_gl_beta_min <- as.numeric(gl_beta_min) != 0
gl_beta_min[nonzero_gl_beta_min]

