```{r}

seeds_to_use <- c(33,34,35,36,37)
diagnostics <- c()
keep_going = TRUE


#Initial Value of MSE to beat
best_mse <- mean(best_fit$residuals^2)

while (keep_going == TRUE){
  #Extract Terms as it Will Change
  terms <- attr(best_fit$terms,"term.labels")
  mse_df <- data.frame(Terms=terms)
  for (s in seeds_to_use){
    mse_list <- c()
    set.seed(s)
    for (j in terms){
      #Version of the Model at Current Step
      bc_fit <- best_fit
      
      #Use a Random Sample of the Data
      bc_idx <- sample(nrow(bc_fit$model), nrow(bc_fit$model)*4/5)
      bc_train <- bc_fit$model[bc_idx,]
      bc_test <- bc_fit$model[bc_idx,]
      #Take off the Jth term
      eval(parse(text=paste("bc_fit <- update(bc_fit, . ~ . -",j,")",sep="")))
      #Fit on Train
      bc_fit <- lm(formula(bc_fit),data=bc_train)
      #Predict on Test
      bc_mse <- mean((predict(bc_fit,newdata = bc_test) - bc_test$Wst)^2)
      #Collect the AICs of Each Removed Regressor
      mse_list <- c(mse_list,bc_mse)
    }
    mse_df[[paste("seed",s,sep="")]] <- mse_list 
    
  } 
  
  mse_df$Number_Better <- ifelse(mse_df$seed33 < best_mse,1,0) +
    ifelse(mse_df$seed34 < best_mse,1,0) +
    ifelse(mse_df$seed35 < best_mse,1,0) +
    ifelse(mse_df$seed36 < best_mse,1,0) +
    ifelse(mse_df$seed37 < best_mse,1,0)
  
  mse_df$Mean_MSE <- (mse_df$seed33 + mse_df$seed34 + mse_df$seed35 + mse_df$seed36 + mse_df$seed37)/5
  lowest_mse <- min(mse_df$Mean_MSE)
  lowest_term <- mse_df$Terms[match(lowest_mse,mse_df$Mean_MSE)]
  mse_df$Lowest_MSE_Before <- best_mse
  
  #If There is an Imporvement by Removing a term.
  if (lowest_mse < best_mse){
    print(paste("Removing",lowest_term,"."))
    best_mse <- lowest_mse
    #Actually Modify the Best Fit Model
    eval(parse(text=paste("best_fit <- update(best_fit, . ~ . -",lowest_term,")",sep="")))
  } else {
    print("Done")
    keep_going = FALSE
    break
  }
  diagnostics <- c(diagnostics,list(mse_df))
}



```