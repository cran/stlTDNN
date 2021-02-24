#' @importFrom forecast mstl
#' @importFrom nnfor mlp
#' @importFrom utils head tail
#' @importFrom graphics plot
#' @importFrom stats as.ts ts
#' @export
#'
STLTDNN <- function(data, stepahead=12){
  STLcomp <- mstl(data)
  STLcomp_plots<-plot(STLcomp)
  data_trn <- ts(head(data, round(length(data) - stepahead)))
  data_test <- ts(tail(data, stepahead))
  STLcomp_trn <- STLcomp[-c(((length(data)-stepahead)+1):length(data)),]
  Fcast_STLcomp <- NULL
  for (STLcomp in 2:ncol(STLcomp_trn)) {
    Indcomp <- NULL
    Indcomp <- STLcomp_trn[ ,STLcomp]
    stlTDNNFit <- nnfor::mlp(as.ts(Indcomp))
    stlTDNN_fcast=forecast::forecast(stlTDNNFit, h=stepahead)
    stlTDNN_fcast_Mean=stlTDNN_fcast$mean
    Fcast_STLcomp <- cbind(Fcast_STLcomp, as.matrix(stlTDNN_fcast_Mean))
  }
  FinalstlTDNN_fcast <- ts(rowSums(Fcast_STLcomp, na.rm = T))
  MAE_stlTDNN=mean(abs(data_test - FinalstlTDNN_fcast))
  MAPE_stlTDNN=mean(abs(data_test - FinalstlTDNN_fcast)/data_test)
  rmse_stlTDNN=sqrt(mean((data_test - FinalstlTDNN_fcast)^2))
  return(list(data_test=data_test, STLcomp_forecast=Fcast_STLcomp,
              FinalSTLcomp_forecast=FinalstlTDNN_fcast, MAE_stlTDNN=MAE_stlTDNN,
              MAPE_stlTDNN=MAPE_stlTDNN, rmse_stlTDNN=rmse_stlTDNN,
              STLcomp_plots=STLcomp_plots))
}
