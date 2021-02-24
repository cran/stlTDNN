\name{STLTDNN}
\alias{STLTDNN}
\title{
STL Based TDNN Hybrid Forecast
}
\description{
The STLTDNN function forecasts univariate time series using a hybrid model made of a decomposition technique called seasonal trend decomposition based on loess (STL) and a neural network based forecasting technique called time delay neural network (TDNN). The function further computes the values of different forecasting evaluation criteria.
}
\usage{
STLTDNN(data, stepahead=12)
}

\arguments{
  \item{data}{
Input univariate time series (ts) data.
}
  \item{stepahead}{
The forecast horizon.
}
}
\details{
This function decomposes a nonlinear, nonstationary and seasonal time series into trend-cycle, seasonal and remainder component using STL (Cleveland et al., 1990). Time delay neural network is used to forecast these components individually (Jha and Sinha, 2014). Finally, the prediction results of all the three components are aggregated to formulate an ensemble output for the input time series.
}
\value{
  \item{data_test }{Testing set used to measure the out of sample performance.}
  \item{STLcomp_forecast }{Forecasted value of all individual components.}
  \item{FinalstlTDNN_forecast }{Final forecasted value of the stlTDNN model. It is obtained by combining the forecasted value of all individual components.}
  \item{MAE_stlTDNN }{Mean Absolute Error (MAE) for stlTDNN model.}
  \item{SMAPE_stlTDNN }{Mean Absolute Percentage Error (MAPE) for stlTDNN model.}
  \item{RMSE_stlTDNN }{Root Mean Square Error (RMSE) for stlTDNN model.}
}
\references{
Cleveland, R.B., Cleveland, W.S., McRae, J.E., Terpenning, I. (1990). STL: A seasonal-trend decomposition procedure based on loess, Journal of Official Statistics, 6, 3--73.


Jha, G.K., Sinha, K. (2014). Time-delay neural networks for time series prediction: An application to the monthly wholesale price of oilseeds in India. Neural Computing and Application, 24, 563--571
}

\examples{
\donttest{
data("Data_potato")
STLTDNN(Data_potato)
}
}
\keyword{ stl }
