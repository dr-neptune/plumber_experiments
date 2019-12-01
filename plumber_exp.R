library(plumber)
source("tg_model.R")

#* @apiTitle teen gambling model API
#* @apiDescription Endpoints for working with the teen gambling dataset

#* Log some information about the incoming request
#* @filter logger
function(req){
  cat(as.character(Sys.time()), "-", 
      req$REQUEST_METHOD, req$PATH_INFO, "-", 
      req$HTTP_USER_AGENT, "@", req$REMOTE_ADDR, "\n")
  
  # Forward the request
  forward()
}

#* Predict gamb values
#* @param sex Sex of person {0, 1}
#* @param status Status of person [0, 100]
#* @param income Income of person [0, 15]
#* @param verbal ??? of person [0, 10]
#* @get /predict
function(sex, status, income, verbal) {
    predict(lmod, newdata = data.frame("sex" = as.numeric(sex),
                                       "status" = as.numeric(status),
                                       "income" = as.numeric(income),
                                       "verbal" = as.numeric(verbal)))
}

#* Get Fitted vs Residual Diagnostic Plot
#* @get /fitted_residual_plot
#* @png
function() {
    lm_aug %>%
        diag_plot(x = .fitted, y = .resid) -> p1
    
    print(p1)
}

#* Get Fitted vs Sqrt Abs Residual Diagnostic Plot
#* @get /fitted_sqrt_abs_resid_plot
#* @png
function() {
    lm_aug %>%
        diag_plot(x = .fitted, y = sqrt(abs(.resid))) -> p1
    
    print(p1)
}

#* Get Normality Plot
#* @get /normality
#* @png
function() {
    lm_aug %>%
        qq_plot(choice = .resid,
                title = "QQ Plot | Residuals") -> p1

    lm_aug %>%
        ggplot(aes(x = .resid)) +
        geom_histogram(bins = 20, fill = "skyblue", color = "black") -> p2

    cowplot::plot_grid(p1, p2, ncol = 1) -> p3

    print(p3)
}

#* Get Shapiro-Wilk Test Diagnostic
#* @json
#* @get /shapiro
function() {
    shapiro.test(residuals(lmod)) %>% tidy()
}

#* Get large leverage points
#* @json
#* @get /leverage
function() {
    lmod %>%
        get_large_lev_points()
}

#* Generate large leverage point plots
#* @png
#* @get /leverage_plots
function() {
    lmod %>%
        gen_leverage_plots() -> p1

    print(p1)
}

#* Get outlier values
#* @json
#* @get /outliers
function() {
    lmod %>%
        get_adj_outliers()
}

#* Get influential points
#* @png
#* @get /influential_plot
function() {
    lm_aug %>% 
    .$.cooksd %>%
    gghalfnorm::gghalfnorm() -> p1

    print(p1)
}

#* Get partial regression plots
#* @param response_var The response variable
#* @png
#* @get /partial_regression_plots
function(response_var) {
    tg %>%
        gen_all_partial(response = as.character(response_var)) -> p1

    print(p1)
}

#* Get partial residual plots
#* @png
#* @get /partial_residual_plots
function() {
    lmod %>%
        gen_all_partial_resids() -> p1

    print(p1)
}
