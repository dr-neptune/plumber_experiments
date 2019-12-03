library(plumber)
source("tg_model.R")

#* @apiTitle teen gambling model API
#* @apiDescription Endpoints for working with the teen gambling dataset

#* Log some information about the incoming request
#* @filter logger
function(req){
    sink("log_file.txt")
    cat(as.character(Sys.time()), "-", 
        req$REQUEST_METHOD, req$PATH_INFO, "-", 
        req$HTTP_USER_AGENT, "@", req$REMOTE_ADDR, "\n")
    sink()
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
    sex <- as.numeric(sex)
    status <- as.numeric(status)
    income <- as.numeric(income)
    verbal <- as.numeric(verbal)
    
    # catch out of range errors, return midpoint
    if (!(between(sex, 0, 1))) {
        cat("Warning : sex out of range. returning sex = 0\n")
        sex <- 0
    }

    if (!(between(status, 0, 100))) {
        cat("Warning : status out of range. Returning status = 50\n")
        status <- 50
    }

    if (!(between(income, 0, 10))) {
        cat("Warning : income out of range. Returning income = 5\n")
        income <- 5
    }

    if (!(between(verbal, 0, 10))) {
        cat("Warning : verbal out of range. Returning verbal = 5\n")
        verbal <- 5
    }
    
    pred <- predict(lmod, newdata = data.frame("sex" = sex,
                                               "status" = status,
                                               "income" = income,
                                               "verbal" = verbal))

    return(pred)
}

#* Get Fitted vs Residual Diagnostic Plot
#* @get /fitted_residual_plot
#* @png (width = 800, height = 800) 
function() {
    lm_aug %>%
        diag_plot(x = .fitted, y = .resid) -> p1
    
    print(p1)
}

#* Get Fitted vs Sqrt Abs Residual Diagnostic Plot
#* @get /fitted_sqrt_abs_resid_plot
#* @png (width = 800, height = 800) 
function() {
    lm_aug %>%
        diag_plot(x = .fitted, y = sqrt(abs(.resid))) -> p1
    
    print(p1)
}

#* Get Normality Plot
#* @get /normality
#* @png (width = 800, height = 800) 
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
#* @png (width = 800, height = 800) 
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
#* @png (width = 800, height = 800) 
#* @get /influential_plot
function() {
    lm_aug %>% 
    .$.cooksd %>%
    gghalfnorm::gghalfnorm() -> p1

    print(p1)
}

#* Get partial regression plots
#* @param response_var The response variable
#* @png (width = 800, height = 800) 
#* @get /partial_regression_plots
function(response_var) {
    tg %>%
        gen_all_partial(response = as.character(response_var)) -> p1

    print(p1)
}

#* Get partial residual plots
#* @png (width = 800, height = 800) 
#* @get /partial_residual_plots
function() {
    lmod %>%
        gen_all_partial_resids() -> p1

    print(p1)
}
