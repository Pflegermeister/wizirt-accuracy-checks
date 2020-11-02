library(wizirt)

data('responses')
data <- responses[,-1]
my_model <- wizirt(data = data, item_type = 'Rasch')


winpfa <- readr::read_csv("~/Dissertation/compare software/winsteps/winsteps_persons.csv")
winifa <- readr::read_csv("~/Dissertation/compare software/winsteps/winsteps_items.csv")


cor(scale(winifa$measure), scale(my_model$fit$parameters$coefficients$difficulty))


ggplot2::qplot(scale(winifa$measure),  scale(my_model$fit$parameters$coefficients$difficulty)) + ggplot2::geom_abline()

ggplot2::qplot(scale(winpfa$measure), scale(my_model$fit$parameters$persons$ability)) + ggplot2::geom_abline()
