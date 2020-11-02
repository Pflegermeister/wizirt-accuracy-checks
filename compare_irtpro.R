library(wizirt)

data('responses')
data <- responses[,-1]
my_model <- wizirt(data = data, item_type = 'Rasch')
print(my_model, type = 'item') %>% print(n = 25)

my_model <- wizirt(data = data, item_type = '1PL')
print(my_model, type = 'item') %>% print(n = 25)

my_model <- wizirt(data = data, item_type = '2PL')
print(my_model, type = 'item') %>% print(n = 25)

my_model <- wizirt(data = data, item_type = '3PL')
print(my_model, type = 'item') %>% print(n = 25)
