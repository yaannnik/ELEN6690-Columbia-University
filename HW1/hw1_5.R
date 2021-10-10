auto = read.csv("auto-mpg.csv", header = T, na.strings = "?", 
                stringsAsFactors = T)
auto = na.omit(auto)
names(auto)
pairs(auto[1:9])

Cor = cor(auto[1:8])

lm_1 = lm(auto$mpg ~ auto$cylinders
          +auto$displacement
          +auto$horsepower
          +auto$weight
          +auto$acceleration
          +auto$model.year
          +auto$origin)
summary(lm_1)

lm_2 = lm(log10(auto$mpg) ~ log10(auto$cylinders)
          +log10(auto$displacement)
          +log10(auto$horsepower)
          +log10(auto$weight)
          +log10(auto$acceleration)
          +log10(auto$model.year)
          +log10(auto$origin))
summary(lm_2)

lm_3 = lm(sqrt(auto$mpg) ~ sqrt(auto$cylinders)
          +sqrt(auto$displacement)
          +sqrt(auto$horsepower)
          +sqrt(auto$weight)
          +sqrt(auto$acceleration)
          +sqrt(auto$model.year)
          +sqrt(auto$origin))
summary(lm_3)

lm_4 = lm(auto$mpg^2 ~ auto$cylinders^2
          +auto$displacement^2
          +auto$horsepower^2
          +auto$weight^2
          +auto$acceleration^2
          +auto$model.year^2
          +auto$origin^2)
summary(lm_4)
