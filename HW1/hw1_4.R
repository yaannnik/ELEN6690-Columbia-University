adv = read.csv("advertising.csv", header = T, na.strings = '?')
adv = na.omit(adv)

Newspaper_fit= lm(adv$Sales ~ adv$Newspaper)
Newspaper_confint = confint(Newspaper_fit, level = 0.92)
plot(adv$Newspaper, adv$Sales, xlab = "Newspaper Advertising", ylab = "Sales", main="Sales-Newspaper Advertising")

abline(Newspaper_fit, col = 'green', lty = 1)
abline(a = Newspaper_confint[1, 1], Newspaper_confint[2, 1], col = 'red', lty = 3)
abline(a = Newspaper_confint[1, 2], Newspaper_confint[2, 2], col = 'red', lty = 3)

legend('topleft', inset = 0.05, c('least-squares', '0.92 condifence interval'), lty = c(1, 3), col = c('green', 'red'), bty = 'o')

Newspaper_confint

