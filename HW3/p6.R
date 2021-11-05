samples = c(0.1, 0.15, 0.2, 0.2, 0.55, 0.6, 0.6, 0.65, 0.7, 0.75)

red = 0
green = 0

for(i in 1:length(samples)) {
  if(samples[i] >= 0.5) {
    red = red + 1
  }
  else {
    green = green + 1
  }
}

mean(samples) >= 0.5

