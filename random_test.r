library(ggplot2)

# Create data frame with x values from -10 to 10
df <- data.frame(x = seq(0, 20, by = 0.1))
b = 5
# Add column with f(x) = 5 + exp(x)/(1+exp(x))
df$y <- 5/(1 + exp(-b*(df$x - 10)))

# Create plot
ggplot(df, aes(x = x, y = y)) +
  geom_line() +
  labs(title = "f(x) = 5 + exp(x)/(1+exp(x))",
       x = "x",
       y = "f(x)") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20)) +
  ylim(0, 7)
