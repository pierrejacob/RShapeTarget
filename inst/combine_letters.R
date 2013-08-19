rm(list = ls())

word <- create_target_from_word(word="bit", lambda=0.1)
names(word)
x <- csr(word$bounding_box, 2500)
logpdf <- word$f(x, word$algo_parameters)

df <- data.frame(cbind(x, logpdf))
names(df) <- c("x", "y", "logd")
ggplot(df, aes(x = x, y = y, colour = logd, fill = logd, weight = logd)) + geom_point()
