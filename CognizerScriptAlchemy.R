install.packages("devtools")
install.packages("https://github.com/jeroenooms/curl/archive/master.tar.gz", repos = NULL)
devtools::install_github("ColumbusCollaboratory/cognizer")

install.packages(c("rmsfact", "testthat"))
library("cognizer", lib.loc="~/R/x86_64-redhat-linux-gnu-library/3.2")

#SERVICE_API_KEY = "61eebcea6c0dc3b63544a80d8416cf9e515460a7"

text <- c("Columbus, Ohio is Awesome!", "Looking forward to UseR2017 in Brussels!")
result <- text_sentiment(text, "61eebcea6c0dc3b63544a80d8416cf9e515460a7")
str(result)

