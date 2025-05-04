library(doconv)
library(locatexec)
library(here)

my.docx <- here::here("results", "GFS-S1 Online Supplement Part 1_Self-rated physical health.docx")
my.pdf <- here::here("results", "GFS-S1 Online Supplement Part 1_Self-rated physical health.pdf")


docx2pdf(input = my.docx, output = my.pdf)



