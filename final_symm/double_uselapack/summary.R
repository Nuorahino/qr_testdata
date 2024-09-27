library(tidyverse)
library(tikzDevice)


full_path <- c("data/");


table <- NULL;

for (j in 1:length(full_path)) {
  files <- list.files(path = full_path[j]);
  for (i in 1:length(files)) {
    print(files[i])
    table <- rbind(table, read.csv(paste0(full_path[j], files[i]), sep = ","));
  }
}
table <- mutate(table, version = recode(table$version, 'basic' = "Eigen submatrix", 'eigen_elementwise' = "Eigen elementwise", 'nested' = "Nested Vectors", 'armadillo' = "Armadillo", 'blaze' = "Blaze", 'elementwise 1' = "One Vector", 'elementwise 2' = "Two Vectors", 'wrapped' = "Eigen Wrapped"));


res_table <- table %>% group_by(version, size, hermitian, complex, tol) %>%
  summarize(avg_runtime = mean(runtime.in.s)
, min_error = min(min_error), max_error = max(max_error), avg_error = mean(avg_error));

res_table <- mutate(res_table, bin = (version != "Eigen"));

tikz('../../../../thesis/tikz/compare_double_time.tex',width=7,height=4.5)
#ggp <-ggplot(filter(res_table, hermitian == 1, complex == 0, tol == 1e-16, size >= 900), aes(size, avg_runtime, colour = version)) + geom_smooth(aes(group=version)) + facet_wrap(~bin, scale='free_y', ncol = 1)
ggp <-ggplot(filter(res_table, hermitian == 1, complex == 0, size >= 900, version != "Eigen"), aes(size, avg_runtime, colour = version)) + geom_smooth(aes(group=version)) + facet_wrap(~tol)
ggp + labs(x =  "size", y = "average runtime in sec") + theme(strip.text.x = element_blank())
dev.off()

tikz('../../../../thesis/tikz/compare_double_runtime.tex',width=7,height=4.5)
#ggp <- ggplot(filter(res_table, hermitian == 1, complex == 0, tol == 1e-16, size >= 900), aes(size, avg_error, colour = version)) + geom_smooth(aes(group=version)) + facet_wrap(~tol, scale='free_y', ncol = 1)
ggp <- ggplot(filter(res_table, hermitian == 1, complex == 0, size >= 900, tol < 1e-9), aes(size, avg_error, colour = version)) + geom_smooth(aes(group=version)) + facet_wrap(~tol)
ggp + labs(x =  "size", y = "average error")
dev.off()
