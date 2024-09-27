library(tidyverse)
library(tikzDevice)


full_path <- c("data/", "../compile/O3_SIMD/");


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

tikz('../../../thesis/tikz/compare_symm.tex',width=7,height=4.5)
ggp <-ggplot(filter(res_table, hermitian == 1, complex == 0, tol == 1e-12, size >= 900), aes(size, avg_runtime, colour = version)) + geom_smooth(aes(group=version)) #+ geom_point()
ggp + labs(x =  "size", y = "average runtime in sec")
dev.off()
