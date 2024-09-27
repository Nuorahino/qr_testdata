library(tidyverse)
library(tikzDevice)


full_path <- c("O3/", "O3_vec/", "O3_SIMD/", "O3_vec_SIMD/");
name <- c("O3", "O3 + better vectorization", "O3 + SIMD", "O3 + better vectorization + SIMD");

table <- NULL;
for(j in 1:4) {
  files <- list.files(path = full_path[j]);

  for (i in 1:length(files)) {
    print(files[i])
    table <- rbind(table, cbind(read.csv(paste0(full_path[j], files[i]), sep = ","), variant = name[j]));
  }
}

table <- mutate(table, version = recode(table$version, 'nested' = "Nested Vectors", 'armadillo' = "Armadillo", 'blaze' = "Blaze", 'elementwise 1' = "One Vector", 'elementwise 2' = "Two Vectors", 'wrapped' = "Eigen Wrapped"));

res_table <- table %>% group_by(version, variant, size, hermitian, complex, tol) %>%
  summarize(avg_runtime = mean(runtime.in.s)
, min_error = min(min_error), max_error = max(max_error), avg_error = mean(avg_error));




# Create unsymmetric matrix graph
tikz('../../../thesis/tikz/compile_options.tex',width=6.9,height=4.8)
ggp <-ggplot(filter(res_table, hermitian == 1, complex == 0, tol == 1e-12, size >= 900), aes(size, avg_runtime, colour = variant)) + geom_smooth(aes(group=variant)) + facet_wrap(~version, ncol=3)
ggp + labs(x =  "size", y = "average runtime in sec")
dev.off()
