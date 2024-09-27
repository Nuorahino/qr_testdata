library(tidyverse)
library(tikzDevice)


full_path <- c("col_new/", "col_old/", "row/", "symm_v1/", "symm_v2/");
name <- c("colwise using new parameter", "colwise using old parameter", "rowwise operations", "first symmetric version", "second symmetric version");

table <- NULL;
for(j in 1:5) {
  files <- list.files(path = full_path[j]);

  for (i in 1:length(files)) {
    print(files[i])
    table <- rbind(table, cbind(read.csv(paste0(full_path[j], files[i]), sep = ","), variant = name[j]));
  }
}

#table <- mutate(table, version = recode(table$version, 'nested' = "Nested Vectors", 'armadillo' = "Armadillo", 'blaze' = "Blaze", 'elementwise 1' = "One Vector", 'elementwise 2' = "Two Vectors", 'wrapped' = "Eigen Wrapped"));

res_table <- table %>% group_by(version, variant, size, hermitian, complex, tol) %>%
  summarize(avg_runtime = mean(runtime.in.s)
, min_error = min(min_error), max_error = max(max_error), avg_error = mean(avg_error));




# Create unsymmetric matrix graph
tikz('../../../thesis/tikz/hessenberg.tex',width=6.9,height=4.8)
#ggp <-ggplot(filter(res_table, hermitian == 0, complex == 1, tol == 1e-12, size >= 900), aes(size, avg_runtime, colour = variant)) + geom_smooth(aes(group=variant)) + geom_point() + facet_wrap(~version, ncol=3)
ggp <-ggplot(filter(res_table, complex == 1, tol == 1e-12, size >= 900), aes(size, avg_runtime, colour = variant)) + geom_smooth(aes(group=variant)) + geom_point() + facet_wrap(~hermitian, ncol=3)
ggp + labs(x =  "size", y = "average runtime in sec")
ggp <-ggplot(filter(res_table, complex == 0, tol == 1e-12, size >= 900), aes(size, avg_runtime, colour = variant)) + geom_smooth(aes(group=variant)) + geom_point() + facet_wrap(~hermitian, ncol=3)
ggp + labs(x =  "size", y = "average runtime in sec")
ggp <-ggplot(filter(res_table, hermitian == 1, tol == 1e-12, size >= 900), aes(size, avg_error, colour = variant)) + geom_smooth(aes(group=variant)) + geom_point() + facet_wrap(~complex, ncol=3)
ggp + labs(x =  "size", y = "average error")
ggp <-ggplot(filter(res_table, hermitian == 0, tol == 1e-12, size >= 900), aes(size, avg_error, colour = variant)) + geom_smooth(aes(group=variant)) + geom_point() + facet_wrap(~complex, ncol=3)
ggp + labs(x =  "size", y = "average error")
dev.off()
