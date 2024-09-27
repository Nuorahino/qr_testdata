library(tidyverse)
library(tikzDevice)
library(cowplot)


full_path <- c("data/", "no_compute/", "lapack/");
name <- c("Reorder Givens", "Use Givens parameter as entry", "Lapack Givens Parameter");

table <- NULL;
for(j in 1:3) {
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
#tikz('../../../thesis/tikz/reordered_givens_time.tex',width=6.9,height=4.8)
#ggp <-ggplot(filter(res_table, hermitian == 1, complex == 0, tol == 1e-12, size >= 900), aes(size, avg_runtime, colour = variant)) + geom_smooth(aes(group=variant)) + facet_wrap(~version, ncol=3)
#ggp + labs(x =  "size", y = "average runtime in sec")
#dev.off()
#
#
#tikz('../../../thesis/tikz/reordered_givens_error.tex',width=6.9,height=4.8)
#ggp <-ggplot(filter(res_table, hermitian == 1, complex == 0, tol == 1e-12), aes(size, avg_error, colour = variant)) + geom_smooth(aes(group=variant)) + facet_wrap(~version, ncol=3)
#ggp + labs(x =  "size", y = "average error")
#dev.off()

ggp <-ggplot(filter(res_table, hermitian == 1, complex == 0, tol == 1e-12, size >= 900, version=="nested"), aes(size, avg_runtime, colour = variant)) + geom_smooth(aes(group=variant)) + labs(x =  "size", y = "average runtime in sec");
ggp2 <-ggplot(filter(res_table, hermitian == 1, complex == 0, tol == 1e-12, version=="nested"), aes(size, avg_error, colour = variant)) + geom_smooth(aes(group=variant)) + labs(x =  "size", y = "average error");
pg <- plot_grid(ggp + theme(legend.position="none"), ggp2 + theme(legend.position="none"));
grobs <- ggplotGrob(ggp + theme(legend.position="bottom"))$grobs;
legend <- grobs[[which(sapply(grobs, function(x) x$name) == "guide-box")]];
p <- plot_grid(pg, legend, nrow = 2, rel_heights = c(1, .09));

tikz('../../../thesis/tikz/reordered_givens.tex',width=7,height=3.5)
p
dev.off()
