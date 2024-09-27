#library(tidyverse)
library(ggplot2)
library(dplyr)
library(cowplot)
#library(tikzDevice)


full_path <- c("col_new/", "col_old/", "row/", "symm_v1/", "symm_v2/", "symm_v3/", "eigen_nonsymm/", "eigen_symm/", "lapack_nonsymm/", "lapack_symm/");
name <- c("colwise using new parameter", "colwise using old parameter", "rowwise operations", "first symmetric version", "second symmetric version", "third symmetric version", "eigen non symmertric", "eigen symmertric", "lapack non symmertric", "lapack symmertric");

table <- NULL;
for(j in 1:length(full_path)) {
  #if (j != 3 && j != 9) {
  files <- list.files(path = full_path[j]);

  for (i in 1:length(files)) {
    print(files[i])
    table <- rbind(table, cbind(read.csv(paste0(full_path[j], files[i]), sep = ","), variant = name[j]));
  }
 # }
}

#table <- mutate(table, version = recode(table$version, 'nested' = "Nested Vectors", 'armadillo' = "Armadillo", 'blaze' = "Blaze", 'elementwise 1' = "One Vector", 'elementwise 2' = "Two Vectors", 'wrapped' = "Eigen Wrapped"));

res_table <- table %>% group_by(version, variant, size, hermitian, complex, tol) %>%
  summarize(avg_runtime = mean(runtime.in.s)
, min_error = min(min_error), max_error = max(max_error), avg_error = mean(avg_error));




# Create unsymmetric matrix graph
#tikz('../../../thesis/tikz/hessenberg.tex',width=6.9,height=4.8)
#ggp <-ggplot(filter(res_table, hermitian == 0, complex == 1, tol == 1e-12, size >= 900), aes(size, avg_runtime, colour = variant)) + geom_smooth(aes(group=variant)) + geom_point() + facet_wrap(~version, ncol=3)

# runtime plots
ggp_h1c0 <-ggplot(filter(res_table, hermitian == 1, complex ==0, tol == 1e-12, size >= 900), aes(size, avg_runtime, colour = variant)) + geom_smooth(aes(group=variant)) #+ facet_wrap(~complex, ncol=3)
ggp_h1c0 + labs(x =  "size", y = "average runtime in sec")
png("testplot_h1c0.png")
print(ggp_h1c0)
dev.off()
ggp_h1c1 <-ggplot(filter(res_table, hermitian == 1, complex ==1, tol == 1e-12, size >= 900), aes(size, avg_runtime, colour = variant)) + geom_smooth(aes(group=variant)) #+ facet_wrap(~complex, ncol=3)
ggp_h1c1 + labs(x =  "size", y = "average runtime in sec")
png("testplot_h1c1.png")
print(ggp_h1c1)
dev.off()


ggp_h0c0 <-ggplot(filter(res_table, hermitian == 0, complex ==0, tol == 1e-12, size >= 900), aes(size, avg_runtime, colour = variant)) + geom_smooth(aes(group=variant)) #+ facet_wrap(~complex, ncol=3)
ggp_h0c0 + labs(x =  "size", y = "average runtime in sec")
png("testplot_h0c0.png")
print(ggp_h0c0)
dev.off()
ggp_h0c1 <-ggplot(filter(res_table, hermitian == 0, complex ==1, tol == 1e-12, size >= 900), aes(size, avg_runtime, colour = variant)) + geom_smooth(aes(group=variant)) #+ facet_wrap(~complex, ncol=3)
ggp_h0c1 + labs(x =  "size", y = "average runtime in sec")
png("testplot_h0c1.png")
print(ggp_h0c1)
dev.off()

#ggp <-ggplot(filter(res_table, complex == 0, tol == 1e-12, size >= 900), aes(size, avg_runtime, colour = variant)) + geom_smooth(aes(group=variant)) + geom_point() + facet_wrap(~hermitian, ncol=3)
#ggp + labs(x =  "size", y = "average runtime in sec")
ggp_error_h1c0 <-ggplot(filter(res_table, hermitian == 1, complex ==0, tol == 1e-12, size >= 900), aes(size, avg_error, colour = variant)) + geom_smooth(aes(group=variant)) #+ facet_wrap(~complex, ncol=3)
ggp_error_h1c0 + labs(x =  "size", y = "average error")
png("testplot_error_h1c0.png")
print(ggp_error_h1c0)
dev.off()

#error plots
ggp_error_h1c1 <-ggplot(filter(res_table, hermitian == 1, complex ==1, tol == 1e-12, size >= 900), aes(size, avg_error, colour = variant)) + geom_smooth(aes(group=variant)) #+ facet_wrap(~complex, ncol=3)
ggp_error_h1c1 + labs(x =  "size", y = "average error")
png("testplot_error_h1c1.png")
print(ggp_error_h1c1)
dev.off()

ggp_error_h0c1 <-ggplot(filter(res_table, hermitian == 0, complex ==1, tol == 1e-12, size >= 900), aes(size, avg_error, colour = variant)) + geom_smooth(aes(group=variant)) #+ facet_wrap(~complex, ncol=3)
ggp_error_h0c1 + labs(x =  "size", y = "average error")
png("testplot_error_h0c1.png")
print(ggp_error_h0c1)
dev.off()

ggp_error_h0c0 <-ggplot(filter(res_table, hermitian == 0, complex ==0, tol == 1e-12, size >= 900), aes(size, avg_error, colour = variant)) + geom_smooth(aes(group=variant)) #+ facet_wrap(~complex, ncol=3)
ggp_error_h0c0 + labs(x =  "size", y = "average error")
png("testplot_error_h0c0.png")
print(ggp_error_h0c0)
dev.off()

ggp_error_h1c0 <-ggplot(filter(res_table, hermitian == 1, complex ==0, tol == 1e-12, size >= 900), aes(size, avg_error, colour = variant)) + geom_smooth(aes(group=variant)) #+ facet_wrap(~complex, ncol=3)
ggp_error_h1c0 + labs(x =  "size", y = "average error")
png("testplot_error_h1c0.png")
print(ggp_error_h1c0)
dev.off()

# plots side by side
pg <- plot_grid(ggp_h1c1 + theme(legend.position="none"), ggp_error_h1c1 + theme(legend.position="none"));
grobs <- ggplotGrob(ggp_h1c1 + theme(legend.position="bottom"))$grobs;
legend <- grobs[[which(sapply(grobs, function(x) x$name) == "guide-box")]];
p <- plot_grid(pg, legend, nrow = 2, rel_heights = c(1, .09));
png("testplot_h1c1_withError.png",width=700,height=350.0)
print(p)
dev.off()

pg <- plot_grid(ggp_h0c1 + theme(legend.position="none"), ggp_error_h0c1 + theme(legend.position="none"));
grobs <- ggplotGrob(ggp_h0c1 + theme(legend.position="bottom"))$grobs;
legend <- grobs[[which(sapply(grobs, function(x) x$name) == "guide-box")]];
p <- plot_grid(pg, legend, nrow = 2, rel_heights = c(1, .09));
png("testplot_h0c1_withError.png",width=700,height=350.0)
print(p)
dev.off()

pg <- plot_grid(ggp_h1c0 + theme(legend.position="none"), ggp_error_h1c0 + theme(legend.position="none"));
grobs <- ggplotGrob(ggp_h1c0 + theme(legend.position="bottom"))$grobs;
legend <- grobs[[which(sapply(grobs, function(x) x$name) == "guide-box")]];
p <- plot_grid(pg, legend, nrow = 2, rel_heights = c(1, .09));
png("testplot_h1c0_withError.png",width=700,height=350.0)
print(p)
dev.off()

pg <- plot_grid(ggp_h0c0 + theme(legend.position="none"), ggp_error_h0c0 + theme(legend.position="none"));
grobs <- ggplotGrob(ggp_h0c0 + theme(legend.position="bottom"))$grobs;
legend <- grobs[[which(sapply(grobs, function(x) x$name) == "guide-box")]];
p <- plot_grid(pg, legend, nrow = 2, rel_heights = c(1, .09));
png("testplot_h0c0_withError.png",width=700,height=350.0)
print(p)
dev.off()

#ggp <-ggplot(filter(res_table, hermitian == 0, tol == 1e-12, size >= 900), aes(size, avg_error, colour = variant)) + geom_smooth(aes(group=variant)) + geom_point() + facet_wrap(~complex, ncol=3)
#ggp + labs(x =  "size", y = "average error")
#dev.off()
