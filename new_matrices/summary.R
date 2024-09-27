library(tidyverse)
library(tikzDevice)

full_path <- rep_len("", 8);
full_path[1] <- "compile1/"
full_path[2] <- "compile2/"
full_path[3] <- "compile3/"
full_path[4] <- "compile4/"
full_path[5] <- "compile11/"
full_path[6] <- "compile12/"
full_path[7] <- "compile13/"
full_path[8] <- "compile14/"
full_path[9] <- "compile15/"

files <- NULL;
table <- NULL;
#res_table <- rep_len(tibble(), 4);

for (j in 1:9) {
  files <- list.files(path = full_path[j]);
  for (i in 1:length(files)) {
    print(files)
    table <- rbind(table, cbind(read.csv(paste0(full_path[j], files[i]), sep = ","), variant = j));
  }
}
full_path = "lapack1/";
files <- list.files(path = full_path);
for (i in 1:length(files)) {
  print(files)
  table <- rbind(table, cbind(filter(read.csv(paste0(full_path, files[i]), sep = ","), version == "elementwise 2") , variant = "dlartg"));
}
#full_path = "lapack2/";
full_path = "lapack3/"
files <- list.files(path = full_path);
for (i in 1:length(files)) {
  print(files)
  table <- rbind(table, cbind(filter(read.csv(paste0(full_path, files[i]), sep = ","), version == "elementwise 2"), variant = "dlartg with ref"));
}
table <- filter(table, tol < 1e-10)
full_path = "only_lapack/";
files <- list.files(path = full_path);
for (i in 1:length(files)) {
  print(files)
  table <- rbind(table, cbind(read.csv(paste0(full_path, files[i]), sep = ","), variant = "lapack"));
}

res_table <- table %>% group_by(version, size, hermitian, complex, tol, variant) %>%
  summarize(avg_runtime = mean(runtime.in.s)
, min_error = min(min_error), max_error = max(max_error), avg_error = mean(avg_error), variant = variant);

#table <- mutate(table, version = recode(table$version, '101' = "Implicit Shift", '201' = "Double Shift with 1ev", '301' = "Double Shift", '991' = "Eigen"));
#table <- filter(table, version != "original", version != "eigen elementwise", version != "eigen transformation")

#full_path <- "new/data/";
#files <- list.files(path = full_path);
#for (i in 1:length(files)) {
#  print(files[i])
#  table <- rbind(table, read.csv(paste0(full_path, files[i]), sep = ","));
#}


# Create unsymmetric matrix graph
#tikz('../../../../acmart/fig/unsymmetricGraph.tex',width=7,height=3.5)
tikz('test.tex',width=7,height=3.5)
#ggp <-ggplot(filter(res_table, hermitian == 1, complex == 0, tol == 1e-12, size >= 900), aes(size, avg_runtime, colour = version)) + facet_wrap(~variant, ncol=4) + geom_smooth(aes(group=version)) #+ geom_point()
#ggp <-ggplot(filter(res_table, hermitian == 1, complex == 0, tol == 1e-12, size >= 900), aes(size, avg_runtime, colour = variant)) + geom_smooth(aes(group=variant)) #+ geom_point()
ggp <-ggplot(filter(res_table, hermitian == 1, complex == 0, size >= 900), aes(size, avg_runtime, colour = variant)) + geom_smooth(aes(group=variant)) #+ geom_point()
ggp <-ggplot(filter(res_table, hermitian == 1, complex == 0, size >= 900), aes(size, avg_error, colour = variant)) + geom_smooth(aes(group=variant)) #+ geom_point()
#ggp <-ggplot(filter(res_table, hermitian == 1, complex == 0, tol == 1e-12), aes(size, avg_runtime, colour = version)) + facet_wrap(~variant, ncol=2) + geom_smooth(aes(group=version)) #+ geom_point()
ggp + labs(x =  "size", y = "average runtime in sec")
dev.off()

#elementwise <- filter(table, hermitian == 1, complex == 0, version == "elementwise")
#elementwise2 <- filter(table, hermitian == 1, complex == 0, version == "elementwise2")
#diff <- left_join(elementwise, elementwise2, by = c("size", "seed", "tol", "complex", "hermitian"));
#diff <- mutate(diff, diff_runtime = runtime.in.s.y - runtime.in.s.x);
#new_diff <- diff %>% group_by(size, hermitian, complex, tol, seed) %>%
#  summarize(avg_runtime = mean(diff_runtime));
#
#tikz('diff.tex',width=7,height=3.5)
#ggp <-ggplot(new_diff, aes(size, avg_runtime)) + geom_smooth() #+ geom_point()
#ggp + labs(x =  "size", y = "averange difference in runtime")
#dev.off()

