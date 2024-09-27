library(tidyverse)
library(tikzDevice)


full_path_new <- "data/";

full_path <- "../compare_symm/";


files <- list.files(path = full_path);
table <- NULL;

for (i in 1:length(files)) {
  print(files[i])
  table <- rbind(table, read.csv(paste0(full_path, files[i]), sep = ","));
}
table <- filter(table, version == "elementwise2", seed == 1);

files <- list.files(path = full_path_new);
new_table <- NULL;

for (i in 1:length(files)) {
  print(files[i])
  new_table <- rbind(new_table, read.csv(paste0(full_path_new, files[i]), sep = ","));
}
#new_table <- filter(new_table, version == "elementwise2", seed == 1);


diff <- left_join(table, new_table, by = c("size", "seed", "tol", "complex", "hermitian"));
diff <- mutate(diff, diff_runtime = runtime.in.s.x - runtime.in.s.y);
diff <- filter(diff, seed == 1) %>%

new_diff <- diff %>% group_by(version.y, size, hermitian, complex, tol, seed) %>%
  summarize(avg_runtime = mean(diff_runtime));
new_diff <- mutate(new_diff, version.y = recode(version.y, eigen_elementwise = "eigen elementwise"))

tikz('diff.tex',width=7,height=3.5)
ggp <-ggplot(new_diff, aes(size, avg_runtime, colour = version.y)) + geom_smooth() #+ geom_point()
ggp + labs(x =  "size", y = "averange difference in runtime")
dev.off()


#table <- mutate(table, version = recode(table$version, '101' = "Implicit Shift", '201' = "Double Shift with 1ev", '301' = "Double Shift", '991' = "Eigen"));


res_table <- table %>% group_by(version, size, hermitian, complex, tol) %>%
  summarize(avg_runtime = mean(runtime.in.s)
, min_error = min(min_error), max_error = max(max_error), avg_error = mean(avg_error));

# Create unsymmetric matrix graph
#tikz('../../../../acmart/fig/unsymmetricGraph.tex',width=7,height=3.5)
tikz('test.tex',width=7,height=3.5)
#ggp <-ggplot(filter(res_table, hermitian == 0, complex == 0, tol < 1e-7, version != "Eigen"), aes(size, avg_error, colour = version)) + geom_smooth(aes(group=version)) + geom_point()
#ggp <-ggplot(filter(res_table, hermitian == 1, complex == 0, tol < 1e-7, version != "new_eigen_matrix", version != "elementwise2", version != "elementwise"), aes(size, avg_error, colour = version)) + geom_smooth(aes(group=version)) #+ geom_point()
#ggp <-ggplot(filter(res_table, hermitian == 1, complex == 0, tol < 1e-7), aes(size, avg_runtime, colour = version)) + geom_smooth(aes(group=version)) #+ geom_point()
ggp <-ggplot(filter(res_table, hermitian == 1, complex == 0, tol < 1e-7, version != "EIGEN", version != "new_eigen_matrix", version != "eigen_elementwise"), aes(size, avg_runtime, colour = version)) + geom_smooth(aes(group=version)) #+ geom_point()
ggp + labs(x =  "size", y = "average runtime")
dev.off()

elementwise <- filter(new_table, hermitian == 1, complex == 0, version == "elementwise")
elementwise2 <- filter(new_table, hermitian == 1, complex == 0, version == "elementwise2")
diff <- left_join(elementwise, elementwise2, by = c("size", "seed", "tol", "complex", "hermitian"));
diff <- mutate(diff, diff_runtime = runtime.in.s.y - runtime.in.s.x);
new_diff <- diff %>% group_by(size, hermitian, complex, tol, seed) %>%
  summarize(avg_runtime = mean(diff_runtime));

tikz('diff.tex',width=7,height=3.5)
ggp <-ggplot(new_diff, aes(size, avg_runtime)) + geom_smooth() #+ geom_point()
ggp + labs(x =  "size", y = "averange difference in runtime")
dev.off()

