require(carData); require(dagitty)
data("GSSvocab")
GSSvocab <- GSSvocab[complete.cases(GSSvocab), ]
g <- dagitty("dag {
             ageGroup [pos=\"0,0\"]
             vocab [pos=\"1,-1\"]
             nativeBorn [pos=\"2,-2\"]
             educ [pos=\"3,-1\"]
             gender [pos=\"4,0\"]
             nativeBorn -> educ
             nativeBorn -> vocab
             educ -> vocab
             gender -> educ
             ageGroup -> vocab
             }")
plot(g)
non_param_dag_model <- non_parametric_dag_model(dag = g, data = GSSvocab)
sim_data <- sim_mixed_dag(dag_model = non_param_dag_model, N = 30000)
boxplot(vocab ~ nativeBorn, data = sim_data)
# verify distribtion in simulated dataset looks similar
boxplot(vocab ~ nativeBorn, data = GSSvocab)