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

nativeBorn_ATE_on_vocab <- get_ate(dag_model = non_param_dag_model, treatment = "nativeBorn", exposure = "vocab")
print(nativeBorn_ate_on_vocab)
