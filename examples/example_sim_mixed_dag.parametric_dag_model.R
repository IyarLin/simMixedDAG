require(dagitty)
g <- dagitty("dag {
sales [pos=\"0,0\"]
             mkt [pos=\"2,0\"]
             comp [pos=\"1,1\"]
             visits [pos=\"1,0\"]
             visits -> sales
             mkt -> visits
             comp -> mkt
             comp -> sales
             }")

plot(g)

param_dag_model <- parametric_dag_model(
  dag = g,
  f.args = list(
    sales = list(betas = list(visits = 0.3, comp = -0.9)),
    visits = list(betas = list(mkt = 0.5)),
    mkt = list(betas = list(comp = 0.6))
  )
)

sim_data <- sim_mixed_dag(dag_model = param_dag_model)

plot(sim_data$mkt, sim_data$sales) # confounded relation
