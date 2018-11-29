
using Convex
using Cbc


type SDep
  from    :: Int64
  to      :: Int64
  latency :: Int64
end

type SModel
  model    :: Problem
  schedule :: Variable
  cycles   :: AbstractExpr
end

function sched_model(n_instrs :: Int64,
                     deps :: AbstractArray{SDep, 1},
					 n_exec_units :: Int64 = 2,
                     max_cycle :: Int64 = 12,
					 mode :: Symbol = :Real)
  sched_ext = Variable(n_instrs + 1, max_cycle, Positive(), mode)
  exit_node = n_instrs + 1
  sched = sched_ext[1:n_instrs,:]

  # Each instruction shall be scheduled exactly once
  sched_once = sum(sched_ext, 2) == 1

  # Cannot exceed number of HW resources in each cycle
  resources_ok = sum(sched, 1) <= n_exec_units

  # Exit dependencies
  exit_deps = [ SDep(i, exit_node, 0) for i in 1:n_instrs ]

  # Dependency constraints have to be satisfied
  deps_ok = [ sum(sched_ext[d.to,1:c]) <= sum(sched_ext[d.from, 1:(c - d.latency)])
							for d in [deps; exit_deps] for c in 1:max_cycle ]

  last_cycle = dot((1:max_cycle)', sched_ext[exit_node,:])

  p = minimize(last_cycle, [sched_once; deps_ok; resources_ok])
  solve!(p, CbcSolver())
  SModel(p, sched_ext, last_cycle)
end


