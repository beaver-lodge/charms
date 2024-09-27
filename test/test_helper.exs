ExUnit.start()

children = [
  {DynamicSupervisor, name: Charms.TestDynamicSupervisor, strategy: :one_for_one}
]

Supervisor.start_link(children, strategy: :one_for_one)
