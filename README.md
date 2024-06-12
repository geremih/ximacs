An agent-runner for Xi in Emacs

## Installation
### Expected directory structure
Xi  
|-- agents  
|-- logs  
|-- xal-javascript  
\`-- xi-core  

### Load module
`M-x: package-install-file RET 'path_to_ximacs.el' RET`

Use `M-x customize-variable` to set the value of  `xi-directory` to the path of the root-directory and `xi-init-agents` to the list of agent names  

## Commands

### xi-start
Start xi-core and agents specified in `xi-init-agents`

### xi-kill
Kill xi-core and running agents.

### xi-start-agent / xi-kill-agent / xi-restart-agent
Start/kill/restart the specified agent.

### xi-start-core
Start xi-core.
Do not use `xi-start-agent` to start xi-core.

### xi-show-log
Display the log of the specified agent

### xi-list-running-agents
Display the currently running agents.  
Use `p` and `n` to navigate the list, `l` to display the logs, `r` to restart the agent and `k` to kill the agent.





