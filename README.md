An agent-runner for Xi in Emacs

##Installation
###Expected directory structure
Xi  
|-- agents  
|-- logs  
|-- xal-javascript  
\`-- xi-core  

###Load module
In your emacs config:

    (load <path_to_ximacs.el>)

Set `xi-directory` to the path of the root-directory and `xi-init-agents` to the list of agent names  
To be crude, simply copy the contents of the `ximacs.el` into a buffer, change the required variables and run `eval-buffer`.

##Commands

###xi-start
Start xi-core and agents specified in `xi-init-agents`

###xi-kill
Kill xi-core and running agents.

###xi-start-agent / xi-kill-agent / xi-restart-agent
Start/kill/restart the specified agent.

###xi-start-core
Start xi-core.
Do not use `xi-start-agent` to start xi-core.

###xi-show-log
Display the log of the specified agent

###xi-list-running-agents
Display the currently running agents.  
Use `p` and `n` to navigate the list, `l` to display the logs and `k` to kill the agent.



