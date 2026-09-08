# Enrichment tool purpose
An alert provides a minimal information about alert source. Usually it's only host name and what's wrong on it. An operator need a full information about alert subject and it infrastructure. Thus, we need to gather and provide all available information about alert subject. We have additional sources of information: Jira, Confluence, Jira assets plugin, chat conversations and others.

# Implementation details
Each additional information source should be cached on the app side as a records in the database. Each record should have a link to source of information and a timestamp when this information was actualized.
Also each additional information provider should be represented as a tool for LLM sub-system which can use it to gather more info during alert analisis procedure.

# Phase 0 targets
- make a module to gather information from Jira Assets database
- attach an info card to an alert: object type, who is owner, what is cluster and/or database name, ip addresses, datacenters and so on
- provide this information to an LLM via prompt template
- web interface to manage additional info sources (full CRUD for admin role)
- implement a agent role abstraction instead of plain prompt template; It should include role name, prompt template, set of available tools
- alert enrichment methods should have a selectable set of agent roles to perform 
- mock service for jira assets plugin to perform a smoke tests against it

# References
- design_docs/assets-api.md
- design_docs/examples/jira_assets_api_readonly_test.sh
