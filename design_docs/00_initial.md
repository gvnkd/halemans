# Overview

- Halemans :: Home ALErt MANagement System
- Client-server application to gather alerts from various alerting systems like Zabbix, Grafana, Prometheus Alertmanage and so on.
- Client side is a web application which represents current status for observed environmnets and allow operators to manage alerts via provided tools
- Server side is a bunch of services in Haskell language with using IHP framework which gather, group, deduplicate alerts, listen for web-hooks from external services and trigger web-hooks of an external services
- Halemans try to heavily use variuos LLM to analyze current state and new alerts to provide a detailed information about environment state and advice how to resolve issues and how to find and fix it root causes

# Features
## Server side
- gather alerts from Zabbix via it API
- gather alerts from Grafana via it API
- gather information about alert subjects from Confluence CMDB via API
- gather information about related tasks from Jura tickets via API
- internally manage alert state, including: alert start time, is it acknowledged, who and when acknowledge it, what is current state including comments from SRE team, etc.
- has a rule set how to group alerts to prevent identical notifications
- has an internal users database with it roles, profiles, desired settings, etc.
- has an internal roles database with it privileges
- has an internal notification and escalation rules
- has a teams abstraction which used in notification and escalation rule sets
- each user can be a member of multiple teams
- has a environment/host/service blackouts conceptions

## Client side (web UI)
- main page is a overview dashboard which represents current state of all environments
- each environment has a dedicated detailed page
- each alert has an own card which represents current alert state and history of all actions about this alert
- users can define own dashboards with desired set of environments and filters
- support for color themes for each user: Catpuccin, Dracula and so on
- interface to interact with alerts: acknowledge, close, escalate, etc.
- cards to show additional information about alert: related jira tickets, confluence document, LLM-agent analisis, etc.
