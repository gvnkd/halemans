module Application.Script.HalemansMcp (run) where

import Application.Script.Prelude hiding (run)
import Application.Service.Agent.Mcp (mcpStdioServer)

-- MCP stdio server entry point (internal API milestone). The serving loop is
-- in Application.Service.Agent.Mcp; runScript supplies ?modelContext and the
-- app config. HALEMANS_MCP_USER selects the act-as user.
run :: Script
run = mcpStdioServer
