#!/usr/bin/env bun
import { parseArgs } from "node:util";
import { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";
import type { CallToolResult } from "@modelcontextprotocol/sdk/types.js";
import { z } from "zod";
import { readTypify, runJson } from "./reader.ts";
import metadata from "./package.json";

const usage = "typify-mcp [--reader PATH] [--declarations FILE] [--workplace DIR --preview EXECUTABLE]";

function executable(value: string) {
  const path = Bun.which(value);
  if (!path) throw new Error(`Executable not found: ${value}`);
  return path;
}

async function result(call: Promise<{ body: Record<string, unknown>; failed: boolean }>): Promise<CallToolResult> {
  try {
    const { body, failed } = await call;
    return { content: [{ type: "text", text: JSON.stringify(body) }], structuredContent: body, isError: failed };
  } catch (error) {
    const message = error instanceof Error ? error.message : String(error);
    return { content: [{ type: "text", text: message }], isError: true };
  }
}

async function main() {
  const { values } = parseArgs({
    args: Bun.argv.slice(2),
    options: {
      reader: { type: "string" },
      declarations: { type: "string" },
      workplace: { type: "string" },
      preview: { type: "string" },
      help: { type: "boolean", short: "h" },
    },
  });
  if (values.help) {
    console.log(usage);
    return;
  }
  if (Boolean(values.workplace) !== Boolean(values.preview)) {
    throw new Error("--workplace and --preview must be supplied together");
  }
  const reader = executable(values.reader ?? "typify");
  const adapter = values.preview ? executable(values.preview) : undefined;
  const workplace = values.workplace;
  const configuredFile = values.declarations && Bun.file(values.declarations);
  async function source(text?: string) {
    if (text !== undefined) return text;
    if (configuredFile) return configuredFile.text();
    throw new Error("Supply declaration text in declarations or start the server with --declarations FILE.");
  }
  const server = new McpServer({ name: metadata.name, version: metadata.version });
  const declarations = z.string().optional().describe("Typify declaration source. Omit to use the configured file.");
  const language = z.string().min(1).describe("Declared language name, preserving case.");
  const annotations = { readOnlyHint: true, openWorldHint: false };
  server.registerTool("ask", {
    description: "Look up one word in a declared language. Returns the reader's definition, forms, constraints and nearby words.",
    inputSchema: z.strictObject({ declarations, language, word: z.string().min(1) }),
    annotations,
  }, ({ declarations: text, language, word }, { signal }) =>
    result(source(text).then((src) => readTypify(reader, "ask", src, { language, word }, signal))));

  server.registerTool("check", {
    description: "Validate declarations alone, or supply language and program together to check a proposed program. Returns native terms and diagnostics.",
    inputSchema: z.strictObject({
      declarations,
      language: language.optional(),
      program: z.string().optional(),
      programPath: z.string().min(1).optional().describe("Logical relative filename for a program subject to a file declaration."),
    }),
    annotations,
  }, ({ declarations: text, language, program, programPath }, { signal }) =>
    result(source(text).then((src) => {
      if ((language === undefined) !== (program === undefined) || (programPath !== undefined && program === undefined)) {
        throw new Error("Supply language and program together. programPath applies only to a program.");
      }
      return readTypify(reader, "check", src, { language, program, programPath }, signal);
    })));

  if (workplace && adapter) {
    server.registerTool("preview", {
      description: "Compare the configured workplace with proposed Typify file edits. Shows changes to the effective seat policy without applying the edits or starting a floor.",
      inputSchema: z.strictObject({
        edits: z.array(z.strictObject({
          path: z.string().min(1).describe("Project-relative .typify source path."),
          content: z.string().nullable().describe("Complete proposed file text, or null to remove a file."),
        })),
      }),
      annotations,
    }, ({ edits }, { signal }) =>
      result(runJson([adapter, workplace], {
        input: JSON.stringify({ edits }), signal, timeoutMs: 300_000,
        env: { ...Bun.env, TYPIFY_PREVIEW_READER: reader },
      })));
  }
  await server.connect(new StdioServerTransport());
}

try {
  await main();
} catch (error) {
  console.error(error instanceof Error ? error.message : String(error));
  process.exitCode = 1;
}
