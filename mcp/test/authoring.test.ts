import { afterAll, beforeAll, expect, setDefaultTimeout, test } from "bun:test";
import { chmod, mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { delimiter, dirname, join } from "node:path";
import { Client } from "@modelcontextprotocol/sdk/client/index.js";
import { StdioClientTransport } from "@modelcontextprotocol/sdk/client/stdio.js";
import { CallToolResultSchema } from "@modelcontextprotocol/sdk/types.js";

setDefaultTimeout(30_000);

const catalog = `#lang typify
declare : Catalog
is : category = "book" | "journal"
is : title = /[^=]+/
term : item : category title
form : item : <category> = <title>
does : item <category> <title>
count : item = 1..*
end!
`;

const server = Bun.fileURLToPath(new URL("../server.ts", import.meta.url));
const readerSource = Bun.fileURLToPath(
  new URL("../../src/main.rkt", import.meta.url),
);
let project: string;
let declarationsPath: string;
let reader: string;

function quoteShell(value: string) {
  return "'" + value.replaceAll("'", "'\\''") + "'";
}

beforeAll(async () => {
  const racket = Bun.which("racket");
  if (!racket)
    throw new Error("Install Racket to run the MCP reader integration tests.");
  project = await mkdtemp(join(tmpdir(), "typify-mcp-client-"));
  reader = join(project, "test-reader");
  declarationsPath = join(project, "Catalog.typify");
  await Bun.write(
    reader,
    `#!/bin/sh\nexec ${quoteShell(racket)} -U ${quoteShell(readerSource)} "$@"\n`,
  );
  await chmod(reader, 0o700);
  await Bun.write(declarationsPath, catalog);
});

afterAll(async () => {
  if (project) await rm(project, { recursive: true, force: true });
});

async function withClient(
  args: string[],
  run: (client: Client) => Promise<void>,
) {
  const client = new Client({ name: "typify-author-test", version: "1.0.0" });
  const transport = new StdioClientTransport({
    command: Bun.argv[0],
    args: [server, "--reader", reader, ...args],
    cwd: project,
    stderr: "inherit",
    env: {
      HOME: project,
      PATH: [dirname(Bun.argv[0]), Bun.env.PATH].join(delimiter),
      LANG: "C.UTF-8",
    },
  });
  try {
    await client.connect(transport);
    await run(client);
  } finally {
    await client.close();
  }
}

async function call(
  client: Client,
  name: string,
  args: Record<string, unknown>,
) {
  return CallToolResultSchema.parse(
    await client.callTool({ name, arguments: args }),
  );
}

test("reader tools discover and explain the language selected by the host", async () => {
  await withClient(["--declarations", declarationsPath], async (client) => {
    const names = (await client.listTools()).tools
      .map((tool) => tool.name)
      .sort();
    expect(names).toEqual(["ask", "check"]);
    const result = await call(client, "ask", {
      language: "Catalog",
      word: "item",
    });
    expect(result.isError).not.toBe(true);
    expect(result.structuredContent).toMatchObject({
      status: "declared",
      word: "item",
      lines: expect.arrayContaining(["? : form : item : <category> = <title>"]),
    });
    const text = result.content.find((part) => part.type === "text");
    expect(text?.type === "text" && JSON.parse(text.text)).toEqual(
      result.structuredContent,
    );

    const unknown = await call(client, "ask", {
      language: "Catalog",
      word: "shelf",
    });
    expect(unknown.isError).not.toBe(true);
    expect(unknown.structuredContent).toMatchObject({
      status: "undeclared",
      word: "shelf",
    });
  });
});

test("concurrent proposed edits get independent terms and native source diagnostics", async () => {
  await withClient([], async (client) => {
    const valid = call(client, "check", {
      declarations: catalog,
      language: "Catalog",
      program: "item : journal = Survey\n",
    });
    const invalid = call(client, "check", {
      declarations: catalog.replace('"journal"', '"leaf"'),
      language: "Catalog",
      program: "item : journal = Survey\n",
    });
    const [accepted, rejected] = await Promise.all([valid, invalid]);
    expect(accepted.isError).not.toBe(true);
    expect(accepted.structuredContent).toMatchObject({
      ok: true,
      terms: [["item", "journal", "Survey"]],
    });
    expect(rejected.isError).toBe(true);
    expect(rejected.structuredContent).toMatchObject({
      ok: false,
      terms: [],
      diagnostics: expect.arrayContaining([
        expect.objectContaining({
          code: "E_CLASS",
          data: expect.objectContaining({
            class: "category",
            token: "journal",
          }),
          range: expect.objectContaining({
            start: expect.objectContaining({ line: 1, column: 8 }),
          }),
        }),
      ]),
    });
  });
});

test("a declaration alone can be checked and a proposed declaration leaves the host file intact", async () => {
  await withClient(["--declarations", declarationsPath], async (client) => {
    const clean = await call(client, "check", {});
    expect(clean.structuredContent).toMatchObject({
      ok: true,
      diagnostics: [],
    });
    const failed = await call(client, "check", {
      declarations: "#lang typify\ndeclare : Catalog\n",
    });
    expect(failed.isError).toBe(true);
    expect(failed.structuredContent).toMatchObject({
      ok: false,
      diagnostics: expect.arrayContaining([
        expect.objectContaining({ code: "E_DECLARATION" }),
      ]),
    });
    expect(await Bun.file(declarationsPath).text()).toBe(catalog);
  });
});

test("the logical program path supplies the declared file captures", async () => {
  const declarations = `#lang typify
declare : Slots
is : seat = /[0-9]+/
file : SLOT[<seat>]/data.typify
term : tag : seat
form : ready!
does : tag <seat>
end!
`;
  await withClient([], async (client) => {
    const result = await call(client, "check", {
      declarations,
      language: "Slots",
      programPath: "SLOT[27]/data.typify",
      program: "ready!",
    });
    expect(result.isError).not.toBe(true);
    expect(result.structuredContent).toMatchObject({
      ok: true,
      terms: [["tag", "27"]],
    });
  });
});

test("an optional preview adapter receives only the configured workplace and requested edits", async () => {
  const preview = join(project, "test-preview");
  await Bun.write(
    preview,
    `#!/usr/bin/env bun
const input = await Bun.stdin.json();
console.log(JSON.stringify({status:"ok",workspace:Bun.argv[2],edits:input.edits}));
`,
  );
  await chmod(preview, 0o700);
  await withClient(
    ["--workplace", project, "--preview", preview],
    async (client) => {
      const names = (await client.listTools()).tools
        .map((tool) => tool.name)
        .sort();
      expect(names).toEqual(["ask", "check", "preview"]);
      const edits = [
        { path: "Catalog.typify", content: catalog.replace("book", "volume") },
      ];
      const result = await call(client, "preview", { edits });
      expect(result.isError).not.toBe(true);
      expect(result.structuredContent).toMatchObject({
        status: "ok",
        workspace: project,
        edits,
      });
      expect(await Bun.file(declarationsPath).text()).toBe(catalog);
    },
  );
});

test("cancelling an MCP preview releases its running adapter", async () => {
  const preview = join(project, "slow-preview");
  await Bun.write(
    preview,
    `#!/usr/bin/env bun
await Bun.write(Bun.argv[2] + "/adapter.pid", String(process.pid));
await Bun.sleep(20_000);
`,
  );
  await chmod(preview, 0o700);
  async function waitUntil(
    condition: () => boolean | Promise<boolean>,
    message: string,
  ) {
    const deadline = Date.now() + 6_000;
    while (!(await condition())) {
      if (Date.now() >= deadline) throw new Error(message);
      await Bun.sleep(25);
    }
  }
  await withClient(
    ["--workplace", project, "--preview", preview],
    async (client) => {
      const controller = new AbortController();
      const request = client
        .callTool(
          {
            name: "preview",
            arguments: {
              edits: [{ path: "Catalog.typify", content: catalog }],
            },
          },
          CallToolResultSchema,
          { signal: controller.signal },
        )
        .then(
          () => "resolved",
          () => "cancelled",
        );
      try {
        let pid = 0;
        await waitUntil(async () => {
          const file = Bun.file(join(project, "adapter.pid"));
          if (await file.exists()) pid = Number(await file.text());
          return Number.isInteger(pid) && pid > 1;
        }, "Adapter did not start");
        controller.abort();
        expect(await request).toBe("cancelled");
        await waitUntil(() => {
          try {
            process.kill(pid, 0);
            return false;
          } catch (error) {
            if ((error as NodeJS.ErrnoException).code !== "ESRCH") throw error;
            return true;
          }
        }, "Adapter process survived cancellation");
        expect((await client.listTools()).tools).toHaveLength(3);
      } finally {
        controller.abort();
      }
    },
  );
});
