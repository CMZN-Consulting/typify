# Typify over MCP

An editor or model can ask what a word means in a particular Typify language and submit an edit for checking. The server speaks MCP over standard input and output. It runs the Typify reader supplied by the operator. A workplace can also supply its planner for previews.

## Run locally

Use the latest released Bun and an executable Typify v1 reader with `ask` and `check`. If you are using a source checkout with Racket 9.3 CS, the following commands link the reader and create its launcher. Run them from the Typify repository root on a new installation.

```sh
raco pkg install --scope user --deps fail --name typify --link ./src
racket -e '(require setup/dirs) (displayln (build-path (find-user-console-bin-dir) "typify"))'
cd mcp
bun install --frozen-lockfile
bun server.ts --help
```

Point a stdio MCP client at the Bun executable and the server. Use the absolute launcher path printed by Racket as the value of `--reader`. If the launcher's directory is on the server's `PATH`, you can omit `--reader`. The optional `--declarations` file supplies a default when a caller omits declaration text.

```json
{
  "mcpServers": {
    "typify": {
      "command": "/absolute/path/to/bun",
      "args": [
        "/absolute/path/to/typify/mcp/server.ts",
        "--reader",
        "/absolute/path/to/typify-reader",
        "--declarations",
        "/absolute/path/to/catalog-language.typify"
      ]
    }
  }
}
```

## Author a language or program

With the Catalog declarations from the [repository example](../README.md), pass these arguments to the corresponding tools.

| Tool    | Arguments                                                    | Result                                                                                                  |
| ------- | ------------------------------------------------------------ | ------------------------------------------------------------------------------------------------------- |
| `ask`   | `{"language":"Catalog","word":"item"}`                       | The reader's declaration and accepted forms for `item`. An unknown word returns `status: "undeclared"`. |
| `check` | `{}`                                                         | The declaration file's validation result and diagnostics.                                               |
| `check` | `{"language":"Catalog","program":"item : journal = Survey"}` | The reader returns `["item","journal","Survey"]` in its terms.                                          |

To check a proposed change to the language itself, pass its complete source in `declarations`. To check a program whose declaration binds part of its filename, also pass the logical relative `programPath`. The server writes both texts under a fresh temporary directory per call. Native diagnostics retain their codes and source ranges in `structuredContent`. Rejected input sets the MCP tool error flag.

## Preview a workplace

Add `--workplace /absolute/path/to/workplace-checkout` and `--preview /absolute/path/to/tmux-workplace/bin/workplace-preview` together to expose `preview`. The adapter and its sibling `bin/workplace` must come from the same frame checkout.

By default the adapter uses the checkout's parent as the workplace root. If the plan depends on the checkout's position in a larger tree, set `WORKPLACE_ROOT` in the server environment. The checkout must be at or beneath that root. For example, a disposable checkout at `/tmp/trial/Workspace/house` with `WORKPLACE_ROOT=/tmp/trial` keeps the relative path `Workspace/house` during preview.

Call `preview` with `{"edits":[]}` for a baseline comparison. For a proposal, send one entry per file with a project-relative `path` and the full proposed `content`. A null content removes that file from the candidate. Edits may address a declared language layer or a file under `programs/`.

The adapter plans the original and proposed sources in a private directory and reports `planChanged`, `viewChanged`, `changedPrograms`, and before/after changes for individual seats. An edit can change the plan while leaving those seat settings equal. The planner skips physical desk inventory. Applying a proposal, starting a floor, and measuring runtime enforcement are separate operator actions. A large comparison runs the planner twice. A client can allow up to five minutes for the call.

## Verify

```sh
bun run typecheck
bun test
```

The tests start the server through the MCP SDK's stdio client and invoke the Racket reader from this checkout. They cover word lookup, invalid edits, file captures, concurrent requests, adapter discovery and cancellation.
