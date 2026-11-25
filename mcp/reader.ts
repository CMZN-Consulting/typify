import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

type JsonResult = { body: Record<string, unknown>; failed: boolean };
type JsonOptions = {
  input?: string;
  signal?: AbortSignal;
  timeoutMs?: number;
  env?: Record<string, string | undefined>;
  cwd?: string;
};

export async function runJson(
  command: string[],
  options: JsonOptions = {},
): Promise<JsonResult> {
  const timeout = AbortSignal.timeout(options.timeoutMs ?? 60_000);
  const limit = new AbortController();
  const signals = [timeout, limit.signal];
  if (options.signal) signals.push(options.signal);
  const stop = AbortSignal.any(signals);
  stop.throwIfAborted();
  const grouped = process.platform !== "win32";
  const child = Bun.spawn(command, {
    cwd: options.cwd,
    env: options.env,
    stdin: options.input === undefined ? "ignore" : new Blob([options.input]),
    stdout: "pipe",
    stderr: "pipe",
    detached: grouped,
  });
  function terminate(signal: "SIGTERM" | "SIGKILL") {
    if (!grouped) return child.kill(signal);
    try {
      process.kill(-child.pid, signal);
    } catch (error) {
      if ((error as { code?: string }).code !== "ESRCH") throw error;
    }
  }
  const abort = () => terminate("SIGTERM");
  stop.addEventListener("abort", abort, { once: true });
  let bytes = 0;
  function output(stream: ReadableStream<Uint8Array>): Promise<string> {
    const capped = stream.pipeThrough(
      new TransformStream<Uint8Array, Uint8Array>({
        transform(chunk, controller) {
          bytes += chunk.byteLength;
          if (bytes > 8 * 1024 * 1024) {
            limit.abort(
              new Error("Subprocess exceeded the 8 MiB output limit"),
            );
          } else {
            controller.enqueue(chunk);
          }
        },
      }),
      { signal: stop },
    );
    return new Response(capped).text();
  }
  try {
    const [stdout, stderr, exit] = await Promise.all([
      output(child.stdout),
      output(child.stderr),
      child.exited,
    ]);
    stop.throwIfAborted();
    let body: unknown;
    try {
      body = JSON.parse(stdout);
    } catch {
      throw new Error(
        `${command[0]} returned invalid JSON (exit ${exit}): ${stderr.trim().slice(0, 1024)}`,
      );
    }
    if (typeof body !== "object" || !body || Array.isArray(body)) {
      throw new Error(`${command[0]} returned an unexpected JSON value`);
    }
    return { body: body as Record<string, unknown>, failed: exit !== 0 };
  } finally {
    stop.removeEventListener("abort", abort);
    if (stop.aborted) {
      await Promise.race([child.exited, Bun.sleep(1500)]);
      terminate("SIGKILL");
      await child.exited;
      stop.throwIfAborted();
    }
  }
}

type ReaderOptions = {
  language?: string;
  word?: string;
  program?: string;
  programPath?: string;
};

export async function readTypify(
  reader: string,
  operation: "ask" | "check",
  declarations: string,
  options: ReaderOptions,
  signal?: AbortSignal,
): Promise<JsonResult> {
  signal?.throwIfAborted();
  const executable = Bun.which(reader);
  if (!executable)
    throw new Error(`Typify reader executable not found: ${reader}`);

  const { language, word, program, programPath = "program.typify" } = options;
  if (
    /[\\\0]/.test(programPath) ||
    /^[A-Za-z]:/.test(programPath) ||
    programPath
      .split("/")
      .some((part) => !part || part === "." || part === "..")
  )
    throw new Error(
      "programPath must be a relative file path inside the proposed program tree",
    );

  const declarationPath =
    programPath.split("/")[0].toLowerCase() === "declarations.typify"
      ? "language.typify"
      : "declarations.typify";
  const args = ["--json", "--", operation, declarationPath];
  if (operation === "ask") {
    if (language === undefined || word === undefined)
      throw new Error("ask requires language and word");
    args.push(language, word);
  } else if (
    language !== undefined ||
    program !== undefined ||
    options.programPath !== undefined
  ) {
    if (language === undefined || program === undefined)
      throw new Error("checking a program requires language and program");
    args.push(language, programPath);
  }

  const directory = await mkdtemp(join(tmpdir(), "typify-mcp-"));
  try {
    await Bun.write(join(directory, declarationPath), declarations);
    if (operation === "check" && program !== undefined) {
      await Bun.write(join(directory, programPath), program, {
        createPath: true,
      });
    }
    return await runJson([executable, ...args], { cwd: directory, signal });
  } finally {
    await rm(directory, { recursive: true, force: true });
  }
}
