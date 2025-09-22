import * as ts from "typescript";

const PORT = 8033;

let startTime = 0;
let resettableCompiledSnippets = 0;

console.log(`Starting server on http://localhost:${PORT}...`);

/**
 * Compiles a string of TypeScript code into JavaScript.
 * @param typescriptCode The TypeScript code to compile.
 * @returns The compiled JavaScript code.
 */
function compileTypeScript(typescriptCode: string): string {
  // Use the TypeScript compiler API to transpile the code.
  // transpileModule is a simple way to compile a single string of code.
  const result = ts.transpileModule(typescriptCode, {
    compilerOptions: {
      module: ts.ModuleKind.ESNext,
      target: ts.ScriptTarget.ESNext,
      removeComments: true,
    },
  });

  // The compiled JavaScript code is in the outputText property.
  return result.outputText;
}

// --- Server Setup ---
const server = Bun.serve({
  port: PORT,
  async fetch(req) {
    const url = new URL(req.url);

    if (url.pathname === '/reset_counter') {
      startTime = performance.now();
      resettableCompiledSnippets = 0;
      return new Response("", { status: 200 });
    } else if (url.pathname === '/print_counter') {
      const tookSeconds = ((performance.now() - startTime) / 1000).toFixed(3);
      console.log(`Compiled ${resettableCompiledSnippets} TypeScript snippets in ${tookSeconds}s`);
      console.log(`  (NOTE: this time is NOT an accurate depiction of TSC's compilation times)`)
      return new Response("" + resettableCompiledSnippets, { status: 200 });
    }

    // Only respond to POST requests to the /compile endpoint
    if (url.pathname !== "/compile" || req.method !== "POST") {
      return new Response("Not Found", { status: 404 });
    }

    resettableCompiledSnippets++;

    try {
      // Get the TypeScript code from the request body
      const typescriptCode = await req.text();

      if (!typescriptCode) {
        return new Response("Request body is empty. Please provide TypeScript code.", {
          status: 400,
        });
      }

      console.log("Received TypeScript code. Compiling...");
      const javascriptCode = compileTypeScript(typescriptCode);
      console.log("Compilation successful. Sending response.");

      // Send the compiled JavaScript back to the client
      return new Response(javascriptCode, {
        headers: { "Content-Type": "application/javascript" },
      });
    } catch (error) {
      console.error("An error occurred during compilation:", error);
      // If something goes wrong, send a server error response
      return new Response(`Compilation failed: ${error instanceof Error ? error.message : "Unknown error"}`, {
        status: 500,
      });
    }
  },
  error() {
    return new Response("An unexpected error occurred.", { status: 500 });
  },
});

console.log(`Bun server listening on http://localhost:${server.port}`);
