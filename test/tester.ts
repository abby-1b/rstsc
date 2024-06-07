// import { transpile } from "https://deno.land/x/emit/mod.ts";

// const tests = (() => {
//     let file = Deno.readTextFileSync("./tests.ts");
//     return file
//         .slice(1, file.length - 1)
//         .split("}\n{");
// })();

enum PassState {
    TextMatches,
}

async function compileWithTSC(code: string): Promise<string> {
    // Create temp file
    const tempFile = './.' + crypto.randomUUID() + '.ts';
    await Deno.writeTextFile(tempFile, code);

    // Compile
    const out = (await new Deno.Command('tsc', { args: [
        tempFile, '--outFile', '/dev/stdout'
    ] }).output()).stdout;
    const compiled = decoder.decode(out);

    // Delete temp file
    Deno.remove(tempFile);

    // Return
    return compiled;
}

async function compileWithRSTSC(code: string): Promise<string> {

}

const decoder = new TextDecoder();
async function runTest(test: string): PassState {
    const expected = await compileWithTSC(test);
    const got = await compileWithRSTSC(test);


}

// await runTest(`
// let a: number = 123;
// `);

