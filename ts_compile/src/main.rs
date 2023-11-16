
mod compiler;
pub mod tokenizer;
pub mod transformer;
pub mod emitter;

// const SOURCE: &str = "
// type ChainClass<T> =  T extends {
//   (...args: unknown[]): unknown;
//   private?: boolean;
// } ? T : Record<keyof T, (...args: any) => T>;
// class EngineInstance implements ChainClass<EngineInstance> {
//   private modules = new Set<typeof Module>();

//   public module<A extends any[], M extends Module>(
//     m: typeof Module & { init: (...args: A) => M },
//     ...args: A
//   ): this {
//     console.log('Adding module:', m.name); // Log
    
//     if (this.modules.has(m)) {
//      // warn(`Module `${m.name}` has already been initialized!`);
//     }

//     m.init(...args); // Initiate the module
//     if (!m.wasProperlyInitialized()) {
//       // error(`Module `${m.name}` not properly initialized!`);
//       return this;
//     }

//     this.modules.add(m); // Add the module to the list
//     return this;
//   }
// }

// export const Engine = new EngineInstance();

// // Expose the engine!
// (window as unknown as any).Engine = Engine;
// ";

const SOURCE: &str = include_str!("../test.ts");

// const SOURCE: &str = "
// enum A {
//     nice = 1
// }
// ";

fn main() {
    // type Test = number
    let output = compiler::compile(SOURCE);
    if output.is_err() {
        println!("Error: {}", output.err().unwrap());
    } else {
        println!("{}", output.unwrap());
    }
}
