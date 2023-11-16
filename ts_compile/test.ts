type ChainClass<T> =  T extends {
  (...args: unknown[]): unknown;
  private?: boolean;
} ? T : Record<keyof T, (...args: any) => T>;
class EngineInstance implements ChainClass<EngineInstance> {
  private modules = new Set<typeof Module>();
  public module<A extends any[], M extends Module>(
    m: typeof Module & { init: (...args: A) => M },
    ...args: A
  ): this {
    console.log('Adding module:', m.name);
    if (this.modules.has(m)) {
      // warn(`Module \`${m.name}\` has already been initialized!`);
    }
    m.init(...args); // Initiate the module
    if (!m.wasProperlyInitialized()) {
      // error(`Module \`${m.name}\` not properly initialized!`);
      return this;
    }
    this.modules.add(m);
    return this;
  }
}
export const Engine = new EngineInstance();
(window as unknown as any).Engine = Engine;
const enum A {
  some = "hello" + "world",other = ""
}
type B = Record<string, number> & {ok: string};

// const fn = <T>(a: T): T => { return a; }
// fn<123>(123);
// (fn)<number>(123);
// [fn][0]<number>(123);

// console.log(test1 < number, 2);
// console.log(test1 < number > 2);
// console.log(test1 , number > (2));
// console.log(test1 < number > (2));
// console.log(test1 < number > (2 + 1));
// console.log(()<{} | {}>(1 + 2)]);

enum Test {
  a = 1,
  b,
  c
}
