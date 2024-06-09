mod common;
use common::test_multiple;

#[test]
fn basic_functions() {
    test_multiple(&[

"
function add(a: number, b: number): number { return a + b; }
",
"
function greet(name: string) { console.log('Hello, ' + name); }
",
"
function identity<T>(arg: T): T { return arg; }
",
"
function log(message: string, userId?: string) { console.log(message); }
",
"
function increment(value: number, incrementBy: number = 1): number {
    return value + incrementBy;
}
",
"
function double(x: number): number;
function double(x: string): string;
function double(x: any): any { return x + x; }
",
"
function isString(x: any): x is string { return typeof x === 'string'; }
",
"
function reverse<T>(items: T[]): T[] { return items.reverse(); }
",
"
function logLength(obj: { length: number }) { console.log(obj.length); }
",
"
type Container<T> = { value: T };
let numberContainer: Container<number> = { value: 123 };
",
"
type ReadonlyPoint = { readonly x: number; readonly y: number };
let readonlyPoint: ReadonlyPoint = { x: 10, y: 20 };
",
"
function map<T, U>(array: T[], fn: (item: T) => U): U[] {
    return array.map(fn);
}
",
"
function identityFunction<T>(arg: T): T { return arg; }
",
"
function assertNever(x: never): never {
    throw new Error('Unexpected value: ' + x);
}
",
"
function nonNull<T>(arg: T | null): T {
    if (arg === null) throw new Error('Null value'); return arg;
}
let nonNullValue = nonNull('Non-null');
",
"
function throwError(message: string): never { throw new Error(message); }
",
"
function doLog(message: any, ...optionalParams: any[]) {
    console.log(message, ...optionalParams);
}
doLog('Message', 1, 2, 3);
",
"
function overloadedFunction(x: number): number;
function overloadedFunction(x: string): string;
function overloadedFunction(x: any): any { return x; }
let overloadedResult = overloadedFunction('test');
",
"
function withDefaultParameter<T>(value: T = {} as T): T { return value; }
let defaultParameterResult = withDefaultParameter();
",
"
function hasProperty<T extends object, K extends keyof T>(
    obj: T, key: K
): boolean {
    return key in obj;
}
let hasPropertyResult = hasProperty({ a: 1, b: 2 }, 'a');
",
"
function assertIsString(val: any): asserts val is string {
    if (typeof val !== 'string') throw new Error('Not a string');
}
let maybeString: any = 'I\\'m a string';
assertIsString(maybeString);
",
"
function weirdSpacingFunction ( a : number , b : number ) : number { return a + b ; }
",
"
function variadicTuples(...args: [string, number, boolean]): void {}
variadicTuples('test', 123, true);
",
"
function defaultGeneric<T = string>(value: T): T { return value; }
let defaultGenericResult = defaultGeneric('lol');
",

    ], common::WhiteSpace::IgnoreAll);
}
