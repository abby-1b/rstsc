let x: number = 10;

function add(a: number, b: number): number { return a + b; }

function greet(name: string) { console.log('Hello, ' + name); }

interface Person { name: string; age: number; }

class Car { make: string; model: string; year: number; }

class Book { readonly title: string = 'TypeScript Handbook'; }

function identity<T>(arg: T): T { return arg; }

type StringOrNumber = string | number;

enum Color { Red, Green, Blue }

enum SomeEnum {
    ok = (13 * 2) + 1 * 2,
    test = 'crap',
    another,
}
let a = SomeEnum.ok;

const enum SomeEnum {
    ok = (13 * 2) + 1 * 2,
    test = 'crap',
}
let a = SomeEnum.ok;

let pair: [number, string] = [1, 'one'];

let id: string | number = 123;

type PersonDetails = { name: string } & { age: number };

function log(message: string, userId?: string) { console.log(message); }

function increment(value: number, incrementBy: number = 1): number {
    return value + incrementBy;
}

let numbers: number[] = [1, 2, 3];

let someValue: any = 'this is a string';
let strLength: number = (someValue as string).length;

function double(x: number): number;
function double(x: string): string;
function double(x: any): any { return x + x; }

namespace Geometry {
    export function area(radius: number): number {
        return Math.PI * radius * radius;
    }
}

abstract class Animal { abstract makeSound(): void; move(): void { console.log('Moving...'); } }

function isString(x: any): x is string { return typeof x === 'string'; }

let tuple: [number, string, boolean] = [1, 'two', true];

type User = { id: number; name: string; };
let user: User = { id: 1, name: 'Alice' };

function reverse<T>(items: T[]): T[] { return items.reverse(); }

let reversedNumbers: number[] = reverse([1, 2, 3]);

class Animal { constructor(public name: string) {} }
class Dog extends Animal { bark() { console.log('Woof!'); } }

interface Shape { area(): number; }
class Circle implements Shape {
    constructor(public radius: number) {}
    area(): number { return Math.PI * this.radius * this.radius; }
}

type Nullable<T> = T | null;
let nullableString: Nullable<string> = null;

function logLength(obj: { length: number }) { console.log(obj.length); }

type Container<T> = { value: T };
let numberContainer: Container<number> = { value: 123 };

type ReadonlyPoint = { readonly x: number; readonly y: number };
let readonlyPoint: ReadonlyPoint = { x: 10, y: 20 };

type Point = { x: number; y: number; };
function logPoint(p: Point): void { console.log(`x: ${p.x}, y: ${p.y}`); }
type PartialPoint = Partial<Point>;
let partialPoint: PartialPoint = { x: 10 };
type RequiredPoint = Required<PartialPoint>;
let requiredPoint: RequiredPoint = { x: 10, y: 20 };

type Record<K extends keyof any, T> = { [P in K]: T };

let nameAgeMap: Record<string, number> = { Alice: 25, Bob: 30 };

let mixedArray: (number | string)[] = [1, 'two', 3, 'four'];

type Predicate = (x: number) => boolean;
let isEven: Predicate = x => x % 2 === 0;

function map<T, U>(array: T[], fn: (item: T) => U): U[] {
    return array.map(fn);
}

let lengths: number[] = map(['one', 'two', 'three'], s => s.length);

function identityFunction<T>(arg: T): T { return arg; }

let identityValue: number = identityFunction<number>(42);

type ReadonlyArray<T> = readonly T[];
let readonlyNumbers: ReadonlyArray<number> = [1, 2, 3];

type Callback = () => void;
let callback: Callback = () => { console.log('Callback called'); };

type Intersection = { id: number } & { name: string };
let intersectedObject: Intersection = { id: 1, name: 'Intersected' };

type Union = { a: number } | { b: string };
let unionObject: Union = { a: 42 };

type ExtractedType = Extract<'a' | 'b' | 'c', 'a' | 'c'>;
let extractedValue: ExtractedType = 'a';

type ExcludedType = Exclude<'a' | 'b' | 'c', 'a' | 'c'>;
let excludedValue: ExcludedType = 'b';

function assertNever(x: never): never {
    throw new Error('Unexpected value: ' + x);
}

function nonNull<T>(arg: T | null): T {
    if (arg === null) throw new Error('Null value'); return arg;
}
let nonNullValue = nonNull('Non-null');

let doubleNestedArray: number[][] = [[1, 2], [3, 4]];

let deeplyNestedObject: { a: { b: { c: number } } } = {
    a: { b: { c: 42 } }
};

type Mapped<T> = { [K in keyof T]: T[K] };
let mappedObject: Mapped<{ x: number; y: string }> = { x: 10, y: 'hello' };

let neverArray: never[] = [];

function throwError(message: string): never { throw new Error(message); }

function doLog(message: any, ...optionalParams: any[]) {
    console.log(message, ...optionalParams);
}
doLog('Message', 1, 2, 3);

type Conditional<T> = T extends string ? string : number;
let conditionalValue: Conditional<boolean> = 42;

let unknownValue: unknown = 'could be anything';
if (typeof unknownValue === 'string') {
    let stringValue: string = unknownValue;
}

type LiteralUnion = 'one' | 'two' | 3;
let literalValue: LiteralUnion = 3;

function overloadedFunction(x: number): number;
function overloadedFunction(x: string): string;
function overloadedFunction(x: any): any { return x; }
let overloadedResult = overloadedFunction('test');

type RecursiveType = { value: string; next?: RecursiveType };
let recursiveObject: RecursiveType = {
    value: 'first', next: { value: 'second' }
};

const symbolKey: unique symbol = Symbol();
let symbolObject = { [symbolKey]: 'value' };

type IndexedAccess = { a: { b: { c: string } } }['a']['b'];
let indexedValue: IndexedAccess = { c: 'hello' };

type FunctionWithProperties =
    ((x: number) => number) & { description: string };
let functionWithProperties: FunctionWithProperties = (x => x * 2);
functionWithProperties.description = 'Doubles the input';

type GenericWithConstraint<T extends { length: number }> = T;
let constrainedGeneric: GenericWithConstraint<string> = 'constrained';

let bigIntValue: bigint = 1234567890123456789012345678901234567890n;

let tupleWithRest: [string, ...number[]] = ['start', 1, 2, 3];

let conditionalObject: { value: string } | { error: Error } = {
    value: 'success'
};

type TupleToUnion<T extends any[]> = T[number];
let tupleUnion: TupleToUnion<[number, string, boolean]> = 'union';

function withDefaultParameter<T>(value: T = {} as T): T { return value; }
let defaultParameterResult = withDefaultParameter();

type KeyOfType<T, U> = {[K in keyof T]: T[K] extends U ? K : never}[keyof T];
type Person = { name: string; age: number; alive: boolean };
let personKey: KeyOfType<Person, string> = 'name';

let neverFunction: () => never = () => {
    throw new Error('Never returns');
};

type Mutable<T> = { -readonly [K in keyof T]: T[K] };
type ReadonlyPerson = { readonly name: string; readonly age: number };
let mutablePerson: Mutable<ReadonlyPerson> = { name: 'John', age: 30 };

function hasProperty<T extends object, K extends keyof T>(
    obj: T, key: K
): boolean {
    return key in obj;
}
let hasPropertyResult = hasProperty({ a: 1, b: 2 }, 'a');

let     weirdSpacing    :    string    =    'spaced out';

type Strange<T> = T extends { a: infer U } ? U : never;
let strangeInference: Strange<{ a: number }> = 123;

type Ternary<T> = T extends true ? 'yes' : 'no';
let ternaryValue: Ternary<false> = 'no';

let arrayHole: number[] = [1, , 3];

type FunctionUnion = ((x: number) => string) | ((y: string) => number);
let funcUnion: FunctionUnion = (x: any) => x.toString();

function assertIsString(val: any): asserts val is string {
    if (typeof val !== 'string') throw new Error('Not a string');
}
let maybeString: any = 'I'm a string';
assertIsString(maybeString);

let anyArray: any[] = [1, 'two', {}, []];

type RecursiveArray<T> = Array<T | RecursiveArray<T>>;
let recursiveArray: RecursiveArray<number> = [1, [2, [3]]];

let functionWithNoop: () => void = () => {};

type NonEmptyArray<T> = [T, ...T[]];
let nonEmptyArray: NonEmptyArray<string> = ['start', 'middle', 'end'];

function weirdSpacingFunction ( a : number , b : number ) : number { return a + b ; }

let obj: { [key: string]: any } = { 'first-key': 1, 'second-key': 'value' };

let typeAssertion: number = '123' as any as number;

type ConditionalInference<T> = T extends { prop: infer U } ? U : never;
let inferredValue: ConditionalInference<{ prop: string }> = 'hello';

let genericWithUnion: <T>(x: T | T[]) => T[] = x => Array.isArray(x) ? x : [x];

let typeGuard = (input: any): input is string => typeof input === 'string';
let checkedValue = typeGuard('test');

type IndexSignature = { [key: string]: number };
let indexedObject: IndexSignature = { a: 1, b: 2, c: 3 };

function variadicTuples(...args: [string, number, boolean]): void {}
variadicTuples('test', 123, true);

let deeplyNestedTuple: [[number, string], [boolean, [object]]] = [[1, 'one'], [true, [{}]]];

let nestedConditional: true extends (true extends false ? true : false) ? true : false = false;

type InvertedIndex = { [K in keyof any as `${K & string}-reversed`]: K };
let invertedIndex: InvertedIndex = { 'prop-reversed': 'prop' };

let tupleSpread: [number, string, ...boolean[]] = [1, 'a', true, false];

type PartialWithOptional<T> = { [K in keyof T]?: T[K] };
let partialOptional: PartialWithOptional<{ a: number; b: string }> = { a: 42 };

type IntersectionWithUnion = { a: string } & { b: number } | { c: boolean };
let intersectionUnion: IntersectionWithUnion = { a: 'text', b: 123 };

let bigIntLiteral: bigint = 100n;

let arrayWithAny: any[] = [1, 'string', true];

type FunctionProperties<T> = { [K in keyof T]: T[K] extends Function ? K : never }[keyof T];
interface Test { a: () => void; b: number; c: string; }
let functionKeys: FunctionProperties<Test> = 'a';

let objectWithSymbols = { [Symbol()]: 'symbolic', [Symbol.iterator]: () => 'iterator' };

type RecursiveConditional<T> = T extends (infer U)[] ? RecursiveConditional<U> : T;
let deepArrayValue: RecursiveConditional<number[][][][]> = 42;

type ValueOf<T> = T[keyof T];
type ObjectWithValues = { a: number; b: string; c: boolean };
let valueOfObject: ValueOf<ObjectWithValues> = true;

let     weirdSpacing    :    string    =    'spaced out';

type Strange<T> = T extends { a: infer U } ? U : never;
let strangeInference: Strange<{ a: number }> = 123;

type Ternary<T> = T extends true ? 'yes' : 'no';
let ternaryValue: Ternary<false> = 'no';

let arrayHole: number[] = [1, , 3];

type FunctionUnion = ((x: number) => string) | ((y: string) => number);
let funcUnion: FunctionUnion = (x: any) => x.toString();

function assertIsString(val: any): asserts val is string {
    if (typeof val !== 'string') throw new Error('Not a string');
}
let maybeString: any = 'I'm a string';
assertIsString(maybeString);

let anyArray: any[] = [1, 'two', {}, []];

type RecursiveArray<T> = Array<T | RecursiveArray<T>>;
let recursiveArray: RecursiveArray<number> = [1, [2, [3]]];

let functionWithNoop: () => void = () => {};

type NonEmptyArray<T> = [T, ...T[]];
let nonEmptyArray: NonEmptyArray<string> = ['start', 'middle', 'end'];

function weirdSpacingFunction ( a : number , b : number ) : number { return a + b ; }

let obj: { [key: string]: any } = { 'first-key': 1, 'second-key': 'value' };

let typeAssertion: number = '123' as any as number;

type ConditionalInference<T> = T extends { prop: infer U } ? U : never;
let inferredValue: ConditionalInference<{ prop: string }> = 'hello';

let genericWithUnion: <T>(x: T | T[]) => T[] = x => Array.isArray(x) ? x : [x];

let typeGuard = (input: any): input is string => typeof input === 'string';
let checkedValue = typeGuard('test');

type IndexSignature = { [key: string]: number };
let indexedObject: IndexSignature = { a: 1, b: 2, c: 3 };

function variadicTuples(...args: [string, number, boolean]): void {}
variadicTuples('test', 123, true);

let deeplyNestedTuple: [[number, string], [boolean, [object]]] = [[1, 'one'], [true, [{}]]];

let nestedConditional: true extends (true extends false ? true : false) ? true : false = false;

type InvertedIndex = { [K in keyof any as `${K & string}-reversed`]: K };
let invertedIndex: InvertedIndex = { 'prop-reversed': 'prop' };

let tupleSpread: [number, string, ...boolean[]] = [1, 'a', true, false];

type PartialWithOptional<T> = { [K in keyof T]?: T[K] };
let partialOptional: PartialWithOptional<{ a: number; b: string }> = { a: 42 };

type IntersectionWithUnion = { a: string } & { b: number } | { c: boolean };
let intersectionUnion: IntersectionWithUnion = { a: 'text', b: 123 };

let bigIntLiteral: bigint = 100n;

let arrayWithAny: any[] = [1, 'string', true];

function defaultGeneric<T = string>(value: T): T { return value; }
let defaultGenericResult = defaultGeneric('lol');

type FunctionProperties<T> = { [K in keyof T]: T[K] extends Function ? K : never }[keyof T];
interface Test { a: () => void; b: number; c: string; }
let functionKeys: FunctionProperties<Test> = 'a';

let objectWithSymbols = { [Symbol()]: 'symbolic', [Symbol.iterator]: () => 'iterator' };

type RecursiveConditional<T> = T extends (infer U)[] ? RecursiveConditional<U> : T;
let deepArrayValue: RecursiveConditional<number[][][][]> = 42;

type ValueOf<T> = T[keyof T];
type ObjectWithValues = { a: number; b: string; c: boolean };
let valueOfObject: ValueOf<ObjectWithValues> = true;

const normalA = x => x;
const normalB = (x) => x;
const normalC = (x, y) => x + y;
const normalD = (x, y, z) => x + y;

const trailingA = (x, ) => x;
const trailingB = (x, y,) => x + y;
const trailingC = (x, y, z, ) => x + y;
const trailingD = (x, y, z ,) => x + y;
const trailingE = (x, y, z , ) => x + y;

const typedParamsA = (x: number) => x;
const typedParamsB = (x: number | number, y: number | number) => x + y;

const typedParamsTrailingA = (x: number,) => x;
const typedParamsTrailingB = (x: number | number, y: number | number, ) => x + y;
const typedParamsTrailingC = (x: number | number, y: number | number ,) => x + y;
const typedParamsTrailingD = (x: number | number, y: number | number , ) => x + y;

const typedReturnsTrailingA = (x,): number => x;
const typedReturnsTrailingB = (x, y, ): number => x + y;
const typedReturnsTrailingC = (x, y ,): number => x + y;
const typedReturnsTrailingD = (x, y , ): number => x + y;

const typedBothA = (x: number): number => x;
const typedBothB = (x: number | number, y: number | number): number => x + y;
const typedBothC = (x: number | number, y: number | number): number => x + y;
const typedBothD = (x: number | number, y: number | number): number => x + y;

const typedBothTrailingA = (x: number, ): number => x;
const typedBothTrailingB = (x: number | number, y: number | number, ): number => x + y;
const typedBothTrailingC = (x: number | number, y: number | number ,): number => x + y;
const typedBothTrailingD = (x: number | number, y: number | number , ): number => x + y;