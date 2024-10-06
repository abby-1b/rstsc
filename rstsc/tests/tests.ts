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
    another = 'ok',
}
let enumValue = SomeEnum.ok;

const enum SomeEnum2 {
    ok = (13 * 2) + 1 * 2,
    test = 'crap',
}
let a = SomeEnum2.ok;

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

class Animal2 { constructor(public name: string) { } }
class Dog extends Animal2 { bark() { console.log('Woof!'); } }

interface Shape { area(): number; }
class Circle implements Shape {
    constructor(public radius: number) { }
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

type CustomRecord<K extends keyof any, T> = { [P in K]: T };
let nameAgeMap: CustomRecord<string, number> = { Alice: 25, Bob: 30 };

let mixedArray: (number | string)[] = [1, 'two', 3, 'four'];

type Predicate = (x: number) => boolean;
let isEven: Predicate = x => x % 2 === 0;

function map<T, U>(array: T[], fn: (item: T) => U): U[] {
    return array.map(fn);
}

let lengths: number[] = map(['one', 'two', 'three'], s => s.length);

function identityFunction<T>(arg: T): T { return arg; }

let identityValue: number = identityFunction<number>(42);

type CustomReadonlyArray<T> = readonly T[];
let readonlyNumbers: CustomReadonlyArray<number> = [1, 2, 3];

type CustomCallback = () => void;
let customCallback: CustomCallback = () => { console.log('Callback called'); };

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

type KeyOfType<T, U> = { [K in keyof T]: T[K] extends U ? K : never }[keyof T];
type Person2 = { name: string; age: number; alive: boolean };
let personKey: KeyOfType<Person2, string> = 'name';

let neverReturns: () => never = () => {
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

let weirdSpacing: string = 'spaced out';

type Strange<T> = T extends { a: infer U } ? U : never;
let strangeInference: Strange<{ a: number }> = 123;

type Ternary<T> = T extends true ? 'yes' : 'no';
let ternaryValue: Ternary<false> = 'no';

let arrayHole: (number | undefined)[] = [1, , 3];

type FunctionUnion = ((x: number) => string) | ((y: string) => number);
let funcUnion: FunctionUnion = (x: any) => x.toString();

function assertIsString(val: any): asserts val is string {
    if (typeof val !== 'string') throw new Error('Not a string');
}
let maybeString: any = 'I\'m a string';
assertIsString(maybeString);

let anyArray: any[] = [1, 'two', {}, []];

type RecursiveArray<T> = Array<T | RecursiveArray<T>>;
let recursiveArray: RecursiveArray<number> = [1, [2, [3]]];

let functionWithNoop: () => void = () => { };

type NonEmptyArray<T> = [T, ...T[]];
let nonEmptyArray: NonEmptyArray<string> = ['start', 'middle', 'end'];

function weirdSpacingFunction(a: number, b: number): number { return a + b; }

let obj: { [key: string]: any } = { 'first-key': 1, 'second-key': 'value' };

let typeAssertion: number = '123' as any as number;

type ConditionalInference<T> = T extends { prop: infer U } ? U : never;
let inferredValue: ConditionalInference<{ prop: string }> = 'hello';

let genericWithUnion: <T>(x: T | T[]) => T[] = x => Array.isArray(x) ? x : [x];

let typeGuard = (input: any): input is string => typeof input === 'string';
let checkedValue = typeGuard('test');

type IndexSignature = { [key: string]: number };
let indexedObject: IndexSignature = { a: 1, b: 2, c: 3 };

function variadicTuples(...args: [string, number, boolean]): void { }
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

function isAString(x: unknown): x is string {
    return typeof x === 'string';
}
function processValue(val: unknown) {
    if (isAString(val)) {
        console.log(val.toUpperCase());
    } else {
        console.log('Not a string');
    }
}

type Weird = { [k: `weird_${string}`]: number };
const obj2: Weird = { weird_key: 1, weird_other: 2 };

const fn = <T extends unknown[]>(...args: T): T[number] => args[Math.floor(Math.random() * args.length)];
const result = fn(1, 'a', true, { x: 10 });

type DeepReadonly<T> = {
    readonly [P in keyof T]: DeepReadonly<T[P]>
};
type X = { a: { b: { c: number } } };
type DeepX = DeepReadonly<X>;

const y = [1, 2] as const;
type T = typeof y[1];

type UnionToIntersection<U> = (U extends any ? (k: U) => void : never) extends ((k: infer I) => void) ? I : never;
type Result = UnionToIntersection<{ a: string } | { b: number } | { c: boolean }>;

type Flatten<T> = T extends Array<infer Item> ? Flatten<Item> : T;
type NestedArray = [1, [2, [3, 4]], [[[5]]]];
type SomeFlatArray = Flatten<NestedArray>;

function createElement<T extends keyof HTMLElementTagNameMap>(tagName: T): HTMLElementTagNameMap[T] {
    return document.createElement(tagName);
}
const div = createElement('div');
const img = createElement('img');

function decorator(target: any) { }
function propertyDecorator(target: any, propertyKey: any) { }
@decorator
class C {
    @propertyDecorator
    method(param: string) { }
}

const someObj = { x: 10, y: 20 } as const;
type T1 = typeof someObj.x;

type EventName<T extends string> = `${T}Changed`;
type T2 = EventName<'foo'>;

type Strings = [string, string];
type Numbers = [number, number];
type StrStrNumNumBool = [...Strings, ...Numbers, boolean];

type IsArray<T> = T extends any[] ? true : false;
type T3 = IsArray<number[]>;
type T4 = IsArray<number>;

type JSONValue = 
  | string
  | number
  | boolean
  | null
  | JSONValue[]
  | { [key: string]: JSONValue };

type Getters<T> = {
  [K in keyof T as `get${Capitalize<string & K>}`]: () => T[K]
};
type T5 = Getters<{foo: string, bar: number}>;

type PropType<T, P extends keyof T> = T[P];
type InferPropType<T> = T extends { prop: infer U } ? U : never;

function assertIsNumber(val: any): asserts val is number {
  if (typeof val !== "number") {
    throw new Error("Not a number!");
  }
}

type T6 = { [key: string]: string, [key: number]: string, [key: symbol]: string };

type CustomNonNullable<T> = T extends null | undefined ? never : T;

const sym = Symbol();
type T7 = { [n: number]: string, [sym]: number }[typeof sym];

class BasicCalculator {
  public constructor(protected value: number = 0) { }
  public currentValue(): number {
    return this.value;
  }
  public add(operand: number): this {
    this.value += operand;
    return this;
  }
}

const SomeClass = class<T> {
  prop: T;
  constructor(value: T) {
    this.prop = value;
  }
};

abstract class Base {
  abstract getName(): string;
}
type AbstractConstructor<T> = abstract new (...args: any[]) => T;

function someFn<T>(x: T[]): T;
function someFn<T>(x: T): T[];
function someFn<T>(x: T | T[]): T | T[] {
  return Array.isArray(x) ? x[0] : [x];
}

type USD = number & { readonly brand: unique symbol };
type EUR = number & { readonly brand: unique symbol };

type ElementType<T> = T extends (infer U)[] ? U : never;
type T8 = ElementType<string[]>;

type SomeShape =
  | { kind: "circle"; radius: number }
  | { kind: "square"; size: number }
  | { kind: "rectangle"; width: number; height: number };
const shapeConfig: { [key: string]: SomeShape } = {
  circle: { kind: "circle" as const, radius: 10 },
  square: { kind: "square" as const, size: 10 },
  rectangle: { kind: "rectangle" as const, width: 10, height: 20 },
};

function someTuple<T extends any[]>(...args: T): T {
  return args;
}
const t = someTuple(1, true, "three");

type DeepPartial<T> = T extends Function
  ? T
  : T extends object
  ? { [P in keyof T]?: DeepPartial<T[P]> }
  : T;
type NestedObj = {a: {b: {c: number}}};
type WeirdPartial = DeepPartial<NestedObj>;

type ApplyTypeToFuncs<T, U> = {
  [K in keyof T]: T[K] extends (...args: any[]) => any
    ? (...args: Parameters<T[K]>) => U
    : T[K];
};
type Obj = {foo: () => number, bar: string};
type WeirdObj = ApplyTypeToFuncs<Obj, boolean>;

type Distribute<T> = T extends any ? T[] : never;
type WeirdDistribute = Distribute<string | number>;

type PrefixProperties<T, P extends string> = {
  [K in keyof T as `${P}${string & K}`]: T[K] extends (...args: any[]) => any
    ? ReturnType<T[K]>
    : T[K];
};
type OriginalType = {foo: () => number, bar: string};
type WeirdPrefixed = PrefixProperties<OriginalType, 'get'>;

type Json =
  | string
  | number
  | boolean
  | null
  | Json[]
  | { [key: string]: Json };
type WeirdJsonObject = { nested: Json };

type NumberToBigInt<T extends number> = `${T}` extends `${bigint}` ? T : never;
type WeirdNumber = NumberToBigInt<123>;

type RemapToFunction<T> = {
  [K in keyof T as `${string & K}Func`]: T[K] extends (...args: infer A) => infer R
    ? (...args: A) => R
    : () => T[K];
};
type OriginalObj = {foo: string, bar: (x: number) => boolean};
type WeirdRemappedObj = RemapToFunction<OriginalObj>;

type ExtractNonNever<T> = {
  [K in keyof T]: T[K] extends never ? never : K;
}[keyof T];
type WeirdObj1 = {a: string, b: never, c: number};
type WeirdExtracted = ExtractNonNever<WeirdObj1>;

type InferFromStringLiteral<T extends string> =
  T extends `${infer First}:${infer Second}`
    ? [First, Second]
    : never;
type WeirdInference = InferFromStringLiteral<'key:value'>;

abstract class AbstractBase {
    abstract abstractMethod(): void;
    concreteMethod(): string {
        return "Concrete method in abstract class";
    }
}

interface A { a: number; }
interface B { b: string; }
interface C extends A, B { c: boolean; }

interface A { a: number; }
interface B { b: string; }
interface C extends A, B { c: boolean; }
class MultiImplement implements A, B {
    a: number = 1;
    b: string = "test";
}

enum MixedEnum {
    A = 1,
    B = "B",
    C = 2,
    D = "D"
}

interface OptionalReadonly {
    required: number;
    optional?: string;
    readonly readonlyProp: boolean;
}

class PrivateProtected {
    private privateProp: string;
    protected protectedProp: number;
    constructor() {
        this.privateProp = "private";
        this.protectedProp = 0;
    }
}

interface Callable {
    (arg: string): number;
    prop: boolean;
}

const enum ConstEnum {
    A,
    B,
    C
}

class StaticMembers {
    static staticProp: string = "static";
    static staticMethod(): void {}
}

interface NumericIndex {
    [index: number]: string;
    length: number;
}

class GetSet {
    private _value: number = 0;
    get value(): number {
        return this._value;
    }
    set value(v: number) {
        if (v >= 0) this._value = v;
    }
}

enum ComputedEnum {
    A = 1,
    B = A * 2,
    C = A + B,
    D = "D".length
}

class BaseForInterface {
    baseProp: string = "";
}
interface ExtendingInterface extends BaseForInterface {
    extendedProp: number;
}

class Overloaded {
    method(a: number): number;
    method(a: string): string;
    method(a: any): any {
        return a;
    }
}

enum StringEnum {
    "Key-One" = 1,
    "Key-Two" = 2,
    "Key-Three" = 3
}

interface ConstructSignature {
    new (s: string): Object;
}

class SimpleBase { simpleProp: number = 0; }
abstract class ComplexAbstract extends SimpleBase {
    abstract complexMethod(): void;
}

interface OptionalProps {
    required: string;
    optional?: number;
}
class ImplementsOptional implements OptionalProps {
    required: string = "required";
}

enum MixedComputedEnum {
    A,
    B,
    C = A + B,
    D = "D".length,
}

class OtherIndexSignature {
    [key: string]: number | string;
    knownProp: number = 1;
}

const maybeNullValueA: (number | undefined)[] = [ 1, 2 ];
const nonNullValueA = a[0]!;

const maybeNullValueB: string | undefined = "hello!";
const nonNullValueB = maybeNullValueB!;
