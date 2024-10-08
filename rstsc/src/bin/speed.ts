let obj = {
    a: 123,
    b: 'nice',
    c: false
};
console.log(obj);
let fn = () => 123;

function test(x: number | string = 0): number {
    const a = 2 * 3 + 1 * (((2 * 2 - 4 + 28) - 1) + 4);
    if (x + '' === x) {
        return parseFloat(x);
    } else {
        return x as number;
    }
}

function weirdOperation() {
    return (
        (((10 + 21) - 30) * 40)
        + (function() {
            return (
                (((40 - 50) + 10) * 20) -
                (function() {
                    return (
                        (((20 + 30) - 40) * 50) +
                        (function() {
                            return (((30 - 10) + 20) * 30) - (((40 - 50) + 10) * 20);
                        })()
                    );
                })()
            );
        })()
    );
}

console.log(weirdOperation());

function lol() {
    let a = 123;
    return a;
}

function log(message: string, userId?: string) {
    console.log(message);
}

type Some = 123;
function isString(x: any): x is string {
    return typeof x === 'string';
}

let test1: () => void, test2: (x: number) => void, test3: (string | number)[], test4: (x: number, y: number) => void;

function doLog(message: any, ...optionalParams: any[]) {
    console.log(message, ...optionalParams);
}
doLog('Message', 1, 2, 3);

interface TestFn {
    (x: number, y: number);
    (x: number, y: number, z: string);
    something: number
}

const a: {
    (x: number, y: number)
    lmao: 123
} = () => 123;

const m = (a: number, b: number) => 123;
m.something = 456;
const b: TestFn = m;

interface TestProps {
    a: number;
    b: string,
    c: number | string
    d: TestFn
    e
}

type C = { some: number } | { lol: string } & { other: boolean }
type D = { some: number } & { lol: string } | { other: boolean }
const c: C = { some: 123 };
const d: C = { lol: 'ok', other: true };
const e: D = { some: 123, lol: 'ok' };
const f: string | number & boolean = 'hello!';

type SOME = {
    someVoid: void,
    someUndf: undefined
};
const g: SOME['someVoid'] = void 0;
const h: SOME['someVoid'] = undefined;
const i: SOME['someUndf'] = void 0;
const j: SOME['someUndf'] = undefined;

function assertIsString(val: any): asserts val is string {
    if (typeof val !== 'string') throw new Error('Not a string');
}
let maybeString: any = 'I\'m a string';
assertIsString(maybeString);

type A = number;
type B = string;
let k: () => number;

type RecursiveConditional<T> = T extends (infer U)[] ? RecursiveConditional<U> : T;
let deepArrayValue: RecursiveConditional<number[][][][]> = 42;

type ValueOf<T> = T[keyof T];
type ObjectWithValues = { a: number; b: string; c: boolean };
let valueOfObject: ValueOf<ObjectWithValues> = true;

let some: true extends true ? true : false;
let nestedConditional: true extends (true extends false ? true : false) ? true : false = false;

type Callback = () => void;
let callback: Callback = () => { console.log('Callback called'); }

let neverFunction: () => never = () => { throw new Error('Never returns'); };

let l: { [key: string]: number } = { 'hey': 123, 'there': 456 };

function identityFunction<T>(a: T): T { return a; }
let identityValue: number = identityFunction<number>(42);

let genericWithUnion: <T>(x: T | T[]) => T[] = x => Array.isArray(x) ? x : [x];

type Point = { x: number; y: number; };
function logPoint(p: Point): void { console.log(`x: ${p.x}, y: ${p.y}`); }
type PartialPoint = Partial<Point>;
let partialPoint: PartialPoint = { x: 10 };
type RequiredPoint = Required<PartialPoint>;
let requiredPoint: RequiredPoint = { x: 10, y: 20 };

interface Shape { area(): number; }
interface Another {}
class Circle implements Shape {
  constructor(public radius: number) {}
  area(): number { return Math.PI * this.radius * this.radius; }
  lmao(some?: number) { return some; }
}

type ElementType<T> = T extends (infer U)[] ? U : never;
type T8 = ElementType<string[]>;

const sym = Symbol();
type T7 = { [n: number]: string, [sym]: number }[typeof sym];
