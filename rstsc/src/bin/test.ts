
// let obj = {
//     a: 123,
//     b: 'nice',
//     c: false
// };
// console.log(obj);
// let fn = () => 123;

// function test(x: number | string = 0): number {
//     const a = 2 * 3 + 1 * (((2 * 2 - 4 + 28) - 1) + 4);
//     if (x + '' === x) {
//         return parseFloat(x);
//     } else {
//         return x as number;
//     }
// }

// function weirdOperation() {
//     return (
//         (((10 + 21) - 30) * 40)
//         + (function() {
//             return (
//                 (((40 - 50) + 10) * 20) -
//                 (function() {
//                     return (
//                         (((20 + 30) - 40) * 50) +
//                         (function() {
//                             return (((30 - 10) + 20) * 30) - (((40 - 50) + 10) * 20);
//                         })()
//                     );
//                 })()
//             );
//         })()
//     );
// }

// console.log(weirdOperation());

// function lol() {
//     let a = 123;
//     return a;
// }

// function log(message: string, userId?: string) {
//     console.log(message);
// }

// type Some = 123;
// function isString(x: any): x is string {
//     return typeof x === 'string';
// }

// let test1: () => void, test2: (x: number) => void, test3: (string | number)[], test4: (x: number, y: number) => void;

// function doLog(message: any, ...optionalParams: any[]) {
//     console.log(message, ...optionalParams);
// }
// doLog('Message', 1, 2, 3);

// function assertIsString(val: any): asserts val is string {
//     if (typeof val !== 'string') throw new Error('Not a string');
// }
// let maybeString: any = 'I\'m a string';
// assertIsString(maybeString);


// interface TestFn {
//     (x: number, y: number);
// }

// let a: TestFn = (x: number) => x;

// interface TestProps {
//     a: number;
//     b: string,
//     c: number | string
//     d: TestFn
//     e
// }


// type A = { some: number } | { lol: string } & { other: boolean }
// type B = { some: number } & { lol: string } | { other: boolean }
// const a: A = { some: 123 };
// const b: A = { lol: 'ok', other: true };
// const c: B = { some: 123, lol: 'ok' };
// const d: string | number & boolean = 'hello!';

// type SOME = {
//     someVoid: void,
//     someUndf: undefined
// };
// const a: SOME['someVoid'] = void 0;
// const b: SOME['someVoid'] = undefined;
// const c: SOME['someUndf'] = void 0;
// const d: SOME['someUndf'] = undefined;

// function assertIsString(val: any): asserts val is string {
//     if (typeof val !== 'string') throw new Error('Not a string');
// }
// let maybeString: any = 'I\'m a string';
// assertIsString(maybeString);

// type A = number;
// type B = string;
// let a: () => number;

// type RecursiveConditional<T> = T extends (infer U)[] ? RecursiveConditional<U> : T;
// let deepArrayValue: RecursiveConditional<number[][][][]> = 42;

// type ValueOf<T> = T[keyof T];
// type ObjectWithValues = { a: number; b: string; c: boolean };
// let valueOfObject: ValueOf<ObjectWithValues> = true;


let some: true extends true ? true : false;
// let nestedConditional: true extends (true extends false ? true : false) ? true : false = false;
