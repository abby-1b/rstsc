
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

function isString(x: any): x is string {
    return typeof x === 'string';
}
