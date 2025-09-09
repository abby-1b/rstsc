
type PartialWithOptional<T> = { [K in keyof T]?: T[K] };
let partialOptional: PartialWithOptional<{ a: number; b: string }> = { a: 42 };

type DeepPartial<T> = T extends Function
  ? T
  : T extends object
  ? { [P in keyof T]?: DeepPartial<T[P]> }
  : T;
type NestedObj = {a: {b: {c: number}}};
type WeirdPartial = DeepPartial<NestedObj>;

// export class Tab<T extends Record<string, any>> {
//   public horizontalNumbers<N extends string, K extends [...string[]]>(
//     label: N, labels: K, hideLabel = true
//   ): Tab<T & Record<N, { [J in keyof K]: number }>> {
//     return this;
//   }
// }

// const COMPOST_TYPES: Record<string, number> = {
//   'Composta de madera': 1010,
//   'Composta de gallinaza': 1333,
//   'Composta de setas': 1333,
//   'Composta de pulpa de café': 626,
//   'Composta de estiercol de caballo': 866,
//   'Composta de estiércol de vaca': 1131,
//   'Biosólidos compostados': 1131,
// };

// const maybeNullValueA: (number | undefined)[] = [ 1, 2 ];
// const nonNullValueA = a[0]!;

// interface Test {
//   area(): number
// }

// let fn = () => 123;

// function some<number 123() {
  
// }

// const ĸ = ""
// console.log(ĸ)
// const b = 123;

// class GetSet {
//   private _value: number = 0;
//   get value(): number {
//     return this._value;
//   }
//   set value(v: number) {
//     if (v >= 0) this._value = v;
//   }
// }

// let convertToArray: <T>(x: T | T[]) => T[] = x => Array.isArray(x) ? x : [x];
// const a = `number ${ 'a' + `fuck my ${2} life` }` as const;

// const enum MixedComputedEnum {
//   A,
//   B,
//   C,
//   D = "D".length,
//   E = "nice"
// }
// let b = 123;
// const a = MixedComputedEnum["nice"];

// function weirdOperation() {
//   return (
//     (((10 + 21) - 30) * 40)
//     + (function() {
//       return (
//         (((40 - 50) + 10) * 20) -
//         (function() {
//           return (
//             (((20 + 30) - 40) * 50) +
//             (function() {
//               return (((30 - 10) + 20) * 30) - (((40 - 50) + 10) * 20);
//             })()
//           );
//         })()
//       );
//     })()
//   );
// }

// function weirdOperation() {
//   return (function() {
//     return 1;
//   })();
// }
