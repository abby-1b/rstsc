class TestB {
  constructor(private a = 32) {}
}

const b = new TestB();
b.a = 456;

// // Test basic namespace
// namespace Geometry {
//   export function area(radius: number): number {
//     return Math.PI * radius * radius;
//   }

//   export const PI = 3.14159;
// }

// // Test nested namespace
// namespace Outer {
//   export namespace Inner {
//     export const value = 42;
//   }
// }

// // Test export namespace
// export namespace Utils {
//   export function log(message: string): void {
//     console.log(message);
//   }
// }

// // Test declare namespace (should be preserved)
// declare namespace Declared {
//   export interface Point {
//     x: number;
//     y: number;
//   }
// }

// // Test usage
// const circleArea = Geometry.area(5);
// const piValue = Geometry.PI;
// const innerValue = Outer.Inner.value;
