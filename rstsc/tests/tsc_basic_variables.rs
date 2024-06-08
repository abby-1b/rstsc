mod common;
use common::test_multiple;

#[test]
fn run_all_tests() {
    test_multiple(&[

"
let x: number = 10;
",
"
let pair: [number, string] = [1, 'one'];
",
"
let tuple: [number, string, boolean] = [1, 'two', true];
",
"
let id: string | number = 123;
",
"
let numbers: number[] = [1, 2, 3];
",
"
type User = { id: number; name: string; };
let user: User = { id: 1, name: 'Alice' };
",
"
let nameAgeMap: Record<string, number> = { Alice: 25, Bob: 30 };
",
"
let mixedArray: (number | string)[] = [1, 'two', 3, 'four'];
",
"
let doubleNestedArray: number[][] = [[1, 2], [3, 4]];
",
"
let deeplyNestedObject: { a: { b: { c: number } } } = {
    a: { b: { c: 42 } }
};
",
"
let neverArray: never[] = [];
",
"
let bigIntValue: bigint = 1234567890123456789012345678901234567890n;
",
"
let tupleWithRest: [string, ...number[]] = ['start', 1, 2, 3];
",
"
let     weirdSpacing    :    string    =    'spaced out';
",
"
let arrayHole: number[] = [1, , 3];
",
"
let anyArray: any[] = [1, 'two', {}, []];
",
"
let obj: { [key: string]: any } = { 'first-key': 1, 'second-key': 'value' };
",
"
let deeplyNestedTuple: [[number, string], [boolean, [object]]] = [[1, 'one'], [true, [{}]]];
",
"
let arrayWithAny: any[] = [1, 'string', true];
",
"
let objectWithSymbols = { [Symbol()]: 'symbolic', [Symbol.iterator]: () => 'iterator' };
",

    ], common::WhiteSpace::IgnoreAll);
}
