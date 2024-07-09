mod common;
use common::test_multiple;

#[test]
fn other_tests() {
    test_multiple(&[
"
type A = { some: number } | { lol: string } & { other: boolean }
type B = { some: number } & { lol: string } | { other: boolean }
const a: A = { some: 123 };
const b: A = { lol: 'ok', other: true };
const c: B = { some: 123, lol: 'ok' };
const d: string | number & boolean = 'hello!';
",
"
type SOME = {
    someVoid: void,
    someUndf: undefined
};
const a: SOME['someVoid'] = void 0;
const b: SOME['someVoid'] = undefined;
const c: SOME['someUndf'] = void 0;
const d: SOME['someUndf'] = undefined;
"
    ], common::WhiteSpace::IgnoreAll);
}
    