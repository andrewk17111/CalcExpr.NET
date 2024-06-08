﻿using DiceEngine.Expressions;
using DiceEngine.Expressions.Collections;
using TestDiceEngine.TestUtils;

namespace TestDiceEngine.TestData;

public static partial class TestCases
{
    public static TestCase[] Collections =
    [
        new TestCase("[ 1,  2, 3  , 4, 5]  ", UtilFunctions.Range<Vector>(1, 5)),
        new TestCase("[ 1*1,  2*2, 3 *3  , 4*4, 5*5]  ",
            new Vector(Enumerable.Range(1, 5).Select(x => new BinaryOperator("*", (Number)x, (Number)x))),
            new Vector(Enumerable.Range(1, 5).Select(x => (Number)(x * x))),
            Enumerable.Range(1, 4).Select(i => (IExpression)new Vector(
                Enumerable.Range(1, i).Select(x => (IExpression)(Number)(x * x))
                    .Union(Enumerable.Range(i + 1, 5 - i)
                        .Select(x => new BinaryOperator("*", (Number)x, (Number)x))))).ToArray()),
        new TestCase("+[1,2,3,4,5]", new UnaryOperator("+", true, UtilFunctions.Range<Vector>(1, 5)),
            new Vector(Enumerable.Range(1, 5).Select(x => (Number)(+x)))),
        new TestCase("-[1,2,3,4,5]", new UnaryOperator("-", true, UtilFunctions.Range<Vector>(1, 5)),
            new Vector(Enumerable.Range(1, 5).Select(x => (Number)(-x)))),
        new TestCase("~[1,2,3,4,5]", new UnaryOperator("~", true, UtilFunctions.Range<Vector>(1, 5)),
            new Vector(Enumerable.Range(1, 5).Select(x => new UnaryOperator("~", true, (Number)x).Evaluate()))),
        new TestCase("!{1,2,3,4,5}", new UnaryOperator("!", true, UtilFunctions.Range<Set>(1, 5)),
            new Set(Enumerable.Range(1, 5).Select(x => new UnaryOperator("!", true, (Number)x).Evaluate()))),
        new TestCase("--[1,2,3,4,5]", new UnaryOperator("--", true, UtilFunctions.Range<Vector>(1, 5)),
            new Vector(Enumerable.Range(1, 5).Select(x => new UnaryOperator("--", true, (Number)x).Evaluate()))),
        new TestCase("++[1,2,3,4,5]", new UnaryOperator("++", true, UtilFunctions.Range<Vector>(1, 5)),
            new Vector(Enumerable.Range(1, 5).Select(x => new UnaryOperator("++", true, (Number)x).Evaluate()))),
        new TestCase("[1,2,3,4,5]!", new UnaryOperator("!", false, UtilFunctions.Range<Vector>(1, 5)),
            new Vector(Enumerable.Range(1, 5).Select(x => new UnaryOperator("!", false, (Number)x).Evaluate()))),
        new TestCase("[1,2,3,4,5]%", new UnaryOperator("%", false, UtilFunctions.Range<Vector>(1, 5)),
            new Vector(Enumerable.Range(1, 5).Select(x => new UnaryOperator("%", false, (Number)x).Evaluate()))),
        new TestCase("[1,2,3,4,5]!!", new UnaryOperator("!!", false, UtilFunctions.Range<Vector>(1, 5)),
            new Vector(Enumerable.Range(1, 5).Select(x => new UnaryOperator("!!", false, (Number)x).Evaluate()))),
        new TestCase("[1,2,3,4,5]#", new UnaryOperator("#", false, UtilFunctions.Range<Vector>(1, 5)),
            new Vector(Enumerable.Range(1, 5).Select(x => new UnaryOperator("#", false, (Number)x).Evaluate()))),
        new TestCase("[1,2,3,4,5]--", new UnaryOperator("--", false, UtilFunctions.Range<Vector>(1, 5)),
            new Vector(Enumerable.Range(1, 5).Select(x => new UnaryOperator("--", false, (Number)x).Evaluate()))),
        new TestCase("{1,2,3,4,5}++", new UnaryOperator("++", false, UtilFunctions.Range<Set>(1, 5)),
            new Set(Enumerable.Range(1, 5).Select(x => new UnaryOperator("++", false, (Number)x).Evaluate()))),
        new TestCase("[1,2,3]+[7,8,9]",
            new BinaryOperator("+", UtilFunctions.Range<Vector>(1, 3), UtilFunctions.Range<Vector>(7, 3)),
            new Vector(((int[])[1, 2, 3]).Select(x => new BinaryOperator("+", (Number)x, (Number)(x + 6)).Evaluate()))),
        new TestCase("[1,2,3]-[7,8,9]",
            new BinaryOperator("-", UtilFunctions.Range<Vector>(1, 3), UtilFunctions.Range<Vector>(7, 3)),
            new Vector(((int[])[1, 2, 3]).Select(x => new BinaryOperator("-", (Number)x, (Number)(x + 6)).Evaluate()))),
        new TestCase("[1,2,3]*[7,8,9]",
            new BinaryOperator("*", UtilFunctions.Range<Vector>(1, 3), UtilFunctions.Range<Vector>(7, 3)),
            new Vector(((int[])[1, 2, 3]).Select(x => new BinaryOperator("*", (Number)x, (Number)(x + 6)).Evaluate()))),
        new TestCase("[1,2,3]×[7,8,9]",
            new BinaryOperator("×", UtilFunctions.Range<Vector>(1, 3), UtilFunctions.Range<Vector>(7, 3)),
            new Vector(((int[])[1, 2, 3]).Select(x => new BinaryOperator("×", (Number)x, (Number)(x + 6)).Evaluate()))),
        new TestCase("[1,2,3]/[7,8,9]",
            new BinaryOperator("/", UtilFunctions.Range<Vector>(1, 3), UtilFunctions.Range<Vector>(7, 3)),
            new Vector(((int[])[1, 2, 3]).Select(x => new BinaryOperator("/", (Number)x, (Number)(x + 6)).Evaluate()))),
        new TestCase("[1,2,3]÷[7,8,9]",
            new BinaryOperator("÷", UtilFunctions.Range<Vector>(1, 3), UtilFunctions.Range<Vector>(7, 3)),
            new Vector(((int[])[1, 2, 3]).Select(x => new BinaryOperator("÷", (Number)x, (Number)(x + 6)).Evaluate()))),
        new TestCase("[1,2,3]^[7,8,9]",
            new BinaryOperator("^", UtilFunctions.Range<Vector>(1, 3), UtilFunctions.Range<Vector>(7, 3)),
            new Vector(((int[])[1, 2, 3]).Select(x => new BinaryOperator("^", (Number)x, (Number)(x + 6)).Evaluate()))),
        new TestCase("[1,2,3]%[7,8,9]",
            new BinaryOperator("%", UtilFunctions.Range<Vector>(1, 3), UtilFunctions.Range<Vector>(7, 3)),
            new Vector(((int[])[1, 2, 3]).Select(x => new BinaryOperator("%", (Number)x, (Number)(x + 6)).Evaluate()))),
        new TestCase("[1,2,3]%%[7,8,9]",
            new BinaryOperator("%%", UtilFunctions.Range<Vector>(1, 3), UtilFunctions.Range<Vector>(7, 3)),
            new Vector(((int[])[1, 2, 3]).Select(x => new BinaryOperator("%%", (Number)x, (Number)(x + 6))
                .Evaluate()))),
        new TestCase("[1,2,3]//[7,8,9]",
            new BinaryOperator("//", UtilFunctions.Range<Vector>(1, 3), UtilFunctions.Range<Vector>(7, 3)),
            new Vector(((int[])[1, 2, 3]).Select(x => new BinaryOperator("//", (Number)x, (Number)(x + 6))
                .Evaluate()))),
        new TestCase("[1,2,3]&&[7,8,9]",
            new BinaryOperator("&&", UtilFunctions.Range<Vector>(1, 3), UtilFunctions.Range<Vector>(7, 3)),
            new Vector(((int[])[1, 2, 3]).Select(x => new BinaryOperator("&&", (Number)x, (Number)(x + 6))
                .Evaluate()))),
        new TestCase("[1,2,3]∧[7,8,9]",
            new BinaryOperator("∧", UtilFunctions.Range<Vector>(1, 3), UtilFunctions.Range<Vector>(7, 3)),
            new Vector(((int[])[1, 2, 3]).Select(x => new BinaryOperator("∧", (Number)x, (Number)(x + 6))
                .Evaluate()))),
        new TestCase("[1,2,3]||[7,8,9]",
            new BinaryOperator("||", UtilFunctions.Range<Vector>(1, 3), UtilFunctions.Range<Vector>(7, 3)),
            new Vector(((int[])[1, 2, 3]).Select(x => new BinaryOperator("||", (Number)x, (Number)(x + 6))
                .Evaluate()))),
        new TestCase("[1,2,3]∨[7,8,9]",
            new BinaryOperator("∨", UtilFunctions.Range<Vector>(1, 3), UtilFunctions.Range<Vector>(7, 3)),
            new Vector(((int[])[1, 2, 3]).Select(x => new BinaryOperator("∨", (Number)x, (Number)(x + 6))
                .Evaluate()))),
        new TestCase("[1,2,3]⊕[7,8,9]",
            new BinaryOperator("⊕", UtilFunctions.Range<Vector>(1, 3), UtilFunctions.Range<Vector>(7, 3)),
            new Vector(((int[])[1, 2, 3]).Select(x => new BinaryOperator("⊕", (Number)x, (Number)(x + 6)).Evaluate()))),
        new TestCase("[1,2,3]==[1,2,3]",
            new BinaryOperator("==", UtilFunctions.Range<Vector>(1, 3), UtilFunctions.Range<Vector>(1, 3)),
            (Number)1),
        new TestCase("[1,2,3]==[7,8,9]",
            new BinaryOperator("==", UtilFunctions.Range<Vector>(1, 3), UtilFunctions.Range<Vector>(7, 3)),
            (Number)0),
        new TestCase("[1,2,3]!=[7,8,9]",
            new BinaryOperator("!=", UtilFunctions.Range<Vector>(1, 3), UtilFunctions.Range<Vector>(7, 3)),
            (Number)1),
        new TestCase("[1,2,3]<[7,8,9]",
            new BinaryOperator("<", UtilFunctions.Range<Vector>(1, 3), UtilFunctions.Range<Vector>(7, 3)),
            new Vector(((int[])[1, 2, 3]).Select(x => new BinaryOperator("<", (Number)x, (Number)(x + 6)).Evaluate()))),
        new TestCase("[1,2,3]<=[7,8,9]",
            new BinaryOperator("<=", UtilFunctions.Range<Vector>(1, 3), UtilFunctions.Range<Vector>(7, 3)),
            new Vector(((int[])[1, 2, 3]).Select(x => new BinaryOperator("<=", (Number)x, (Number)(x + 6))
                .Evaluate()))),
        new TestCase("[1,2,3]>[7,8,9]",
            new BinaryOperator(">", UtilFunctions.Range<Vector>(1, 3), UtilFunctions.Range<Vector>(7, 3)),
            new Vector(((int[])[1, 2, 3]).Select(x => new BinaryOperator(">", (Number)x, (Number)(x + 6)).Evaluate()))),
        new TestCase("[1,2,3]>=[7,8,9]",
            new BinaryOperator(">=", UtilFunctions.Range<Vector>(1, 3), UtilFunctions.Range<Vector>(7, 3)),
            new Vector(((int[])[1, 2, 3]).Select(x => new BinaryOperator(">=", (Number)x, (Number)(x + 6))
                .Evaluate()))),
        new TestCase("[1,2,3]+5",
            new BinaryOperator("+", UtilFunctions.Range<Vector>(1, 3), (Number)5),
            new Vector(((int[])[1, 2, 3]).Select(x => new BinaryOperator("+", (Number)x, (Number)5).Evaluate()))),
        new TestCase("[1,2,3]-5",
            new BinaryOperator("-", UtilFunctions.Range<Vector>(1, 3), (Number)5),
            new Vector(((int[])[1, 2, 3]).Select(x => new BinaryOperator("-", (Number)x, (Number)5).Evaluate()))),
        new TestCase("{1,2,3}*5",
            new BinaryOperator("*", UtilFunctions.Range<Set>(1, 3), (Number)5),
            new Set(((int[])[1, 2, 3]).Select(x => new BinaryOperator("*", (Number)x, (Number)5).Evaluate()))),
        new TestCase("[1,2,3]×5",
            new BinaryOperator("×", UtilFunctions.Range<Vector>(1, 3), (Number)5),
            new Vector(((int[])[1, 2, 3]).Select(x => new BinaryOperator("×", (Number)x, (Number)5).Evaluate()))),
        new TestCase("[1,2,3]/5",
            new BinaryOperator("/", UtilFunctions.Range<Vector>(1, 3), (Number)5),
            new Vector(((int[])[1, 2, 3]).Select(x => new BinaryOperator("/", (Number)x, (Number)5).Evaluate()))),
        new TestCase("[1,2,3]÷5",
            new BinaryOperator("÷", UtilFunctions.Range<Vector>(1, 3), (Number)5),
            new Vector(((int[])[1, 2, 3]).Select(x => new BinaryOperator("÷", (Number)x, (Number)5).Evaluate()))),
        new TestCase("[1,2,3]^5",
            new BinaryOperator("^", UtilFunctions.Range<Vector>(1, 3), (Number)5),
            new Vector(((int[])[1, 2, 3]).Select(x => new BinaryOperator("^", (Number)x, (Number)5).Evaluate()))),
        new TestCase("[1,2,3]%5",
            new BinaryOperator("%", UtilFunctions.Range<Vector>(1, 3), (Number)5),
            new Vector(((int[])[1, 2, 3]).Select(x => new BinaryOperator("%", (Number)x, (Number)5).Evaluate()))),
        new TestCase("{1,2,3}%%5",
            new BinaryOperator("%%", UtilFunctions.Range<Set>(1, 3), (Number)5),
            new Set(((int[])[1, 2, 3]).Select(x => new BinaryOperator("%%", (Number)x, (Number)5).Evaluate()))),
        new TestCase("[1,2,3]//5",
            new BinaryOperator("//", UtilFunctions.Range<Vector>(1, 3), (Number)5),
            new Vector(((int[])[1, 2, 3]).Select(x => new BinaryOperator("//", (Number)x, (Number)5).Evaluate()))),
        new TestCase("[1,2,3]&&5",
            new BinaryOperator("&&", UtilFunctions.Range<Vector>(1, 3), (Number)5),
            new Vector(((int[])[1, 2, 3]).Select(x => new BinaryOperator("&&", (Number)x, (Number)5).Evaluate()))),
        new TestCase("[1,2,3]∧5",
            new BinaryOperator("∧", UtilFunctions.Range<Vector>(1, 3), (Number)5),
            new Vector(((int[])[1, 2, 3]).Select(x => new BinaryOperator("∧", (Number)x, (Number)5).Evaluate()))),
        new TestCase("[1,2,3]||5",
            new BinaryOperator("||", UtilFunctions.Range<Vector>(1, 3), (Number)5),
            new Vector(((int[])[1, 2, 3]).Select(x => new BinaryOperator("||", (Number)x, (Number)5).Evaluate()))),
        new TestCase("[1,2,3]∨5",
            new BinaryOperator("∨", UtilFunctions.Range<Vector>(1, 3), (Number)5),
            new Vector(((int[])[1, 2, 3]).Select(x => new BinaryOperator("∨", (Number)x, (Number)5).Evaluate()))),
        new TestCase("[1,2,3]⊕5",
            new BinaryOperator("⊕", UtilFunctions.Range<Vector>(1, 3), (Number)5),
            new Vector(((int[])[1, 2, 3]).Select(x => new BinaryOperator("⊕", (Number)x, (Number)5).Evaluate()))),
        new TestCase("{1,2,3}==5",
            new BinaryOperator("==", UtilFunctions.Range<Set>(1, 3), (Number)5),
            new Set([(Number)0, (Number)0, (Number)0])),
        new TestCase("[1,2,3]!=5",
            new BinaryOperator("!=", UtilFunctions.Range<Vector>(1, 3), (Number)5),
            new Vector([(Number)1, (Number)1, (Number)1])),
        new TestCase("[1,2,3]<5",
            new BinaryOperator("<", UtilFunctions.Range<Vector>(1, 3), (Number)5),
            new Vector(((int[])[1, 2, 3]).Select(x => new BinaryOperator("<", (Number)x, (Number)5).Evaluate()))),
        new TestCase("[1,2,3]<=5",
            new BinaryOperator("<=", UtilFunctions.Range<Vector>(1, 3), (Number)5),
            new Vector(((int[])[1, 2, 3]).Select(x => new BinaryOperator("<=", (Number)x, (Number)5).Evaluate()))),
        new TestCase("[1,2,3]>5",
            new BinaryOperator(">", UtilFunctions.Range<Vector>(1, 3), (Number)5),
            new Vector(((int[])[1, 2, 3]).Select(x => new BinaryOperator(">", (Number)x, (Number)5).Evaluate()))),
        new TestCase("{1,2,3}>=5",
            new BinaryOperator(">=", UtilFunctions.Range<Set>(1, 3), (Number)5),
            new Set(((int[])[1, 2, 3]).Select(x => new BinaryOperator(">=", (Number)x, (Number)5).Evaluate()))),
        new TestCase("{1,2,3}=={1,2,3}",
            new BinaryOperator("==", UtilFunctions.Range<Set>(1, 3), UtilFunctions.Range<Set>(1, 3)),
            (Number)1),
        new TestCase("{1,2,3}=={7,8,9}",
            new BinaryOperator("==", UtilFunctions.Range<Set>(1, 3), UtilFunctions.Range<Set>(7, 3)),
            (Number)0),
        new TestCase("{1,2,3}!={7,8,9}",
            new BinaryOperator("!=", UtilFunctions.Range<Set>(1, 3), UtilFunctions.Range<Set>(7, 3)),
            (Number)1),
        new TestCase("{1,2,3}<{7,8,9}",
            new BinaryOperator("<", UtilFunctions.Range<Set>(1, 3), UtilFunctions.Range<Set>(7, 3)),
            new Set(((int[])[1, 2, 3]).Select(x => new BinaryOperator("<", (Number)x, (Number)(x + 6)).Evaluate()))),
        new TestCase("{1,2,3}<={7,8,9}",
            new BinaryOperator("<=", UtilFunctions.Range<Set>(1, 3), UtilFunctions.Range<Set>(7, 3)),
            new Set(((int[])[1, 2, 3]).Select(x => new BinaryOperator("<=", (Number)x, (Number)(x + 6))
                .Evaluate()))),
        new TestCase("{1,2,3}>{7,8,9}",
            new BinaryOperator(">", UtilFunctions.Range<Set>(1, 3), UtilFunctions.Range<Set>(7, 3)),
            new Set(((int[])[1, 2, 3]).Select(x => new BinaryOperator(">", (Number)x, (Number)(x + 6)).Evaluate()))),
        new TestCase("{1,2,3}>={7,8,9}",
            new BinaryOperator(">=", UtilFunctions.Range<Set>(1, 3), UtilFunctions.Range<Set>(7, 3)),
            new Set(((int[])[1, 2, 3]).Select(x => new BinaryOperator(">=", (Number)x, (Number)(x + 6))
                .Evaluate()))),
        new TestCase("[1,2,3]=={1,2,3}",
            new BinaryOperator("==", UtilFunctions.Range<Vector>(1, 3), UtilFunctions.Range<Set>(1, 3)),
            (Number)0),
        new TestCase("[1,2,3][0] ", new Indexer(UtilFunctions.Range<Vector>(1, 3), (Number)0), (Number)1),
        new TestCase("{1,2,3}[1]", new Indexer(UtilFunctions.Range<Set>(1, 3), (Number)1), (Number)2),
        new TestCase("[1,2,3][2]", new Indexer(UtilFunctions.Range<Vector>(1, 3), (Number)2), (Number)3),
        new TestCase("[1,2,3] [4] ", new Indexer(UtilFunctions.Range<Vector>(1, 3), (Number)4), Constant.UNDEFINED),
        new TestCase("3[2]", new Indexer((Number)3, (Number)2), Constant.UNDEFINED),
        new TestCase("-[2]", new UnaryOperator("-", true, new Vector([(Number)2])), new Vector([(Number)(-2)])),
    ];
}
