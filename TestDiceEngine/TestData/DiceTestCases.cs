using DiceEngine.Expressions;
using DiceEngine.Expressions.Dice;
using TestDiceEngine.TestModels;
using TestDiceEngine.TestUtils;

namespace TestDiceEngine.TestData;

public static partial class TestCases
{
    public readonly static TestCase[] DiceNotation =
    [
        new TestCase("d1", (Die)1, new RandomValue(1, 1)),
        new TestCase("d2", (Die)2, new RandomValue(1, 2)),
        new TestCase("d4", (Die)4, new RandomValue(1, 4)),
        new TestCase("d6", (Die)6, new RandomValue(1, 6)),
        new TestCase("d8", (Die)8, new RandomValue(1, 8)),
        new TestCase("d10", (Die)10, new RandomValue(1, 10)),
        new TestCase("d12", (Die)12, new RandomValue(1, 12)),
        new TestCase("d20", (Die)20, new RandomValue(1, 20)),
        new TestCase("d100", (Die)100, new RandomValue(1, 100)),
        new TestCase("1+d1", new BinaryOperator("+", (Number)1, (Die)1),
            new RandomValue(2, 2), new BinaryOperator("+", (Number)1, new RandomValue(1, 1))),
        new TestCase("1+d2", new BinaryOperator("+", (Number)1, (Die)2),
            new RandomValue(2, 3), new BinaryOperator("+", (Number)1, new RandomValue(1, 2))),
        new TestCase("1+d4", new BinaryOperator("+", (Number)1, (Die)4),
            new RandomValue(2, 5), new BinaryOperator("+", (Number)1, new RandomValue(1, 4))),
        new TestCase("1+d6", new BinaryOperator("+", (Number)1, (Die)6),
            new RandomValue(2, 7), new BinaryOperator("+", (Number)1, new RandomValue(1, 6))),
        new TestCase("1+d8", new BinaryOperator("+", (Number)1, (Die)8),
            new RandomValue(2, 9), new BinaryOperator("+", (Number)1, new RandomValue(1, 8))),
        new TestCase("1+d10", new BinaryOperator("+", (Number)1, (Die)10),
            new RandomValue(2, 11), new BinaryOperator("+", (Number)1, new RandomValue(1, 10))),
        new TestCase("1+d12", new BinaryOperator("+", (Number)1, (Die)12),
            new RandomValue(2, 13), new BinaryOperator("+", (Number)1, new RandomValue(1, 12))),
        new TestCase("1+d20", new BinaryOperator("+", (Number)1, (Die)20),
            new RandomValue(2, 21), new BinaryOperator("+", (Number)1, new RandomValue(1, 20))),
        new TestCase("1+d100", new BinaryOperator("+", (Number)1, (Die)100),
            new RandomValue(2, 101), new BinaryOperator("+", (Number)1, new RandomValue(1, 100))),
    ];

    private static bool CompareDieToResult(IDie expected, IExpression actual)
        => actual is RollResult result && expected.Equals(result.Die);
}
