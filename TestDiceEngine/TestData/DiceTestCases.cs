using DiceEngine.Expressions;
using DiceEngine.Expressions.Dice;
using TestDiceEngine.TestModels;
using TestDiceEngine.TestUtils;

namespace TestDiceEngine.TestData;

public static partial class TestCases
{
    public readonly static TestCase[] DiceNotation =
    [
        new TestCase("d1", (Die)1, new RandomCollection(1, 1, 1, 1,
            collectionValidator: x => CompareDieToResult((Die)1, x))),
        new TestCase("d2", (Die)2, new RandomCollection(1, 1, 1, 2,
            collectionValidator: x => CompareDieToResult((Die)2, x))),
        new TestCase("d4", (Die)4, new RandomCollection(1, 1, 1, 4,
            collectionValidator: x => CompareDieToResult((Die)4, x))),
        new TestCase("d6", (Die)6, new RandomCollection(1, 1, 1, 6,
            collectionValidator: x => CompareDieToResult((Die)6, x))),
        new TestCase("d8", (Die)8, new RandomCollection(1, 1, 1, 8,
            collectionValidator: x => CompareDieToResult((Die)8, x))),
        new TestCase("d10", (Die)10, new RandomCollection(1, 1, 1, 10,
            collectionValidator: x => CompareDieToResult((Die)10, x))),
        new TestCase("d12", (Die)12, new RandomCollection(1, 1, 1, 12,
            collectionValidator: x => CompareDieToResult((Die)12, x))),
        new TestCase("d20", (Die)20, new RandomCollection(1, 1, 1, 20,
            collectionValidator: x => CompareDieToResult((Die)20, x))),
        new TestCase("d100", (Die)100, new RandomCollection(1, 1, 1, 100,
            collectionValidator: x => CompareDieToResult((Die)100, x))),
        new TestCase("1+d1", new BinaryOperator("+", (Number)1, (Die)1),
            new RandomValue(2, 2), new BinaryOperator("+", (Number)1,
                new RandomCollection(1, 1, 1, 1, collectionValidator: x => CompareDieToResult((Die)1, x)))),
        new TestCase("1+d2", new BinaryOperator("+", (Number)1, (Die)2),
            new RandomValue(2, 3), new BinaryOperator("+", (Number)1,
                new RandomCollection(1, 1, 1, 2, collectionValidator: x => CompareDieToResult((Die)2, x))),
            new BinaryOperator("+", (Number)1, new RandomValue(1, 2))),
        new TestCase("1+d4", new BinaryOperator("+", (Number)1, (Die)4),
            new RandomValue(2, 5), new BinaryOperator("+", (Number)1,
                new RandomCollection(1, 1, 1, 4, collectionValidator: x => CompareDieToResult((Die)4, x))),
            new BinaryOperator("+", (Number)1, new RandomValue(1, 4))),
        new TestCase("1+d6", new BinaryOperator("+", (Number)1, (Die)6),
            new RandomValue(2, 7), new BinaryOperator("+", (Number)1,
                new RandomCollection(1, 1, 1, 6, collectionValidator: x => CompareDieToResult((Die)6, x))),
            new BinaryOperator("+", (Number)1, new RandomValue(1, 6))),
        new TestCase("1+d8", new BinaryOperator("+", (Number)1, (Die)8),
            new RandomValue(2, 9), new BinaryOperator("+", (Number)1,
                new RandomCollection(1, 1, 1, 8, collectionValidator: x => CompareDieToResult((Die)8, x))),
            new BinaryOperator("+", (Number)1, new RandomValue(1, 8))),
        new TestCase("1+d10", new BinaryOperator("+", (Number)1, (Die)10),
            new RandomValue(2, 11), new BinaryOperator("+", (Number)1,
                new RandomCollection(1, 1, 1, 10, collectionValidator: x => CompareDieToResult((Die)10, x))),
            new BinaryOperator("+", (Number)1, new RandomValue(1, 10))),
        new TestCase("1+d12", new BinaryOperator("+", (Number)1, (Die)12),
            new RandomValue(2, 13), new BinaryOperator("+", (Number)1,
                new RandomCollection(1, 1, 1, 12, collectionValidator: x => CompareDieToResult((Die)12, x))),
            new BinaryOperator("+", (Number)1, new RandomValue(1, 12))),
        new TestCase("1+d20", new BinaryOperator("+", (Number)1, (Die)20),
            new RandomValue(2, 21), new BinaryOperator("+", (Number)1,
                new RandomCollection(1, 1, 1, 20, collectionValidator: x => CompareDieToResult((Die)20, x))),
            new BinaryOperator("+", (Number)1, new RandomValue(1, 20))),
        new TestCase("1+d100", new BinaryOperator("+", (Number)1, (Die)100),
            new RandomValue(2, 101), new BinaryOperator("+", (Number)1,
                new RandomCollection(1, 1, 1, 100, collectionValidator: x => CompareDieToResult((Die)100, x))),
            new BinaryOperator("+", (Number)1, new RandomValue(1, 100))),
        new TestCase("d%", new PercentileDie(), new RandomCollection(1, 1, 0, 90,
            x => x is Number num && num.Value % 10 == 0 && num.Value >= 0 && num.Value <= 90,
            x => CompareDieToResult(new PercentileDie(), x))),
    ];

    private static bool CompareDieToResult(IDie expected, IEnumerable<IExpression> actual)
        => actual is RollResult result && expected.Equals(result.Die);
}
