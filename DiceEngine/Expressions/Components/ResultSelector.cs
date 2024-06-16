using DiceEngine.Expressions.Dice;
using System.Collections.ObjectModel;

namespace DiceEngine.Expressions.Components;

/// <summary>
/// The selector for a <see cref="Dice.DiceOperator"/>.
/// </summary>
/// <param name="selector">The symbol representing the method of selecting results.</param>
/// <param name="value">The value to use in the selection.</param>
public readonly struct ResultSelector(string selector, int value)
{
    private static readonly ReadOnlyDictionary<string, Func<int, RollResult, IEnumerable<int>>> SELECTORS =
        new Dictionary<string, Func<int, RollResult, IEnumerable<int>>>
        {
            { "", SelectLiteral },
            { "h", SelectHighest },
            { "l", SelectLowest },
            { "<", SelectLessThan },
            { "<=", SelectLessThanOrEqual },
            { ">", SelectGreaterThan },
            { ">=", SelectGreaterThanOrEqual },
        }.AsReadOnly();

    public readonly string Selector = selector;
    public readonly int Value = value;

    private static IEnumerable<int> SelectLiteral(int value, RollResult result)
    {
        for (int i = 0; i < result.Length; i++)
        {
            if (result[i] == value)
                yield return i;
        }
    }

    private static IEnumerable<int> SelectHighest(int value, RollResult result)
    {
        return result.Select((x, i) => (i, x))
            .OrderByDescending(pair => pair.x)
            .Take(value)
            .Select(pair => pair.i);
    }

    private static IEnumerable<int> SelectLowest(int value, RollResult result)
    {
        return result.Select((x, i) => (i, x))
            .OrderBy(pair => pair.x)
            .Take(value)
            .Select(pair => pair.i);
    }

    private static IEnumerable<int> SelectLessThan(int value, RollResult result)
    {
        for (int i = 0; i < result.Length; i++)
        {
            if (result[i] < value)
                yield return i;
        }
    }

    private static IEnumerable<int> SelectLessThanOrEqual(int value, RollResult result)
    {
        for (int i = 0; i < result.Length; i++)
        {
            if (result[i] <= value)
                yield return i;
        }
    }

    private static IEnumerable<int> SelectGreaterThan(int value, RollResult result)
    {
        for (int i = 0; i < result.Length; i++)
        {
            if (result[i] > value)
                yield return i;
        }
    }

    private static IEnumerable<int> SelectGreaterThanOrEqual(int value, RollResult result)
    {
        for (int i = 0; i < result.Length; i++)
        {
            if (result[i] >= value)
                yield return i;
        }
    }
}
