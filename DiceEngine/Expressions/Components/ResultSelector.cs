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
    private static readonly ReadOnlyDictionary<string, Func<int, IEnumerable<RollValue>, IEnumerable<int>>> SELECTORS =
        new Dictionary<string, Func<int, IEnumerable<RollValue>, IEnumerable<int>>>
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

    public int[] Select(IEnumerable<RollValue> result)
    {
        if (SELECTORS.TryGetValue(Selector, out Func<int, IEnumerable<RollValue>, IEnumerable<int>>? selector))
            return selector(Value, result).ToArray();

        return [];
    }

    private static IEnumerable<int> SelectLiteral(int value, IEnumerable<RollValue> results)
    {
        int i = 0;

        foreach (RollValue result in results)
        {
            if (result == value)
                yield return i++;
        }
    }

    private static IEnumerable<int> SelectHighest(int value, IEnumerable<RollValue> results)
    {
        return results.Select((x, i) => (i, x))
            .OrderByDescending(pair => pair.x)
            .Take(value)
            .Select(pair => pair.i);
    }

    private static IEnumerable<int> SelectLowest(int value, IEnumerable<RollValue> results)
    {
        return results.Select((x, i) => (i, x))
            .OrderBy(pair => pair.x)
            .Take(value)
            .Select(pair => pair.i);
    }

    private static IEnumerable<int> SelectLessThan(int value, IEnumerable<RollValue> results)
    {
        int i = 0;

        foreach (RollValue result in results)
        {
            if (result < value)
                yield return i++;
        }
    }

    private static IEnumerable<int> SelectLessThanOrEqual(int value, IEnumerable<RollValue> results)
    {
        int i = 0;

        foreach (RollValue result in results)
        {
            if (result <= value)
                yield return i++;
        }
    }

    private static IEnumerable<int> SelectGreaterThan(int value, IEnumerable<RollValue> results)
    {
        int i = 0;

        foreach (RollValue result in results)
        {
            if (result > value)
                yield return i++;
        }
    }

    private static IEnumerable<int> SelectGreaterThanOrEqual(int value, IEnumerable<RollValue> results)
    {
        int i = 0;

        foreach (RollValue result in results)
        {
            if (result >= value)
                yield return i++;
        }
    }
}
