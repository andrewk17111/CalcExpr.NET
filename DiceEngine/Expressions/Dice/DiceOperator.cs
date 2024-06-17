using DiceEngine.Expressions.Components;
using System.Collections.ObjectModel;

namespace DiceEngine.Expressions.Dice;

/// <summary>
/// Initializes a new instance of the <see cref="DiceOperator"/> class.
/// </summary>
/// <param name="op">The identifier for the operator.</param>
/// <param name="result">The result of the dice roll.</param>
/// <param name="right">The <see cref="IExpression"/> right operand for this operator.</param>
public class DiceOperator(string op, RollResult result, ResultSelector selector)
{
    private static readonly ReadOnlyDictionary<string, Func<RollResult, ResultSelector, IEnumerable<RollValue>>> OPERATORS =
        new Dictionary<string, Func<RollResult, ResultSelector, IEnumerable<RollValue>>>
        {
            { "k",  Keep },
            { "d",  Drop },
            { "rr",  Reroll },
            { "r",  Reroll },
            { "ro", RerollOnce },
            { "e",  Explode },
            { "mi", Minimum },
            { "ma", Maximum },
        }.AsReadOnly();

    public readonly string Identifier = op;
    public readonly RollResult Result = result;
    public readonly ResultSelector Selector = selector;

    public IExpression Evaluate()
    {
        if (OPERATORS.TryGetValue(Identifier, out Func<RollResult, ResultSelector, IEnumerable<RollValue>>? op) &&
            !((Identifier == "ma" || Identifier == "mi") && Selector.Selector != ""))
                return new RollResult(op(Result, Selector), Result.Die);

        return Constant.UNDEFINED;
    }

    private static IEnumerable<RollValue> Keep(RollResult result, ResultSelector selector)
    {
        int[] selected = selector.Select(result);

        return result.Select((r, i) => selected.Contains(i) ? r : r.Drop());
    }

    private static IEnumerable<RollValue> Drop(RollResult result, ResultSelector selector)
    {
        int[] selected = selector.Select(result);

        return result.Select((r, i) => selected.Contains(i) ? r.Drop() : r);
    }

    private static IEnumerable<RollValue> Reroll(RollResult result, ResultSelector selector)
    {
        IEnumerable<RollValue> values = result;
        
        for (int[] selected = selector.Select(values); selected.Length > 0; selected = selector.Select(values))
        {
            values = values.SelectMany<RollValue, RollValue>(
                (r, i) => selected.Contains(i) ? [r.Drop(), (RollValue)result.Die.Roll()] : [r]);
        }

        return values;
    }

    private static IEnumerable<RollValue> RerollOnce(RollResult result, ResultSelector selector)
    {
        int[] selected = selector.Select(result);
        
        return result.SelectMany<RollValue, RollValue>(
                (r, i) => selected.Contains(i) ? [r.Drop(), (RollValue)result.Die.Roll()] : [r]);
    }

    // TODO: Reroll and Add.

    private static IEnumerable<RollValue> Explode(RollResult result, ResultSelector selector)
    {
        IEnumerable<RollValue> values = result;
        
        for (int[] selected = selector.Select(values); selected.Length > 0; selected = selector.Select(values))
        {
            values = values.SelectMany<RollValue, RollValue>(
                (r, i) => selected.Contains(i) ? [r.Explode(), (RollValue)result.Die.Roll()] : [r]);
        }

        return values;
    }

    private static IEnumerable<RollValue> Minimum(RollResult result, ResultSelector selector)
    {
        return result.SelectMany<RollValue, RollValue>(
            (r, i) => r < selector.Value ? [r.Drop(), (RollValue)selector.Value] : [r]);
    }

    private static IEnumerable<RollValue> Maximum(RollResult result, ResultSelector selector)
    {
        return result.SelectMany<RollValue, RollValue>(
            (r, i) => r < selector.Value ? [r.Drop(), (RollValue)selector.Value] : [r]);
    }
}
