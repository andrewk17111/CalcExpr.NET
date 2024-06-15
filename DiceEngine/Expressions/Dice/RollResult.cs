using DiceEngine.Context;
using System.Collections;

namespace DiceEngine.Expressions.Dice;

public class RollResult(IEnumerable<int> results, IDie die) : Number(double.NaN), IEnumerable<IExpression>
{
    private readonly int[] _results = results.ToArray();

    public readonly IDie Die = die;

    public new int Value => _results.Sum();

    public int this[int index]
        => _results[index];

    public int[] this[Range range]
        => _results[range];

    public int Length
        => _results.Length;

    public RollResult(int result, IDie die) : this([result], die)
    {
    }

    public new IExpression Evaluate(ExpressionContext _)
        => (Number)Value;

    public new IExpression StepEvaluate(ExpressionContext _)
        => (Number)Value;

    public override int GetHashCode()
        => HashCode.Combine(Die, _results);

    public override bool Equals(object? obj)
        => obj is RollResult result && Die.Equals(result.Die) && _results.SequenceEqual(result._results);

    public override string ToString()
        => ToString(null);

    public new string ToString(string? format)
        => $"{Die} ({String.Join(", ", _results.Select(r => r.ToString(format)))})";

    public IEnumerator<IExpression> GetEnumerator()
        => _results.Select(x => (Number)x).Cast<IExpression>().GetEnumerator();

    IEnumerator IEnumerable.GetEnumerator()
        => _results.GetEnumerator();
}
