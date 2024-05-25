using DiceEngine.Context;
using System.Collections;

namespace DiceEngine.Expressions.Dice;

public class RollResult(IEnumerable<Number> results, IDie die) : Number(double.NaN), IEnumerable<IExpression>
{
    private readonly Number[] _results = results.ToArray();

    public readonly IDie Die = die;

    public new double Value => ((Number)Evaluate()).Value;

    public Number this[int index]
        => _results[index];

    public Number[] this[Range range]
        => _results[range];

    public int Length
        => _results.Length;

    public RollResult(Number result, IDie die)
        : this(new Number[] { result }, die)
    {
    }

    public new IExpression Evaluate(ExpressionContext _)
        => _results.Aggregate((a, b) => (Number)(a.Value + b.Value));

    public new IExpression StepEvaluate(ExpressionContext _)
        => _results.Aggregate((a, b) => (Number)(a.Value + b.Value));

    public override string ToString()
        => ToString(null);

    public new string ToString(string? format)
        => $"{Die} ({String.Join(", ", _results.Select(r => r.ToString(format)))})";

    public IEnumerator<IExpression> GetEnumerator()
        => ((IEnumerable<IExpression>)_results).GetEnumerator();

    IEnumerator IEnumerable.GetEnumerator()
        => _results.GetEnumerator();
}
