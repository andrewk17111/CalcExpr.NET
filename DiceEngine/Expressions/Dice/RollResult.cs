using DiceEngine.Context;
using System.Collections;

namespace DiceEngine.Expressions.Dice;

public class RollResult(IEnumerable<IExpression> results, IDie die) : IExpression, IEnumerable<IExpression>
{
    private readonly IExpression[] _results = results.ToArray();

    public readonly IDie Die = die;

    public IExpression this[int index]
        => _results[index];

    public IExpression[] this[Range range]
        => _results[range];

    public int Length
        => _results.Length;

    public IExpression Evaluate()
        => Evaluate(new ExpressionContext());

    public IExpression Evaluate(ExpressionContext context)
        => _results.Aggregate((a, b) => new BinaryOperator("+", a, b).Evaluate(context));

    public IExpression StepEvaluate()
        => StepEvaluate(new ExpressionContext());

    public IExpression StepEvaluate(ExpressionContext context)
        => _results.Aggregate((a, b) => new BinaryOperator("+", a, b).Evaluate(context));

    public override string ToString()
        => ToString(null);

    public string ToString(string? format)
        => $"{Die} ({String.Join(", ", _results.Select(r => r.ToString(format)))})";

    public IEnumerator<IExpression> GetEnumerator()
        => ((IEnumerable<IExpression>)_results).GetEnumerator();

    IEnumerator IEnumerable.GetEnumerator()
        => _results.GetEnumerator();
}
