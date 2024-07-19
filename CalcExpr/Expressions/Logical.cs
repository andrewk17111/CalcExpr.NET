using CalcExpr.Context;

namespace CalcExpr.Expressions;

/// <summary>
/// Creates a new instance of the <see cref="Logical"/> class.
/// </summary>
/// <param name="value">The value of the <see cref="Logical"/>.</param>
public class Logical(bool value) : IExpression
{
    public static readonly Logical TRUE = new Logical(true);
    public static readonly Logical FALSE = new Logical(false);

    public readonly bool Value = value;

    public IExpression Evaluate()
        => Value ? (Number)1 : (Number)0;

    public IExpression Evaluate(ExpressionContext _)
        => Evaluate();

    public IExpression StepEvaluate()
        => Evaluate();

    public IExpression StepEvaluate(ExpressionContext _)
        => Evaluate();

    public override bool Equals(object? obj)
        => obj is not null && obj is Logical logical && logical.Value == Value;

    public override int GetHashCode()
        => Value.GetHashCode();

    public override string ToString()
        => ToString(null);

    public string ToString(string? _)
        => Value ? "true" : "false";
}
