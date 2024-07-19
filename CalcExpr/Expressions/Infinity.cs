using CalcExpr.Context;

namespace CalcExpr.Expressions;

/// <summary>
/// Initializes a new instance of the the <see cref="Infinity"/> class.
/// </summary>
/// <param name="identifier">The identifier <see cref="string"/> for this <see cref="Infinity"/>.</param>
/// <param name="positive">Boolean value indicating whether this <see cref="Infinity"/> is positive.</param>
public class Infinity(string identifier, bool positive = true) : IExpression
{
    public static readonly Infinity POSITIVE = new Infinity("∞", true);
    public static readonly Infinity NEGATIVE = new Infinity("∞", false);

    public readonly string Identifier = identifier;
    public readonly bool Positive = positive;
    public readonly bool Negative = !positive;

    public IExpression Evaluate()
        => this;

    public IExpression Evaluate(ExpressionContext _)
        => this;

    public IExpression StepEvaluate()
        => this;

    public IExpression StepEvaluate(ExpressionContext _)
        => this;

    public override bool Equals(object? obj)
        => obj is not null && obj is Infinity i && i.Positive == Positive;

    public override int GetHashCode()
        => Positive.GetHashCode();

    public override string ToString()
        => ToString(null);

    public string ToString(string? format)
        => $"{(Positive ? "" : '-')}{Identifier}";
}
