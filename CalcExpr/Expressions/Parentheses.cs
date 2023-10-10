using CalcExpr.Context;

namespace CalcExpr.Expressions;

public class Parentheses : IExpression
{
    public readonly IExpression Inside;

    /// <summary>
    /// Initializes a new instance of the <see cref="Parentheses"/> class.
    /// </summary>
    /// <param name="inside">The <see cref="IExpression"/> inside of this <see cref="Parentheses"/>.</param>
    public Parentheses(IExpression inside)
        => Inside = inside;

    public IExpression Clone()
        => new Parentheses(Inside.Clone());

    public IExpression Evaluate()
        => Evaluate(new ExpressionContext());

    public IExpression Evaluate(ExpressionContext variables)
        => Inside.Evaluate(variables);

    public IExpression StepEvaluate()
        => StepEvaluate(new ExpressionContext());

    public IExpression StepEvaluate(ExpressionContext variables)
        => Inside is Number n
            ? n.StepEvaluate(variables)
            : new Parentheses(Inside.StepEvaluate(variables));

    public override bool Equals(object? obj)
        => obj is not null && obj is Parentheses paren && paren.Inside.Equals(Inside);

    public override int GetHashCode()
        => Inside.GetHashCode();

    public override string ToString()
        => ToString(null);

    public string ToString(string? format)
        => $"({Inside.ToString(format)})";
}
