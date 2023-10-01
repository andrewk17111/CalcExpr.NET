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
        => Evaluate(new Dictionary<string, IExpression>());

    public IExpression Evaluate(Dictionary<string, IExpression> variables)
        => Inside.Evaluate(variables);

    public IExpression StepEvaluate()
        => StepEvaluate(new Dictionary<string, IExpression>());

    public IExpression StepEvaluate(Dictionary<string, IExpression> variables)
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
