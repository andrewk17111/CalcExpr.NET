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
        => Inside.Evaluate();

    public IExpression StepEvaluate()
        => Inside is Number n
            ? n.StepEvaluate()
            : new Parentheses(Inside.StepEvaluate());

    public IExpression Evaluate(Dictionary<string, IExpression> variables)
        => throw new NotImplementedException();

    public IExpression StepEvaluate(Dictionary<string, IExpression> variables)
        => throw new NotImplementedException();

    public override bool Equals(object? obj)
        => obj is not null && obj is Parentheses paren && paren.Inside.Equals(Inside);

    public override int GetHashCode()
        => Inside.GetHashCode();

    public override string ToString()
        => ToString(null);

    public string ToString(string? format)
        => $"({Inside.ToString(format)})";
}
