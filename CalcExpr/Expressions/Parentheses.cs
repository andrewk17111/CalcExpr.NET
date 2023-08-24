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
        => throw new NotImplementedException();

    public IExpression Evaluate()
        => throw new NotImplementedException();

    public IExpression StepEvaluate()
        => throw new NotImplementedException();

    public override bool Equals(object? obj)
        => throw new NotImplementedException();

    public override int GetHashCode()
        => throw new NotImplementedException();

    public override string ToString()
        => ToString(null);

    public string ToString(string? format)
        => throw new NotImplementedException();
}
