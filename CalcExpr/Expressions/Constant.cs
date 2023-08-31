namespace CalcExpr.Expressions;

public class Constant : IExpression
{
    public readonly string Identifier;

    /// <summary>
    /// Initializes a new instance of the the <see cref="Constant"/> class.
    /// </summary>
    /// <param name="identifier">The identifier <see cref="string"/> for this <see cref="Constant"/>.</param>
    public Constant(string identifier)
        => Identifier = identifier;

    public IExpression Clone()
        => new Constant(Identifier);

    public IExpression Evaluate()
        => throw new NotImplementedException();

    public IExpression StepEvaluate()
        => throw new NotImplementedException();

    public override bool Equals(object? obj)
        => obj is not null && obj is Constant c && c.Identifier == Identifier;

    public override int GetHashCode()
        => Identifier.GetHashCode();

    public override string ToString()
        => ToString(null);

    public string ToString(string? format)
        => Identifier;
}
