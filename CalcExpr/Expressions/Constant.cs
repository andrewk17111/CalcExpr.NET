namespace CalcExpr.Expressions;

public class Constant : IExpression
{
    private static readonly Dictionary<string, IExpression> _values = new Dictionary<string, IExpression>()
    {
        { "∞", new Constant("∞") },
        { "inf", new Constant("inf") },
        { "infinity", new Constant("infinity") },
        { "π", new Number(Math.PI) },
        { "pi", new Number(Math.PI) },
        { "τ", new Number(Math.Tau) },
        { "tau", new Number(Math.Tau) },
        { "e", new Number(Math.E) },
        { "true", new Number(1) },
        { "false", new Number(0) },
    };

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
        => _values[Identifier].Clone();

    public IExpression Evaluate(Dictionary<string, IExpression> variables)
        => throw new NotImplementedException();

    public IExpression StepEvaluate()
        => _values[Identifier].Clone();

    public IExpression StepEvaluate(Dictionary<string, IExpression> variables)
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
