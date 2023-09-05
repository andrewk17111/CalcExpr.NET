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
        { "undefined", new Constant("undefined") },
        { "dne", new Constant("dne") },
    };

    public static Constant INFINITY
        => new Constant("∞");

    public static Constant PI
        => new Constant("π");

    public static Constant TAU
        => new Constant("τ");

    public static Constant E
        => new Constant("e");

    public static Constant TRUE
        => new Constant("true");

    public static Constant FALSE
        => new Constant("false");

    public static Constant UNDEFINED
        => new Constant("undefined");

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
        => Evaluate(null);

    public IExpression Evaluate(Dictionary<string, IExpression>? variables)
        => _values[Identifier].Clone();

    public IExpression StepEvaluate()
        => StepEvaluate(null);

    public IExpression StepEvaluate(Dictionary<string, IExpression>? variables)
        => _values[Identifier].Clone();

    public override bool Equals(object? obj)
        => obj is not null && obj is Constant c && Identifier switch
            {
                "∞" or "inf" or "infinity" => c.Identifier == "∞" || c.Identifier == "inf" || c.Identifier == "infinity",
                "π" or "pi" => c.Identifier == "π" || c.Identifier == "pi",
                "τ" or "tau" => c.Identifier == "τ" || c.Identifier == "tau",
                "e" or "true" or "false" => c.Identifier == Identifier,
                "undefined" or "dne" => c.Identifier == "undefined" || c.Identifier == "dne",
                _ => false,
            };

    public override int GetHashCode()
        => Identifier.GetHashCode();

    public override string ToString()
        => ToString(null);

    public string ToString(string? format)
        => Identifier;
}
