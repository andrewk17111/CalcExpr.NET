using CalcExpr.Context;
using CalcExpr.Expressions.Collections;

namespace CalcExpr.Expressions;

/// <summary>
/// Initializes a new instance of the the <see cref="Constant"/> class.
/// </summary>
/// <param name="identifier">The identifier <see cref="string"/> for this <see cref="Constant"/>.</param>
public class Constant(string identifier) : IExpression
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
        { "-∞", new Constant("-∞") },
        { "-inf", new Constant("-inf") },
        { "-infinity", new Constant("-infinity") },
        { "∅", new Set() },
        { "empty", new Set() },
        { "empty_set", new Set() },
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

    public static Constant NEGATIVE_INFINITY
        => new Constant("-∞");

    public readonly string Identifier = identifier;

    public IExpression Evaluate()
        => Evaluate(new ExpressionContext());

    public IExpression Evaluate(ExpressionContext variables)
        => _values[Identifier];

    public IExpression StepEvaluate()
        => StepEvaluate(new ExpressionContext());

    public IExpression StepEvaluate(ExpressionContext variables)
        => _values[Identifier];

    public override bool Equals(object? obj)
        => obj is not null && obj is Constant c && 
            (c.Identifier == Identifier || Identifier switch
            {
                "∞" or "inf" or "infinity" => c.Identifier == "∞" || c.Identifier == "inf" ||
                    c.Identifier == "infinity",
                "π" or "pi" => c.Identifier == "π" || c.Identifier == "pi",
                "τ" or "tau" => c.Identifier == "τ" || c.Identifier == "tau",
                "e" or "true" or "false" => c.Identifier == Identifier,
                "undefined" or "dne" => c.Identifier == "undefined" || c.Identifier == "dne",
                "-∞" or "-inf" or "-infinity" => c.Identifier == "-∞" || c.Identifier == "-inf" ||
                    c.Identifier == "-infinity",
                "∅" or "empty" or "empty_set" => c.Identifier == "∅" || c.Identifier == "empty" ||
                    c.Identifier == "empty_set",
                _ => false,
            });

    public override int GetHashCode()
        => Identifier.GetHashCode();

    public override string ToString()
        => ToString(null);

    public string ToString(string? format)
        => Identifier;
}
