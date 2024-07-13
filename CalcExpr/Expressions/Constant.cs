using CalcExpr.BuiltInFunctions;
using CalcExpr.Context;
using CalcExpr.Expressions.Collections;
using CalcExpr.Expressions.Interfaces;

namespace CalcExpr.Expressions;

/// <summary>
/// Initializes a new instance of the the <see cref="Constant"/> class.
/// </summary>
/// <param name="identifier">The identifier <see cref="string"/> for this <see cref="Constant"/>.</param>
public class Constant(string identifier) : IExpression, IBoolConvertible, IPrefixOperable, IPostfixOperable
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

    public readonly string Identifier = identifier;

    public static Constant INFINITY { get; } = new Constant("∞");
    public static Constant PI { get; } = new Constant("π");
    public static Constant TAU { get; } = new Constant("τ");
    public static Constant E { get; } = new Constant("e");
    public static Constant TRUE { get; } = new Constant("true");
    public static Constant FALSE { get; } = new Constant("false");
    public static Constant UNDEFINED { get; } = new Constant("undefined");
    public static Constant NEGATIVE_INFINITY { get; } = new Constant("-∞");

    public IExpression Evaluate()
        => Evaluate(new ExpressionContext());

    public IExpression Evaluate(ExpressionContext variables)
        => _values[Identifier];

    public IExpression StepEvaluate()
        => StepEvaluate(new ExpressionContext());

    public IExpression StepEvaluate(ExpressionContext variables)
        => _values[Identifier];

    public Constant ToBool()
        => Identifier switch
        {
            "false" => FALSE,
            "undefined" or "dne" => UNDEFINED,
            _ => TRUE,
        };

    public IExpression PrefixOperate(string identifier, ExpressionContext context)
    {
        switch (identifier)
        {
            case PrefixOperator.POSITIVE:
                return this;
            case PrefixOperator.NOT or PrefixOperator.NOT_ALT:
                return ToBool().Identifier switch
                {
                    "true" => FALSE,
                    "false" => TRUE,
                    _ => UNDEFINED,
                };
        }

        switch (Identifier)
        {
            case "π" or "pi" or "τ" or "tau" or "e" or "true" or "false":
                return ((Number)Evaluate(context)).PrefixOperate(identifier, context);
            case "∞" or "inf" or "infinity":
                return identifier switch
                {
                    PrefixOperator.PRE_DECREMENT or PrefixOperator.PRE_INCREMENT or PrefixOperator.SUBFACTORIAL
                        => INFINITY,
                    PrefixOperator.NEGATIVE => NEGATIVE_INFINITY,
                    _ => UNDEFINED,
                };
            case "-∞" or "-inf" or "-infinity":
                return identifier switch
                {
                    PrefixOperator.NEGATIVE or PrefixOperator.PRE_DECREMENT or PrefixOperator.PRE_INCREMENT
                        => NEGATIVE_INFINITY,
                    _ => UNDEFINED,
                };
            case "∅" or "empty" or "empty_set":
                return this;
            default:
                return UNDEFINED;
        };
    }

    public IExpression PostfixOperate(string identifier, ExpressionContext context)
    {
        return identifier switch
        {
            "π" or "pi" or "τ" or "tau" or "e" or "true" or "false"
                => ((Number)Evaluate(context)).PostfixOperate(identifier, context),
            "∞" or "inf" or "infinity" or "∅" or "empty" or "empty_set" => this,
            "-∞" or "-inf" or "-infinity" => identifier switch
            {
                PostfixOperator.PERCENT or PostfixOperator.POST_DECREMENT or PostfixOperator.POST_INCREMENT
                    => NEGATIVE_INFINITY,
                _ => UNDEFINED,
            },
            _ => UNDEFINED,
        };
    }

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
