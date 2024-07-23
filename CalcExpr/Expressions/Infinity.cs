using CalcExpr.BuiltInFunctions;
using CalcExpr.Context;
using CalcExpr.Expressions.Interfaces;

namespace CalcExpr.Expressions;

/// <summary>
/// Initializes a new instance of the the <see cref="Infinity"/> class.
/// </summary>
/// <param name="identifier">The identifier <see cref="string"/> for this <see cref="Infinity"/>.</param>
/// <param name="positive">Boolean value indicating whether this <see cref="Infinity"/> is positive.</param>
public class Infinity(string identifier, bool positive = true) : IExpression, IBoolConvertible, IPrefixOperable,
    IPostfixOperable
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

    public Logical ToBool()
        => Logical.TRUE;

    public IExpression PrefixOperate(string identifier, ExpressionContext _)
    {
        return identifier switch
        {
            PrefixOperator.POSITIVE => this,
            PrefixOperator.NEGATIVE => new Infinity(Identifier, Negative),
            PrefixOperator.NOT or PrefixOperator.NOT_ALT => Logical.FALSE,
            PrefixOperator.SUBFACTORIAL => ((Number)FactorialFunctions.Subfactorial(this)).Evaluate(),
            PrefixOperator.PRE_DECREMENT or PrefixOperator.PRE_INCREMENT => this,
            _ => Undefined.UNDEFINED,
        };
    }

    public IExpression PostfixOperate(string identifier, ExpressionContext _)
    {
        return identifier switch
        {
            PostfixOperator.FACTORIAL => ((Number)FactorialFunctions.Factorial(this)).Evaluate(),
            PostfixOperator.DOUBLE_FACTORIAL => ((Number)FactorialFunctions.DoubleFactorial(this)).Evaluate(),
            PostfixOperator.PRIMORIAL => ((Number)FactorialFunctions.Primorial(this)).Evaluate(),
            PostfixOperator.PERCENT or PostfixOperator.POST_DECREMENT or PostfixOperator.POST_INCREMENT => this,
            _ => Undefined.UNDEFINED,
        };
    }

    public override bool Equals(object? obj)
        => obj is not null && obj is Infinity i && i.Positive == Positive;

    public override int GetHashCode()
        => Positive.GetHashCode();

    public override string ToString()
        => ToString(null);

    public string ToString(string? format)
        => $"{(Positive ? "" : '-')}{Identifier}";

    public static implicit operator double(Infinity infinity)
        => infinity.Positive ? Double.PositiveInfinity : Double.NegativeInfinity;
}
