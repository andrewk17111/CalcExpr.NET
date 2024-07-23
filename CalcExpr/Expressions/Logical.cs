using CalcExpr.BuiltInFunctions;
using CalcExpr.Context;
using CalcExpr.Expressions.Interfaces;

namespace CalcExpr.Expressions;

/// <summary>
/// Creates a new instance of the <see cref="Logical"/> class.
/// </summary>
/// <param name="value">The value of the <see cref="Logical"/>.</param>
public class Logical(bool value) : IExpression, IPrefixOperable, IPostfixOperable
{
    public static readonly Logical TRUE = new Logical(true);
    public static readonly Logical FALSE = new Logical(false);

    public readonly bool Value = value;

    public IExpression Evaluate()
        => Value ? (Number)1 : (Number)0;

    public IExpression Evaluate(ExpressionContext _)
        => Evaluate();

    public IExpression StepEvaluate()
        => Evaluate();

    public IExpression StepEvaluate(ExpressionContext _)
        => Evaluate();

    public IExpression PrefixOperate(string identifier, ExpressionContext _)
    {
        return identifier switch
        {
            PrefixOperator.NOT or PrefixOperator.NOT_ALT => Value ? FALSE : TRUE,
            PrefixOperator.SUBFACTORIAL => (Number)FactorialFunctions.Subfactorial(Value ? 1 : 0),
            PrefixOperator.POSITIVE or PrefixOperator.NEGATIVE => this,
            PrefixOperator.PRE_DECREMENT => Value ? (Number)0 : (Number)(-1),
            PrefixOperator.PRE_INCREMENT => Value ? (Number)2 : (Number)0,
            _ => Undefined.UNDEFINED,
        };
    }

    public IExpression PostfixOperate(string identifier, ExpressionContext _)
    {
        return identifier switch
        {
            PostfixOperator.PERCENT => Value ? (Number)0.01 : (Number)0,
            PostfixOperator.FACTORIAL => (Number)FactorialFunctions.Factorial(this),
            PostfixOperator.DOUBLE_FACTORIAL => (Number)FactorialFunctions.DoubleFactorial(this),
            PostfixOperator.PRIMORIAL => (Number)FactorialFunctions.Primorial(this),
            PostfixOperator.POST_DECREMENT or PostfixOperator.POST_INCREMENT => this,
            _ => Undefined.UNDEFINED,
        };
    }

    public override bool Equals(object? obj)
        => obj is not null && obj is Logical logical && logical.Value == Value;

    public override int GetHashCode()
        => Value.GetHashCode();

    public override string ToString()
        => ToString(null);

    public string ToString(string? _)
        => Value ? "true" : "false";

    public static implicit operator bool(Logical logical)
        => logical.Value;

    public static implicit operator int(Logical logical)
        => logical.Value ? 1 : 0;
}
