using CalcExpr.BuiltInFunctions;
using CalcExpr.Context;
using CalcExpr.Expressions.Interfaces;

namespace CalcExpr.Expressions;

/// <summary>
/// Initializes a new instance of the the <see cref="Number"/> class.
/// </summary>
/// <param name="value">The numeric value.</param>
public class Number(double value) : IExpression, IBoolConvertible, IPrefixOperable, IPostfixOperable
{
    public double Value { get; set; } = value;

    public IExpression Evaluate()
        => Evaluate(null!);

    public IExpression Evaluate(ExpressionContext variables)
        => Value switch
        {
            Double.PositiveInfinity => Infinity.POSITIVE,
            Double.NegativeInfinity => Infinity.NEGATIVE,
            Double.NaN => Undefined.UNDEFINED,
            _ => this
        };

    public IExpression StepEvaluate()
        => StepEvaluate(new ExpressionContext());

    public IExpression StepEvaluate(ExpressionContext variables)
        => Evaluate(variables);

    public Constant ToBool()
        => Value switch
        {
            0 => Constant.FALSE,
            _ => Constant.TRUE
        };

    public IExpression PrefixOperate(string identifier, ExpressionContext _)
    {
        return identifier switch
        {
            PrefixOperator.POSITIVE => this,
            PrefixOperator.NEGATIVE => new Number(-Value),
            PrefixOperator.NOT or PrefixOperator.NOT_ALT => Value == 0 ? Constant.TRUE : Constant.FALSE,
            PrefixOperator.SUBFACTORIAL => new Number(FactorialFunctions.Subfactorial(Value)),
            PrefixOperator.PRE_DECREMENT => new Number(Value - 1),
            PrefixOperator.PRE_INCREMENT => new Number(Value + 1),
            _ => Constant.UNDEFINED,
        };
    }

    public IExpression PostfixOperate(string identifier, ExpressionContext _)
    {
        return identifier switch
        {
            PostfixOperator.FACTORIAL => new Number(FactorialFunctions.Factorial(Value)),
            PostfixOperator.PERCENT => new Number(Value / 100),
            PostfixOperator.DOUBLE_FACTORIAL => new Number(FactorialFunctions.DoubleFactorial(Value)),
            PostfixOperator.PRIMORIAL => new Number(FactorialFunctions.Primorial(Value)),
            PostfixOperator.POST_DECREMENT or PostfixOperator.POST_INCREMENT => this,
            _ => Constant.UNDEFINED,
        };
    }

    public override bool Equals(object? obj)
        => obj is not null && obj is Number n && n.Value == Value;

    public override int GetHashCode()
        => Value.GetHashCode();

    public override string ToString()
        => ToString(null);

    public string ToString(string? format)
        => Value.ToString(format);

    public static explicit operator decimal(Number n)
        => (decimal)n.Value;

    public static explicit operator double(Number n)
        => n.Value;

    public static explicit operator float(Number n)
        => (float)n.Value;

    public static explicit operator long(Number n)
        => (long)n.Value;

    public static explicit operator int(Number n)
        => (int)n.Value;

    public static explicit operator short(Number n)
        => (short)n.Value;

    public static explicit operator sbyte(Number n)
        => (sbyte)n.Value;

    public static explicit operator ulong(Number n)
        => (ulong)n.Value;

    public static explicit operator uint(Number n)
        => (uint)n.Value;

    public static explicit operator ushort(Number n)
        => (ushort)n.Value;

    public static explicit operator byte(Number n)
        => (byte)n.Value;

    public static explicit operator Number(decimal n)
        => new Number((double)n);

    public static explicit operator Number(double n)
        => new Number(n);

    public static explicit operator Number(float n)
        => new Number(n);

    public static explicit operator Number(long n)
        => new Number(n);

    public static explicit operator Number(int n)
        => new Number(n);

    public static explicit operator Number(short n)
        => new Number(n);

    public static explicit operator Number(sbyte n)
        => new Number(n);

    public static explicit operator Number(ulong n)
        => new Number(n);

    public static explicit operator Number(uint n)
        => new Number(n);

    public static explicit operator Number(ushort n)
        => new Number(n);

    public static explicit operator Number(byte n)
        => new Number(n);
}
