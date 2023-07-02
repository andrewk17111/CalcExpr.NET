using CalcExpr.Parsing;
using System.Text.RegularExpressions;

namespace CalcExpr.Expressions;

public class Number : IExpression
{
    public double Value { get; private set; }

    public Number(double value)
        => Value = value;

    public IExpression Evaluate()
        => Clone();

    public IExpression Simplify()
        => Clone();

    public IExpression StepEvaluate()
        => Clone();

    public IExpression StepSimplify()
        => Clone();

    public IExpression Clone()
        => new Number(Value);

    public static explicit operator decimal(Number n)
        => throw new NotImplementedException();

    public static explicit operator double(Number n)
        => throw new NotImplementedException();

    public static explicit operator float(Number n)
        => throw new NotImplementedException();

    public static explicit operator long(Number n)
        => throw new NotImplementedException();

    public static explicit operator int(Number n)
        => throw new NotImplementedException();

    public static explicit operator short(Number n)
        => throw new NotImplementedException();

    public static explicit operator sbyte(Number n)
        => throw new NotImplementedException();

    public static explicit operator ulong(Number n)
        => throw new NotImplementedException();

    public static explicit operator uint(Number n)
        => throw new NotImplementedException();

    public static explicit operator ushort(Number n)
        => throw new NotImplementedException();

    public static explicit operator byte(Number n)
        => throw new NotImplementedException();

    public static explicit operator Number(decimal n)
        => throw new NotImplementedException();

    public static explicit operator Number(double n)
        => throw new NotImplementedException();

    public static explicit operator Number(float n)
        => throw new NotImplementedException();

    public static explicit operator Number(long n)
        => throw new NotImplementedException();

    public static explicit operator Number(int n)
        => throw new NotImplementedException();

    public static explicit operator Number(short n)
        => throw new NotImplementedException();

    public static explicit operator Number(sbyte n)
        => throw new NotImplementedException();

    public static explicit operator Number(ulong n)
        => throw new NotImplementedException();

    public static explicit operator Number(uint n)
        => throw new NotImplementedException();

    public static explicit operator Number(ushort n)
        => throw new NotImplementedException();

    public static explicit operator Number(byte n)
        => throw new NotImplementedException();
}
