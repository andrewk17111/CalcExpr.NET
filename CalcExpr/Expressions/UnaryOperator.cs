using CalcExpr.Exceptions;

namespace CalcExpr.Expressions;

public class UnaryOperator : IExpression
{
    private static readonly Dictionary<string, Func<double, double>> _prefixes
        = new Dictionary<string, Func<double, double>>
        {
            { "+", operand => +operand },
            { "-", operand => -operand },
            { "!", operand => Not(operand) },
            { "~", operand => Not(operand) },
            { "¬", operand => Not(operand) },
        };
    private static readonly Dictionary<string, Func<double, double>> _postfixes
        = new Dictionary<string, Func<double, double>>
        {
            { "!", Factorial },
            { "%", operand => operand / 100 }
        };

    private Func<double, double> _operation
        => IsPrefix ? _prefixes[Identifier] : _postfixes[Identifier];

    public readonly string Identifier;
    public readonly bool IsPrefix;
    public readonly IExpression Inside;

    public UnaryOperator(string op, bool is_prefix, IExpression expression)
    {
        Identifier = op;
        IsPrefix = is_prefix;
        Inside = expression;
    }

    public IExpression Clone()
        => new UnaryOperator(Identifier, IsPrefix, Inside.Clone());

    public IExpression Evaluate()
        => new Number(_operation(((Number)Inside.Evaluate()).Value));

    public IExpression StepEvaluate()
        => Inside is Number n
            ? new Number(_operation(n.Value))
            : new UnaryOperator(Identifier, IsPrefix, Inside.StepEvaluate());

    public override string ToString()
        => ToString(null);

    public override bool Equals(object? obj)
        => obj is not null && obj is UnaryOperator uo && uo.IsPrefix == IsPrefix && uo.Identifier == Identifier &&
            uo.Inside.Equals(Inside);

    public override int GetHashCode()
        => HashCode.Combine(Identifier, IsPrefix);

    public string ToString(string? format)
        => IsPrefix
            ? $"{Identifier}{Inside.ToString(format)}"
            : $"{Inside.ToString(format)}{Identifier}";

    private static double Not(double x)
        => x == 0 ? 1 : 0;

    private static double Factorial(double x)
        => x == 0
            ? 1
            : x > 0 && x == (int)x
                ? x * Factorial(x - 1)
                : throw new ArgumentValueException(x);
}
