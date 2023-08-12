using CalcExpr.Exceptions;

namespace CalcExpr.Expressions;

public class UnaryOperator : IExpression
{
    private static readonly Dictionary<string, Func<double, double>> _prefixes
        = new Dictionary<string, Func<double, double>>
        {
            { "+", operand => +operand },
            { "-", operand => -operand },
            { "~", operand => Not(operand) },
            { "¬", operand => Not(operand) },
            { "!", operand => Subfactorial(operand) },
        };
    private static readonly Dictionary<string, Func<double, double>> _postfixes
        = new Dictionary<string, Func<double, double>>
        {
            { "!", Factorial },
            { "%", operand => operand / 100 },
            { "!!", DoubleFactorial },
            { "#", Primorial },
        };

    private Func<double, double> _operation
        => IsPrefix ? _prefixes[Identifier] : _postfixes[Identifier];

    public readonly string Identifier;
    public readonly bool IsPrefix;
    public readonly IExpression Inside;

    /// <summary>
    /// Initializes a new instance of the the <see cref="UnaryOperator"/> class.
    /// </summary>
    /// <param name="op">The identifier for the operator.</param>
    /// <param name="is_prefix">
    /// <see langword="true"/> if the operator is a prefix and <see langword="false"/> if the operator is a postfix.
    /// </param>
    /// <param name="expression">The <see cref="IExpression"/> operand for this operator.</param>
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

    private static double Subfactorial(double n)
        => n == 0
            ? 1
            : n > 0 && n == (int)n
                ? (int)(0.5 + Factorial(n) / Math.E)
                : throw new ArgumentValueException(n);

    private static double Not(double x)
        => x == 0 ? 1 : 0;

    private static double Factorial(double n)
        => n == 0 || n == 1
            ? 1
            : n > 0 && n == (int)n
                ? n * Factorial(n - 1)
                : throw new ArgumentValueException(n);

    private static double DoubleFactorial(double n)
        => n == 0 || n == 1
            ? 1
            : n > 0 && n == (int)n
                ? n % 2 == 0
                    ? Math.Pow(2, n / 2) * Factorial(n / 2)
                    : Factorial(n) / (Math.Pow(2, (n - 1) / 2) * Factorial((n - 1) / 2))
                : throw new ArgumentValueException(n);

    private static double Primorial(double n)
        => n == 0
            ? 1
            : n > 0 && n == (int)n
                ? GetNPrimes((int)n).Aggregate((a, b) => a * b)
                : throw new ArgumentValueException(n);

    private static int[] GetNPrimes(int length)
    {
        List<int> primes = new List<int>() { 2, 3 };

        if (length < 2)
            return primes.ToArray()[..length];

        int i = primes.Last() + 2;

        while (primes.Count < length)
        {
            bool is_prime = true;

            for (int j = 1; j < primes.Count && is_prime; j++)
                if (i % primes[j] == 0)
                    is_prime = false;

            if (is_prime)
                primes.Add(i);

            i += 2;
        }

        return primes.ToArray();
    }
}
