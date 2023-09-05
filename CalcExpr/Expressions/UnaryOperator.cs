using CalcExpr.Exceptions;

namespace CalcExpr.Expressions;

public class UnaryOperator : IExpression
{
    private static readonly Dictionary<string, Func<IExpression, IExpression>> _prefixes
        = new Dictionary<string, Func<IExpression, IExpression>>
        {
            { "+", Positive },
            { "-", Negative },
            { "~", Not },
            { "¬", Not },
            { "!", Subfactorial },
        };
    private static readonly Dictionary<string, Func<IExpression, IExpression>> _postfixes
        = new Dictionary<string, Func<IExpression, IExpression>>
        {
            { "!", Factorial },
            { "%", Percent },
            { "!!", DoubleFactorial },
            { "#", Primorial },
        };

    private Func<IExpression, IExpression> _operation
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
        => Evaluate(null);

    public IExpression Evaluate(Dictionary<string, IExpression>? variables)
        => _operation(Inside.Evaluate(variables));

    public IExpression StepEvaluate()
        => StepEvaluate(null);

    public IExpression StepEvaluate(Dictionary<string, IExpression>? variables)
        => Inside is Number || (Inside is Constant c && (c == Constant.INFINITY || c == Constant.UNDEFINED)) ||
            (Inside is UnaryOperator uo && uo.Identifier == "-" && uo.Inside == Constant.INFINITY)
            ? _operation(Inside)
            : new UnaryOperator(Identifier, IsPrefix, Inside.StepEvaluate(variables));

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

    private static IExpression Positive(IExpression x)
        => x;

    private static IExpression Negative(IExpression x)
    {
        if (x is Number n)
        {
            return new Number(-n.Value);
        }
        else if (x is Constant c)
        {
            if (c == Constant.INFINITY)
                return new UnaryOperator("-", true, Constant.INFINITY);
            
            // Other constants (except for undefined) should evaluate to a Number.
        }
        else if (x is UnaryOperator uo)
        {
            if (uo.IsPrefix && uo.Identifier == "-")
                return uo.Inside.Clone();
            
            // UnaryOperators should evaluate to a Number unless it's a negative operator with an Inside value of
            // infinity, but if for some reason it doesn't, negative operators will still cancel out.
        }

        // Other IExpressions should evaluate to either a Number, Constant, or UnaryOperator dealt with previously.

        return Constant.UNDEFINED;
    }

    private static IExpression Not(IExpression x)
        => x is Number n && n.Value == 0
            ? new Number(1)
            : x is Constant c && c == Constant.UNDEFINED
                ? Constant.UNDEFINED
                // Any value that is not 0 or undefined should result in 0.
                : new Number(0);

    private static IExpression Subfactorial(IExpression x)
    {
        if (x is Number n && n.Value % 1 == 0)
        {
            if (n.Value == 0)
            {
                return new Number(1);
            }
            else if (n.Value > 0)
            {
                IExpression n_fact = Factorial(n);

                if (n_fact is Number n_fact_n)
                    return new Number((int)(0.5 + n_fact_n.Value / Math.E));
                else if (n_fact is Constant n_fact_c && n_fact_c == Constant.INFINITY)
                    return Constant.INFINITY;

                // Factorial should only return a Number, infinity, or undefined and therefore subfactorial should only
                // result in a Number, infinity, or be undefined.
            }
        }
        else if (x is Constant c && c == Constant.INFINITY)
        {
            return Constant.INFINITY;

            // Other constants (except for undefined) should evaluate to a Number.
        }

        // Other IExpressions should evaluate to either a Number or Constant dealt with previously, or result in an
        // undefined value.

        return Constant.UNDEFINED;
    }

    private static IExpression Factorial(IExpression x)
    {
        if (x is Number n && n.Value % 1 == 0)
        {
            if (n.Value == 0 || n.Value == 1)
            {
                return new Number(1);
            }
            else if (n.Value > 0)
            {
                double output = 1;

                for (int i = 2; i <= n.Value; i++)
                    output *= i;

                return output == Double.PositiveInfinity
                    ? Constant.INFINITY
                    : new Number(output);
            }
        }
        else if (x is Constant c && c == Constant.INFINITY)
        {
            return Constant.INFINITY;
            
            // Other constants (except for undefined) should evaluate to a Number.
        }

        // Other IExpressions should evaluate to either a Number or Constant dealt with previously, or result in an
        // undefined value.

        return Constant.UNDEFINED;
    }

    private static IExpression Percent(IExpression x)
        => x is Number n
            ? new Number(n.Value / 100)
            : x is Constant c && c == Constant.INFINITY
                ? Constant.INFINITY
                // Other constants (except for undefined) should evaluate to a Number.
                : x is UnaryOperator uo && uo.Inside == Constant.INFINITY
                    ? uo.Clone()
                    // Other IExpressions should evaluate to either a Number, Constant, or UnaryOperator dealt with
                    // previously, or result in an undefined value.
                    : Constant.UNDEFINED;

    private static IExpression DoubleFactorial(IExpression x)
    {
        if (x is Number n && n.Value % 1 == 0)
        {
            if (n.Value == 0 || n.Value == 1)
            {
                return new Number(1);
            }
            else if (n.Value > 0)
            {
                if (n.Value % 2 == 0)
                {
                    IExpression n_fact = Factorial(new Number(n.Value / 2));

                    if (n_fact is Number n_fact_n)
                    {
                        double output = Math.Pow(2, n.Value / 2) * n_fact_n.Value;

                        return output == Double.PositiveInfinity
                            ? Constant.INFINITY
                            : new Number(output);
                    }
                    else if (n_fact is Constant n_fact_c && n_fact_c == Constant.INFINITY)
                    {
                        return Constant.INFINITY;
                    }
                }
                else
                {
                    IExpression n_fact = Factorial(n);
                    IExpression n_less_fact = Factorial(new Number((n.Value - 1) / 2));

                    if (n_fact is Number n_fact_n && n_less_fact is Number n_less_fact_n)
                    {
                        return new Number(n_fact_n.Value / (Math.Pow(2, (n.Value - 1) / 2) * n_less_fact_n.Value));
                    }
                    else if (n_fact is Constant n_fact_c && n_fact_c == Constant.INFINITY &&
                        n_less_fact is not Constant)
                    {
                        // If the numerator contains infinity, then the resulting output is infinity.
                        return Constant.INFINITY;
                    }
                    else if (n_less_fact is Constant n_less_fact_c && n_less_fact_c == Constant.INFINITY &&
                        n_fact is not Constant)
                    {
                        // If the denominator contains infinity, then the resulting output is 0.
                        return new Number(0);
                    }
                }
            }
        }
        else if (x is Constant c && c == Constant.INFINITY)
        {
            return Constant.INFINITY;

            // Other constants (except for undefined) should evaluate to a Number.
        }

        // Other IExpressions should evaluate to either a Number or Constant dealt with previously, or result in an
        // undefined value.

        return Constant.UNDEFINED;
    }

    private static IExpression Primorial(IExpression x)
    {
        if (x is Number n && n.Value % 1 == 0)
        {
            if (n.Value == 0)
            {
                return new Number(1);
            }
            else if (n.Value > 0)
            {
                double output = GetNPrimes((int)n.Value).Aggregate((a, b) => a * b);

                return output == Double.PositiveInfinity
                    ? Constant.INFINITY
                    : new Number(output);
            }
        }
        else if (x is Constant c && c == Constant.INFINITY)
        {
            return Constant.INFINITY;

            // Other constants (except for undefined) should evaluate to a Number.
        }

        // Other IExpressions should evaluate to either a Number or Constant dealt with previously, or result in an
        // undefined value.

        return Constant.UNDEFINED;
    }

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
