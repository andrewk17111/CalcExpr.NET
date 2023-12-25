using CalcExpr.Context;

namespace CalcExpr.Expressions;

/// <summary>
/// Initializes a new instance of the the <see cref="UnaryOperator"/> class.
/// </summary>
/// <param name="op">The identifier for the operator.</param>
/// <param name="is_prefix">
/// <see langword="true"/> if the operator is a prefix and <see langword="false"/> if the operator is a postfix.
/// </param>
/// <param name="expression">The <see cref="IExpression"/> operand for this operator.</param>
public class UnaryOperator(string op, bool is_prefix, IExpression expression) : IExpression
{
    private static readonly Dictionary<string, Func<IExpression, ExpressionContext, IExpression>> _prefixes
        = new Dictionary<string, Func<IExpression, ExpressionContext, IExpression>>
        {
            { "+", Positive },
            { "-", Negative },
            { "~", Not },
            { "¬", Not },
            { "!", Subfactorial },
            { "--", PreDecrement },
            { "++", PreIncrement },
        };
    private static readonly Dictionary<string, Func<IExpression, ExpressionContext, IExpression>> _postfixes
        = new Dictionary<string, Func<IExpression, ExpressionContext, IExpression>>
        {
            { "!", Factorial },
            { "%", Percent },
            { "!!", DoubleFactorial },
            { "#", Primorial },
            { "--", PostDecrement },
            { "++", PostIncrement },
        };

    private Func<IExpression, ExpressionContext, IExpression> _operation
        => IsPrefix ? _prefixes[Identifier] : _postfixes[Identifier];

    public readonly string Identifier = op;
    public readonly bool IsPrefix = is_prefix;
    public readonly IExpression Inside = expression;

    public IExpression Clone()
        => new UnaryOperator(Identifier, IsPrefix, Inside.Clone());

    public IExpression Evaluate()
        => Evaluate(new ExpressionContext());

    public IExpression Evaluate(ExpressionContext variables)
        => _operation(Inside, variables);

    public IExpression StepEvaluate()
        => StepEvaluate(new ExpressionContext());

    public IExpression StepEvaluate(ExpressionContext variables)
        => Inside is Number || Constant.INFINITY.Equals(Inside) || Constant.NEGATIVE_INFINITY.Equals(Inside)
            ? _operation(Inside, variables)
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

    private static IExpression Positive(IExpression x, ExpressionContext variables)
        => x.Evaluate(variables);

    private static IExpression Negative(IExpression x, ExpressionContext variables)
    {
        IExpression x_eval = x.Evaluate(variables);

        if (x_eval is Number n)
        {
            return new Number(-n.Value);
        }
        else if (Constant.INFINITY.Equals(x_eval))
        {
            return Constant.NEGATIVE_INFINITY;
            
            // Other constants (except for undefined) should evaluate to a Number.
        }
        else if (x_eval is UnaryOperator uo && uo.IsPrefix && uo.Identifier == "-")
        {
            return uo.Inside.Clone();
            
            // If for some reason the inside expression didn't evaluate to a Number or infinity, negative operators will
            // still cancel out.
        }

        // Other IExpressions should evaluate to either a Number, Constant, or UnaryOperator dealt with previously.

        return Constant.UNDEFINED;
    }

    private static IExpression Not(IExpression x, ExpressionContext variables)
    {
        IExpression x_eval = x.Evaluate(variables);

        return x_eval is Number n && n.Value == 0
            ? new Number(1)
            : x_eval is Constant c && Constant.UNDEFINED.Equals(c)
                ? Constant.UNDEFINED
                // Any value that is not 0 or undefined should result in 0.
                : new Number(0);
    }

    private static IExpression Subfactorial(IExpression x, ExpressionContext variables)
    {
        IExpression x_eval = x.Evaluate(variables);

        if (x_eval is Number n && n.Value % 1 == 0)
        {
            if (n.Value == 0)
            {
                return new Number(1);
            }
            else if (n.Value > 0)
            {
                IExpression n_fact = Factorial(n, variables);

                if (n_fact is Number n_fact_n)
                    return new Number((int)(0.5 + n_fact_n.Value / Math.E));
                else if (n_fact is Constant n_fact_c && Constant.INFINITY.Equals(n_fact_c))
                    return Constant.INFINITY;

                // Factorial should only return a Number, infinity, or undefined and therefore subfactorial should only
                // result in a Number, infinity, or be undefined.
            }
        }
        else if (x_eval is Constant c && Constant.INFINITY.Equals(c))
        {
            return Constant.INFINITY;

            // Other constants (except for undefined) should evaluate to a Number.
        }

        // Other IExpressions should evaluate to either a Number or Constant dealt with previously, or result in an
        // undefined value.

        return Constant.UNDEFINED;
    }

    private static IExpression Factorial(IExpression x, ExpressionContext variables)
    {
        IExpression x_eval = x.Evaluate(variables);

        if (x_eval is Number n && n.Value % 1 == 0)
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
        else if (x_eval is Constant c && Constant.INFINITY.Equals(c))
        {
            return Constant.INFINITY;
            
            // Other constants (except for undefined) should evaluate to a Number.
        }

        // Other IExpressions should evaluate to either a Number or Constant dealt with previously, or result in an
        // undefined value.

        return Constant.UNDEFINED;
    }

    private static IExpression Percent(IExpression x, ExpressionContext variables)
    {
        IExpression x_eval = x.Evaluate(variables);

        return x_eval is Number n
            ? new Number(n.Value / 100)
            : x_eval is Constant c && Constant.INFINITY.Equals(c)
                ? Constant.INFINITY
                // Other constants (except for undefined) should evaluate to a Number.
                : x_eval is UnaryOperator uo && Constant.INFINITY.Equals(uo.Inside)
                    ? uo.Clone()
                    // Other IExpressions should evaluate to either a Number, Constant, or UnaryOperator dealt with
                    // previously, or result in an undefined value.
                    : Constant.UNDEFINED;
    }

    private static IExpression DoubleFactorial(IExpression x, ExpressionContext variables)
    {
        IExpression x_eval = x.Evaluate(variables);

        if (x_eval is Number n && n.Value % 1 == 0)
        {
            if (n.Value == 0 || n.Value == 1)
            {
                return new Number(1);
            }
            else if (n.Value > 0)
            {
                if (n.Value % 2 == 0)
                {
                    IExpression n_fact = Factorial(new Number(n.Value / 2), variables);

                    if (n_fact is Number n_fact_n)
                    {
                        double output = Math.Pow(2, n.Value / 2) * n_fact_n.Value;

                        return output == Double.PositiveInfinity
                            ? Constant.INFINITY
                            : new Number(output);
                    }
                    else if (n_fact is Constant n_fact_c && Constant.INFINITY.Equals(n_fact_c))
                    {
                        return Constant.INFINITY;
                    }
                }
                else
                {
                    IExpression n_fact = Factorial(n, variables);
                    IExpression n_less_fact = Factorial(new Number((n.Value - 1) / 2), variables);

                    if (n_fact is Number n_fact_n && n_less_fact is Number n_less_fact_n)
                    {
                        return new Number(n_fact_n.Value / (Math.Pow(2, (n.Value - 1) / 2) * n_less_fact_n.Value));
                    }
                    else if (n_fact is Constant n_fact_c && Constant.INFINITY.Equals(n_fact_c) &&
                        n_less_fact is not Constant)
                    {
                        // If the numerator contains infinity, then the resulting output is infinity.
                        return Constant.INFINITY;
                    }
                    else if (n_less_fact is Constant n_less_fact_c && Constant.INFINITY.Equals(n_less_fact_c) &&
                        n_fact is not Constant)
                    {
                        // If the denominator contains infinity, then the resulting output is 0.
                        return new Number(0);
                    }
                }
            }
        }
        else if (x_eval is Constant c && Constant.INFINITY.Equals(c))
        {
            return Constant.INFINITY;

            // Other constants (except for undefined) should evaluate to a Number.
        }

        // Other IExpressions should evaluate to either a Number or Constant dealt with previously, or result in an
        // undefined value.

        return Constant.UNDEFINED;
    }

    private static IExpression Primorial(IExpression x, ExpressionContext variables)
    {
        IExpression x_eval = x.Evaluate(variables);

        if (x_eval is Number n && n.Value % 1 == 0)
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
        else if (x_eval is Constant c && Constant.INFINITY.Equals(c))
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
        List<int> primes = [2, 3];

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

        return [.. primes];
    }

    private static IExpression PreDecrement(IExpression x, ExpressionContext variables)
    {
        IExpression x_eval = x.Evaluate(variables);
        IExpression new_val = new BinaryOperator("-", x_eval, new Number(1)).Evaluate(variables);

        variables ??= new ExpressionContext();

        if (x is Variable v)
            variables[v.Name] = new_val;

        return new_val;
    }

    private static IExpression PreIncrement(IExpression x, ExpressionContext variables)
    {
        IExpression x_eval = x.Evaluate(variables);
        IExpression new_val = new BinaryOperator("+", x_eval, new Number(1)).Evaluate(variables);

        variables ??= new ExpressionContext();

        if (x is Variable v)
            variables[v.Name] = new_val;

        return new_val;
    }

    private static IExpression PostDecrement(IExpression x, ExpressionContext variables)
    {
        IExpression x_eval = x.Evaluate(variables);
        IExpression new_val = new BinaryOperator("-", x_eval, new Number(1)).Evaluate(variables);

        variables ??= new ExpressionContext();

        if (x is Variable v)
            variables[v.Name] = new_val;

        return x_eval;
    }

    private static IExpression PostIncrement(IExpression x, ExpressionContext variables)
    {
        IExpression x_eval = x.Evaluate(variables);
        IExpression new_val = new BinaryOperator("+", x_eval, new Number(1)).Evaluate(variables);

        variables ??= new ExpressionContext();

        if (x is Variable v)
            variables[v.Name] = new_val;

        return x_eval;
    }
}
