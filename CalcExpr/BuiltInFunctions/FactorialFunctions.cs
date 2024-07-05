using CalcExpr.Attributes;
using CalcExpr.Context;
using CalcExpr.Expressions;
using CalcExpr.Expressions.Collections;

namespace CalcExpr.BuiltInFunctions;

public static class FactorialFunctions
{
    [BuiltInFunction("factorial")]
    [Elementwise]
    public static IExpression Factorial(IExpression x)
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
        else if (x is Constant c && Constant.INFINITY.Equals(c))
        {
            return Constant.INFINITY;
        }

        return Constant.UNDEFINED;
    }

    [BuiltInFunction("subfactorial")]
    [Elementwise]
    public static IExpression Subfactorial(IExpression x)
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
                else if (Constant.INFINITY.Equals(n_fact))
                    return Constant.INFINITY;
            }
        }
        else if (Constant.INFINITY.Equals(x))
        {
            return Constant.INFINITY;
        }

        return Constant.UNDEFINED;
    }

    [BuiltInFunction("primorial")]
    [Elementwise]
    public static IExpression Primorial(IExpression x)
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
        else if (Constant.INFINITY.Equals(x))
        {
            return Constant.INFINITY;
        }

        return Constant.UNDEFINED;
    }

    private static int[] GetNPrimes(int length)
    {
        List<int> primes = [2, 3];

        if (length < 2)
            return [.. primes[..length]];

        for (int i = primes.Last() + 2; primes.Count < length; i += 2)
        {
            bool is_prime = true;

            for (int j = 1; j < primes.Count && is_prime; j++)
                if (i % primes[j] == 0)
                    is_prime = false;

            if (is_prime)
                primes.Add(i);
        }

        return [.. primes];
    }

    [BuiltInFunction("double_factorial")]
    [Elementwise]
    public static IExpression DoubleFactorial(IExpression x)
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
                    else if (n_fact is Constant n_fact_c && Constant.INFINITY.Equals(n_fact_c))
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
        else if (Constant.INFINITY.Equals(x))
        {
            return Constant.INFINITY;
        }

        return Constant.UNDEFINED;
    }
}
