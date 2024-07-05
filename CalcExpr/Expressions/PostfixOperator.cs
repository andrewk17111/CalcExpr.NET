using CalcExpr.BuiltInFunctions;
using CalcExpr.Context;
using CalcExpr.Expressions.Collections;

namespace CalcExpr.Expressions;

/// <summary>
/// Initializes a new instance of the the <see cref="PostfixOperator"/> class.
/// </summary>
/// <param name="op">The identifier for the operator.</param>
/// <param name="expression">The <see cref="IExpression"/> operand for this operator.</param>
public class PostfixOperator(string op, IExpression expression) : IExpression
{
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
        => _postfixes[Identifier];

    public readonly string Identifier = op;
    public readonly IExpression Inside = expression;

    public IExpression Evaluate()
        => Evaluate(new ExpressionContext());

    public IExpression Evaluate(ExpressionContext variables)
        => _operation(Inside, variables);

    public IExpression StepEvaluate()
        => StepEvaluate(new ExpressionContext());

    public IExpression StepEvaluate(ExpressionContext context)
    {
        if (Inside is Number || Constant.INFINITY.Equals(Inside) || Constant.NEGATIVE_INFINITY.Equals(Inside))
            return _operation(Inside, context);

        IExpression enum_eval = Inside.StepEvaluate(context);

        if (!enum_eval.Equals(Inside))
            return new PostfixOperator(Identifier, enum_eval);
        else
            return _operation(enum_eval, context);
    }

    public override string ToString()
        => ToString(null);

    public override bool Equals(object? obj)
        => obj is not null && obj is PostfixOperator uo && uo.Identifier == Identifier &&
            uo.Inside.Equals(Inside);

    public override int GetHashCode()
        => Identifier.GetHashCode();

    public string ToString(string? format)
        => $"{Inside.ToString(format)}{Identifier}";

    private static IExpression Factorial(IExpression x, ExpressionContext context)
    {
        IExpression x_eval = x.Evaluate(context);

        if (x_eval is Number || Constant.INFINITY.Equals(x_eval))
        {
            return FactorialFunctions.Factorial(x_eval);
        }
        else if (x_eval is IEnumerableExpression enum_expr)
        {
            return enum_expr.Map(e => Factorial(e, context));
        }

        return Constant.UNDEFINED;
    }

    private static IExpression Percent(IExpression x, ExpressionContext context)
    {
        IExpression x_eval = x.Evaluate(context);

        if (x_eval is Number n)
        {
            return new Number(n.Value / 100);
        }
        else if (x_eval is Constant c && Constant.INFINITY.Equals(c))
        {
            // Other constants (except for undefined) should evaluate to a Number.
            return x_eval;
        }
        else if (x_eval is PostfixOperator uo && Constant.INFINITY.Equals(uo.Inside))
        {
            return uo;
        }
        else if (x_eval is IEnumerableExpression enum_expr)
        {
            return enum_expr.Map(e => Percent(e, context));
        }

        return Constant.UNDEFINED;
    }

    private static IExpression DoubleFactorial(IExpression x, ExpressionContext context)
    {
        IExpression x_eval = x.Evaluate(context);

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
                    IExpression n_fact = Factorial(new Number(n.Value / 2), context);

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
                    IExpression n_fact = Factorial(n, context);
                    IExpression n_less_fact = Factorial(new Number((n.Value - 1) / 2), context);

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
        else if (x_eval is IEnumerableExpression enum_expr)
        {
            return enum_expr.Map(e => DoubleFactorial(e, context));
        }

        // Other IExpressions should evaluate to either a Number or Constant dealt with previously, or result in an
        // undefined value.

        return Constant.UNDEFINED;
    }

    private static IExpression Primorial(IExpression x, ExpressionContext context)
    {
        IExpression x_eval = x.Evaluate(context);

        if (x_eval is Number || Constant.INFINITY.Equals(x_eval))
        {
            return FactorialFunctions.Primorial(x_eval);
        }
        else if (x_eval is IEnumerableExpression enum_expr)
        {
            return enum_expr.Map(e => Primorial(e, context));
        }

        return Constant.UNDEFINED;
    }

    private static IExpression PostDecrement(IExpression x, ExpressionContext context)
    {
        IExpression x_eval = x.Evaluate(context);

        if (x is IEnumerableExpression enum_expr)
        {
            return enum_expr.Map(e => PostDecrement(e, context));
        }
        else if (x_eval is IEnumerableExpression eval_enum_expr)
        {
            return eval_enum_expr.Map(e => PostDecrement(e, context));
        }

        IExpression new_val = new BinaryOperator("-", x_eval, new Number(1)).Evaluate(context);

        context ??= new ExpressionContext();

        if (x is Variable v)
            context[v.Name] = new_val;

        return x_eval;
    }

    private static IExpression PostIncrement(IExpression x, ExpressionContext context)
    {
        IExpression x_eval = x.Evaluate(context);

        if (x is IEnumerableExpression enum_expr)
        {
            return enum_expr.Map(e => PostIncrement(e, context));
        }
        else if (x_eval is IEnumerableExpression eval_enum_expr)
        {
            return eval_enum_expr.Map(e => PostIncrement(e, context));
        }

        IExpression new_val = new BinaryOperator("+", x_eval, new Number(1)).Evaluate(context);

        context ??= new ExpressionContext();

        if (x is Variable v)
            context[v.Name] = new_val;

        return x_eval;
    }
}
