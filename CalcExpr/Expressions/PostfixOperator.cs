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
        => IFunction.ForEach(new Function(FactorialFunctions.Factorial, true), [x], context);

    private static IExpression Percent(IExpression x, ExpressionContext context)
    {
        IExpression x_eval = x.Evaluate(context);

        if (x_eval is Number n)
        {
            return new Number(n.Value / 100);
        }
        else if (x_eval is Constant c && Constant.INFINITY.Equals(c))
        {
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
        => IFunction.ForEach(new Function(FactorialFunctions.DoubleFactorial, true), [x], context);

    private static IExpression Primorial(IExpression x, ExpressionContext context)
        => IFunction.ForEach(new Function(FactorialFunctions.Primorial, true), [x], context);

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
