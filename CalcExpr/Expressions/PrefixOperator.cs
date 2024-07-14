using CalcExpr.BuiltInFunctions;
using CalcExpr.Context;
using CalcExpr.Expressions.Collections;

namespace CalcExpr.Expressions;

/// <summary>
/// Initializes a new instance of the the <see cref="PrefixOperator"/> class.
/// </summary>
/// <param name="op">The identifier for the operator.</param>
/// <param name="expression">The <see cref="IExpression"/> operand for this operator.</param>
public class PrefixOperator(string op, IExpression expression) : IExpression
{
    private static readonly Dictionary<string, Func<IExpression, ExpressionContext, IExpression>> _prefixes
        = new Dictionary<string, Func<IExpression, ExpressionContext, IExpression>>
        {
            { "+", Positive },
            { "-", Negative },
            { "~", (x, cxt) => IFunction.ForEach(new Function(LogicalFunctions.Not, true), [x], cxt) },
            { "¬", (x, cxt) => IFunction.ForEach(new Function(LogicalFunctions.Not, true), [x], cxt) },
            { "!", Subfactorial },
            { "--", PreDecrement },
            { "++", PreIncrement },
        };

    private Func<IExpression, ExpressionContext, IExpression> _operation
        => _prefixes[Identifier];

    public readonly string Identifier = op;
    public readonly IExpression Inside = expression;

    public IExpression Evaluate()
        => Evaluate(new ExpressionContext());

    public IExpression Evaluate(ExpressionContext context)
        => _operation(Inside, context);

    public IExpression StepEvaluate()
        => StepEvaluate(new ExpressionContext());

    public IExpression StepEvaluate(ExpressionContext context)
    {
        if (Inside is Number || Constant.INFINITY.Equals(Inside) || Constant.NEGATIVE_INFINITY.Equals(Inside))
            return _operation(Inside, context);

        IExpression enum_eval = Inside.StepEvaluate(context);

        if (!enum_eval.Equals(Inside))
            return new PrefixOperator(Identifier, enum_eval);
        else
            return _operation(enum_eval, context);
    }

    public override string ToString()
        => ToString(null);

    public override bool Equals(object? obj)
        => obj is not null && obj is PrefixOperator uo && uo.Identifier == Identifier &&
            uo.Inside.Equals(Inside);

    public override int GetHashCode()
        => Identifier.GetHashCode();

    public string ToString(string? format)
        => $"{Identifier}{Inside.ToString(format)}";

    private static IExpression Positive(IExpression x, ExpressionContext context)
        => x.Evaluate(context);

    private static IExpression Negative(IExpression x, ExpressionContext context)
    {
        IExpression x_eval = x.Evaluate(context);

        if (x_eval is Number n)
        {
            return new Number(-n.Value);
        }
        else if (Constant.INFINITY.Equals(x_eval))
        {
            // Other constants (except for undefined) should evaluate to a Number.
            return Constant.NEGATIVE_INFINITY;
        }
        else if (x_eval is PrefixOperator uo && uo.Identifier == "-")
        {
            // If for some reason the inside expression didn't evaluate to a Number or infinity, negative operators will
            // still cancel out.
            return uo.Inside;
        }
        else if (x_eval is IEnumerableExpression enum_expr)
        {
            return enum_expr.Map(e => Negative(e, context));
        }

        // Other IExpressions should evaluate to either a Number, Constant, or UnaryOperator dealt with previously.
        return Undefined.UNDEFINED;
    }

    private static IExpression Subfactorial(IExpression x, ExpressionContext context)
        => IFunction.ForEach(new Function(FactorialFunctions.Subfactorial, true), [x], context);

    private static IExpression PreDecrement(IExpression x, ExpressionContext context)
    {
        IExpression x_eval = x.Evaluate(context);

        if (x is IEnumerableExpression enum_expr)
        {
            return enum_expr.Map(e => PreDecrement(e, context));
        }
        else if (x_eval is IEnumerableExpression eval_enum_expr)
        {
            return eval_enum_expr.Map(e => PreDecrement(e, context));
        }

        IExpression new_val = new BinaryOperator("-", x_eval, new Number(1)).Evaluate(context);

        context ??= new ExpressionContext();

        if (x is Variable v)
            context[v.Name] = new_val;

        return new_val;
    }

    private static IExpression PreIncrement(IExpression x, ExpressionContext context)
    {
        IExpression x_eval = x.Evaluate(context);

        if (x is IEnumerableExpression enum_expr)
        {
            return enum_expr.Map(e => PreIncrement(e, context));
        }
        else if (x_eval is IEnumerableExpression eval_enum_expr)
        {
            return eval_enum_expr.Map(e => PreIncrement(e, context));
        }

        IExpression new_val = new BinaryOperator("+", x_eval, new Number(1)).Evaluate(context);

        context ??= new ExpressionContext();

        if (x is Variable v)
            context[v.Name] = new_val;

        return new_val;
    }
}
