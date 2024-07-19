using CalcExpr.BuiltInFunctions;
using CalcExpr.Context;
using CalcExpr.Expressions.Collections;

namespace CalcExpr.Expressions;

/// <summary>
/// Initializes a new instance of the <see cref="BinaryOperator"/> class.
/// </summary>
/// <param name="op">The identifier for the operator.</param>
/// <param name="left">The <see cref="IExpression"/> left operand for this operator.</param>
/// <param name="right">The <see cref="IExpression"/> right operand for this operator.</param>
public class BinaryOperator(string op, IExpression left, IExpression right) : IExpression
{
    private static readonly Dictionary<string, Func<IExpression, IExpression, ExpressionContext, IExpression>> _operators
        = new Dictionary<string, Func<IExpression, IExpression, ExpressionContext, IExpression>>
        {
            { "+", Add },
            { "-", Subtract },
            { "*", Multiply },
            { "×", Multiply },
            { "/", Divide },
            { "÷", Divide },
            { "^", Exponent },
            { "%", EuclideanModulus },
            { "%%", TruncatedModulus },
            { "//", IntDivide },
            { "&&", (a, b, cxt) => IFunction.ForEach(new Function(LogicalFunctions.And, true), [a, b], cxt) },
            { "∧", (a, b, cxt) => IFunction.ForEach(new Function(LogicalFunctions.And, true), [a, b], cxt) },
            { "||", (a, b, cxt) => IFunction.ForEach(new Function(LogicalFunctions.Or, true), [a, b], cxt) },
            { "∨", (a, b, cxt) => IFunction.ForEach(new Function(LogicalFunctions.Or, true), [a, b], cxt) },
            { "⊕", (a, b, cxt) => IFunction.ForEach(new Function(LogicalFunctions.Xor, true), [a, b], cxt) },
            { "==", IsEqual },
            { "!=", IsNotEqual },
            { "≠", IsNotEqual },
            { "<>", IsNotEqual },
            { "<", IsLessThan },
            { "<=", IsLessThanOrEqualTo },
            { "≤", IsLessThanOrEqualTo },
            { ">", IsGreaterThan },
            { ">=", IsGreaterThanOrEqualTo },
            { "≥", IsGreaterThanOrEqualTo },
        };

    private Func<IExpression, IExpression, ExpressionContext, IExpression> _operation
        => (a, b, vars) => Undefined.UNDEFINED.Equals(a) || Undefined.UNDEFINED.Equals(b)
            ? Undefined.UNDEFINED
            : _operators[Identifier](a, b, vars);

    public readonly string Identifier = op;
    public readonly IExpression Left = left;
    public readonly IExpression Right = right;

    public IExpression Evaluate()
        => Evaluate(new ExpressionContext());

    public IExpression Evaluate(ExpressionContext variables)
        => _operation(Left, Right, variables);

    public IExpression StepEvaluate()
        => StepEvaluate(new ExpressionContext());

    public IExpression StepEvaluate(ExpressionContext variables)
    {
        IExpression l = Left.StepEvaluate(variables);

        if (l.Equals(Left))
        {
            IExpression r = Right.StepEvaluate(variables);

            return r.Equals(Right) ? _operation(l, r, variables) : new BinaryOperator(Identifier, Left, r);
        }

        return new BinaryOperator(Identifier, l, Right);
    }

    public override bool Equals(object? obj)
        => obj is not null && obj is BinaryOperator bin_op && bin_op.Identifier == Identifier &&
            bin_op.Left.Equals(Left) && bin_op.Right.Equals(Right);

    public override int GetHashCode()
        => Identifier.GetHashCode();

    public override string ToString()
        => ToString(null);

    public string ToString(string? format)
        => $"{Left.ToString(format)}{Identifier}{Right.ToString(format)}";

    private static IExpression Add(IExpression a, IExpression b, ExpressionContext context)
    {
        IExpression a_eval = a.Evaluate(context);
        IExpression b_eval = b.Evaluate(context);

        if (a_eval is Number a_num && b_eval is Number b_num)
        {
            return new Number(a_num.Value + b_num.Value).Evaluate();
        }
        else if (Infinity.POSITIVE.Equals(a_eval) || Infinity.POSITIVE.Equals(b_eval) &&
            (a_eval is Number || b_eval is Number ||
            (Infinity.POSITIVE.Equals(a_eval) && Infinity.POSITIVE.Equals(b_eval))))
        { 
            return Infinity.POSITIVE;
        }
        else if ((Infinity.NEGATIVE.Equals(a_eval) || Infinity.NEGATIVE.Equals(b_eval)) &&
            ((a_eval is Number || b_eval is Number) ||
            (Infinity.NEGATIVE.Equals(a_eval) && Infinity.NEGATIVE.Equals(b_eval))))
        {
            return Infinity.NEGATIVE;
        }
        else if (a_eval is IEnumerableExpression a_enum_expr)
        {
            if (b_eval is not IEnumerableExpression)
                return a_enum_expr.Map(e => Add(e, b_eval, context));
            else if (b_eval is IEnumerableExpression b_enum_expr && a_enum_expr.Count() == b_enum_expr.Count())
                return a_enum_expr.Combine(b_enum_expr, (x, y) => Add(x, y, context));
        }
        else if (b_eval is IEnumerableExpression b_enum_expr)
        {
            return b_enum_expr.Map(e => Add(a_eval, e, context));
        }

        return Undefined.UNDEFINED;
    }

    private static IExpression Subtract(IExpression a, IExpression b, ExpressionContext context)
    {
        IExpression a_eval = a.Evaluate(context);
        IExpression b_eval = b.Evaluate(context);

        if (a_eval is Number a_num && b_eval is Number b_num)
        {
            return new Number(a_num.Value - b_num.Value).Evaluate();
        }
        else if ((a_eval is Number || Infinity.POSITIVE.Equals(a_eval)) &&
            (b_eval is Number || Infinity.NEGATIVE.Equals(b_eval)))
        {
            return Infinity.POSITIVE;
        }
        else if ((a_eval is Number || Infinity.NEGATIVE.Equals(a_eval)) &&
            (b_eval is Number || Infinity.POSITIVE.Equals(b_eval)))
        {
            return Infinity.NEGATIVE;
        }
        else if (a_eval is IEnumerableExpression a_enum_expr)
        {
            if (b_eval is not IEnumerableExpression)
                return a_enum_expr.Map(e => Subtract(e, b_eval, context));
            else if (b_eval is IEnumerableExpression b_enum_expr && a_enum_expr.Count() == b_enum_expr.Count())
                return a_enum_expr.Combine(b_enum_expr, (x, y) => Subtract(x, y, context));
        }
        else if (b_eval is IEnumerableExpression b_enum_expr)
        {
            return b_enum_expr.Map(e => Subtract(a_eval, e, context));
        }

        return Undefined.UNDEFINED;
    }

    private static IExpression Multiply(IExpression a, IExpression b, ExpressionContext context)
    {
        IExpression a_eval = a.Evaluate(context);
        IExpression b_eval = b.Evaluate(context);

        if (a_eval is Number num_a && b_eval is Number num_b)
        {
            return new Number(num_a.Value * num_b.Value).Evaluate();
        }
        else if ((a_eval is Number a_num && a_num.Value != 0 && Infinity.POSITIVE.Equals(b_eval)) ||
            (b_eval is Number b_num && b_num.Value != 0 && Infinity.POSITIVE.Equals(a_eval)) ||
            (Infinity.POSITIVE.Equals(a_eval) && Infinity.POSITIVE.Equals(b_eval)) ||
            (Infinity.NEGATIVE.Equals(a_eval) && Infinity.NEGATIVE.Equals(b_eval)))
        {
            return Infinity.POSITIVE;
        }
        else if ((Infinity.NEGATIVE.Equals(a_eval) && (b_eval is Number ||
                Infinity.POSITIVE.Equals(b_eval))) ||
            (Infinity.NEGATIVE.Equals(b_eval) && (a_eval is Number || Infinity.POSITIVE.Equals(a_eval))))
        {
            return Infinity.NEGATIVE;
        }
        else if (a_eval is IEnumerableExpression a_enum_expr)
        {
            if (b_eval is not IEnumerableExpression)
                return a_enum_expr.Map(e => Multiply(e, b_eval, context));
            else if (b_eval is IEnumerableExpression b_enum_expr && a_enum_expr.Count() == b_enum_expr.Count())
                return a_enum_expr.Combine(b_enum_expr, (x, y) => Multiply(x, y, context));
        }
        else if (b_eval is IEnumerableExpression b_enum_expr)
        {
            return b_enum_expr.Map(e => Multiply(a_eval, e, context));
        }

        return Undefined.UNDEFINED;
    }

    private static IExpression Divide(IExpression a, IExpression b, ExpressionContext context)
    {
        IExpression a_eval = a.Evaluate(context);
        IExpression b_eval = b.Evaluate(context);

        if (a_eval is Number a_num && b_eval is Number b_num && b_num.Value != 0)
        {
            return new Number(a_num.Value / b_num.Value);
        }
        else if (a_eval is Number && (Infinity.POSITIVE.Equals(b_eval) || Infinity.NEGATIVE.Equals(b_eval)))
        {
            return new Number(0);
        }
        else if (Infinity.POSITIVE.Equals(a_eval) && b_eval is Number b_n && b_n.Value != 0)
        {
            return Infinity.POSITIVE;
        }
        else if (Infinity.NEGATIVE.Equals(a_eval) && b_eval is Number)
        {
            return Infinity.NEGATIVE;
        }
        else if (a_eval is IEnumerableExpression a_enum_expr)
        {
            if (b_eval is not IEnumerableExpression)
                return a_enum_expr.Map(e => Divide(e, b_eval, context));
            else if (b_eval is IEnumerableExpression b_enum_expr && a_enum_expr.Count() == b_enum_expr.Count())
                return a_enum_expr.Combine(b_enum_expr, (x, y) => Divide(x, y, context));
        }
        else if (b_eval is IEnumerableExpression b_enum_expr)
        {
            return b_enum_expr.Map(e => Divide(a_eval, e, context));
        }

        return Undefined.UNDEFINED;
    }

    private static IExpression Exponent(IExpression a, IExpression b, ExpressionContext context)
    {
        IExpression a_eval = a.Evaluate(context);
        IExpression b_eval = b.Evaluate(context);

        if (a_eval is Number a_num && b_eval is Number b_num && !(a_num.Value == 0 && b_num.Value <= 0))
        {
            return new Number(Math.Pow(a_num.Value, b_num.Value));
        }
        else if (Infinity.POSITIVE.Equals(a_eval) && b_eval is Number b_num_inf)
        {
            if (b_num_inf.Value < 0)
                return new Number(0);
            else if (b_num_inf.Value > 0)
                return Infinity.POSITIVE;
        }
        else if (Infinity.NEGATIVE.Equals(a_eval) && b_eval is Number b_num_neg_inf)
        {
            if (b_num_neg_inf.Value < 0)
                return new Number(0);
            else if (b_num_neg_inf.Value > 0)
                if (b_num_neg_inf.Value % 2 == 0)
                    return Infinity.POSITIVE;
                else if (b_num_neg_inf.Value % 2 == 1)
                    return Infinity.NEGATIVE;
        }
        else if ((Infinity.POSITIVE.Equals(a_eval) || Infinity.NEGATIVE.Equals(a_eval)) &&
            Infinity.POSITIVE.Equals(b_eval))
        {
            return Infinity.POSITIVE;
        }
        else if ((Infinity.NEGATIVE.Equals(b_eval) && ((a_eval is Number a_num_neg_inf &&
            (a_num_neg_inf.Value != 0 &&
                a_num_neg_inf.Value != 1 && a_num_neg_inf.Value != -1)) || Infinity.POSITIVE.Equals(a_eval))) &&
            (a_eval is Number a_n && a_n.Value == 0 && Infinity.POSITIVE.Equals(b_eval)))
        {
            return new Number(0);
        }
        else if (a_eval is IEnumerableExpression a_enum_expr)
        {
            if (b_eval is not IEnumerableExpression)
                return a_enum_expr.Map(e => Exponent(e, b_eval, context));
            else if (b_eval is IEnumerableExpression b_enum_expr && a_enum_expr.Count() == b_enum_expr.Count())
                return a_enum_expr.Combine(b_enum_expr, (x, y) => Exponent(x, y, context));
        }
        else if (b_eval is IEnumerableExpression b_enum_expr)
        {
            return b_enum_expr.Map(e => Exponent(a_eval, e, context));
        }

        return Undefined.UNDEFINED;
    }

    private static IExpression EuclideanModulus(IExpression a, IExpression b, ExpressionContext context)
    {
        IExpression a_eval = a.Evaluate(context);
        IExpression b_eval = b.Evaluate(context);

        if (a_eval is Number a_num && b_eval is Number b_num && b_num.Value != 0)
        {
            return new Number(a_num.Value - Math.Abs(b_num.Value) * Math.Floor(a_num.Value / Math.Abs(b_num.Value)));
        }
        else if (a_eval is IEnumerableExpression a_enum_expr)
        {
            if (b_eval is not IEnumerableExpression)
                return a_enum_expr.Map(e => EuclideanModulus(e, b_eval, context));
            else if (b_eval is IEnumerableExpression b_enum_expr && a_enum_expr.Count() == b_enum_expr.Count())
                return a_enum_expr.Combine(b_enum_expr, (x, y) => EuclideanModulus(x, y, context));
        }
        else if (b_eval is IEnumerableExpression b_enum_expr)
        {
            return b_enum_expr.Map(e => EuclideanModulus(a_eval, e, context));
        }

        return Undefined.UNDEFINED;
    }

    private static IExpression TruncatedModulus(IExpression a, IExpression b, ExpressionContext context)
    {
        IExpression a_eval = a.Evaluate(context);
        IExpression b_eval = b.Evaluate(context);

        if (a_eval is Number a_num && b_eval is Number b_num && b_num.Value != 0)
        {
            return new Number(a_num.Value % b_num.Value);
        }
        else if (a_eval is IEnumerableExpression a_enum_expr)
        {
            if (b_eval is not IEnumerableExpression)
                return a_enum_expr.Map(e => TruncatedModulus(e, b_eval, context));
            else if (b_eval is IEnumerableExpression b_enum_expr && a_enum_expr.Count() == b_enum_expr.Count())
                return a_enum_expr.Combine(b_enum_expr, (x, y) => TruncatedModulus(x, y, context));
        }
        else if (b_eval is IEnumerableExpression b_enum_expr)
        {
            return b_enum_expr.Map(e => TruncatedModulus(a_eval, e, context));
        }

        return Undefined.UNDEFINED;
    }

    private static IExpression IntDivide(IExpression a, IExpression b, ExpressionContext context)
    {
        IExpression a_eval = a.Evaluate(context);
        IExpression b_eval = b.Evaluate(context);

        if (a_eval is Number a_num && b_eval is Number b_num)
        {
            return new Number(Math.Sign(b_num.Value) * Math.Floor(a_num.Value / Math.Abs(b_num.Value)));
        }
        else if (a_eval is IEnumerableExpression a_enum_expr)
        {
            if (b_eval is not IEnumerableExpression)
                return a_enum_expr.Map(e => IntDivide(e, b_eval, context));
            else if (b_eval is IEnumerableExpression b_enum_expr && a_enum_expr.Count() == b_enum_expr.Count())
                return a_enum_expr.Combine(b_enum_expr, (x, y) => IntDivide(x, y, context));
        }
        else if (b_eval is IEnumerableExpression b_enum_expr)
        {
            return b_enum_expr.Map(e => IntDivide(a_eval, e, context));
        }

        return Undefined.UNDEFINED;
    }

    private static IExpression IsEqual(IExpression a, IExpression b, ExpressionContext context)
    {
        IExpression a_eval = a.Evaluate(context);
        IExpression b_eval = b.Evaluate(context);

        if (a_eval is IEnumerableExpression a_enum_expr)
        {
            if (b_eval is not IEnumerableExpression)
                return a_enum_expr.Map(e => IsEqual(e, b_eval, context));
            else if (b_eval is IEnumerableExpression b_enum_expr && a_enum_expr.Count() == b_enum_expr.Count())
                return new Number(a_enum_expr.Equals(b_enum_expr) ? 1 : 0);
        }
        else if (b_eval is IEnumerableExpression b_enum_expr)
        {
            return b_enum_expr.Map(e => IsEqual(a_eval, e, context));
        }
        else if (a_eval is Number a_num)
        {
            return new Number(a_num.Equals(b_eval) ? 1 : 0);
        }
        else if (a_eval is Logical a_logic && b_eval is Logical b_logic)
        {
            return new Number(a_logic.Value == b_logic.Value ? 1 : 0);
        }
        else if (a_eval is Constant || a_eval is Infinity || a_eval is Undefined)
        {
            return new Number(a_eval.Equals(b_eval) ? 1 : 0);
        }
        else if (a_eval is PrefixOperator a_unop)
        {
            return new Number(a_unop.Equals(b) ? 1 : 0);
        }
        else if (a_eval is IEnumerableExpression && b_eval is IEnumerableExpression)
        {
            return new Number(a_eval.Equals(b_eval) ? 1 : 0);
        }

        return Undefined.UNDEFINED;
    }

    private static IExpression IsNotEqual(IExpression a, IExpression b, ExpressionContext context)
    {
        IExpression a_eval = a.Evaluate(context);
        IExpression b_eval = b.Evaluate(context);

        if (a_eval is IEnumerableExpression a_enum_expr)
        {
            if (b_eval is not IEnumerableExpression)
                return a_enum_expr.Map(e => IsNotEqual(e, b_eval, context));
            else if (b_eval is IEnumerableExpression b_enum_expr && a_eval.GetType() == b_eval.GetType() &&
                a_enum_expr.Count() == b_enum_expr.Count())
                return new Number(a_enum_expr.Equals(b_enum_expr) ? 0 : 1);
        }
        else if (b_eval is IEnumerableExpression b_enum_expr)
        {
            return b_enum_expr.Map(e => IsNotEqual(a_eval, e, context));
        }

        return IsEqual(a, b, context) is Number equals
            ? new Number((equals.Value - 1) * -1)
            : Undefined.UNDEFINED;
    }

    private static IExpression IsLessThan(IExpression a, IExpression b, ExpressionContext context)
    {
        IExpression a_eval = a.Evaluate(context);
        IExpression b_eval = b.Evaluate(context);

        if (a_eval is Number a_num && b_eval is Number b_num)
        {
            return new Number(a_num.Value < b_num.Value ? 1 : 0);
        }
        else if (Infinity.POSITIVE.Equals(a_eval) || Infinity.NEGATIVE.Equals(b_eval))
        {
            return new Number(0);
        }
        else if (Infinity.POSITIVE.Equals(b_eval) || Infinity.NEGATIVE.Equals(a_eval))
        {
            return new Number(1);
        }
        else if (a_eval is IEnumerableExpression a_enum_expr)
        {
            if (b_eval is not IEnumerableExpression)
                return a_enum_expr.Map(e => IsLessThan(e, b_eval, context));
            else if (b_eval is IEnumerableExpression b_enum_expr && a_enum_expr.Count() == b_enum_expr.Count())
                return a_enum_expr.Combine(b_enum_expr, (x, y) => IsLessThan(x, y, context));
        }
        else if (b_eval is IEnumerableExpression b_enum_expr)
        {
            return b_enum_expr.Map(e => IsLessThan(a_eval, e, context));
        }

        return Undefined.UNDEFINED;
    }

    private static IExpression IsLessThanOrEqualTo(IExpression a, IExpression b, ExpressionContext context)
    {
        IExpression a_eval = a.Evaluate(context);
        IExpression b_eval = b.Evaluate(context);

        if (a_eval is IEnumerableExpression a_enum_expr)
        {
            if (b_eval is not IEnumerableExpression)
                return a_enum_expr.Map(e => IsLessThanOrEqualTo(e, b_eval, context));
            else if (b_eval is IEnumerableExpression b_enum_expr && a_enum_expr.Count() == b_enum_expr.Count())
                return a_enum_expr.Combine(b_enum_expr, (x, y) => IsLessThanOrEqualTo(x, y, context));
        }
        else if (b_eval is IEnumerableExpression b_enum_expr)
        {
            return b_enum_expr.Map(e => IsLessThanOrEqualTo(a_eval, e, context));
        }
        else if (IsEqual(a, b, context) is Number eq_num)
        {
            if (eq_num.Value == 1)
                return eq_num;
            else
                return IsLessThan(a, b, context);
        }

        return Undefined.UNDEFINED;
    }

    private static IExpression IsGreaterThan(IExpression a, IExpression b, ExpressionContext context)
    {
        IExpression a_eval = a.Evaluate(context);
        IExpression b_eval = b.Evaluate(context);

        if (a_eval is Number a_num && b_eval is Number b_num)
        {
            return new Number(a_num.Value > b_num.Value ? 1 : 0);
        }
        else if (Infinity.POSITIVE.Equals(b_eval) || Infinity.NEGATIVE.Equals(a_eval))
        {
            return new Number(0);
        }
        else if (Infinity.POSITIVE.Equals(a_eval) || Infinity.NEGATIVE.Equals(b_eval))
        {
            return new Number(1);
        }
        else if (a_eval is IEnumerableExpression a_enum_expr)
        {
            if (b_eval is not IEnumerableExpression)
                return a_enum_expr.Map(e => IsGreaterThan(e, b_eval, context));
            else if (b_eval is IEnumerableExpression b_enum_expr && a_enum_expr.Count() == b_enum_expr.Count())
                return a_enum_expr.Combine(b_enum_expr, (x, y) => IsGreaterThan(x, y, context));
        }
        else if (b_eval is IEnumerableExpression b_enum_expr)
        {
            return b_enum_expr.Map(e => IsGreaterThan(a_eval, e, context));
        }

        return Undefined.UNDEFINED;
    }

    private static IExpression IsGreaterThanOrEqualTo(IExpression a, IExpression b, ExpressionContext context)
    {
        IExpression a_eval = a.Evaluate(context);
        IExpression b_eval = b.Evaluate(context);

        if (a_eval is IEnumerableExpression a_enum_expr)
        {
            if (b_eval is not IEnumerableExpression)
                return a_enum_expr.Map(e => IsGreaterThanOrEqualTo(e, b_eval, context));
            else if (b_eval is IEnumerableExpression b_enum_expr && a_enum_expr.Count() == b_enum_expr.Count())
                return a_enum_expr.Combine(b_enum_expr, (x, y) => IsGreaterThanOrEqualTo(x, y, context));
        }
        else if (b_eval is IEnumerableExpression b_enum_expr)
        {
            return b_enum_expr.Map(e => IsGreaterThanOrEqualTo(a_eval, e, context));
        }
        else if (IsEqual(a, b, context) is Number eq_num)
        {
            if (eq_num.Value == 1)
                return eq_num;
            else
                return IsGreaterThan(a, b, context);
        }

        return Undefined.UNDEFINED;
    }
}
