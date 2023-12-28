using CalcExpr.BuiltInFunctions;
using CalcExpr.Context;
using CalcExpr.Exceptions;

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
            { "&&", (a, b, cxt) => LogicalFunctions.And(a.Evaluate(cxt), b.Evaluate(cxt)) },
            { "∧", (a, b, cxt) => LogicalFunctions.And(a.Evaluate(cxt), b.Evaluate(cxt)) },
            { "||", (a, b, cxt) => LogicalFunctions.Or(a.Evaluate(cxt), b.Evaluate(cxt)) },
            { "∨", (a, b, cxt) => LogicalFunctions.Or(a.Evaluate(cxt), b.Evaluate(cxt)) },
            { "⊕", (a, b, cxt) => LogicalFunctions.Xor(a.Evaluate(cxt), b.Evaluate(cxt)) },
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
        => (a, b, vars) => Constant.UNDEFINED.Equals(a) || Constant.UNDEFINED.Equals(b)
            ? Constant.UNDEFINED
            : _operators[Identifier](a, b, vars);

    public readonly string Identifier = op;
    public readonly IExpression Left = left;
    public readonly IExpression Right = right;

    public IExpression Clone()
        => new BinaryOperator(Identifier, Left.Clone(), Right.Clone());

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

    private static IExpression Add(IExpression a, IExpression b, ExpressionContext variables)
    {
        IExpression a_eval = a.Evaluate(variables);
        IExpression b_eval = b.Evaluate(variables);

        if (a_eval is Number a_num && b_eval is Number b_num)
            return new Number(a_num.Value + b_num.Value).Evaluate();
        else if (Constant.INFINITY.Equals(a_eval) || Constant.INFINITY.Equals(b_eval) &&
            (a_eval is Number || b_eval is Number ||
            (Constant.INFINITY.Equals(a_eval) && Constant.INFINITY.Equals(b_eval))))
            return Constant.INFINITY;
        else if ((Constant.NEGATIVE_INFINITY.Equals(a_eval) || Constant.NEGATIVE_INFINITY.Equals(b_eval)) &&
            ((a_eval is Number || b_eval is Number) ||
            (Constant.NEGATIVE_INFINITY.Equals(a_eval) && Constant.NEGATIVE_INFINITY.Equals(b_eval))))
            return new UnaryOperator("-", true, Constant.INFINITY);

        return Constant.UNDEFINED;
    }

    private static IExpression Subtract(IExpression a, IExpression b, ExpressionContext variables)
    {
        IExpression a_eval = a.Evaluate(variables);
        IExpression b_eval = b.Evaluate(variables);

        if (a_eval is Number a_num && b_eval is Number b_num)
            return new Number(a_num.Value - b_num.Value).Evaluate();
        else if ((a_eval is Number || Constant.INFINITY.Equals(a_eval)) &&
            (b_eval is Number || Constant.NEGATIVE_INFINITY.Equals(b_eval)))
            return Constant.INFINITY;
        else if ((a_eval is Number || Constant.NEGATIVE_INFINITY.Equals(a_eval)) &&
            (b_eval is Number || Constant.INFINITY.Equals(b_eval)))
            return new UnaryOperator("-", true, Constant.INFINITY);

        return Constant.UNDEFINED;
    }

    private static IExpression Multiply(IExpression a, IExpression b, ExpressionContext variables)
    {
        IExpression a_eval = a.Evaluate(variables);
        IExpression b_eval = b.Evaluate(variables);

        if (a_eval is Number num_a && b_eval is Number num_b)
            return new Number(num_a.Value * num_b.Value).Evaluate();
        else if ((a_eval is Number a_num && a_num.Value != 0 && Constant.INFINITY.Equals(b_eval)) ||
            (b_eval is Number b_num && b_num.Value != 0 && Constant.INFINITY.Equals(a_eval)) ||
            (Constant.INFINITY.Equals(a_eval) && Constant.INFINITY.Equals(b_eval)) ||
            (Constant.NEGATIVE_INFINITY.Equals(a_eval) && Constant.NEGATIVE_INFINITY.Equals(b_eval)))
            return Constant.INFINITY;
        else if ((Constant.NEGATIVE_INFINITY.Equals(a_eval) && (b_eval is Number ||
                Constant.INFINITY.Equals(b_eval))) ||
            (Constant.NEGATIVE_INFINITY.Equals(b_eval) && (a_eval is Number || Constant.INFINITY.Equals(a_eval))))
            return new UnaryOperator("-", true, Constant.INFINITY);

        return Constant.UNDEFINED;
    }

    private static IExpression Divide(IExpression a, IExpression b, ExpressionContext variables)
    {
        IExpression a_eval = a.Evaluate(variables);
        IExpression b_eval = b.Evaluate(variables);

        if (a_eval is Number a_num && b_eval is Number b_num && b_num.Value != 0)
            return new Number(a_num.Value / b_num.Value);
        else if (a_eval is Number && (Constant.INFINITY.Equals(b_eval) || Constant.NEGATIVE_INFINITY.Equals(b_eval)))
            return new Number(0);
        else if (Constant.INFINITY.Equals(a_eval) && b_eval is Number b_n && b_n.Value != 0)
            return Constant.INFINITY;
        else if (Constant.NEGATIVE_INFINITY.Equals(a_eval) && b_eval is Number)
            return new UnaryOperator("-", true, Constant.INFINITY);

        return Constant.UNDEFINED;
    }

    private static IExpression Exponent(IExpression a, IExpression b, ExpressionContext variables)
    {
        IExpression a_eval = a.Evaluate(variables);
        IExpression b_eval = b.Evaluate(variables);

        if (a_eval is Number a_num && b_eval is Number b_num && !(a_num.Value == 0 && b_num.Value <= 0))
        {
            return new Number(Math.Pow(a_num.Value, b_num.Value));
        }
        else if (Constant.INFINITY.Equals(a_eval) && b_eval is Number b_num_inf)
        {
            if (b_num_inf.Value < 0)
                return new Number(0);
            else if (b_num_inf.Value > 0)
                return Constant.INFINITY;
        }
        else if (Constant.NEGATIVE_INFINITY.Equals(a_eval) && b_eval is Number b_num_neg_inf)
        {
            if (b_num_neg_inf.Value < 0)
                return new Number(0);
            else if (b_num_neg_inf.Value > 0)
                if (b_num_neg_inf.Value % 2 == 0)
                    return Constant.INFINITY;
                else if (b_num_neg_inf.Value % 2 == 1)
                    return Constant.NEGATIVE_INFINITY;
        }
        else if ((Constant.INFINITY.Equals(a_eval) || Constant.NEGATIVE_INFINITY.Equals(a_eval)) &&
            Constant.INFINITY.Equals(b_eval))
        {
            return Constant.INFINITY;
        }
        else if ((Constant.NEGATIVE_INFINITY.Equals(b_eval) && ((a_eval is Number a_num_neg_inf &&
            (a_num_neg_inf.Value != 0 &&
                a_num_neg_inf.Value != 1 && a_num_neg_inf.Value != -1)) || Constant.INFINITY.Equals(a_eval))) &&
            (a_eval is Number a_n && a_n.Value == 0 && Constant.INFINITY.Equals(b_eval)))
        {
            return new Number(0);
        }

        return Constant.UNDEFINED;
    }

    private static IExpression EuclideanModulus(IExpression a, IExpression b, ExpressionContext variables)
        => a.Evaluate(variables) is Number a_num && b.Evaluate(variables) is Number b_num && b_num.Value != 0
            ? new Number(a_num.Value - Math.Abs(b_num.Value) * Math.Floor(a_num.Value / Math.Abs(b_num.Value)))
            : Constant.UNDEFINED;

    private static IExpression TruncatedModulus(IExpression a, IExpression b, ExpressionContext variables)
        => a.Evaluate(variables) is Number a_num && b.Evaluate(variables) is Number b_num && b_num.Value != 0
            ? new Number(a_num.Value % b_num.Value)
            : Constant.UNDEFINED;

    private static IExpression IntDivide(IExpression a, IExpression b, ExpressionContext variables)
        => a.Evaluate(variables) is Number a_num && b.Evaluate(variables) is Number b_num
            ? new Number(Math.Sign(b_num.Value) * Math.Floor(a_num.Value / Math.Abs(b_num.Value)))
            : Constant.UNDEFINED;

    private static IExpression IsEqual(IExpression a, IExpression b, ExpressionContext variables)
    {
        IExpression a_eval = a.Evaluate(variables);
        IExpression b_eval = b.Evaluate(variables);

        return a_eval is Number a_num
            ? new Number(a_num.Equals(b_eval) ? 1 : 0)
            : a_eval is Constant a_const
                ? new Number(a_const.Equals(b_eval) ? 1 : 0)
                : a_eval is UnaryOperator a_unop
                    ? new Number(a_unop.Equals(b) ? 1 : 0)
                    : Constant.UNDEFINED;
    }

    private static IExpression IsNotEqual(IExpression a, IExpression b, ExpressionContext variables)
        => IsEqual(a, b, variables) is Number equals
            ? new Number((equals.Value - 1) * -1)
            : Constant.UNDEFINED;

    private static IExpression IsLessThan(IExpression a, IExpression b, ExpressionContext variables)
    {
        IExpression a_eval = a.Evaluate(variables);
        IExpression b_eval = b.Evaluate(variables);

        if (a_eval is Number a_num && b_eval is Number b_num)
            return new Number(a_num.Value < b_num.Value ? 1 : 0);
        else if (Constant.INFINITY.Equals(a_eval) || Constant.NEGATIVE_INFINITY.Equals(b_eval))
            return new Number(0);
        else if (Constant.INFINITY.Equals(b_eval) || Constant.NEGATIVE_INFINITY.Equals(a_eval))
            return new Number(1);

        return Constant.UNDEFINED;
    }

    private static IExpression IsLessThanOrEqualTo(IExpression a, IExpression b, ExpressionContext variables)
        => IsEqual(a, b, variables) is Number eq_num
            ? eq_num.Value == 1
                ? eq_num.Clone()
                : IsLessThan(a, b, variables).Clone()
            : Constant.UNDEFINED;

    private static IExpression IsGreaterThan(IExpression a, IExpression b, ExpressionContext variables)
    {
        IExpression a_eval = a.Evaluate(variables);
        IExpression b_eval = b.Evaluate(variables);

        if (a_eval is Number a_num && b_eval is Number b_num)
            return new Number(a_num.Value > b_num.Value ? 1 : 0);
        else if (Constant.INFINITY.Equals(b_eval) || Constant.NEGATIVE_INFINITY.Equals(a_eval))
            return new Number(0);
        else if (Constant.INFINITY.Equals(a_eval) || Constant.NEGATIVE_INFINITY.Equals(b_eval))
            return new Number(1);

        return Constant.UNDEFINED;
    }

    private static IExpression IsGreaterThanOrEqualTo(IExpression a, IExpression b, ExpressionContext variables)
        => IsEqual(a, b, variables) is Number eq_num
            ? eq_num.Value == 1
                ? eq_num.Clone()
                : IsGreaterThan(a, b, variables).Clone()
            : Constant.UNDEFINED;
}
