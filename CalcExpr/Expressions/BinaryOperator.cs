namespace CalcExpr.Expressions;

public class BinaryOperator : IExpression
{
    private static readonly Dictionary<string, Func<IExpression, IExpression, IExpression>> _operators
        = new Dictionary<string, Func<IExpression, IExpression, IExpression>>
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
            { "&&", And },
            { "∧", And },
            { "||", Or },
            { "∨", Or },
            { "⊕", Xor },
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

    private Func<IExpression, IExpression, IExpression> _operation
        => (a, b) => Constant.UNDEFINED.Equals(a) || Constant.UNDEFINED.Equals(b)
            ? Constant.UNDEFINED
            : _operators[Identifier](a, b);

    public readonly string Identifier;
    public readonly IExpression Left;
    public readonly IExpression Right;

    /// <summary>
    /// Initializes a new instance of the <see cref="BinaryOperator"/> class.
    /// </summary>
    /// <param name="op">The identifier for the operator.</param>
    /// <param name="left">The <see cref="IExpression"/> left operand for this operator.</param>
    /// <param name="right">The <see cref="IExpression"/> right operand for this operator.</param>
    public BinaryOperator(string op, IExpression left, IExpression right)
    {
        Identifier = op;
        Left = left;
        Right = right;
    }

    public IExpression Clone()
        => new BinaryOperator(Identifier, Left.Clone(), Right.Clone());

    public IExpression Evaluate()
        => Evaluate(null);

    public IExpression Evaluate(Dictionary<string, IExpression>? variables)
        => _operation(Left.Evaluate(variables), Right.Evaluate(variables));

    public IExpression StepEvaluate()
        => StepEvaluate(null);

    public IExpression StepEvaluate(Dictionary<string, IExpression>? variables)
        => Left is not Number a
            ? new BinaryOperator(Identifier, Left.StepEvaluate(variables), Right)
            : Right is not Number b
                ? new BinaryOperator(Identifier, Left, Right.StepEvaluate(variables))
                : _operation(a, b);

    public override bool Equals(object? obj)
        => obj is not null && obj is BinaryOperator bin_op && bin_op.Identifier == Identifier &&
            bin_op.Left.Equals(Left) && bin_op.Right.Equals(Right);

    public override int GetHashCode()
        => Identifier.GetHashCode();

    public override string ToString()
        => ToString(null);

    public string ToString(string? format)
        => $"{Left.ToString(format)}{Identifier}{Right.ToString(format)}";

    private static IExpression Add(IExpression a, IExpression b)
    {
        if (a is Number a_num && b is Number b_num)
            return new Number(a_num.Value + b_num.Value).Evaluate();
        else if (Constant.INFINITY.Equals(a) || Constant.INFINITY.Equals(b) &&
            (a is Number || b is Number || (Constant.INFINITY.Equals(a) && Constant.INFINITY.Equals(b))))
                return Constant.INFINITY;
        else if ((Constant.NEGATIVE_INFINITY.Equals(a) || Constant.NEGATIVE_INFINITY.Equals(b)) && ((a is Number || b is Number) || (Constant.NEGATIVE_INFINITY.Equals(a) && Constant.NEGATIVE_INFINITY.Equals(b))))
            return new UnaryOperator("-", true, Constant.INFINITY);

        return Constant.UNDEFINED;
    }

    private static IExpression Subtract(IExpression a, IExpression b)
    {
        if (a is Number a_num && b is Number b_num)
            return new Number(a_num.Value - b_num.Value).Evaluate();
        else if ((a is Number || Constant.INFINITY.Equals(a)) && (b is Number || Constant.NEGATIVE_INFINITY.Equals(b)))
            return Constant.INFINITY;
        else if ((a is Number || Constant.NEGATIVE_INFINITY.Equals(a)) && (b is Number || Constant.INFINITY.Equals(b)))
            return new UnaryOperator("-", true, Constant.INFINITY);

        return Constant.UNDEFINED;
    }

    private static IExpression Multiply(IExpression a, IExpression b)
    {
        if (a is Number && b is Number)
            return new Number(((Number)a).Value * ((Number)b).Value).Evaluate();
        else if ((a is Number a_num && a_num.Value != 0 && Constant.INFINITY.Equals(b)) ||
            (b is Number b_num && b_num.Value != 0 && Constant.INFINITY.Equals(a)) ||
            (Constant.INFINITY.Equals(a) && Constant.INFINITY.Equals(b)) || (Constant.NEGATIVE_INFINITY.Equals(a) && Constant.NEGATIVE_INFINITY.Equals(b)))
            return Constant.INFINITY;
        else if ((Constant.NEGATIVE_INFINITY.Equals(a) && (b is Number || Constant.INFINITY.Equals(b))) ||
            (Constant.NEGATIVE_INFINITY.Equals(b) && (a is Number || Constant.INFINITY.Equals(a))))
            return new UnaryOperator("-", true, Constant.INFINITY);

        return Constant.UNDEFINED;
    }

    private static IExpression Divide(IExpression a, IExpression b)
    {
        if (a is Number a_num && b is Number b_num && b_num.Value != 0)
            return new Number(a_num.Value / b_num.Value);
        else if (a is Number && (Constant.INFINITY.Equals(b) || Constant.NEGATIVE_INFINITY.Equals(b)))
            return new Number(0);
        else if (Constant.INFINITY.Equals(a) && b is Number b_n && b_n.Value != 0)
            return Constant.INFINITY;
        else if (Constant.NEGATIVE_INFINITY.Equals(a) && b is Number)
            return new UnaryOperator("-", true, Constant.INFINITY);

        return Constant.UNDEFINED;
    }

    private static IExpression Exponent(IExpression a, IExpression b)
    {
        if (a is Number a_num && b is Number b_num && !(a_num.Value == 0 && b_num.Value <= 0))
        {
            return new Number(Math.Pow(a_num.Value, b_num.Value));
        }
        else if (Constant.INFINITY.Equals(a) && b is Number b_num_inf)
        {
            if (b_num_inf.Value < 0)
                return new Number(0);
            else if (b_num_inf.Value > 0)
                return Constant.INFINITY;
        }
        else if (Constant.NEGATIVE_INFINITY.Equals(a) && b is Number b_num_neg_inf)
        {
            if (b_num_neg_inf.Value < 0)
                return new Number(0);
            else if (b_num_neg_inf.Value > 0)
                if (b_num_neg_inf.Value % 2 == 0)
                    return Constant.INFINITY;
                else if (b_num_neg_inf.Value % 2 == 1)
                    return Constant.NEGATIVE_INFINITY;
        }
        else if ((Constant.INFINITY.Equals(a) || Constant.NEGATIVE_INFINITY.Equals(a)) && Constant.INFINITY.Equals(b))
        {
            return Constant.INFINITY;
        }
        else if ((Constant.NEGATIVE_INFINITY.Equals(b) && ((a is Number a_num_neg_inf && (a_num_neg_inf.Value != 0 &&
                a_num_neg_inf.Value != 1 && a_num_neg_inf.Value != -1)) || Constant.INFINITY.Equals(a))) &&
            (a is Number a_n && a_n.Value == 0 && Constant.INFINITY.Equals(b)))
        {
            return new Number(0);
        }

        return Constant.UNDEFINED;
    }

    private static IExpression EuclideanModulus(IExpression a, IExpression b)
        => a is Number a_num && b is Number b_num && b_num.Value != 0
            ? new Number(a_num.Value - Math.Abs(b_num.Value) * Math.Floor(a_num.Value / Math.Abs(b_num.Value)))
            : Constant.UNDEFINED;

    private static IExpression TruncatedModulus(IExpression a, IExpression b)
        => a is Number a_num && b is Number b_num && b_num.Value != 0
            ? new Number(a_num.Value % b_num.Value)
            : Constant.UNDEFINED;

    private static IExpression IntDivide(IExpression a, IExpression b)
        => a is Number a_num && b is Number b_num
            ? new Number(Math.Sign(b_num.Value) * Math.Floor(a_num.Value / Math.Abs(b_num.Value)))
            : Constant.UNDEFINED;

    private static IExpression And(IExpression a, IExpression b)
        => (a is Number a_num && a_num.Value == 0) || (b is Number b_num && b_num.Value == 0)
            ? new Number(0)
            : new Number(1);

    private static IExpression Or(IExpression a, IExpression b)
        => a is Number a_num && a_num.Value == 0 && b is Number b_num && b_num.Value == 0
            ? new Number(0)
            : new Number(1);

    private static IExpression Xor(IExpression a, IExpression b)
    {
        bool a_bool = a is Number a_num && a_num.Value != 0;
        bool b_bool = b is Number b_num && b_num.Value != 0;

        return new Number((a_bool || b_bool) && !(a_bool && b_bool) ? 1 : 0);
    }

    private static IExpression IsEqual(IExpression a, IExpression b)
        => a is Number a_num
            ? new Number(a_num.Equals(b) ? 1 : 0)
            : a is Constant a_const
                ? new Number(a_const.Equals(b) ? 1 : 0)
                : a is UnaryOperator a_unop
                    ? new Number(a_unop.Equals(b) ? 1 : 0)
                    : Constant.UNDEFINED;

    private static IExpression IsNotEqual(IExpression a, IExpression b)
        => IsEqual(a, b) is Number equals
            ? new Number((equals.Value - 1) * -1)
            : Constant.UNDEFINED;

    private static IExpression IsLessThan(IExpression a, IExpression b)
    {
        if (a is Number a_num && b is Number b_num)
            return new Number(a_num.Value < b_num.Value ? 1 : 0);
        else if (Constant.INFINITY.Equals(a) || Constant.NEGATIVE_INFINITY.Equals(b))
            return new Number(0);
        else if (Constant.INFINITY.Equals(b) || Constant.NEGATIVE_INFINITY.Equals(a))
            return new Number(1);

        return Constant.UNDEFINED;
    }

    private static IExpression IsLessThanOrEqualTo(IExpression a, IExpression b)
        => IsEqual(a, b) is Number eq_num
            ? eq_num.Value == 1
                ? eq_num.Clone()
                : IsLessThan(a, b).Clone()
            : Constant.UNDEFINED;

    private static IExpression IsGreaterThan(IExpression a, IExpression b)
    {
        if (a is Number a_num && b is Number b_num)
            return new Number(a_num.Value > b_num.Value ? 1 : 0);
        else if (Constant.INFINITY.Equals(b) || Constant.NEGATIVE_INFINITY.Equals(a))
            return new Number(0);
        else if (Constant.INFINITY.Equals(a) || Constant.NEGATIVE_INFINITY.Equals(b))
            return new Number(1);

        return Constant.UNDEFINED;
    }

    private static IExpression IsGreaterThanOrEqualTo(IExpression a, IExpression b)
        => IsEqual(a, b) is Number eq_num
            ? eq_num.Value == 1
                ? eq_num.Clone()
                : IsGreaterThan(a, b).Clone()
            : Constant.UNDEFINED;
}
