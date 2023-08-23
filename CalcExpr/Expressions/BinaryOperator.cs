namespace CalcExpr.Expressions;

public class BinaryOperator : IExpression
{
    private static readonly Dictionary<string, Func<double, double, double>> _operators
        = new Dictionary<string, Func<double, double, double>>
        {
            { "+", (a, b) => a + b },
            { "-", (a, b) => a - b },
            { "*", (a, b) => a * b },
            { "×", (a, b) => a * b },
            { "/", (a, b) => a / b },
            { "÷", (a, b) => a / b },
            { "^", (a, b) => Math.Pow(a, b) },
            { "%", (a, b) => a - Math.Abs(b) * Math.Floor(a / Math.Abs(b)) },
            { "%%", (a, b) => a % b },
            { "//", (a, b) => Math.Sign(b) * Math.Floor(a / Math.Abs(b)) },
            { "&&", (a, b) => Convert.ToInt32(Convert.ToBoolean(a) && Convert.ToBoolean(b)) },
            { "∧", (a, b) => Convert.ToInt32(Convert.ToBoolean(a) && Convert.ToBoolean(b)) },
            { "||", (a, b) => Convert.ToInt32(Convert.ToBoolean(a) || Convert.ToBoolean(b)) },
            { "∨", (a, b) => Convert.ToInt32(Convert.ToBoolean(a) || Convert.ToBoolean(b)) },
            { "⊕", (a, b) => Xor(a, b) },
            { "==", (a, b) => Convert.ToInt32(a == b) },
            { "!=", (a, b) => Convert.ToInt32(a != b) },
            { "≠", (a, b) => Convert.ToInt32(a != b) },
            { "<>", (a, b) => Convert.ToInt32(a != b) },
            { "<", (a, b) => Convert.ToInt32(a < b) },
            { "<=", (a, b) => Convert.ToInt32(a <= b) },
            { "≤", (a, b) => Convert.ToInt32(a <= b) },
            { ">", (a, b) => Convert.ToInt32(a > b) },
            { ">=", (a, b) => Convert.ToInt32(a >= b) },
            { "≥", (a, b) => Convert.ToInt32(a >= b) },
        };

    private Func<double, double, double> _operation
        => _operators[Identifier];

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
        => new Number(_operation(((Number)Left.Evaluate()).Value, ((Number)Right.Evaluate()).Value));

    public IExpression StepEvaluate()
        => Left is not Number a
            ? new BinaryOperator(Identifier, Left.StepEvaluate(), Right)
            : Right is not Number b 
                ? new BinaryOperator(Identifier, Left, Right.StepEvaluate())
                : new Number(_operation(a.Value, b.Value));

    public override bool Equals(object? obj)
        => obj is not null && obj is BinaryOperator bin_op && bin_op.Identifier == Identifier &&
            bin_op.Left.Equals(Left) && bin_op.Right.Equals(Right);

    public override int GetHashCode()
        => Identifier.GetHashCode();

    public override string ToString()
        => ToString(null);

    public string ToString(string? format)
        => $"{Left.ToString(format)}{Identifier}{Right.ToString(format)}";

    private static double Xor(double a, double b)
    {
        bool bool_a = Convert.ToBoolean(a);
        bool bool_b = Convert.ToBoolean(b);

        return Convert.ToInt32((bool_a || bool_b) && !(bool_a && bool_b));
    }
}
