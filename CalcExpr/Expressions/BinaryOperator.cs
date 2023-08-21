namespace CalcExpr.Expressions;

public class BinaryOperator : IExpression
{
    public readonly string Identifier;
    public readonly IExpression Left;
    public readonly IExpression Right;

    public BinaryOperator(string op, IExpression left, IExpression right)
    {
        Identifier = op;
        Left = left;
        Right = right;
    }

    public IExpression Clone()
        => new BinaryOperator(Identifier, Left.Clone(), Right.Clone());

    public IExpression Evaluate()
        => throw new NotImplementedException();

    public IExpression StepEvaluate()
        => throw new NotImplementedException();

    public override bool Equals(object? obj)
        => obj is not null && obj is BinaryOperator bin_op && bin_op.Identifier == Identifier &&
            bin_op.Left.Equals(Left) && bin_op.Right.Equals(Right);

    public override int GetHashCode()
        => Identifier.GetHashCode();

    public override string ToString()
        => ToString(null);

    public string ToString(string? format)
        => $"{Left.ToString(format)}{Identifier}{Right.ToString(format)}";
}
