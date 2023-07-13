namespace CalcExpr.Expressions;

public class UnaryOperator : IExpression
{
    public readonly string Identifier;
    public readonly bool IsPrefix;
    public readonly IExpression Inside;

    public UnaryOperator(string op, bool is_prefix, IExpression expression)
    {
        Identifier = op;
        IsPrefix = is_prefix;
        Inside = expression;
    }

    public IExpression Clone()
        => new UnaryOperator(Identifier, IsPrefix, Inside.Clone());

    public IExpression Evaluate()
        => throw new NotImplementedException();

    public IExpression Simplify()
        => throw new NotImplementedException();

    public IExpression StepEvaluate()
        => throw new NotImplementedException();

    public IExpression StepSimplify()
        => throw new NotImplementedException();

    public override string ToString()
        => ToString(null);

    public string ToString(string? format)
        => IsPrefix
            ? $"{Identifier}{Inside.ToString(format)}"
            : $"{Inside.ToString(format)}{Identifier}";
}
