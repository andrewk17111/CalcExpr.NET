namespace CalcExpr.Expressions;

public class UnaryOperator : IExpression
{
    public readonly string Identifier;
    public readonly bool IsPrefix;
    public readonly IExpression Inside;

    public UnaryOperator(string op, bool is_prefix, IExpression expression)
    {
        throw new NotImplementedException();
    }

    public IExpression Clone()
        => throw new NotImplementedException();

    public IExpression Evaluate()
        => throw new NotImplementedException();

    public IExpression Simplify()
        => throw new NotImplementedException();

    public IExpression StepEvaluate()
        => throw new NotImplementedException();

    public IExpression StepSimplify()
        => throw new NotImplementedException();

    public override string ToString()
        => throw new NotImplementedException();

    public string ToString(string? format)
        => throw new NotImplementedException();
}
