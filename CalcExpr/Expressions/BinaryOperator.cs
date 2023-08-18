namespace CalcExpr.Expressions;

public class BinaryOperator : IExpression
{
    public string Identifier
        => throw new NotImplementedException();

    public IExpression Left
        => throw new NotImplementedException();

    public IExpression Right
        => throw new NotImplementedException();

    public BinaryOperator(string op, IExpression left, IExpression right)
        => throw new NotImplementedException();

    public IExpression Clone()
        => throw new NotImplementedException();

    public IExpression Evaluate()
        => throw new NotImplementedException();

    public IExpression StepEvaluate()
        => throw new NotImplementedException();

    public override string ToString()
        => throw new NotImplementedException();

    public string ToString(string? format)
        => throw new NotImplementedException();
}
