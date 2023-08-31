namespace CalcExpr.Expressions;

public class Constant : IExpression
{
    public readonly string Identifier;

    public Constant(string identifier)
        => throw new NotImplementedException();

    public IExpression Clone()
        => throw new NotImplementedException();

    public IExpression Evaluate()
        => throw new NotImplementedException();

    public IExpression StepEvaluate()
        => throw new NotImplementedException();

    public override bool Equals(object? obj)
        => throw new NotImplementedException();

    public override int GetHashCode()
        => throw new NotImplementedException();

    public override string ToString()
        => throw new NotImplementedException();

    public string ToString(string? format)
        => throw new NotImplementedException();
}
