using CalcExpr.Context;

namespace CalcExpr.Expressions;

public class Function : IExpression
{
    private readonly IReadOnlyList<string> _parameters;

    public readonly Delegate Body;

    public string[] Parameters
        => _parameters.ToArray();

    public Function(Delegate body) : this(body.Method.GetParameters().Select(p => p.Name ?? ""), body)
    { }

    public Function(IEnumerable<string> parameters, Delegate body)
        => throw new NotImplementedException();

    public IExpression Evaluate()
        => throw new NotImplementedException();

    public IExpression Evaluate(ExpressionContext variables)
        => throw new NotImplementedException();

    public IExpression StepEvaluate()
        => throw new NotImplementedException();

    public IExpression StepEvaluate(ExpressionContext variables)
        => throw new NotImplementedException();

    public IExpression Clone()
        => throw new NotImplementedException();

    public override bool Equals(object? obj)
        => throw new NotImplementedException();

    public override int GetHashCode()
        => throw new NotImplementedException();

    public override string ToString()
        => ToString(null);

    public string ToString(string? format)
        => throw new NotImplementedException();
}
