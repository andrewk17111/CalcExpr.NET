using CalcExpr.Context;
using System.Collections.Immutable;

namespace CalcExpr.Expressions;

public class FunctionCall : IExpression
{
    private readonly IReadOnlyList<IExpression> _arguments;

    public readonly string Name;

    public IExpression[] Arguments
        => _arguments.ToArray();

    public FunctionCall(string name, IExpression[] arguments)
        => throw new NotImplementedException();

    public IExpression Evaluate()
        => throw new NotImplementedException();

    public IExpression Evaluate(ExpressionContext context)
        => throw new NotImplementedException();

    public IExpression StepEvaluate()
        => throw new NotImplementedException();

    public IExpression StepEvaluate(ExpressionContext context)
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