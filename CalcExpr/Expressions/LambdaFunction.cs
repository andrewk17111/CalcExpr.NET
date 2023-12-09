using CalcExpr.Context;

namespace CalcExpr.Expressions;

public class LambdaFunction : IFunction
{
    private readonly IReadOnlyList<string> _parameters;

    public readonly IExpression Body;

    public string[] Parameters
        => _parameters.ToArray();

    public LambdaFunction(string[] parameters, IExpression body)
    {
        _parameters = parameters;
        Body = body;
    }

    public static IExpression Invoke(IExpression body, ExpressionContext context, IExpression[] arguments)
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
        => throw new NotImplementedException();

    public string ToString(string? format)
        => throw new NotImplementedException();
}
