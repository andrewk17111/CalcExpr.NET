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
        => obj is LambdaFunction lambda && lambda.Parameters.Length == Parameters.Length &&
            lambda.Parameters.Select((arg, i) => arg == Parameters[i]).Aggregate((a, b) => a && b) &&
            lambda.Body == Body;

    public override int GetHashCode()
        => Parameters.GetHashCode();

    public override string ToString()
        => ToString(null);

    public string ToString(string? format)
        => $"({String.Join(",", Parameters)})=>{Body.ToString(format)}";
}
