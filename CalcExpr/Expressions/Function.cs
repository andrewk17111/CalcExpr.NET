using CalcExpr.Context;

namespace CalcExpr.Expressions;

public class Function : IFunction
{
    private readonly IReadOnlyList<string> _parameters;

    public readonly Delegate Body;
    public readonly bool RequiresContext;

    public string[] Parameters
        => _parameters.ToArray();

    public Function(Delegate body) : this(body.Method.GetParameters().Select(p => p.Name ?? ""), body)
    { }

    public Function(IEnumerable<string> parameters, Delegate body)
    {
        _parameters = parameters.ToArray() ?? Array.Empty<string>();
        RequiresContext = body.Method.GetParameters().Select(p => p.ParameterType == typeof(ExpressionContext))
            .Contains(true);
        Body = body;
    }

    public IExpression Evaluate()
        => Clone();

    public IExpression Evaluate(ExpressionContext context)
        => Clone();

    public IExpression StepEvaluate()
        => Clone();

    public IExpression StepEvaluate(ExpressionContext context)
        => Clone();

    public IExpression Clone()
        => new Function(Parameters, Body);

    public override bool Equals(object? obj)
        => obj is not null && obj is Function func && func.Body.Equals(Body) &&
            func.Parameters.Select((p, i) => p.Equals(Parameters[i])).Aggregate((a, b) => a && b);

    public override int GetHashCode()
        => HashCode.Combine(Parameters, Body);

    public override string ToString()
        => ToString(null);

    public string ToString(string? format)
        => $"";
}

public interface IFunction : IExpression
{
    public string[] Parameters { get; }
}
