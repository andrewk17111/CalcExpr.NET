using CalcExpr.Context;

namespace CalcExpr.Expressions;

public class FunctionCall : IExpression
{
    private readonly IReadOnlyList<IExpression> _arguments;

    public readonly string Name;

    public IExpression[] Arguments
        => _arguments.ToArray();

    public FunctionCall(string name, IExpression[] arguments)
    {
        Name = name;
        _arguments = arguments.ToArray();
    }

    public IExpression Evaluate()
        => Evaluate(new ExpressionContext());

    public IExpression Evaluate(ExpressionContext context)
        => context[Name, _arguments].Evaluate(context);

    public IExpression StepEvaluate()
        => StepEvaluate(new ExpressionContext());

    public IExpression StepEvaluate(ExpressionContext context)
        => throw new NotImplementedException();

    public IExpression Clone()
        => new FunctionCall(Name, Arguments);

    public override bool Equals(object? obj)
        => obj is not null && obj is FunctionCall call && call.Name == Name &&
            call._arguments.Select((a, i) => a.Equals(_arguments[i])).Aggregate((a, b) => a && b);

    public override int GetHashCode()
        => HashCode.Combine(Name, _arguments);

    public override string ToString()
        => ToString(null);

    public string ToString(string? format)
        => $"{Name}({String.Join(", ", Arguments.Select(arg => arg.ToString(format)))})";
}