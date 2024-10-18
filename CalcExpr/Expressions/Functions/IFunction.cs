using CalcExpr.Context;
using CalcExpr.Expressions.Components;

namespace CalcExpr.Expressions.Functions;

public interface IFunction : IExpression
{
    public IParameter[] Parameters { get; }

    public bool IsElementwise { get; }

    public object?[]? ProcessArguments(IEnumerable<IExpression> arguments, ExpressionContext context)
    {
        IParameter[] parameters = Parameters.Where(p => p is not ContextParameter).ToArray();
        IExpression[] args = arguments.ToArray();
        List<object?> results = [];

        for (int i = 0; i < parameters.Length; i++)
        {
            object? argument = parameters[i].ProcessArgument(args[i], context);

            if (argument is null && !parameters[i].AllowNull)
                return null;

            results.Add(argument);
        }

        return [.. results];
    }

    public IExpression Invoke(IExpression[] arguments, ExpressionContext context);
}
