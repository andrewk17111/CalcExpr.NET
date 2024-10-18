using CalcExpr.Context;
using CalcExpr.Expressions.Collections;
using CalcExpr.Expressions.Components;
using CalcExpr.Expressions.Terminals;
using System.Reflection;

namespace CalcExpr.Expressions.Functions;

internal abstract class Function : Terminal, IFunction
{
    public abstract IParameter[] Parameters { get; }

    public abstract bool IsElementwise { get; }

    public abstract IExpression Invoke(IExpression[] arguments, ExpressionContext context);
    
    public abstract override string ToString(string? format);

    public static ExpressionContext ContextReconciliation(ExpressionContext outer_context,
        ExpressionContext inner_context, IEnumerable<Parameter> parameters)
    {
        foreach (string variable in inner_context.Variables.Except(parameters.Select(p => p.Name)))
            outer_context[variable] = inner_context[variable];

        return outer_context;
    }

    public static IExpression ForEach(MethodInfo function, IEnumerable<IExpression> arguments, ExpressionContext context)
        => ForEach(new NativeFunction(function), arguments, context);

    public static IExpression ForEach(IFunction function, IEnumerable<IExpression> arguments, ExpressionContext context)
    {
        if (arguments.Count() != function.Parameters.Where(p => p is not ContextParameter).Count())
            return Undefined.UNDEFINED;

        if (function.IsElementwise && arguments.Any(arg => arg is IEnumerableExpression))
        {
            int length = arguments.Where(arg => arg is IEnumerableExpression)
                .Select(arg => ((IEnumerableExpression)arg).Count())
                .Min();
            List<IExpression> results = [];

            for (int i = 0; i < length; i++)
                results.Add(function.Invoke(arguments.Select(arg => arg is IEnumerableExpression enum_expr
                    ? enum_expr.ElementAt(i)
                    : arg).ToArray(), context));

            Type enum_type = arguments.First(arg => arg is IEnumerableExpression).GetType();
            MethodInfo? create_method = enum_type.GetMethod("ConvertIEnumerable", [typeof(IEnumerable<IExpression>)]);

            MethodInfo[] methods = enum_type.GetMethods(BindingFlags.Static | BindingFlags.Public);
            return (IEnumerableExpression?)create_method!.Invoke(null, [results])!;
        }
        else
        {
            return function.Invoke([.. arguments], context);
        }
    }
}
