using CalcExpr.Expressions;

namespace CalcExpr.Context;

public class ExpressionContext
{
    private Dictionary<string, IExpression> _variables;
    private Dictionary<string, Function> _functions;

    public IExpression this[string variable]
    {
        get => _variables.ContainsKey(variable) ? _variables[variable] : Constant.UNDEFINED;
        set => _variables[variable] = value;
    }

    public IExpression this[string function, IEnumerable<IExpression> arguments]
    {
        get
        {
            if (!_functions.ContainsKey(function))
                return Constant.UNDEFINED;

            Function func = _functions[function];
            List<object?> args = new List<object?>();

            if (func.RequiresContext)
            {
                bool[] is_context = func.Body.Method.GetParameters()
                    .Select(p => p.ParameterType == typeof(ExpressionContext))
                    .ToArray();
                int arg_i = 0;

                for (int i = 0; i < is_context.Length; i++)
                    args.Add(is_context[i] ? this : arguments.ElementAt(arg_i++));
            }
            else
            {
                args.AddRange(arguments.Select(arg => arg.Evaluate(this)));
            }

            return (IExpression?)func.Body.Method.Invoke(func, args.ToArray()) ?? Constant.UNDEFINED;
        }
    }

    public ExpressionContext(Dictionary<string, IExpression>? variables = null,
        Dictionary<string, Function>? functions = null)
    {
        Dictionary<string, IExpression> vars = new Dictionary<string, IExpression>();
        Dictionary<string, Function> funcs = functions?.ToDictionary(kvp => kvp.Key, kvp => kvp.Value)
            ?? new Dictionary<string, Function>();

        if (variables is not null)
            foreach (string var in variables.Keys)
                if (variables[var] is Function func)
                    funcs.Add(var, func);
                else
                    vars.Add(var, variables[var]);

        _variables = vars;
        _functions = funcs;
    }

    public bool SetVariable(string name, IExpression expression)
    {
        if (expression is null)
            return _variables.Remove(name);

        _variables[name] = expression;
        return true;
    }

    public bool RemoveVariable(string name)
        => _variables.Remove(name);

    public bool ContainsVariable(string name)
        => _variables.ContainsKey(name);

    public bool SetFunction(string name, Function functions)
        => throw new NotImplementedException();

    public bool RemoveFunction(string name)
        => throw new NotImplementedException();

    public bool ContainsFunction(string name)
        => throw new NotImplementedException();
}
