using CalcExpr.Expressions;
using CalcExpr.Extensions;
using System;
using System.Reflection;

namespace CalcExpr.Context;

public partial class ExpressionContext
{
    private readonly Dictionary<string, IFunction> _functions;

    public string[] Functions
        => [.. _functions.Keys];

    public IExpression this[string function, IEnumerable<IExpression> arguments]
    {
        get
        {
            if (ContainsFunction(function))
            {
                IFunction func = _functions[function];

                return IFunction.ForEach(func, arguments, this);
            }

            return Constant.UNDEFINED;
        }
    }

    public void SetFunctions(Assembly assembly)
    {
        foreach (Type t in assembly.GetTypes())
        {
            Dictionary<string[], IFunction> built_in_functions = t.GetFunctions(_type_converters.Keys);

            SetFunctions(built_in_functions);
        }
    }

    public void SetFunctions(IEnumerable<KeyValuePair<string[], IFunction>> functions)
    {
        foreach (KeyValuePair<string[], IFunction> func in functions)
            SetFunctions(func.Key.ToDictionary(k => k, k => func.Value));
    }

    public void SetFunctions(IEnumerable<KeyValuePair<string, IFunction>> functions)
    {
        foreach (KeyValuePair<string, IFunction> func in functions)
            SetFunction(func.Key, func.Value);
    }

    public bool SetFunction(string name, IFunction function)
    {
        if (function is null)
            return _functions.Remove(name);

        _functions[name] = function;
        return true;
    }

    public bool RemoveFunction(string name)
        => _functions.Remove(name);

    public bool ContainsFunction(string name)
        => _functions.ContainsKey(name);
}
