using DiceEngine.Expressions;
using System.Reflection;

namespace DiceEngine.Context;

public class ExpressionContext
{
    private readonly Dictionary<string, IExpression> _variables;
    private readonly Dictionary<string, IFunction> _functions;

    public readonly Random Random;

    public string[] Variables
        => _variables.Keys.Concat(Functions).ToArray();

    public string[] Functions
        => _functions.Keys.ToArray();

    public IExpression this[string variable]
    {
        get => _variables.TryGetValue(variable, out IExpression? var_value)
            ? var_value
            : _functions.TryGetValue(variable, out IFunction? func_value)
                ? func_value
                : Constant.UNDEFINED;
        set => SetVariable(variable, value);
    }

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

    public ExpressionContext(Dictionary<string, IExpression>? variables = null,
        Dictionary<string, IFunction>? functions = null, int? seed = null)
    {
        Dictionary<string, IExpression> vars = [];
        Dictionary<string, IFunction> funcs = functions?.ToDictionary(kvp => kvp.Key, kvp => kvp.Value) ?? [];

        if (variables is not null)
            foreach (string var in variables.Keys)
                if (variables[var] is IFunction func)
                    funcs.Add(var, func);
                else
                    vars.Add(var, variables[var]);

        if (functions is null)
        {
            foreach (Type t in Assembly.GetExecutingAssembly().GetTypes())
            {
                if (!t.IsClass || t.Namespace != "DiceEngine.BuiltInFunctions")
                    continue;

                foreach (MethodInfo method in t.GetMethods())
                {
                    if (!Function.IsValidFunction(method, out string[]? aliases))
                        continue;
                    
                    Function function = new Function(method);

                    foreach (string alias in aliases!)
                        funcs.Add(alias, function);
                }
            }
        }

        _variables = vars;
        _functions = funcs;
        Random = seed is null ? new Random() : new Random(seed.Value);
    }

    public ExpressionContext Clone()
    {
        Dictionary<string, IExpression> vars = [];
        Dictionary<string, IFunction> funcs = [];

        foreach (string var in _variables.Keys)
            vars.Add(var, _variables[var]);

        foreach (string func in _functions.Keys)
            funcs.Add(func, (IFunction)_functions[func]);

        return new ExpressionContext(vars, funcs);
    }

    public bool SetVariable(string name, IExpression expression)
    {
        if (expression is null)
            return RemoveVariable(name);

        if (expression is not IFunction function)
        {
            _variables[name] = expression;
            return true;
        }
        else
        {
            return SetFunction(name, function);
        }
    }

    public bool RemoveVariable(string name)
        => _variables.Remove(name) || _functions.Remove(name);

    public bool ContainsVariable(string name)
        => _variables.ContainsKey(name) || ContainsFunction(name);
    
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
