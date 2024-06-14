using CalcExpr.Expressions;

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
