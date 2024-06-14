using CalcExpr.Expressions;
using CalcExpr.TypeConverters;

namespace CalcExpr.Context;

public partial class ExpressionContext
{
    internal static readonly Dictionary<Type, List<ITypeConverter>> DEFAULT_CONVERTERS =
        new Dictionary<Type, List<ITypeConverter>>
        {
            { typeof(bool), [new BooleanTypeConverter()] },
            { typeof(sbyte), [new IntegerTypeConverter<sbyte>()] },
            { typeof(short), [new IntegerTypeConverter<short>()] },
            { typeof(int), [new IntegerTypeConverter<int>()] },
            { typeof(long), [new IntegerTypeConverter<long>()] },
            { typeof(Int128), [new IntegerTypeConverter<Int128>()] },
            { typeof(byte), [new IntegerTypeConverter<byte>()] },
            { typeof(ushort), [new IntegerTypeConverter<ushort>()] },
            { typeof(uint), [new IntegerTypeConverter<uint>()] },
            { typeof(ulong), [new IntegerTypeConverter<ulong>()] },
            { typeof(UInt128), [new IntegerTypeConverter<UInt128>()] },
            { typeof(Half), [new FloatTypeConverter<Half>()] },
            { typeof(float), [new FloatTypeConverter<float>()] },
            { typeof(double), [new FloatTypeConverter<double>()] },
            { typeof(decimal), [new DecimalTypeConverter()] },
        };
    internal static readonly Type[] DEFAULT_TYPES = [.. DEFAULT_CONVERTERS.Keys];

    private readonly Dictionary<string, IExpression> _variables;
    private readonly Dictionary<Type, List<ITypeConverter>> _type_converters;

    public string[] Variables
        => _variables.Keys.Concat(Functions).ToArray();

    public IExpression this[string variable]
    {
        get => _variables.TryGetValue(variable, out IExpression? var_value)
            ? var_value
            : _functions.TryGetValue(variable, out IFunction? func_value)
                ? func_value
                : Constant.UNDEFINED;
        set => SetVariable(variable, value);
    }

    public ExpressionContext(Dictionary<string, IExpression>? variables = null,
        bool register_default_functions = true, IEnumerable<ITypeConverter>? type_converters = null)
    {
        Dictionary<string, IExpression> vars = [];
        Dictionary<string, IFunction> funcs = [];
        Dictionary<Type, List<ITypeConverter>> types = [];

        if (variables is not null)
            foreach (string var in variables.Keys)
                if (variables[var] is IFunction func)
                    funcs.Add(var, func);
                else
                    vars.Add(var, variables[var]);

        if (type_converters is null)
        {
            types = DEFAULT_CONVERTERS;
        }
        else
        {
            foreach (ITypeConverter converter in type_converters)
            {
                Type? convert_type = converter.GetType().GetInterface("ITypeConverter`1")?.GetGenericArguments()?
                    .First();

                if (convert_type == typeof(Nullable<>))
                    convert_type = convert_type.GetGenericArguments().First();

                if (convert_type is not null)
                {
                    if (convert_type.GetGenericArguments().Length == 0)
                    {
                        if (types.TryGetValue(convert_type, out List<ITypeConverter>? convert_list))
                            convert_list.Add(converter);
                        else
                            types[convert_type] = [converter];
                    }
                }
            }
        }

        _type_converters = types;
        _variables = vars;
        _functions = funcs;

        if (register_default_functions)
            SetFunctions(GetType().Assembly);
    }

    public ExpressionContext Clone()
    {
        Dictionary<string, IExpression> vars = [];
        Dictionary<string, IFunction> funcs = [];

        foreach (string var in _variables.Keys)
            vars.Add(var, _variables[var]);

        ExpressionContext result = new ExpressionContext(vars, false, _type_converters.SelectMany(t => t.Value));

        // TODO: Replace with streamlined function when created.
        foreach (string func in _functions.Keys)
            funcs.Add(func, _functions[func]);

        foreach (KeyValuePair<string, IFunction> func in _functions)
            result.SetFunction(func.Key, func.Value);

        return result;
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

    public ITypeConverter[] GetTypeConverters<T>()
        => GetTypeConverters(typeof(T));

    public ITypeConverter[] GetTypeConverters(Type type)
    {
        if (_type_converters.TryGetValue(type, out List<ITypeConverter>? converters))
            return [.. converters];
        else
            return [];
    }
}
