﻿using CalcExpr.Expressions.Functions;
using CalcExpr.Expressions.Terminals;
using CalcExpr.TypeConverters;
using System.Collections.ObjectModel;

namespace CalcExpr.Context;

public partial class ExpressionContext
{
    protected internal static readonly ReadOnlyDictionary<Type, List<ITypeConverter>> DEFAULT_CONVERTERS =
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
        }.AsReadOnly();
    protected internal static readonly IReadOnlyList<Type> DEFAULT_TYPES = [.. DEFAULT_CONVERTERS.Keys];

    protected readonly Dictionary<string, Terminal> _variables;
    protected readonly ReadOnlyDictionary<Type, List<ITypeConverter>> _type_converters;
    protected readonly Dictionary<string, bool> _aliases;

    public string[] Variables
        => [.. _aliases.Keys];

    public virtual Terminal this[string name]
    {
        get
        {
            if (_aliases.TryGetValue(name, out bool is_func))
                return is_func
                    ? _functions[name]
                    : _variables[name];
            
            return Undefined.UNDEFINED;
        }
        set => SetVariable(name, value);
    }

    public ExpressionContext(Dictionary<string, Terminal>? variables = null,
        bool register_default_functions = true, IEnumerable<ITypeConverter>? type_converters = null)
    {
        Dictionary<string, bool> aliases = [];
        Dictionary<string, Terminal> vars = [];
        Dictionary<string, Function> funcs = [];
        Dictionary<Type, List<ITypeConverter>> types = [];

        if (variables is not null)
        {
            foreach (string var in variables.Keys)
            {
                if (variables[var] is Function func)
                {
                    funcs[var] = func;
                    aliases[var] = true;
                }
                else
                {
                    vars[var] = variables[var];
                    aliases[var] = false;
                }
            }
        }

        if (type_converters is null)
        {
            types = DEFAULT_CONVERTERS.ToDictionary();
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

        _type_converters = types.AsReadOnly();
        _aliases = aliases;
        _variables = vars;
        _functions = funcs;

        if (register_default_functions)
            SetFunctions(GetType().Assembly);
    }

    public virtual ExpressionContext Clone()
    {
        Dictionary<string, Terminal> vars = [];
        Dictionary<string, Function> funcs = [];

        foreach (string var in _variables.Keys)
            vars.Add(var, _variables[var]);

        ExpressionContext result = new ExpressionContext(vars, false, _type_converters.SelectMany(t => t.Value));

        // TODO: Replace with streamlined function when created.
        foreach (string func in _functions.Keys)
            funcs.Add(func, _functions[func]);

        foreach (KeyValuePair<string, Function> func in _functions)
            result.SetFunction(func.Key, func.Value);

        return result;
    }

    public virtual bool SetVariable(string name, Terminal expression)
    {
        if (expression is null)
            return RemoveVariable(name);

        if (expression is not Function function)
        {
            _functions.Remove(name);
            _aliases[name] = false;
            _variables[name] = expression;
            return true;
        }
        else
        {
            return SetFunction(name, function);
        }
    }

    public virtual bool RemoveVariable(string name)
        => _aliases.Remove(name) && (_variables.Remove(name) || _functions.Remove(name));

    public virtual bool ContainsVariable(string name)
        => _aliases.ContainsKey(name);

    public virtual ITypeConverter[] GetTypeConverters<T>()
        => GetTypeConverters(typeof(T));

    public virtual ITypeConverter[] GetTypeConverters(Type type)
    {
        if (_type_converters.TryGetValue(type, out List<ITypeConverter>? converters))
            return [.. converters];
        else
            return [];
    }
}
