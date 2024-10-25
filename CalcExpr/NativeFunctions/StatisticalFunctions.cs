﻿using CalcExpr.Attributes;
using CalcExpr.Expressions;
using CalcExpr.Expressions.Collections;
using CalcExpr.Expressions.Terminals;
using CalcExpr.FunctionAttributes.ConditionalAttributes;

namespace CalcExpr.NativeFunctions;

public static class StatisticalFunctions
{
    [NativeFunction("count", "length", "len")]
    public static int Count(IExpression expressions)
    {
        return expressions is IEnumerableExpression enum_expr
            ? enum_expr.Count()
            : 1;
    }

    [NativeFunction("max", "maximum")]
    public static Terminal Max([AreNumbers] IExpression expressions)
    {
        return expressions is IEnumerableExpression enumExpr
            ? (Terminal)(enumExpr.MaxBy(x => ((Number)x).Value) ?? Undefined.UNDEFINED)
            : (Number)expressions;
    }

    [NativeFunction("average", "mean")]
    public static Terminal Mean([AreNumbers] IExpression expressions)
    {
        if (expressions is IEnumerableExpression enumExpr)
        {
            int count = enumExpr.Count();
            
            return count == 0
                ? Undefined.UNDEFINED
                : (Number)(enumExpr.Select(x => ((Number)x).Value).Aggregate((a, b) => a + b) / count);
        }

        return (Number)expressions;
    }

    [NativeFunction("median")]
    public static Terminal Median([AreNumbers] IExpression expressions)
    {
        if (expressions is IEnumerableExpression enumExpr)
        {
            IEnumerable<IExpression> sorted = enumExpr.OrderBy(x => ((Number)x).Value);
            int half = sorted.Count() / 2;

            return sorted.Count() % 2 == 0
                ? (Number)((((Number)enumExpr.ElementAt(half - 1)).Value +
                    ((Number)enumExpr.ElementAt(half)).Value) / 2)
                : (Number)enumExpr.ElementAt(half);
        }

        return (Number)expressions;
    }

    [NativeFunction("min", "minimum")]
    public static Terminal Min([AreNumbers] IExpression expressions)
    {
        return expressions is IEnumerableExpression enumExpr
            ? (Terminal)(enumExpr.MinBy(x => ((Number)x).Value) ?? Undefined.UNDEFINED)
            : (Number)expressions;
    }

    [NativeFunction("mode")]
    public static Terminal Mode([AreNumbers] IExpression expressions)
    {
        if (expressions is IEnumerableExpression enumExpr)
        {
            Dictionary<Number, ushort> counts = [];

            foreach (Number expression in enumExpr.Cast<Number>())
                if (!counts.TryAdd(expression, 1))
                    counts[expression]++;

            int max = counts.MaxBy(kvp => kvp.Value).Value;
            IEnumerable<Number> modes = counts.OrderBy(kvp => kvp.Value)
                .Where(kvp => kvp.Value == max)
                .Select(kvp => kvp.Key);

            return modes.Count() == 1
                ? modes.Single()
                : TerminalCollection.TerminateCollection(new Set(modes));
        }

        return (Number)expressions;
    }

    [NativeFunction("percentile")]
    public static Terminal Percentile([AreNumbers] IExpression expressions, double p)
    {
        if (expressions is IEnumerableExpression enumExpr)
        {
            int count = enumExpr.Count();

            if (count == 0 || p < 0 || p > 1)
                return Undefined.UNDEFINED;
            else if (count == 1)
                return (Number)enumExpr.Single();

            IEnumerable<Number> values = enumExpr.Cast<Number>().OrderBy(x => x.Value);
            double i = (count - 1) * p;
            int prev_i = (int)Math.Floor(i);

            if (prev_i == count - 1)
                return values.Last();

            double prev = values.ElementAt(prev_i).Value;
            double next = values.ElementAt(prev_i + 1).Value;

            return (Terminal)(prev + (i % 1) * (next - prev));
        }
            
        return (Number)expressions;
    }

    [NativeFunction("quartile")]
    public static Terminal Quartile([AreNumbers] IExpression expressions, [Range(0, 4)] double q)
    {
        return Percentile(expressions, q / 4);
    }

    [NativeFunction("stdev", "stdevs")]
    public static Terminal SampleStandardDeviation([AreNumbers] IExpression expressions)
    {
        if (expressions is IEnumerableExpression enum_expr)
        {
            double mean = ((Number)Mean(expressions)).Value;

            return (Number)Math.Sqrt(((double)1 / (enum_expr.Count() - 1)) *
                ((Number)Sum(enum_expr.Map(x => (Number)Math.Pow(((Number)x).Value - mean, 2)))).Value);
        }

        return Undefined.UNDEFINED;
    }

    [NativeFunction("stdevp")]
    public static Terminal PopulationStandardDeviation([AreNumbers] IExpression expressions)
    {
        if (expressions is IEnumerableExpression enum_expr)
        {
            double mean = ((Number)Mean(expressions)).Value;

            return (Number)Math.Sqrt(((double)1 / enum_expr.Count()) *
                ((Number)Sum(enum_expr.Map(x => (Number)Math.Pow(((Number)x).Value - mean, 2)))).Value);
        }

        return (Number)0;
    }

    [NativeFunction("sum", "total")]
    public static Terminal Sum([AreNumbers] IExpression expressions)
    {
        return expressions is IEnumerableExpression enumExpr
            ? (Number)enumExpr.Select(x => ((Number)x).Value).Aggregate((a, b) => a + b)
            : (Number)expressions;
    }
}
