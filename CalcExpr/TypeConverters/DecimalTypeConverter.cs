﻿using CalcExpr.Expressions;
using CalcExpr.Expressions.Terminals;

namespace CalcExpr.TypeConverters;

public class DecimalTypeConverter : ITypeConverter<decimal?>
{
    public IExpression ConvertToExpression(decimal? value)
    {
        try
        {
            if (value.HasValue)
                return (Terminal)Convert.ToDouble(value.Value);
        }
        catch
        {
        }

        return Undefined.UNDEFINED;
    }

    public decimal? ConvertFromExpression(IExpression? expression)
    {
        try
        {
            if (expression is not null && expression is Number num)
                return Convert.ToDecimal(num.Value);
        }
        catch
        {
        }

        return null;
    }
}
