﻿using CalcExpr.Expressions;
using CalcExpr.Expressions.Terminals;
using CalcExpr.FunctionAttributes.PreprocessAttributes;

namespace CalcExpr.TypeConverters;

public class BooleanTypeConverter : ITypeConverter<bool?>
{
    public Terminal ConvertToExpression(bool? value)
    {
        if (value.HasValue)
        {
            return value.Value ? Logical.TRUE : Logical.FALSE;
        }

        return Undefined.UNDEFINED;
    }

    public bool? ConvertFromExpression(IExpression? expression)
    {
        if (expression is null)
            return null;

        IExpression result = new AsBooleanAttribute().Preprocess(expression);

        if (Undefined.UNDEFINED.Equals(result))
            return null;

        return Logical.TRUE.Equals(result);
    }
}
