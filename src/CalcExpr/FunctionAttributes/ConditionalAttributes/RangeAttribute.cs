﻿using CalcExpr.Expressions;
using CalcExpr.Expressions.Interfaces;
using CalcExpr.Expressions.Terminals;

namespace CalcExpr.FunctionAttributes.ConditionalAttributes;

public class RangeAttribute(double minimum, double maximum, bool allowUndefined = false, bool inclusive = true)
    : ConditionAttribute
{
    public readonly Terminal Minimum = (Terminal)minimum;
    public readonly Terminal Maximum = (Terminal)maximum;
    public readonly bool AllowUndefined = allowUndefined;
    public readonly bool Inclusive = inclusive;

    public override bool CheckCondition(IExpression expression)
    {
        IExpression lowerConditionResult = IBinaryOperable.Operate(Inclusive ? ">=" : ">", expression, Minimum);

        if (lowerConditionResult is Logical isHigher)
        {
            if (!isHigher.Value)
            {
                return false;
            }
        }
        else if (!AllowUndefined)
        {
            return false;
        }

        IExpression upperConditionResult = IBinaryOperable.Operate(Inclusive ? "<=" : "<", expression, Maximum);

        return upperConditionResult is Logical isLower ? isLower.Value : AllowUndefined;
    }
}