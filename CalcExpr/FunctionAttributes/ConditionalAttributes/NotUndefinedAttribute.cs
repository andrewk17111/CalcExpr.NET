﻿using CalcExpr.Expressions;

namespace CalcExpr.FunctionAttributes.ConditionalAttributes;

public class NotUndefinedAttribute : ConditionAttribute
{
    public override bool CheckCondition(IExpression expression)
        => !Constant.UNDEFINED.Equals(expression);
}
