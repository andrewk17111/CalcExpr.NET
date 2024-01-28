using CalcExpr.Expressions;

namespace CalcExpr.FunctionAttributes.ConditionalAttributes;

public class IsNumberAttribute() : ConditionAttribute(TestCondition)
{
    private static bool TestCondition(IExpression expression)
        => expression is Number;
}
