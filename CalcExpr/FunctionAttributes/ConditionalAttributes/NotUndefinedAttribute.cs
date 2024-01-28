using CalcExpr.Expressions;

namespace CalcExpr.FunctionAttributes.ConditionalAttributes;

public class NotUndefinedAttribute() : ConditionAttribute(TestCondition)
{
    private static bool TestCondition(IExpression expression)
        => !Constant.UNDEFINED.Equals(expression);
}
